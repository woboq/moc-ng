/****************************************************************************
 *  Copyright (C) 2013-2016 Woboq GmbH
 *  Olivier Goffart <contact at woboq.com>
 *  https://woboq.com/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "generator.h"
#include "mocng.h"
#include "qbjs.h"
#include <string>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/Sema/Sema.h>

#include <iostream>

// Remove the decltype if possible
static clang::QualType getDesugarType(const clang::QualType &QT) {
    if (auto DL = QT->getAs<clang::DecltypeType>()) {
        return DL->desugar();
    }
    return QT;
}

/* Wrapper for the change in the name in clang 3.5 */
template <typename T> static auto getResultType(T *decl) -> decltype(decl->getResultType())
{ return getDesugarType(decl->getResultType()); }
template <typename T> static auto getResultType(T *decl) -> decltype(decl->getReturnType())
{ return getDesugarType(decl->getReturnType()); }

/**
 * Return the type as writen in the file at the given SourceRange.
 * May return an empty string if the type was expended from macros.
 */
static std::string TypeStringFromSourceRange(clang::SourceRange Range, const clang::SourceManager &SM)
{
    if (Range.isInvalid() || !Range.getBegin().isFileID())
        return {};
    clang::FileID FID = SM.getFileID(Range.getBegin());
    if (FID != SM.getFileID(Range.getEnd()))
        return {};
    const llvm::MemoryBuffer *Buffer = SM.getBuffer(FID);
    const char *Buf = Buffer->getBufferStart();
    auto B = SM.getFileOffset(Range.getBegin());
    auto E = SM.getFileOffset(Range.getEnd());
    if (Buf[E] == '>') { // a type can be terminated be either a '>' or an identifier
        E++;
    } else {
        while (std::isalnum(Buf[E]) || Buf[E] == '\\' || Buf[E] == '_') E++;
    }
    return std::string(Buf+B, Buf+E);
}

// Returns true if the last argument of this mehod is a 'QPrivateSignal'
static bool HasPrivateSignal(const clang::CXXMethodDecl *MD) {
    if (MD && MD->getNumParams()) {
        clang::CXXRecordDecl* RD = MD->getParamDecl(MD->getNumParams()-1)->getType()->getAsCXXRecordDecl();
        return RD && RD->getIdentifier() && RD->getName() == "QPrivateSignal";
    }
    return false;
}

// Executes the 'Functor'  for each method,  including clones
template<typename T, typename F>
static void ForEachMethod(const std::vector<T> &V, F && Functor) {
    for(auto it : V) {
        int Clones = it->getNumParams() - it->getMinRequiredArguments();
        for (int C = 0; C <= Clones; ++C)
            Functor(it, C);
    }
}

// Count the number of method in the vector, including clones
template<typename T>
int CountMethod(const std::vector<T> &V) {
    int R  = 0;
    ForEachMethod(V, [&](const clang::CXXMethodDecl*, int) { R++; });
    return R;
}

// Count the total number of parametters in the vector
template<typename T> int AggregateParameterCount(const std::vector<T>& V) {
    int R = 0;
    ForEachMethod(V, [&](const clang::CXXMethodDecl *M, int Clone) {
        R += M->getNumParams() - Clone;
        R += 1; // return value;
        if (HasPrivateSignal(M))
            R--;
    });
    return R;
}

// Generate the data in the data array for the function in the given vector.
//  ParamIndex is a reference to the index in which to store the parametters.
template <typename T>
void Generator::GenerateFunctions(const std::vector<T> &V, const char* TypeName, MethodFlags Type, int& ParamIndex)
{
    if (V.empty())
        return;

    OS << "\n // " << TypeName << ": name, argc, parameters, tag, flags\n";

    ForEachMethod(V, [&](const clang::CXXMethodDecl *M, int Clone) {
        unsigned int Flags = Type;
        if (M->getAccess() == clang::AS_private)
            Flags |= AccessPrivate;
        else if (M->getAccess() == clang::AS_public)
            Flags |= AccessPublic;
        else if (M->getAccess() == clang::AS_protected)
            Flags |= AccessProtected;

        if (Clone)
            Flags |= MethodCloned;

        for (auto attr_it = M->specific_attr_begin<clang::AnnotateAttr>();
             attr_it != M->specific_attr_end<clang::AnnotateAttr>();
             ++attr_it) {
            const clang::AnnotateAttr *A = *attr_it;
            if (A->getAnnotation() == "qt_scriptable") {
                Flags |= MethodScriptable;
            } else if (A->getAnnotation().startswith("qt_revision:")) {
                Flags |= MethodRevisioned;
            } else if (A->getAnnotation() == "qt_moc_compat") {
                Flags |= MethodCompatibility;
            }
        }

        int argc =  M->getNumParams() - Clone;
        if (HasPrivateSignal(M))
            argc--;

        std::string tag = Moc->GetTag(M->getSourceRange().getBegin(), Ctx.getSourceManager());
        OS << "    " << StrIdx(M->getNameAsString()) << ", " << argc << ", " << ParamIndex << ", " << StrIdx(tag) << ", 0x";
        OS.write_hex(Flags) << ",\n";
        ParamIndex += 1 + argc * 2;
    });
}


static bool IsIdentChar(char c) {
    return (c=='_' || c=='$' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
}

//Generate the type information for the argument
void Generator::GenerateTypeInfo(clang::QualType Type)
{
    if (Type->isVoidType()) {
        OS << "QMetaType::Void";
        return;
    }

    // remove const or const &
    if (Type->isReferenceType() && Type.getNonReferenceType().isConstQualified())
        Type = Type.getNonReferenceType();
    Type.removeLocalConst();

    const clang::TypedefType * TT = Type->getAs<clang::TypedefType>();
    // Handle builtin types as QMetaType,  but ignores typedef their name is likely not registered
    //  (FIXME:  all the registered typedef such as uint and qint64 should go there.
    if (Type->isBuiltinType() && (!TT)) {
        const clang::BuiltinType * BT = Type->getAs<clang::BuiltinType>();
        switch(+BT->getKind()) {
#define BUILTIN(Type) \
            case clang::BuiltinType::Type: \
                OS << "QMetaType::" #Type; \
                return;
            BUILTIN(Bool)
            BUILTIN(Int)
            BUILTIN(UInt)
            BUILTIN(LongLong)
            BUILTIN(ULongLong)
            BUILTIN(Double)
            BUILTIN(Long)
            BUILTIN(Short)
            // Char?
            BUILTIN(ULong)
            BUILTIN(UShort)
            BUILTIN(UChar)
            BUILTIN(Float)
            BUILTIN(SChar)
#undef BUILTIN
        }
    }
    // TODO:  Find more QMetaType


    clang::PrintingPolicy Policy = PrintPolicy;
    Policy.SuppressScope = true;
    std::string TypeString = getDesugarType(Type).getAsString(Policy);

    // Remove the spaces;
    int k = 0;
    for (uint i = 0; i < TypeString.size(); ++i) {
        char C = TypeString[i];
        if (C == ' ') {
            if (k == 0)
                continue;
            if (i+1 == TypeString.size())
                continue;
            char P = TypeString[k-1];
            char N = TypeString[i+1];
            if (!(IsIdentChar(P) && IsIdentChar(N))
                && !(P == '>' && N == '>'))
                continue;
        }
        TypeString[k++] = C;
    }
    TypeString.resize(k);

    //adjust unsigned
    uint UPos = 0;
    while ((UPos = TypeString.find("unsigned ", UPos)) < TypeString.size()) {
        const int L = sizeof("unsigned ") - 1; // don't include \0
        llvm::StringRef R(&TypeString[UPos + L],
                          TypeString.size() - L);
        if (R.startswith("int") || (R.startswith("long") &&
            !R.startswith("long int") && !R.startswith("long long"))) {
            TypeString.replace(UPos, L, "u");
        }
        UPos++;
    }
    OS << "0x80000000 | " << StrIdx(TypeString);
}

// Generate the data in the data array for the parametters of functions in the vector
template <typename T>
void Generator::GenerateFunctionParameters(const std::vector< T* >& V, const char* TypeName)
{
    if (V.empty())
        return;

    OS << "\n // " << TypeName << ": parameters\n";

    ForEachMethod(V, [&](const clang::CXXMethodDecl *M, int Clone) {
        int argc =  M->getNumParams() - Clone;
        if (HasPrivateSignal(M))
            argc--;
        OS << "    ";
        //Types
        if (std::is_same<T, clang::CXXConstructorDecl>::value)
            OS << "0x80000000 | " << StrIdx("");
        else
            GenerateTypeInfo(getResultType(M));
        OS <<  ",";
        for (int j = 0; j < argc; j++) {
            OS << " ";
            GenerateTypeInfo(M->getParamDecl(j)->getOriginalType());
            OS <<  ",";
        }

        //Names
        for (int j = 0; j < argc; j++) {
            auto P = M->getParamDecl(j);
            if (P->getIdentifier())
                OS << " " << StrIdx(P->getName()) << ",";
            else
                OS << " " << StrIdx("") << ",";
        }
        OS << "\n";
    });
}

// return true if a staticMetaObject is found in the bases
static bool hasStaticMetaObject(clang::QualType T) {
    auto RD = T->getAsCXXRecordDecl();
    if (!RD)
        return false;

    for (auto it = RD->decls_begin(); it != RD->decls_end(); ++it) {
        if (const clang::NamedDecl *Sub = llvm::dyn_cast<const clang::NamedDecl>(*it)) {
            if (Sub->getIdentifier() && Sub->getName() == "staticMetaObject")
                return true;
        }
    }

    if (RD->getNumBases()) {
        return hasStaticMetaObject(RD->bases_begin()->getType());
    }
    return false;
}

template <typename T>
static void PrintTemplateParamName(llvm::raw_ostream &Out, const T *D, bool PrintPack = false)
{
    if (auto II = D->getIdentifier()) {
        Out << II->getName();
    } else {
        Out << "T_" << D->getDepth() << "_" << D->getIndex();
    }
    if (PrintPack && D->isParameterPack()) {
        Out << "...";
    }
}

static void PrintTemplateParameters(llvm::raw_ostream &Out, clang::TemplateParameterList *List,
                                    clang::PrintingPolicy &PrintPolicy)
{
    Out << "template <";
    bool NeedComa = false;
    for (clang::NamedDecl *Param : *List) {
        if (NeedComa) Out << ", ";
        NeedComa = true;
        if (const auto *TTP = llvm::dyn_cast<clang::TemplateTypeParmDecl>(Param)) {
            if (TTP->wasDeclaredWithTypename()) {
                Out << "typename ";
            } else {
                Out << "class ";
            }
            if (TTP->isParameterPack())
                Out << "...";
            PrintTemplateParamName(Out, TTP);
        } else if (const auto *NTTP = llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(Param)) {
            auto Type = NTTP->getType();
            bool Pack = NTTP->isParameterPack();
            if (auto *PET = Type->getAs<clang::PackExpansionType>()) {
                Pack = true;
                Type = PET->getPattern();
            }
            llvm::SmallString<25> Name;
            {
                llvm::raw_svector_ostream OS (Name);
                PrintTemplateParamName(OS, NTTP);
            }
            Type.print(Out, PrintPolicy, (Pack ? "... " : "") + Name);
        } else if (const auto *TTPD = llvm::dyn_cast<clang::TemplateTemplateParmDecl>(Param)) {
            PrintTemplateParameters(Out, TTPD->getTemplateParameters(), PrintPolicy);
            Out << "class ";
            if (TTPD->isParameterPack())
                Out << "... ";
            PrintTemplateParamName(Out, TTPD);
        }
    }
    Out << "> ";
}

Generator::Generator(const ClassDef* CDef, llvm::raw_ostream& OS, clang::ASTContext& Ctx, MocNg* Moc) :
    CDef(CDef), OS(OS), Ctx(Ctx), PrintPolicy(Ctx.getPrintingPolicy()), Moc(Moc)
{
    PrintPolicy.SuppressTagKeyword = true;
    PrintPolicy.SuppressUnwrittenScope = true;
    PrintPolicy.AnonymousTagLocations = false;

    {
        llvm::raw_string_ostream QualNameS(QualName);
        CDef->Record->printQualifiedName(QualNameS, PrintPolicy);
    }

    if (CDef->Record->getNumBases()) {
        auto Base = CDef->Record->bases_begin()->getTypeSourceInfo();
        // We need to try to get the type name as written. Because we don't want qualified name if
        // it was not qualified.  For example:
        //   namespace X { struct F; namespace Y { struct X; struct G : F { Q_OBJECT };  } }
        //   We don't want to use X::F  because X would be the struct and not the namespace
        BaseName = TypeStringFromSourceRange(Base->getTypeLoc().getSourceRange(),
                                             Ctx.getSourceManager());
        if (BaseName.empty()) {
            BaseName = Base->getType().getAsString(PrintPolicy);
        }
        BaseHasStaticMetaObject = hasStaticMetaObject(Base->getType());
    }

    MethodCount = CountMethod(CDef->Signals) + CountMethod(CDef->Slots) + CountMethod(CDef->Methods) + CDef->PrivateSlotCount;

    if (auto Tpl = CDef->Record->getDescribedClassTemplate()) {
        llvm::raw_string_ostream TemplatePrefixStream(TemplatePrefix);
        PrintTemplateParameters(TemplatePrefixStream, Tpl->getTemplateParameters(), PrintPolicy);
        llvm::raw_string_ostream TemplatePostfixStream(QualName);
        bool NeedComa = false;
        TemplatePostfixStream << "<";
        for (clang::NamedDecl *Param : *Tpl->getTemplateParameters()) {
            if (NeedComa) TemplatePostfixStream << ", ";
            NeedComa = true;
            if (const auto *TTP = llvm::dyn_cast<clang::TemplateTypeParmDecl>(Param)) {
                PrintTemplateParamName(TemplatePostfixStream, TTP, true);
            } else if (const auto *NTTP = llvm::dyn_cast<clang::NonTypeTemplateParmDecl>(Param)) {
                PrintTemplateParamName(TemplatePostfixStream, NTTP, true);
            } else if (const auto *TTPD = llvm::dyn_cast<clang::TemplateTemplateParmDecl>(Param)) {
                PrintTemplateParamName(TemplatePostfixStream, TTPD, true);
            }
        }
        TemplatePostfixStream << ">";
    }
}

void Generator::GenerateCode()
{
    // Build the data array

    std::string QualifiedClassNameIdentifier = QualName;
    if (CDef->Record->getDescribedClassTemplate()) {
        auto pos = QualifiedClassNameIdentifier.find('<');
        QualifiedClassNameIdentifier.resize(std::min(QualifiedClassNameIdentifier.size(), pos));
    }
    std::replace(QualifiedClassNameIdentifier.begin(), QualifiedClassNameIdentifier.end(), ':', '_');

    int Index = MetaObjectPrivateFieldCount;

    //Helper function which adds N to the index and return a value suitable to be placed in the array.
    auto I = [&](int N) {
        if (!N) return 0;
        int R = Index;
        Index += N;
        return R;
    };

    OS << "\nstatic const uint qt_meta_data_" << QualifiedClassNameIdentifier << "[] = {\n"
          "    " << OutputRevision << ", // revision\n"
          "    " << StrIdx(QualName) << ", // classname\n"
          "    " << CDef->ClassInfo.size() << ", " << I(CDef->ClassInfo.size() * 2) << ", //classinfo\n";

    OS << "    " << MethodCount << ", " << I(MethodCount * 5) << ", // methods \n";

    if (CDef->RevisionMethodCount)
        Index += MethodCount;

    int ParamsIndex = Index;
    int TotalParameterCount = AggregateParameterCount(CDef->Signals) + AggregateParameterCount(CDef->Slots)
                            + AggregateParameterCount(CDef->Methods) + AggregateParameterCount(CDef->Constructors);
    for (const PrivateSlotDef &P : CDef->PrivateSlots)
        TotalParameterCount += 1 + P.Args.size() * (1 + P.NumDefault) - (P.NumDefault * (P.NumDefault + 1) / 2);
    Index += TotalParameterCount * 2 // type and parameter names
           - MethodCount - CountMethod(CDef->Constructors);  // return parameter don't have names

    OS << "    " << CDef->Properties.size() << ", " << I(CDef->Properties.size() * 3) << ", // properties \n";

    if (CDef->NotifyCount)
        Index += CDef->Properties.size();
    if (CDef->RevisionPropertyCount)
        Index += CDef->Properties.size();

    OS << "    " << CDef->Enums.size() << ", " << I(CDef->Enums.size() * 4) << ", // enums \n";
    int EnumIndex = Index;
    for (auto e : CDef->Enums)
        for (auto it = std::get<0>(e)->enumerator_begin() ; it != std::get<0>(e)->enumerator_end(); ++it)
            Index += 2;

    int ConstructorCount = CountMethod(CDef->Constructors);
    OS << "    " << ConstructorCount << ", " << I(ConstructorCount * 5) << ", // constructors \n";

    int flags = 0;
    if (CDef->HasQGadget) {
        // Ideally, all the classes could have that flag. But this broke classes generated
        // by qdbusxml2cpp which generate code that require that we call qt_metacall for properties
        flags |= PropertyAccessInStaticMetaCall;
    }
    OS << "    " << flags << ", // flags \n";

    OS << "    " << CountMethod(CDef->Signals) << ", // signalCount \n";

    if (CDef->ClassInfo.size()) {
        OS << "\n  // classinfo: key, value\n";
        for (const auto &I : CDef->ClassInfo)
            OS << "    " << StrIdx(I.first) << ", " << StrIdx(I.second) << ",\n";
    }


    GenerateFunctions(CDef->Signals, "signals", MethodSignal, ParamsIndex);
    GenerateFunctions(CDef->Slots, "slots", MethodSlot, ParamsIndex);
    for (const PrivateSlotDef &P : CDef->PrivateSlots) {
        for (int Clone = 0; Clone <= P.NumDefault; ++Clone) {
            int argc = (P.Args.size() - Clone);
            OS << "    " << StrIdx(P.Name) << ", " << argc << ", " << ParamsIndex << ", 0, 0x";
            unsigned int Flag = AccessPrivate | MethodSlot;
            if (Clone) Flag |= MethodCloned;
            OS.write_hex(Flag) << ",\n";
            ParamsIndex += 1 + argc * 2;
        }
    }
    GenerateFunctions(CDef->Methods, "methods", MethodMethod, ParamsIndex);

    if (CDef->RevisionMethodCount) {
        auto GenerateRevision = [&](const clang::CXXMethodDecl *M, int Clone) {
            llvm::StringRef SubStr = "0";
            for (auto attr_it = M->specific_attr_begin<clang::AnnotateAttr>();
                    attr_it != M->specific_attr_end<clang::AnnotateAttr>();
                    ++attr_it) {
                const clang::AnnotateAttr *A = *attr_it;
                if (A->getAnnotation().startswith("qt_revision:")) {
                    SubStr = A->getAnnotation().substr(sizeof("qt_revision:")-1);
                }
            }
            OS << " " << SubStr << ",";
        };
        OS << "\n // method revisions\n    ";
        ForEachMethod(CDef->Signals, GenerateRevision);
        OS << "\n    ";
        ForEachMethod(CDef->Slots, GenerateRevision);
        //OS << "\n    ";
        for (const PrivateSlotDef &P : CDef->PrivateSlots) {
            for (int Clone = 0; Clone <= P.NumDefault; ++Clone)
                OS << " 0,    ";
        }
        OS << "\n    ";
        ForEachMethod(CDef->Methods, GenerateRevision);
        OS << "\n";
    }



    GenerateFunctionParameters(CDef->Signals, "signals");
    GenerateFunctionParameters(CDef->Slots, "slots");
    for (const PrivateSlotDef &P : CDef->PrivateSlots) {
        for (int Clone = 0; Clone <= P.NumDefault; ++Clone) {
            int argc = (P.Args.size() - Clone);
            OS << "    ";
            if (P.ReturnType == "void") OS << "QMetaType::Void";
            else OS << "0x80000000 | " << StrIdx(P.ReturnType);
            for (int j = 0; j < argc; j++) {
                OS << ", ";
                if (P.Args[j] == "void") OS << "QMetaType::Void";
                else OS << "0x80000000 | " << StrIdx(P.Args[j]);
            }
            //Names
            for (int j = 0; j < argc; j++) {
                    OS << ", " << StrIdx("");
            }
            OS << ",\n";
        }
    }
    GenerateFunctionParameters(CDef->Methods, "methods");
    GenerateFunctionParameters(CDef->Constructors, "constructors");

    GenerateProperties();
    GenerateEnums(EnumIndex);

    GenerateFunctions(CDef->Constructors, "constructors", MethodConstructor, ParamsIndex);

    OS << "\n    0    // eod\n};\n";



    // StringArray;

    int TotalLen = 1;
    for (const auto &S : Strings)
        TotalLen += S.size() + 1;


    OS << "struct qt_meta_stringdata_" << QualifiedClassNameIdentifier << "_t {\n"
          "    QByteArrayData data[" << Strings.size() << "];\n"
          "    char stringdata[" << TotalLen << "];\n"
          "};\n"
          "#define QT_MOC_LITERAL(idx, ofs, len) \\\n"
          "    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \\\n"
          "    qptrdiff(offsetof(qt_meta_stringdata_"<<  QualifiedClassNameIdentifier << "_t, stringdata) + ofs \\\n"
          "        - idx * sizeof(QByteArrayData)) \\\n"
          "    )\n"
          "static const qt_meta_stringdata_"<<  QualifiedClassNameIdentifier << "_t qt_meta_stringdata_"<<  QualifiedClassNameIdentifier << " = {\n"
          "    {\n";
    int Idx = 0;
    int LitteralIndex = 0;
    for (const auto &S : Strings) {
        if (LitteralIndex)
            OS << ",\n";
        OS << "QT_MOC_LITERAL("<< (LitteralIndex++) << ", " << Idx << ", " << S.size() << ")";
        Idx += S.size() + 1;
    }
    OS << "\n    },\n    \"";
    int Col = 0;
    for (const auto &S : Strings) {
        if (Col && Col + S.size() >= 72) {
            OS << "\"\n    \"";
            Col = 0;
        } else if (S.size() && ((S[0] >= '0' && S[0] <= '9') || S[0] == '?')) {
            OS << "\"\"";
            Col += 2;
        }

        // Can't use write_escaped because of the trigraph
        for (unsigned i = 0, e = S.size(); i != e; ++i) {
            unsigned char c = S[i];
            switch (c) {
            case '\\': OS << '\\' << '\\'; break;
            case '\t': OS << '\\' << 't'; break;
            case '\n': OS << '\\' << 'n'; break;
            case '"': OS << '\\' << '"'; break;
            case '?':
                if (i != 0 && S[i-1] == '?') {
                    OS << '\\';
                    Col++;
                }
                OS << '?';
                break;
            default:
                if (std::isprint(c)) {
                    OS << c;
                    break;
                }
                // Use 3 character octal sequence
                OS << '\\' << char('0' + ((c >> 6) & 7)) << char('0' + ((c >> 3) & 7)) << char('0' + ((c >> 0) & 7));
                Col += 3;
            }
        }

        OS << "\\0";
        Col += 2 + S.size();
    }
    OS << "\"\n};\n"
          "#undef QT_MOC_LITERAL\n";

    if (!CDef->Extra.empty()) {
        OS << "static const QMetaObject * const qt_meta_extradata_" << QualifiedClassNameIdentifier << "[] = {\n" ;
        for (clang::CXXRecordDecl *E : CDef->Extra)
            //TODO: Check that extra is a QObject
            OS << "    &" << E->getQualifiedNameAsString() << "::staticMetaObject,\n";

        OS << "    0\n};\n";

    }

    if (IsQtNamespace) {
        OS << "\nconst QMetaObject QObject::staticQtMetaObject = {\n"
              "    { 0, qt_meta_stringdata_Qt.data, qt_meta_data_Qt, 0, 0, 0 }\n};\n";
        return;
    }

    OS << "\n" << TemplatePrefix << "const QMetaObject " << QualName << "::staticMetaObject = {\n"
          "    { ";
    if (BaseName.empty() || (CDef->HasQGadget && !BaseHasStaticMetaObject)) OS << "0";
    else OS << "&" << BaseName << "::staticMetaObject";

    OS << ", qt_meta_stringdata_"<< QualifiedClassNameIdentifier <<".data,\n"
          "      qt_meta_data_" << QualifiedClassNameIdentifier << ", ";

    bool HasStaticMetaCall = CDef->HasQObject || !CDef->Methods.empty() || !CDef->Properties.empty() || !CDef->Constructors.empty();
    if (HasStaticMetaCall) OS << "qt_static_metacall, ";
    else OS << "0, ";

    if (!CDef->Extra.empty()) OS << "qt_meta_extradata_" << QualifiedClassNameIdentifier << ", ";
    else OS << "0, ";
    OS << "0}\n};\n";

    if (CDef->HasQObject) {
        OS << TemplatePrefix << "const QMetaObject *" << QualName << "::metaObject() const\n{\n"
              "    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;\n}\n";


        OS <<  TemplatePrefix << "void *" << QualName << "::qt_metacast(const char *_clname)\n{\n"
              "    if (!_clname) return 0;\n"
              "    if (!strcmp(_clname, qt_meta_stringdata_" << QualifiedClassNameIdentifier << ".stringdata))\n"
              "        return static_cast<void*>(this);\n";

        if (CDef->Record->getNumBases() > 1) {
            for (auto BaseIt = CDef->Record->bases_begin()+1; BaseIt != CDef->Record->bases_end(); ++BaseIt) {
                if (BaseIt->getAccessSpecifier() == clang::AS_private)
                    continue;
                auto B = BaseIt->getType().getAsString(PrintPolicy);
                OS << "    if (!qstrcmp(_clname, \"" << B << "\"))\n"
                      "        return static_cast< " << B << "*>(this);\n";
            }
        }

        for (const auto &Itrf : CDef->Interfaces) {
            OS << "    if (!qstrcmp(_clname, qobject_interface_iid< " << Itrf << " *>()))\n"
                  "        return static_cast< " << Itrf << "  *>(this);\n";
        }

        if (BaseName.empty()) OS << "    return 0;\n}\n";
        else OS << "    return "<< BaseName <<"::qt_metacast(_clname);\n"
                   "}\n";

        GenerateMetaCall();
        GenerateStaticMetaCall();

        int SigIdx = 0;
        for (const clang::CXXMethodDecl *MD : CDef->Signals) {
            GenerateSignal(MD, SigIdx);
            SigIdx += 1 + MD->getNumParams() - MD->getMinRequiredArguments();
        }
    } else if (HasStaticMetaCall) {
        GenerateStaticMetaCall();
    }

    if (!CDef->Plugin.IID.empty()) {
        OS << "\nQT_PLUGIN_METADATA_SECTION const uint qt_section_alignment_dummy = 42;\n"
              "#ifdef QT_NO_DEBUG\n";
        GeneratePluginMetaData(false);
        OS << "#else\n";
        GeneratePluginMetaData(true);
        OS << "#endif\n";

        OS << "QT_MOC_EXPORT_PLUGIN(" << QualName << ", " << CDef->Record->getName() << ")\n\n";
    }
}

void Generator::GenerateMetaCall()
{
    OS << "\n" << TemplatePrefix << "int " << QualName << "::qt_metacall(QMetaObject::Call _c, int _id, void **_a)\n{\n";
    if (!BaseName.empty()) {
        OS << "    _id = " << BaseName << "::qt_metacall(_c, _id, _a);\n"
              "    if (_id < 0)\n"
              "        return _id;\n";
    }

    if (MethodCount) {
        OS << "    if (_c == QMetaObject::InvokeMetaMethod || _c == QMetaObject::RegisterMethodArgumentMetaType) {\n"
              "        if (_id < " << MethodCount << ")\n"
              "            qt_static_metacall(this, _c, _id, _a);\n"
              "        _id -= " << MethodCount << ";\n"
              "    }\n";
    }

    if (CDef->Properties.size()) {
        bool needDesignable = false;
        bool needScriptable = false;
        bool needStored = false;
        bool needEditable = false;
        bool needUser = false;
        for (const PropertyDef &p : CDef->Properties) {
            auto IsFunction = [](const std::string &S) { return S.size() && S[S.size()-1] == ')'; };
            needDesignable |= IsFunction(p.designable);
            needScriptable |= IsFunction(p.scriptable);
            needStored |= IsFunction(p.stored);
            needEditable |= IsFunction(p.editable);
            needUser |= IsFunction(p.user);
        }

        OS << "    ";
        if (MethodCount)
            OS << "else ";

        OS << " if (_c == QMetaObject::ReadProperty || _c == QMetaObject::WriteProperty\n"
              "            || _c == QMetaObject::ResetProperty || _c == QMetaObject::RegisterPropertyMetaType) {\n"
              "        if (_id < " << CDef->Properties.size() <<  ")\n"
              "            qt_static_metacall(this, _c, _id, _a);\n"
              "        _id -= " << CDef->Properties.size() <<  ";\n"
              "    }\n";

        // Helper for all the QMetaObject::QueryProperty*
        typedef std::string (PropertyDef::*Accessor);
        auto HandleQueryPropertyAction = [&](bool Need, const char *Action, Accessor A) {
            OS << " else ";
            OS << "if (_c == QMetaObject::" << Action << ") {\n";
            if (Need) {
                OS << "        switch (_id) {\n";
                int I = 0;
                for (const PropertyDef &p : CDef->Properties) {
                    OS << "        case " << (I++) <<": ";
                    const std::string &S = (p.*A);
                    if (!S.empty() && S[S.size()-1] == ')')
                        OS << "*reinterpret_cast<bool*>(_a[0]) = " << S << "; ";
                    OS << "break;\n";
                }
                OS << "        default: break;\n";
                OS << "        }";
            }
            OS << "        _id -= " << CDef->Properties.size() << ";\n    }";
        };

        HandleQueryPropertyAction(needDesignable, "QueryPropertyDesignable", &PropertyDef::designable);
        HandleQueryPropertyAction(needScriptable, "QueryPropertyScriptable", &PropertyDef::scriptable);
        HandleQueryPropertyAction(needScriptable, "QueryPropertyStored", &PropertyDef::stored);
        HandleQueryPropertyAction(needEditable, "QueryPropertyEditable", &PropertyDef::editable);
        HandleQueryPropertyAction(needUser, "QueryPropertyUser", &PropertyDef::user);
        HandleQueryPropertyAction(needUser, "QueryPropertyUser", &PropertyDef::user);

    }
    OS << "\n    return _id;"
          "}\n";
}

void Generator::GenerateStaticMetaCall()
{
    llvm::StringRef ClassName = CDef->Record->getName();
    OS << "\n" <<  TemplatePrefix << "void " << QualName << "::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)\n{\n    ";
    bool NeedElse = false;

    if (!CDef->Constructors.empty()) {
        OS << "    if (_c == QMetaObject::CreateInstance) {\n"
              "        switch (_id) {\n";

        llvm::StringRef resultType = CDef->HasQObject ? "QObject" : "void";

        int CtorIndex = 0;
        ForEachMethod(CDef->Constructors, [&](const clang::CXXConstructorDecl *MD, int C) {
            OS << "        case " << (CtorIndex++) << ": { " << resultType << " *_r = new " << ClassName << "(";

            for (uint j = 0 ; j < MD->getNumParams() - C; ++j) {
                if (j) OS << ",";
                if (j == MD->getNumParams() - 1 && HasPrivateSignal(MD))
                    OS << "QPrivateSignal()";
                else
                    OS << "*reinterpret_cast< " << Ctx.getPointerType(MD->getParamDecl(j)->getType().getNonReferenceType().getUnqualifiedType()).getAsString(PrintPolicy) << " >(_a[" << (j+1) << "])";
            }
            OS << ");\n            if (_a[0]) *reinterpret_cast<" << resultType << "**>(_a[0]) = _r; } break;\n";
        });
        OS << "        default: break;\n"
              "        }\n"
              "    }";

        NeedElse = true;
    }

    if (MethodCount) {
        if(NeedElse) OS << " else ";
        NeedElse = true;
        OS << "if (_c == QMetaObject::InvokeMetaMethod) {\n";
        if (CDef->HasQObject) {
//          OS << "        Q_ASSERT(staticMetaObject.cast(_o));\n";
            OS << "        " << ClassName <<" *_t = static_cast<" << ClassName << " *>(_o);\n";
        } else {
            OS << "        " << ClassName <<" *_t = reinterpret_cast<" << ClassName << " *>(_o);\n";
        }

        OS << "        switch(_id) {\n" ;
        int MethodIndex = 0;
        auto GenerateInvokeMethod = [&](const clang::CXXMethodDecl *MD, int Clone) {
            if (!MD->getIdentifier())
                return;

            OS << "        case " << MethodIndex << ": ";
            MethodIndex++;

            if (WorkaroundTests(ClassName, MD, OS))
                return;

            auto ReturnType = getResultType(MD);
            // Original moc don't support reference as return type: see  Moc::parseFunction
            bool IsVoid = ReturnType->isVoidType() || ReturnType->isReferenceType();
            // If we have a decltype, we need to use auto type deduction
            bool AutoType = llvm::isa<clang::DecltypeType>(ReturnType) || llvm::isa<clang::AutoType>(ReturnType);
            if (!IsVoid) {
                OS << "{ ";
                if (AutoType)
                    OS << "auto";
                else
                    ReturnType.getUnqualifiedType().print(OS, PrintPolicy);
                OS << " _r =  ";
            }

            OS << "_t->" << MD->getName() << "(";

            for (uint j = 0 ; j < MD->getNumParams() - Clone; ++j) {
                if (j) OS << ",";
                if (j == MD->getNumParams() - 1 && HasPrivateSignal(MD))
                    OS << "QPrivateSignal()";
                else
                    OS << "*reinterpret_cast< " << Ctx.getPointerType(MD->getParamDecl(j)->getType().getNonReferenceType().getUnqualifiedType()).getAsString(PrintPolicy) << " >(_a[" << (j+1) << "])";
            }
            OS << ");";
            if (!IsVoid) {
                OS << "\n            if (_a[0]) *reinterpret_cast< ";
                if (AutoType)
                    OS << "decltype(&_r)";
                else
                    Ctx.getPointerType(ReturnType.getNonReferenceType().getUnqualifiedType()).print(OS, PrintPolicy);
                OS << " >(_a[0]) = _r; }";
            }
            OS <<  " break;\n";
        };
        ForEachMethod(CDef->Signals, GenerateInvokeMethod);
        ForEachMethod(CDef->Slots, GenerateInvokeMethod);
        for (const PrivateSlotDef &P : CDef->PrivateSlots) {
            for (int Clone = 0; Clone <= P.NumDefault; ++Clone) {
                OS << "        case " << MethodIndex << ": ";
                // Original moc don't support reference as return type: see  Moc::parseFunction
                bool IsVoid = P.ReturnType == "void" || P.ReturnType.empty() || P.ReturnType.back() == '&';
                if (!IsVoid)
                    OS << "{ " << P.ReturnType << " _r =  ";
                OS << "_t->" << P.InPrivateClass << "->" << P.Name << "(";
                for (uint j = 0 ; j < P.Args.size() - Clone; ++j) {
                    if (j) OS << ",";
                    OS << "*reinterpret_cast< " << P.Args[j] << " *>(_a[" << (j+1) << "])";
                }
                OS << ");";
                if (!IsVoid) {
                    OS << "\n            if (_a[0]) *reinterpret_cast< " << P.ReturnType << " *>(_a[0]) = _r; }";
                }
                OS <<  " break;\n";
                MethodIndex++;
            }
        }
        ForEachMethod(CDef->Methods, GenerateInvokeMethod);
        OS << "        default: break;\n"
              "        }\n"
              "    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {\n"
              "        switch ((_id << 16) | *reinterpret_cast<int*>(_a[1])) {\n"
              "        default: *reinterpret_cast<int*>(_a[0]) = -1; break;\n";


        MethodIndex = 0;
        auto GenerateRegisterMethodArguments = [&](const clang::CXXMethodDecl *MD, int Clone) {
            if (!MD->getIdentifier()) {
                MethodIndex++;
                return;
            }
          //  RegisterT(getResultType(MD), (MethodIndex << 16));
            int argc = MD->getNumParams() - Clone - (HasPrivateSignal(MD)?1:0);
            for (int j = 0 ; j < argc ; ++j) {
                auto Type = MD->getParamDecl(j)->getType();
                if (!Moc->ShouldRegisterMetaType(Type))
                    break;
                OS << "       case 0x";
                OS.write_hex((MethodIndex << 16) | j);
                OS << ": *reinterpret_cast<int*>(_a[0]) = ";
                OS <<  "QtPrivate::QMetaTypeIdHelper< " << Type.getNonReferenceType().getUnqualifiedType().getAsString(PrintPolicy)
                    << " >::qt_metatype_id(); break;\n";
            }
            MethodIndex++;
        };

        ForEachMethod(CDef->Signals, GenerateRegisterMethodArguments);
        ForEachMethod(CDef->Slots, GenerateRegisterMethodArguments);
        MethodIndex += CDef->PrivateSlotCount; // TODO: we should also register these types.
        ForEachMethod(CDef->Methods, GenerateRegisterMethodArguments);

        OS << "        }\n    }";

    }
    if (!CDef->Signals.empty()) {

        int MethodIndex = 0;
        OS << " else if (_c == QMetaObject::IndexOfMethod) {\n"
              "        int *result = reinterpret_cast<int *>(_a[0]);\n"
              "        void **func = reinterpret_cast<void **>(_a[1]);\n";

        for (const clang::CXXMethodDecl *MD: CDef->Signals) {
            int Idx = MethodIndex;
            MethodIndex += MD->getNumParams() - MD->getMinRequiredArguments() + 1;
            if (MD->isStatic() || !MD->getIdentifier())
                continue;
            OS << "        {\n"
                  "            typedef " << getResultType(MD).getAsString(PrintPolicy) << " (" << ClassName << "::*_t)(";
            for (uint j = 0 ; j < MD->getNumParams(); ++j) {
                if (j) OS << ",";
                OS << MD->getParamDecl(j)->getType().getAsString(PrintPolicy);
            }
            if (MD->isConst()) OS << ") const;\n";
            else OS << ");\n";

            OS << "            if (*reinterpret_cast<_t *>(func) == static_cast<_t>(&"<< ClassName <<"::"<< MD->getName() <<")) {\n"
                  "                *result = " << Idx << ";\n"
                  "            }\n"
                  "        }\n";
        }
        OS << "    }";
    }

    if (!CDef->Properties.empty()) {
        if(NeedElse) OS << " else ";
        NeedElse = true;

        bool needGet = false;
        //bool needTempVarForGet = false;
        bool needSet = false;
        bool needReset = false;
        for (const PropertyDef &p : CDef->Properties) {
            needGet |= !p.read.empty() || !p.member.empty();
            /*if (!p.read.empty())
                needTempVarForGet |= (p.gspec != PropertyDef::PointerSpec
                && p.gspec != PropertyDef::ReferenceSpec);*/
            needSet |= !p.write.empty() || (!p.member.empty() && !p.constant);
            needReset |= !p.reset.empty();
        }


        // Generate the code for QMetaObject::'Action'.  calls 'Functor' to generate the  code for
        // each properties
        auto HandlePropertyAction = [&](bool Need, const char *Action,
                                        const std::function<void(const PropertyDef &)> &Functor) {
            OS << "if (_c == QMetaObject::" << Action << ") {\n";
            if (Need) {
                if (CDef->HasQObject) {
                    // OS << "        Q_ASSERT(staticMetaObject.cast(_o));\n";
                    OS << "        " << ClassName <<" *_t = static_cast<" << ClassName << " *>(_o);\n";
                } else {
                    OS << "        " << ClassName <<" *_t = reinterpret_cast<" << ClassName << " *>(_o);\n";
                }
                OS << "        switch (_id) {\n";
                int I = 0;
                for (const PropertyDef &p : CDef->Properties) {
                    OS << "        case " << (I++) <<": ";
                    Functor(p);
                    OS << "break;\n";
                }
                OS << "        default: break;\n";
                OS << "        }";
            }
            OS << "        _id -= " << CDef->Properties.size() << ";\n    }";
        };

        HandlePropertyAction(needGet, "ReadProperty", [&](const PropertyDef &p) {
            if (p.read.empty() && p.member.empty())
                return;

            std::string Prefix = "_t->";
            if (p.inPrivateClass.size()) {
                Prefix += p.inPrivateClass;
                Prefix += "->";
            }

            //FIXME: enums case
            if (p.PointerHack) {
              OS << "_a[0] = const_cast<void*>(static_cast<const void*>(" << Prefix << p.read << "())); ";
            } else {
              OS << "*reinterpret_cast< " << p.type << "*>(_a[0]) = " << Prefix;
              if (!p.read.empty())
                  OS << p.read << "(); ";
              else
                  OS << p.member << "; ";
            }
        });
        OS << " else ";
        HandlePropertyAction(needSet, "WriteProperty", [&](const PropertyDef &p) {
            if (p.constant)
                return;

            std::string Prefix = "_t->";
            if (p.inPrivateClass.size()) {
                Prefix += p.inPrivateClass;
                Prefix += "->";
            }

            if (!p.write.empty()) {
                OS << Prefix << p.write << "(*reinterpret_cast< " << p.type << "*>(_a[0])); ";
            } else if (!p.member.empty()) {
                std::string M = Prefix + p.member;
                std::string A = "*reinterpret_cast< " + p.type + "*>(_a[0])";
                if (p.notify.notifyId >= 0) {
                    OS << "\n"
                          "            if (" << M << " != " << A << ") {\n"
                          "                " << M << " = " << A << ";\n"
                          "                Q_EMIT _t->" << p.notify.Str << "(";
                    if (p.notify.MD->getMinRequiredArguments() > 0)
                        OS << M;
                    OS << ");\n"
                          "            } ";
                } else {
                    OS << M << " = " << A << "; ";
                }
            }
        });
        OS << " else ";
        HandlePropertyAction(needReset, "ResetProperty", [&](const PropertyDef &p) {
            if (p.reset.empty() || p.reset[p.reset.size()-1] != ')')
                return;
            std::string Prefix = "_t->";
            if (p.inPrivateClass.size()) {
                Prefix += p.inPrivateClass;
                Prefix += "->";
            }
            OS << Prefix << p.reset << "; ";
        });

        OS << "if (_c == QMetaObject::RegisterPropertyMetaType) {\n"
              "        switch (_id) {\n"
              "        default: *reinterpret_cast<int*>(_a[0]) = -1; break;\n";

        //FIXME: optimize (group same properties, and don't generate for builtin
        int Idx = 0;
        for (const PropertyDef &P: CDef->Properties) {
            int OldIdx = Idx++;
            if (P.PossiblyForwardDeclared) {
                const auto &MTS = Moc->registered_meta_type;
                if (!std::any_of(MTS.begin(), MTS.end(), [&](const clang::Type*T){
                    return clang::QualType(T,0).getAsString(PrintPolicy) == P.type;
                } ))
                    continue;
            }
            OS << "        case " << OldIdx << ": *reinterpret_cast<int*>(_a[0]) = QtPrivate::QMetaTypeIdHelper<"
               << P.type << " >::qt_metatype_id(); break;\n";
        }
        OS << "        }\n";
        OS << "    }\n";
    }


    OS << "\n    Q_UNUSED(_o); Q_UNUSED(_id); Q_UNUSED(_c); Q_UNUSED(_a);";
    OS << "\n}\n";
}

void Generator::GenerateSignal(const clang::CXXMethodDecl *MD, int Idx)
{
    if (MD->isPure())
        return;

    clang::QualType ReturnType = getResultType(MD);
    // getResultType will desugar.  So if we are still a decltype, it probably means this is type
    // dependant and therefore must be a literal decltype which we need to have in the proper scope
    bool TrailingReturn = llvm::isa<clang::DecltypeType>(ReturnType);

    OS << "\n// SIGNAL " << Idx << "\n" << TemplatePrefix;
    if (TrailingReturn)
        OS << "auto ";
    else
        OS << ReturnType.getAsString(PrintPolicy) << " ";
    OS << QualName << "::" << MD->getName() + "(";
    for (uint j = 0 ; j < MD->getNumParams(); ++j) {
        if (j) OS << ",";
        OS << MD->getParamDecl(j)->getType().getAsString(PrintPolicy);
        if (!(j == MD->getNumParams() - 1 && HasPrivateSignal(MD)))
            OS << " _t" << (j+1);;
    }
    OS << ")";
    std::string This = "this";
    if (MD->isConst()) {
        OS << " const";
        This = "const_cast< " + CDef->Record->getNameAsString()  + " *>(this)";
    }
    if (TrailingReturn)
        OS << " -> " << ReturnType.getAsString(PrintPolicy);
    OS << "\n{\n";
    bool IsVoid = ReturnType->isVoidType();
    unsigned int NumParam = MD->getNumParams();
    if (HasPrivateSignal(MD)) NumParam--;
    if (IsVoid && NumParam == 0) {
        OS << "    QMetaObject::activate(" << This << ", &staticMetaObject, " << Idx << ", 0);\n";
    } else {
        std::string T = ReturnType.getNonReferenceType().getUnqualifiedType().getAsString(PrintPolicy);
        if (ReturnType->isPointerType()) {
            OS << "    " << ReturnType.getAsString(PrintPolicy) << " _t0 = 0;\n";
        } else if (!IsVoid) {
            OS << "    " << T << " _t0 = " << T << "();\n";
        }
        OS << "    void *_a[] = { ";
        if (IsVoid) OS << "0";
        else OS << "&_t0";


        for (uint j = 0 ; j < NumParam; ++j) {
            if (MD->getParamDecl(j)->getType().isVolatileQualified())
                OS << ", const_cast<void*>(reinterpret_cast<const volatile void*>(&_t" << (j+1) << "))";
            else
                OS << ", const_cast<void*>(reinterpret_cast<const void*>(&_t" << (j+1) << "))";
        }

        OS << " };\n"
              "    QMetaObject::activate(" << This << ", &staticMetaObject, " << Idx << ", _a);\n";

        if (!IsVoid)
            OS << "    return _t0;\n";
    }
    OS <<"}\n";
}

// Generate the data in the data array for the properties
void Generator::GenerateProperties()
{
    if (CDef->Properties.empty())
        return;

    OS << "\n // properties: name, type, flags\n";

    for (const PropertyDef &p : CDef->Properties) {
        unsigned int flags = Invalid;
        if (p.isEnum)
            flags |= EnumOrFlag;
        if (!p.member.empty() && !p.constant)
            flags |= Writable;
        if (!p.read.empty() || !p.member.empty())
            flags |= Readable;
        if (!p.write.empty()) {
            flags |= Writable;

            llvm::StringRef W(p.write);
            if (W.startswith("set") && W[3] == char(toupper(p.name[0])) &&  W.substr(4) == &p.name[1])
                flags |= StdCppSet;
        }
        if (!p.reset.empty())
            flags |= Resettable;
        if (p.designable.empty())
            flags |= ResolveDesignable;
        else if (p.designable != "false")
            flags |= Designable;
        if (p.scriptable.empty())
            flags |= ResolveScriptable;
        else if (p.scriptable != "false")
            flags |= Scriptable;
        if (p.stored.empty())
            flags |= ResolveStored;
        else if (p.stored != "false")
            flags |= Stored;
        if (p.editable.empty())
            flags |= ResolveEditable;
        else if (p.editable != "false")
            flags |= Editable;
        if (p.user.empty())
            flags |= ResolveUser;
        else if (p.user != "false")
            flags |= User;
        if (p.notify.notifyId != -1)
            flags |= Notify;
        if (p.revision > 0)
            flags |= Revisioned;
        if (p.constant)
            flags |= Constant;
        if (p.final)
            flags |= Final;
        OS << "    " << StrIdx(p.name) << ", 0x80000000 | " << StrIdx(p.type) << ", 0x";
        OS.write_hex(flags) << ", // " << p.name << "\n";
    }

    if(CDef->NotifyCount) {
         OS << "\n // properties: notify_signal_id\n";
         for (const PropertyDef &P : CDef->Properties) {
             OS << "    " << std::max(0, P.notify.notifyId) << ",\n";
         }
     }

     if(CDef->RevisionPropertyCount) {
         OS << "\n // properties: revision\n";
         for (const PropertyDef &P : CDef->Properties) {
             OS << "    " << P.revision << ",\n";
         }
     }
}

// Generate the data in the data array for the enum.
void Generator::GenerateEnums(int EnumIndex)
{
    if (CDef->Enums.empty())
        return;

    OS << "\n  // enums: name, flags, count, data\n";

    for (auto e : CDef->Enums) {
        int Count = 0;
        for (auto it = std::get<0>(e)->enumerator_begin() ; it != std::get<0>(e)->enumerator_end(); ++it)
            Count++;
        OS << "    " << StrIdx(std::get<1>(e)) << ", " << std::get<2>(e) << ", " << Count << ", " << EnumIndex << ",\n";
        EnumIndex += Count*2;
    }

    OS << "\n  // enums data: key, valus\n";
    for (auto e : CDef->Enums) {
        for (auto it = std::get<0>(e)->enumerator_begin() ; it != std::get<0>(e)->enumerator_end(); ++it) {
            clang::EnumConstantDecl *E = *it;
            OS << "    " << StrIdx(E->getName()) << ", uint(" << QualName << "::";
            if (std::get<0>(e)->isScoped())
                OS << std::get<0>(e)->getName() << "::";
            OS << E->getName() <<"),\n";
        }
    }
}

// Returns the index of a string in the string data.
// Register the string if it is not yet registered.
int Generator::StrIdx(llvm::StringRef Str)
{
    std::string S = Str;
    auto It = std::find(Strings.begin(), Strings.end(), S);
    if (It != Strings.end())
        return It - Strings.begin();
    Strings.push_back(std::move(S));
    return Strings.size() - 1;
}

void Generator::GeneratePluginMetaData(bool Debug)
{
    QBJS::Value Data;
    Data.T = QBJS::Object;
    Data.Props["IID"] = CDef->Plugin.IID;
    Data.Props["className"] = CDef->Record->getNameAsString();
    Data.Props["version"] = double(QT_VERSION);
    Data.Props["MetaData"] = CDef->Plugin.MetaData;
    Data.Props["debug"] = Debug;
    for (const auto &It : MetaData) {
        QBJS::Value &Array = Data.Props[It.first];
        if (Array.T == QBJS::Undefined)
            Array.T = QBJS::Array;
        Array.Elems.push_back(std::string(It.second));
    }
    OS << "QT_PLUGIN_METADATA_SECTION\n"
          "static const unsigned char qt_pluginMetaData[] = {\n"
          "    'Q', 'T', 'M', 'E', 'T', 'A', 'D', 'A', 'T', 'A', ' ', ' ',\n"
          "    'q', 'b', 'j', 's', 0x1, 0, 0, 0,\n    ";
    QBJS::Stream JSON(OS);
    JSON << Data;
    OS << "\n};\n";
}
