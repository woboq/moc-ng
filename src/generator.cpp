/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


#include "generator.h"
#include "mocng.h"
#include "qbjs.h"
#include <string>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ASTContext.h>
#include <clang/Sema/Sema.h>

#include <iostream>

static bool HasPrivateSignal(const clang::CXXMethodDecl *MD) {
    if (MD && MD->getNumParams()) {
        clang::CXXRecordDecl* RD = MD->getParamDecl(MD->getNumParams()-1)->getType()->getAsCXXRecordDecl();
        return RD && RD->getIdentifier() && RD->getName() == "QPrivateSignal";
    }
    return false;
}

//  foreach method,  including  clones
template<typename T, typename F>
static void ForEachMethod(const std::vector<T> &V, F && Functor) {
    for(auto it : V) {
        int Clones = it->getNumParams() - it->getMinRequiredArguments();
        for (int C = 0; C <= Clones; ++C)
            Functor(it, C);
    }
}

template<typename T> int CountMethod(const T& V) {
    int R  = 0;
    ForEachMethod(V, [&](const clang::CXXMethodDecl*, int) { R++; });
    return R;
}

template<typename T> int AggregatePerameterCount(const std::vector<T>& V) {
    int R = 0;
    ForEachMethod(V, [&](const clang::CXXMethodDecl *M, int C) {
        R += M->getNumParams() - C;
        R += 1; // return value;
        if (HasPrivateSignal(M))
            R--;
    });
    return R;
}
/*
template<typename F>
int ForAll(const ClassDef *CDef, F && Functor , bool Constructors = false) {
    int R = Functor(CDef->Signals) + Functor(CDef->Slots) + Functor(CDef->Methods);
    if (Constructors)
        R += Functor(CDef->Constructors);
    return R;
}*/

template <typename T>
void Generator::GenerateFunction(const T& V, const char* TypeName, MethodFlags Type, int& ParamIndex)
{
    if (V.empty())
        return;

    OS << "\n // " << TypeName << ": name, argc, parameters, tag, flags\n";

    ForEachMethod(V, [&](const clang::CXXMethodDecl *M, int C) {
        unsigned int Flags = Type;
        if (Type == MethodSignal)
            Flags |= AccessProtected;  // That's what moc beleive
        else if (M->getAccess() == clang::AS_private)
            Flags |= AccessPrivate;
        else if (M->getAccess() == clang::AS_public)
            Flags |= AccessPublic;
        else if (M->getAccess() == clang::AS_protected)
            Flags |= AccessProtected;

        if (C)
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

        int argc =  M->getNumParams() - C;
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

void Generator::GetTypeInfo(clang::QualType Type)
{
    if (Type->isVoidType()) {
        OS << "QMetaType::Void";
        return;
    }
    // TODO:  Find the QMetaType

    // remove const or const &
    if (Type->isReferenceType() && Type.getNonReferenceType().isConstQualified())
        Type = Type.getNonReferenceType();
    Type.removeLocalConst();

    clang::PrintingPolicy Policy = PrintPolicy;
    Policy.SuppressScope = true;
    std::string TypeString = Type.getAsString(Policy);

    // Remove the spaces;
    int k = 0;
    for (int i = 0; i < TypeString.size(); ++i) {
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
    int UPos = 0;
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


template <typename T>
void Generator::GenerateFunctionParameters(const std::vector< T* >& V, const char* TypeName)
{
    if (V.empty())
        return;

    OS << "\n // " << TypeName << ": parameters\n";

    ForEachMethod(V, [&](const clang::CXXMethodDecl *M, int C) {
        int argc =  M->getNumParams() - C;
        if (HasPrivateSignal(M))
            argc--;
        OS << "    ";
        //Types
        if (std::is_same<T, clang::CXXConstructorDecl>::value)
            OS << "0x80000000 | " << StrIdx("");
        else
            GetTypeInfo(M->getResultType());
        OS <<  ",";
        for (int j = 0; j < argc; j++) {
            OS << " ";
            GetTypeInfo(M->getParamDecl(j)->getOriginalType());
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



Generator::Generator(const ClassDef* CDef, llvm::raw_ostream& OS, clang::ASTContext& Ctx, MocNg* Moc) :
    CDef(CDef), OS(OS), Ctx(Ctx), PrintPolicy(Ctx.getPrintingPolicy()), Moc(Moc)
{
    PrintPolicy.SuppressTagKeyword = true;
    PrintPolicy.SuppressUnwrittenScope = true;
    PrintPolicy.AnonymousTagLocations = false;

    QualName = clang::QualType(CDef->Record->getTypeForDecl(), 0).getAsString(PrintPolicy);

    if (CDef->Record->getNumBases())
        BaseName = CDef->Record->bases_begin()->getType().getAsString(PrintPolicy);

    MethodCount = CountMethod(CDef->Signals) + CountMethod(CDef->Slots) + CountMethod(CDef->Methods) + CDef->PrivateSlotCount;
}

void Generator::GenerateCode()
{

    // Build the data array
    std::string QualifiedClassNameIdentifier = QualName;
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
    int TotalParameterCount = AggregatePerameterCount(CDef->Signals) + AggregatePerameterCount(CDef->Slots)
                            + AggregatePerameterCount(CDef->Methods) + AggregatePerameterCount(CDef->Constructors);
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

    OS << "    " << 0 << ", // flags \n";

    OS << "    " << CountMethod(CDef->Signals) << ", // signalCount \n";


    if (CDef->ClassInfo.size()) {
        OS << "\n  // classinfo: key, value\n";
        for (const auto &I : CDef->ClassInfo)
            OS << "    " << StrIdx(I.first) << ", " << StrIdx(I.second) << ",\n";
    }


    GenerateFunction(CDef->Signals, "signals", MethodSignal, ParamsIndex);
    GenerateFunction(CDef->Slots, "slots", MethodSlot, ParamsIndex);
    for (const PrivateSlotDef &P : CDef->PrivateSlots) {
        for (int C = 0; C <= P.NumDefault; ++C) {
            int argc = (P.Args.size() - C);
            OS << "    " << StrIdx(P.Name) << ", " << argc << ", " << ParamsIndex << ", 0, 0x";
            unsigned int Flag = AccessPrivate | MethodSlot;
            if (C) Flag |= MethodCloned;
            OS.write_hex(Flag) << ",\n";
            ParamsIndex += 1 + argc * 2;
        }
    }
    GenerateFunction(CDef->Methods, "methods", MethodMethod, ParamsIndex);

    if (CDef->RevisionMethodCount) {
        auto GenerateRevision = [&](const clang::CXXMethodDecl *M, int C) {
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
            for (int C = 0; C <= P.NumDefault; ++C)
                OS << " 0,    ";
        }
        OS << "\n    ";
        ForEachMethod(CDef->Methods, GenerateRevision);
        OS << "\n";
    }



    GenerateFunctionParameters(CDef->Signals, "signals");
    GenerateFunctionParameters(CDef->Slots, "slots");
    for (const PrivateSlotDef &P : CDef->PrivateSlots) {
        for (int C = 0; C <= P.NumDefault; ++C) {
            int argc = (P.Args.size() - C);
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

    GenerateFunction(CDef->Constructors, "constructors", MethodConstructor, ParamsIndex);

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
          "    offsetof(qt_meta_stringdata_"<<  QualifiedClassNameIdentifier << "_t, stringdata) + ofs \\\n"
          "        - idx * sizeof(QByteArrayData) \\\n"
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
        } else if (S.size() && S[0] >= '0' && S[0] <= '9') {
            OS << "\"\"";
        }
        OS.write_escaped(S) << "\\0";
        Col += 2 + S.size();
    }
    OS << "\"\n};\n"
          "#undef QT_MOC_LITERAL\n";

    if (!CDef->Extra.empty()) {
        OS << "static const QMetaObject *qt_meta_extradata_" << QualifiedClassNameIdentifier << "[] = {\n" ;
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

    OS << "\nconst QMetaObject " << QualName << "::staticMetaObject = {\n"
          "    { ";
    if (BaseName.empty()) OS << "0";
    else OS << "&" << BaseName << "::staticMetaObject";

    OS << ", qt_meta_stringdata_"<< QualifiedClassNameIdentifier <<".data,\n"
          "      qt_meta_data_" << QualifiedClassNameIdentifier << ", ";

    if (CDef->HasQObject) OS << "qt_static_metacall, ";
    else OS << "0, ";

    if (!CDef->Extra.empty()) OS << "qt_meta_extradata_" << QualifiedClassNameIdentifier << ", ";
    else OS << "0, ";
    OS << "0}\n};\n";

    if (CDef->HasQObject) {
        OS << "const QMetaObject *" << QualName << "::metaObject() const\n{\n"
              "    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;\n}\n";


        OS << "void *" << QualName << "::qt_metacast(const char *_clname)\n{\n"
              "    if (!_clname) return 0;\n"
              "    if (!strcmp(_clname, qt_meta_stringdata_" << QualifiedClassNameIdentifier << ".stringdata))\n"
              "        return static_cast<void*>(this);\n";

        if (CDef->Record->getNumBases() > 1) {
            for (auto BaseIt = CDef->Record->bases_begin()+1; BaseIt != CDef->Record->bases_end(); ++BaseIt) {
                if (BaseIt->getAccessSpecifier() == clang::AS_private)
                    continue;
                llvm::StringRef B = BaseIt->getType().getAsString(PrintPolicy);
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
    OS << "\nint " << QualName << "::qt_metacall(QMetaObject::Call _c, int _id, void **_a)\n{\n";
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
        bool needGet = false;
        //bool needTempVarForGet = false;
        bool needSet = false;
        bool needReset = false;
        bool needDesignable = false;
        bool needScriptable = false;
        bool needStored = false;
        bool needEditable = false;
        bool needUser = false;
        for (const PropertyDef &p : CDef->Properties) {
            needGet |= !p.read.empty() || !p.member.empty();
            /*if (!p.read.empty())
                needTempVarForGet |= (p.gspec != PropertyDef::PointerSpec
                && p.gspec != PropertyDef::ReferenceSpec);*/
            needSet |= !p.write.empty() || (!p.member.empty() && !p.constant);
            needReset |= !p.reset.empty();

            auto IsFunction = [](const std::string &S) { return S.size() && S[S.size()-1] == ')'; };
            needDesignable |= IsFunction(p.designable);
            needScriptable |= IsFunction(p.scriptable);
            needStored |= IsFunction(p.stored);
            needEditable |= IsFunction(p.editable);
            needUser |= IsFunction(p.user);
        }

        OS << "#ifndef QT_NO_PROPERTIES\n    ";
        if (MethodCount)
            OS << "else ";

        auto HandleProperty = [&](bool Need, const char *Action, const std::function<void(const PropertyDef &)> &F) {
            OS << "if (_c == QMetaObject::" << Action << ") {\n";
            if (Need) {
                OS << "        switch (_id) {\n";
                int I = 0;
                for (const PropertyDef &p : CDef->Properties) {
                    OS << "        case " << (I++) <<": ";
                    F(p);
                    OS << "break;\n";
                }
                OS << "        }";
            }
            OS << "        _id -= " << CDef->Properties.size() << ";\n    }";
        };

        HandleProperty(needGet, "ReadProperty", [&](const PropertyDef &p) {
            if (p.read.empty() && p.member.empty())
                return;

            //FIXME: enums case
            if (p.PointerHack) {
              OS << "_a[0] = const_cast<void*>(static_cast<const void*>(";
              if (p.inPrivateClass.size())
                  OS << p.inPrivateClass << "->" ;
              OS << p.read << "())); ";
            } else {
              OS << "*reinterpret_cast< " << p.type << "*>(_a[0]) = ";
              if (p.inPrivateClass.size())
                  OS << p.inPrivateClass << "->" ;
              if (!p.read.empty())
                  OS << p.read << "(); ";
              else
                  OS << p.member << "; ";
            }
        });
        OS << " else ";
        HandleProperty(needSet, "WriteProperty", [&](const PropertyDef &p) {
            if (p.constant)
                return;
            if (!p.write.empty()) {
                if (p.inPrivateClass.size())
                    OS << p.inPrivateClass << "->" ;
                OS << p.write << "(*reinterpret_cast< " << p.type << "*>(_a[0])); ";
            } else if (!p.member.empty()) {
                std::string M = p.member;
                std::string A = "*reinterpret_cast< " + p.type + "*>(_a[0])";
                if (p.inPrivateClass.size())
                    M = p.inPrivateClass + "->" + M;
                if (p.notify.notifyId >= 0) {
                    OS << "\n"
                          "            if (" << M << " != " << A << ") {\n"
                          "                " << M << " = " << A << ";\n"
                          "                emit " << p.notify.Str << "(";
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
        HandleProperty(needReset, "ResetProperty", [&](const PropertyDef &p) {
            if (p.reset.empty() || p.reset[p.reset.size()-1] != ')')
                return;

            if (p.inPrivateClass.size())
                OS << p.inPrivateClass << "->" ;
            OS << p.reset << "; ";
        });

        typedef std::string (PropertyDef::*Accessor);
        auto HandleQueryProperty = [&](bool Need, const char *Action, Accessor A) {
            HandleProperty(Need, Action, [&](const PropertyDef &p) {
                const std::string &S = (p.*A);
                if (S.empty() || S[S.size()-1] != ')')
                    return;
                OS << "*reinterpret_cast<bool*>(_a[0]) = " << S << "; ";
            });
        };

        OS << " else ";
        HandleQueryProperty(needDesignable, "QueryPropertyDesignable", &PropertyDef::designable);
        OS << " else ";
        HandleQueryProperty(needScriptable, "QueryPropertyScriptable", &PropertyDef::scriptable);
        OS << " else ";
        HandleQueryProperty(needScriptable, "QueryPropertyStored", &PropertyDef::stored);
        OS << " else ";
        HandleQueryProperty(needEditable, "QueryPropertyEditable", &PropertyDef::editable);
        OS << " else ";
        HandleQueryProperty(needUser, "QueryPropertyUser", &PropertyDef::user);
        OS << " else ";
        HandleQueryProperty(needUser, "QueryPropertyUser", &PropertyDef::user);

        OS << " else if (_c == QMetaObject::RegisterPropertyMetaType) {\n"
              "        if (_id < " << CDef->Properties.size() <<  ")\n"
              "            qt_static_metacall(this, _c, _id, _a);\n"
              "        _id -= " << CDef->Properties.size() <<  ";\n"
              "    }\n"
              "#endif // QT_NO_PROPERTIES\n";
    }
    OS << "    return _id;"
          "}\n";
}

void Generator::GenerateStaticMetaCall()
{
    llvm::StringRef ClassName = CDef->Record->getName();
    OS << "\nvoid " << QualName << "::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)\n{\n    ";
    bool NeedElse = false;

    if (!CDef->Constructors.empty()) {
        OS << "    if (_c == QMetaObject::CreateInstance) {\n"
              "        switch (_id) {\n";

        int CtorIndex = 0;
        ForEachMethod(CDef->Constructors, [&](const clang::CXXConstructorDecl *MD, int C) {
            OS << "        case " << (CtorIndex++) << ": { QObject *_r = new " << ClassName << "(";

            for (int j = 0 ; j < MD->getNumParams() - C; ++j) {
                if (j) OS << ",";
                if (j == MD->getNumParams() - 1 && HasPrivateSignal(MD))
                    OS << "QPrivateSignal()";
                else
                    OS << "*reinterpret_cast< " << Ctx.getPointerType(MD->getParamDecl(j)->getType().getNonReferenceType().getUnqualifiedType()).getAsString(PrintPolicy) << " >(_a[" << (j+1) << "])";
            }
            OS << ");\n            if (_a[0]) *reinterpret_cast<QObject**>(_a[0]) = _r; } break;\n";
        });
        OS << "        }\n"
              "    }";

        NeedElse = true;
    }

    if (MethodCount) {
        if(NeedElse) OS << " else ";
        NeedElse = true;
        OS << "if (_c == QMetaObject::InvokeMetaMethod) {\n"
//            "        Q_ASSERT(staticMetaObject.cast(_o));\n"
              "        " << ClassName <<" *_t = static_cast<" << ClassName << " *>(_o);\n"
              "        switch(_id) {\n" ;
        int MethodIndex = 0;
        auto GenM = [&](const clang::CXXMethodDecl *MD, int C) {
            if (!MD->getIdentifier())
                return;

            OS << "        case " << MethodIndex << ": ";
            // Original moc don't support reference as return type: see  Moc::parseFunction
            bool IsVoid = MD->getResultType()->isVoidType() || MD->getResultType()->isReferenceType();
            if (!IsVoid)
                OS << "{ " << MD->getResultType().getUnqualifiedType().getAsString(PrintPolicy) << " _r =  ";

            OS << "_t->" << MD->getName() << "(";

            for (int j = 0 ; j < MD->getNumParams() - C; ++j) {
                if (j) OS << ",";
                if (j == MD->getNumParams() - 1 && HasPrivateSignal(MD))
                    OS << "QPrivateSignal()";
                else
                    OS << "*reinterpret_cast< " << Ctx.getPointerType(MD->getParamDecl(j)->getType().getNonReferenceType().getUnqualifiedType()).getAsString(PrintPolicy) << " >(_a[" << (j+1) << "])";
            }
            OS << ");";
            if (!IsVoid) {
                OS << "\n            if (_a[0]) *reinterpret_cast< "
                   << Ctx.getPointerType(MD->getResultType().getNonReferenceType().getUnqualifiedType()).getAsString(PrintPolicy)
                   << " >(_a[0]) = _r; }";
            }
            OS <<  " break;\n";
            MethodIndex++;
        };
        ForEachMethod(CDef->Signals, GenM);
        ForEachMethod(CDef->Slots, GenM);
        for (const PrivateSlotDef &P : CDef->PrivateSlots) {
            for (int C = 0; C <= P.NumDefault; ++C) {
                OS << "        case " << MethodIndex << ": ";
                // Original moc don't support reference as return type: see  Moc::parseFunction
                bool IsVoid = P.ReturnType == "void" || P.ReturnType.empty() || P.ReturnType.back() == '&';
                if (!IsVoid)
                    OS << "{ " << P.ReturnType << " _r =  ";
                OS << "_t->" << P.InPrivateClass << "->" << P.Name << "(";
                for (int j = 0 ; j < P.Args.size() - C; ++j) {
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
        ForEachMethod(CDef->Methods, GenM);

        OS << "        }\n"
              "    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {\n"
              "        switch ((_id << 16) | *reinterpret_cast<int*>(_a[1])) {\n"
              "        default: *reinterpret_cast<int*>(_a[0]) = -1; break;\n";


        auto RegisterT = [&](const clang::QualType T, unsigned int Idx) {
            if (!Moc->ShouldRegisterMetaType(T))
                return;
            OS << "       case 0x";
            OS.write_hex(Idx);
            OS << ": *reinterpret_cast<int*>(_a[0]) = ";
            OS <<  "QtPrivate::QMetaTypeIdHelper< " << T.getNonReferenceType().getUnqualifiedType().getAsString(PrintPolicy)
                << " >::qt_metatype_id(); break;\n";
        };


        MethodIndex = 0;
        auto RegisterM = [&](const clang::CXXMethodDecl *MD, int C) {
            if (!MD->getIdentifier())
                return;
          //  RegisterT(MD->getResultType(), (MethodIndex << 16));
            int argc = MD->getNumParams() - C - (HasPrivateSignal(MD)?1:0);
            for (int j = 0 ; j < argc ; ++j)
                RegisterT(MD->getParamDecl(j)->getType(), (MethodIndex << 16) | j);
            MethodIndex++;
        };

        ForEachMethod(CDef->Signals, RegisterM);
        ForEachMethod(CDef->Slots, RegisterM);
        MethodIndex += CDef->PrivateSlotCount; //Or should we?
        ForEachMethod(CDef->Methods, RegisterM);

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
                  "            typedef " << MD->getResultType().getAsString(PrintPolicy) << " (" << ClassName << "::*_t)(";
            for (int j = 0 ; j < MD->getNumParams(); ++j) {
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

#if 0
        if (methodList.empty()) {
            fprintf(out, "    Q_UNUSED(_o);\n");
            if (CDef->constructorList.empty() && automaticPropertyMetaTypes.empty() && methodsWithAutomaticTypesHelper(methodList).empty()) {
                fprintf(out, "    Q_UNUSED(_id);\n");
                fprintf(out, "    Q_UNUSED(_c);\n");
            }
        }
        if (!isUsed_a)
            fprintf(out, "    Q_UNUSED(_a);\n");

        fprintf(out, "}\n\n");
#endif
    OS << "\n    Q_UNUSED(_o); Q_UNUSED(_id); Q_UNUSED(_c); Q_UNUSED(_a);";
    OS << "\n}\n";
}

void Generator::GenerateSignal(const clang::CXXMethodDecl *MD, int Idx)
{
    if (MD->isPure())
        return;

    OS << "\n// SIGNAL " << Idx << "\n"
       << MD->getResultType().getAsString(PrintPolicy) << " " << QualName << "::" << MD->getName() + "(";
    for (int j = 0 ; j < MD->getNumParams(); ++j) {
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
    OS << "\n{\n";
    bool IsVoid = MD->getResultType()->isVoidType();
    unsigned int NumParam = MD->getNumParams();
    if (HasPrivateSignal(MD)) NumParam--;
    if (IsVoid && NumParam == 0) {
        OS << "    QMetaObject::activate(" << This << ", &staticMetaObject, " << Idx << ", 0);\n";
    } else {
        std::string T = MD->getResultType().getNonReferenceType().getUnqualifiedType().getAsString(PrintPolicy);
        if (MD->getResultType()->isPointerType()) {
            OS << "    " << MD->getResultType().getAsString(PrintPolicy) << " _t0 = 0;\n";
        } else if (!IsVoid) {
            OS << "    " << T << " _t0 = " << T << "();\n";
        }
        OS << "    void *_a[] = { ";
        if (IsVoid) OS << "0";
        else OS << "&_t0";


        for (int j = 0 ; j < NumParam; ++j) {
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
    OS << "QT_PLUGIN_METADATA_SECTION\n"
          "static const unsigned char qt_pluginMetaData[] = {\n"
          "    'Q', 'T', 'M', 'E', 'T', 'A', 'D', 'A', 'T', 'A', ' ', ' ',\n"
          "    'q', 'b', 'j', 's', 0x1, 0, 0, 0,\n    ";
    QBJS::Stream JSON(OS);
    JSON << Data;
    OS << "\n};\n";
}

