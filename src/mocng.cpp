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

#include "mocng.h"
#include "propertyparser.h"
#include "qbjs.h"

#include <clang/Basic/Version.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Lex/LiteralSupport.h>
#include <clang/Lex/LexDiagnostic.h>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Type.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/Lookup.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/YAMLParser.h>
#include <llvm/Support/SourceMgr.h>

#include <iostream>

static clang::SourceLocation GetFromLiteral(clang::Token Tok, clang::StringLiteral *Lit, clang::Preprocessor &PP) {
    return Lit->getLocationOfByte(PP.getSourceManager().getFileOffset(Tok.getLocation()),
                           PP.getSourceManager(), PP.getLangOpts(), PP.getTargetInfo());
}


//FIXME.  make it less stupid
static void parseInterfaces(ClassDef &Def, clang::Expr *Content, clang::Sema &Sema) {
    clang::Preprocessor &PP = Sema.getPreprocessor();
    clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(Content);
    if (!Val) {
        PP.getDiagnostics().Report(Content->getExprLoc(),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                                       "Invalid Q_INTERFACES annotation"));
        return;
    }

    llvm::MemoryBuffer* Buf = maybe_unique(llvm::MemoryBuffer::getMemBufferCopy(Val->getString(), "Q_INTERFACES"));
    clang::Lexer Lex(CreateFileIDForMemBuffer(PP, Buf, Content->getExprLoc()),
                     Buf, PP.getSourceManager(), PP.getLangOpts());

    clang::Token Tok;
    bool Append = false;
    bool Error = false;
    while (true) {
        Lex.LexFromRawLexer(Tok);

        if (Tok.is(clang::tok::eof))
            break;

        if (Tok.is(clang::tok::raw_identifier))
            PP.LookUpIdentifierInfo(Tok);

        if (Tok.is(clang::tok::identifier)) {
            if (Append)
                Def.Interfaces.back() += PP.getSpelling(Tok);
            else
                Def.Interfaces.push_back(PP.getSpelling(Tok));
            Append = false;
            continue;
        }

        if (Append) {
            Error = true;
            break;
        }

        if (Tok.is(clang::tok::coloncolon)) {
            Def.Interfaces.back() += PP.getSpelling(Tok);
            Append = true;
            continue;
        }

        if (!Tok.is(clang::tok::colon)) {
            Error = true;
            break;
        }
    }

     if (Error || Append || !Tok.is(clang::tok::eof)) {
         PP.getDiagnostics().Report(GetFromLiteral(Tok, Val, PP),
                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                    "parse error in Q_INTERFACES"));
     }

     // TODO: check interface validity
}


static void parsePluginMetaData(ClassDef &Def, clang::Expr *Content, clang::Sema &Sema) {
    clang::Preprocessor &PP = Sema.getPreprocessor();
    clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(Content);
    if (!Val) {
        PP.getDiagnostics().Report(Content->getExprLoc(),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                                       "Invalid Q_PLUGIN_METADATA annotation"));
        return;
    }

    llvm::MemoryBuffer* Buf = maybe_unique(llvm::MemoryBuffer::getMemBufferCopy(Val->getString(), "Q_PLUGIN_METADATA"));
    clang::Lexer Lex(CreateFileIDForMemBuffer(PP, Buf, Content->getExprLoc()),
                     Buf, PP.getSourceManager(), PP.getLangOpts());

    clang::Token Tok;
    Lex.LexFromRawLexer(Tok);
    while (Tok.is(clang::tok::raw_identifier)) {
        clang::IdentifierInfo *II =  PP.LookUpIdentifierInfo(Tok);
        if (II->getName() != "IID" && II->getName() != "FILE") {
            Lex.LexFromRawLexer(Tok);
            continue;
        }

        Lex.LexFromRawLexer(Tok);
        if (!Tok.is(clang::tok::string_literal)) {
            PP.getDiagnostics().Report(GetFromLiteral(Tok, Val, PP),
                        PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                            "Expected string literal"));
            return;
        }

        llvm::SmallVector<clang::Token, 4> StrToks;
        do {
            StrToks.push_back(Tok);
            Lex.LexFromRawLexer(Tok);
        } while (Tok.is(clang::tok::string_literal));
#if CLANG_VERSION_MAJOR!=3 || CLANG_VERSION_MINOR>4
        clang::StringLiteralParser Literal(StrToks, PP);
#else
        clang::StringLiteralParser Literal(&StrToks[0], StrToks.size(), PP);
#endif
        if (Literal.hadError)
            return;

        if (II->getName() == "IID")
            Def.Plugin.IID = Literal.GetString();
        else {
            llvm::StringRef Filename = Literal.GetString();
            const clang::DirectoryLookup *CurDir;
#if CLANG_VERSION_MAJOR < 10
            const clang::FileEntry *File =
#else
            const clang::Optional<clang::FileEntryRef> fileRef
                {
#endif
                    PP.LookupFile(
                                    Val->getSourceRange().getBegin(),
                                    Filename, false, nullptr,
                    #if CLANG_VERSION_MAJOR!=3 || CLANG_VERSION_MINOR>5
                                    nullptr,
                    #endif
                                    CurDir, nullptr, nullptr, nullptr
                    #if CLANG_VERSION_MAJOR >= 5
                                    , nullptr
                    #endif
                    #if CLANG_VERSION_MAJOR >= 9
                                    , nullptr
                    #endif
                    )
#if CLANG_VERSION_MAJOR >= 10
                };

            const clang::FileEntry *File = fileRef ? &fileRef->getFileEntry() : nullptr;
#else
                    ;
#endif

            if (!File) {
                PP.getDiagnostics().Report(GetFromLiteral(StrToks.front(), Val, PP), clang::diag::err_pp_file_not_found)
                    << Filename;
                return;
            }
            const llvm::MemoryBuffer* JSonBuf = PP.getSourceManager().getMemoryBufferForFile(File);
            llvm::SourceMgr SM;
            llvm::yaml::Stream YAMLStream(JSonBuf->getBuffer(), SM);
            llvm::yaml::document_iterator I = YAMLStream.begin();
            if (I == YAMLStream.end() || !I->getRoot() || !QBJS::Parse(I->getRoot(), Def.Plugin.MetaData)) {
                // FIXME
                PP.getDiagnostics().Report(GetFromLiteral(Tok, Val, PP),
                                            PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                            "Error pwhile parsing JSON"));
                return;
            }
        }
     }

     if (!Tok.is(clang::tok::eof)) {
         PP.getDiagnostics().Report(GetFromLiteral(Tok, Val, PP),
                                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                                        "Parse error: Expected 'IID' or 'FILE'"));
         return;
     }

}



static void parseEnums(BaseDef &Def, clang::DeclContext *Context, bool isFlag, clang::Expr *Content, clang::Sema &Sema) {
    clang::Preprocessor &PP = Sema.getPreprocessor();
    clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(Content);
    if (!Val) {
        PP.getDiagnostics().Report(Content->getExprLoc(),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                   "Invalid Q_ENUMS annotation"));
        return;
    }

    llvm::MemoryBuffer* Buf = maybe_unique(llvm::MemoryBuffer::getMemBufferCopy(Val->getString(), "Q_ENUMS"));
    clang::Lexer Lex(CreateFileIDForMemBuffer(PP, Buf, Content->getExprLoc()),
                     Buf, PP.getSourceManager(), PP.getLangOpts());

    clang::CXXScopeSpec SS;
    clang::Token Tok, Next;
    Lex.LexFromRawLexer(Tok);
    for (; !Tok.is(clang::tok::eof); Tok = Next) {
        Lex.LexFromRawLexer(Next);
        clang::IdentifierInfo* II = nullptr;
        if (Tok.is(clang::tok::raw_identifier))
            II = PP.LookUpIdentifierInfo(Tok);


        if (Tok.is(clang::tok::identifier)) {

            if (Next.is(clang::tok::coloncolon)) {
                auto TokLoc = GetFromLiteral(Tok, Val, PP);
                auto NextLoc = GetFromLiteral(Next, Val, PP);
#if CLANG_VERSION_MAJOR >= 4
                clang::Sema::NestedNameSpecInfo NameInfo(II, TokLoc, NextLoc);
                if (Sema.ActOnCXXNestedNameSpecifier(Sema.getScopeForContext(Context),
                        NameInfo, false, SS))
#else
                if (Sema.ActOnCXXNestedNameSpecifier(Sema.getScopeForContext(Context), *II,
                        TokLoc, NextLoc, {}, false, SS))
#endif
                {
                    SS.SetInvalid({TokLoc, NextLoc});
                }
                Lex.LexFromRawLexer(Next);
                continue;
            }

            clang::LookupResult Found(Sema, II, GetFromLiteral(Tok, Val, PP), clang::Sema::LookupNestedNameSpecifierName);
            if (SS.isEmpty())
                Sema.LookupQualifiedName(Found, Context);
            else {
                clang::DeclContext* DC = Sema.computeDeclContext(SS);
                Sema.LookupQualifiedName(Found, DC ? DC : Context);
            }

            llvm::StringRef Alias;
            clang::EnumDecl* R = Found.getAsSingle<clang::EnumDecl>();

            if (!R) {
                if (clang::TypedefDecl *TD = Found.getAsSingle<clang::TypedefDecl>()) {
                    const clang::EnumType* ET = TD->getUnderlyingType()->getAs<clang::EnumType>();
                    const clang::TemplateSpecializationType* TDR = TD->getUnderlyingType()->getAs<clang::TemplateSpecializationType>();
                    if(TDR && TDR->getNumArgs() == 1 && TDR->getTemplateName().getAsTemplateDecl()->getName() == "QFlags")
                        ET = TDR->getArg(0).getAsType()->getAs<clang::EnumType>();
                    if (ET) {
                        R = ET->getDecl();
                        if (TD->getIdentifier())
                            Alias = TD->getName();
                    }
                }
            }

            if (Found.empty() || !R) {
                // TODO: typo correction

                // This should be an error, but the official moc do not understand that as an error.
                PP.getDiagnostics().Report(GetFromLiteral(Tok, Val, PP),
                                           PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                            "no enum names %0")) << Found.getLookupName();
                break;
            }
            if (R->getDeclContext() == Context) {
                if (Alias.empty() && R->getIdentifier())
                    Alias = R->getName();
                Def.addEnum(R, Alias.empty() ? R->getNameAsString() : std::string(Alias), isFlag);
            } else if (R->getDeclContext()->isRecord() &&  llvm::isa<clang::CXXRecordDecl>(R->getDeclContext())) {
                // TODO: check it is a QObject
                Def.addExtra(llvm::cast<clang::CXXRecordDecl>(R->getDeclContext()));
            }
            SS.clear();
            continue;
        } else if (Tok.is(clang::tok::coloncolon)) {
            if (SS.isEmpty()) {
                SS.MakeGlobal(Sema.getASTContext(), GetFromLiteral(Tok, Val, PP));
                continue;
            }
        }

        PP.getDiagnostics().Report(GetFromLiteral(Tok, Val, PP),
                                       PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                       "Invalid token in Q_ENUMS"));
        break;
    }


}

template<int N>
static std::pair<clang::StringLiteral*, clang::StringLiteral *> ExtractLiterals(clang::Expr *E,
                                                                                const clang::Preprocessor &PP,
                                                                                const char *Keyword,
                                                                                const char (&Error)[N]) {
    clang::BinaryOperator* BO = llvm::dyn_cast<clang::BinaryOperator>(E);
    clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
    if (!BO) {
        PP.getDiagnostics().Report(E->getExprLoc(),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                    "Invalid %0 annotation")) << Keyword;
    } else {
        if (!(Val1 = llvm::dyn_cast<clang::StringLiteral>(BO->getLHS())))
            PP.getDiagnostics().Report(BO->getLHS()->getExprLoc(),
                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error, Error));
        if (!(Val2 = llvm::dyn_cast<clang::StringLiteral>(BO->getRHS())))
            PP.getDiagnostics().Report(BO->getRHS()->getExprLoc(),
                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error, Error));
    }
    return {Val1, Val2};
}

static void parseClassInfo(BaseDef &Def, clang::Expr *SubExp, clang::Preprocessor &PP)
{
    clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
    std::tie(Val1, Val2) = ExtractLiterals(SubExp, PP, "Q_CLASSINFO",
                                                        "Expected string literal in Q_CLASSINFO");

    if (Val1 && Val2) {
        Def.ClassInfo.emplace_back(Val1->getString(), Val2->getString());
    }
}

static bool IsAnnotationStaticAssert(clang::Decl *Decl, llvm::StringRef *Key, clang::Expr **SubExp) {
    if (clang::StaticAssertDecl *S = llvm::dyn_cast<clang::StaticAssertDecl>(Decl)) {
        if (auto *Cast = llvm::dyn_cast<clang::ImplicitCastExpr>(S->getAssertExpr()))
            if (auto *E = llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(Cast->getSubExpr()))
                if (clang::ParenExpr *PE = llvm::dyn_cast<clang::ParenExpr>(E->getArgumentExpr()))
                {
                    *Key = S->getMessage()->getString();
                    *SubExp = PE->getSubExpr();
                    return true;
                }
    }
    return false;
}

ClassDef MocNg::parseClass(clang::CXXRecordDecl* RD, clang::Sema& Sema)
{
    clang::Preprocessor &PP = Sema.getPreprocessor();
    ClassDef Def;
    Def.Record = RD;

    for (auto decl : RD->decls()) {
        llvm::StringRef key;
        clang::Expr *SubExp;
        if (IsAnnotationStaticAssert(decl, &key, &SubExp)) {
            if (key == "qt_property") {
                clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(SubExp);
                if (Val) {
                    PropertyParser Parser(Val->getString(),
//                                          Val->getStrTokenLoc(0),
                                        Val->getLocationOfByte(0, PP.getSourceManager(), PP.getLangOpts(), PP.getTargetInfo()),
                                        Sema, Def.Record);
                    Def.Properties.push_back(Parser.parseProperty());
                    Def.addExtra(Parser.Extra);
                } else {
                    PP.getDiagnostics().Report(decl->getLocation(),
                                                PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                "Invalid Q_PROPERTY annotation"));
                }
            } else if (key == "qt_private_property") {
                clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
                std::tie(Val1, Val2) = ExtractLiterals(SubExp, PP, "Q_PRIVATE_PROPERTY",
                                                        "Invalid Q_PRIVATE_PROPERTY annotation");

                if (Val1 && Val2) {
                    PropertyParser Parser(Val2->getString(),
                                            Val2->getLocationOfByte(0, PP.getSourceManager(), PP.getLangOpts(), PP.getTargetInfo()),
                                            Sema, Def.Record);
                    PropertyDef P = Parser.parseProperty(true);
                    P.inPrivateClass = Val1->getString();
                    Def.Properties.push_back(std::move(P));
                    Def.addExtra(Parser.Extra);
                }
            } else if (key == "qt_private_slot") {
                clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
                std::tie(Val1, Val2) = ExtractLiterals(SubExp, PP, "Q_PRIVATE_SLOT",
                                                        "Invalid Q_PRIVATE_SLOT annotation");
                if (Val1 && Val2) {
                    PropertyParser Parser(Val2->getString(),
                                            Val2->getLocationOfByte(0, PP.getSourceManager(), PP.getLangOpts(), PP.getTargetInfo()),
                                            Sema, Def.Record);
                    PrivateSlotDef P = Parser.parsePrivateSlot();
                    P.InPrivateClass = Val1->getString();
                    if (!P.Name.empty()) {
                        Def.PrivateSlotCount += P.NumDefault + 1;
                        Def.PrivateSlots.push_back(std::move(P));
                    }
                }
            } else if (key == "qt_enums")  {
                parseEnums(Def, Def.Record, false, SubExp, Sema);
            } else if (key == "qt_flags")  {
                parseEnums(Def, Def.Record, true, SubExp, Sema);
            } else if (key == "qt_qobject") {
                Def.HasQObject = true;
            } else if (key == "qt_fake") {
                Def.HasQGadget = false;
                Def.HasQObject = false;
            } else if (key == "qt_qgadget") {
                Def.HasQGadget = true;
            } else if (key == "qt_classinfo") {
                parseClassInfo(Def, SubExp, PP);
            } else if (key == "qt_interfaces") {
                parseInterfaces(Def, SubExp, Sema);
            } else if (key == "qt_plugin_metadata") {
                parsePluginMetaData(Def, SubExp, Sema);
                HasPlugin = true;
            }
        } else if (clang::CXXMethodDecl *M = llvm::dyn_cast<clang::CXXMethodDecl>(decl)) {
            for (auto attr_it = M->specific_attr_begin<clang::AnnotateAttr>();
                attr_it != M->specific_attr_end<clang::AnnotateAttr>();
                ++attr_it) {

                const clang::AnnotateAttr *A = *attr_it;
                if (A->getAnnotation() == "qt_signal") {
                        Def.Signals.push_back(M);
                } else if (A->getAnnotation() == "qt_slot") {
                        Def.Slots.push_back(M);
                } else if (A->getAnnotation() == "qt_invokable" || A->getAnnotation() == "qt_scriptable" ) {
                    if (auto *C = llvm::dyn_cast<clang::CXXConstructorDecl>(M)) {
                            Def.Constructors.push_back(C);
                    } else {
                            Def.Methods.push_back(M);
                    }
                } else if (A->getAnnotation().startswith("qt_revision:")) {
                    Def.RevisionMethodCount++;
                }
            }
        }
    }

    //Check notify Signals
    for (PropertyDef &P: Def.Properties) {
        if (!P.notify.Str.empty()) {
            int Idx = 0;
            auto errorLevel = clang::DiagnosticsEngine::Error;
            for (clang::CXXMethodDecl *MD : Def.Signals) {
                if (MD->getName() == P.notify.Str) {
                    P.notify.notifyId = Idx;
                    P.notify.MD = MD;
                    break;
                }
                Idx += 1 + MD->getNumParams() - MD->getMinRequiredArguments();
            }
            if (P.notify.notifyId < 0 ) {
                // Search in base classes
                clang::CXXRecordDecl *Base = Def.Record;
                do {
                    if (!Base->getNumBases())
                        break;
                    Base = Base->bases_begin()->getType()->getAsCXXRecordDecl();
                    if (!Base)
                        break;
                    for (auto it = Base->decls_begin(); it != Base->decls_end(); ++it) {
                        if (auto *MD = llvm::dyn_cast<clang::CXXMethodDecl>(*it)) {

                            if (MD->getIdentifier() && MD->getName() == P.notify.Str) {
                                // We found a possible match. Check if it is indeed a signal
                                if (std::any_of(MD->specific_attr_begin<clang::AnnotateAttr>(),
                                                MD->specific_attr_end<clang::AnnotateAttr>(),
                                                [&](const clang::AnnotateAttr *a) {
                                                    return a->getAnnotation() == "qt_signal";
                                                })) {
                                    P.notify.MD = MD;
                                    break;
                                }
                                // Since the official moc let this compile and the runtime will show
                                // a warning, we just change the level to Warning.
                                // (required for tst_qmetaobject which tests that)
                                errorLevel = clang::DiagnosticsEngine::Warning;
                            }
                        }
                    }
                } while(!P.notify.MD);
            }
            if (!P.notify.MD) {
                PP.getDiagnostics().Report(P.notify.Loc,
                        PP.getDiagnostics().getCustomDiagID(errorLevel,
                            "NOTIFY signal '%0' of property '%1' does not exist in class %2"))
                    << P.notify.Str << P.name << Def.Record;
            }
            Def.NotifyCount++;
        }

        if (P.revision > 0)
            Def.RevisionPropertyCount++;
    }
    return Def;
}

NamespaceDef MocNg::parseNamespace(clang::NamespaceDecl* ND, clang::Sema& Sema)
{
    NamespaceDef Def;
    Def.Namespace = ND;
    for (auto it = ND->decls_begin(); it != ND->decls_end(); ++it) {
        llvm::StringRef key;
        clang::Expr *SubExp;
        if (IsAnnotationStaticAssert(*it, &key, &SubExp)) {
            if (key == "qt_qnamespace") {
                Def.hasQNamespace = true;
            } else if (key == "qt_enums")  {
                parseEnums(Def, ND, false, SubExp, Sema);
            } else if (key == "qt_flags")  {
                parseEnums(Def, ND, true, SubExp, Sema);
            } else if (key == "qt_classinfo") {
                parseClassInfo(Def, SubExp, Sema.getPreprocessor());
            }
        }
    }
    return Def;
}

std::string MocNg::GetTag(clang::SourceLocation DeclLoc, const clang::SourceManager &SM)
{
    clang::SourceLocation FileLoc = SM.getFileLoc(DeclLoc);
    clang::FileID FID = SM.getFileID(FileLoc);
    const llvm::MemoryBuffer *Buffer = SM.getBuffer(FID);
    const char *B = Buffer->getBufferStart();
    int Off = SM.getFileOffset(FileLoc);
    int Orig = Off;
    while (Off > 0 && B[Off] != ';' && B[Off]!=',' && B[Off] != '}' && B[Off] != ':' /*&& B[Off] != '\n'*/ ) {
        Off--;
    }

    auto it_before = Tags.lower_bound(FileLoc.getLocWithOffset(Off - Orig));
    auto it_after = Tags.upper_bound(FileLoc);
    if (it_before != Tags.end() && it_after != Tags.begin() && it_before == (--it_after)) {
        return it_before->second;
    }
    return {};
}

bool MocNg::ShouldRegisterMetaType(clang::QualType T)
{
    if (T->isVoidType() || (T->isReferenceType() && !T.getNonReferenceType().isConstQualified()))
        return false;

    if (registered_meta_type.count(T->getCanonicalTypeUnqualified().getTypePtr()))
        return true;

    T = T.getNonReferenceType();

    if (T->isPointerType()) {
        // registering pointer to forward declared type fails.
        const clang::CXXRecordDecl* Pointee = T->getPointeeCXXRecordDecl();
        if (Pointee && !Pointee->hasDefinition())
            return false;
        return true;
    }

    if (auto TD = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(T->getAsCXXRecordDecl())) {
        if (!TD->hasDefinition()) {
            if (auto CTD = TD->getSpecializedTemplate()) {
                if (CTD->getTemplatedDecl() && !CTD->getTemplatedDecl()->hasDefinition())
                    return false;
            }
        }
        for (uint I = 0; I < TD->getTemplateArgs().size(); ++I) {
            const auto &Arg = TD->getTemplateArgs().get(I);
            if (Arg.getKind() == clang::TemplateArgument::Type) {
                if (!ShouldRegisterMetaType(Arg.getAsType()))
                    return false;
            }
        }
    }
    return true;
}
