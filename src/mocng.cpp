/****************************************************************************
 *  Copyright (C) 2013-2014 Woboq GmbH
 *  Olivier Goffart <contact at woboq.com>
 *  http://woboq.com/
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

    llvm::MemoryBuffer* Buf = llvm::MemoryBuffer::getMemBufferCopy(Val->getString(), "Q_INTERFACES");
    clang::Lexer Lex(PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Content->getExprLoc()),
                     Buf, PP.getSourceManager(), PP.getLangOpts());

    clang::Token Tok;
    bool Append = false;
    bool Error = false;
    while (true) {
        Lex.LexFromRawLexer(Tok);

        if (Tok.is(clang::tok::eof))
            break;

        clang::IdentifierInfo* II = nullptr;
        if (Tok.is(clang::tok::raw_identifier))
            II = PP.LookUpIdentifierInfo(Tok);

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

    llvm::MemoryBuffer* Buf = llvm::MemoryBuffer::getMemBufferCopy(Val->getString(), "Q_PLUGIN_METADATA");
    clang::Lexer Lex(PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Content->getExprLoc()),
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
        clang::StringLiteralParser Literal(&StrToks[0], StrToks.size(), PP);
        if (Literal.hadError)
            return;

        if (II->getName() == "IID")
            Def.Plugin.IID = Literal.GetString();
        else {
            llvm::StringRef Filename = Literal.GetString();
            const clang::DirectoryLookup *CurDir;
            const clang::FileEntry *File = PP.LookupFile(
#if CLANG_VERSION_MAJOR!=3 || CLANG_VERSION_MINOR>3
                Val->getLocStart(),
#endif
                Filename, false, nullptr, CurDir, nullptr, nullptr, nullptr);

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



static void parseEnums(ClassDef &Def, bool isFlag, clang::Expr *Content, clang::Sema &Sema) {
    clang::Preprocessor &PP = Sema.getPreprocessor();
    clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(Content);
    if (!Val) {
        PP.getDiagnostics().Report(Content->getExprLoc(),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                   "Invalid Q_ENUMS annotation"));
        return;
    }

    llvm::MemoryBuffer* Buf = llvm::MemoryBuffer::getMemBufferCopy(Val->getString(), "Q_ENUMS");
    clang::Lexer Lex(PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Content->getExprLoc()),
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
                if (Sema.ActOnCXXNestedNameSpecifier(Sema.getScopeForContext(Def.Record), *II,
                        GetFromLiteral(Tok, Val, PP), GetFromLiteral(Next, Val, PP), {}, false, SS))
                    SS.SetInvalid({GetFromLiteral(Tok, Val, PP), GetFromLiteral(Next, Val, PP)});

                Lex.LexFromRawLexer(Next);
                continue;
            }

            clang::LookupResult Found(Sema, II, GetFromLiteral(Tok, Val, PP), clang::Sema::LookupNestedNameSpecifierName);
            if (SS.isEmpty())
                Sema.LookupQualifiedName(Found, Def.Record);
            else {
                clang::DeclContext* DC = Sema.computeDeclContext(SS);
                Sema.LookupQualifiedName(Found, DC ? DC : Def.Record);
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
            if (R->getDeclContext() == Def.Record) {
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

static std::pair<clang::StringLiteral*, clang::StringLiteral *> ExtractLiterals(clang::ParenExpr *PE,
                                                                                const clang::Preprocessor &PP,
                                                                                const char *Keyword,
                                                                                const char *Error) {
    clang::BinaryOperator* BO = llvm::dyn_cast<clang::BinaryOperator>(PE->getSubExpr());
    clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
    if (!BO) {
        PP.getDiagnostics().Report(PE->getExprLoc(),
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

ClassDef MocNg::parseClass(clang::CXXRecordDecl* RD, clang::Sema& Sema)
{
    clang::Preprocessor &PP = Sema.getPreprocessor();
    ClassDef Def;
    Def.Record = RD;

    for (auto it = RD->decls_begin(); it != RD->decls_end(); ++it) {
        if (clang::StaticAssertDecl *S = llvm::dyn_cast<clang::StaticAssertDecl>(*it) ) {
            if (auto *E = llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(S->getAssertExpr()))
                if (clang::ParenExpr *PE = llvm::dyn_cast<clang::ParenExpr>(E->getArgumentExpr()))
            {
                llvm::StringRef key = S->getMessage()->getString();
                if (key == "qt_property") {
                    clang::StringLiteral *Val = llvm::dyn_cast<clang::StringLiteral>(PE->getSubExpr());
                    if (Val) {
                        PropertyParser Parser(Val->getString(),
    //                                          Val->getStrTokenLoc(0),
                                            Val->getLocationOfByte(0, PP.getSourceManager(), PP.getLangOpts(), PP.getTargetInfo()),
                                            Sema, Def.Record, &registered_meta_type);
                        Def.Properties.push_back(Parser.parseProperty());
                        Def.addExtra(Parser.Extra);
                    } else {
                        PP.getDiagnostics().Report(S->getLocation(),
                                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                   "Invalid Q_PROPERTY annotation"));
                    }
                } else if (key == "qt_private_property") {
                    clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
                    std::tie(Val1, Val2) = ExtractLiterals(PE, PP, "Q_PRIVATE_PROPERTY",
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
                    std::tie(Val1, Val2) = ExtractLiterals(PE, PP, "Q_PRIVATE_SLOT",
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
                    parseEnums(Def, false, PE->getSubExpr(), Sema);
                } else if (key == "qt_flags")  {
                    parseEnums(Def, true, PE->getSubExpr(), Sema);
                } else if (key == "qt_qobject") {
                    Def.HasQObject = true;
                } else if (key == "qt_qgadget") {
                    Def.HasQGadget = true;
                } else if (key == "qt_classinfo") {
                    clang::StringLiteral *Val1 = nullptr, *Val2 = nullptr;
                    std::tie(Val1, Val2) = ExtractLiterals(PE, PP, "Q_CLASSINFO",
                                                           "Expected string literal in Q_CLASSINFO");

                    if (Val1 && Val2) {
                        Def.ClassInfo.emplace_back(Val1->getString(), Val2->getString());
                    }
                } else if (key == "qt_interfaces") {
                    parseInterfaces(Def, PE->getSubExpr(), Sema);
                } else if (key == "qt_plugin_metadata") {
                    parsePluginMetaData(Def, PE->getSubExpr(), Sema);
                    HasPlugin = true;
                }
            }
        } else if (clang::CXXMethodDecl *M = llvm::dyn_cast<clang::CXXMethodDecl>(*it)) {
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
            for (clang::CXXMethodDecl *MD : Def.Signals) {
                if (MD->getName() == P.notify.Str) {
                    P.notify.notifyId = Idx;
                    P.notify.MD = MD;
                    break;
                }
                Idx += 1 + MD->getNumParams() - MD->getMinRequiredArguments();
            }
            if (P.notify.notifyId < 0 ) {
                PP.getDiagnostics().Report(P.notify.Loc,
                        PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                        "NOTIFY signal '%0' of property '%1' does not exist in class %2"))
                    << P.notify.Str << P.name << Def.Record;
            } else {
                Def.NotifyCount++;
            }
        }

        if (P.revision > 0)
            Def.RevisionPropertyCount++;
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
    if (it_before != Tags.end() && it_before == (--it_after)) {
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

    const clang::ClassTemplateSpecializationDecl* TD = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(T->getAsCXXRecordDecl());
    if (TD) {
        if (!TD->hasDefinition())
            return false;
        for (int I = 0; I < TD->getTemplateArgs().size(); ++I) {
            const auto &Arg = TD->getTemplateArgs().get(I);
            if (Arg.getKind() == clang::TemplateArgument::Type) {
                if (!ShouldRegisterMetaType(Arg.getAsType()))
                    return false;
            }
        }
    }
    return true;
}

