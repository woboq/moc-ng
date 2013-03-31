/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#include "mocppcallbacks.h"
#include "mocastconsumer.h"
#include "generator.h"
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>

#include <iostream>

static bool IsQtVirtual(const clang::CXXMethodDecl *MD) {
    if (!MD->getIdentifier())
        return false;
    auto Name = MD->getName();
    return (Name == "qt_metacall" || Name == "qt_metacast" || Name == "metaObject"
            || Name == "qt_static_metacall");
}

void MocASTConsumer::Initialize(clang::ASTContext& Ctx) {
    ctx = &Ctx;
    PPCallbacks = new MocPPCallbacks(ci.getPreprocessor());
    ci.getPreprocessor().addPPCallbacks(PPCallbacks);
//   ci.getDiagnostics().setClient(new DiagnosticClient(), true);
}

bool MocASTConsumer::HandleTopLevelDecl(clang::DeclGroupRef D) {

    auto &PP = ci.getPreprocessor();

    if (done) {
        PP.enableIncrementalProcessing(false);
        return true;
    }

#if 0
    //TODO:  make use of this to check that interfaces are registered.
    if (D.isSingleDecl()) {
        clang::FunctionDecl* FD = llvm::dyn_cast<clang::FunctionDecl>(D.getSingleDecl());
        if (FD && FD->getIdentifier() && FD->getName() == "qobject_interface_iid" ) {

            do {
                const clang::TemplateArgumentList* Args = FD->getTemplateSpecializationArgs();
                if (!Args || Args->size() != 1 || Args->get(0).getKind() != clang::TemplateArgument::Type)
                    break;
                const clang::CXXRecordDecl *RC = Args->get(0).getAsType()->getPointeeCXXRecordDecl();
                if (!RC) break;
                auto *Body = llvm::dyn_cast_or_null<clang::CompoundStmt>(FD->getBody());
                if (!Body) break;
                auto *Ret = llvm::dyn_cast_or_null<clang::ReturnStmt>(Body->body_back());
                if (!Ret) break;
                auto *Cast = llvm::dyn_cast_or_null<clang::CastExpr>(Ret->getRetValue());
                if (!Cast) break;
                clang::StringLiteral *Lit = llvm::dyn_cast_or_null<clang::StringLiteral>(Cast->getSubExpr());
                if (!Lit) break;

                Moc.interfaces.insert( {Lit->getString(), RC});
            } while (false);

        }
    }
#endif


    if (!objects.size())
        return true;

    if (!PPCallbacks->IsInMainFile)
        return true;


    PP.getCurrentFileLexer();

    PP.EnableBacktrackAtThisPos();
    clang::Token Tok;
    PP.Lex(Tok);

    while(Tok.is(clang::tok::semi))
        PP.Lex(Tok);

    if (Tok.is(clang::tok::eof)) {
        done = true;
        PP.CommitBacktrackedTokens();
        std::string code = generate();
        if (!code.empty()) {
            std::cout << code << std::endl;
            objects.clear();
            auto Buf = llvm::MemoryBuffer::getMemBufferCopy( code, "qt_moc");
            clang::SourceLocation Loc = PP.getSourceManager().getFileLoc(D.getSingleDecl()->getLocEnd());
            PP.EnterSourceFile( PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Loc), nullptr, Loc);
        } else {
            ci.getPreprocessor().enableIncrementalProcessing(false);
            PP.Backtrack();
        }
    } else {
        PP.Backtrack();
    }
    return true;
}


void MocASTConsumer::HandleTagDeclDefinition(clang::TagDecl* D)
{
    clang::CXXRecordDecl *RD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
    if (!RD)
        return;

    clang::ClassTemplateSpecializationDecl* TD = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(RD);
    if (TD && TD->getIdentifier() && TD->getName() == "QMetaTypeId" && TD->getTemplateArgs().size() == 1) {
        const clang::EnumType* ET = TD->getTemplateArgs().get(0).getAsType()->getAs<clang::EnumType>();
        if (ET)
            Moc.registered_meta_type.insert(ET->getDecl());
    }


    /*if (!(PPCallbacks->seenQ_OBJECT.isValid() &&
            ctx->getSourceManager().isBeforeInTranslationUnit(D->getSourceRange().getBegin(),
                        PPCallbacks->seenQ_OBJECT) &&
            ctx->getSourceManager().isBeforeInTranslationUnit(PPCallbacks->seenQ_OBJECT,
                        D->getSourceRange().getEnd())
         ))
        return;

    PPCallbacks->seenQ_OBJECT = {};*/

    ClassDef Def = Moc.parseClass(RD, ci.getSema());
    if (Def.HasQObject || Def.HasQGadget) {
        objects.push_back(std::move(Def));
        ci.getPreprocessor().enableIncrementalProcessing();
    }
}


std::string MocASTConsumer::generate()
{
    std::string Code;
    llvm::raw_string_ostream OS(Code);


    for (const ClassDef &Def : objects ) {

        auto RD = Def.Record;

        // find a key function: first non inline virtual method
#if  CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
        const clang::CXXMethodDecl *Key = ctx->getCurrentKeyFunction(RD);
#else
        const clang::CXXMethodDecl *Key = ctx->getKeyFunction(RD);
#endif
        if (Key &&  IsQtVirtual(Key))
            Key = nullptr;

        if (!Key) {
            for (auto it = RD->method_begin(); it != RD->method_end(); ++it ) {

                if (Key && !it->isVirtual())
                    continue;

                if (it->isPure() || it->isImplicit() || it->hasInlineBody()
                        || it->isInlineSpecified() || !it->isUserProvided() )
                    continue;

                const clang::FunctionDecl *Def;
                if (it->hasBody(Def) && Def->isInlineSpecified())
                    continue;

                if (IsQtVirtual(*it))
                    continue;

                /* if (Key->isFunctionTemplateSpecialization())
                     continue; */

                if (std::any_of(it->specific_attr_begin<clang::AnnotateAttr>(),
                                it->specific_attr_end<clang::AnnotateAttr>(),
                                [](clang::AnnotateAttr *A) {
                                    return A->getAnnotation() == "qt_signal";
                                }))
                    continue;

                Key = *it;
                if (Key->isVirtual())
                    break;
            }
        }

        if (Key && !Key->hasBody())
            continue;


        Generator G(&Def, OS, *ctx);
        G.GenerateCode();
    }
    return Code;

}


