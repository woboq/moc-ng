/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#include "mocppcallbacks.h"
#include "mocastconsumer.h"
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/Sema/Sema.h>


#include <iostream>

void MocASTConsumer::Initialize(clang::ASTContext& Ctx) {
    ctx = &Ctx;
    PPCallbacks = new MocPPCallbacks(ci.getPreprocessor());
    ci.getPreprocessor().addPPCallbacks(PPCallbacks);
//   ci.getDiagnostics().setClient(new DiagnosticClient(), true);
}


void MocASTConsumer::HandleTagDeclDefinition(clang::TagDecl* D)
{
    clang::CXXRecordDecl *RD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
    if (!RD)
        return;

    clang::ClassTemplateSpecializationDecl* TD = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(RD);
    if (TD && TD->getIdentifier() && TD->getName() == "QMetaTypeId" && TD->getTemplateArgs().size() == 1) {
        Moc.registered_meta_type.insert(TD->getTemplateArgs().get(0).getAsType()->getCanonicalTypeUnqualified().getTypePtr());
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

