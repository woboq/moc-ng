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

#include "mocppcallbacks.h"
#include "mocastconsumer.h"
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/Sema/Sema.h>


#include <iostream>

void MocASTConsumer::Initialize(clang::ASTContext& Ctx) {
    ctx = &Ctx;
    PPCallbacks = new MocPPCallbacks(ci.getPreprocessor(), Moc.Tags);
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

