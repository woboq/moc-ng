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

#include "mocppcallbacks.h"
#include "mocastconsumer.h"
#include "clangversionabstraction.h"
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/Sema/Sema.h>
#include <clang/Basic/MacroBuilder.h>


void MocASTConsumer::Initialize(clang::ASTContext& Ctx) {
    ctx = &Ctx;
    PPCallbacks = new MocPPCallbacks(ci.getPreprocessor(), Moc.Tags);
    ci.getPreprocessor().addPPCallbacks(maybe_unique(PPCallbacks));
//   ci.getDiagnostics().setClient(new DiagnosticClient(), true);

    // We will enable this when we require Qt >= 5.6.1 and libclang >= 3.8
    // Then we will be able to get rid of qobjectdefs-injected
#if 0
    std::string qtPredefinesBuffer;
    llvm::raw_string_ostream qtPredefines(qtPredefinesBuffer);
    clang::MacroBuilder builder(qtPredefines);
    builder.append("# 1 \"<moc-ng built-in>\" 1");
    builder.defineMacro("QT_ANNOTATE_CLASS(type,...)", "static_assert(sizeof(#__VA_ARGS__),#type);");
    builder.defineMacro("QT_ANNOTATE_CLASS2(type,a1,a2)", "static_assert(sizeof(#a1,#a2),#type);");
    builder.defineMacro("QT_ANNOTATE_FUNCTION(a)", "__attribute__((annotate(#a)))");
    builder.defineMacro("QT_ANNOTATE_ACCESS_SPECIFIER(a)", "__attribute__((annotate(#a)))");
    builder.defineMacro("Q_CLASSINFO(name,value)", "static_assert(sizeof(name,value),\"qt_classinfo\");");
    builder.defineMacro("Q_REVISION(v)", "__attribute__((annotate(\"qt_revision:\" QT_STRINGIFY2(v))))");
    // prepend the Qt defines so the command line argument can override them.
    ci.getPreprocessor().setPredefines(qtPredefines.str() + ci.getPreprocessor().getPredefines());
#endif
}


void MocASTConsumer::HandleTagDeclDefinition(clang::TagDecl* D)
{
    clang::CXXRecordDecl *RD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
    if (!RD)
        return;

    if (!shouldParseDecl(D))
        return;

    clang::ClassTemplateSpecializationDecl* TD = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(RD);
    if (TD && TD->getIdentifier() && TD->getName() == "QMetaTypeId" && TD->getTemplateArgs().size() == 1) {
        Moc.registered_meta_type.insert(TD->getTemplateArgs().get(0).getAsType()->getCanonicalTypeUnqualified().getTypePtr());
    }

    if (TD) {
        // Do not parse class for class template specialization
        return;
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

bool MocASTConsumer::HandleTopLevelDecl(clang::DeclGroupRef D)
{
    for (clang::Decl *Decl : D) {
        if (clang::NamespaceDecl *NS = llvm::dyn_cast<clang::NamespaceDecl>(Decl)) {
            if (!shouldParseDecl(Decl))
                continue;
            HandleNamespaceDefinition(NS);
        }
    }
    return clang::ASTConsumer::HandleTopLevelDecl(D);
}

template<typename T>
static void operator+=(std::vector<T> &v1, const std::vector<T> &v2)
{
    v1.insert(v1.end(), v2.begin(), v2.end());
}

void MocASTConsumer::HandleNamespaceDefinition(clang::NamespaceDecl* D)
{
    // Try to find Q_NAMESPACE
    NamespaceDef Def = Moc.parseNamespace(D, ci.getSema());
    if (Def.hasQNamespace) {
        auto Canonical = D->getCanonicalDecl();
        auto it = std::find_if(namespaces.begin(), namespaces.end(), [&](const NamespaceDef &d)
            { return d.Namespace && d.Namespace->getCanonicalDecl() == Canonical; });
        if (it == namespaces.end()) {
            namespaces.push_back(std::move(Def));
        } else {
            // merge the two.
            it->Enums += Def.Enums;
            it->Extra += Def.Extra;
            it->ClassInfo += Def.ClassInfo;
        }
        ci.getPreprocessor().enableIncrementalProcessing();
    }
    for (auto it = D->decls_begin(); it != D->decls_end(); ++it) {
        if (clang::NamespaceDecl *NS = llvm::dyn_cast<clang::NamespaceDecl>(*it))
            HandleNamespaceDefinition(NS);
    }
}

