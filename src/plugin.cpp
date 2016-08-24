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

#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Attr.h>

#include "mocastconsumer.h"
#include "mocppcallbacks.h"
#include "generator.h"

static bool IsQtInternal(const clang::CXXMethodDecl *MD) {
  if (!MD->getIdentifier())
    return false;
  auto Name = MD->getName();
  // qt_metacall, qt_metacast, qt_static_metacall, qt_check_for_QGADGET_macro
  return (Name.startswith("qt_") || Name == "metaObject");
}

class MocPluginASTConsumer : public MocASTConsumer {
    bool done = false;

    bool HandleTopLevelDecl(clang::DeclGroupRef D) override {
      MocASTConsumer::HandleTopLevelDecl(D);
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


      if (!objects.size() && !namespaces.size())
        return true;

      if (!PPCallbacks->IsInMainFile)
        return true;

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
          objects.clear();
          namespaces.clear();
          auto Buf = maybe_unique(llvm::MemoryBuffer::getMemBufferCopy(code, "qt_moc"));
          PP.EnterSourceFile(CreateFileIDForMemBuffer(PP, Buf, {}), nullptr, {});
        } else {
          ci.getPreprocessor().enableIncrementalProcessing(false);
          PP.Backtrack();
        }
      } else {
        PP.Backtrack();
      }
      return true;
    }

    std::string generate()
    {
      std::string Code;
      llvm::raw_string_ostream OS(Code);

      for (const ClassDef &Def : objects ) {
          auto RD = Def.Record;

          // find a key function: first non inline virtual method
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
          const clang::CXXMethodDecl *Key = ctx->getCurrentKeyFunction(RD);
#else
          const clang::CXXMethodDecl *Key = ctx->getKeyFunction(RD);
#endif
          if (Key &&  IsQtInternal(Key))
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

                  if (IsQtInternal(*it))
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

          Generator G(&Def, OS, *ctx, &Moc);
          G.GenerateCode();
      }
      for (const NamespaceDef &Def : namespaces) {
            const clang::FunctionDecl *Key = nullptr;
            for (auto it = Def.Namespace->decls_begin(); it != Def.Namespace->decls_end(); ++it) {
                Key = llvm::dyn_cast<const clang::FunctionDecl>(*it);
                if (Key)
                    break;
                if (auto RD = llvm::dyn_cast<const clang::CXXRecordDecl>(*it)) {
                    Key = ctx->getCurrentKeyFunction(RD);
                    if (Key)
                        break;
                }
            }
            if (Key && !Key->hasBody())
                continue;
            Generator G(&Def, OS, *ctx, &Moc);
            G.GenerateCode();
      }
      return Code;
    }

public:
    MocPluginASTConsumer(clang::CompilerInstance& ci) : MocASTConsumer(ci) {}
};

class MocPluginAction : public clang::PluginASTAction {
protected:
    #if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR <= 5
    clang::ASTConsumer *
    #else
    std::unique_ptr<clang::ASTConsumer>
    #endif
    CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef f) override {
        return maybe_unique(new MocPluginASTConsumer(CI));
    }
    bool ParseArgs(const clang::CompilerInstance& CI, const std::vector< std::string >& arg) override {
        return true;
    }
};


static clang::FrontendPluginRegistry::Add<MocPluginAction>
X("moc", "Qt MetaObjectCompiler");
