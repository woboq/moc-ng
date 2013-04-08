
#include <clang/Frontend/FrontendPluginRegistry.h>

#include "mocastconsumer.h"

class MocPluginAction : public clang::PluginASTAction {
protected:
    clang::ASTConsumer *CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef f) override {
        return new MocASTConsumer(CI);
    }
    bool ParseArgs(const clang::CompilerInstance& CI, const std::vector< std::string >& arg) override {
        return true;
    }
};


static clang::FrontendPluginRegistry::Add<MocPluginAction>
X("moc", "create the dxr index database");
