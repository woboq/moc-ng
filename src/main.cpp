/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


#include <clang/Frontend/FrontendAction.h>
#include <clang/Tooling/Tooling.h>

#include <vector>
#include <iostream>

#include "mocastconsumer.h"


namespace Options {

namespace cl = llvm::cl;


cl::list<std::string> SourcePaths(
    cl::Positional,
    cl::desc("<source0> [... <sourceN>]"),
    cl::OneOrMore);

cl::opt<std::string> OutputPath(
    "o",
    cl::desc("write output to file rather than stdout"),
    cl::Optional);

cl::list<std::string> IncludePaths(
    "I",
    cl::desc("add dir to the include path for header files"),
    cl::ZeroOrMore);

//   -E                 preprocess only; do not generate meta object code

cl::list<std::string> Defines(
    "D",
    cl::desc("define macro, with optional definition"),
    cl::ZeroOrMore);

cl::list<std::string> Undef(
    "U",
    cl::desc("undefine macro"),
    cl::ZeroOrMore);

cl::opt<bool> NoInclude(
    "i",
    cl::desc("do not generate an #include statement"),
    cl::Optional);

cl::opt<std::string> PathPrefix(
    "p",
    cl::desc("path prefix for included file"),
    cl::Optional);


cl::opt<std::string> ForceInclude(
    "f",
    cl::desc("force #include, optional file name (overwrite default)"),
    cl::ZeroOrMore);

cl::opt<std::string> PrependInclude(
    "b",
    cl::desc("prepend #include <file> (preserve default include)"),
    cl::ZeroOrMore);

cl::opt<bool> NoNotes(
    "nn",
    cl::desc("do not display nodes"),
    cl::Optional);

cl::opt<bool> NoWarnings(
    "nw",
    cl::desc("do not display warnings"),
    cl::Optional);

cl::opt<bool> Version(
    "v",
    cl::desc("display version"),
    cl::Optional);


/*

cl::opt<std::string> BuildPath(
  "b",
  cl::desc("<build-path>"),
  cl::Optional);




cl::list<std::string> ProjectPaths(
    "p",
    cl::desc("<project>:<path>[:<revision>]"),
    cl::ZeroOrMore);


cl::list<std::string> ExternalProjectPaths(
    "e",
    cl::desc("<project>:<path>:<url>"),
    cl::ZeroOrMore);

cl::opt<std::string> DataPath(
    "d",
    cl::desc("<data path>"),
    cl::Optional);
*/

}


class MocAction : public clang::ASTFrontendAction {
protected:
    virtual clang::ASTConsumer *CreateASTConsumer(clang::CompilerInstance &CI,
                                           llvm::StringRef InFile) override {

        CI.getFrontendOpts().SkipFunctionBodies = true;
        return new MocASTConsumer(CI);
    }

public:
    // CHECK
    virtual bool hasCodeCompletionSupport() const { return true; }
};



int main(int argc, const char **argv) {

    std::vector<std::string> Args;
    for (int i = 0; i < argc; ++i)
        Args.push_back(argv[i]);

    Args.push_back("-fsyntax-only");
    Args.push_back("-x");
    Args.push_back("c++");
    clang::FileManager FM({"."});
    clang::tooling::ToolInvocation Inv(Args, new MocAction, &FM);
    return Inv.run();
/*
    llvm::cl::ParseCommandLineOptions(argc, argv);
    clang::tooling::FixedCompilationDatabase DB(".", Args);


    clang::tooling::ClangTool Tool(DB, Options::SourcePaths);

    return Tool.run(clang::tooling::newFrontendActionFactory<MocAction>());
*/
//    return !Inv.run();

#if 0
  llvm::OwningPtr<CompilationDatabase> Compilations(
    FixedCompilationDatabase::loadFromCommandLine(argc, argv));
  llvm::cl::ParseCommandLineOptions(argc, argv);
  if (!Compilations) {
    std::string ErrorMessage;
    Compilations.reset(CompilationDatabase::loadFromDirectory(BuildPath,
                                                            ErrorMessage));

    /*if (!BuildPath.empty()) {
      Compilations.reset(
         CompilationDatabase::autoDetectFromDirectory(BuildPath, ErrorMessage));
    } else {
      Compilations.reset(CompilationDatabase::autoDetectFromSource(
          SourcePaths[0], ErrorMessage));
    }*/
    if (!Compilations)
      llvm::report_fatal_error(ErrorMessage);
  }
  int i;
;
  ClangTool Tool(*Compilations, SourcePaths);
  return Tool.run(newFrontendActionFactory<MyAction>());
#endif
}




