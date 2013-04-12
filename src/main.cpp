/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


#include <clang/Frontend/FrontendAction.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Driver/Driver.h>
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Tool.h>


#include <clang/Driver/Job.h>
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include <clang/Lex/Preprocessor.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclCXX.h>
#include <llvm/Support/Host.h>

#include <vector>
#include <iostream>

#include "mocastconsumer.h"
#include "generator.h"

#if 0
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
}
#endif


struct MocOptions {
  bool NoInclude = false;
  std::vector<std::string> Includes;
  std::string Output;
} Options;



struct MocNGASTConsumer : public MocASTConsumer {
    std::string InFile;
    MocNGASTConsumer(clang::CompilerInstance& ci, llvm::StringRef InFile) : MocASTConsumer(ci), InFile(InFile) {}

    void HandleTranslationUnit(clang::ASTContext& Ctx) override {

        if (ci.getDiagnostics().hasErrorOccurred())
          return;

        llvm::raw_ostream *OS = ci.createOutputFile(Options.Output, false);
        if (!OS) return;
        llvm::raw_ostream &Out = *OS;

        Out <<  "/****************************************************************************\n"
                "** Meta object code from reading C++ file '" << InFile << "'\n"
                "**\n"
                "** Created by MOC-NG version " MOCNG_VERSION_STR " by Woboq [http://woboq.com]\n"
                "** WARNING! All changes made in this file will be lost!\n"
                "*****************************************************************************/\n\n";

        if (!Options.NoInclude) {
          for (auto &s : Options.Includes) {
            Out << s << "\n";
          }
          if (llvm::StringRef(InFile).endswith(".h"))
            Out << "#include \"" << InFile << "\"\n";
        }

        Out << "#if !defined(Q_MOC_OUTPUT_REVISION)\n"
               "#error \"The header file '" << InFile << "' doesn't include <QObject>.\"\n"
               "#elif Q_MOC_OUTPUT_REVISION != " << mocOutputRevision << "\n"
               "#error \"This file was generated using MOC-NG " MOCNG_VERSION_STR ".\"\n"
               "#error \"It cannot be used with the include files from this version of Qt.\"\n"
               "#endif\n\n"
               "QT_BEGIN_MOC_NAMESPACE\n";


        int Count = 0;
        for (const ClassDef &Def : objects ) {
          if (!Def.Record || !Def.Record->getDefinition())
            continue;
          auto SL = Def.Record->getDefinition()->getSourceRange().getBegin();
          SL = ci.getSourceManager().getExpansionLoc(SL);
          if (ci.getSourceManager().getFileID(SL) != ci.getSourceManager().getMainFileID())
            continue;
          Count++;
          Generator G(&Def, Out, Ctx);
          G.GenerateCode();
        };

        Out << "QT_END_MOC_NAMESPACE\n";

        if (!Count)
          ci.getDiagnostics().Report(ci.getSourceManager().getLocForStartOfFile(ci.getSourceManager().getMainFileID()),
                  ci.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                      "No Q_OBJECT class found in this file"));
    }
};


class MocAction : public clang::ASTFrontendAction {
protected:

    virtual clang::ASTConsumer *CreateASTConsumer(clang::CompilerInstance &CI,
                                           llvm::StringRef InFile) override {

        CI.getFrontendOpts().SkipFunctionBodies = true;
        CI.getPreprocessor().enableIncrementalProcessing(true);
        CI.getPreprocessor().SetSuppressIncludeNotFoundError(true);
        CI.getLangOpts().DelayedTemplateParsing = true;

        //enable all the extension
        CI.getLangOpts().MicrosoftExt = true;
        CI.getLangOpts().DollarIdents = true;
        CI.getLangOpts().CPlusPlus11 = true;
        CI.getLangOpts().CPlusPlus1y = true;
        CI.getLangOpts().GNUMode = true;

        return new MocNGASTConsumer(CI, InFile);
    }

public:
    // CHECK
    virtual bool hasCodeCompletionSupport() const { return true; }
};



int main(int argc, const char **argv)
{
  std::vector<std::string> Argv;
  Argv.push_back(argv[0]);
  Argv.push_back("-x");  // Type need to go first
  Argv.push_back("c++");
  Argv.push_back("-fsyntax-only");
  Argv.push_back("-fPIE");

  Options.Output = "-";

  for (int I = 1 ; I < argc; ++I) {
    if (argv[I][0] == '-') {
      if (argv[I][1] == 'o') {
        if (argv[I][2]) Options.Output = &argv[I][2];
        else if ((++I) < argc) Options.Output = argv[I];
        continue;
      } else if (argv[I][1] == 'i') {
        Options.NoInclude = true;
        continue;
      }
    }
    Argv.push_back(argv[I]);
  }

  //FIXME
  Argv.push_back("-I/usr/include/qt5");
  Argv.push_back("-I/usr/include/qt5/QtCore");
  Argv.push_back("-I/usr/lib/clang/3.2/include/");

  clang::FileManager FM({"."});
  clang::tooling::ToolInvocation Inv(Argv, new MocAction, &FM);
  return !Inv.run();


#if 0

  clang::FileManager Files({"."});


  const char *const BinaryName = Argv[0];
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> DiagOpts = new clang::DiagnosticOptions();
  clang::TextDiagnosticPrinter DiagnosticPrinter(llvm::errs(), &*DiagOpts);
  clang::DiagnosticsEngine Diagnostics(
    llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs>(new clang::DiagnosticIDs()),
                                &*DiagOpts, &DiagnosticPrinter, false);

  const std::string DefaultOutputName = "a.out"; //FIXME
  llvm::OwningPtr<clang::driver::Driver> Driver ( new clang::driver::Driver(BinaryName,
                              llvm::sys::getDefaultTargetTriple(), DefaultOutputName, false, Diagnostics));
  Driver->setTitle("moc-ng");
//  Driver->setCheckInputsExist(false);
  const llvm::OwningPtr<clang::driver::Compilation> Compilation(
      Driver->BuildCompilation(llvm::makeArrayRef(Argv)));


  const clang::driver::JobList &Jobs = Compilation->getJobs();
  if (Jobs.size() != 1 || !llvm::isa<clang::driver::Command>(*Jobs.begin())) {
    llvm::SmallString<256> error_msg;
    llvm::raw_svector_ostream error_stream(error_msg);
    Compilation->PrintJob(error_stream, Compilation->getJobs(), "; ", true);
//    Diagnostics.Report(clang::diag::err_fe_expected_compiler_job) << error_stream.str();
    return 0;
  }
  // The one job we find should be to invoke clang again.
  const clang::driver::Command *Cmd = llvm::cast<clang::driver::Command>(*Jobs.begin());
  if (llvm::StringRef(Cmd->getCreator().getName()) != "clang") {
  //  Diagnostics.Report(clang::diag::err_fe_expected_clang_command);
    return 0;
  }
  const clang::driver::ArgStringList *const CC1Args = &Cmd->getArguments();
  llvm::OwningPtr<clang::CompilerInvocation> Invocation ( new clang::CompilerInvocation );
  clang::CompilerInvocation::CreateFromArgs(*Invocation, CC1Args->data() + 1, CC1Args->data() + CC1Args->size(), Diagnostics);
//  Invocation->getFrontendOpts().DisableFree = false;  // FIXME

  if (Invocation->getHeaderSearchOpts().Verbose) {
    llvm::errs() << "clang Invocation:\n";
    Compilation->PrintJob(llvm::errs(), Compilation->getJobs(), "\n", true);
    llvm::errs() << "\n";
  }
  // Create a compiler instance to handle the actual work.
  clang::CompilerInstance Compiler;
  Compiler.setInvocation(Invocation.take());
  Compiler.setFileManager(&Files);
  // FIXME: What about LangOpts?

  // ToolAction can have lifetime requirements for Compiler or its members, and
  // we need to ensure it's deleted earlier than Compiler. So we pass it to an
  // OwningPtr declared after the Compiler variable.
  llvm::OwningPtr<clang::FrontendAction> ScopedToolAction(new MocAction);
  // Create the compilers actual diagnostics engine.
  Compiler.createDiagnostics(argc, argv);
  if (!Compiler.hasDiagnostics())
    return false;
  Compiler.createSourceManager(Files);
  const bool Success = Compiler.ExecuteAction(*ScopedToolAction);
  Compiler.resetAndLeakFileManager();
  Files.clearStatCaches();
  return Success;

#endif
}




