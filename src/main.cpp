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

#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Driver/Driver.h>
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Tool.h>
#include <clang/Basic/DiagnosticIDs.h>
#include <clang/Lex/LexDiagnostic.h>

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
#include "mocppcallbacks.h"
#include "embedded_includes.h"

struct MocOptions {
  bool NoInclude = false;
  std::vector<std::string> Includes;
  std::string Output;
  std::string OutputTemplateHeader;
  std::vector<std::pair<llvm::StringRef, llvm::StringRef>> MetaData;
  void addOutput(llvm::StringRef);
} Options;

void MocOptions::addOutput(llvm::StringRef Out)
{
    if (Output.empty()) {
        Output = Out.str();
    } else if (OutputTemplateHeader.empty()) {
        OutputTemplateHeader = Out.str();
    } else {
        std::cerr << "moc-ng: Too many output file specified" << std::endl;
    }
}


/* Proxy that changes some errors into warnings  */
struct MocDiagConsumer : clang::DiagnosticConsumer {
    std::unique_ptr<DiagnosticConsumer> Proxy;
    MocDiagConsumer(std::unique_ptr<DiagnosticConsumer> Previous) : Proxy(std::move(Previous))  {}

    int HadRealError = 0;

#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR <= 2
    DiagnosticConsumer* clone(clang::DiagnosticsEngine& Diags) const override {
        return new MocDiagConsumer { Proxy->clone(Diags) };
    }
#endif
    void BeginSourceFile(const clang::LangOptions& LangOpts, const clang::Preprocessor* PP = 0) override {
        Proxy->BeginSourceFile(LangOpts, PP);
    }
    void clear() override {
        Proxy->clear();
    }
    void EndSourceFile() override {
        Proxy->EndSourceFile();
    }
    void finish() override {
        Proxy->finish();
    }
    void HandleDiagnostic(clang::DiagnosticsEngine::Level DiagLevel, const clang::Diagnostic& Info) override {

        /* Moc ignores most of the errors since it even can operate on non self-contained headers.
         * So try to change errors into warning.
         */

        auto DiagId = Info.getID();
        auto Cat = Info.getDiags()->getDiagnosticIDs()->getCategoryNumberForDiag(DiagId);

        bool ShouldReset = false;

        if (DiagLevel >= clang::DiagnosticsEngine::Error ) {
            if (Cat == 2 || Cat == 4
                || DiagId == clang::diag::err_param_redefinition
                || DiagId == clang::diag::err_pp_expr_bad_token_binop ) {
                if (!HadRealError)
                    ShouldReset = true;
                DiagLevel = clang::DiagnosticsEngine::Warning;
            } else {
                HadRealError++;
            }
        }

        DiagnosticConsumer::HandleDiagnostic(DiagLevel, Info);
        Proxy->HandleDiagnostic(DiagLevel, Info);

        if (ShouldReset) {
            // FIXME:  is there another way to ignore errors?
            const_cast<clang::DiagnosticsEngine *>(Info.getDiags())->Reset();
        }
    }
};



struct MocNGASTConsumer : public MocASTConsumer {
    std::string InFile;
    MocNGASTConsumer(clang::CompilerInstance& ci, llvm::StringRef InFile) : MocASTConsumer(ci), InFile(InFile) { }


#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR < 8
    // Clang 3.8 changed when Initialize is called. It is now called before the main file has been entered.
    // But with Clang < 3.8 it is called after, and PPCallbacks::FileChanged is not called when entering the main file
    void Initialize(clang::ASTContext& Ctx) override {
        MocASTConsumer::Initialize(Ctx);
        PPCallbacks->EnterMainFile(InFile);
    }
#endif

    void HandleTagDeclDefinition(clang::TagDecl* D) override {
        // We only want to parse the Qt macro in classes that are in the main file.
        auto SL = D->getSourceRange().getBegin();
        SL = ci.getSourceManager().getExpansionLoc(SL);
        if (ci.getSourceManager().getFileID(SL) != ci.getSourceManager().getMainFileID())
            return;
        MocASTConsumer::HandleTagDeclDefinition(D);
    }

    void HandleTranslationUnit(clang::ASTContext& Ctx) override {

        if (ci.getDiagnostics().hasErrorOccurred())
            return;

        if (!objects.size()) {
          ci.getDiagnostics().Report(ci.getSourceManager().getLocForStartOfFile(ci.getSourceManager().getMainFileID()),
                                     ci.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                                         "No relevant classes found. No output generated"));
          //actually still create an empty file like moc does.
          ci.createOutputFile(Options.Output, false, true, "", "", false, false);
          return;
        }

        // createOutputFile returns a raw_pwrite_stream* before Clang 3.9, and a std::unique_ptr<raw_pwrite_stream> after
        auto OS = ci.createOutputFile(Options.Output, false, true, "", "", false, false);

        if (!OS) return;
        llvm::raw_ostream &Out = *OS;

        auto WriteHeader = [&](llvm::raw_ostream & Out) {
            Out <<  "/****************************************************************************\n"
                    "** Meta object code from reading C++ file '" << InFile << "'\n"
                    "**\n"
                    "** Created by MOC-NG version " MOCNG_VERSION_STR " by Woboq [https://woboq.com]\n"
                    "** WARNING! All changes made in this file will be lost!\n"
                    "*****************************************************************************/\n\n";
        };
        WriteHeader(Out);

        if (!Options.NoInclude) {
          for (auto &s : Options.Includes) {
            Out << s << "\n";
          }

          auto spos = InFile.rfind('/');
          auto ppos = InFile.rfind('.');
          // If it finished by .h, or if there is no dot after a slash,  we should include the source file
          if (ppos == std::string::npos || (spos != std::string::npos && spos > ppos) || llvm::StringRef(InFile).endswith(".h"))
            Out << "#include \"" << InFile << "\"\n";
          if (Moc.HasPlugin)
            Out << "#include <QtCore/qplugin.h>\n";
        }

        Out << "#include <QtCore/qbytearray.h>\n";

        Out << "#if !defined(Q_MOC_OUTPUT_REVISION)\n"
               "#error \"The header file '" << InFile << "' doesn't include <QObject>.\"\n"
               "#elif Q_MOC_OUTPUT_REVISION != " << mocOutputRevision << "\n"
               "#error \"This file was generated using MOC-NG " MOCNG_VERSION_STR ".\"\n"
               "#error \"It cannot be used with the include files from this version of Qt.\"\n"
               "#endif\n\n"
               "QT_BEGIN_MOC_NAMESPACE\n";

        decltype(OS) OS_TemplateHeader = nullptr;
        if (!Options.OutputTemplateHeader.empty()) {
            OS_TemplateHeader =
                ci.createOutputFile(Options.OutputTemplateHeader, false, true, "", "", false, false);
            if (!OS_TemplateHeader)
                return;
            WriteHeader(*OS_TemplateHeader);
            (*OS_TemplateHeader) << "QT_BEGIN_MOC_NAMESPACE\n";
        }

        for (const ClassDef &Def : objects ) {
          Generator G(&Def, Out, Ctx, &Moc,
                      Def.Record->getDescribedClassTemplate() ? &*OS_TemplateHeader : nullptr);
          G.MetaData = Options.MetaData;
          if (llvm::StringRef(InFile).endswith("global/qnamespace.h"))
              G.IsQtNamespace = true;
          G.GenerateCode();
        };

        Out << "QT_END_MOC_NAMESPACE\n";
        if (OS_TemplateHeader) {
            (*OS_TemplateHeader) << "QT_END_MOC_NAMESPACE\n";
        }
    }
};

class MocAction : public clang::ASTFrontendAction {
protected:
#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR <= 5
    clang::ASTConsumer *
#else
    std::unique_ptr<clang::ASTConsumer>
#endif
    CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef InFile) override {

        CI.getFrontendOpts().SkipFunctionBodies = true;
        CI.getPreprocessor().enableIncrementalProcessing(true);
        CI.getPreprocessor().SetSuppressIncludeNotFoundError(true);
        CI.getLangOpts().DelayedTemplateParsing = true;

        //enable all the extension
        CI.getLangOpts().MicrosoftExt = true;
        CI.getLangOpts().DollarIdents = true;
        CI.getLangOpts().CPlusPlus11 = true;
#if CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR <= 5
        CI.getLangOpts().CPlusPlus1y = true;
#else
        CI.getLangOpts().CPlusPlus14 = true;
#endif
        CI.getLangOpts().GNUMode = true;

        CI.getDiagnostics().setClient(new MocDiagConsumer(
            std::unique_ptr<clang::DiagnosticConsumer>(CI.getDiagnostics().takeClient())));

        return maybe_unique(new MocNGASTConsumer(CI, InFile));
    }

public:
    // CHECK
    virtual bool hasCodeCompletionSupport() const { return true; }
};

static void showVersion(bool /*Long*/) {
    std::cerr << "moc-ng version " MOCNG_VERSION_STR " by Woboq [https://woboq.com]" << std::endl;
}

static void showHelp() {
    std::cerr << "Usage moc: [options] <header-file>\n"
              "  -o<file>           write output to file rather than stdout\n"
              "  -I<dir>            add dir to the include path for header files\n"
              "  -E                 preprocess only; do not generate meta object code\n"
              "  -D<macro>[=<def>]  define macro, with optional definition\n"
              "  -U<macro>          undefine macro\n"
              "  -M<key=valye>      add key/value pair to plugin meta data\n"
              "  -i                 do not generate an #include statement\n"
//               "  -p<path>           path prefix for included file\n"
//               "  -f[<file>]         force #include, optional file name\n"
//               "  -nn                do not display notes\n"
//               "  -nw                do not display warnings\n"
//               "  @<file>            read additional options from file\n"
              "  -v                 display version of moc-ng\n"
              "  -include <file>    Adds an implicit #include into the predefines buffer which is read before the source file is preprocessed\n"

/* undocumented options
              "  -W<warnings>       Enable the specified warning\n"
              "  -f<option>         clang option\n"
              "  -X<ext> <arg>      extensions arguments\n"
*/



              << std::endl;


    showVersion(false);
}


int main(int argc, const char **argv)
{
  bool PreprocessorOnly = false;
  std::vector<std::string> Argv;
  Argv.push_back(argv[0]);
  Argv.push_back("-x");  // Type need to go first
  Argv.push_back("c++");
  Argv.push_back("-fPIE");
  Argv.push_back("-fPIC");
  Argv.push_back("-Wno-microsoft"); // get rid of a warning in qtextdocument.h
  Argv.push_back("-std=c++11");

  bool NextArgNotInput = false;
  bool HasInput = false;

  for (int I = 1 ; I < argc; ++I) {
    if (argv[I][0] == '-') {
        NextArgNotInput = false;
        switch (argv[I][1]) {
            case 'h':
            case '?':
                showHelp();
                return EXIT_SUCCESS;
            case 'v':
                showVersion(true);
                return EXIT_SUCCESS;
            case 'o':
                if (argv[I][2]) Options.addOutput(&argv[I][2]);
                else if ((++I) < argc) Options.addOutput(argv[I]);
                continue;
            case 'i':
                if (argv[I] == llvm::StringRef("-i")) {
                    Options.NoInclude = true;
                    continue;
                } else if (argv[I] == llvm::StringRef("-include")) {
                    NextArgNotInput = true;
                    break;
                }
                goto invalidArg;
            case 'M': {
                llvm::StringRef Arg;
                if (argv[I][2]) Arg = &argv[I][2];
                else if ((++I) < argc) Arg = argv[I];
                size_t Eq = Arg.find('=');
                if (Eq == llvm::StringRef::npos) {
                    std::cerr << "moc-ng: missing key or value for option '-M'" << std::endl;
                    return EXIT_FAILURE;
                }
                Options.MetaData.push_back({Arg.substr(0, Eq), Arg.substr(Eq+1)});
                continue;
            }
            case 'E':
                PreprocessorOnly = true;
                break;
            case 'I':
            case 'U':
            case 'D':
                NextArgNotInput = (argv[I][2] == '\0');
                break;
            case 'X':
                NextArgNotInput = true;
                break;
            case 'f': //this is understood as compiler option rather than moc -f
            case 'W': // same
                break;
            case 'n': //not implemented, silently ignored
                continue;
            case '-':
                if (llvm::StringRef(argv[I]) == "--include" ||
                        llvm::StringRef(argv[I]).startswith("--include=")) {
                    llvm::StringRef File;
                    if (llvm::StringRef(argv[I]).startswith("--include=")) {
                        File = llvm::StringRef(argv[I]).substr(llvm::StringRef("--include=").size());
                    } else if (I + 1 < argc) {
                        File = llvm::StringRef(argv[++I]);
                    }
                    if (File.endswith("/moc_predefs.h")) {
                        // qmake generates moc_predefs with compiler defined stuff.
                        // We can't support it because everything is already pre-defined.
                        // So skip it.
                        continue;
                    }
                    Argv.push_back("-include");
                    Argv.push_back(File);
                    continue;
                }
                if (llvm::StringRef(argv[I]).startswith("--compiler-flavor")) {
                    if (llvm::StringRef(argv[I]) == "--compiler-flavor")
                        ++I;
                    // MSVC flavor not yet implemented
                    continue;
                }
                LLVM_FALLTHROUGH;
            default:
invalidArg:
                std::cerr << "moc-ng: Invalid argument '" << argv[I] << "'" << std::endl;
                showHelp();
                return EXIT_FAILURE;
        }
    } else if (!NextArgNotInput) {
        if (HasInput) {
            std::cerr << "error: Too many input files specified" << std::endl;
            return EXIT_FAILURE;
        }
        HasInput = true;
    }
    Argv.push_back(argv[I]);
  }

  if (Options.Output.empty())
    Options.Output = "-";


  //FIXME
  Argv.push_back("-I/usr/include/qt5");
  Argv.push_back("-I/usr/include/qt5/QtCore");
  Argv.push_back("-I/usr/include/qt");
  Argv.push_back("-I/usr/include/qt/QtCore");
  Argv.push_back("-I.uic"); // workaround the fact that the uic generated code cannot be found

  Argv.push_back("-I/builtins");

  clang::FileManager FM({"."});
  FM.Retain();

  if (PreprocessorOnly) {
      Argv.push_back("-P");
      clang::tooling::ToolInvocation Inv(Argv, new clang::PrintPreprocessedAction, &FM);
      return !Inv.run();
  }

  Argv.push_back("-fsyntax-only");

  Argv.push_back("-include"); Argv.push_back("QtCore/QObject"); // QObject should always be included

  clang::tooling::ToolInvocation Inv(Argv, new MocAction, &FM);

  const EmbeddedFile *f = EmbeddedFiles;
  while (f->filename) {
      Inv.mapVirtualFile(f->filename, {f->content , f->size } );
      f++;
  }

  return !Inv.run();
}




