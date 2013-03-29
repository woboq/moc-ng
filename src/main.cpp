/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#include "clang/Basic/Version.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Attr.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "clang/AST/DeclCXX.h"


#include <clang/Frontend/CompilerInstance.h>


#include <iostream>
#include <limits>
#include <functional>


#include "mocng.h"
#include "generator.h"

namespace Options {

namespace cl = llvm::cl;
/*

cl::opt<std::string> BuildPath(
  "b",
  cl::desc("<build-path>"),
  cl::Optional);

cl::list<std::string> SourcePaths(
  cl::Positional,
  cl::desc("<source0> [... <sourceN>]"),
  cl::OneOrMore);

cl::opt<std::string> OutputPath(
    "o",
    cl::desc("<output path>"),
    cl::Required);


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

// FIXME
std::string toStr(unsigned int i) {
    static char buf[15] = { '\0' };
    char *ptr = &buf[13];
    do {
        *ptr-- = (i % 10) + '0';
        i = i/10;
    } while (i > 0);
    return ptr + 1;
}



class MocASTConsumer;

class MocPPCallbacks : public clang::PPCallbacks {
    clang::Preprocessor &PP;
    MocASTConsumer* Consumer;

    void InjectQObjectDefs(clang::SourceLocation Loc) {
        #include "qobjectdefs-injected.h"
        auto Buf = llvm::MemoryBuffer::getMemBuffer(Injected, "qobjectdefs-injected.moc");
        PP.EnterSourceFile( PP.getSourceManager().createFileIDForMemBuffer(Buf), nullptr, Loc);
    }

public:

    virtual bool FileNotFound(llvm::StringRef FileName, llvm::SmallVectorImpl< char >& RecoveryPath) override
    {
        //std::cout << "FILE NOT FOUND " << std::string(FileName) << std::endl;
        if (FileName.endswith(".moc") || FileName.endswith("_moc.cpp") || FileName.startswith("moc_")) {
            PP.SetSuppressIncludeNotFoundError(true);
            /*PP.getFileManager().getVirtualFile(("/qt_dummy_moc/" + FileName).str(), 0, 0);
            const char *R = "/qt_dummy_moc/";
            RecoveryPath.assign(14, *R);
            std::cout << RecoveryPath.data() << " " << std::endl;
            return true;*/
        } else {
            PP.SetSuppressIncludeNotFoundError(false);
        }
        return false;
    }



    MocPPCallbacks(clang::Preprocessor &PP, MocASTConsumer* Consumer) : PP(PP), Consumer(Consumer) {}

    /*
    clang::SourceLocation seenQ_OBJECT;


    void MacroExpands(const clang::Token& MacroNameTok, const clang::MacroInfo* MI,
                              clang::SourceRange Range) override {
        auto MacroString = MacroNameTok.getIdentifierInfo()->getName();
        if (MacroString == "Q_OBJECT")
            seenQ_OBJECT = MacroNameTok.getLocation();
    }

    */

#if  CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
    void MacroUndefined(const clang::Token& MacroNameTok, const clang::MacroDirective* MD) override {
        const auto *MI = MD->getMacroInfo();
#else
    virtual void MacroUndefined(const clang::Token& MacroNameTok, const clang::MacroInfo* MI) override {
#endif
        //Workaround to get moc's test to compile
        if(MacroNameTok.getIdentifierInfo()->getName() == "QT_NO_KEYWORDS") {
            //re-inject qobjectdefs
            InjectQObjectDefs(MacroNameTok.getLocation());
        }
    }


/*
#if  CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
    void MacroDefined(const clang::Token& MacroNameTok, const clang::MacroDirective* MD) override
    { const auto *MI = MD->getMacroInfo();
#else
    void MacroDefined(const clang::Token& MacroNameTok, const clang::MacroInfo* MI) override {
#endif
        auto Loc = MacroNameTok.getLocation();
        if (!Loc.isValid() || !Loc.isFileID())
            return;
        // TODO: check we are in qobjectdefs

        auto MacroString = MacroNameTok.getIdentifierInfo()->getName();
        if (MacroString == "signals" || MacroString == "Q_SIGNALS" || MacroString == "Q_SIGNAL")
            AddToMacro(MI, "__attribute__((annotate(\"qt_signal\"))) //\n");
        else if (MacroString == "slots" || MacroString == "Q_SLOTS" || MacroString == "Q_SLOT")
            AddToMacro(MI, "__attribute__((annotate(\"qt_slot\"))) //\n");
        else if (MacroString == "Q_INVOKABLE")
            AddToMacro(MI, "__attribute__((annotate(\"qt_invokable\"))) //\n");
        else if (MacroString == "Q_SCRIPTABLE")
            AddToMacro(MI, "__attribute__((annotate(\"qt_scriptable\"))) //\n");
        else if (MacroString == "Q_PROPERTY")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_property\", QT_STRINGIFY2(text)); //\n");
        else if (MacroString == "Q_PRIVATE_PROPERTY")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_private_property\", QT_STRINGIFY2(d)) \",\" QT_STRINGIFY2(text)); //\n");
        else if (MacroString == "Q_CLASSINFO")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_classinfo\", QT_STRINGIFY2(name) \",\" QT_STRINGIFY2(value)); //\n");
        else if (MacroString == "Q_ENUMS")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_enums\", QT_STRINGIFY2(x)); //\n");
        else if (MacroString == "Q_FLAGS")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_flags\", QT_STRINGIFY2(x)); //\n");
        else if (MacroString == "Q_INTERFACES")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_interface\", QT_STRINGIFY2(x)); //\n");
        else if (MacroString == "Q_OBJECT")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_qobject\",\" \"); //\n");
        else if (MacroString == "Q_GADGET")
            AddToMacro(MI, "__extension__ _Static_assert(sizeof \"qt_qgadget\",\" \"); //\n");

    }*/

    virtual void FileChanged(clang::SourceLocation Loc, FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType,
                             clang::FileID PrevFID) override {
            if (Reason != ExitFile)
                return;
            auto F = PP.getSourceManager().getFileEntryForID(PrevFID);
            if (!F)
                return;

            llvm::StringRef name = F->getName();
            if (name.endswith("qobjectdefs.h")) {
                InjectQObjectDefs(Loc);
            }
        }

private:
    /*void AddToMacro(const clang::MacroInfo* MI, const char *Text) {
        auto MI2 = const_cast<clang::MacroInfo*>(MI);
        //clang::Lexer Lex({}, PP.getLangOpts(), Text, Text, Text + std::strlen(Text) + 1);
        auto Buf = llvm::MemoryBuffer::getMemBufferCopy(Text, "qt_moc");
        clang::Lexer Lex(PP.getSourceManager().createFileIDForMemBuffer(Buf), Buf,
                         PP.getSourceManager(), PP.getLangOpts());
        clang::Token Tok;
        while (!Lex.LexFromRawLexer(Tok)) {
            if (Tok.is(clang::tok::raw_identifier)) {
                PP.LookUpIdentifierInfo(Tok);
            }
     //       Tok.setLocation(MI2->getDefinitionLoc().getLocWithOffset(5));
            MI2->AddTokenToBody(Tok);
        }
    }*/
};



class MocASTConsumer : public clang::ASTConsumer
{
    clang::CompilerInstance &ci;
    clang::ASTContext *ctx = nullptr;
    MocPPCallbacks *PPCallbacks = nullptr;

    std::vector<ClassDef> objects;


    static bool IsQtVirtual(const clang::CXXMethodDecl *MD) {
        auto Name = MD->getNameAsString();
        return (Name == "qt_metacall" || Name == "qt_metacast" || Name == "metaObject"
            || Name == "qt_static_metacall");
    }


public:
    MocASTConsumer(clang::CompilerInstance &ci) :ci(ci)
    {
        //ci.getLangOpts().DelayedTemplateParsing = (true);

    }
    virtual ~MocASTConsumer() {
//        ci.getDiagnostics().setClient(new clang::IgnoringDiagConsumer, true);
    }

    virtual void Initialize(clang::ASTContext& Ctx) override {
        ctx = &Ctx;
        PPCallbacks = new MocPPCallbacks(ci.getPreprocessor(), this);
        ci.getPreprocessor().addPPCallbacks(PPCallbacks);
     //   ci.getDiagnostics().setClient(new DiagnosticClient(), true);
    }

    //void HandleTranslationUnit(clang::ASTContext& Ctx) override {
  //      std::cout << "YMMMMOYO" << std::endl;
//        MocASTVisitor v();
//        v.TraverseDecl(Ctx.getTranslationUnitDecl());

    bool done = false;
    virtual bool HandleTopLevelDecl(clang::DeclGroupRef D) {

        auto &PP = ci.getPreprocessor();
        //std::cout <<"hi "  << objects.size() << done  << " " << PP.isIncrementalProcessingEnabled() << " " << PP.isCodeCompletionEnabled() << std::endl;

        if (done) {
            PP.enableIncrementalProcessing(false);
            return true;
        }

        if (!objects.size())
            return true;


       // auto &PP = ci.getPreprocessor();

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
                PP.EnterSourceFile( PP.getSourceManager().createFileIDForMemBuffer(Buf), nullptr, {});
            } else {
                ci.getPreprocessor().enableIncrementalProcessing(false);
                PP.Backtrack();
            }
        } else {
            PP.Backtrack();
        }
        return true;
    }

    std::string generate() {

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
                                   [](clang::AnnotateAttr *A) { return A->getAnnotation() == "qt_signal"; }))
                        continue;

                    Key = *it;
                    if (Key->isVirtual())
                        break;
                }
            }
/*
            std::cout << RD->getQualifiedNameAsString() <<  " Has it a body? " << (bool)(Key) << (Key && !Key->hasBody()) << std::endl;
            if (Key)
                Key->dump();
            std::cout << std::endl;*/

            if (Key && !Key->hasBody())
                continue;


            Generator G(&Def, OS, *ctx);
            G.GenerateCode();

#if 0

            //We need to generate the contents for the signals:


            int signal_index = 0;

            for ( auto it = RD->method_begin(); it != RD->method_end(); ++it ) {

                if (!it->getIdentifier())
                    continue;

                clang::PrintingPolicy PrPo (ctx->getLangOpts());
                PrPo.SuppressTagKeyword = true;


                for (auto it2 = it->specific_attr_begin<clang::AnnotateAttr>();
                     it2 != it->specific_attr_end<clang::AnnotateAttr>();
                     ++it2) {

                    const clang::AnnotateAttr *A = *it2;
                    if (A->getAnnotation() == "qt_signal") {

                        Code += "void " + it->getQualifiedNameAsString() + "(";
                        int arg_index = 0;
                        int clones = 0;
                        for (auto p_it = it->param_begin() ; p_it != it->param_end(); ++p_it) {
                            clang::QualType T = (*p_it)->getType();
                            if (arg_index != 0)
                                Code += ", ";
                            arg_index++;
                            if ((*p_it)->hasDefaultArg())
                                clones++;
                            Code += T.getAsString() + " " + "_t" + toStr(arg_index);
                        }
                        Code += ") \n { \n  void *_a[] = { 0 ";

                        arg_index = 0;
                        for (auto p_it = it->param_begin() ; p_it != it->param_end(); ++p_it) {
                            arg_index++;
                            Code += ", const_cast<void*>(reinterpret_cast<const void *>(&_t" + toStr(arg_index) + ")) ";
                        }

                        Code += "};\n  QMetaObject::activate(this, &staticMetaObject, " + toStr(signal_index) + ", _a); } \n";

                        signal_index += 1 + clones;

                    }
                    if (A->getAnnotation() == "qt_slot") {
                        std::cout << "SLOT: " << RD->getNameAsString() << "::" << it->getNameAsString() << std::endl;
                    }

                }


                if (it->getIdentifier()->getName() == "metaObject") {
                    Code += "const QMetaObject *" + it->getQualifiedNameAsString() + "() const { \n";
                    Code += "return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject; }\n";


                    Code += "const QMetaObject " + RD->getQualifiedNameAsString() +  "::staticMetaObject = "
                        + RD->bases_begin()->getType().getAsString(PrPo) + "::staticMetaObject ;\n";

                }
                if (it->getIdentifier()->getName() == "qt_metacast") {
                    Code += "void *" + it->getQualifiedNameAsString() + "(const char *_clname) {\n";
                 /*   code += "if (!_clname) return 0;\n";
                    code += "if (!strcmp(_clname, qt_meta_stringdata_MyObj.stringdata))\n"
                    "    return static_cast<void*>(const_cast<" +  RD->getNameAsString() + "*>(this));\n" */
                    Code += "return "+ RD->bases_begin()->getType().getAsString(PrPo) +"::qt_metacast(_clname); }\n";
                }
                if (it->getIdentifier()->getName() == "qt_metacall") {
                    Code += "int " + it->getQualifiedNameAsString() + "(QMetaObject::Call _c, int _id, void **_a)  { return 0; }\n";
                }

            }


            for (const PropertyDef &P : Def.Properties) {
                Code += "/*  " + P.type + " -> " + P.name  + "  " + P.read  + " */ \n";
            }
        #endif
        }
        return Code;

    }
/*
    void HandleVTable(clang::CXXRecordDecl* RD, bool DefinitionRequired) override {
        std::cout << "--HandleVTable-- " << RD->getNameAsString() << std::endl;
    }
*/
    void HandleTagDeclDefinition(clang::TagDecl* D) override {
        clang::CXXRecordDecl *RD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
        if (!RD)
            return;
        /*if (!(PPCallbacks->seenQ_OBJECT.isValid() &&
                ctx->getSourceManager().isBeforeInTranslationUnit(D->getSourceRange().getBegin(),
                            PPCallbacks->seenQ_OBJECT) &&
                ctx->getSourceManager().isBeforeInTranslationUnit(PPCallbacks->seenQ_OBJECT,
                            D->getSourceRange().getEnd())
             ))
            return;

        PPCallbacks->seenQ_OBJECT = {};*/

        ClassDef Def = parseClass(RD, ci.getPreprocessor());
        if (Def.HasQObject || Def.HasQGadget) {
            objects.push_back(std::move(Def));
            ci.getPreprocessor().enableIncrementalProcessing();
        }

/*
       for (auto it = RD->decls_begin(); it != RD->decls_end(); ++it) {
            if (auto *S = llvm::dyn_cast<clang::StaticAssertDecl>(*it) )
                if(auto *E = llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(S->getAssertExpr()))
                    if (auto *Kw = llvm::dyn_cast<clang::StringLiteral>(E->getArgumentExpr()))
            {
                llvm::StringRef key = Kw->getString();
                if (key == "qt_property") {
                    std::cout << "PROPERTY: " << std::string(S->getMessage()->getString()) << std::endl;
                }
            }
        }
*/
#if 0

        // find the signals, and slot.
        for ( auto it = RD->method_begin(); it != RD->method_end(); ++it ) {

        /*    if (IsQtVirtual(it->getName())) {
                it->setImplicit();
            }*/

            for (auto it2 = it->specific_attr_begin<clang::AnnotateAttr>();
                 it2 != it->specific_attr_end<clang::AnnotateAttr>();
                ++it2) {

                const clang::AnnotateAttr *A = *it2;
                if (A->getAnnotation() == "qt_signal") {
                    std::cout << "SIGNAL: " << RD->getNameAsString() << "::" << it->getNameAsString() << std::endl;
                }
                if (A->getAnnotation() == "qt_slot") {
                    std::cout << "SLOT: " << RD->getNameAsString() << "::" << it->getNameAsString() << std::endl;
                }

            }
        }

#endif


    }


};


/*

class MocAction : public clang::ASTFrontendAction {
    static std::set<std::string> processed;
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
*/


class MocAction : public clang::PluginASTAction {
protected:
    clang::ASTConsumer *CreateASTConsumer(clang::CompilerInstance &CI, llvm::StringRef f) override {
        return new MocASTConsumer(CI);
    }
    bool ParseArgs(const clang::CompilerInstance& CI, const std::vector< std::string >& arg) override {
        return true;
    }

};

#include "clang/Frontend/FrontendPluginRegistry.h"

static clang::FrontendPluginRegistry::Add<MocAction>
X("moc", "create the dxr index database");


# if 0

int main(int argc, const char **argv) {
  using namespace clang::tooling;

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
}

#endif



