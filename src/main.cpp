/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


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

// This is a dictionary that maps macro info to source locations
template<typename T>
class SourceLocationDict :
    std::map<clang::SourceLocation, T, std::function<bool(clang::SourceLocation, clang::SourceLocation)>>
{

    SourceLocationDict(clang::SourceManager &SM)
        : std::map<clang::SourceLocation, T, std::function<bool(clang::SourceLocation, clang::SourceLocation)>>
            ([&SM](clang::SourceLocation LHS, clang::SourceLocation RHS){
                    return SM.isBeforeInTranslationUnit(LHS, RHS);
                }) {}
};





struct PropertyDef {
    std::string name, type, member, read, write, reset, designable, scriptable, editable, stored,
                user, notify, inPrivateClass;
    int notifyId = -1;
    bool constant = false;
    bool final = false;

    // ### ???
    enum Specification  { ValueSpec, ReferenceSpec, PointerSpec };
    Specification gspec = ValueSpec;

    int revision = 0;
};


struct ClassDef {

    clang::CXXRecordDecl *Record = nullptr;

    // This list only includes the things registered with the keywords
    std::vector<clang::CXXMethodDecl*> Signals;
    std::vector<clang::CXXMethodDecl*> Slots;
    std::vector<clang::CXXMethodDecl*> Method;
    std::vector<clang::CXXConstructorDecl*> Constructors;
    std::vector<std::pair<clang::EnumDecl*, bool>> Enums;  //### or string?

    std::vector<std::pair<std::string, std::string>> Interfaces;
    std::vector<std::pair<std::string, std::string>> ClassInfo;

    std::vector<PropertyDef> Properties;

    bool HasQObject = false;
    bool HasQGadget = false;

    //TODO: PluginData;
    //TODO: FLagAliases;
};



static ClassDef parseClass (clang::CXXRecordDecl *RD) {
    ClassDef res;
    res.Record = RD;

    // find the signals, and slot.
    for ( auto it = RD->method_begin(); it != RD->method_end(); ++it ) {

       // int Clones = it->getNumParams() - it->getMinRequiredArguments();

        for (auto attr_it = it->specific_attr_begin<clang::AnnotateAttr>();
             attr_it != it->specific_attr_end<clang::AnnotateAttr>();
             ++attr_it) {

            const clang::AnnotateAttr *A = *attr_it;
            if (A->getAnnotation() == "qt_signal") {
        //        for (int i = 0; i < Clones; ++i)
                    res.Signals.push_back(*it);
            } else if (A->getAnnotation() == "qt_slot") {
       //         for (int i = 0; i < Clones; ++i)
                    res.Slots.push_back(*it);
            } else if (A->getAnnotation() == "qt_invokable") {
                if (auto *C = llvm::dyn_cast<clang::CXXConstructorDecl>(*it)) {
        //            for (int i = 0; i < Clones; ++i)
                        res.Constructors.push_back(C);
                } else {
        //            for (int i = 0; i < Clones; ++i)
                        res.Method.push_back(*it);
                }
            }
        }
    }

    return res;
}


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
public:
    MocPPCallbacks(clang::Preprocessor &PP, MocASTConsumer* Consumer) : PP(PP), Consumer(Consumer) {}

    clang::SourceLocation seenQ_OBJECT;



    virtual void MacroExpands(const clang::Token& MacroNameTok, const clang::MacroInfo* MI,
                              clang::SourceRange Range) override {
        auto MacroString = MacroNameTok.getIdentifierInfo()->getName();
        if (MacroString == "Q_OBJECT")
            seenQ_OBJECT = MacroNameTok.getLocation();
    }

    virtual void MacroDefined(const clang::Token& MacroNameTok, const clang::MacroInfo* MI) override {
        auto Loc = MacroNameTok.getLocation();
        if (!Loc.isValid() || !Loc.isFileID())
            return;
        // TODO: check we are in qobjectdefs

        auto MacroString = MacroNameTok.getIdentifierInfo()->getName();
        if (MacroString == "signals" || MacroString == "Q_SIGNALS")
            AddToMacro(MI, "__attribute__((annotate(\"qt_signal\")))");
        else if (MacroString == "slots" || MacroString == "Q_SLOTS")
            AddToMacro(MI, "__attribute__((annotate(\"qt_slot\")))");

    }

    virtual void FileChanged(clang::SourceLocation Loc, FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType,
                             clang::FileID PrevFID) override;

private:
    void AddToMacro(const clang::MacroInfo* MI, const char *Text) {
        auto MI2 = const_cast<clang::MacroInfo*>(MI);
        clang::Lexer Lex(MI->getDefinitionLoc(), PP.getLangOpts(), Text, Text, Text + std::strlen(Text) + 1);
        clang::Token Tok;
        while (!Lex.LexFromRawLexer(Tok)) {
            if (Tok.is(clang::tok::raw_identifier)) {
                PP.LookUpIdentifierInfo(Tok);
            }
            MI2->AddTokenToBody(Tok);
        }
    }
};



class MocASTConsumer : public clang::ASTConsumer
{
    clang::CompilerInstance &ci;
    clang::ASTContext *ctx = nullptr;
    MocPPCallbacks *PPCallbacks = nullptr;

    std::vector<clang::CXXRecordDecl*> objects;


    static bool IsQtVirtual(llvm::StringRef Name) {
        return (Name == "qt_metacall" || Name == "qt_matacast" || Name == "metaObject"
            || Name == "qt_static_metacall");
    }


public:
    MocASTConsumer(clang::CompilerInstance &ci) :ci(ci)
    {
        //ci.getLangOpts().DelayedTemplateParsing = (true);
        ci.getPreprocessor().enableIncrementalProcessing();
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

        if (!objects.size())
            return true;

        if (done)
            return true;

        auto &PP = ci.getPreprocessor();

        PP.EnableBacktrackAtThisPos();
        clang::Token Tok;
        PP.Lex(Tok);
        while(Tok.is(clang::tok::semi))
            PP.Lex(Tok);
        PP.Backtrack();
        if (Tok.is(clang::tok::eof)) {
            done = true;

            std::string code = generate();
            if (!code.empty()) {
                objects.clear();
                PP.getSourceManager();
                auto Buf = llvm::MemoryBuffer::getMemBufferCopy(";" + code + ";", "qt_moc");

                PP.EnterSourceFile( PP.getSourceManager().createFileIDForMemBuffer(Buf), nullptr, {});
            }
        }
        return true;
    }

    std::string generate() {

        std::string code;


        for (clang::CXXRecordDecl *RD : objects ) {

            // find a key function: first non inline virtual method
            const clang::CXXMethodDecl *Key = ctx->getKeyFunction(RD);
            if (Key && IsQtVirtual(Key->getName()))
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

                    if (IsQtVirtual(it->getName()))
                        continue;

                   /* if (Key->isFunctionTemplateSpecialization())
                        continue; */

                    Key = *it;
                    if (Key->isVirtual())
                        break;
                }
            }
            if (Key && !Key->hasBody())
                continue;


            //We need to generate the contents for the signals:


            int signal_index = 0;

            for ( auto it = RD->method_begin(); it != RD->method_end(); ++it ) {

                clang::PrintingPolicy PrPo (ctx->getLangOpts());
                PrPo.SuppressTagKeyword = true;


                for (auto it2 = it->specific_attr_begin<clang::AnnotateAttr>();
                     it2 != it->specific_attr_end<clang::AnnotateAttr>();
                ++it2) {

                    const clang::AnnotateAttr *A = *it2;
                    if (A->getAnnotation() == "qt_signal") {

                        code += "void " + it->getQualifiedNameAsString() + "(";
                        int arg_index = 0;
                        int clones = 0;
                        for (auto p_it = it->param_begin() ; p_it != it->param_end(); ++p_it) {
                            clang::QualType T = (*p_it)->getType();
                            if (arg_index != 0)
                                code += ", ";
                            arg_index++;
                            if ((*p_it)->hasDefaultArg())
                                clones++;
                            code += T.getAsString() + " " + "_t" + toStr(arg_index);
                        }
                        code += ") \n { \n  void *_a[] = { 0 ";

                        arg_index = 0;
                        for (auto p_it = it->param_begin() ; p_it != it->param_end(); ++p_it) {
                            arg_index++;
                            code += ", const_cast<void*>(reinterpret_cast<const void *>(&_t" + toStr(arg_index) + ")) ";
                        }

                        code += "};\n  QMetaObject::activate(this, &staticMetaObject, " + toStr(signal_index) + ", _a); } \n";

                        signal_index += 1 + clones;

                    }
                    if (A->getAnnotation() == "qt_slot") {
                        std::cout << "SLOT: " << RD->getNameAsString() << "::" << it->getNameAsString() << std::endl;
                    }

                }


                if (it->getName() == "metaObject") {
                    code += "const QMetaObject *" + it->getQualifiedNameAsString() + "() const { \n";
                    code += "return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject; }\n";


                    code += "const QMetaObject " + RD->getQualifiedNameAsString() +  "::staticMetaObject = "
                        + RD->bases_begin()->getType().getAsString(PrPo) + "::staticMetaObject ;\n";

                }
                if (it->getName() == "qt_metacast") {
                    code += "void *" + it->getQualifiedNameAsString() + "(const char *_clname) {\n";
                 /*   code += "if (!_clname) return 0;\n";
                    code += "if (!strcmp(_clname, qt_meta_stringdata_MyObj.stringdata))\n"
                    "    return static_cast<void*>(const_cast<" +  RD->getNameAsString() + "*>(this));\n" */
                    code += "return "+ RD->bases_begin()->getType().getAsString(PrPo) +"::qt_metacast(_clname); }\n";
                }
                if (it->getName() == "qt_metacall") {
                    code += "int " + it->getQualifiedNameAsString() + "(QMetaObject::Call _c, int _id, void **_a)  { return 0; }\n";
                }

            }

        }
        return code;
    }

    void HandleVTable(clang::CXXRecordDecl* RD, bool DefinitionRequired) override {
        std::cout << "################### YOYO " << RD->getNameAsString() << std::endl;
    }

    void HandleTagDeclDefinition(clang::TagDecl* D) override {
        clang::CXXRecordDecl *RD = llvm::dyn_cast<clang::CXXRecordDecl>(D);
        if (!RD)
            return;
        if (!(PPCallbacks->seenQ_OBJECT.isValid() &&
                ctx->getSourceManager().isBeforeInTranslationUnit(D->getSourceRange().getBegin(),
                            PPCallbacks->seenQ_OBJECT) &&
                ctx->getSourceManager().isBeforeInTranslationUnit(PPCallbacks->seenQ_OBJECT,
                            D->getSourceRange().getEnd())
             ))
            return;

        PPCallbacks->seenQ_OBJECT = {};

        objects.push_back(RD);

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








void MocPPCallbacks::FileChanged(clang::SourceLocation Loc, clang::PPCallbacks::FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType, clang::FileID PrevFID)
{
   /* std::cout << "FILE CHANGED " << Reason << std::endl;
    auto F = PP.getSourceManager().getFileEntryForID(PrevFID);
    if (F)
        std::cout << F->getName() << std::endl;


    if (Reason == ExitFile && PrevFID == PP.getSourceManager().getMainFileID()) {
    } else {
        return;
    }

    std::string code = Consumer->generate();
    if (!code.empty()) {
        PP.getSourceManager();
        auto Buf = llvm::MemoryBuffer::getMemBufferCopy(code, "qt_moc");
        PP.EnterSourceFile( PP.getSourceManager().createFileIDForMemBuffer(Buf), nullptr, {});
    }*/
}



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



