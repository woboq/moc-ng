/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#include <clang/Lex/Preprocessor.h>
#include <clang/Basic/Version.h>

class MocPPCallbacks : public clang::PPCallbacks {
    clang::Preprocessor &PP;

    bool IncludeNotFoundSupressed = false;


public:

    MocPPCallbacks(clang::Preprocessor &PP) : PP(PP) {}

    bool IsInMainFile = false;

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
//        const auto *MI = MD->getMacroInfo();
#else
    virtual void MacroUndefined(const clang::Token& MacroNameTok, const clang::MacroInfo* MI) override {
#endif
        //Workaround to get moc's test to compile
        if(MacroNameTok.getIdentifierInfo()->getName() == "QT_NO_KEYWORDS") {
            //re-inject qobjectdefs
            InjectQObjectDefs(MacroNameTok.getLocation());
        }
    }

    void FileChanged(clang::SourceLocation Loc, FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType,
                             clang::FileID PrevFID) override;
    bool FileNotFound(llvm::StringRef FileName, llvm::SmallVectorImpl< char >& RecoveryPath) override;

    void InjectQObjectDefs(clang::SourceLocation Loc);
};
