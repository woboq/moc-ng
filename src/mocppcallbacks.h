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
    bool ShouldWarnHeaderNotFound = false;
    bool InQMOCRUN = false;
    std::set<std::string> PossibleTags;
    std::map<clang::SourceLocation, std::string> &Tags;

public:

    MocPPCallbacks(clang::Preprocessor &PP, std::map<clang::SourceLocation, std::string> &Tags) : PP(PP), Tags(Tags) {}

    bool IsInMainFile = false;
    void InjectQObjectDefs(clang::SourceLocation Loc);

protected:
#if  CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
    typedef const clang::MacroDirective *MacroParam;
#else
    typedef const clang::MacroInfo *MacroParam;
#endif


    void MacroUndefined(const clang::Token& MacroNameTok, MacroParam) override {
        //Workaround to get moc's test to compile
        if (MacroNameTok.getIdentifierInfo()->getName() == "QT_NO_KEYWORDS") {
            //re-inject qobjectdefs
            InjectQObjectDefs(MacroNameTok.getLocation());
        }
    }

    void FileChanged(clang::SourceLocation Loc, FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType,
                             clang::FileID PrevFID) override;
    bool FileNotFound(llvm::StringRef FileName, llvm::SmallVectorImpl< char >& RecoveryPath) override;
    void InclusionDirective(clang::SourceLocation HashLoc, const clang::Token& IncludeTok,
                            llvm::StringRef FileName, bool IsAngled, clang::CharSourceRange FilenameRange,
                            const clang::FileEntry* File, llvm::StringRef SearchPath, llvm::StringRef RelativePath,
                            const clang::Module* Imported) override;

    void Defined(const clang::Token& MacroNameTok
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
            ,MacroParam = 0
#endif
    ) override {
        if (MacroNameTok.getIdentifierInfo()->getName() != "Q_MOC_RUN")
            return;
        auto F = PP.getSourceManager().getFileEntryForID(PP.getSourceManager().getFileID(MacroNameTok.getLocation()));
        if (!F) return;
        llvm::StringRef name = F->getName();
        if (name.endswith("qobjectdefs.h") || name.endswith("qglobal.h"))
            return;
        InQMOCRUN = true;
    }
    void Ifdef(clang::SourceLocation Loc, const clang::Token& MacroNameTok
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
            ,MacroParam
#endif
    ) override { Defined(MacroNameTok); }
    void Ifndef(clang::SourceLocation Loc, const clang::Token& MacroNameTok
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
            ,MacroParam
#endif
    ) override { Defined(MacroNameTok); }

    void Endif(clang::SourceLocation Loc, clang::SourceLocation IfLoc)  override {
        InQMOCRUN = false;
        //TODO: handle embedded Ifs
    }

    void MacroDefined(const clang::Token& MacroNameTok, MacroParam) override {
        if (!InQMOCRUN)
            return;
        PossibleTags.insert(MacroNameTok.getIdentifierInfo()->getName());
    }


    void MacroExpands(const clang::Token& MacroNameTok, MacroParam, clang::SourceRange Range) override {
        if (InQMOCRUN) return;
        if (PossibleTags.count(MacroNameTok.getIdentifierInfo()->getName())) {
            clang::SourceLocation FileLoc = PP.getSourceManager().getFileLoc(MacroNameTok.getLocation());
            Tags.insert({FileLoc, MacroNameTok.getIdentifierInfo()->getName()});
        }
    }

};
