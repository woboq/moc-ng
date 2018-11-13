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

#include <clang/Lex/Preprocessor.h>
#include <clang/Basic/Version.h>
#include <set>

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
    void EnterMainFile(clang::StringRef Name);

protected:
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR >= 7
    typedef const clang::MacroDefinition &MacroParam;
    typedef const clang::MacroDirective *MacroParam2;
#elif CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
    typedef const clang::MacroDirective *MacroParam;
    typedef const clang::MacroDirective *MacroParam2;
#else
    typedef const clang::MacroInfo *MacroParam;
    typedef MacroParam MacroParam2;
#endif


    void MacroUndefined(const clang::Token& MacroNameTok, MacroParam
#if CLANG_VERSION_MAJOR >= 5
                        , const clang::MacroDirective *
#endif
                        ) override {
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
                            const clang::Module* Imported
#if CLANG_VERSION_MAJOR >= 7
                            , clang::SrcMgr::CharacteristicKind
#endif
                  ) override;

    void Defined(const clang::Token& MacroNameTok
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
            ,MacroParam = {}
#endif
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 3
            , clang::SourceRange = {}
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

    void MacroDefined(const clang::Token& MacroNameTok, MacroParam2) override {
        if (!InQMOCRUN)
            return;
        PossibleTags.insert(MacroNameTok.getIdentifierInfo()->getName());
    }


    void MacroExpands(const clang::Token& MacroNameTok, MacroParam, clang::SourceRange Range
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
            , const clang::MacroArgs *
#endif
    ) override {
        if (InQMOCRUN) return;
        if (PossibleTags.count(MacroNameTok.getIdentifierInfo()->getName())) {
            clang::SourceLocation FileLoc = PP.getSourceManager().getFileLoc(MacroNameTok.getLocation());
            Tags.insert({FileLoc, MacroNameTok.getIdentifierInfo()->getName()});
        }
    }

};
