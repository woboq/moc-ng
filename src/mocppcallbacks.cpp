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

#include "mocppcallbacks.h"
#include "clangversionabstraction.h"

void MocPPCallbacks::InjectQObjectDefs(clang::SourceLocation Loc) {
    #include "qobjectdefs-injected.h"
    auto Buf = maybe_unique(llvm::MemoryBuffer::getMemBuffer(Injected, "qobjectdefs-injected.moc"));
    Loc = PP.getSourceManager().getFileLoc(Loc);
    PP.EnterSourceFile( CreateFileIDForMemBuffer(PP, Buf, Loc), nullptr, Loc);
}

void MocPPCallbacks::EnterMainFile(llvm::StringRef Name)
{
    if (Name.endswith("global/qnamespace.h")) {
        // qnamsepace.h is a bit special because it contains all the Qt namespace enums
        // but all the Q_ENUMS are within a Q_MOC_RUN scope, which also do all sort of things.

        clang::MacroInfo *MI = PP.AllocateMacroInfo({});
        MI->setIsBuiltinMacro();
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 2
        PP.appendDefMacroDirective(PP.getIdentifierInfo("Q_MOC_RUN"), MI);
#else
        PP.setMacroInfo(PP.getIdentifierInfo("Q_MOC_RUN"), MI);
#endif
        InjectQObjectDefs({});
    }
}

void MocPPCallbacks::FileChanged(clang::SourceLocation Loc, clang::PPCallbacks::FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType, clang::FileID PrevFID) {

    clang::SourceManager &SM = PP.getSourceManager();
    IsInMainFile = (SM.getFileID(SM.getFileLoc(Loc)) == SM.getMainFileID());

    if (IsInMainFile && Reason == EnterFile) {
        EnterMainFile(SM.getFilename(Loc));
    }

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

bool MocPPCallbacks::FileNotFound(llvm::StringRef FileName, llvm::SmallVectorImpl< char >& RecoveryPath) {
    if (FileName.endswith(".moc") || FileName.endswith("_moc.cpp") || FileName.startswith("moc_")) {
        if (!PP.GetSuppressIncludeNotFoundError()) {
            PP.SetSuppressIncludeNotFoundError(true);
            IncludeNotFoundSupressed = true;
        }
    } else {
        if (IncludeNotFoundSupressed) {
            PP.SetSuppressIncludeNotFoundError(false);
            IncludeNotFoundSupressed = false;
        } else {
            ShouldWarnHeaderNotFound = true;
        }
    }
    return false;
}

void MocPPCallbacks::InclusionDirective(clang::SourceLocation HashLoc, const clang::Token& IncludeTok,
                                        llvm::StringRef FileName, bool IsAngled,
                                        clang::CharSourceRange FilenameRange,
                                        const clang::FileEntry* File, llvm::StringRef SearchPath,
                                        llvm::StringRef RelativePath, const clang::Module* Imported
#if CLANG_VERSION_MAJOR >= 7
                                        , clang::SrcMgr::CharacteristicKind
#endif
                                       )
{
    if (!File && ShouldWarnHeaderNotFound) {
        /* This happens when we are not running as a plugin
         * We want to transform the "include not found" error in a warning.
         */
        PP.getDiagnostics().Report(FilenameRange.getBegin(),
                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                        "'%0' file not found"))
            << FileName << FilenameRange;
    }
    ShouldWarnHeaderNotFound = false;
}
