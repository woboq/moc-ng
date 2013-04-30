/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#include "mocppcallbacks.h"

void MocPPCallbacks::InjectQObjectDefs(clang::SourceLocation Loc) {
    #include "qobjectdefs-injected.h"
    auto Buf = llvm::MemoryBuffer::getMemBuffer(Injected, "qobjectdefs-injected.moc");
    Loc = PP.getSourceManager().getFileLoc(Loc);
    PP.EnterSourceFile( PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Loc), nullptr, Loc);
}

void MocPPCallbacks::FileChanged(clang::SourceLocation Loc, clang::PPCallbacks::FileChangeReason Reason, clang::SrcMgr::CharacteristicKind FileType, clang::FileID PrevFID) {

    clang::SourceManager &SM = PP.getSourceManager();
    IsInMainFile = (SM.getFileID(SM.getFileLoc(Loc)) == SM.getMainFileID());


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

void MocPPCallbacks::InclusionDirective(clang::SourceLocation HashLoc, const clang::Token& IncludeTok, llvm::StringRef FileName, bool IsAngled, clang::CharSourceRange FilenameRange, const clang::FileEntry* File, llvm::StringRef SearchPath, llvm::StringRef RelativePath, const clang::Module* Imported)
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
