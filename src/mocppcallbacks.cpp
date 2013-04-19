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
    //std::cout << "FILE NOT FOUND " << std::string(FileName) << std::endl;
    if (FileName.endswith(".moc") || FileName.endswith("_moc.cpp") || FileName.startswith("moc_")) {
        if (!PP.GetSuppressIncludeNotFoundError()) {
            PP.SetSuppressIncludeNotFoundError(true);
            IncludeNotFoundSupressed = true;
        }
    } else {
        if (IncludeNotFoundSupressed) {
            PP.SetSuppressIncludeNotFoundError(false);
            IncludeNotFoundSupressed = false;
        }
    }
    return false;
}
