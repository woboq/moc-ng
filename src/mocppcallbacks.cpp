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
