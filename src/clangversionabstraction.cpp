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

#include "clangversionabstraction.h"
#include <clang/Lex/Preprocessor.h>


namespace clang {
}

clang::FileID CreateFileIDForMemBuffer(clang::Preprocessor &PP, llvm::MemoryBuffer *Buf, clang::SourceLocation Loc)
{
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR > 4
    return PP.getSourceManager().createFileID(maybe_unique(Buf), clang::SrcMgr::C_User, 0, 0, Loc);
#else
    return PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Loc);
#endif
}