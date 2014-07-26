/****************************************************************************
 *  Copyright (C) 2013-2014 Woboq GmbH
 *  Olivier Goffart <contact at woboq.com>
 *  http://woboq.com/
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

#pragma once

#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/Version.h>

namespace clang {
    class Preprocessor;
}

// Abstract the API changes in clang

clang::FileID CreateFileIDForMemBuffer(clang::Preprocessor &PP, llvm::MemoryBuffer *Buf, clang::SourceLocation Loc);
