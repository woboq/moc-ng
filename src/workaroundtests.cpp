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

#include "generator.h"
#include <clang/AST/DeclCXX.h>
#include <llvm/ADT/StringSwitch.h>

/*
 * Some tests do not pass because moc is different from moc-ng and there is no reason for them
 * to change  (for example the tests expect different warnings message)
 * This cheat by detecting these tests and generating code to SKIP them
 *
 * Keep a comment for each blacklisted test case with the reason why it is blacklisted
 */

bool Generator::WorkaroundTests(llvm::StringRef ClassName, const clang::CXXMethodDecl* MD,
                                llvm::raw_ostream& OS)
{
    llvm::StringRef MethodName = MD->getName();
    bool Match = false;
    if (ClassName == "tst_Moc") {
        Match = llvm::StringSwitch<bool>(MethodName)
            // The wording of the warnings is not the same with moc-ng (it is better)
            .Cases("warnings", "warnOnExtraSignalSlotQualifiaction", "warnOnMultipleInheritance", true)
            .Cases("forgottenQInterface", "warnOnPropertyWithoutREAD", true)
            .Cases("warnOnVirtualSignal", "notifyError", "optionsFileError", true)

            // Small difference in the handling of the options
            .Case("ignoreOptionClashes", true)

            // Not implemented because I was focusing on Linux! ### FIXME
            .Case("frameworkSearchPath", true)

            // The header is not self contained (that could be easy to solve upstream)
            .Case("templateGtGt", true)

            // moc -E  to preprocess behave differently
            .Case("unterminatedFunctionMacro", true)

            // MSVC compat mode for $INCLUDE environment variable not implemented
            .Case("environmentIncludePaths", true)

            .Default(false);
    } else if(ClassName == "tst_QObject"){
        if (MethodName == "normalize") {
            // "unsigned long int" is a builtin type, but is not registered by default, and clang
            // mangle it  as "unsigned long" with no way to get what was the actual string.
            // By registering the right string we ensure that the function is found when.
            // This is actually a bug in QMetaType that should be fixed in Qt https://codereview.qt-project.org/55193
            OS << "\n            qRegisterMetaType<unsigned long int>(\"unsigned long int\");\n            ";
            return false; // The normal function will be called
        }
    }

    if (!Match)
        return false;

    OS << " QSKIP(\"This test does not make sens with moc-ng\");\n";
    // testlib is expected to be included in the test.
    // QSKIP calls return;  so no need to add the break;
    return true;
}
