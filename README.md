# moc-ng:  a replacement for Qt's moc that is using clang libraries

This is really two project:

  * A plugin for clang which removes the needs of running moc
    if your code is compiled with this plugin.

  * A replacement for moc that aim to be compatible with qt's moc,
    but uses the clang libraries for the parsing step.

Read the blog post:  https://woboq.com/blog/moc-with-clang.html

[![Build Status](https://travis-ci.org/woboq/moc-ng.svg?branch=master)](https://travis-ci.org/woboq/moc-ng)

## Browse the source code online
https://code.woboq.org/mocng/src/

## Compile

You need llvm and clang (>= 3.4).
Then run cmake and make (adapt your paths)

 cmake . -DCMAKE_CXX_COMPILER=/opt/llvm/bin/clang++  -DLLVM_CONFIG_EXECUTABLE=/opt/llvm/bin/llvm-config
 make

## Use

 * As a binary:  replace the moc provided by Qt by the one which is in src/moc

 * As a clang plugin: Tell your build system not to run moc, and add this to the CXXFLAGS
    -Xclang -load  -Xclang /path/to/src/libmocng_plugin.so -Xclang -add-plugin -Xclang moc

## Differences with upstream moc

This version of moc has nice additional support compared to upstream moc:

 * Template support: You can have templated QObject
 * Automatically register all types using qRegisterMetaType
 * Supports trailing return type, auto return types for signals and slot and decltype in the
   return type or parameter types.
 * Support for nested classes.

Not supported:
 * OSX Framework options (-F)

## Problems?

Make sure that the Qt include paths (and other include paths) are correctly passed to moc via the -I option

Report bugs on github: https://github.com/woboq/moc-ng/issues

## Tests

Replace the moc binary in the builddir (qtbase/bin/moc)  and run the Qt tests.
Expecially the tst_moc and the tests for corelib/kernel.
The tests that are known to fail are worked around in "workaroundtests.cpp".
Tests for features not supported by normal Qt (such as templates) are found in
the tests subdirectory.
Check the README in the tests subdirectory for the moc-ng specific tests
