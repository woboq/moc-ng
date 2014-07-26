# /****************************************************************************
#  *  Copyright (C) 2013-2014 Woboq GmbH
#  *  Olivier Goffart <contact at woboq.com>
#  *  http://woboq.com/
#  *
#  *  This program is free software: you can redistribute it and/or modify
#  *  it under the terms of the GNU General Public License as published by
#  *  the Free Software Foundation, either version 3 of the License, or
#  *  (at your option) any later version.
#  *
#  *  This program is distributed in the hope that it will be useful,
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  *  GNU General Public License for more details.
#  *
#  *  You should have received a copy of the GNU General Public License
#  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#  */

cmake_minimum_required(VERSION 2.8)

project(mocng_plugin CXX)

Find_Package(LLVM REQUIRED)
#include_directories(${LLVM_INCLUDE_DIR})
link_directories( ${LLVM_LIB_DIR} )

Find_Package(Clang REQUIRED)
include_directories(${CLANG_INCLUDE_DIRS})

add_library(mocng_plugin SHARED plugin.cpp mocng.cpp generator.cpp propertyparser.cpp mocppcallbacks.cpp mocastconsumer.cpp qbjs.cpp clangversionabstraction.cpp)

target_link_libraries(mocng_plugin  ${CLANG_LIBS} ${LLVM_LIBS_CORE} )

set_target_properties(mocng_plugin PROPERTIES
         LINKER_LANGUAGE CXX
         LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/ExportedSymbolsList"
         SOVERSION 1.0)

add_executable(moc  main.cpp mocng.cpp generator.cpp propertyparser.cpp mocppcallbacks.cpp mocastconsumer.cpp qbjs.cpp clangversionabstraction.cpp)
target_link_libraries(moc   ${CLANG_LIBS} ${LLVM_LIBS_CORE} ${LLVM_LDFLAGS} )

# install(TARGETS mocng_plugin LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
# install(TARGETS moc LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

ADD_DEFINITIONS("-std=c++11 -g -Wall -fno-rtti " )
ADD_DEFINITIONS("-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS ")


if (APPLE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -stdlib=libc++ ")
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}  -stdlib=libc++" )
endif()



# Embed the clang header into the binary:
exec_program(${LLVM_CONFIG_EXECUTABLE} ARGS --version      OUTPUT_VARIABLE LLVM_VERSION )
string(REPLACE "svn" "" LLVM_VERSION "${LLVM_VERSION}")
file(GLOB BUILTINS_HEADERS "${LLVM_BIN_DIR}/../lib/clang/${LLVM_VERSION}/include/*.h")
foreach(BUILTIN_HEADER ${BUILTINS_HEADERS})
    file(READ ${BUILTIN_HEADER} BINARY_DATA HEX)
    string(REGEX REPLACE "(..)" "\\\\x\\1" BINARY_DATA "${BINARY_DATA}")
    string(REPLACE "${LLVM_BIN_DIR}/../lib/clang/${LLVM_VERSION}/include/" "/builtins/" FN "${BUILTIN_HEADER}"  )
    set(EMBEDDED_DATA "${EMBEDDED_DATA} { \"${FN}\" , \"${BINARY_DATA}\" } , ")
endforeach()

configure_file(embedded_includes.h.in embedded_includes.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})


