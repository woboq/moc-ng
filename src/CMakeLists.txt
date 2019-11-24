# /****************************************************************************
#  *  Copyright (C) 2013-2016 Woboq GmbH
#  *  Olivier Goffart <contact at woboq.com>
#  *  https://woboq.com/
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

cmake_minimum_required(VERSION 3.1)

project(mocng_plugin CXX)

Find_Package(LLVM REQUIRED)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
Find_Package(Clang REQUIRED CONFIG HINTS "${LLVM_INSTALL_PREFIX}/lib/cmake/clang")
message(STATUS "Found Clang in ${CLANG_INSTALL_PREFIX}")

set (CMAKE_CXX_STANDARD 11)

SET(common_srcs mocng.cpp generator.cpp propertyparser.cpp mocppcallbacks.cpp mocastconsumer.cpp
                qbjs.cpp clangversionabstraction.cpp workaroundtests.cpp)

set(CLANG_LIBS clangFrontend clangParse clangSema clangAST clangBasic clangLex clangTooling clangSerialization)

if(TARGET LLVM)
    set(CLANG_LIBS ${CLANG_LIBS} LLVM)
else()
   # We cannot use llvm_config() here because in some versions it uses PRIVATE when calling target_link_libraries
   # and in some it doesn't. If our calls of target_link_libraries don't do it the same way, we get a
   # fatal error.
   llvm_map_components_to_libnames(llvm_libs core support objectyaml)
   set(CLANG_LIBS ${CLANG_LIBS} ${llvm_libs})
endif()

if (NOT APPLE)
    #  Don't link with libs that overlaps our options
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--as-needed" )
endif()


add_library(mocng_plugin SHARED plugin.cpp ${common_srcs})
target_include_directories(mocng_plugin PRIVATE ${CLANG_INCLUDE_DIRS})
target_link_libraries(mocng_plugin PRIVATE ${CLANG_LIBS}  )

set_target_properties(mocng_plugin PROPERTIES
         LINKER_LANGUAGE CXX
         LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/ExportedSymbolsList"
         SOVERSION 1.0)

add_executable(moc  main.cpp ${common_srcs})
target_include_directories(moc PRIVATE ${CLANG_INCLUDE_DIRS})
target_link_libraries(moc PRIVATE ${CLANG_LIBS})

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -fno-rtti -Wall")

if (APPLE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -stdlib=libc++ ")
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}  -stdlib=libc++" )
endif()

# Embed the clang header into the binary:
string(REPLACE "svn" "" LLVM_VERSION "${LLVM_VERSION}")
string(REGEX REPLACE "git.*$" "" LLVM_VERSION "${LLVM_VERSION}")
if(NOT CLANG_BUILTIN_HEADERS_DIR)
    set(CLANG_BUILTIN_HEADERS_DIR "${LLVM_LIBRARY_DIR}/clang/${LLVM_VERSION}/include")
endif()
file(GLOB BUILTINS_HEADERS "${CLANG_BUILTIN_HEADERS_DIR}/*.h")
if(NOT BUILTINS_HEADERS)
    message(FATAL_ERROR "Could not find any clang builtins headers in ${CLANG_BUILTIN_HEADERS_DIR}")
endif()
foreach(BUILTIN_HEADER ${BUILTINS_HEADERS})
    #filter files that are way to big
    if(NOT BUILTIN_HEADER MATCHES ".*/(arm_neon.h|altivec.h|vecintrin.h|avx512.*intrin.h)")
        file(READ ${BUILTIN_HEADER} BINARY_DATA)
        string(REPLACE "\\" "\\\\" BINARY_DATA "${BINARY_DATA}")
        string(REPLACE "\"" "\\\"" BINARY_DATA "${BINARY_DATA}")
        string(REPLACE "\n" "\\n\"\n\"" BINARY_DATA "${BINARY_DATA}")
        #workaround the fact that stdint.h includes itself
        string(REPLACE "__CLANG_STDINT_H" "__CLANG_STDINT_H2" BINARY_DATA "${BINARY_DATA}")
        string(REPLACE "${CLANG_BUILTIN_HEADERS_DIR}/" "/builtins/" FN "${BUILTIN_HEADER}"  )
        set(EMBEDDED_DATA "${EMBEDDED_DATA} { \"${FN}\" , \"${BINARY_DATA}\" } , \n")
    endif()
endforeach()

configure_file(embedded_includes.h.in embedded_includes.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

include("GNUInstallDirs")
install(TARGETS moc mocng_plugin
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )
