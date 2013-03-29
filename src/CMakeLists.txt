cmake_minimum_required(VERSION 2.8)

project(mocng_plugin CXX)

Find_Package(LLVM REQUIRED)
#include_directories(${LLVM_INCLUDE_DIR})
link_directories( ${LLVM_LIB_DIR} )

Find_Package(Clang REQUIRED)
include_directories(${CLANG_INCLUDE_DIRS})


 add_library(mocng_plugin SHARED main.cpp mocng.cpp generator.cpp propertyparser.cpp)
# add_executable(mocng_plugin main.cpp)
target_link_libraries(mocng_plugin  ${CLANG_LIBS} ${LLVM_LIBS_CORE} )



set_target_properties(mocng_plugin PROPERTIES
         LINKER_LANGUAGE CXX
         LINK_FLAGS "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/ExportedSymbolsList"
         SOVERSION 1.0)

# install(TARGETS mocng_plugin LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})


ADD_DEFINITIONS("-std=c++11 -g -Wall -fno-rtti")
ADD_DEFINITIONS("-D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS ")


if (APPLE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}  -stdlib=libc++ ")
    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}  -stdlib=libc++" )
endif()

