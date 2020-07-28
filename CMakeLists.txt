cmake_minimum_required(VERSION 3.16)
set(CMAKE_UNITY_BUILD TRUE)
set(CMAKE_UNITY_BUILD_BATCH_SIZE 8)
add_compile_options($<$<CONFIG:Debug>:-Og> -gsplit-dwarf)

add_subdirectory(src)
