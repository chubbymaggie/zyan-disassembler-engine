cmake_minimum_required(VERSION 2.8.3)

project(VerteronDisassemblerEngine)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_EXAMPLES "Build examples" TRUE)
option(BUILD_C_BINDINGS "Build C bindings" TRUE) # TODO: default to FALSE when ready
option(BUILD_RUST_BINDINGS "Build Rust bindings" FALSE)

if (NOT BUILD_C_BINDINGS AND BUILD_RUST_BINDINGS)
    message(SEND_ERROR "C bindings are required in order to build rust bindings")
endif ()

if (NOT CONFIGURED_ONCE)
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR
            "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        set(compiler_specific "-Werror")
        set(compiler_specific_cxx "-std=c++0x")
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
        set(compiler_specific "/WX /D_CRT_SECURE_NO_WARNINGS")
    endif ()

    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${compiler_specific} ${compiler_specific_cxx}"
        CACHE STRING "Flags used by the compiler during all build types." FORCE)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${compiler_specific}"
        CACHE STRING "Flags used by the compiler during all build types." FORCE)
endif ()

# Library
set(vde_headers
    "VerteronDisassemblerEngine/VXDisassembler.h"
    "VerteronDisassemblerEngine/VXDisassemblerTypes.h"
    "VerteronDisassemblerEngine/VXDisassemblerUtils.h"
    "VerteronDisassemblerEngine/VXInstructionDecoder.h"
    "VerteronDisassemblerEngine/VXInstructionFormatter.h"
    "VerteronDisassemblerEngine/VXOpcodeTable.h")
set(vde_sources
    "VerteronDisassemblerEngine/VXDisassemblerUtils.cpp"
    "VerteronDisassemblerEngine/VXInstructionFormatter.cpp"
    "VerteronDisassemblerEngine/VXOpcodeTable.cpp"
    "VerteronDisassemblerEngine/VXInstructionDecoder.cpp")

add_library("VerteronDisassemblerEngine" ${vde_headers} ${vde_sources})

# C bindings
if (BUILD_C_BINDINGS)
    set(vdec_headers
        "Bindings/C/VXDisassemblerC.h"
        "Bindings/C/VXDisassemblerTypesC.h"
        "Bindings/C/VXDisassemblerUtilsC.h"
        "Bindings/C/VXInstructionDecoderC.h"
        "Bindings/C/VXInstructionFormatterC.h"
        "Bindings/C/VXOpcodeTableC.h")
    set(vdec_sources
        "Bindings/C/VXDisassemblerUtilsC.cpp"
        "Bindings/C/VXInstructionFormatterC.cpp"
        "Bindings/C/VXOpcodeTableC.cpp"
        "Bindings/C/VXInstructionDecoderC.cpp")
    add_library("VerteronDisassemblerEngineC" ${vdec_headers} ${vdec_sources})
    target_link_libraries("VerteronDisassemblerEngineC" "VerteronDisassemblerEngine")
endif ()

# Rust bindings
if (BUILD_RUST_BINDINGS)
    find_package(rustc)
    find_package(rustdoc)
    include(Rust)

    get_rust_deps("Bindings/Rust/src/lib.rs" rust_bindings_deps)
    rust_crate("Bindings/Rust/src/lib.rs"
               ALL
               TARGET_NAME VerteronDisassemblerEngineRust
               DESTINATION lib
               DEPENDS "${rust_bindings_deps}"
               OTHER_RUSTC_FLAGS --crate-type staticlib --crate-type rlib)
    #add_custom_target(VerteronDisassemblerEngineRust2
    #                  ALL 
    #                  DEPENDS ${VerteronDisassemblerEngineRust_FULL_TARGET})
endif ()

# Examples
if (BUILD_EXAMPLES)
    include_directories("VerteronDisassemblerEngine")

    add_executable("CustomDataSource" "Examples/CustomDataSource/Main.cpp")
    target_link_libraries("CustomDataSource" "VerteronDisassemblerEngine")

    add_executable("PerformanceTest" "Examples/PerformanceTest/Main.cpp")
    target_link_libraries("PerformanceTest" "VerteronDisassemblerEngine")

    add_executable("SimpleDemo" "Examples/SimpleDemo/Main.cpp")
    target_link_libraries("SimpleDemo" "VerteronDisassemblerEngine")

    if (BUILD_C_BINDINGS)
        add_executable("CBindingsTest" "Examples/CBindings/test.c")
        include_directories("Bindings/C")
        target_link_libraries("CBindingsTest" "VerteronDisassemblerEngineC")
    endif ()

    if (BUILD_RUST_BINDINGS)

    endif ()

    if (WIN32)
        add_executable("SymbolResolver" "Examples/SymbolResolver/Main.cpp")
        target_link_libraries("SymbolResolver" "VerteronDisassemblerEngine")
    else ()
        message(STATUS "Example 'SymbolResolver' not compatible with platform, ignoring.")
    endif ()
endif ()

set(CONFIGURED_ONCE TRUE CACHE INTERNAL "CMake has configured at least once.")
