cmake_minimum_required(VERSION 3.11)

set(CMAKE_CXX_STANDARD 17 CACHE STRING "")
set(CMAKE_CXX_STANDARD_REQUIRED TRUE CACHE BOOL "")
set(CMAKE_CXX_EXTENSIONS FALSE CACHE BOOL "")

set(CMAKE_INSTALL_BINDIR "bin")
set(CMAKE_INSTALL_LIBDIR "lib")
set(CMAKE_INSTALL_INCLUDEDIR "include")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY
  "${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
  "${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
  "${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}")

set(CMAKE_BUILD_TYPE "Release" CACHE STRING "")
set_property(CACHE CMAKE_BUILD_TYPE
  PROPERTY STRINGS
  Release Debug RelWithDebInfo MinSizeRel
)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE BOOL "" FORCE)

project(functional_cpp CXX)
include(CTest)

if(MSVC)
else()
  add_compile_options(
    -Wall
    -Wpedantic
    -Werror
  )
  if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options(
      -Weverything
      -Wno-c++98-compat
      -Wno-c++98-compat-pedantic
    )
  endif()
endif()

################################################################################
find_package(Boost REQUIRED system)

################################################################################
# Catch2 targets
add_library(catch2 INTERFACE)
target_include_directories(catch2
  SYSTEM INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/Catch2/single_include
)

# Cause all Catch2 macros to be prefixed with 'CATCH_' to avoid name
# clashes.
target_compile_definitions(catch2
  INTERFACE
  "CATCH_CONFIG_PREFIX_ALL"
)

add_library(catch2_main STATIC catch2_main.cpp)
target_link_libraries(catch2_main PUBLIC catch2)

set_target_properties(
    catch2_main
    PROPERTIES
         CXX_CLANG_TIDY ""
)

add_library(Catch2::Catch2 ALIAS catch2)
add_library(Catch2::Main ALIAS catch2_main)

################################################################################
# Targets for expected library
add_library(expected INTERFACE)
target_include_directories(expected
  SYSTEM INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/expected
)

add_library(TL::expected ALIAS expected)

################################################################################

add_executable(chapter2 chapter2.cpp)
target_link_libraries(chapter2 PRIVATE Catch2::Main)
target_compile_options(chapter2 PRIVATE "-Wno-padded")
add_test(NAME chapter2 COMMAND chapter2)

add_executable(chapter3 chapter3_1.cpp chapter3_2.cpp)
target_link_libraries(chapter3 PRIVATE Catch2::Main)
target_compile_options(chapter3 PRIVATE "-Wno-padded")
add_test(NAME chapter3 COMMAND chapter3)

add_subdirectory(chapter12)
