#------------------------------------------------------------------------------
# HBOffice CMake build script
# Copyright (C) 2024 Francisco Garci­a Collado
#------------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.0)
project(HBOffice)

set(NAPPGUI_ROOT_PATH ${CMAKE_CURRENT_SOURCE_DIR})

# Configurations
set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE INTERNAL "Configuration Types" FORCE)

# Globals
set(NAPPGUI_SHARED False CACHE BOOL "Build NAppGUI shared libraries.")
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(INSTALL_PERM "OWNER_READ;OWNER_WRITE;OWNER_EXECUTE;GROUP_READ;GROUP_EXECUTE;WORLD_READ;WORLD_EXECUTE")

# Welcome
string(TIMESTAMP CURRENT_YEAR %Y)
include(${NAPPGUI_ROOT_PATH}/prj/NAppVersion.cmake)
include(${NAPPGUI_ROOT_PATH}/prj/NAppUtils.cmake)
message (STATUS "---------------------------------------------")
message (STATUS "Harbour LibreOffice Wrapper")
message (STATUS "${CURRENT_YEAR} Francisco Garcia Collado")
message (STATUS "---------------------------------------------")
nap_build_opt("NAppGUI Version" "${NAPPGUI_MAJOR}.${NAPPGUI_MINOR}.${NAPPGUI_PATCH}.${NAPPGUI_BUILD}")
nap_build_opt("Generator" "${CMAKE_GENERATOR}")
nap_build_opt("Build shared" "${NAPPGUI_SHARED}")

# Config compiler
include(${NAPPGUI_ROOT_PATH}/prj/NAppCompilers.cmake)
nap_config_compiler()
nap_build_opt("Compiler" "${CMAKE_CXX_COMPILER_ID}")
nap_build_opt("Compiler Vers" "${CMAKE_CXX_COMPILER_VERSION}")
nap_build_opt("Build type" "${CMAKE_BUILD_TYPE}")
nap_build_opt("Build arch" "${CMAKE_ARCHITECTURE}")
nap_build_opt("Host arch" "${CMAKE_HOST_ARCHITECTURE}")
nap_build_opt("PackageID" "${CMAKE_PACKAGE_ID}")
message (STATUS "---------------------------------------------")

# Generate targets
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
include(${NAPPGUI_ROOT_PATH}/prj/NAppTarget.cmake)
add_subdirectory(src/sewer)
add_subdirectory(src/osbs)
add_subdirectory(src/core)
add_subdirectory(src/officesdk)
