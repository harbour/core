#------------------------------------------------------------------------------
# Full Exemplo Development solution
# 2024 Francisco Garcia Collado
#------------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.0)
project(Exemplo)

# Globals
set(LOROOT "$ENV{LIBREOFFICE_HOME}")
set(HARBOUR_ROOT_PATH ${CMAKE_CURRENT_SOURCE_DIR})
set(NAPPGUI_ROOT_PATH ${HARBOUR_ROOT_PATH}/contrib/gtnap)
set(GTNAP_ROOT_PATH ${HARBOUR_ROOT_PATH}/contrib/gtnap)
set(HBOFFICE_ROOT_PATH ${HARBOUR_ROOT_PATH}/contrib/hboffice)
set(NAPPGUI_SHARED False CACHE BOOL "Build NAppGUI shared libraries.")
set(HBOFFICE_DEVELOPER_MODE YES)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Welcome
string(TIMESTAMP CURRENT_YEAR %Y)
include(${NAPPGUI_ROOT_PATH}/prj/NAppVersion.cmake)
include(${NAPPGUI_ROOT_PATH}/prj/NAppUtils.cmake)
message (STATUS "---------------------------------------------")
message (STATUS "Harbour Exemplo FULL Debug solution")
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

# Avoid warnings
if (WIN32)
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        string(REGEX REPLACE "/w44668" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
        string(REGEX REPLACE "/w44255" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
        string(REGEX REPLACE "/w44668" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        string(REGEX REPLACE "/w44255" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        removeFlag(CMAKE_CXX_FLAGS_DEBUG "/MTd")
        removeFlag(CMAKE_CXX_FLAGS_DEBUG "/MDd")
        removeFlag(CMAKE_C_FLAGS_DEBUG "/MTd")
        removeFlag(CMAKE_C_FLAGS_DEBUG "/MDd")
        addFlag(CMAKE_CXX_FLAGS_DEBUG "/MT")
        addFlag(CMAKE_C_FLAGS_DEBUG "/MT")
    endif()

endif()

# Tools required to generate NAppGUI projets ('nrc')
# They need to be compiled before NAppGUI configure/generate
include(${NAPPGUI_ROOT_PATH}/prj/NAppGenerateTools.cmake)
nap_generate_tools()

# Generate C executable targets for .hbp
include(nap-dev/HarbourTools.cmake)
harbour_to_c_exe("exemplo" "${GTNAP_ROOT_PATH}/src/exemplo" "${GTNAP_ROOT_PATH}/tests/cuademo/gtnap_cualib/exemplo.hbp")

# Generate targets
include(${NAPPGUI_ROOT_PATH}/prj/NAppTarget.cmake)

# NAppGUI
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/sewer)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/osbs)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/core)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/geom2d)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/draw2d)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/osgui)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/gui)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/osapp)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/inet)

# Debugger
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/deblib)
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/debugger)

# GTNAP
add_subdirectory(${NAPPGUI_ROOT_PATH}/src/gtnap)

# HBOffice
add_subdirectory(${HBOFFICE_ROOT_PATH}/src/officesdk)
add_subdirectory(${HBOFFICE_ROOT_PATH}/src/hboffice)

# Add the Harbour executable projects
set(EXECUTABLE_NAME "exemplo")
add_subdirectory("${GTNAP_ROOT_PATH}/src/exemplo")
