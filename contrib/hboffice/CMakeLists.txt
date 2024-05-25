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

# Configure C runtime library (avoid link with debug versions)
if (HBOFFICE_DEVELOPER_MODE)
    if (WIN32)
        if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
            removeFlag(CMAKE_CXX_FLAGS_DEBUG "/MTd")
            removeFlag(CMAKE_CXX_FLAGS_DEBUG "/MDd")
            removeFlag(CMAKE_C_FLAGS_DEBUG "/MTd")
            removeFlag(CMAKE_C_FLAGS_DEBUG "/MDd")
            addFlag(CMAKE_CXX_FLAGS_DEBUG "/MT")
            addFlag(CMAKE_C_FLAGS_DEBUG "/MT")
        endif()
    endif()
endif()

# Generate targets
include(${NAPPGUI_ROOT_PATH}/prj/NAppTarget.cmake)
add_subdirectory(src/sewer)
add_subdirectory(src/osbs)
add_subdirectory(src/core)
add_subdirectory(src/officesdk)

# If we are in dev mode, a complete debugeable solution will be created
if (HBOFFICE_DEVELOPER_MODE)
    message(STATUS "- HBOFFICE Developer mode")
    set(HBOFFICE_PATH ${CMAKE_CURRENT_SOURCE_DIR})
    set(HARBOUR_ROOT_PATH ${HBOFFICE_PATH}/../..)

    if (WIN32)
        if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
            string(REGEX REPLACE "/w44668" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
            string(REGEX REPLACE "/w44255" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
            string(REGEX REPLACE "/w44668" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
            string(REGEX REPLACE "/w44255" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        endif()
    endif()

    # Add hboffice wrapper to solution
    add_subdirectory("src/hboffice")

    # Link with Harbour libraries
    set(HARBOUR_LIBS "")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbct;hbextern;hbdebug;hbvm;hbrtl;hblang;hbcpage")
    if (WIN32)
        set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtcgi;gtpca;gtstd;hbrtl")
    else()
        set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtcgi;gtpca;gtstd;gttrm;hbrtl")
    endif()
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbrdd;hbuddall;hbusrrdd;rddntx;hbrtl;rddcdx;rddnsx;rddfpt;hbrdd")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbhsx;hbsix;hbmacro;hbcplr;hbpp;hbcommon;hbvm")

    if (WIN32)
        if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
            set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/win/msvc64)
        elseif (${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
            set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/win/mingw64)
        endif()

        set(HARBOUR_LIBS ${HARBOUR_LIBS} "winmm;gtwin")
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
        set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/darwin/clang)
    else()
        set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/linux/gcc)
    endif()

    # Link with LibreOffice libraries
    set(LOROOT "$ENV{LIBREOFFICE_HOME}")

    if (NOT LOROOT)
        message(FATAL_ERROR "LIBREOFFICE_HOME environment variable is required to compile GTNAP")
    endif()

    if (WIN32)
        set(LIBREOFFICE_LIB_PATH ${LOROOT}/sdk/lib)
        if(NOT CMAKE_GENERATOR MATCHES "Visual Studio")
            message(FATAL_ERROR "HBOFFICE developer solution only allows Visual Studio in Windows")
        endif()
        #file(TO_NATIVE_PATH \"${LIBREOFFICE_LIB_PATH}\" LIBREOFFICE_LIB_PATH)
        set(LIBREOFFICE_LIBS "icppu;icppuhelper;ipurpenvhelper;isal;isalhelper")
    else()
        set(LIBREOFFICE_LIB_PATH "${LOROOT}/program;${LOROOT}/sdk/lib")
        set(LIBREOFFICE_LIBS "uno_sal;uno_cppu;uno_cppuhelpergcc3;uno_salhelpergcc3;unoidllo;xmlreaderlo;reglo;mergedlo")
    endif()

    # Add example applications to development solution
    file(COPY "${HBOFFICE_PATH}/resources/CMakeLists.txt" DESTINATION "${HBOFFICE_PATH}/src/sheet1")
    if (WIN32)
        file(COPY "${HBOFFICE_PATH}/resources/hbmk2_win.c" DESTINATION "${HBOFFICE_PATH}/src/sheet1")
    else()
        file(COPY "${HBOFFICE_PATH}/resources/hbmk2_lin.c" DESTINATION "${HBOFFICE_PATH}/src/sheet1")
    endif()
    set(EXAMPLE_NAME "sheet1")
    add_subdirectory("src/sheet1")

    # file(COPY "${GTNAP_PATH}/resources/CMakeLists.txt" DESTINATION "${GTNAP_PATH}/src/hello")
    # if (WIN32)
    #     file(COPY "${GTNAP_PATH}/resources/hbmk2_win.c" DESTINATION "${GTNAP_PATH}/src/hello")
    # else()
    #     file(COPY "${GTNAP_PATH}/resources/hbmk2_lin.c" DESTINATION "${GTNAP_PATH}/src/hello")
    # endif()
    # set(EXAMPLE_NAME "hello")
    # add_subdirectory("src/hello")

endif()
