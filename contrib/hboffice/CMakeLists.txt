#------------------------------------------------------------------------------
# HBOffice CMake build script
# Copyright (C) 2024 Francisco García Collado
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

# # Tools required to generate NAppGUI projets ('nrc')
# # They need to be compiled before NAppGUI configure/generate
# include(${NAPPGUI_ROOT_PATH}/prj/NAppGenerateTools.cmake)
# nap_generate_tools()

# Generate targets
include(${NAPPGUI_ROOT_PATH}/prj/NAppTarget.cmake)
# include(${NAPPGUI_ROOT_PATH}/CMakeTargets.cmake)

# Configure C runtime library (avoid link with debug versions)
# if (GTNAP_DEVELOPER_MODE)
#     if (WIN32)
#         if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
#             removeFlag(CMAKE_CXX_FLAGS_DEBUG "/MTd")
#             removeFlag(CMAKE_CXX_FLAGS_DEBUG "/MDd")
#             removeFlag(CMAKE_C_FLAGS_DEBUG "/MTd")
#             removeFlag(CMAKE_C_FLAGS_DEBUG "/MDd")
#             addFlag(CMAKE_CXX_FLAGS_DEBUG "/MT")
#             addFlag(CMAKE_C_FLAGS_DEBUG "/MT")
#         endif()
#     endif()
# endif()

# Enable the support for LibreOffice in source code
# if (GTNAP_LIBREOFFICE)
#     add_definitions("-DGTNAP_LIBREOFFICE")
# endif()

if (LIBREOFFICE_DLL)
    add_subdirectory(src/sewer)
    add_subdirectory(src/osbs)
    add_subdirectory(src/core)
    add_subdirectory(src/officesdk)
endif()

# # If we are in dev mode, 'gtnap' and 'exemplo' target will be built in same solution
# if (GTNAP_DEVELOPER_MODE)
#     message(STATUS "- GTNAP Developer mode")
#     set(GTNAP_PATH ${CMAKE_CURRENT_SOURCE_DIR})
#     set(HARBOUR_ROOT_PATH ${GTNAP_PATH}/../..)

#     if (WIN32)
#         if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
#             string(REGEX REPLACE "/w44668" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
#             string(REGEX REPLACE "/w44255" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
#             string(REGEX REPLACE "/w44668" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
#             string(REGEX REPLACE "/w44255" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
#         endif()
#     endif()

#     # Link with Harbour libraries
#     set(HARBOUR_LIBS "")
#     set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbct;hbextern;hbdebug;hbvm;hbrtl;hblang;hbcpage")
#     if (WIN32)
#         set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtcgi;gtpca;gtstd;hbrtl")
#     else()
#         set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtcgi;gtpca;gtstd;gttrm;hbrtl")
#     endif()
#     set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbrdd;hbuddall;hbusrrdd;rddntx;hbrtl;rddcdx;rddnsx;rddfpt;hbrdd")
#     set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbhsx;hbsix;hbmacro;hbcplr;hbpp;hbcommon;hbvm")

#     if (WIN32)
#         if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
#             set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/win/msvc64)
#         elseif (${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
#             set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/win/mingw64)
#         endif()

#         set(HARBOUR_LIBS ${HARBOUR_LIBS} "winmm;gtwin")
#     elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
#         set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/darwin/clang)
#     else()
#         set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/linux/gcc)
#     endif()

#     add_subdirectory("src/gtnap")

#     if (GTNAP_LIBREOFFICE)
#         # Libreoffice link libraries
#         set(LOROOT "$ENV{LIBREOFFICE_HOME}")

#         if (NOT LOROOT)
#             message(FATAL_ERROR "LIBREOFFICE_HOME environment variable is required to compile GTNAP")
#         endif()

#         if (WIN32)
#             set(LIBREOFFICE_LIB_PATH ${LOROOT}/sdk/lib)
#             if(CMAKE_GENERATOR MATCHES "MinGW Makefiles")
#                 STRING(REGEX REPLACE "\\\\" "/" LIBREOFFICE_LIB_PATH ${LIBREOFFICE_LIB_PATH})
#             else()
#                 file(TO_NATIVE_PATH \"${LIBREOFFICE_LIB_PATH}\" LIBREOFFICE_LIB_PATH)
#             endif()
#             set(LIBREOFFICE_LIBS "icppu;icppuhelper;ipurpenvhelper;isal;isalhelper")
#         else()
#             set(LIBREOFFICE_LIB_PATH "${LOROOT}/program;${LOROOT}/sdk/lib")
#             set(LIBREOFFICE_LIBS "uno_sal;uno_cppu;uno_cppuhelpergcc3;uno_salhelpergcc3;unoidllo;xmlreaderlo;reglo;mergedlo")
#         endif()

#     endif()

#     # Example applications
#     file(COPY "${GTNAP_PATH}/resources/CMakeLists.txt" DESTINATION "${GTNAP_PATH}/src/exemplo")
#     if (WIN32)
#         file(COPY "${GTNAP_PATH}/resources/hbmk2_win.c" DESTINATION "${GTNAP_PATH}/src/exemplo")
#     else()
#         file(COPY "${GTNAP_PATH}/resources/hbmk2_lin.c" DESTINATION "${GTNAP_PATH}/src/exemplo")
#     endif()
#     set(EXAMPLE_NAME "exemplo")
#     add_subdirectory("src/exemplo")

#     file(COPY "${GTNAP_PATH}/resources/CMakeLists.txt" DESTINATION "${GTNAP_PATH}/src/hello")
#     if (WIN32)
#         file(COPY "${GTNAP_PATH}/resources/hbmk2_win.c" DESTINATION "${GTNAP_PATH}/src/hello")
#     else()
#         file(COPY "${GTNAP_PATH}/resources/hbmk2_lin.c" DESTINATION "${GTNAP_PATH}/src/hello")
#     endif()
#     set(EXAMPLE_NAME "hello")
#     add_subdirectory("src/hello")

# endif()
