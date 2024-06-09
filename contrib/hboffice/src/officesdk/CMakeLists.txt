cmake_minimum_required(VERSION 3.5)
set(TARGET_NAME officesdk)

if (HBOFFICE_DEVELOPER_MODE)
    set(OFFICESDK_SHARED NO)
else()
    set(OFFICESDK_SHARED YES)
endif()

nap_library(${TARGET_NAME} "core" ${OFFICESDK_SHARED} NRC_NONE)
target_include_directories(${TARGET_NAME} PUBLIC "..")

if (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
    target_compile_options(${TARGET_NAME} PUBLIC "-Wno-inline" "-Wno-attributes")
endif()

set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD 11)
set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

set(LOROOT "$ENV{LIBREOFFICE_HOME}")

if (LOROOT)
    message("- LIBREOFFICE_HOME: ${LOROOT}")
else()
    message(FATAL_ERROR "LIBREOFFICE_HOME environment variable is required to compile ${TARGET_NAME}")
endif()

target_include_directories(${TARGET_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)

if (WIN32)
    if (NOT CMAKE_GENERATOR MATCHES "Visual Studio")
        message(FATAL_ERROR "Visual Studio is required to compile ${TARGET_NAME} in Windows")
    endif()
else()
    target_compile_definitions(${TARGET_NAME} PUBLIC -DLINUX)
endif()

# Generate the LibreOffice-SDK C++ headers
# from SDK *.rdb files
IF(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/com/sun/star")
    message("- Generating LibreOffice C++ headers with cppumaker")
    set(CPPUMAKER ${LOROOT}/sdk/bin/cppumaker)
    set(CPPTYPES ${LOROOT}/program/types.rdb)
    set(CPPOFFAPI ${LOROOT}/program/types/offapi.rdb)
    execute_process(COMMAND "${CPPUMAKER}" "-Gc" "-O${CMAKE_CURRENT_BINARY_DIR}" "${CPPTYPES}" "${CPPOFFAPI}" RESULT_VARIABLE RES_VAR OUTPUT_VARIABLE OUT_VAR ERROR_VARIABLE ERR_VAR)

    if (NOT "${RES_VAR}" STREQUAL "0")
        message("CPPUMAKER RES: ${RES_VAR}")

        if (OUT_VAR)
            message("CPPUMAKER OUT: ${OUT_VAR}")
        endif()

        if (ERR_VAR)
            message("CPPUMAKER ERR: ${ERR_VAR}")
        endif()

        message(FATAL_ERROR "Error generating LibreOffice C++ header files")
    endif()

endif()

# Link with the LibreOffice libraries
if (OFFICESDK_SHARED)
    if (WIN32)
        set(LIBREOFFICE_LIB_PATH ${LOROOT}/sdk/lib)
        set(LIBREOFFICE_LIBS "icppu;icppuhelper;ipurpenvhelper;isal;isalhelper")
    else()
        set(LIBREOFFICE_LIB_PATH "${LOROOT}/program;${LOROOT}/sdk/lib")
        set(LIBREOFFICE_LIBS "uno_sal;uno_cppu;uno_cppuhelpergcc3;uno_salhelpergcc3;unoidllo;xmlreaderlo;reglo;mergedlo")
    endif()

    foreach(lib ${LIBREOFFICE_LIBS})
        find_library(LIB_NAME_${lib} NAMES ${lib} PATHS ${LIBREOFFICE_LIB_PATH})
        target_link_libraries(${TARGET_NAME} ${LIB_NAME_${lib}})
    endforeach()
endif()
