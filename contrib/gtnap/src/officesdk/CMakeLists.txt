cmake_minimum_required(VERSION 3.5)
set(TARGET_NAME officesdk)
nap_library(${TARGET_NAME} "core" NO NRC_NONE)
target_include_directories(${TARGET_NAME} PUBLIC "..")

set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD 11)
set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

set(LOROOT "$ENV{LIBREOFFICE_HOME}")

if (LOROOT)
    message("- LIBREOFFICE_HOME: ${LOROOT}")
else()
    message(FATAL_ERROR "LIBREOFFICE_HOME environment variable is required to compile GTNAP")
endif()

target_include_directories(${TARGET_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)
target_link_directories(${TARGET_NAME} PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

if (NOT WIN32)
    add_definitions(-DLINUX)
endif()

# Generate the LibreOffice-SDK C++ headers
set(CPPUMAKER ${LOROOT}/sdk/bin/cppumaker)
set(CPPTYPES ${LOROOT}/program/types.rdb)
set(CPPOFFAPI ${LOROOT}/program/types/offapi.rdb)
file(TO_NATIVE_PATH \"${CPPUMAKER}\" CPPUMAKER)
file(TO_NATIVE_PATH \"${CPPTYPES}\" CPPTYPES)
file(TO_NATIVE_PATH \"${CPPOFFAPI}\" CPPOFFAPI)
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
