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

execute_process(COMMAND ${LOROOT}/sdk/bin/cppumaker -Gc -O${CMAKE_CURRENT_BINARY_DIR} ${LOROOT}/program/types.rdb ${LOROOT}/program/types/offapi.rdb)
