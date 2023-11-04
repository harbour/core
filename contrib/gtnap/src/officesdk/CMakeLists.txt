cmake_minimum_required(VERSION 3.5)
set(TARGET_NAME officesdk)
nap_library(${TARGET_NAME} "core" NO NRC_NONE)
target_include_directories(${TARGET_NAME} PUBLIC "..")


set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD 11)
set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

# set(TARGET_NAME officesdk)
# set(CMAKE_CXX_STANDARD 11)
# set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(LOROOT "$ENV{LIBREOFFICE_HOME}")

if (NOT LOROOT)
    message(FATAL_ERROR "LIBREOFFICE_HOME environment variable is required to compile GTNAP")
endif()

# add_library(${targetName} STATIC officesdk.cpp)

# add_executable(unotest main.cpp)

# SET(CMAKE_INCLUDE_CURRENT_DIR ON)
target_include_directories(${TARGET_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)
target_link_directories(${TARGET_NAME} PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

# target_include_directories()
# include_directories(${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)

# target_link_directories(unotest PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

target_link_libraries(${TARGET_NAME}
    -luno_sal
    -luno_cppu
    -luno_cppuhelpergcc3
    -luno_salhelpergcc3
    -lunoidllo
    -lxmlreaderlo
    -lreglo
    -lmergedlo
    )

add_definitions(-DLINUX)
execute_process(COMMAND ${LOROOT}/sdk/bin/cppumaker -Gc -O${CMAKE_CURRENT_BINARY_DIR} ${LOROOT}/program/types.rdb ${LOROOT}/program/types/offapi.rdb)

