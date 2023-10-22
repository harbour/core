cmake_minimum_required(VERSION 3.5)

project(unotest LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# TODO: From '$OFFICE_HOME'
set(LOROOT /usr/lib/libreoffice)

add_executable(unotest main.cpp)

SET(CMAKE_INCLUDE_CURRENT_DIR ON)


include_directories(${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)

target_link_directories(unotest PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

target_link_libraries(unotest
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

