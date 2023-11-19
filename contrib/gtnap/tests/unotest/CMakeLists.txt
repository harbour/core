cmake_minimum_required(VERSION 3.5)

project(unotest LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# TODO: From '$OFFICE_HOME'
if (WIN32)
    set(LOROOT "C:/Program Files/LibreOffice")
else()
    set(LOROOT /usr/lib/libreoffice)
endif()

add_executable(unotest main.cpp)

SET(CMAKE_INCLUDE_CURRENT_DIR ON)

include_directories(${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)

target_link_directories(unotest PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

if (WIN32)
target_link_libraries(unotest
    icppu
    icppuhelper
    ipurpenvhelper
    isal
    isalhelper
    )

else()
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
endif()

#set(CPPUMAKER "${LOROOT}/sdk/bin/cppulmaker\"" -Gc -O\"${CMAKE_CURRENT_BINARY_DIR}\" \"${LOROOT}/program/types.rdb\" \"${LOROOT}/program/types/offapi.rdb\")
#set(CPPUMAKER "\"${LOROOT}\\sdk\\bin\\cppumaker\"" -Gc -O\"${CMAKE_CURRENT_BINARY_DIR}\" \"${LOROOT}/program/types.rdb\" \"${LOROOT}/program/types/offapi.rdb\")
#message("${CPPUMAKER}")
set(CPPUMAKER ${LOROOT}/sdk/bin/cppumaker)
set(CPPTYPES ${LOROOT}/program/types.rdb)
set(CPPOFFAPI ${LOROOT}/program/types/offapi.rdb)
file(TO_NATIVE_PATH \"${CPPUMAKER}\" CPPUMAKER)
file(TO_NATIVE_PATH \"${CPPTYPES}\" CPPTYPES)
file(TO_NATIVE_PATH \"${CPPOFFAPI}\" CPPOFFAPI)
# message(${CPPUMAKER})
execute_process(COMMAND "${CPPUMAKER}" "-Gc" "-O${CMAKE_CURRENT_BINARY_DIR}" "${CPPTYPES}" "${CPPOFFAPI}" RESULT_VARIABLE RES OUTPUT_VARIABLE OUT_VAR ERROR_VARIABLE ERR_VAR)
#execute_process(COMMAND ${CPPUMAKER} OUTPUOT_VARIABLE out ERROR_VARIABLE err)
# message("RES: ${RES}")
# message("OUT: ${OUT_VAR}")
# message("ERR: ${ERR_VAR}")

