# Generate CMake-based C executable from .hbp project
function(harbour_to_c_exe prjName destCPath hbpFullPath)

if (NOT HARBOUR_ROOT_PATH)
    message(FATAL_ERROR "HARBOUR_ROOT_PATH is not set.")
endif()

message(" - Generating '${prjName}' C files")

# Clean destiny of C files
file(REMOVE_RECURSE ${destCPath})
file(MAKE_DIRECTORY ${destCPath})

# Select Harbour compiler
if (WIN32)
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        set(HBMK2_COMPILER msvc64)
        set(HBMK2_COMMAND ${HARBOUR_ROOT_PATH}/bin/win/${HBMK2_COMPILER}/hbmk2.exe)
    else()
        message(FATAL_ERROR "Compiler not supported")
    endif()

elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(HBMK2_COMPILER gcc)
    set(HBMK2_COMMAND ${HARBOUR_ROOT_PATH}/bin/linux/${HBMK2_COMPILER}/hbmk2)

elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(HBMK2_COMPILER clang)
    set(HBMK2_COMMAND ${HARBOUR_ROOT_PATH}/bin/darwin/${HBMK2_COMPILER}/hbmk2)

else()
    message(FATAL_ERROR "Platform not supported")

endif()

# Launch HBMK2 over .hbp
# Important!! HBMK2 can generate errors, but not important for us at this point.
# We only want the .c files from the .prg files
execute_process(
                COMMAND
                ${HBMK2_COMMAND}
                "-debug"
                "-comp=${HBMK2_COMPILER}"
                "-keepc"
                "-workdir=${destCPath}"
                "-o${CMAKE_BINARY_DIR}/${prjName}"
                ${hbpFullPath}
                RESULT_VARIABLE HBMKResult
                OUTPUT_VARIABLE HBMKOutput
                ERROR_VARIABLE HBMKError)

# Copy the CMakeLists.txt for the generated C project
file(COPY "${HARBOUR_ROOT_PATH}/nap-dev/CMakeLists.txt" DESTINATION "${destCPath}")

# Copy the hbmk2 linker files to general terminals
if (WIN32)
    file(COPY "${HARBOUR_ROOT_PATH}/nap-dev/hbmk2_win.c" DESTINATION "${destCPath}")
else()
    file(COPY "${HARBOUR_ROOT_PATH}/nap-dev/hbmk2_lin.c" DESTINATION "${destCPath}")
endif()

endfunction()

