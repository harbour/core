# This file is part of NAppGUI-SDK project
# See README.txt and LICENSE.txt

# Check for valid GCC version
#------------------------------------------------------------------------------
# macro(nap_gcc_version)

#     if (CMAKE_COMPILER_IS_GNUCC)
#        if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.6)
#             message(FATAL_ERROR "GCC 4.6 is the minimum supported version in Linux.")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.7)
#             set(GCC_VERSION "gcc4_6")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9)
#             set(GCC_VERSION "gcc4_8")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5)
#             set(GCC_VERSION "gcc4_9")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 6)
#             set(GCC_VERSION "gcc5")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7)
#             set(GCC_VERSION "gcc6")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 8)
#             set(GCC_VERSION "gcc7")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9)
#             set(GCC_VERSION "gcc8")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10)
#             set(GCC_VERSION "gcc9")
#        elseif (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 11)
#             set(GCC_VERSION "gcc10")
#        else()
#             set(GCC_VERSION "gcc11")
#        endif()

#     else()
#         message(FATAL_ERROR "GCC Is not present.")

#     endif()

# endmacro()

macro(nap_gcc_flags arch)

    # Warnings
    set(FLAGS "-Wall -Wextra -pedantic -fPIE -Wno-long-long -Wno-overlength-strings")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${FLAGS}")

    # Force ANSI-C for old compilers
    if(${CMAKE_VERSION} VERSION_LESS "3.1.0" OR ${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "5.0.0")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu90")
    endif()

    # Architecture
    if (${arch} STREQUAL "x86")
        set(ARCH_FLAG "-m32")

    elseif (${arch} STREQUAL "x64")
        set(ARCH_FLAG "-m64")

    elseif (${arch} STREQUAL "arm64")
        # Ok!

    elseif (${arch} STREQUAL "arm")
        set(ARCH_FLAG "-march=armv7-a")

    endif()

    if (ARCH_FLAG)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ARCH_FLAG}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ARCH_FLAG}")
    endif()

endmacro()

