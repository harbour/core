# This CMakeLists.txt is for a Harbour .hbp project converted to C project
# Will generated an executable that link with GTNAP/HBOFFICE ready to full debug
if (NOT EXECUTABLE_NAME)
    message(FATAL_ERROR "EXECUTABLE_NAME is not set.")
endif()

if (NOT HARBOUR_ROOT_PATH)
    message(FATAL_ERROR "HARBOUR_ROOT_PATH is not set.")
endif()

if (NOT LOROOT)
    message(FATAL_ERROR "LOROOT is not set.")
endif()

nap_desktop_app(${EXECUTABLE_NAME} "gtnap;hboffice" NRC_NONE)
target_include_directories(${EXECUTABLE_NAME} PUBLIC "../../src")

# Harbour libraries
set(HARBOUR_LIBS "")
set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbct;hbextern;hbdebug;hbvm;hbrtl;hblang;hbcpage")
if (WIN32)
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtcgi;gtpca;gtstd;hbrtl")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtwin")
else()
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "gtcgi;gtpca;gtstd;gttrm;hbrtl")
endif()
set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbrdd;hbuddall;hbusrrdd;rddntx;hbrtl;rddcdx;rddnsx;rddfpt;hbrdd")
set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbhsx;hbsix;hbmacro;hbcplr;hbpp;hbcommon;hbvm")

# LibreOffice libraries
set(LIBREOFFICE_LIBS "")
if (WIN32)
    set(LIBREOFFICE_LIBS "icppu;icppuhelper;ipurpenvhelper;isal;isalhelper")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(LIBREOFFICE_LIBS "uno_sal;uno_cppu;uno_cppuhelpergcc3;uno_salhelpergcc3;unoidllo;xmlreaderlo;reglo;mergedlo")
elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(LIBREOFFICE_LIBS "uno_cppu;uno_cppuhelpergcc3;uno_purpenvhelpergcc3;uno_sal;uno_salhelpergcc3")
endif()

# LibPaths
if (WIN32)
    if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
        set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/win/msvc64)
    elseif (${CMAKE_CXX_COMPILER_ID} STREQUAL GNU)
        set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/win/mingw64)
    endif()
    set(LIBREOFFICE_LIB_PATH ${LOROOT}/sdk/lib)

elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/linux/gcc)
    set(LIBREOFFICE_LIB_PATH "${LOROOT}/program;${LOROOT}/sdk/lib")

elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(HARBOUR_LIB_PATH ${HARBOUR_ROOT_PATH}/lib/darwin/clang)
    set(LIBREOFFICE_LIB_PATH "${LOROOT}/Contents/Frameworks")

else()
    message(FATAL_ERROR "Platform not supported")

endif()

# Link executable with Harbour libs
foreach(lib ${HARBOUR_LIBS})
    find_library(LIB_NAME_${lib} NAMES ${lib} PATHS "${HARBOUR_LIB_PATH}")
    target_link_libraries(${EXECUTABLE_NAME} ${LIB_NAME_${lib}})
endforeach()

if (${CMAKE_CXX_COMPILER_ID} STREQUAL MSVC)
    target_link_libraries(${EXECUTABLE_NAME} "winmm")
endif()

# Link executable with LibreOffice libs
foreach(lib ${LIBREOFFICE_LIBS})
    if(CMAKE_GENERATOR MATCHES "MinGW Makefiles")
        # find_library() fails in VS libs with MinGW
        target_link_libraries(${EXECUTABLE_NAME} "${LIBREOFFICE_LIB_PATH}/${lib}.lib")
    else()
        find_library(LIB_NAME_${lib} NAMES ${lib} PATHS ${LIBREOFFICE_LIB_PATH})
        target_link_libraries(${EXECUTABLE_NAME} ${LIB_NAME_${lib}})
    endif()
endforeach()
