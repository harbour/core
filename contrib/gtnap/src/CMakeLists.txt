#------------------------------------------
# NAppGUI CMake build script
#------------------------------------------
cmake_minimum_required(VERSION 2.8.12)

if (NOT GTNAP_DEVELOPER_MODE)
    project(NAppGUI)
else()
    project(Cuademo)
endif()

# Using NAppGUI build system
get_filename_component(GTNAP_PATH ${CMAKE_CURRENT_SOURCE_DIR} PATH)
include(${GTNAP_PATH}/prj/CMakeNAppGUI.cmake)

# Static libraries
staticLib("sewer" "sewer" "" NRC_NONE)
staticLib("osbs" "osbs" "sewer" NRC_NONE)
staticLib("core" "core" "osbs" NRC_NONE)
staticLib("geom2d" "geom2d" "core" NRC_NONE)
staticLib("draw2d" "draw2d" "geom2d" NRC_NONE)
staticLib("osgui" "osgui" "draw2d" NRC_NONE)
staticLib("gui" "gui" "draw2d" NRC_EMBEDDED)
staticLib("inet" "inet" "core" NRC_NONE)
staticLib("osapp" "osapp" "osgui;gui" NRC_NONE)

if (GTNAP_DEVELOPER_MODE)
    message("GTNAP Developer mode")
    string(REGEX REPLACE "/w44668" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    string(REGEX REPLACE "/w44255" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    string(REGEX REPLACE "/w44668" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    string(REGEX REPLACE "/w44255" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    staticLib("gtnap" "gtnap" "osapp" NRC_NONE)
    desktopApp(exemplo "exemplo" "gtnap" NRC_NODE)
endif()

generateSolution()

if (GTNAP_DEVELOPER_MODE)
    cmake_policy(SET CMP0079 NEW)
    set(ROOT_PATH ${GTNAP_PATH}/../..)
    set(HARBOUR_LIBS "")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbct.lib;hbextern.lib;hbdebug.lib;hbvm.lib;hbrtl.lib;hblang.lib;hbcpage.lib")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbrdd.lib;hbuddall.lib;hbusrrdd.lib;rddntx.lib;rddcdx.lib;rddnsx.lib;rddfpt.lib;hbrdd.lib")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbhsx.lib;hbsix.lib;hbmacro.lib;hbcplr.lib;hbpp.lib;hbcommon.lib")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "winmm.lib;gtwin.lib")

    target_include_directories("gtnap" PUBLIC ${ROOT_PATH}/include)
    target_link_directories("exemplo" PUBLIC ${ROOT_PATH}/lib/win/msvc)
    target_link_libraries("exemplo" ${HARBOUR_LIBS})

endif()