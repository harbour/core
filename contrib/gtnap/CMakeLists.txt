# # CMake-based project for cuademo
# cmake_minimum_required(VERSION 2.8.12)
# project(Cuademo)

# # Cuademo *.c files (after hbmk2)
# file(GLOB SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/build/*.c)

# add_executable(exemplo ${SRC_FILES})

#------------------------------------------
# CMake-based project for cuademo
#------------------------------------------
cmake_minimum_required(VERSION 2.8.12)
project(Cuademo)

# Using NAppGUI Build system
get_filename_component(ROOT_PATH ${CMAKE_CURRENT_SOURCE_DIR} PATH)
# set(NAPPGUI_PATH ${ROOT_PATH}/../../nappgui)
#include(${CMAKE_CURRENT_SOURCE_DIR}/nappgui/prj/CMakeNAppGUI.cmake)
message(STATUS "Source dir: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "Build dir: ${CMAKE_CURRENT_BINARY_DIR}")

# # Cuademo *.c files (after hbmk2)
file(GLOB SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/tests/cuademo/gtnap_cualib/build/*.c)
add_executable(exemplo ${SRC_FILES})

include_external_msproject(sewer "../build/sewer/sewer.vcxproj")
# include_external_msproject(osbs "../build/osbs/osbs.vcxproj")
# include_external_msproject(core "../build/core/core.vcxproj")
# include_external_msproject(geom2d "../build/geom2d/geom2d.vcxproj")
# include_external_msproject(draw2d "../build/draw2d/draw2d.vcxproj")
# include_external_msproject(osgui "../build/osgui/osgui.vcxproj")
# include_external_msproject(gui "../build/gui/gui.vcxproj")
# include_external_msproject(inet "../build/inet/inet.vcxproj")
# include_external_msproject(osapp "../build/osapp/osapp.vcxproj")



# Static libraries
# staticLib("sewer" "nappgui/src/sewer" "" NRC_NONE)
# staticLib("osbs" "nappgui/src/osbs" "sewer" NRC_NONE)
# staticLib("core" "nappgui/src/core" "osbs" NRC_NONE)
# staticLib("geom2d" "nappgui/src/geom2d" "core" NRC_NONE)
# staticLib("draw2d" "nappgui/src/draw2d" "geom2d" NRC_NONE)
# staticLib("osgui" "nappgui/src/osgui" "draw2d" NRC_NONE)
# staticLib("gui" "nappgui/src/gui" "draw2d" NRC_EMBEDDED)
# staticLib("inet" "nappgui/src/inet" "core" NRC_NONE)
# staticLib("osapp" "nappgui/src/osapp" "osgui;gui" NRC_NONE)

# Your projects here!

#generateSolution()

