#------------------------------------------
# NAppGUI CMake build script
#------------------------------------------
cmake_minimum_required(VERSION 2.8.12)
project(NAppGUI)

# NAppGUI Build Scripts
get_filename_component(ROOT_PATH ${CMAKE_CURRENT_SOURCE_DIR} PATH)
include(${ROOT_PATH}/prj/CMakeNAppGUI.cmake)

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

# Your projects here!

generateSolution()
