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
    # set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} "/wd4668")
    # set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "/wd4668")
    staticLib("gtnap" "gtnap" "osapp" NRC_NONE)
    desktopApp(exemplo "exemplo" "gtnap" NRC_NODE)
endif()

generateSolution()

if (GTNAP_DEVELOPER_MODE)
    cmake_policy(SET CMP0079 NEW)
    set(ROOT_PATH ${GTNAP_PATH}/../..)
    set(HARBOUR_LIBS "")
    set(HARBOUR_LIBS ${HARBOUR_LIBS} "hbct.lib hbextern.lib hbdebug.lib hbvm.lib hbrtl.lib hblang.lib hbcpage.lib")
    target_include_directories("gtnap" PUBLIC ${ROOT_PATH}/include)
    target_link_directories("exemplo" PUBLIC ${ROOT_PATH}/lib/win/msvc)
    target_link_libraries("exemplo" ${HARBOUR_LIBS})
    #target_compile_options("example" PRIVATE /WD4668)


    #cmake_policy()
 #C:\harbour_nappgui\contrib\gtnap\src\exemplo\hbmk_17hew9.obj
#  -libpath:C:\harbour_nappgui\lib\win\msvc
#  -libpath:C:\harbour_nappgui\contrib\gtnap\build
#  -libpath:C:\harbour_nappgui\contrib\gtnap\build\Debug\lib
#  /NODEFAULTLIB:LIBCMT.lib
#  -debug -subsystem:windows
#  -nxcompat -dynamicbase
#  -fixed:no  gtnap.lib osapp.lib gui.lib osgui.lib draw2d.lib geom2d.lib core.lib osbs.lib sewer.lib

#   gtcgi.lib gtpca.lib gtstd.lib gtwin.lib gtwvt.lib gtgui.lib hbrdd.lib hbuddall.lib hbusrrdd.lib rddntx.lib rddcdx.lib rddnsx.lib rddfpt.lib hbrdd.lib hbhsx.lib hbsix.lib hbmacro.lib hbcplr.lib hbpp.lib hbcommon.lib winmm.lib kernel32.lib user32.lib gdi32.lib advapi32.lib ws2_32.lib iphlpapi.lib winspool.lib comctl32.lib comdlg32.lib shell32.lib uuid.lib ole32.lib oleaut32.lib mpr.lib mapi32.lib imm32.lib msimg32.lib wininet.lib hbpcre.lib hbzlib.lib



endif()