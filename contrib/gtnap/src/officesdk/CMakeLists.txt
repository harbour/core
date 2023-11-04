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
target_include_directories(${TARGET_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)
target_link_directories(${TARGET_NAME} PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

# target_include_directories()
# include_directories(${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}/com/sun/star ${LOROOT}/sdk/include)

# target_link_directories(unotest PRIVATE ${LOROOT}/program ${LOROOT}/sdk/lib)

# target_link_libraries(${TARGET_NAME}
#     -luno_sal
#     -luno_cppu
#     -luno_cppuhelpergcc3
#     -luno_salhelpergcc3
#     -lunoidllo
#     -lxmlreaderlo
#     -lreglo
#     -lmergedlo
#     )

add_definitions(-DLINUX)
execute_process(COMMAND ${LOROOT}/sdk/bin/cppumaker -Gc -O${CMAKE_CURRENT_BINARY_DIR} ${LOROOT}/program/types.rdb ${LOROOT}/program/types/offapi.rdb)



# g++ '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemplo.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemploauxiliar.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemploentradadados.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemplomenu.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemplodbf.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemplovetor.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemplotextomemoria.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/exemplotextoarquivo.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/acentos.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/arqtexto.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/entrada.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/fileio2.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/outros.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/janela.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/selecaoa.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/selecaov.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/menuvert.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/tbrowsesubclass.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/stubs.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/inkey_.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/texto.o' '/home/fran/harbour_nappgui/contrib/gtnap/src/exemplo/hbmk_0s3xss.o'   -Wl,--start-group -lgtnap -lofficesdk -losapp -lgui -losgui -ldraw2d -lgeom2d -lcore -losbs -lsewer -lpthread -l'uno_sal' -l'uno_cppu' -l'uno_cppuhelpergcc3' -l'uno_salhelpergcc3' -lunoidllo -lxmlreaderlo -lreglo -lmergedlo -l'gtk-3' -l'gdk-3' -l'pangocairo-1.0' -lcairo -l'gdk_pixbuf-2.0' -l'pango-1.0' -l'gio-2.0' -l'glib-2.0' -l'gobject-2.0' -lhbct -lhbextern -lhbdebug -lhbvm -lhbrtl -lhblang -lhbcpage -lgtcgi -lgtpca -lgtstd -lgttrm -lhbrdd -lhbuddall -lhbusrrdd -lrddntx -lrddcdx -lrddnsx -lrddfpt -lhbrdd -lhbhsx -lhbsix -lhbmacro -lhbcplr -lhbpp -lhbcommon -lm -ldl -lrt -lpcre -lz   -Wl,--end-group -o'/home/fran/harbour_nappgui/contrib/gtnap/build/exemplo' -L'/home/fran/harbour_nappgui/lib/linux/gcc' -L'/home/fran/harbour_nappgui/contrib/gtnap/build' -L/usr/lib/libreoffice/program -L/usr/lib/libreoffice/sdk/lib -L'/home/fran/harbour_nappgui/contrib/gtnap/build/Debug/lib'
