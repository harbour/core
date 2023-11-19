nap_desktop_app(${EXAMPLE_NAME} "gtnap" NRC_NONE)
target_include_directories(${EXAMPLE_NAME} PUBLIC "../../src")

foreach(lib ${HARBOUR_LIBS})
    find_library(LIB_NAME_${lib} NAMES ${lib} PATHS "${HARBOUR_LIB_PATH}")
    target_link_libraries(${EXAMPLE_NAME} ${LIB_NAME_${lib}})
endforeach()

foreach(lib ${LIBREOFFICE_LIBS})
    find_library(LIB_NAME_${lib} NAMES ${lib} PATHS ${LIBREOFFICE_LIB_PATH})
    target_link_libraries(${EXAMPLE_NAME} ${LIB_NAME_${lib}})
endforeach()
