nap_library(gtnap "osapp;officesdk" "${NAPPGUI_SHARED}" NRC_NONE)
target_include_directories(gtnap PUBLIC "..")
target_include_directories(gtnap PUBLIC "../..") # For gtnap.ch
target_include_directories(gtnap PUBLIC "../../../../include")
target_compile_definitions(gtnap PUBLIC "-DBINARY_DIR=\"${CMAKE_BINARY_DIR}\"")
