nap_library(gtnap "osapp;officesdk;deblib" "${NAPPGUI_SHARED}" NRC_NONE)
target_include_directories(gtnap PUBLIC "..")
target_include_directories(gtnap PUBLIC "../..") # For gtnap.ch
target_include_directories(gtnap PUBLIC "../../../../include")
