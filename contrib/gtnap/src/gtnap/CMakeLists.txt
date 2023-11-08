nap_library(gtnap "osapp;officesdk" "${NAPPGUI_SHARED}" NRC_NONE)
target_include_directories(gtnap PUBLIC "..")
target_include_directories(gtnap PUBLIC "../..") # For gtnap.ch
target_include_directories(gtnap PUBLIC "../../../../include")

# processStaticLib(gtnap "osapp" "")
# target_include_directories(gtnap PUBLIC "C:/harbour_nappgui/contrib/gtnap/../../include")
# target_include_directories(gtnap PUBLIC "C:/harbour_nappgui/contrib/gtnap")


