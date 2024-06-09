nap_library(gtnap "osapp;deblib" "${NAPPGUI_SHARED}" NRC_NONE)
target_include_directories(gtnap PUBLIC "..")                   # For NAppGUI headers
target_include_directories(gtnap PUBLIC "../..")                # For gtnap.ch
target_include_directories(gtnap PUBLIC "../../../../include")  # For Harbours headers
