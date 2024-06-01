nap_library(hboffice "officesdk" NO NRC_NONE)
target_include_directories(hboffice PUBLIC "..")                    # For NAppGUI headers
target_include_directories(hboffice PUBLIC "../..")                 # For hboffice.ch
target_include_directories(hboffice PUBLIC "../../../../include")   # For Harbours headers
