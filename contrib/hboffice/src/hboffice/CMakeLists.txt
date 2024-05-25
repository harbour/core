nap_library(hboffice "officesdk" NO NRC_NONE)
target_include_directories(hboffice PUBLIC "..")
target_include_directories(hboffice PUBLIC "../..") # For hboffice.ch
target_include_directories(hboffice PUBLIC "../../../../include")
