nap_library(deblib "gui" NO NRC_NONE)
target_compile_definitions(deblib PUBLIC "-DDEBUGGER_PATH=\"${CMAKE_BINARY_DIR}\"")
