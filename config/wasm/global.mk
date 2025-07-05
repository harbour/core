all : first

ifeq ($(HB_COMPILER),wasm)
   BIN_EXT := .js
   DYN_EXT := .so
endif
DYN_PREF := lib

HB_GT_LIBS += gtcgi
