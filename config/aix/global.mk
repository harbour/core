
all : first

BIN_EXT :=
DYN_EXT := .so
DYN_PREF := lib

HB_GT_LIBS += gttrm

ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif
