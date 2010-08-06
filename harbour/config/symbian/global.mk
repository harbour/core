#
# $Id$
#

all : first

BIN_EXT :=
# ?
DYN_EXT := .so
DYN_PREF := lib

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && $(HB_CCPREFIX)strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif
