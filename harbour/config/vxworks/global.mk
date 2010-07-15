#
# $Id$
#

all : first

BIN_EXT := .vxe
DYN_EXT := .so
DYN_PREF := lib

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && strip$(HB_CCPOSTFIX) $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif
