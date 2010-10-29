#
# $Id$
#

all : first

BIN_EXT := .exe
DYN_EXT := .dll

HB_GT_LIBS += gttrm

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif
