#
# $Id$
#

all : first

BIN_EXT := .vxe
DYN_EXT := .so
DYN_PREF := lib

ifeq ($(HB_CCPOSTFIX),)
   ifeq ($(HB_CPU),x86)
      export HB_CCPOSTFIX := pentium
   else
   ifeq ($(HB_CPU),arm)
      export HB_CCPOSTFIX := arm
   else
   ifeq ($(HB_CPU),mips)
      export HB_CCPOSTFIX := mips
   else
   ifeq ($(HB_CPU),ppc)
      export HB_CCPOSTFIX := ppc
   endif
   endif
   endif
   endif
endif

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && strip$(HB_CCPOSTFIX) $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif
