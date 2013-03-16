
all : first

BIN_EXT := .vxe
DYN_EXT := .so
DYN_PREF := lib

ifeq ($(HB_CPU),x86)
   ifeq ($(HB_CCSUFFIX),)
      export HB_CCSUFFIX := pentium
   endif
   _HB_VXCPU := _VX_SIMPENTIUM
else
ifeq ($(HB_CPU),arm)
   ifeq ($(HB_CCSUFFIX),)
      export HB_CCSUFFIX := arm
   endif
   _HB_VXCPU := _VX_ARMARCH7
else
ifeq ($(HB_CPU),mips)
   ifeq ($(HB_CCSUFFIX),)
      export HB_CCSUFFIX := mips
   endif
   _HB_VXCPU :=
else
ifeq ($(HB_CPU),ppc)
   ifeq ($(HB_CCSUFFIX),)
      export HB_CCSUFFIX := ppc
   endif
   _HB_VXCPU :=
endif
endif
endif
endif

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && strip$(HB_CCSUFFIX) $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif
