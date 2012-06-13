#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

ifneq ($(HB_COMPILER),mingw64)
   ifneq ($(findstring unicows,$(3RDLIBS)),)
      # Required to be able to link harbour-*.dll against unicows lib
      # without it 'Cannot export <*>: symbol not found' errors occur.
      HB_DYN_COPT := -DHB_DYNLIB
   endif
endif

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

# Equivalent to MSVC -GS (default) option:
ifeq ($(filter $(HB_COMPILER_VER),29 34 40),)
   #CFLAGS += -fstack-protector
endif

# It is also supported by official mingw 4.4.x and mingw64 4.4.x,
# but not supported by mingw tdm 4.4.x, so I only enable it on or
# above 4.5.0.
ifneq ($(HB_COMPILER_VER),)
   ifeq ($(filter $(HB_COMPILER_VER),29 34 40 41 42 43 44),)
      LDFLAGS += -Wl,--nxcompat -Wl,--dynamicbase
      DFLAGS += -Wl,--nxcompat -Wl,--dynamicbase
   endif
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
   # CFLAGS += -Wextra
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # -O3 is not recommended for GCC 4.x by some packagers (see http://www.gentoo.org/doc/en/gcc-optimization.xml)
   CFLAGS += -O3
   # This option is not needed in x86_64 mode.
   ifneq ($(HB_COMPILER),mingw64)
      # It makes debugging hard or impossible on x86 systems.
      ifneq ($(HB_BUILD_DEBUG),yes)
         CFLAGS += -fomit-frame-pointer
      endif
   endif
   ifeq ($(HB_COMPILER),mingw)
      CFLAGS += -march=i586 -mtune=pentiumpro
   endif
endif

#ifeq ($(HB_COMPILER),mingw64)
#   CFLAGS += -m64
#   DFLAGS += -m64
#   LDFLAGS += -m64
#   RCFLAGS += -m64
#else
#   CFLAGS += -m32
#   DFLAGS += -m32
#   LDFLAGS += -m32
#   RCFLAGS += -m32
#endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

RC := $(HB_CCPATH)$(HB_CCPREFIX)windres
RC_OUT := -o$(subst x,x, )
RCFLAGS += -I. -I$(HB_HOST_INC) -O coff

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && ${HB_CCPATH}${HB_CCPREFIX}strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(3RDLIB_DIR),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),-l$(lib))

# Add the standard C entry
ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_MAIN),)
      LDLIBS += -lhbmainstd
   endif
endif

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPATH)$(HB_CCPREFIX)ar

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define library_object
   @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file))$(ECHOQUOTE) >> __lib__.tmp

endef
define create_library
   $(if $(wildcard __lib__.tmp),@$(RM) __lib__.tmp,)
   $(foreach file,$^,$(library_object))
   ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ @__lib__.tmp $(ARSTRIP) ) || ( $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@) && $(FALSE) )
endef

AR_RULE = $(create_library)

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DLIBS) -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def $(DYSTRIP)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
