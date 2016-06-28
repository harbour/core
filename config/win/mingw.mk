ifeq ($(HB_COMPILER_VER),)
   $(info ! Warning: HB_COMPILER_VER variable empty. Either stop manually setting HB_COMPILER to let auto-detection detect it, or set HB_COMPILER_VER manually according to your C compiler version (f.e. 0406 for 4.6.x).)
endif

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

3RDLIBS_DYN := $(3RDLIBS)

ifneq ($(HB_COMPILER),mingw64)

   # Since unicows support in harbour-*.dll effectively
   # doubles build time for core, allow it to be disabled.
   ifeq ($(__HB_HARBOUR_DLL_UNICOWS),no)
      3RDLIBS_DYN := $(filter-out $(3RDLIBS_DYN),unicows)
   endif

   ifneq ($(findstring unicows,$(3RDLIBS_DYN)),)
      # Required to be able to link harbour-*.dll against unicows lib
      # without 'Cannot export <*>: symbol not found' errors.
      HB_DYN_COPT := -DHB_DYNLIB
   endif
endif

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN :=
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC) -c

# Similar to MSVC -GS (default) option:
ifeq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404 0405 0406 0407 0408),)
   #CFLAGS += -fstack-protector-strong
   #SYSLIBS += ssp
else
ifeq ($(filter $(HB_COMPILER_VER),0209 0304 0400),)
   # too weak
   #CFLAGS += -fstack-protector
   # too slow
   #CFLAGS += -fstack-protector-all
   #SYSLIBS += ssp
endif
endif

ifneq ($(HB_COMPILER_VER),)
   ifeq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404 0405 0406 0407),)
      LDFLAGS += -static-libgcc
      DFLAGS += -static-libgcc
#     ifeq ($(HB_BUILD_MODE),cpp)
#        LDFLAGS += -static-libstdc++
#     endif
   endif
endif

# It is also supported by official mingw 4.4.x and mingw64 4.4.x,
# but not supported by mingw tdm 4.4.x, so I only enable it on or
# above 4.5.0.
ifneq ($(HB_COMPILER_VER),)
   ifeq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404),)
      LDFLAGS += -Wl,--nxcompat -Wl,--dynamicbase
      DFLAGS += -Wl,--nxcompat -Wl,--dynamicbase
      ifeq ($(HB_COMPILER),mingw64)
         LDFLAGS += -Wl,--pic-executable,-e,mainCRTStartup
      else
         LDFLAGS += -Wl,--pic-executable,-e,_mainCRTStartup
      endif
      ifeq ($(filter $(HB_COMPILER_VER),0405 0406 0407 0408 0409),)
         ifeq ($(HB_COMPILER),mingw64)
            LDFLAGS += -Wl,--high-entropy-va -Wl,--image-base,0x140000000
            DFLAGS += -Wl,--high-entropy-va -Wl,--image-base,0x180000000
         endif
         # '--no-insert-timestamp' has a bug failing to properly
         # reset timestamp in many (apparently random) cases as
         # of binutils 2.25, so disable for now.
         #LDFLAGS += -Wl,--no-insert-timestamp
         # This has potential risks for .dlls:
         #    https://sourceware.org/bugzilla/show_bug.cgi?id=16887
         #DFLAGS += -Wl,--no-insert-timestamp
      endif
      ARFLAGS += -D
   endif
endif

# Enable this, once better than a no-op
#ifeq ($(filter $(HB_COMPILER_VER),0209 0304),)
#   CFLAGS += -D_FORTIFY_SOURCE=2
#endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
   # CFLAGS += -Wextra -Wformat-security
#  ifeq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401),)
#     # https://gcc.gnu.org/gcc-4.2/changes.html
#     CFLAGS += -Wstrict-overflow=4
#  endif
   ifeq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404 0405 0406 0407 0408 0409 0501 0502 0503 0504),)
      CFLAGS += -Wlogical-op -Wduplicated-cond -Wshift-negative-value -Wnull-dereference -Wunused-variable
   endif
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # -O3 is not recommended for GCC 4.x by some packagers (see https://wiki.gentoo.org/wiki/GCC_optimization)
   CFLAGS += -O3 -fno-ident
   # This option is not needed in x86_64 mode.
   ifneq ($(HB_COMPILER),mingw64)
      # It makes debugging hard or impossible on x86 systems.
      ifneq ($(HB_BUILD_DEBUG),yes)
         # It's the default in 4.6 and up
         ifneq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404 0405),)
            CFLAGS += -fomit-frame-pointer
         endif
      endif
   endif
endif

ifeq ($(HB_COMPILER),mingw64)
   CFLAGS += -m64
   DFLAGS += -m64
   LDFLAGS += -m64
   RCFLAGS += --target=pe-x86-64
else
   CFLAGS += -m32
   DFLAGS += -m32
   LDFLAGS += -m32
   RCFLAGS += --target=pe-i386
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

RC := $(HB_CCPATH)$(HB_CCPREFIX)windres
RC_OUT := -o$(subst x,x, )
RCFLAGS += -I. -I$(HB_HOST_INC) -O coff -c65001

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
   ( $(AR) rcs $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) $(LIB_DIR)/$@ @__lib__.tmp $(ARSTRIP) ) || ( $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@) && $(FALSE) )
endef

AR_RULE = $(create_library)

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS_DYN) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DEF_FILE) $(DLIBS) -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def -Wl,--major-image-version,$(HB_VER_MAJOR) -Wl,--minor-image-version,$(HB_VER_MINOR) $(DYSTRIP)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
