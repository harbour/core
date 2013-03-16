
# GNU Make file for Open Watcom C/C++ compiler
# (shell specific rules)

# ---------------------------------------------------------------
# See option docs here:
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/cpopts.html
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/wlink.html
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/wlib.html
# ---------------------------------------------------------------

# NOTE: Hack to force no extension for Linux binaries created on non-Linux hosts.
#       Otherwise they become '.elf'. [vszakats]
ifeq ($(HB_PLATFORM),linux)
   ifneq ($(HB_HOST_PLAT),linux)
      BIN_EXT := .
   endif
endif

AR := wlib
ARFLAGS += -q -p=72 -c -n

comma := ,
ifneq ($(RES_EXT),)
   LDFILES_COMMA = $(subst $(subst x,x, ),$(comma) ,$(filter-out %$(RES_EXT),$(^F)))
   LDRES_LIST = $(foreach file,$(filter %$(RES_EXT),$(^F)),OPT res=$(file))
else
   LDFILES_COMMA = $(subst $(subst x,x, ),$(comma) ,$(^F))
   LDRES_LIST :=
endif
LDLIBS_COMMA := $(subst $(subst x,x, ),$(comma) ,$(strip $(LDLIBS)))
LD_RULE = $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) NAME $(BIN_DIR)/$@ FILE $(LDFILES_COMMA) $(LDRES_LIST) $(if $(LDLIBS_COMMA), LIB $(LDLIBS_COMMA),)
AR_RULE = $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) $(LIB_DIR)/$@ $(foreach file,$(^F),-+$(file))

ifeq ($(HB_SHELL),dos)

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every command will be separated by LF
   define link_file
      @$(ECHO) $(ECHOQUOTE)FILE $(file)$(ECHOQUOTE) >> __link__.tmp

   endef

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   define link_lib
      @$(ECHO) $(ECHOQUOTE)LIB $(lib)$(ECHOQUOTE) >> __link__.tmp

   endef

   define link_exe_file
      @$(ECHO) $(ECHOQUOTE)$(LDFLAGS) $(HB_LDFLAGS) NAME $(BIN_DIR)/$@$(ECHOQUOTE) > __link__.tmp
      $(foreach file,$(^F),$(link_file))
      $(foreach lib,$(LDLIBS),$(link_lib))
      -$(LD) @__link__.tmp
   endef

   LD_RULE = $(link_exe_file) $(HB_USER_LDFLAGS)

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   define lib_object
      @$(ECHO) $(ECHOQUOTE)-+$(file)$(ECHOQUOTE) >> __lib__.tmp

   endef

   define create_library
      @$(ECHO) $(ECHOQUOTE)$(LIB_DIR)/$@$(ECHOQUOTE) > __lib__.tmp
      $(foreach file,$(^F),$(lib_object))
      $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) @__lib__.tmp
   endef

   AR_RULE = $(create_library)
endif

ifneq ($(findstring wcc386,$(CC)),)
   ifneq ($(HB_HOST_PLAT),linux)
      CC_DIRSEPFROM := /
      CC_DIRSEPTO   := $(subst /,\,\)
   endif
endif

include $(TOP)$(ROOT)config/rules.mk

ANYDOS := no
ifeq ($(HB_SHELL),dos)
   ANYDOS := yes
endif
ifeq ($(HB_PLATFORM),dos)
   ANYDOS := yes
endif

ifeq ($(ANYDOS),yes)

   # disable DOS/4GW Banner
   export DOS4G := quiet
   # disable DOS/32A Banner
   export DOS32A := /NOC

   # work arround to DOS command line size limit
   ifneq ($(findstring wcc386,$(CC)),)
      export WCC386 := $(strip $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS)))
   else
      export WPP386 := $(strip $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS)))
   endif
   CC_FLAGS :=

   export HARBOURCMD := $(HB_FLAGS)
   HB_FLAGS :=
endif
