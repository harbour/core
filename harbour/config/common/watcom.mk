#
# $Id$
#

# GNU Make file for Open Watcom C/C++ compiler
# (shell specific rules)

# ---------------------------------------------------------------
# See option docs here:
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/cpopts.html
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/wlink.html
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/wlib.html
# ---------------------------------------------------------------

ifeq ($(CC),wcc386)
   ifneq ($(HB_HOST_ARCH),linux)
      CPPFLAGS := $(subst /,\,$(CPPFLAGS))
      CC_RULE = $(CC) $(subst /,\,$(HB_INC_DEPEND)) $(CPPFLAGS) $(subst /,\,$(CFLAGS)) $(subst /,\,$(HB_CFLAGS)) $(subst /,\,$(HB_USER_CFLAGS)) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(CC_IN)$(subst /,\,$<)
   endif
endif

# NOTE: Hack to force no extension for Linux binaries created on non-Linux hosts.
#       Otherwise they become '.elf'. [vszakats]
ifeq ($(HB_ARCHITECTURE),linux)
   ifneq ($(HB_HOST_ARCH),linux)
      BIN_EXT := .
   endif
endif

comma := ,
LDFILES_COMMA = $(subst $(subst x,x, ),$(comma) ,$(^F))
LDLIBS_COMMA := $(subst $(subst x,x, ),$(comma) ,$(strip $(LDLIBS)))
LD_RULE = $(LD) $(LDFLAGS) $(HB_USER_LDFLAGS) NAME $(BIN_DIR)/$@$(BIN_EXT) FILE $(LDFILES_COMMA) $(if $(LDLIBS_COMMA), LIB $(LDLIBS_COMMA),)

ifeq ($(HB_SHELL),sh)
   create_library = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) $(LIB_DIR)/$@ $(foreach file,$(^F),-+$(file))
endif

ifeq ($(HB_SHELL),os2)
   # maximum size of command line in OS2 is limited to 1024 characters
   # the trick with divided 'wordlist' is workaround for it:
   #     -$(if $(wordlist   1,100,$(^F)), $(ECHO) $(wordlist   1,100,$(addprefix -+,$(^F))) >> __lib__.tmp,)
   #     -$(if $(wordlist 101,200,$(^F)), $(ECHO) $(wordlist 101,200,$(addprefix -+,$(^F))) >> __lib__.tmp,)
   #     -$(if $(wordlist 201,300,$(^F)), $(ECHO) $(wordlist 301,300,$(addprefix -+,$(^F))) >> __lib__.tmp,)
   # anyhow OS/2 port# of GNU make 3.81 seems to have bug and GPFs when total
   # commands length is too big so for %i in ( *$(OBJ_EXT) ) do ... below is
   # ugly workaround for both problems

   ifeq ($(HB_SHELL),nt)
      FILE := %%f
   else
      FILE := %f
   endif

   define create_library
      @$(ECHO) $(LIB_DIR)/$@ > __lib__.tmp
      for $(FILE) in ( *$(OBJ_EXT) ) do @$(ECHO) -+$(FILE) >> __lib__.tmp
      $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) @__lib__.tmp
   endef
endif

ifneq ($(filter $(HB_SHELL),nt dos),)

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every command will be separated by LF
   define link_file
      @$(ECHO) FILE $(file) >> __link__.tmp

   endef

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   define link_lib
      @$(ECHO) LIB $(lib) >> __link__.tmp

   endef

   define link_exe_file
      @$(ECHO) $(LDFLAGS) NAME $(BIN_DIR)/$@ > __link__.tmp
      $(foreach file,$(^F),$(link_file))
      $(foreach lib,$(LDLIBS),$(link_lib))
      -$(LD) @__link__.tmp
   endef

   LD_RULE = $(link_exe_file) $(HB_USER_LDFLAGS)

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   define lib_object
      @$(ECHO) -+$(file) >> __lib__.tmp

   endef

   define create_library
      @$(ECHO) $(LIB_DIR)/$@ > __lib__.tmp
      $(foreach file,$(^F),$(lib_object))
      $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) @__lib__.tmp
   endef
endif

AR := wlib
ARFLAGS := -q -p=64 -c -n
AR_RULE = $(create_library)

include $(TOP)$(ROOT)config/rules.mk

ifeq ($(HB_SHELL),dos)

   # disable DOS/4GW Banner
   export DOS4G := quiet

   # work arround to DOS command line size limit
   ifeq ($(CC),wcc386)
      export WCC386 := $(strip $(CPPFLAGS))
   else
      export WPP386 := $(strip $(CPPFLAGS))
   endif
   CPPFLAGS :=

   export HARBOURCMD := $(HB_FLAGS)
   HB_FLAGS :=
endif
