# GNU Make file for Borland/CodeGear/Embarcadero C/C++ 32-bit (4.x-)

ifeq ($(HB_COMPILER),bcc64)
   OBJ_EXT := .o
   LIB_PREF :=
   LIB_EXT := .a
else
   OBJ_EXT := .obj
   LIB_PREF :=
   LIB_EXT := .lib
endif

HB_DYN_COPT := -DHB_DYNLIB

ifeq ($(HB_COMPILER),bcc64)
   CC := bcc64.exe
else
   CC := bcc32.exe
endif
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

CFLAGS += -q -tWM
ifneq ($(HB_COMPILER),bcc64)
   CFLAGS += -CP437
endif

ifeq ($(HB_BUILD_WARN),no)
   ifeq ($(HB_COMPILER),bcc64)
      # same as clang
      CFLAGS += -w -Wmissing-braces -Wreturn-type -Wformat
      ifneq ($(HB_BUILD_MODE),cpp)
         CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
      endif
   else
      CFLAGS += -w-aus -w-ccc -w-csu -w-ovf -w-par -w-rch -w-spa
   endif
else
   ifeq ($(HB_COMPILER),bcc64)
      # same as clang
      CFLAGS += -W -Wall
   else
      CFLAGS += -w -Q -w-sig
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   ifeq ($(HB_COMPILER),bcc64)
      # same as clang
      CFLAGS += -O3
   else
      # for some reason -6 generates the exact same code as -4 with both 5.5 and 5.8.
      # -5 seems to be significantly slower than both. [vszakats]
      CFLAGS += -d -O2 -OS -Ov -Oc -Oi -6
   endif
endif

ifeq ($(HB_BUILD_MODE),cpp)
   CFLAGS += -P
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   ifeq ($(HB_COMPILER),bcc64)
      CFLAGS += -g
   else
      CFLAGS += -v -y
   endif
endif

ifneq ($(HB_HOST_PLAT_UNIX),)
   BACKSLASH := $(subst /,\,\\)
else
   BACKSLASH := $(subst /,\,\)
endif

ifeq ($(HB_SHELL),sh)
   ECHOBACKSLASH := $(BACKSLASH)$(BACKSLASH)
else
   ECHOBACKSLASH := $(BACKSLASH)
endif

# Hack to autoconfig bcc, and not require properly set .cfg files in its bin dir.
# It only works if we know compiler location.
ifneq ($(HB_COMP_PATH_PUB),)
   HB_CFLAGS += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include")
   RCFLAGS   += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include")
   ifneq ($(wildcard $(HB_COMP_PATH_PUB)../Include/windows/crtl),)
      HB_CFLAGS += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include/windows/crtl")
      RCFLAGS   += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include/windows/crtl")
   endif
   ifneq ($(wildcard $(HB_COMP_PATH_PUB)../Include/windows/sdk),)
      HB_CFLAGS += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include/windows/sdk")
      RCFLAGS   += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include/windows/sdk")
   endif
   LDFLAGS   += $(subst /,$(BACKSLASH),-L"$(HB_COMP_PATH_PUB)../Lib" -L"$(HB_COMP_PATH_PUB)../Lib/PSDK")
   DFLAGS    += $(subst /,$(BACKSLASH),-L"$(HB_COMP_PATH_PUB)../Lib" -L"$(HB_COMP_PATH_PUB)../Lib/PSDK")
endif

RC := brcc32.exe
RC_OUT := -fo
RCFLAGS += -I. -I$(HB_HOST_INC)

ifeq ($(HB_COMPILER),bcc64)
   LD := ilink64.exe
else
   LD := ilink32.exe
endif
LIBPATHS := $(foreach dir,$(LIB_DIR) $(3RDLIB_DIR),$(subst /,$(BACKSLASH),-L"$(dir)"))
LDFLAGS += $(LIBPATHS) -Gn -Tpe
ifeq ($(HB_COMPILER),bcc64)
   LD_RULE = $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) c0x64$(OBJ_EXT) $(filter-out %$(RES_EXT),$(^F)), "$(subst /,$(BACKSLASH),$(BIN_DIR)/$@)", nul, $(LDLIBS) cw64mt import64,, $(filter %$(RES_EXT),$(^F)) $(LDSTRIP)
else
   LD_RULE = $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) c0x32$(OBJ_EXT) $(filter-out %$(RES_EXT),$(^F)), "$(subst /,$(BACKSLASH),$(BIN_DIR)/$@)", nul, $(LDLIBS) cw32mt import32,, $(filter %$(RES_EXT),$(^F)) $(LDSTRIP)
endif

LDLIBS := $(strip $(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS))

ifeq ($(HB_COMPILER),bcc64)
   AR := tlib64.exe
else
   AR := tlib.exe
endif
ARFLAGS += /P128
AR_RULE = $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) "$(subst /,$(BACKSLASH),$(LIB_DIR)/$@)" $(foreach file,$(?F),-+$(file))

ifneq ($(HB_SHELL),sh)
   ifeq ($(HB_SHELL_XP),)

      ifeq ($(HB_SHELL),nt)
         LINECONT := ^&
      else
         LINECONT := &
      endif

      # NOTE: Command-line limit length defeating methods found below
      #       are only needed to support pre-Windows XP systems, where
      #       limit is 2047 chars. [vszakats]

      # NOTE: The empty line directly before 'endef' HAS TO exist!
      define library_object
         @$(ECHO) $(ECHOQUOTE)-+$(subst /,$(ECHOBACKSLASH),$(file)) $(LINECONT)$(ECHOQUOTE) >> __lib__.tmp

      endef

      define create_library
         $(if $(wildcard __lib__.tmp),@$(RM) __lib__.tmp,)
         $(foreach file,$(?F),$(library_object))
         @$(ECHO) $(ECHOQUOTE)-+$(ECHOQUOTE)>> __lib__.tmp
         $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) "$(subst /,$(BACKSLASH),$(LIB_DIR)/$@)" @__lib__.tmp
      endef

      AR_RULE = $(create_library)

   endif
endif

ifeq ($(HB_COMPILER),bcc64)
   DY := ilink64.exe
else
   DY := ilink32.exe
endif
DFLAGS += -q -Gn -C -aa -Tpd -Gi -x $(LIBPATHS)
DY_OUT :=
# NOTE: .lib extension not added to keep line short enough to work on Win9x/ME
ifeq ($(HB_COMPILER),bcc64)
   DLIBS := $(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS) cw64mt import64
   DLL_STARTUP_MODULE = c0d64$(OBJ_EXT)
else
   DLIBS := $(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS) cw32mt import32
   DLL_STARTUP_MODULE = c0d32$(OBJ_EXT)
endif

# NOTE: The empty line directly before 'endef' HAS TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)$(subst /,$(ECHOBACKSLASH),$(file)) +$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   @$(ECHO) $(ECHOQUOTE), $(subst /,$(ECHOBACKSLASH),$(DYN_DIR)/$@),, $(subst /,$(ECHOBACKSLASH),$(DLIBS))$(ECHOQUOTE) >> __dyn__.tmp
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DLL_STARTUP_MODULE) @__dyn__.tmp
   @$(CP) $(subst /,$(DIRSEP),$(DYN_DIR)/$(basename $@)$(LIB_EXT)) $(subst /,$(DIRSEP),$(IMP_FILE))
   @$(RM) $(subst /,$(DIRSEP),$(DYN_DIR)/$(basename $@)$(LIB_EXT))
endef

DY_RULE = $(create_dynlib)


# Don't let Clang-based (7.60+ (?)) bcc64 parse UTF-8 in our sources.
# https://quality.embarcadero.com/browse/RSP-26502 got closed but the issue remains:
# if Clang-based bcc64 detects UTF-8, it will convert it to ANSI in the binary.
# So, for known UTF-8 string literals in our sources, we modify CC_RULE:
# first, a PowerShell script converts UTF-8 to \123 octal characters, and
# then the result is fed into bcc64 instead of the original file.
# PowerShell could be changed to C if anyone cares.
# As the source location changes, -I$(GRANDP) must be added as well.
ifneq ($(filter bcc64:hbcpage bcc64:hblang,$(HB_COMPILER):$(LIBNAME)),)
   define cc_bcc64_hack
      powershell " \
         function byteToCLiteral { \
            param([string]$$m) \
	    $$n = [int]$$m; \
            $$(If($$n -lt 128) { [string][char]$$n } \
               Else { '\{0}' -f [Convert]::ToString($$n, 8) } \
            ) \
         } \
         function fileFixHighBit { \
            param([string]$$filename) \
            [regex]::Replace((Get-Content -Raw -Encoding byte $$filename), '(\b\d+ *)', $$function:byteToCLiteral) \
         } \
         $$f = '$(subst /,$(DIRSEP),$<)'; \
         $$fn = '$(<F)'; \
         $$p = '#include .(l_\w*\.h).*$$'; \
         $$q = (Select-String -Path $$f -Pattern $$p); \
         if ($$q) { \
            $$inc=($$q.Line -replace $$p, '$(subst /,$(DIRSEP),$(<D)/)$$1'); \
            Set-Content -Path $$fn ((Get-Content $$f) -replace $$p, (fileFixHighBit($$inc))) \
         } else { \
            Set-Content -Path $$fn (fileFixHighBit($$f)) \
         } \
      "
      $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) -I$(GRANDP) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(HB_CFLAGS_STA) $(CC_IN) $(<F))
   endef
   CC_RULE = $(cc_bcc64_hack)
   ifneq ($(HB_BUILD_DYN),no)
      ifneq ($(HB_DYN_COPT),)
         ifneq ($(LIBNAME),)
            ifneq ($(filter $(LIBNAME),$(HB_DYN_LIBS)),)
               define cc_comp_all
	          $(cc_bcc64_hack)
                  $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) -I$(GRANDP) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_DYN_SUFFIX)$(OBJ_EXT)) $(HB_DYN_COPT) $(HB_CFLAGS_DYN) $(CC_IN) $(<F))
               endef
               CC_RULE = $(cc_comp_all)
            endif
         endif
      endif
   endif
endif

include $(TOP)$(ROOT)config/rules.mk
