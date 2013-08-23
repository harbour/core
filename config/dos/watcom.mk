
OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib
DYN_EXT := .dll

ifeq ($(HB_BUILD_MODE),cpp)
   CC := wpp386
else
   CC := wcc386
endif
CC_IN :=
CC_OUT := -fo=

CFLAGS += -zq -bt=dos
LDFLAGS += OP quiet

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -w3
else
   CFLAGS += -w1 -wcd201 -wcd367 -wcd368
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -wcd124 -wcd136
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # architecture flags
   CFLAGS += -5r -fp5

   # optimization flags
   # don't enable -ol optimization in OpenWatcom 1.1 - gives buggy code
   # -oxaht
   CFLAGS += -onaehtr -s -ei -zp4 -zt0
   #CFLAGS += -obl+m
   ifeq ($(CC),wpp386)
      CFLAGS += -oi+
   else
      CFLAGS += -oi
   endif
else
   CFLAGS += -3r
endif

CFLAGS += -i. -i$(HB_HOST_INC)

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -d2
endif

LD := wlink
ifeq ($(HB_BUILD_DEBUG),yes)
   LDFLAGS += DEBUG ALL
endif

# different SYS values: dos4g (default), pmodew (commercial), causeway,
# dos32a (DOS/32A LE executable), dos32x (DOS/32A LX executable)
LDFLAGS += SYS dos32a

LDLIBS := $(HB_USER_LIBS)
LDLIBS += $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))

ifneq ($(HB_LINKING_RTL),)
   ifneq ($(HB_HAS_WATT),)
      LDLIBS += $(HB_LIB_WATT)/wattcpwf
   endif
   LDLIBS += $(LIB_DIR)/hbpmcom
endif

# workaround for not included automatically CLIB in pure C mode builds
ifeq ($(CC),wcc386)
   LDLIBS += clib3r
endif

ifeq ($(HB_BUILD_DYN),dostest)

   DY := $(LD)
   DFLAGS += OP quiet SYS cwdllr
   DY_OUT :=
   DLIBS := $(foreach lib,$(HB_USER_LIBS),$(lib))
   DLIBS += $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))
   DLIBS += $(foreach lib,$(SYSLIBS),$(lib))
   DLIBS := $(strip $(DLIBS))

   ifneq ($(DLIBS),)
      comma := ,
      DLIBS_COMMA := LIB $(subst $(subst x,x, ),$(comma) ,$(DLIBS))
   else
      DLIBS_COMMA :=
   endif

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   define dynlib_object
      @$(ECHO) $(ECHOQUOTE)FILE '$(file)'$(ECHOQUOTE) >> __dyn__.tmp

   endef
   define create_dynlib
      $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
      $(foreach file,$^,$(dynlib_object))
      $(DY) $(DFLAGS) $(HB_USER_DFLAGS) NAME '$(subst /,$(DIRSEP),$(DYN_DIR)/$@)' OP implib='$(IMP_FILE)' @__dyn__.tmp $(DLIBS_COMMA)
   endef

   DY_RULE = $(create_dynlib)

endif # HB_BUILD_DYN

include $(TOP)$(ROOT)config/common/watcom.mk
