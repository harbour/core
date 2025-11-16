RC := zig rc
ifeq ($(HB_SHELL),sh)
   RCFLAGS := -I. -I$(HB_HOST_INC) -C 1252
   RC_OUT := -FO$(subst x,x, )
else
   RCFLAGS := /I. /I$(HB_HOST_INC) /C 1252
   RC_OUT := /FO$(subst x,x, )
endif
RES_EXT := .res

IMPLIBFLAGS = -Wl,--out-implib,$(IMP_FILE)

include $(TOP)$(ROOT)config/common/zig.mk
