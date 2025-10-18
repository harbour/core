RC := zig rc
RCFLAGS := /I. /I$(HB_HOST_INC) /C 1252
RC_OUT := /FO$(subst x,x, )
RES_EXT := .res

ifeq ($(HB_PLATFORM),win)
   IMPLIBFLAGS = -Wl,--out-implib,$(IMP_FILE)
endif

include $(TOP)$(ROOT)config/common/zig.mk
