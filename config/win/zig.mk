RC := zig rc
RCFLAGS := /I. /I$(HB_HOST_INC) /C 1252
RC_OUT := /FO$(subst x,x, )
RES_EXT := .res

IMPLIBFLAGS = -Wl,--out-implib,$(IMP_FILE)

include $(TOP)$(ROOT)config/common/zig.mk
