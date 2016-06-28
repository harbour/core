# -fno-common enables building .dylib files
CFLAGS += -fno-common

include $(TOP)$(ROOT)config/common/clang.mk
