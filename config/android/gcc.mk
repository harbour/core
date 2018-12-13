DFLAGS += -Wl,--no-undefined -Wl,-z,noexecstack

include $(TOP)$(ROOT)config/linux/gcc.mk
