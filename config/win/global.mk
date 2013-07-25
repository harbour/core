all : first

RES_EXT := .res
BIN_EXT := .exe
DYN_EXT := .dll

HB_CFLAGS += -DUNICODE

HB_GT_LIBS += gtwvt gtgui gtwin

# kernel32: needed by some compilers (pocc/watcom)
# user32: *Clipboard*(), GetKeyState(), GetKeyboardState(), SetKeyboardState(), gtwvt stuff
# ws2_32/wsock32: hbsocket
# ws2_32: WSAIoctl()
# advapi32: GetUserName()
# gdi32: gtwvt

# unicows lib must come after user libs and before Windows system libs
ifneq ($(wildcard $(TOP)$(ROOT)lib/3rd/$(HB_PLATFORM)/$(HB_COMPILER)),)
   3RDLIB_DIR := $(TOP)$(ROOT)lib/3rd/$(HB_PLATFORM)/$(HB_COMPILER)
   3RDLIBS := unicows
endif

SYSLIBS += kernel32 user32 ws2_32 advapi32 gdi32
