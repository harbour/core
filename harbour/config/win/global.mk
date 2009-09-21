#
# $Id$
#

all : first

BIN_EXT := .exe
DYN_EXT := .dll

HB_GT_LIBS += gtwvt gtgui gtwin

# enable UNICODE by default when building for any non-x86 targets (x86_64 or ia64)
ifneq ($(HB_CPU),x86)
   HB_CFLAGS += -DUNICODE
endif

# kernel32: needed by some compilers (pocc/watcom)
# user32: *Clipboard*(), MessageBox(), CharToOemBuff(), OemToCharBuff(), GetKeyState(), GetKeyboardState(), SetKeyboardState()
# ws2_32: hbsocket
# advapi32: GetUserName()
# gdi32: gtwvt
SYSLIBS := kernel32 user32 ws2_32 advapi32 gdi32
