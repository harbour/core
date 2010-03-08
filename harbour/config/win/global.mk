#
# $Id$
#

all : first

BIN_EXT := .exe
DYN_EXT := .dll

HB_GT_LIBS += gtwvt gtgui gtwin

# kernel32: needed by some compilers (pocc/watcom)
# user32: *Clipboard*(), GetKeyState(), GetKeyboardState(), SetKeyboardState(), gtwvt stuff
# wsock32: hbsocket
# advapi32: GetUserName()
# gdi32: gtwvt
SYSLIBS += kernel32 user32 wsock32 advapi32 gdi32
