#
# $Id$
#

all : first

BIN_EXT := .exe
DYN_EXT := .dll

HB_GT_LIBS += gtwvt gtgui

HB_CFLAGS += -D_WIN32_WCE=0x501 -DUNDER_CE
HB_BUILD_UNICODE := yes

SYSLIBS += coredll ws2
