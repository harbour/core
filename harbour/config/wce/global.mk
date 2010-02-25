#
# $Id$
#

all : first

BIN_EXT := .exe
DYN_EXT := .dll

HB_GT_LIBS += gtwvt gtgui

HB_CFLAGS += -DUNICODE
HB_CFLAGS += -DUNDER_CE

SYSLIBS += coredll ws2
