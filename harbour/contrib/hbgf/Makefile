#
# $Id$
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)
DIRS = hbgfw32
endif

ifeq ($(HB_ARCHITECTURE),linux)
DIRS = hbgfgtk
endif

ifeq ($(HB_ARCHITECTURE),os2)
DIRS = hbgfos2
endif

ifneq ($(DIRS),)
include $(ROOT)config/dir.cf
else
include $(TOP)$(ROOT)config/none.cf
endif
