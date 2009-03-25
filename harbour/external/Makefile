#
# $Id$
#

ROOT = ../

ifeq ($(HB_EXTERNALLIBS),)

# standalone contribs
DIRS=\
	sqlite3 \

else

ifneq ($(HB_EXTERNALLIBS),no)
DIRS = $(HB_EXTERNALLIBS)
endif

endif

include $(ROOT)config/dir.cf
