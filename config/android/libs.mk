#
# $Id$
#

include $(TOP)$(ROOT)config/linux/libs.mk

SYSLIBS := $(filter-out rt pthread, $(SYSLIBS))
