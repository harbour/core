#
# $Id$
#

SYSLIBPATHS :=

ifeq ($(HB_CPU),x86)
   SYSLIBPATHS_BIN := $(WIND_BASE)/target/lib/usr/lib/simpentium/SIMPENTIUM/common
   SYSLIBPATHS_DYN := $(WIND_BASE)/target/lib/usr/lib/simpentium/SIMPENTIUM/common/PIC
else
ifeq ($(HB_CPU),arm)
   SYSLIBPATHS_BIN := $(WIND_BASE)/target/lib/usr/lib/arm/ARMARCH7/common
   SYSLIBPATHS_DYN := $(WIND_BASE)/target/lib/usr/lib/arm/ARMARCH7/common/PIC
endif
endif

# For <dlfcn.h> support. I couldn't make it work though.
# The lib is missing from PIC dir, so it can only work
# for static targets, but even then, various tls symbols
# are missing.
#SYSLIBS_BIN += dl
