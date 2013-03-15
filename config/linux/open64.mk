#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := openCC
else
   HB_CMP := opencc
endif

# it supports most of GCC switches (at least as dummy parameters) so we do
# not have to create separate configurations for Open64 compiler

include $(TOP)$(ROOT)config/$(HB_PLATFORM)/gcc.mk
