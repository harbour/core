#
# $Id$
#

ROOT := ../

ifeq ($(HB_HOST_BUILD),yes)

DIRS := \
   common \
   nortl \
   pp{common,nortl} \
   compiler{pp} \
   main{compiler} \

else

ifeq ($(HB_HOST_BUILD),lib)
   HB_COMP_DIR :=
   HB_COMP_REF :=
else
   HB_COMP_DIR := main{compiler}
   HB_COMP_REF := {main}
endif

DIRS := \
   common \
   nortl \
   pp{common,nortl} \
   compiler{pp} \
   $(HB_COMP_DIR) \
   rtl$(HB_COMP_REF) \
   vm$(HB_COMP_REF) \
   macro \
   codepage \
   lang \
   rdd$(HB_COMP_REF) \
   hbextern$(HB_COMP_REF) \
   hbpcre \
   hbzlib \
   debug$(HB_COMP_REF) \

endif

include $(ROOT)config/dir.mk
