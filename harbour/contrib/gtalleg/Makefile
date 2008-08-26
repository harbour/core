#
# $Id$
#

ROOT = ../../

LIBNAME=gtalleg

ifneq ($(HB_ARCHITECTURE),pocc)
ifneq ($(HB_ARCHITECTURE),xcc)

ifeq ($(HB_INC_ALLEGRO),)
ifeq ($(HB_XBUILD),)
HB_INC_ALLEGRO = /usr/include
endif
endif

HB_INC_ALLEGRO_OK += $(foreach d, $(HB_INC_ALLEGRO), $(if $(wildcard $(d)/allegro.h),$(d),))

ifneq ($(strip $(HB_INC_ALLEGRO_OK)),)

C_USR += $(foreach d, $(HB_INC_ALLEGRO_OK), -I$(d))

C_SOURCES=\
	gtalleg.c \
	ssf.c \

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
