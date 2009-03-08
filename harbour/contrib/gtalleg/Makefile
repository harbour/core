#
# $Id$
#

ROOT = ../../

LIBNAME=gtalleg

ifneq ($(HB_COMPILER),pocc)
ifneq ($(HB_COMPILER),xcc)
ifneq ($(HB_COMPILER),dmc)
ifneq ($(HB_COMPILER),owatcom)

ifeq ($(HB_INC_ALLEGRO),)
ifeq ($(HB_XBUILD),)
HB_INC_ALLEGRO = /usr/include /opt/local/include
endif
endif

HB_INC_ALLEGRO_OK += $(foreach d, $(HB_INC_ALLEGRO), $(if $(wildcard $(d)/allegro.h),$(d),))

ifneq ($(strip $(HB_INC_ALLEGRO_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_ALLEGRO_OK), -I$(d))

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
else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
