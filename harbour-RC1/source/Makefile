#
# $Id$
#

ROOT = ../

ifeq ($(HB_HOST_BUILD),yes)

DIRS=\
	common \
	pp \
	compiler \
	main \

else

ifeq ($(HB_HOST_BUILD),lib)
HB_COMP_DIR=
else
HB_COMP_DIR=main
endif

DIRS=\
	common \
	pp \
	compiler \
	$(HB_COMP_DIR) \
	rtl \
	vm \
	macro \
	codepage \
	lang \
	rdd \
	hbpcre \
	hbzlib \
	debug \

endif

include $(ROOT)config/dir.cf
