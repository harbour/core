#
# $Id$
#

ROOT = ../

ifeq ($(HB_HOST_BUILD),yes)

DIRS=\
	common \
	pp{common} \
	compiler{pp} \
	main{compiler} \

else

ifeq ($(HB_HOST_BUILD),lib)
HB_COMP_DIR=
else
HB_COMP_DIR=main{compiler}
endif

DIRS=\
	common \
	pp{common} \
	compiler{pp} \
	$(HB_COMP_DIR) \
	rtl{main} \
	vm{main} \
	macro \
	codepage \
	lang \
	rdd{main} \
	hbextern{main} \
	hbpcre \
	hbzlib \
	debug{main} \

endif

include $(ROOT)config/dir.cf
