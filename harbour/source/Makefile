#
# $Id$
#

ROOT = ../

ifeq ($(HB_COMPILER),bcc16)

# Don't build the Harbour compiler for 16-bit DOS,
# because it frequently runs out of memory, so it
# is necessary to use the DJGPP Harbour compiler.

DIRS=\
	common \
	pp \
	rtl \
	vm \
	macro \
	lang \
	rdd \
	debug \

else

DIRS=\
	common \
	pp \
	compiler \
	rtl \
	vm \
	macro \
	lang \
	rdd \
	debug \

endif

include $(ROOT)config/dir.cf
