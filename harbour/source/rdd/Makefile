#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	dbcmd.c \
	workarea.c \
	dbf1.c \
	dbnubs.c \
	dbsql.c \
	delim1.c \
	sdf1.c \
	hbdbsort.c \

PRG_SOURCES=\
	dbdelim.prg \
	dbfuncs.prg \
	dbjoin.prg \
	dbtotal.prg \
	dblist.prg \
	dbsdf.prg \
	dbsort.prg \
	dbstrux.prg \
	dbupdat.prg \
	rddord.prg \
	rddsys.prg \
 
ifeq ($(HB_MT),MT)
   MT_LIBNAME=rddmt
endif

LIBNAME=rdd


# The list of all valid DB drivers is defined in config/global.cf.
DIRS=$(HB_DB_DRIVERS)

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
