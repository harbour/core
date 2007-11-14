#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	dbcmd.c \
	dbcmd53.c \
	dbcmdx.c \
	dbdrop.c \
	dbexists.c \
	fieldhb.c \
	workarea.c \
	wacore.c \
	wafunc.c \
	dbf1.c \
	dbnubs.c \
	dbsql.c \
	delim1.c \
	sdf1.c \
	rddinfo.c \
	hbdbsort.c \

PRG_SOURCES=\
	dbdelim.prg \
	dbfuncs.prg \
	dbfuncsx.prg \
	dbjoin.prg \
	dbjoinx.prg \
	dblist.prg \
	dblistx.prg \
	dbsdf.prg \
	dbsort.prg \
	dbsortx.prg \
	dbstrux.prg \
	dbstruxx.prg \
	dbtotal.prg \
	dbtotalx.prg \
	dbupdat.prg \
	dbupdatx.prg \
	rddord.prg \
	rddsys.prg \

ifeq ($(HB_MT),MT)
   MT_LIBNAME=rddmt
endif

LIBNAME=rdd


# The list of all valid DB drivers is defined in config/global.cf.
DIRS=nulsys $(HB_DB_DRIVERS)

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
