#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   HB_PRGFLAGS := -gc3
endif

C_SOURCES := \
   dbcmd.c \
   dbcmd53.c \
   dbcmdhb.c \
   dbcmdx.c \
   dbdetach.c \
   dbdetacx.c \
   dbdrop.c \
   dbexists.c \
   dbrename.c \
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

PRG_SOURCES := \
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
   dbstruxu.prg \
   dbstruxx.prg \
   dbtotal.prg \
   dbtotalx.prg \
   dbupdat.prg \
   dbupdatx.prg \
   rddord.prg \
   rddordu.prg \
   rddsys.prg \

LIBNAME := hbrdd

DIRS = \
   nulsys \
   dbfntx \
   dbfnsx \
   dbfcdx \
   dbffpt \
   hbsix \
   hsx \
   usrrdd \
   usrrdd/rdds \

include $(TOP)$(ROOT)config/lib.mk
include $(TOP)$(ROOT)config/dir.mk
