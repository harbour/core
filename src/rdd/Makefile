ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),wce)
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS := -gc3
   endif
endif

C_SOURCES := \
   dbcmd.c \
   dbcmd53.c \
   dbcmdhb.c \
   dbdetach.c \
   dbdrop.c \
   dbexists.c \
   dbf1.c \
   dbnubs.c \
   dbrename.c \
   dbsql.c \
   delim1.c \
   fieldhb.c \
   hbdbsort.c \
   ordcount.c \
   ordwldsk.c \
   workarea.c \
   wacore.c \
   wafunc.c \
   rddinfo.c \
   rddshort.c \
   sdf1.c \

PRG_SOURCES := \
   dbdelim.prg \
   dbjoin.prg \
   dblist.prg \
   dbsdf.prg \
   dbsort.prg \
   dbstrux.prg \
   dbstruxu.prg \
   dbtotal.prg \
   dbupdat.prg \
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
