#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_BUILD_DLL),no)
   ifneq ($(HB_ARCHITECTURE),dos)

      DYNNAME_POST := -$(HB_DYN_VER)

      ifeq ($(HB_ARCHITECTURE),win)
         ifeq ($(HB_COMPILER),bcc)
            DYNNAME_POST := $(DYNNAME_POST)-bcc
         else
            ifeq ($(HB_CPU),x86_64)
               DYNNAME_POST := $(DYNNAME_POST)-x64
            else
               ifeq ($(HB_CPU),ia64)
                  DYNNAME_POST := $(DYNNAME_POST)-ia64
               endif
            endif
         endif
      else
         ifeq ($(HB_ARCHITECTURE),wce)
            DYNNAME_POST := $(DYNNAME_POST)-wce
            ifeq ($(HB_CPU),arm)
               DYNNAME_POST := $(DYNNAME_POST)-arm
            else
               ifeq ($(HB_CPU),mips)
                  DYNNAME_POST := $(DYNNAME_POST)-mips
               else
                  ifeq ($(HB_CPU),sh)
                     DYNNAME_POST := $(DYNNAME_POST)-sh
                  endif
               endif
            endif
         endif
      endif

      DYNDIRLIST_BASE := source/common source/pp source/rtl source/macro source/lang source/codepage source/hbpcre source/hbzlib source/hbextern source/rdd source/rdd/dbfntx source/rdd/dbfnsx source/rdd/dbfcdx source/rdd/dbffpt source/rdd/hbsix source/rdd/hsx source/rdd/usrrdd source/rtl/gtcgi source/rtl/gtpca source/rtl/gtstd source/rtl/gtwvt source/rtl/gtgui
      ifneq ($(HB_ARCHITECTURE),wce)
         DYNDIRLIST_BASE += source/rtl/gtwin
      endif
      ifeq ($(HB_ARCHITECTURE)-$(HB_COMPILER),win-watcom)
         DYNDIRLIST_BASE += source/vm/maindllh
      endif

      export DYNDIRLIST_BASE
      export DYNNAME_POST

      DYNDIRLIST := $(DYNDIRLIST_BASE) source/vm
      DYNNAME := harbour$(DYNNAME_POST)

      DIRS := mt

      include $(TOP)$(ROOT)config/dyn.mk
      ifneq ($(DIRS),)
         include $(TOP)$(ROOT)config/dir.mk
      endif
   else
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   include $(TOP)$(ROOT)config/none.mk
endif
