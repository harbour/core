#
# $Id$
#

ROOT := ../

include $(TOP)$(ROOT)config/global.mk

ifeq ($(HB_HOST_BUILD),yes)

   DIRS := \
      common \
      nortl \
      pp{common,nortl} \
      compiler{pp} \
      main{compiler} \

else

   ifneq ($(HB_BUILD_DLL),no)

      ifneq ($(HB_PLATFORM_UNIX),)
         DYNNAME_POST := .$(HB_DYN_VER)
      else
         DYNNAME_POST := -$(HB_DYN_VER)
      endif

      ifeq ($(HB_PLATFORM),win)
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
         ifeq ($(HB_PLATFORM),wce)
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
         else
            ifeq ($(HB_PLATFORM),os2)
               DYNNAME_POST := $(DYNNAME_POST)-os2
            endif
         endif
      endif

      DYNDIRLIST_BASE := \
         source/common \
         source/pp \
         source/rtl \
         source/macro \
         source/lang \
         source/codepage \
         source/hbpcre \
         source/hbzlib \
         source/hbextern \
         source/rdd \
         source/rdd/dbfntx \
         source/rdd/dbfnsx \
         source/rdd/dbfcdx \
         source/rdd/dbffpt \
         source/rdd/hbsix \
         source/rdd/hsx \
         source/rdd/usrrdd \
         source/rtl/gtcgi \
         source/rtl/gtpca \
         source/rtl/gtstd

      ifeq ($(HB_PLATFORM),dos)
         # Don't consume environment space
         DYNDIRLIST_BASE = x
      endif
      ifeq ($(HB_PLATFORM),os2)
         DYNDIRLIST_BASE += source/rtl/gtos2
      endif
      ifeq ($(HB_PLATFORM),wce)
         DYNDIRLIST_BASE += source/rtl/gtwvt source/rtl/gtgui
      endif
      ifeq ($(HB_PLATFORM),win)
         DYNDIRLIST_BASE += source/rtl/gtwvt source/rtl/gtgui source/rtl/gtwin
      endif
      ifneq ($(HB_PLATFORM_UNIX),)
         DYNDIRLIST_BASE += source/rtl/gttrm
         DYNDIRLIST_BASE += source/rtl/gtcrs
         DYNDIRLIST_BASE += source/rtl/gtsln
      endif
      ifeq ($(HB_COMPILER),watcom)
         DYNDIRLIST_BASE += source/vm/maindllh
      endif
      ifeq ($(HB_COMPILER),xcc)
         DYNDIRLIST_BASE += source/vm/maindllh
      endif

      export DYNDIRLIST_BASE
      export DYNNAME_POST
   endif

   ifeq ($(HB_HOST_BUILD),lib)
      HB_COMP_DIR :=
      HB_COMP_REF :=
   else
      HB_COMP_DIR := main{compiler}
      HB_COMP_REF := {main}
   endif

   DIRS := \
      common \
      nortl \
      pp{common,nortl} \
      compiler{pp} \
      $(HB_COMP_DIR) \
      rtl$(HB_COMP_REF) \
      vm$(HB_COMP_REF) \
      macro \
      codepage \
      lang \
      rdd$(HB_COMP_REF) \
      hbextern$(HB_COMP_REF) \
      hbpcre \
      hbzlib \
      debug$(HB_COMP_REF)

      ifneq ($(HB_BUILD_DLL),no)
         DIRS += dynlib{codepage,common,hbextern,hbpcre,hbzlib,lang,macro,pp,rdd,rtl,vm}
      endif

endif

include $(ROOT)config/dir.mk
