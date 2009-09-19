#
# $Id$
#

ROOT := ../

include $(TOP)$(ROOT)config/global.mk

ifeq ($(HB_BUILD_PART),compiler)

   DIRS := \
      common \
      nortl \
      pp{common,nortl} \
      compiler{pp} \
      main{compiler} \

else

   ifneq ($(HB_BUILD_DLL),no)

      DYNDIRLIST_BASE := \
         source/common \
         source/pp \
         source/rtl \
         source/macro \
         source/lang \
         source/codepage \
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
         DYNDIRLIST_BASE := x
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
         DYNDIRLIST_BASE += source/rtl/gtxwc
      endif
      ifeq ($(HB_COMPILER),watcom)
         DYNDIRLIST_BASE += source/vm/maindllh
      endif
      ifeq ($(HB_COMPILER),xcc)
         DYNDIRLIST_BASE += source/vm/maindllh
      endif

      ifneq ($(HB_HAS_PCRE_LOCAL),)
         DYNDIRLIST_BASE += external/pcre
      endif
      ifneq ($(HB_HAS_ZLIB_LOCAL),)
         DYNDIRLIST_BASE += external/zlib
      endif

      export DYNDIRLIST_BASE
      export DYNNAME_POST
   endif

   ifeq ($(HB_BUILD_PART),lib)
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
      debug$(HB_COMP_REF)

      ifneq ($(HB_BUILD_DLL),no)
         DIRS += dynlib{codepage,common,hbextern,lang,macro,pp,rdd,rtl,vm}
      endif

endif

include $(ROOT)config/dir.mk
