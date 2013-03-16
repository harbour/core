
ROOT := ../

include $(TOP)$(ROOT)config/global.mk

ifeq ($(HB_BUILD_PARTS),compiler)

   DIRS := \
      common \
      nortl \
      pp{common,nortl} \
      compiler \
      main{pp,compiler} \

else

   ifneq ($(HB_BUILD_DYN),no)

      DYNDIRLIST_BASE := \
         src/common \
         src/pp \
         src/rtl \
         src/macro \
         src/lang \
         src/codepage \
         src/hbextern \
         src/rdd \
         src/rdd/dbfntx \
         src/rdd/dbfnsx \
         src/rdd/dbfcdx \
         src/rdd/dbffpt \
         src/rdd/hbsix \
         src/rdd/hsx \
         src/rdd/usrrdd \
         src/rtl/gtcgi \
         src/rtl/gtpca \
         src/rtl/gtstd

      ifeq ($(HB_PLATFORM),dos)
         DYNDIRLIST_BASE += src/rtl/gtdos
      endif
      ifeq ($(HB_PLATFORM),os2)
         DYNDIRLIST_BASE += src/rtl/gtos2
      endif
      ifeq ($(HB_PLATFORM),wce)
         DYNDIRLIST_BASE += src/rtl/gtwvt src/rtl/gtgui
      endif
      ifeq ($(HB_PLATFORM),win)
         DYNDIRLIST_BASE += src/rtl/gtwvt src/rtl/gtgui src/rtl/gtwin
      endif
      ifneq ($(HB_PLATFORM_UNIX),)
         DYNDIRLIST_BASE += src/rtl/gttrm
         DYNDIRLIST_BASE += src/rtl/gtcrs
         DYNDIRLIST_BASE += src/rtl/gtsln
         DYNDIRLIST_BASE += src/rtl/gtxwc
      endif
      ifeq ($(HB_COMPILER),watcom)
         DYNDIRLIST_BASE += src/vm/maindllh
      endif
      ifeq ($(HB_COMPILER),xcc)
         DYNDIRLIST_BASE += src/vm/maindllh
      endif

      ifneq ($(HB_HAS_PCRE_LOCAL),)
         DYNDIRLIST_BASE += src/3rd/pcre
      endif
      ifneq ($(HB_HAS_ZLIB_LOCAL),)
         DYNDIRLIST_BASE += src/3rd/zlib
      endif

      export DYNDIRLIST_BASE
   endif

   ifeq ($(HB_BUILD_PARTS),lib)
      HB_COMP_DIR :=
      HB_COMP_REF :=
      HB_COMP_RF2 := {pp}
   else
      HB_COMP_DIR := main{pp,compiler}
      HB_COMP_REF := {main}
      HB_COMP_RF2 := {main}
   endif

   DIRS := \
      3rd \
      common \
      nortl \
      pp{common,nortl} \
      compiler \
      $(HB_COMP_DIR) \
      vm$(HB_COMP_RF2) \
      rtl$(HB_COMP_REF) \
      macro \
      codepage \
      lang \
      rdd$(HB_COMP_REF) \
      hbextern$(HB_COMP_REF) \
      debug$(HB_COMP_REF)

      ifneq ($(HB_BUILD_DYN),no)
         DIRS += dynlib{3rd,codepage,common,hbextern,lang,macro,pp,rdd,rtl,vm}
      endif

endif

include $(ROOT)config/dir.mk
