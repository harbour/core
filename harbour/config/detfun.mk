#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
# See COPYING for licensing terms.
#
# This make file will detect optional external components
# used in Harbour core code. Generic function.
# ---------------------------------------------------------------

# Show verbose information (empty|yes|very)
ifneq ($(_DET_OPT_VERB),)
   do_info = $(info ! Component: $(1))
else
   do_info =
endif

_DET_RES_TEXT :=

ifeq ($($(_DET_VAR_HAS_)),)
   ifneq ($($(_DET_VAR_INC_)),no)
      _DET_POS := $(filter-out !%,$(_DET_FLT_PLAT))
      _DET_NEG := $(subst !,,$(filter !%,$(_DET_FLT_PLAT)))
      ifneq ($(if $(_DET_POS),$(filter $(HB_PLATFORM),$(_DET_POS)),ok),)
         ifeq ($(filter $(HB_PLATFORM),$(_DET_NEG)),)
            _DET_POS := $(filter-out !%,$(_DET_FLT_COMP))
            _DET_NEG := $(subst !,,$(filter !%,$(_DET_FLT_COMP)))
            ifneq ($(if $(_DET_POS),$(filter $(HB_COMPILER),$(_DET_POS)),ok),)
               ifeq ($(filter $(HB_COMPILER),$(_DET_NEG)),)
                  $(_DET_VAR_HAS_) := $($(_DET_VAR_INC_))
                  ifeq ($($(_DET_VAR_HAS_)),yes)
                     $(_DET_VAR_HAS_) :=
                  endif
                  ifeq ($($(_DET_VAR_HAS_)),)
                     ifeq ($(HB_XBUILD),)
                        $(_DET_VAR_HAS_) := $(_DET_INC_DEFP)
                     endif
                  endif
                  ifneq ($($(_DET_VAR_HAS_)),)
                     $(_DET_VAR_HAS_) := $(strip $(foreach d,$($(_DET_VAR_HAS_)),$(if $(wildcard $(d)$(_DET_INC_HEAD)),$(d),)))
                     ifeq ($($(_DET_VAR_HAS_)),)
                        _DET_RES_TEXT := $(_DET_DSP_NAME) not found
                        $(call do_info,$(_DET_RES_TEXT))
                     else
                        _DET_RES_TEXT := $(_DET_DSP_NAME) found in $($(_DET_VAR_HAS_))
                        ifeq ($(_DET_OPT_VERB),very)
                           $(call do_info,$(_DET_RES_TEXT))
                        endif
                     endif
                  else
                     _DET_RES_TEXT := $(_DET_DSP_NAME) location not specified
                     $(call do_info,$(_DET_RES_TEXT))
                  endif
               else
                  _DET_RES_TEXT := $(_DET_DSP_NAME) not supported with $(HB_COMPILER) compiler
                  $(call do_info,$(_DET_RES_TEXT))
               endif
            else
               _DET_RES_TEXT := $(_DET_DSP_NAME) not supported with $(HB_COMPILER) compiler
               $(call do_info,$(_DET_RES_TEXT))
            endif
         else
            _DET_RES_TEXT := $(_DET_DSP_NAME) not supported on $(HB_PLATFORM) platform
            $(call do_info,$(_DET_RES_TEXT))
         endif
      else
         _DET_RES_TEXT := $(_DET_DSP_NAME) not supported on $(HB_PLATFORM) platform
         $(call do_info,$(_DET_RES_TEXT))
      endif
      _DET_POS :=
      _DET_NEG :=
   else
      _DET_RES_TEXT := $(_DET_DSP_NAME) explicitly disabled
      $(call do_info,$(_DET_RES_TEXT))
   endif
endif
