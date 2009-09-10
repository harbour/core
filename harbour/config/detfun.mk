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

# USAGE:
#    ON CALL:
#       _DET_DSP_NAME - human readable name of external component.
#       _DET_VAR_INC_ - variable name containing user component control (typically "HB_INC_*").
#       _DET_VAR_HAS_ - variable name receiving detection result (typically "HB_HAS_*").
#       _DET_FLT_PLAT - positive and negative platform filters. Prefix negative ones with '!' char.
#       _DET_FLT_COMP - positive and negative compiler filters. Prefix negative ones with '!' char.
#       _DET_INC_DEFP - default location to look at. Not effective in 'HB_BUILD_EXTDEF=no' mode.
#       _DET_INC_HEAD - header filename to look for. Unless looking for a directory, prefix with forward slash.
#       - variable name specified by _DET_VAR_INC_ (typically "HB_INC_*") should contains:
#          (empty) or yes - will enable external component if found on default locations.
#          no             - will disable external component.
#          force          - will forcibly enable external component, bypass location checks,
#                           HB_HAS_* will have the content '.' (as local dir).
#          <dirlist>      - will specify locations to check for the external component.
#    ON RETURN:
#       - above variables cleared.
#       - _DET_RES_TEXT with human readable detection result.
#       - variable name specified in _DET_VAR_HAS_ (typically "HB_HAS_*") will
#         have any these values:
#          (empty)        - we can't use this component
#          <dirlist>      - component headers were found at these locations (typically one)

# show verbose information (empty|yes|very)
ifneq ($(_DET_OPT_VERB),)
   do_info = $(info ! Component: $(1))
else
   do_info =
endif

# preparing switch to HB_WITH_* variables from HB_INC_*
ifneq ($($(subst HB_INC_,HB_WITH_,$(_DET_VAR_INC_))),)
   $($(subst HB_INC_,HB_WITH_,$(_DET_VAR_INC_))) := $($(_DET_VAR_INC_))
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
                  ifeq ($($(_DET_VAR_INC_)),yes)
                     $(_DET_VAR_HAS_) :=
                  else
                     # bypass local check
                     ifeq ($($(_DET_VAR_INC_)),force)
                        $(_DET_VAR_HAS_) := .
                     endif
                  endif
                  ifeq ($($(_DET_VAR_HAS_)),)
                     ifneq ($(HB_BUILD_EXTDEF),no)
                        $(_DET_VAR_HAS_) := $(_DET_INC_DEFP)
                     endif
                  endif
                  ifneq ($($(_DET_VAR_HAS_)),)
                     ifneq ($($(_DET_VAR_HAS_)),.)
                        $(_DET_VAR_HAS_) := $(strip $(foreach d,$($(_DET_VAR_HAS_)),$(if $(wildcard $(d)$(_DET_INC_HEAD)),$(d),)))
                        ifeq ($($(_DET_VAR_HAS_)),)
                           _DET_RES_TEXT := '$(_DET_DSP_NAME)' not found
                           $(call do_info,$(_DET_RES_TEXT))
                        else
                           _DET_RES_TEXT := '$(_DET_DSP_NAME)' found in $($(_DET_VAR_HAS_))
                           ifeq ($(_DET_OPT_VERB),very)
                              $(call do_info,$(_DET_RES_TEXT))
                           endif
                        endif
                     endif
                  else
                     _DET_RES_TEXT := '$(_DET_DSP_NAME)' location not specified
                     $(call do_info,$(_DET_RES_TEXT))
                  endif
               else
                  _DET_RES_TEXT := '$(_DET_DSP_NAME)' not supported with $(HB_COMPILER) compiler
                  $(call do_info,$(_DET_RES_TEXT))
               endif
            else
               _DET_RES_TEXT := '$(_DET_DSP_NAME)' not supported with $(HB_COMPILER) compiler
               $(call do_info,$(_DET_RES_TEXT))
            endif
         else
            _DET_RES_TEXT := '$(_DET_DSP_NAME)' not supported on $(HB_PLATFORM) platform
            $(call do_info,$(_DET_RES_TEXT))
         endif
      else
         _DET_RES_TEXT := '$(_DET_DSP_NAME)' not supported on $(HB_PLATFORM) platform
         $(call do_info,$(_DET_RES_TEXT))
      endif
      _DET_POS :=
      _DET_NEG :=
   else
      _DET_RES_TEXT := '$(_DET_DSP_NAME)' explicitly disabled
      $(call do_info,$(_DET_RES_TEXT))
   endif
endif

_DET_DSP_NAME :=
_DET_VAR_INC_ :=
_DET_VAR_HAS_ :=
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP :=
_DET_INC_HEAD :=
