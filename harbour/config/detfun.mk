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

# Show verbose information
_DET_SHOW_RES := yes

ifeq ($(_DET_SHOW_RES),yes)
   do_info = $(info ! Component: $(1))
else
   do_info =
endif

ifeq ($($(_DET_VAR_HAS_)),)
   ifneq ($($(_DET_VAR_INC_)),no)
      ifeq ($(filter $(HB_PLATFORM),$(_DET_UNS_PLAT)),)
         ifeq ($(filter $(HB_COMPILER),$(_DET_UNS_COMP)),)
            $(_DET_VAR_HAS_) := $($(_DET_VAR_INC_))
            ifeq ($($(_DET_VAR_HAS_)),)
               ifeq ($(HB_XBUILD),)
                  $(_DET_VAR_HAS_) := $(_DET_INC_DEFP)
               endif
            endif
            ifneq ($($(_DET_VAR_HAS_)),)
               $(_DET_VAR_HAS_) := $(strip $(foreach d,$($(_DET_VAR_HAS_)),$(if $(wildcard $(d)$(_DET_INC_HEAD)),$(d),)))
               ifeq ($($(_DET_VAR_HAS_)),)
                  $(call do_info,$(_DET_DSP_NAME) not found)
               else
                  $(call do_info,$(_DET_DSP_NAME) found in $($(_DET_VAR_HAS_)))
               endif
            else
               $(call do_info,$(_DET_DSP_NAME) location not specified)
            endif
         else
            $(call do_info,$(_DET_DSP_NAME) not supported with $(HB_COMPILER) compiler)
         endif
      else
         $(call do_info,$(_DET_DSP_NAME) not supported on $(HB_PLATFORM) platform)
      endif
   else
      $(call do_info,$(_DET_DSP_NAME) explicitly deselected)
   endif
endif
