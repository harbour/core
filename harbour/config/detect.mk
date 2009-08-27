#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
# See COPYING for licensing terms.
#
# This make file will detect optional external components
# used in Harbour core code.
# ---------------------------------------------------------------

# config/conf.mk if present, is able to override HB_HAS_* values.

ifeq ($(DETECT_MK_),)
export DETECT_MK_ := yes

_DET_OPT_VERB := very

# Reset everything to default
export HB_HAS_OPENSSL :=
export HB_HAS_GPM     :=
export HB_HAS_SLANG   :=
export HB_HAS_CURSES  :=
export HB_HAS_X11     :=

# Allow detection by external (generated) config file

-include $(TOP)$(ROOT)config/conf.mk

# Exclude Harbour-wide features prohibiting commercial use

ifeq ($(HB_COMMERCE),yes)
   export HB_INC_GPM := no
   export HB_INC_SLANG := no
endif

# Detect OpenSSL

_DET_DSP_NAME := openssl
_DET_VAR_INC_ := HB_INC_OPENSSL
_DET_VAR_HAS_ := HB_HAS_OPENSSL
_DET_FLT_PLAT := !dos
_DET_FLT_COMP := !watcom
_DET_INC_DEFP := /usr/include /usr/local/ssl/include
_DET_INC_HEAD := /openssl/ssl.h

include $(TOP)$(ROOT)config/detfun.mk

# Detect GPM mouse

_DET_DSP_NAME := gpm
_DET_VAR_INC_ := HB_INC_GPM
_DET_VAR_HAS_ := HB_HAS_GPM
_DET_FLT_PLAT := linux
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/include
_DET_INC_HEAD := /gpm.h

include $(TOP)$(ROOT)config/detfun.mk

# Detect slang

_DET_DSP_NAME := slang
_DET_VAR_INC_ := HB_INC_SLANG
_DET_VAR_HAS_ := HB_HAS_SLANG
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP :=
_DET_INC_HEAD := /slang.h

ifeq ($(HB_LOCAL_SLN),yes)
   _DET_INC_DEFP += /usr/local/include /usr/local/include/slang
else
   _DET_INC_DEFP += /usr/include /usr/include/slang
   _DET_INC_DEFP += /usr/usr/local/include /usr/local/include/slang
   _DET_INC_DEFP += /sw/include /sw/include/slang
   _DET_INC_DEFP += /opt/local/include /opt/local/include/slang
endif

include $(TOP)$(ROOT)config/detfun.mk

# Detect curses

_DET_DSP_NAME := curses
_DET_VAR_INC_ := HB_INC_CURSES
_DET_VAR_HAS_ := HB_HAS_CURSES
_DET_FLT_PLAT := !os2
_DET_FLT_COMP :=
_DET_INC_DEFP :=
_DET_INC_HEAD := /curses.h

ifeq ($(HB_NCURSES_194),yes)
   _DET_INC_DEFP += /usr/include/ncur194
else
   _DET_INC_DEFP += /usr/include /usr/local/include /sw/include /opt/local/include
endif
ifeq ($(HB_COMPILER),djgpp)
   _DET_INC_DEFP += $(foreach d, $(subst $(PTHSEP), ,$(PATH)), $(d)/../include)
endif

include $(TOP)$(ROOT)config/detfun.mk

# Detect X11

_DET_DSP_NAME := x11
_DET_VAR_INC_ := HB_INC_X11
_DET_VAR_HAS_ := HB_HAS_X11
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
_DET_INC_HEAD := /X11/Xlib.h

include $(TOP)$(ROOT)config/detfun.mk

# Finished

_DET_OPT_VERB :=

endif # DETECT_MK_
