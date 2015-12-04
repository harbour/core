# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# This makefile will detect optional 3rd party components
# used in Harbour core code.

# config.mk if present is root, is able to override HB_HAS_* values.

ifeq ($(DETECT_MK_),)
export DETECT_MK_ := yes

_DET_OPT_VERB := very

# Reset everything to default
export HB_HAS_ZLIB       :=
export HB_HAS_ZLIB_LOCAL :=
export HB_HAS_PCRE       :=
export HB_HAS_PCRE_LOCAL :=
export HB_HAS_OPENSSL    :=
export HB_HAS_GPM        :=
export HB_HAS_SLANG      :=
export HB_HAS_CURSES     :=
export HB_HAS_X11        :=
export HB_HAS_WATT       :=

# Exclude Harbour-wide features prohibiting commercial use

ifeq ($(HB_BUILD_NOGPLLIB),yes)
   export HB_INC_GPM := no
   export HB_INC_SLANG := no
endif

# Allow detection by external (generated) config file

-include $(TOP)$(ROOT)config.mk

# Detect zlib

_DET_DSP_NAME := zlib
_DET_VAR_INC_ := HB_INC_ZLIB
_DET_VAR_HAS_ := HB_HAS_ZLIB
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/include /boot/develop/headers/3rdparty
_DET_INC_LOCL := src/3rd/zlib
_DET_INC_HEAD := /zlib.h

include $(TOP)$(ROOT)config/detfun.mk

# Detect pcre2

_DET_DSP_NAME := pcre2
_DET_VAR_INC_ := HB_INC_PCRE2
_DET_VAR_HAS_ := HB_HAS_PCRE2
_DET_FLT_PLAT :=
_DET_FLT_COMP := !bcc
_DET_INC_DEFP := /usr/include /usr/local/include /usr/pkg/include /opt/csw/include
_DET_INC_LOCL := src/3rd/pcre2
_DET_INC_HEAD := /pcre2.h

include $(TOP)$(ROOT)config/detfun.mk

ifeq ($(HB_HAS_PCRE2),)

   # Detect pcre

   _DET_DSP_NAME := pcre
   _DET_VAR_INC_ := HB_INC_PCRE
   _DET_VAR_HAS_ := HB_HAS_PCRE
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include /usr/local/include /usr/pkg/include /opt/csw/include
   _DET_INC_LOCL := src/3rd/pcre
   _DET_INC_HEAD := /pcre.h

   include $(TOP)$(ROOT)config/detfun.mk
endif

# Detect GPM mouse

_DET_DSP_NAME := gpm
_DET_VAR_INC_ := HB_INC_GPM
_DET_VAR_HAS_ := HB_HAS_GPM
_DET_FLT_PLAT := linux
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include /usr/local/include
_DET_INC_HEAD := /gpm.h

include $(TOP)$(ROOT)config/detfun.mk

# Detect s-lang

_DET_DSP_NAME := s-lang
_DET_VAR_INC_ := HB_INC_SLANG
_DET_VAR_HAS_ := HB_HAS_SLANG
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP :=
_DET_INC_HEAD := /slang.h

_DET_INC_DEFP += /usr/include /usr/include/slang
_DET_INC_DEFP += /usr/local/include /usr/local/include/slang
_DET_INC_DEFP += /sw/include /sw/include/slang
_DET_INC_DEFP += /opt/local/include /opt/local/include/slang
_DET_INC_DEFP += /usr/pkg/include/slang2 /usr/pkg/include

include $(TOP)$(ROOT)config/detfun.mk

# Detect curses

_DET_DSP_NAME := curses
_DET_VAR_INC_ := HB_INC_CURSES
_DET_VAR_HAS_ := HB_HAS_CURSES
_DET_FLT_PLAT := !os2
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/local/opt/ncurses/include /usr/include /usr/include/ncurses /usr/local/include /sw/include /opt/local/include /boot/develop/headers/3rdparty
ifeq ($(HB_PLATFORM),win)
_DET_INC_HEAD := /ncursesw/curses.h
else
_DET_INC_HEAD := /curses.h
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
_DET_INC_DEFP := /usr/include /usr/local/include /usr/X11R6/include /usr/pkg/include /usr/pkg/X11R6/include /opt/X11/include
_DET_INC_HEAD := /X11/Xlib.h

include $(TOP)$(ROOT)config/detfun.mk

# Detect WATTCP/WATT-32 in MS-DOS builds

_DET_DSP_NAME := wattcp/watt-32
_DET_VAR_INC_ := HB_INC_WATT
_DET_VAR_HAS_ := HB_HAS_WATT
_DET_FLT_PLAT := dos
_DET_FLT_COMP :=
_DET_INC_DEFP := $(if $(WATT_ROOT),$(subst \,/,$(WATT_ROOT))/inc,) /usr/include
_DET_INC_HEAD := /sys/socket.h

include $(TOP)$(ROOT)config/detfun.mk

HB_LIB_WATT := $(subst \,/,$(HB_HAS_WATT))
export HB_LIB_WATT := $(HB_LIB_WATT:/inc=/lib)

# Finished

_DET_OPT_VERB :=

endif # DETECT_MK_
