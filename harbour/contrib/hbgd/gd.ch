/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GD graphic library header file.
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef GD_CH
#define GD_CH

/* The maximum number of palette entries in palette-based images.
   In the wonderful new world of gd 2.0, you can of course have
   many more colors when using truecolor mode. */

#define gdMaxColors                   256

#define gdAlphaMax                    127
#define gdAlphaOpaque                 0
#define gdAlphaTransparent            127
#define gdRedMax                      255
#define gdGreenMax                    255
#define gdBlueMax                     255

/* For backwards compatibility only. Use gdImageSetStyle()
   for MUCH more flexible line drawing. Also see gdImageSetBrush(). */
#define gdDashSize                    4

/* Special colors. */

#define gdStyled                      -2
#define gdBrushed                     -3
#define gdStyledBrushed               -4
#define gdTiled                       -5

/* NOT the same as the transparent color index.
   This is used in line styles only. */
#define gdTransparent                 -6

#define gdAntiAliased                 -7

#define gdFTEX_LINESPACE              1
#define gdFTEX_CHARMAP                2
#define gdFTEX_RESOLUTION             4

/* These are NOT flags; set one in 'charmap' if you set the
   gdFTEX_CHARMAP bit in 'flags'. */
#define gdFTEX_Unicode                0
#define gdFTEX_Shift_JIS              1
#define gdFTEX_Big5                   2

#define gdArc                         0
#define gdPie                         gdArc
#define gdChord                       1
#define gdNoFill                      2
#define gdEdged                       4

#define GD2_CHUNKSIZE                 128
#define GD2_CHUNKSIZE_MIN             64
#define GD2_CHUNKSIZE_MAX             4096

#define GD2_FMT_RAW                   1
#define GD2_FMT_COMPRESSED            2

#define GD_CMP_IMAGE                  1   /* Actual image IS different */
#define GD_CMP_NUM_COLORS             2   /* Number of Colours in pallette differ */
#define GD_CMP_COLOR                  4   /* Image colours differ */
#define GD_CMP_SIZE_X                 8   /* Image width differs */
#define GD_CMP_SIZE_Y                 16  /* Image heights differ */
#define GD_CMP_TRANSPARENT            32  /* Transparent colour */
#define GD_CMP_BACKGROUND             64  /* Background colour */
#define GD_CMP_INTERLACE              128 /* Interlaced setting */
#define GD_CMP_TRUECOLOR              256 /* Truecolor vs palette differs */

/* resolution affects ttf font rendering, particularly hinting */
#define GD_RESOLUTION                 96  /* pixels per inch */

/* Legal values for Disposal. gdDisposalNone is always used by
   the built-in optimizer if previm is passed. */
#define gdDisposalUnknown             0
#define gdDisposalNone                1
#define gdDisposalRestoreBackground   2
#define gdDisposalRestorePrevious     3

/* FSG - text alignment */
#define gdAlignLeft                   0
#define gdAlignCenter                 1
#define gdAlignRight                  2

#endif /* GD_CH */
