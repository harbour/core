/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Cairo library: .prg include file
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * www - http://www.harbour-project.org
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

#ifndef HB_CAIRO_CH_
#define HB_CAIRO_CH_

/* cairo_font_slant_t */
#define CAIRO_FONT_SLANT_NORMAL    0
#define CAIRO_FONT_SLANT_ITALIC    1
#define CAIRO_FONT_SLANT_OBLIQUE   2

/* cairo_font_weight_t */
#define CAIRO_FONT_WEIGHT_NORMAL   0
#define CAIRO_FONT_WEIGHT_BOLD     1

/* cairo_path_data_type_t */
#define CAIRO_PATH_MOVE_TO         0
#define CAIRO_PATH_LINE_TO         1
#define CAIRO_PATH_CURVE_TO        2
#define CAIRO_PATH_CLOSE_PATH      3

/* cairo_line_cap_t */
#define CAIRO_LINE_CAP_BUTT        0
#define CAIRO_LINE_CAP_ROUND       1
#define CAIRO_LINE_CAP_SQUARE      2

/* cairo_line_join_t */
#define CAIRO_LINE_JOIN_MITER      0
#define CAIRO_LINE_JOIN_ROUND      1
#define CAIRO_LINE_JOIN_BEVEL      2

/* cairo_format_t */
#define CAIRO_FORMAT_ARGB32        0
#define CAIRO_FORMAT_RGB24         1
#define CAIRO_FORMAT_A8            2
#define CAIRO_FORMAT_A1            3

#endif /* HB_CAIRO_CH_ */ 
