/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for menu classes and related functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#ifndef HB_BUTTON_CH_
#define HB_BUTTON_CH_
#define _BUTTON_DEFINED /* NOTE: For complete CA-Cl*pper compatibility */

/* orientation modes for scrollbar class */
#define SCROLL_VERTICAL          1
#define SCROLL_HORIZONTAL        2

/* menu separators strings */
#define MENU_SEPARATOR           Chr( 196 ) /* "─" */
#define SEPARATOR_DOUBLE         ( Chr( 204 ) + Chr( 205 ) + Chr( 185 ) )      /* "╠═╣" double line left and right and double separator for popup menu */
#define SEPARATOR_SINGLE         ( Chr( 195 ) + MENU_SEPARATOR + Chr( 180 ) )  /* "├" "┤" single separator for popup menu */
#define SEPARATOR_DOUBLE_SINGLE  ( Chr( 199 ) + MENU_SEPARATOR + Chr( 182 ) )  /* "╟" "╢" double line left and right and single separator for popup menu */

#define HB_MENU_SEPARATOR_UNI          hb_UTF8ToStrBox( "─" )
#define HB_SEPARATOR_DOUBLE_UNI        hb_UTF8ToStrBox( "╠═╣" )
#define HB_SEPARATOR_SINGLE_UNI        hb_UTF8ToStrBox( "├─┤" )
#define HB_SEPARATOR_DOUBLE_SINGLE_UNI hb_UTF8ToStrBox( "╟─╢" )

/* return values for HitTest methods */
#define HTNOWHERE                0

#define HTTOPLEFT                -1
#define HTTOP                    -2
#define HTTOPRIGHT               -3
#define HTRIGHT                  -4
#define HTBOTTOMRIGHT            -5
#define HTBOTTOM                 -6
#define HTBOTTOMLEFT             -7
#define HTLEFT                   -8

#define HTBORDERFIRST            -8
#define HTBORDERLAST             -1

#define HTCAPTION                -1025

#define HTCLIENT                 -2049

#define HTSCROLLTHUMBDRAG        -3073
#define HTSCROLLUNITDEC          -3074
#define HTSCROLLUNITINC          -3075
#define HTSCROLLBLOCKDEC         -3076
#define HTSCROLLBLOCKINC         -3077

#define HTSCROLLFIRST            -3077
#define HTSCROLLLAST             -3073

#define HTDROPBUTTON             -4097
#define HTSEPARATOR              -4098

#define HTCELL                   -5121
#define HTHEADING                -5122
#define HTFOOTING                -5123
#define HTHEADSEP                -5124
#define HTFOOTSEP                -5125
#define HTCOLSEP                 -5126

#define HTMENU                   -6145
#define HTSIZE                   -6146
#define HTMINBUTTON              -6147
#define HTMAXBUTTON              -6148
#define HTGROWBOX                HTSIZE
#define HTREDUCE                 HTMINBUTTON
#define HTZOOM                   HTMAXBUTTON

#endif /* HB_BUTTON_CH_ */
