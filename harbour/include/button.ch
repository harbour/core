/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for menu classes and related functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_BUTTON_CH_
#define HB_BUTTON_CH_
#define _BUTTON_DEFINED /* NOTE: For complete CA-Clipper compatibility */

/* orientation modes for scrollbar class */
#define SCROLL_VERTICAL          1
#define SCROLL_HORIZONTAL        2

/* menu separators strings */
#define MENU_SEPARATOR           Chr(196)
#define SEPARATOR_DOUBLE         ( Chr(204) + Chr(205) + Chr(185) )        /* double line left and right and double separator for popup menu */
#define SEPARATOR_SINGLE         ( Chr(195) + MENU_SEPARATOR + Chr(180) )  /* single separator for popup menu */
#define SEPARATOR_DOUBLE_SINGLE  ( Chr(199) + MENU_SEPARATOR + Chr(182) )  /* double line left and right and single separator for popup menu */

/* string constants for menuitems display */
#define HB_TMENUITEM_STYLE       ( Chr(251) + Chr(16) )

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
