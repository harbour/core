/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               17Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"

/*----------------------------------------------------------------------*/

FUNCTION GetStyleSheet( cWidget )
   LOCAL s, txt_:={}

   DO CASE
   CASE cWidget == "QListView"
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView {                                                            " )
      aadd( txt_, "     alternate-background-color: yellow;                                " )
      aadd( txt_, " }                                                                      " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView {                                                            " )
      aadd( txt_, "     show-decoration-selected: 1;                                       " )
      aadd( txt_, " }                                                                      " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView::item:alternate {                                            " )
      aadd( txt_, "     background: #EEEEEE;                                               " )
      aadd( txt_, " }                                                                      " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView::item:selected {                                             " )
      aadd( txt_, "     border: 1px solid #6a6ea9;                                         " )
      aadd( txt_, " }                                                                      " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView::item:selected:!active {                                     " )
      aadd( txt_, "     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,            " )
      aadd( txt_, "                                 stop: 0 #ABAFE5, stop: 1 #8588B2);     " )
      aadd( txt_, " }                                                                      " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView::item:selected:active {                                      " )
      aadd( txt_, "     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,            " )
      aadd( txt_, "                                 stop: 0 #6a6ea9, stop: 1 #888dd9);     " )
      aadd( txt_, " }                                                                      " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView::item:hover {                                                " )
      aadd( txt_, "     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,            " )
      aadd( txt_, "                                 stop: 0 #FAFBFE, stop: 1 #DCDEF1);     " )
      aadd( txt_, "}                                                                       " )
      aadd( txt_, "                                                                        " )
      aadd( txt_, " QListView {                                                            " )
      aadd( txt_, "     background: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 0,            " )
      aadd( txt_, "                                 stop: 0 #8588B2, stop: 1 #DCDEF1);     " )
      aadd( txt_, "}                                                                       " )
      aadd( txt_, "                                                                        " )

   ENDCASE

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   RETURN s

/*----------------------------------------------------------------------*/

