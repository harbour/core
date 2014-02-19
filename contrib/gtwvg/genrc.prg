/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

//                               EkOnkar
//                         ( The LORD is ONE )
//
//                  Xbase++ Parts Compatible functions
//
//                Pritpal Bedi  <bedipritpal@hotmail.com>
//                              03Dec2011
//
#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

THREAD STATIC t_oCrt := NIL

EXIT PROCEDURE KillGTChildren()

   IF HB_ISOBJECT( t_oCrt ) .AND. __objGetClsName( t_oCrt ) == "WVGCRT" .AND. t_oCrt:isGT
      t_oCrt:destroy()
      t_oCrt := NIL
   ENDIF

   RETURN

FUNCTION WvgSetAppWindow( oCrt )

   LOCAL ooCrt := t_oCrt

   IF Empty( oCrt )
      IF Empty( t_oCrt )
         t_oCrt := WvgCrt():new()
         t_oCrt:hWnd := Wvt_GetWindowHandle()
         t_oCrt:pWnd := win_N2P( t_oCrt:hWnd )
         hb_gtInfo( HB_GTI_NOTIFIERBLOCKGUI, {| nEvent, ... | t_oCrt:notifier( nEvent, ... ) } )
         t_oCrt:isGT := .T.
         RETURN t_oCrt
      ENDIF
   ELSE
      t_oCrt := oCrt
   ENDIF

   RETURN ooCrt
