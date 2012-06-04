/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                    Xbase++ dataRef Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               06Dec2008
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

CLASS WvgDataRef

   DATA     changed                               INIT .F.
   DATA     dataLink                              INIT NIL
   DATA     lastValid                             INIT .T.
   DATA     sl_undo                               INIT NIL
   DATA     undoBuffer                            INIT NIL
   DATA     sl_validate                           INIT NIL

   METHOD   new()

   DATA     sl_editBuffer
   DATA     sl_buffer

   ACCESS   editBuffer                             INLINE ::sl_editBuffer
   ASSIGN   editBuffer( xData )                    INLINE ::sl_editBuffer := xData

   METHOD   getData()
   METHOD   setData( xValue, mp2 )
   METHOD   undo()

   METHOD   validate( xParam )                     SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgDataRef:new()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD WvgDataRef:getData()

   DO CASE
   CASE ::className == "COMBOBOX"
      IF HB_ISOBJECT( ::XbpListBox ) .AND. HB_ISBLOCK( ::XbpListBox:dataLink )
         ::sl_editBuffer := ::XbpListBox:getData()
      ELSEIF HB_ISOBJECT( ::XbpSLE ) .AND. HB_ISBLOCK( ::XbpSLE:dataLink )
         ::sl_editBuffer := ::XbpSLE:getData()
      ELSEIF HB_ISOBJECT( ::XbpListBox )
         ::sl_editBuffer := ::XbpListBox:getData()
      ENDIF

   CASE ::className == "EDIT"
      ::sl_editBuffer := WVG_GetMessageText( ::hWnd, WM_GETTEXT, ::bufferLength + 1 )

   CASE ::className == "LISTBOX"
      ::sl_editBuffer := WVG_LBGetCurSel( ::hWnd )+ 1

#if 0 /* This is contrary the documentation of Xbase++ */
      IF ::oParent:className == "COMBOBOX"
         ::sl_editBuffer := {}
         FOR i := 1 TO ::numItems()
            aadd( ::sl_editBuffer, ::getItem( i ) )
         NEXT
      ELSE
         ::sl_editBuffer := WVG_LBGetCurSel( ::hWnd )+ 1
      ENDIF
#endif
   ENDCASE

   IF HB_ISBLOCK( ::dataLink )
      eval( ::dataLink, ::sl_editBuffer )
   ENDIF

   RETURN ::sl_editBuffer

/*----------------------------------------------------------------------*/

METHOD WvgDataRef:setData( xValue, mp2 )
   LOCAL s

   HB_SYMBOL_UNUSED( mp2 )

   IF HB_ISBLOCK( ::dataLink )
      ::sl_editBuffer := eval( ::dataLink  )
   ELSEIF xValue != NIL
      ::sl_editBuffer := xValue
   ENDIF

   DO CASE

   CASE ::className == "BUTTON"     /* CheckBox, Radio, 3State */
      ::sendMessage( BM_SETCHECK, iif( ::sl_editBuffer, BST_CHECKED, BST_UNCHECKED ), 0 )

   CASE ::className == "LISTBOX"    /* Single Selection */
      IF HB_ISNUMERIC( ::sl_editBuffer )
         RETURN WVG_LBSetCurSel( ::hWnd, ::sl_editBuffer - 1 ) >= 0
      ENDIF

   CASE ::className == "SysTreeView32"
      IF ::sl_editBuffer != NIL .and. ::sl_editBuffer:hItem != NIL
         WVG_TreeView_SelectItem( ::hWnd, ::sl_editBuffer:hItem )
      ENDIF

   CASE ::className == "EDIT"
      IF hb_isChar( ::sl_editBuffer )
         WVG_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::sl_editBuffer )
      ENDIF

   CASE ::className == "SCROLLBAR"
      IF ::sl_editBuffer != NIL
         WAPI_SetScrollPos( ::pWnd, SB_CTL, ::sl_editBuffer, .t. )
      ENDIF

   CASE ::className == "COMBOBOX"
      IF HB_ISARRAY( ::sl_editBuffer )
         // NOT sure which way it should behave.
         // XBase++ documentation IN this regard is crappy.
         FOR EACH s IN ::sl_editBuffer
            ::addItem( s )
         NEXT
      ENDIF

   ENDCASE

   RETURN ::sl_editBuffer

/*----------------------------------------------------------------------*/

METHOD WvgDataRef:undo()

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD WvgDataRef:validate( xParam )

   IF PCount() == 0 .and. HB_ISBLOCK( ::sl_validate )
      RETURN eval( ::sl_validate, self )
   ELSEIF HB_ISBLOCK( xParam )
      ::sl_validate := xParam
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/
