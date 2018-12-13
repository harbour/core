/*
 * Xbase++ dataRef Compatible Class
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/*                                EkOnkar
 *                          ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgDataRef

   VAR    changed                               INIT .F.
   VAR    dataLink                              INIT NIL
   VAR    lastValid                             INIT .T.
   VAR    sl_undo                               INIT NIL
   VAR    undoBuffer                            INIT NIL
   VAR    sl_validate                           INIT NIL

   METHOD new()

   VAR    sl_editBuffer
   VAR    sl_buffer

   ACCESS editBuffer                             INLINE ::sl_editBuffer
   ASSIGN editBuffer( xData )                    INLINE ::sl_editBuffer := xData

   METHOD getData()
   METHOD setData( xValue, mp2 )
   METHOD undo()

   METHOD validate( xParam )                     SETGET

ENDCLASS

METHOD WvgDataRef:new()
   RETURN Self

METHOD WvgDataRef:getData()

   DO CASE
   CASE ::className() == "COMBOBOX"
      IF HB_ISOBJECT( ::XbpListBox ) .AND. HB_ISBLOCK( ::XbpListBox:dataLink )
         ::sl_editBuffer := ::XbpListBox:getData()
      ELSEIF HB_ISOBJECT( ::XbpSLE ) .AND. HB_ISBLOCK( ::XbpSLE:dataLink )
         ::sl_editBuffer := ::XbpSLE:getData()
      ELSEIF HB_ISOBJECT( ::XbpListBox )
         ::sl_editBuffer := ::XbpListBox:getData()
      ENDIF

   CASE ::className() == "EDIT"
      ::sl_editBuffer := wvg_GetMessageText( ::hWnd, WM_GETTEXT, ::bufferLength + 1 )

   CASE ::className() == "LISTBOX"
      ::sl_editBuffer := wvg_LBGetCurSel( ::hWnd ) + 1

#if 0 /* This is contrary the documentation of Xbase++ */
      IF ::oParent:className() == "COMBOBOX"
         ::sl_editBuffer := {}
         FOR i := 1 TO ::numItems()
            AAdd( ::sl_editBuffer, ::getItem( i ) )
         NEXT
      ELSE
         ::sl_editBuffer := wvg_LBGetCurSel( ::hWnd ) + 1
      ENDIF
#endif
   ENDCASE

   IF HB_ISBLOCK( ::dataLink )
      Eval( ::dataLink, ::sl_editBuffer )
   ENDIF

   RETURN ::sl_editBuffer

METHOD WvgDataRef:setData( xValue, mp2 )

   LOCAL s

   HB_SYMBOL_UNUSED( mp2 )

   IF HB_ISBLOCK( ::dataLink )
      ::sl_editBuffer := Eval( ::dataLink  )
   ELSEIF xValue != NIL
      ::sl_editBuffer := xValue
   ENDIF

   DO CASE

   CASE ::className() == "BUTTON"     /* CheckBox, Radio, 3State */
      ::sendMessage( BM_SETCHECK, iif( ::sl_editBuffer, BST_CHECKED, BST_UNCHECKED ), 0 )

   CASE ::className() == "LISTBOX"    /* Single Selection */
      IF HB_ISNUMERIC( ::sl_editBuffer )
         RETURN wvg_lbSetCurSel( ::hWnd, ::sl_editBuffer - 1 ) >= 0
      ENDIF

   CASE ::className() == "SysTreeView32"
      IF ::sl_editBuffer != NIL .AND. ::sl_editBuffer:hItem != NIL
         wvg_TreeView_SelectItem( ::hWnd, ::sl_editBuffer:hItem )
      ENDIF

   CASE ::className() == "EDIT"
      IF HB_ISSTRING( ::sl_editBuffer )
         wvg_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::sl_editBuffer )
      ENDIF

   CASE ::className() == "SCROLLBAR"
      IF ::sl_editBuffer != NIL
         wapi_SetScrollPos( ::pWnd, SB_CTL, ::sl_editBuffer, .T. )
      ENDIF

   CASE ::className() == "COMBOBOX"
      IF HB_ISARRAY( ::sl_editBuffer )
         // NOT sure which way it should behave.
         // Xbase++ documentation IN this regard is crappy.
         FOR EACH s IN ::sl_editBuffer
            ::addItem( s )
         NEXT
      ENDIF

   ENDCASE

   RETURN ::sl_editBuffer

METHOD WvgDataRef:undo()
   RETURN .F.

METHOD WvgDataRef:validate( xParam )

   IF PCount() == 0 .AND. HB_ISBLOCK( ::sl_validate )
      RETURN Eval( ::sl_validate, Self )
   ELSEIF HB_ISBLOCK( xParam )
      ::sl_validate := xParam
   ENDIF

   RETURN .T.
