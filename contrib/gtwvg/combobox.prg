/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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

/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTreeView compatible Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                               26Nov2008
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgComboBox  INHERIT  WvgWindow, WvgDataRef

   VAR    type                                  INIT    WVGCOMBO_DROPDOWN
   VAR    drawMode                              INIT    WVG_DRAW_NORMAL
   VAR    nCurSelected                          INIT    0

   VAR    aInfo                                 INIT    NIL

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD sendCBMessage( nMsg, wParam, lParam ) INLINE Wvg_SendCBMessage( ::pWnd, nMsg, wParam, lParam )
   METHOD listBoxFocus( lFocus )
   METHOD listBoxSize()
   METHOD sleSize()

   METHOD addItem( cItem )
   METHOD clear()                               INLINE ::sendCBMessage( CB_RESETCONTENT )
   METHOD delItem( nIndex )                     INLINE ::sendCBMessage( CB_DELETESTRING, nIndex - 1 )
   METHOD getItem( nIndex )                     INLINE ::sendCBMessage( CB_GETLBTEXT, nIndex - 1 )
   METHOD insItem( nIndex, cItem )              INLINE ::sendCBMessage( CB_INSERTSTRING, nIndex - 1, cItem )
   METHOD setItem( nIndex, cItem )              VIRTUAL
   METHOD setIcon( nItem, cIcon )

   VAR    oSLE
   VAR    oListBox
   ACCESS XbpSLE                                INLINE  ::oSLE
   ACCESS XbpListBox                            INLINE  ::oListBox

   VAR    sl_itemMarked
   VAR    sl_itemSelected
   VAR    sl_drawItem

   METHOD itemMarked( ... )                     SETGET
   METHOD itemSelected( ... )                   SETGET
   METHOD drawItem( ... )                       SETGET

ENDCLASS

METHOD WvgComboBox:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + WS_TABSTOP + WS_BORDER + WS_VSCROLL + CBS_NOINTEGRALHEIGHT + CBS_AUTOHSCROLL
#if 0
   ::exStyle     := WS_EX_CLIENTEDGE
#endif

   ::className   := "COMBOBOX"
   ::objType     := objTypeComboBox

   RETURN Self

METHOD WvgComboBox:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oParent:AddChild( Self )

   IF ::type == WVGCOMBO_DROPDOWNLIST
      ::style += CBS_DROPDOWNLIST
   ELSEIF ::type == WVGCOMBO_SIMPLE
      ::style += CBS_SIMPLE
   ELSE
      ::style += CBS_DROPDOWN
   ENDIF

   ::createControl()

#if 0
   ::SetWindowProcCallback()   /* Let parent control the events - WM_COMMAND */
#endif

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   /* Build SLE and ListBox Part - May not be available for all Windows Versions - How to handle then ? */
   IF ! Empty( ::aInfo := ::sendCBMessage( CB_GETCOMBOBOXINFO ) )
      ::oSLE := WvgSLE():new()
      ::oSLE:oParent := Self
      ::oSLE:hWnd := ::aInfo[ 5 ]
      ::oSLE:pWnd := win_N2P( ::aInfo[ 5 ] )

      ::oListBox := WvgListBox():new()
      ::oListBox:oParent := Self
      ::oListBox:hWnd := ::aInfo[ 6 ]
      ::oListBox:pWnd := win_N2P( ::aInfo[ 6 ] )
   ENDIF

   RETURN Self

METHOD WvgComboBox:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgComboBox:destroy()

#if 0
   IF HB_ISOBJECT( ::oSLE )
      ::oSLE:destroy()
   ENDIF
   IF HB_ISOBJECT( ::oListBox )
      ::oListBox:destroy()
   ENDIF
#endif
   ::wvgWindow:destroy()

   RETURN NIL

METHOD WvgComboBox:handleEvent( nMessage, aNM )

   DO CASE

   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )

   CASE nMessage == HB_GTE_COMMAND
      DO CASE
      CASE aNM[ 1 ] == CBN_SELCHANGE
         ::nCurSelected := ::editBuffer := Wvg_LBGetCurSel( ::hWnd ) + 1
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         ::itemMarked()
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF

      CASE aNM[ 1 ] == CBN_DBLCLK
         ::editBuffer := ::nCurSelected
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         ::itemSelected()
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF

      CASE aNM[ 1 ] == CBN_KILLFOCUS
         ::killInputFocus()

      CASE aNM[ 1 ] == CBN_SETFOCUS
         ::setInputFocus()

      ENDCASE

   CASE nMessage == HB_GTE_KEYTOITEM
      IF aNM[ 1 ] == K_ENTER
         IF ::isParentCrt()
            ::oParent:setFocus()
         ENDIF
         ::itemSelected()
         IF ::isParentCrt()
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
      ENDIF

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         Wvg_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF HB_ISNUMERIC( ::hBrushBG )
         Wvg_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ELSE
         RETURN Wvg_GetCurrentBrush( aNM[ 1 ] )
      ENDIF

   ENDCASE

   RETURN EVENT_UNHANDELLED

METHOD WvgComboBox:addItem( cItem )

   IF HB_ISSTRING( cItem )
      RETURN ::sendCBMessage( CB_ADDSTRING, cItem )
   ENDIF

   RETURN -1

METHOD WvgComboBox:listBoxFocus( lFocus )

   LOCAL lOldFocus := ::sendCBMessage( CB_GETDROPPEDSTATE )

   IF HB_ISLOGICAL( lFocus )
      ::sendCBMessage( CB_SHOWDROPDOWN, lFocus )
   ENDIF

   RETURN lOldFocus

METHOD WvgComboBox:sleSize()

   IF HB_ISOBJECT( ::oSLE )
      RETURN ::oSLE:currentSize()
   ENDIF

   RETURN { 0, 0 }

METHOD WvgComboBox:listBoxSize()

   IF HB_ISOBJECT( ::oListBox )
      RETURN ::oListBox:currentSize()
   ENDIF

   RETURN { 0, 0 }

METHOD WvgComboBox:setIcon( nItem, cIcon )

   HB_SYMBOL_UNUSED( nItem )
   HB_SYMBOL_UNUSED( cIcon )

   RETURN Self

METHOD WvgComboBox:itemMarked( ... )

   LOCAL a_ := hb_AParams()

   IF Len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_itemMarked := a_[ 1 ]
   ELSEIF Len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_itemMarked )
      Eval( ::sl_itemMarked, NIL, NIL, Self )
   ENDIF

   RETURN Self

METHOD WvgComboBox:itemSelected( ... )

   LOCAL a_ := hb_AParams()

   IF Len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_itemSelected := a_[ 1 ]
   ELSEIF Len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_itemSelected )
      Eval( ::sl_itemSelected, NIL, NIL, Self )
   ENDIF

   RETURN Self

METHOD WvgComboBox:drawItem( ... )

   LOCAL a_ := hb_AParams()

   IF Len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_xbePDrawItem := a_[ 1 ]
   ELSEIF Len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_xbePDrawItem )
      Eval( ::sl_xbePDrawItem, a_[ 1 ], a_[ 2 ], Self )
   ENDIF

   RETURN Self
