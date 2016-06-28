/*
 * Xbase++ xbpStatusBar Compatible Class
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

/*                               EkOnkar
 *                         ( The LORD is ONE )
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgStatusBar INHERIT WvgWindow /* WvgActiveXControl */

   VAR    caption                               INIT ""
   VAR    sizeGrip                              INIT .T.

   VAR    aItems                                INIT {}

   METHOD numItems()                            INLINE Len( ::aItems )

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )
   METHOD delItem( nItemORcKey )
   METHOD getItem( nItemORcKey )
   METHOD clear()
   METHOD panelClick( xParam )                  SETGET
   METHOD panelDblClick( xParam )               SETGET

ENDCLASS

METHOD WvgStatusBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WIN_WS_CHILD + WIN_WS_BORDER + SBARS_TOOLTIPS
   ::className   := STATUSCLASSNAME
   ::objType     := objTypeStatusBar

   RETURN Self

METHOD WvgStatusBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::sizeGrip
      ::style += SBARS_SIZEGRIP
   ENDIF

   ::oParent:AddChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF

   ::addItem( , , , , , -1 )

   RETURN Self

METHOD WvgStatusBar:handleEvent( nMessage, aNM )

   LOCAL nHandled := EVENT_UNHANDLED
   LOCAL nObj, aNMH

   DO CASE

   CASE nMessage == HB_GTE_RESIZED
      ::sendMessage( WIN_WM_SIZE, 0, 0 )
      RETURN EVENT_HANDLED

   CASE nMessage == HB_GTE_COMMAND
      IF HB_ISEVALITEM( ::sl_lbClick )
         Eval( ::sl_lbClick, , , self )
         RETURN EVENT_HANDLED
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      aNMH := wvg_GetNMMouseInfo( aNM[ 2 ] )

      DO CASE
      CASE aNMH[ NMH_code ] == NM_CLICK

         IF HB_ISEVALITEM( ::sl_lbClick )
            IF aNMH[ NMH_dwItemSpec ] >= 0
               nObj := aNMH[ NMH_dwItemSpec ] + 1

               Eval( ::sl_lbClick, ::aItems[ nObj ], , Self )
            ENDIF

            nHandled := EVENT_HANDLED
         ENDIF

      ENDCASE

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         wapi_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF Empty( ::hBrushBG )
         RETURN wvg_GetCurrentBrush( aNM[ 1 ] )
      ELSE
         wapi_SetBkMode( aNM[ 1 ], WIN_TRANSPARENT )
         RETURN ::hBrushBG
      ENDIF

   ENDCASE

   RETURN nHandled

METHOD PROCEDURE WvgStatusBar:destroy()

   LOCAL i

   FOR EACH i IN ::aItems
      /* TOFIX: Why was this left empty? */
   NEXT

   ::wvgWindow:destroy()

   RETURN

METHOD WvgStatusBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgStatusBar:addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )

   LOCAL oPanel := WvgStatusBarPanel():new( cCaption, nStyle, cKey )

   oPanel:oParent := self
   oPanel:index := ::numItems + 1

   IF wvg_StatusBarCreatePanel( ::hWnd, hb_defaultValue( nMode, 0 ) )
      AAdd( ::aItems, oPanel )
      RETURN oPanel
   ENDIF

   HB_SYMBOL_UNUSED( xImage )
   HB_SYMBOL_UNUSED( cDLL )

   RETURN NIL

METHOD WvgStatusBar:delItem( nItemORcKey )

   LOCAL nIndex := 0

   DO CASE
   CASE HB_ISNUMERIC( nItemORcKey )
      nIndex := AScan( ::aItems, {| o | o:key == nItemORcKey } )
   CASE HB_ISNUMERIC( nItemORcKey )
      nIndex := nItemORcKey
   ENDCASE

   IF nIndex >= 1 .AND. nIndex <= Len( ::aItems )
      hb_ADel( ::aItems, nIndex, .T. )  /* Delete panel by window */
   ENDIF

   RETURN Self

METHOD WvgStatusBar:getItem( nItemORcKey )

   LOCAL nIndex := 0

   DO CASE
   CASE HB_ISSTRING( nItemORcKey )
      nIndex := AScan( ::aItems, {| o | o:key == nItemORcKey } )
   CASE HB_ISNUMERIC( nItemORcKey )
      nIndex := nItemORcKey
   ENDCASE

   IF nIndex >= 1 .AND. nIndex <= Len( ::aItems )
      RETURN ::aItems[ nIndex ]
   ENDIF

   RETURN NIL

METHOD WvgStatusBar:clear()

   LOCAL i

   FOR i := 1 TO ::numItems
      /* Remove off window */
   NEXT

   ::aItems := {}

   RETURN Self

METHOD WvgStatusBar:panelClick( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

METHOD WvgStatusBar:panelDblClick( xParam )

   IF HB_ISEVALITEM( xParam ) .OR. xParam == NIL
      ::sl_lbDblClick := xParam
   ENDIF

   RETURN Self

/* WvgToolBarButton() Class compatible with XbpToolbarButton() */
CREATE CLASS WvgStatusBarPanel

   VAR    alignment                             INIT WVGALIGN_LEFT
   VAR    autosize                              INIT WVGSTATUSBAR_AUTOSIZE_NONE
   VAR    bevel                                 INIT WVGSTATUSBAR_BEVEL_INSET
   VAR    enabled                               INIT .T.
   VAR    index                                 INIT 0
   VAR    key                                   INIT ""
   VAR    style                                 INIT WVGSTATUSBAR_PANEL_TEXT
   VAR    sl_caption                            INIT ""
   VAR    image
   VAR    tooltipText                           INIT ""
   VAR    visible                               INIT .T.
   VAR    left                                  INIT 0
   VAR    width                                 INIT 0
   VAR    minWidth                              INIT 0

   METHOD new( cCaption, nStyle, cKey )
   METHOD caption( cCaption )                   SETGET

   VAR    oParent

ENDCLASS

METHOD WvgStatusBarPanel:new( cCaption, nStyle, cKey )

   __defaultNIL( @cCaption, ::sl_caption )
   __defaultNIL( @nStyle, ::style )
   __defaultNIL( @cKey, ::key )

   ::sl_caption     := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self

METHOD WvgStatusBarPanel:caption( cCaption )

   IF cCaption == NIL
      RETURN ::sl_caption
   ELSE
      __defaultNIL( @cCaption, ::sl_caption )

      ::sl_caption := cCaption

      wvg_StatusBarSetText( ::oParent:hWnd, ::index, cCaption )
   ENDIF

   RETURN Self
