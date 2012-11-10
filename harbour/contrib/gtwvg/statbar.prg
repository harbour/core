/*
 * $Id$
 */

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

/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                 Xbase++ xbpStatusBar Compatible Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                              25Nov2008
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgStatusBar  INHERIT  WvgWindow /* WvgActiveXControl */

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

   ::style       := WS_CHILD + WS_BORDER + SBARS_TOOLTIPS
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

   ::addItem( , , , , , - 1 )

   RETURN Self

METHOD WvgStatusBar:handleEvent( nMessage, aNM )

   LOCAL nHandled := 1
   LOCAL nObj, aNMH

   DO CASE

   CASE nMessage == HB_GTE_RESIZED
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN 0

   CASE nMessage == HB_GTE_COMMAND
      IF HB_ISBLOCK( ::sl_lbClick )
         Eval( ::sl_lbClick, NIL, NIL, self )
         RETURN 0
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      aNMH := Wvg_GetNMMouseInfo( aNM[ 2 ] )

      DO CASE

      CASE aNMH[ NMH_code ] == NM_CLICK

         IF HB_ISBLOCK( ::sl_lbClick )
            IF aNMH[ NMH_dwItemSpec ] >= 0
               nObj := aNMH[ NMH_dwItemSpec ] + 1

               Eval( ::sl_lbClick, ::aItems[ nObj ], NIL, Self )

            ENDIF

            nHandled := 0
         ENDIF

      ENDCASE

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

   RETURN nHandled

METHOD WvgStatusBar:destroy()

   LOCAL i, nItems

   IF ( nItems := Len( ::aItems ) ) > 0
      FOR i := 1 TO nItems

      NEXT
   ENDIF

   ::wvgWindow:destroy()

   RETURN NIL

METHOD WvgStatusBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgStatusBar:addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )

   LOCAL oPanel, lSuccess

   __defaultNIL( @nMode, 0 )

   HB_SYMBOL_UNUSED( xImage )
   HB_SYMBOL_UNUSED( cDLL )

   oPanel := WvgStatusBarPanel():new( cCaption, nStyle, cKey )
   oPanel:oParent := self

   oPanel:index := ::numItems + 1

   lSuccess := Wvg_StatusBarCreatePanel( ::hWnd, nMode )

   IF lSuccess
      AAdd( ::aItems, oPanel )
   ELSE
      RETURN NIL
   ENDIF

   RETURN oPanel

METHOD WvgStatusBar:delItem( nItemORcKey )

   LOCAL nIndex := 0

   IF HB_ISNUMERIC( nItemORcKey )
      nIndex := AScan( ::aItems, {| o | o:key == nItemORcKey } )
   ELSEIF HB_ISNUMERIC( nItemORcKey )
      nIndex := nItemORcKey
   ENDIF

   IF nIndex > 0
      /* Delete panel by window */
      hb_ADel( ::aItems, nIndex, .T. )
   ENDIF

   RETURN Self

METHOD WvgStatusBar:getItem( nItemORcKey )

   LOCAL nIndex := 0, oPanel

   IF HB_ISSTRING( nItemORcKey  )
      nIndex := AScan( ::aItems, {| o | o:key == nItemORcKey } )

   ELSEIF HB_ISNUMERIC(  nItemORcKey  )
      nIndex := nItemORcKey

   ENDIF

   IF nIndex > 0
      oPanel := ::aItems[ nIndex ]
   ENDIF

   RETURN oPanel

METHOD WvgStatusBar:clear()

   LOCAL i

   FOR i := 1 TO ::numItems
      /* Remove off window */

   NEXT

   ::aItems := {}

   RETURN Self

METHOD WvgStatusBar:panelClick( xParam )

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

METHOD WvgStatusBar:panelDblClick( xParam )

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbDblClick := xParam
   ENDIF

   RETURN Self

/*
 *       WvgToolbarButton() Class compatible with XbpToolbarButton()
 */
CREATE CLASS WvgStatusBarPanel

   VAR    alignment                             INIT WVGALIGN_LEFT
   VAR    autosize                              INIT WVGSTATUSBAR_AUTOSIZE_NONE
   VAR    bevel                                 INIT WVGSTATUSBAR_BEVEL_INSET
   VAR    enabled                               INIT .T.
   VAR    index                                 INIT 0
   VAR    key                                   INIT ""
   VAR    style                                 INIT WVGSTATUSBAR_PANEL_TEXT
   VAR    sl_caption                            INIT ""
   VAR    image                                 INIT NIL
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

      Wvg_StatusBarSetText( ::oParent:hWnd, ::index, cCaption )
   ENDIF

   RETURN Self
