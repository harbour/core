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

//
//
//
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ xbpToolBar Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              23Nov2008
 */
//
//
//

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

//

CREATE CLASS WvgToolBar  INHERIT  WvgWindow /*WvgActiveXControl*/

   VAR      appearance
   VAR      style                                 INIT WVGTOOLBAR_STYLE_STANDARD
   VAR      allowCustomize                        INIT .T.
   VAR      enabled                               INIT .T.
   VAR      showToolTips                          INIT .T.
   VAR      borderStyle                           INIT WVGFRAME_NONE
   VAR      wrappable                             INIT .T.
   VAR      buttonWidth                           INIT 0
   VAR      buttonHeight                          INIT 0
   VAR      textAlign                             INIT WVGALIGN_BOTTOM
   VAR      imageWidth                            INIT 0
   VAR      imageHeight                           INIT 0
   VAR      transparentColor                      INIT 0

   VAR      aItems                                INIT {}
   VAR      hImageList
   VAR      lSized                                INIT .F.

   VAR      sl_change
   VAR      sl_buttonMenuClick
   VAR      sl_buttonDropDown

   METHOD   numItems()                            INLINE Len( ::aItems )

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   handleEvent( nMessage, aNM )
   METHOD   destroy()
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   sendToolbarMessage( nMsg, p1, p2 )
   METHOD   addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey, nMapRGB )
   METHOD   delItem()

   METHOD   getItem()
   METHOD   CLEAR()
   METHOD   customize()
   METHOD   loadImageSet()
   METHOD   saveToolbar()
   METHOD   restToolbar()
   METHOD   setPosAndSize()
   METHOD   setSize()

   METHOD   buttonClick( xParam )                 SETGET
   METHOD   change( xParam )                      SETGET
   METHOD   buttonMenuClick( xParam )             SETGET
   METHOD   buttonDropDown( xParam )              SETGET

ENDCLASS

//

METHOD WvgToolBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

#if 0
   + TBSTYLE_LIST   caption TO the right, OTHERWISE caption TO the bottom
   ::style       := WS_CHILD + TBSTYLE_FLAT + CCS_ADJUSTABLE + CCS_NODIVIDER + CCS_VERT
#endif

   ::exStyle     := TBSTYLE_EX_DOUBLEBUFFER + TBSTYLE_EX_MIXEDBUTTONS
   ::className   := TOOLBARCLASSNAME
   ::objType     := objTypeToolBar

   RETURN Self

//

METHOD WvgToolBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::style == WVGTOOLBAR_STYLE_FLAT
      ::style := TBSTYLE_FLAT
   ELSEIF ::style == WVGTOOLBAR_STYLE_VERTICAL
      ::style := CCS_VERT
   ELSE
      ::style := 0
   ENDIF
   ::style += WS_CHILD

   IF ::wrappable
      ::style += TBSTYLE_WRAPABLE
   ENDIF
   IF ::showToolTips
      ::style += TBSTYLE_TOOLTIPS
   ENDIF
   IF ::borderStyle == WVGFRAME_RECT
      ::style += WS_BORDER
   ENDIF
#if 0
   IF ::appearance == WVG_APPEARANCE_3D
   ENDIF
#endif

   ::oParent:AddChild( SELF )

   ::createControl()

#if 0
   /* Should not be defined as we only require its notifications
    * so the parent of toolbar will process them anyway
    * All other functionality should be default until ownerdraw is introduced.
    */
   ::SetWindowProcCallback()
#endif

   IF ! Empty( ::hWnd )
      ::SendToolbarMessage( TB_BUTTONSTRUCTSIZE )
      ::hImageList := WAPI_ImageList_Create( ::imageWidth, ::imageHeight, ILC_COLOR32 + ILC_MASK, 0, 1 )
      ::SendToolbarMessage( TB_SETIMAGELIST, ::hImageList )

      ::SendToolbarMessage( TB_BUTTONSTRUCTSIZE )
      /* ::SendToolbarMessage( TB_SETINDENT, 10 ) */
   ENDIF

   IF ::visible
      ::show()
   ENDIF

   RETURN Self

//

METHOD WvgToolBar:handleEvent( nMessage, aNM )

   LOCAL nObj, aNMMouse

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN EVENT_HANDELLED

   CASE HB_GTE_COMMAND
      EXIT

   CASE HB_GTE_NOTIFY
      aNMMouse := Wvg_GetNMMouseInfo( aNM[ 2 ] )

      DO CASE

      CASE aNMMouse[ NMH_code ] == NM_CLICK
         IF ( nObj := AScan( ::aItems, {| e_ | e_[ 1 ] == aNMMouse[ NMH_dwItemSpec ] } ) ) > 0
            IF HB_ISBLOCK( ::sl_lbClick )
               IF ::isParentCrt()
                  ::oParent:setFocus()
               ENDIF
               Eval( ::sl_lbClick, ::aItems[ nObj, 2 ], NIL, Self )

            ENDIF
         ENDIF
         RETURN EVENT_HANDELLED

      OTHERWISE
         RETURN EVENT_UNHANDELLED

      ENDCASE

      EXIT
   ENDSWITCH

   RETURN EVENT_UNHANDELLED

//

METHOD WvgToolBar:destroy()

   LOCAL i, nItems

   IF ( nItems := Len( ::aItems ) ) > 0
      FOR i := 1 TO nItems
         IF ::aItems[ i, 2 ]:image != NIL
            WVG_DeleteObject( ::aItems[ i, 2 ]:image )
         ENDIF
         IF ::aItems[ i, 2 ]:disabledImage != NIL
            WVG_DeleteObject( ::aItems[ i, 2 ]:disabledImage )
         ENDIF
         IF ::aItems[ i, 2 ]:hotImage != NIL
            WVG_DeleteObject( ::aItems[ i, 2 ]:hotImage )
         ENDIF
      NEXT
   ENDIF

   IF ! Empty( ::hImageList )
      WAPI_ImageList_Destroy( ::hImageList )
   ENDIF

   ::wvgWindow:destroy()

   RETURN NIL

//

METHOD WvgToolBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

//

METHOD WvgToolBar:sendToolbarMessage( nMsg, p1, p2 )

   RETURN WVG_SendToolbarMessage( ::pWnd, nMsg, p1, p2 )

//

METHOD WvgToolBar:addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey, nMapRGB )

   LOCAL oBtn, pBitmap, nBtn

   HB_SYMBOL_UNUSED( xDisabledImage )
   HB_SYMBOL_UNUSED( xHotImage )
   HB_SYMBOL_UNUSED( cDLL )

   /* Issue this at the begining of first item */
   IF ! ::lSized
#if 0
      ::SendToolbarMessage( TB_SETBUTTONWIDTH, ::buttonWidth, ::buttonWidth )
#endif
      ::lSized := .T.
   ENDIF

   oBtn := WvgToolbarButton():new( cCaption, nStyle, cKey )

   oBtn:index   := ::numItems + 1
   oBtn:command := 100 + oBtn:index

   SWITCH ValType( xImage )
   CASE "C"
      IF "." $ xImage .OR. ;
         "/" $ xImage .OR. ;
         "\" $ xImage .OR. ;
         ":" $ xImage .OR. ;
         hb_FileExists( xImage )
         pBitmap := Wvg_PrepareBitmapFromFile( xImage, ::imageWidth, ::imageHeight, .T., ::hWnd )
      ELSE
         pBitmap := Wvg_PrepareBitmapFromResourceName( xImage, ::imageWidth, ::imageHeight, .T., ::hWnd )
      ENDIF
      EXIT

   CASE "N"
      pBitmap := Wvg_PrepareBitmapFromResourceID( xImage, ::imageWidth, ::imageHeight, .T., ::hWnd )
      EXIT

   CASE "P"
      pBitmap := xImage
      EXIT

   ENDSWITCH

   IF ! Empty( pBitmap )
      /* oBtn:image := pBitmap */

      IF HB_ISNUMERIC( nMapRGB )
         nBtn := WAPI_ImageList_AddMasked( ::hImageList, pBitmap, nMapRGB )
      ELSE
         nBtn := WAPI_ImageList_Add( ::hImageList, pBitmap )
      ENDIF
      IF ! HB_ISPOINTER( xImage )
         WVG_DeleteObject( pBitmap )
      ENDIF

      WVG_AddToolbarButton( ::pWnd, nBtn, oBtn:caption, oBtn:command, 1, ::showToolTips )

      /* Set Button Size */
      ::SendToolbarMessage( TB_SETBUTTONSIZE, ::buttonWidth, ::buttonHeight )

#if 0
      SendMessage( hWndTB, TB_SETPADDING, ( WPARAM ) 0, ( LPARAM ) MAKELPARAM(  10, 10 ) );
         ::sendToolbarMessage( TB_SETPADDING, 10, 10 )
#endif
      ::sendToolbarMessage( TB_AUTOSIZE )
   ELSE
      Wvg_AddToolbarButton( ::pWnd, , , oBtn:command, 3, .F. )

   ENDIF

   AAdd( ::aItems, { oBtn:command, oBtn } )

   RETURN oBtn

//

METHOD WvgToolBar:delItem()

   RETURN Self

//

METHOD WvgToolBar:getItem()

   RETURN Self

//

METHOD WvgToolBar:clear()

   RETURN Self

//

METHOD WvgToolBar:customize()

   RETURN Self

//

METHOD WvgToolBar:loadImageSet()

   RETURN Self

//

METHOD WvgToolBar:saveToolbar()

   RETURN Self

//

METHOD WvgToolBar:restToolbar()

   RETURN Self

//

METHOD WvgToolBar:setPosAndSize()

   RETURN Self

//

METHOD WvgToolBar:setSize()

   ::sendMessage( TB_AUTOSIZE, 0, 0 )

   RETURN Self

//

METHOD WvgToolBar:buttonClick( xParam )

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

//

METHOD WvgToolBar:change( xParam )

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_change := xParam
   ENDIF

   RETURN Self

//

METHOD WvgToolBar:buttonMenuClick( xParam )

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_buttonMenuClick := xParam
   ENDIF

   RETURN Self

//

METHOD WvgToolBar:buttonDropDown( xParam )

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_buttonDropDown := xParam
   ENDIF

   RETURN Self

//
//
//
/*
 *       WvgToolbarButton() Class compatible with XbpToolbarButton()
 */
//
//
//

CREATE CLASS WvgToolBarButton

   VAR      enabled                               INIT .T.
   VAR      INDEX                                 INIT 0
   VAR      KEY                                   INIT ""
   VAR      style                                 INIT WVGTOOLBAR_BUTTON_DEFAULT
   VAR      caption                               INIT ""
   VAR      image                                 INIT NIL
   VAR      disabledImage                         INIT NIL
   VAR      hotImage                              INIT NIL
   VAR      mixedState                            INIT .F.
   VAR      pressed                               INIT .F.
   VAR      visible                               INIT .T.
   VAR      left                                  INIT 0
   VAR      BOTTOM                                INIT 0
   VAR      TOP                                   INIT 0
   VAR      width                                 INIT 0
   VAR      height                                INIT 0
   VAR      description                           INIT ""
   VAR      tooltipText                           INIT ""
   VAR      command                               INIT 0

   METHOD   new( cCaption, nStyle, cKey )

ENDCLASS

//

METHOD WvgToolBarButton:new( cCaption, nStyle, cKey )

   __defaultNIL( @cCaption      , ::caption )
   __defaultNIL( @nStyle        , ::style )
   __defaultNIL( @cKey          , ::key )

   ::caption        := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self
