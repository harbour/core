/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                               EkOnkar
//                         ( The LORD is ONE )
//
//                  Xbase++ xbpToolBar Compatible Class
//
//                  Pritpal Bedi <pritpal@vouchcac.com>
//                              23Nov2008
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include 'hbclass.ch'
#include 'common.ch'
#include 'hbgtinfo.ch'
#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'inkey.ch'

//----------------------------------------------------------------------//

CLASS WvgToolBar  INHERIT  WvgActiveXControl

   DATA     appearance
   DATA     style                                 INIT WVGTOOLBAR_STYLE_STANDARD
   DATA     allowCustomize                        INIT .T.
   DATA     enabled                               INIT .T.
   DATA     showToolTips                          INIT .T.
   DATA     borderStyle                           INIT WVGFRAME_NONE
   DATA     wrappable                             INIT .T.
   DATA     buttonWidth                           INIT 0
   DATA     buttonHeight                          INIT 0
   DATA     textAlign                             INIT WVGALIGN_BOTTOM
   DATA     imageWidth                            INIT 0
   DATA     imageHeight                           INIT 0
   DATA     transparentColor                      INIT 0

   DATA     aItems                                INIT {}

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   addItem()
   METHOD   delItem()
   METHOD   getItem()
   METHOD   numItems()                            INLINE Len( ::aItems )
   METHOD   clear()
   METHOD   customize()
   METHOD   loadImageSet()
   METHOD   saveToolbar()
   METHOD   restToolbar()
   METHOD   setPosAndSize()
   METHOD   setSize()

   DATA     sl_buttonClick
   DATA     sl_change
   DATA     sl_buttonMenuClick
   DATA     sl_buttonDropDown

   METHOD   buttonClick()                         SETGET
   METHOD   change()                              SETGET
   METHOD   buttonMenuClick()                     SETGET
   METHOD   buttonDropDown()                      SETGET


   ENDCLASS
//----------------------------------------------------------------------//

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgToolBar

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::WvgActiveXControl:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + TBSTYLE_FLAT + CCS_ADJUSTABLE //+ CCS_NODIVIDER    //+CCS_VERT
   ::exStyle     := TBSTYLE_EX_DOUBLEBUFFER
   ::className   := TOOLBARCLASSNAME
   ::objType     := objTypeToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgToolBar

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   IF ::visible
      ::style += WS_VISIBLE
   ENDIF
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

   IF ::showToolTips
      ::sendMessage( TB_SETMAXTEXTROWS, 0, 0 )
   ENDIF

   //::sendMessage( TB_SETEXTENDEDSTYLE, 0, TBSTYLE_EX_DOUBLEBUFFER )
   //::sendMessage( TB_SETPADDING      , 0, Win_MakeLParam( 3,3 ) )
   //::sendMessage( TB_SETLISTGAP, 4, 0 )  // vista

   IF ::visible
      ::show()
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgToolBar

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

//----------------------------------------------------------------------//

METHOD destroy() CLASS WvgToolBar
   LOCAL i, nItems

   IF ( nItems := ::numItems() ) > 0
      FOR i := 1 TO nItems
         IF ::aItems[ i ]:image <> NIL
            Win_DeleteObject( ::aItems[ i ]:image )
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey ) CLASS WvgToolBar
   LOCAL oBtn, hBitmap, cType

   HB_SYMBOL_UNUSED( xDisabledImage )
   HB_SYMBOL_UNUSED( xHotImage )
   HB_SYMBOL_UNUSED( cDLL )


   oBtn := WvgToolbarButton():new( cCaption, nStyle, cKey )

   oBtn:index   := ::numItems + 1
   oBtn:command := 100 + oBtn:index

   cType := valtype( xImage )

   DO CASE

   CASE cType == 'C'
      hBitmap = Wvg_PrepareBitmapFromFile( xImage, ::imageWidth, ::imageHeight, .t., ::hWnd )

      IF hBitmap <> 0
         oBtn:image := hBitmap
         Wvg_AddToolbarButton( ::hWnd, oBtn:image, oBtn:caption, oBtn:command, 1 )

      endif

   ENDCASE

   aadd( ::aItems, { oBtn:command, oBtn } )

   RETURN Self

//----------------------------------------------------------------------//

METHOD delItem() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD getItem() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD clear() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD customize() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD loadImageSet() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD saveToolbar() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD restToolbar() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD setPosAndSize() CLASS WvgToolBar

   RETURN Self

//----------------------------------------------------------------------//

METHOD setSize() CLASS WvgToolBar

   ::sendMessage( TB_AUTOSIZE, 0, 0 )

   RETURN Self

//----------------------------------------------------------------------//

METHOD buttonClick( xParam ) CLASS WvgToolBar

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonClick := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD change( xParam ) CLASS WvgToolBar

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_change := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD buttonMenuClick( xParam ) CLASS WvgToolBar

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonMenuClick := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD buttonDropDown( xParam ) CLASS WvgToolBar

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonDropDown := xParam
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//       WvgToolbarButton() Class compatible with XbpToolbarButton()
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

CLASS WvgToolbarButton

   DATA     enabled                               INIT .T.
   DATA     index                                 INIT 0
   DATA     key                                   INIT ''
   DATA     style                                 INIT WVGTOOLBAR_BUTTON_DEFAULT
   DATA     caption                               INIT ''
   DATA     image                                 INIT NIL
   DATA     disabledImage                         INIT NIL
   DATA     hotImage                              INIT NIL
   DATA     mixedState                            INIT .F.
   DATA     pressed                               INIT .F.
   DATA     visible                               INIT .T.
   DATA     left                                  INIT 0
   DATA     bottom                                INIT 0
   DATA     top                                   INIT 0
   DATA     width                                 INIT 0
   DATA     height                                INIT 0
   DATA     description                           INIT ''
   DATA     tooltipText                           INIT ''
   DATA     command                               INIT 0

   METHOD   new()

   ENDCLASS
//----------------------------------------------------------------------//
METHOD new( cCaption, nStyle, cKey ) CLASS WvgToolbarButton

   DEFAULT cCaption       TO ::caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::caption        := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self
//----------------------------------------------------------------------//
