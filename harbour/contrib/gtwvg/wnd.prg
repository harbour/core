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
 *                  Xbase++ Compatible xbpWindow Class
 *
 *                 Pritpal Bedi  <pritpal@vouchcac.com>
 *                              08Nov2008
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
// To Switch Over from ASCALLBACK() to SET/GET_Prop() calls
//
#if 0
#define __BYASCALLBACK__
#else
#define __BYSETPROP__
#endif

//

CREATE CLASS WvgWindow  INHERIT  WvgPartHandler

   /*  CONFIGURATION */
   VAR      animate                               INIT  .F.
   VAR      clipChildren                          INIT  .F.
   VAR      clipParent                            INIT  .F.
   VAR      clipSiblings                          INIT  .T.
   VAR      group                                 INIT  0    /* XBP_NO_GROUP */
   VAR      sizeRedraw                            INIT  .F.
   VAR      tabStop                               INIT  .F.
   VAR      visible                               INIT  .T.

   VAR      pointerFocus                          INIT  .T.

   /*  RUNTIME VAR  */
   VAR      dropZone                              INIT  .F.
   VAR      helpLink
   VAR      s_tooltipText                         INIT  ""
   METHOD   tooltipText                           SETGET

   VAR      clr_FG
   VAR      clr_BG
   VAR      fnt_COMMPOUNDNAME
   VAR      fnt_hFont

   /*  CALLBACK SLOTS */
   VAR      sl_enter
   VAR      sl_leave
   VAR      sl_lbClick
   VAR      sl_lbDblClick
   VAR      sl_lbDown
   VAR      sl_lbUp
   VAR      sl_mbClick
   VAR      sl_mbDblClick
   VAR      sl_mbDown
   VAR      sl_mbUp
   VAR      sl_motion
   VAR      sl_rbClick
   VAR      sl_rbDblClick
   VAR      sl_rbDown
   VAR      sl_rbUp
   VAR      sl_wheel

   VAR      sl_helpRequest
   VAR      sl_keyboard
   VAR      sl_killInputFocus
   VAR      sl_move
   VAR      sl_paint
   VAR      sl_quit
   VAR      sl_resize
   VAR      sl_setInputFocus
   VAR      sl_dragEnter
   VAR      sl_dragMotion
   VAR      sl_dragLeave
   VAR      sl_dragDrop

   VAR      sl_close
   VAR      sl_setDisplayFocus
   VAR      sl_killDisplayFocus

   VAR      hBrushBG
   VAR      is_hidden                             INIT   .F.
   VAR      is_enabled                            INIT   .T.
   VAR      title                                 INIT   " "
   VAR      icon                                  INIT   0
   VAR      closable                              INIT   .T.
   VAR      resizable                             INIT   .T.
   VAR      resizeMode                            INIT   0
   VAR      style                                 INIT   WS_OVERLAPPEDWINDOW
   VAR      exStyle                               INIT   0
   VAR      lModal                                INIT   .F.
   VAR      pGTp
   VAR      pGT
   VAR      objType                               INIT   objTypeNone
   VAR      className                             INIT   ""

   VAR      hWnd
   VAR      pWnd
   VAR      aPos                                  INIT   { 0, 0 }
   VAR      aSize                                 INIT   { 0, 0 }
   VAR      aPresParams                           INIT   {}
   VAR      lHasInputFocus                        INIT   .F.
   VAR      nFrameState                           INIT   0       /* normal */

   VAR      maxCol                                INIT   79
   VAR      maxRow                                INIT   24
   VAR      mouseMode                             INIT   1

   VAR      nID                                   INIT   0
   VAR      nControlID                            INIT   5000
   VAR      nOldProc                              INIT   0

   VAR      oMenu

   VAR      nTop
   VAR      nLeft
   VAR      nBottom
   VAR      nRight

   VAR      hWndTT

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   SetWindowProcCallback()

   METHOD   captureMouse()
   METHOD   currentPos()
   METHOD   currentSize()
   METHOD   disable()
   METHOD   enable()
   METHOD   getHWND()
   METHOD   getModalState()
   METHOD   hasInputFocus()
   METHOD   hide()
   METHOD   invalidateRect( aRect )
   METHOD   lockPS()
   METHOD   lockUpdate()
   METHOD   isDerivedFrom( cClassORoObject )
   METHOD   setColorBG( nRGB )
   METHOD   setModalState()
   METHOD   setPointer()
   METHOD   setTrackPointer()
   METHOD   setPos( aPos, lPaint )
   METHOD   setPosAndSize( aPos, aSize, lPaint )
   METHOD   setSize( aSize, lPaint )
   METHOD   setFont()
   METHOD   setFontCompoundName( xFont )
   METHOD   setPresParam()
   METHOD   show()
   METHOD   toBack()
   METHOD   toFront()
   METHOD   unlockPS()
   METHOD   winDevice()

   METHOD   Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   setFocus()
   METHOD   sendMessage( nMessage, nlParam, nwParam )
   METHOD   findObjectByHandle( hWnd )

   METHOD   getControlID()                        INLINE ++::nControlID
   METHOD   HandleEvent()                         INLINE EVENT_UNHANDELLED
   METHOD   isEnabled()                           INLINE ::is_enabled
   METHOD   isVisible()                           INLINE ! ::is_hidden
   METHOD   setColorFG( nRGB )                    INLINE ::clr_FG := iif( HB_ISSTRING( nRGB ), Wvt_GetRGBColorByString( nRGB, 0 ), nRGB ), ::invalidateRect()

   METHOD   enter( xParam )                       SETGET
   METHOD   leave( xParam )                       SETGET
   METHOD   lbClick( xParam )                     SETGET
   METHOD   lbDblClick( xParam )                  SETGET
   METHOD   lbDown( xParam )                      SETGET
   METHOD   lbUp( xParam )                        SETGET
   METHOD   mbClick( xParam )                     SETGET
   METHOD   mbDblClick( xParam )                  SETGET
   METHOD   mbDown( xParam )                      SETGET
   METHOD   mbUp( xParam )                        SETGET
   METHOD   motion( xParam )                      SETGET
   METHOD   rbClick( xParam )                     SETGET
   METHOD   rbDblClick( xParam )                  SETGET
   METHOD   rbDown( xParam )                      SETGET
   METHOD   rbUp( xParam )                        SETGET
   METHOD   wheel( xParam )                       SETGET
   METHOD   CLOSE( xParam )                       SETGET
   METHOD   helpRequest( xParam )                 SETGET
   METHOD   KEYBOARD( xParam )                    SETGET
   METHOD   killDisplayFocus( xParam )            SETGET
   METHOD   killInputFocus( xParam )              SETGET
   METHOD   move( xParam )                        SETGET
   METHOD   paint( xParam )                       SETGET
   METHOD   quit( xParam, xParam1 )               SETGET
   METHOD   resize( xParam, xParam1 )             SETGET
   METHOD   setDisplayFocus( xParam )             SETGET
   METHOD   setInputFocus( xParam )               SETGET
   METHOD   dragEnter( xParam, xParam1 )          SETGET
   METHOD   dragMotion( xParam )                  SETGET
   METHOD   dragLeave( xParam )                   SETGET
   METHOD   dragDrop( xParam, xParam1 )           SETGET
   PROTECTED:
   METHOD   getPosAndSize( aPs, aSz )
   METHOD   isParentCrt()                         INLINE ( ::oParent:objType == objTypeCrt )
   METHOD   rePosition()
   METHOD   createControl()

ENDCLASS

//

METHOD WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   __defaultNIL( @oParent    , ::oParent )
   __defaultNIL( @oOwner     , ::oOwner )
   __defaultNIL( @aPos       , ::aPos )
   __defaultNIL( @aSize      , ::aSize )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible   , ::visible )

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   ::WvgPartHandler:new( oParent, oOwner )

   RETURN Self

//

METHOD WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   __defaultNIL( @oParent    , ::oParent )
   __defaultNIL( @oOwner     , ::oOwner )
   __defaultNIL( @aPos       , ::aPos )
   __defaultNIL( @aSize      , ::aSize )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible   , ::visible )

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   IF Empty( ::oParent )
      IF ! ( __objGetClsName( Self ) $ "WVGCRT,WVGDIALOG" )
         ::oParent := WvgSetAppWindow()
      ENDIF
   ENDIF

   ::WvgPartHandler:create( oParent, oOwner )

   RETURN Self

//

METHOD WvgWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   __defaultNIL( @oParent    , ::oParent )
   __defaultNIL( @oOwner     , ::oOwner )
   __defaultNIL( @aPos       , ::aPos )
   __defaultNIL( @aSize      , ::aSize )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible   , ::visible )

   RETURN Self

//

METHOD WvgWindow:destroy()

   IF ! Empty( ::oParent )
      ::oParent:removeChild( Self )
   ENDIF

   IF Len( ::aChildren ) > 0
      AEval( ::aChildren, {| o | o:destroy() } )
      ::aChildren := {}
   ENDIF

   WVG_ReleaseWindowProcBlock( ::pWnd )

   IF WVG_IsWindow( ::hWndTT )
      WVG_DestroyWindow( ::hWndTT )
   ENDIF
   IF WVG_IsWindow( ::hWnd )
      WVG_DestroyWindow( ::hWnd )
   ENDIF

   IF ::hBrushBG != NIL
      WVG_DeleteObject( ::hBrushBG )
   ENDIF

   ::hWnd                   := NIL
   ::hWndTT                 := NIL
   ::pWnd                   := NIL
   ::aPos                   := NIL
   ::aSize                  := NIL
   ::aPresParams            := NIL
   ::lHasInputFocus         := NIL
   ::nFrameState            := NIL
   ::maxCol                 := NIL
   ::maxRow                 := NIL
   ::mouseMode              := NIL
   ::nID                    := NIL
   ::nControlID             := NIL
   ::nOldProc               := NIL
   ::oMenu                  := NIL
   ::animate                := NIL
   ::clipChildren           := NIL
   ::clipParent             := NIL
   ::clipSiblings           := NIL
   ::group                  := NIL
   ::sizeRedraw             := NIL
   ::tabStop                := NIL
   ::visible                := NIL
   ::dropZone               := NIL
   ::helpLink               := NIL
   ::tooltipText            := NIL
   ::clr_FG                 := NIL
   ::clr_BG                 := NIL
   ::fnt_COMMPOUNDNAME      := NIL
   ::fnt_hFont              := NIL
   ::sl_enter               := NIL
   ::sl_leave               := NIL
   ::sl_lbClick             := NIL
   ::sl_lbDblClick          := NIL
   ::sl_lbDown              := NIL
   ::sl_lbUp                := NIL
   ::sl_mbClick             := NIL
   ::sl_mbDblClick          := NIL
   ::sl_mbDown              := NIL
   ::sl_mbUp                := NIL
   ::sl_motion              := NIL
   ::sl_rbClick             := NIL
   ::sl_rbDblClick          := NIL
   ::sl_rbDown              := NIL
   ::sl_rbUp                := NIL
   ::sl_wheel               := NIL
   ::sl_helpRequest         := NIL
   ::sl_keyboard            := NIL
   ::sl_killInputFocus      := NIL
   ::sl_move                := NIL
   ::sl_paint               := NIL
   ::sl_quit                := NIL
   ::sl_resize              := NIL
   ::sl_setInputFocus       := NIL
   ::sl_dragEnter           := NIL
   ::sl_dragMotion          := NIL
   ::sl_dragLeave           := NIL
   ::sl_dragDrop            := NIL
   ::sl_close               := NIL
   ::sl_setDisplayFocus     := NIL
   ::sl_killDisplayFocus    := NIL

   RETURN NIL

//

METHOD WvgWindow:SetWindowProcCallback()

   ::nOldProc := WVG_SetWindowProcBlock( ::pWnd, {| h, m, w, l | ::ControlWndProc( h, m, w, l ) } )

   RETURN Self

//

METHOD WvgWindow:captureMouse()

   RETURN Self

//

METHOD WvgWindow:disable()

   IF WVG_EnableWindow( ::hWnd, .F. )
      ::is_enabled := .F.
      RETURN .T.
   ENDIF

   RETURN .F.

//

METHOD WvgWindow:enable()

   IF WVG_EnableWindow( ::hWnd, .T. )
      ::is_enabled := .T.
      RETURN .T.
   ENDIF

   RETURN .F.

//

METHOD WvgWindow:hide()

   IF WVG_IsWindow( ::hWnd )
      WVG_ShowWindow( ::hWnd, SW_HIDE )
      ::is_hidden := .T.
   ENDIF

   RETURN Self

//

METHOD WvgWindow:invalidateRect( aRect )

   RETURN WVG_InvalidateRect( ::hWnd, aRect )

//

METHOD WvgWindow:lockPS()

   RETURN Self

//

METHOD WvgWindow:lockUpdate()

   RETURN Self

//

METHOD WvgWindow:setColorBG( nRGB )

   LOCAL hBrush

   IF HB_ISSTRING( nRGB )
      nRGB := Wvt_GetRGBColorByString( nRGB, 1 )
   ENDIF
   IF HB_ISNUMERIC( nRGB )
      hBrush := WVG_CreateBrush( BS_SOLID, nRGB, 0 )
      IF hBrush != 0
         ::clr_BG := nRGB
         ::hBrushBG := hBrush

         IF ::className == "WVGDIALOG" .OR. __objGetClsName( Self ) == "WVGCHECKBOX"
            Wvg_SetCurrentBrush( ::hWnd, ::hBrushBG )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

//

METHOD WvgWindow:setModalState()

   RETURN Self

//

METHOD WvgWindow:setPointer()

   RETURN Self

//

METHOD WvgWindow:setTrackPointer()

   RETURN Self

//

METHOD WvgWindow:setPos( aPos, lPaint )

   LOCAL aPosSz

   IF HB_ISARRAY( aPos )
      __defaultNIL( @lPaint, .T. )

      SWITCH ::objType

      CASE objTypeCrt
         EXIT

      OTHERWISE
         aPosSz := ::getPosAndSize( aPos )
         WVG_SetWindowPosition( ::hWnd, aPosSz[ 1 ], aPosSz[ 2 ], lPaint )

      ENDSWITCH
   ENDIF

   RETURN Self

//
   /* This will always be called from HB_GTE_RESIZED message of WVG engine */

METHOD WvgWindow:rePosition()

   RETURN ::setPosAndSize( ::aPos, ::aSize )

//

METHOD WvgWindow:setPosAndSize( aPos, aSize, lPaint )

   LOCAL aPosSz

   __defaultNIL( @aPos , ::aPos )
   __defaultNIL( @aSize, ::aSize )

   IF HB_ISARRAY( aPos ) .AND. HB_ISARRAY( aSize )
      __defaultNIL( @lPaint, .T. )

      SWITCH ::objType

      CASE objTypeCrt
         EXIT

      OTHERWISE
         aPosSz := ::getPosAndSize( aPos, aSize )
         WVG_SetWindowPosAndSize( ::hWnd, aPosSz[ 1 ], aPosSz[ 2 ], aPosSz[ 3 ], aPosSz[ 4 ], lPaint )

      ENDSWITCH
   ENDIF

   RETURN Self

//

METHOD WvgWindow:setSize( aSize, lPaint )

   LOCAL aPosSz

   IF HB_ISARRAY( aSize )
      __defaultNIL( @lPaint, .T. )

      SWITCH ::objType

      CASE objTypeCrt
         EXIT

      OTHERWISE
         aPosSz := ::getPosAndSize( , aSize )
         WVG_SetWindowSize( ::hWnd, aPosSz[ 3 ], aPosSz[ 4 ], lPaint )

      ENDSWITCH
   ENDIF

   RETURN Self

//

METHOD WvgWindow:isDerivedFrom( cClassORoObject )

   LOCAL lTrue := .F.
   LOCAL cCls := __objGetClsName( self )

   /* Compares without Xbp or Wvg prefixes  */

   IF HB_ISSTRING( cClassORoObject )
      IF Upper( SubStr( cClassORoObject,4 ) ) == Upper( SubStr( cCls,4 ) )
         lTrue := .T.
      ENDIF

   ELSEIF HB_ISOBJECT( cClassORoObject )
      IF Upper( SubStr( cClassORoObject:className,4 ) ) == Upper( SubStr( cCls,4 ) )
         lTrue := .T.
      ENDIF
   ENDIF

   RETURN lTrue

//

METHOD WvgWindow:show()

   WVG_ShowWindow( ::hWnd, SW_NORMAL )
   ::is_hidden      := .F.
   ::lHasInputFocus := .T.

   RETURN Self

//

METHOD WvgWindow:toBack()

   RETURN WVG_SetWindowPosToBack( ::hWnd )

//

METHOD WvgWindow:toFront()

   /*RETURN WVG_SetForeGroundWindow( ::hWnd ) */

   RETURN WVG_SetWindowPosToTop( ::hWnd )

//

METHOD WvgWindow:unlockPS()

   RETURN Self

//

METHOD WvgWindow:winDevice()

   RETURN Self

//

METHOD WvgWindow:setFont()

   RETURN Self

//

METHOD WvgWindow:setFontCompoundName( xFont )

   LOCAL cOldFont, s, n, nPoint, cFont, cAttr, cFace
   LOCAL aAttr := { "normal", "italic", "bold" }

   cOldFont := ::fnt_COMMPOUNDNAME

   IF HB_ISNUMERIC( cFont )

   ELSE
      IF !Empty( xFont )
         cFont := xFont
         s := Lower( cFont )
         n := AScan( aAttr, {| e | At( e, cFont ) > 0 } )
         IF n > 0
            cAttr := aAttr[ n ]
            n := At( cAttr, s )
            cFont := SubStr( cFont, 1, n - 1 )
         ELSE
            cAttr := "normal"
         ENDIF

         IF ( n := At( ".", cFont ) ) > 0
            nPoint := Val( SubStr( cFont,1,n - 1 ) )
            cFont  := SubStr( cFont, n + 1 )
         ELSE
            nPoint := 0
         ENDIF

         cFace := AllTrim( cFont )

         HB_SYMBOL_UNUSED( cFace )
         HB_SYMBOL_UNUSED( cAttr )
         HB_SYMBOL_UNUSED( nPoint )
      ENDIF
   ENDIF

   RETURN cOldFont

//

METHOD WvgWindow:setPresParam()

   RETURN Self

//

METHOD WvgWindow:currentPos()

   LOCAL aRect := WVG_GetWindowRect( ::hWnd )

   RETURN { aRect[ 1 ], aRect[ 2 ] }

//

METHOD WvgWindow:currentSize()

   LOCAL aRect

   aRect := WVG_GetClientRect( ::hWnd )

   RETURN { aRect[ 3 ] - aRect[ 1 ], aRect[ 4 ] - aRect[ 2 ] }

//

METHOD WvgWindow:getHWND()

   RETURN ::hWnd

//

METHOD WvgWindow:getModalState()

   RETURN Self

//

METHOD WvgWindow:hasInputFocus()

   RETURN Self

//
//                           Callback Methods
//

METHOD WvgWindow:enter( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_enter )
      Eval( ::sl_enter, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_enter := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:leave( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_leave )
      Eval( ::sl_leave, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_leave := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:lbClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbClick )
      Eval( ::sl_lbClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:lbDblClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbDblClick )
      Eval( ::sl_lbDblClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbDblClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:lbDown( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbDown )
      Eval( ::sl_lbDown, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbDown := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:lbUp( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbUp )
      Eval( ::sl_lbUp, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbUp := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:mbClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbClick )
      Eval( ::sl_mbClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:mbDblClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbDblClick )
      Eval( ::sl_mbDblClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbDblClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:mbDown( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbDown )
      Eval( ::sl_mbDown, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbDown := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:mbUp( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbUp )
      Eval( ::sl_mbUp, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbUp := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:motion( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_motion )
      Eval( ::sl_motion, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_motion := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:rbClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbClick )
      Eval( ::sl_rbClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:rbDblClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbDblClick )
      Eval( ::sl_rbDblClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbDblClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:rbDown( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbDown )
      Eval( ::sl_rbDown, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbDown := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:rbUp( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbUp )
      Eval( ::sl_rbUp, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbUp := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:wheel( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_wheel )
      Eval( ::sl_wheel, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_wheel := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//
//                           Other Messages
//

METHOD WvgWindow:close( xParam )

   if ::objType == objTypeCrt
      IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_close )
         Eval( ::sl_close, NIL, NIL, Self )
         RETURN Self
      ENDIF

      IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
         ::sl_close := xParam
         RETURN NIL
      ENDIF
   ENDIF

   RETURN Self

//

METHOD WvgWindow:helpRequest( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_helpRequest )
      Eval( ::sl_helpRequest, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_helpRequest := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:keyboard( xParam )

   IF HB_ISNUMERIC( xParam ) .AND. HB_ISBLOCK( ::sl_keyboard )
      Eval( ::sl_keyboard, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_keyboard := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:killDisplayFocus( xParam )

   if ::objType == objTypeCrt
      IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_killDisplayFocus )
         Eval( ::sl_killDisplayFocus, NIL, NIL, Self )
         RETURN Self
      ENDIF

      IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
         ::sl_killDisplayFocus := xParam
         RETURN NIL
      ENDIF
   ENDIF

   RETURN Self

//

METHOD WvgWindow:killInputFocus( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_killInputFocus )
      Eval( ::sl_killInputFocus, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_killInputFocus := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:move( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_move )
      Eval( ::sl_move, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_move := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:paint( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_paint )
      Eval( ::sl_paint, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_paint := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:quit( xParam, xParam1 )

   IF HB_ISNUMERIC( xParam ) .AND. HB_ISBLOCK( ::sl_quit )
      Eval( ::sl_quit, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_quit := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:resize( xParam, xParam1 )

   IF HB_ISARRAY( xParam ) .AND. HB_ISARRAY( xParam1 ) .AND. HB_ISBLOCK( ::sl_resize )
      Eval( ::sl_resize, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) /*.or. HB_ISNIL( xParam )*/
      ::sl_resize := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:setDisplayFocus( xParam )

   if ::objType == objTypeCrt
      IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::setDisplayFocus )
         Eval( ::setDisplayFocus, NIL, NIL, Self )
         RETURN Self
      ENDIF

      IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
         ::setDisplayFocus := xParam
         RETURN NIL
      ENDIF
   ENDIF

   RETURN Self

//

METHOD WvgWindow:setInputFocus( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_setInputFocus )
      Eval( ::sl_setInputFocus, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_setInputFocus := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:dragEnter( xParam, xParam1 )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_dragEnter )
      Eval( ::sl_dragEnter, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragEnter := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:dragMotion( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_dragMotion )
      Eval( ::sl_dragMotion, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragMotion := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:dragLeave( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_dragLeave )
      Eval( ::sl_dragLeave, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragLeave := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:dragDrop( xParam, xParam1 )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_dragDrop )
      Eval( ::sl_dragDrop, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragDrop := xParam
      RETURN NIL
   ENDIF

   RETURN Self

//

METHOD WvgWindow:Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   __defaultNIL( @oParent    , ::oParent )
   __defaultNIL( @oOwner     , ::oOwner )
   __defaultNIL( @aPos       , ::aPos )
   __defaultNIL( @aSize      , ::aSize )
   __defaultNIL( @aPresParams, ::aPresParams )
   __defaultNIL( @lVisible   , ::visible )

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPresParams := aPresParams
   ::visible     := lVisible

   RETURN Self

//

METHOD WvgWindow:setFocus()

   WVG_SetFocus( ::hWnd )

   RETURN Self

//

METHOD WvgWindow:sendMessage( nMessage, nlParam, nwParam )

   RETURN WVG_SendMessage( ::hWnd, nMessage, nlParam, nwParam )

//

METHOD WvgWindow:findObjectByHandle( hWnd )

   LOCAL nObj

   IF Len( ::aChildren ) > 0
      IF ( nObj := AScan( ::aChildren, {| o | o:hWnd == hWnd } ) ) > 0
         RETURN ::aChildren[ nObj ]
      ENDIF
   ENDIF

   RETURN NIL

//

METHOD WvgWindow:getPosAndSize( aPs, aSz )

   LOCAL nX, nY, nW, nH, aXY
   LOCAL aPos, aSize
   LOCAL nFH, nFW

   __defaultNIL( @aPs, AClone( ::aPos  ) )
   __defaultNIL( @aSz, AClone( ::aSize ) )

   aPos  := AClone( aPs )
   aSize := AClone( aSz )

   IF ::isParentCrt()
      IF HB_ISBLOCK( aPos[ 1 ] )
         aPos[ 1 ] := Eval( aPos[ 1 ] )
      ENDIF
      IF HB_ISBLOCK( aPos[ 2 ] )
         aPos[ 2 ] := Eval( aPos[ 2 ] )
      ENDIF
      IF HB_ISBLOCK( aSize[ 1 ] )
         aSize[ 1 ] := Eval( aSize[ 1 ] )
      ENDIF
      IF HB_ISBLOCK( aSize[ 2 ] )
         aSize[ 2 ] := Eval( aSize[ 2 ] )
      ENDIF

      IF aPos[ 1 ] < 0 .AND. aPos[ 2 ] < 0 .AND. aSize[ 1 ] < 0 .AND. aSize[ 2 ] < 0
         nX := Abs( aPos[ 2 ] )
         IF nX < 1
            nX := 0
         ENDIF
         nY := Abs( aPos[ 1 ] )
         IF nY < 1
            nY := 0
         ENDIF
         nW := Abs( aSize[ 2 ] )
         IF nW < 1
            nW := 0
         ENDIF
         nH := Abs( aSize[ 1 ] )
         IF nH < 1
            nH := 0
         ENDIF
         aXY  := Wvt_GetXYFromRowCol( nY, nX )
         nFH := Wvt_GetFontInfo()[ 6 ]
         nFW := Wvt_GetFontInfo()[ 7 ]
         RETURN { aXY[ 1 ], aXY[ 2 ], nW * nFW, nH * nFH }
      ENDIF
   ENDIF

   RETURN { aPos[ 1 ], aPos[ 2 ], aSize[ 1 ], aSize[ 2 ] }

//

METHOD WvgWindow:toolTipText( cText )

   IF HB_ISSTRING( cText )
      ::s_toolTipText := cText
      IF WVG_IsWindow( ::hWndTT )
         WVG_SetTooltipText( ::hWnd, ::hWndTT, ::s_toolTipText )
      ENDIF
   ENDIF

   RETURN ::s_toolTipText

//

METHOD WvgWindow:createControl()

   LOCAL hWnd, aPosSz

   ::nID := ::oParent:GetControlId()

   aPosSz := ::getPosAndSize( ::aPos, ::aSize )

   hWnd := WVG_CreateWindowEx( ::exStyle, ;
      ::className, ;
      "", ;                              /* window name */
      ::style, ;
      aPosSz[ 1 ], aPosSz[ 2 ], ;
      aPosSz[ 3 ], aPosSz[ 4 ], ;
      ::oParent:hWnd, ;
      ::nID, ;                           /* hMenu       */
      NIL, ;                             /* hInstance   */
      NIL )                              /* lParam      */
   IF WVG_IsWindow( hWnd )
      ::hWnd := hWnd
      ::pWnd := WIN_N2P( hWnd )
      ::sendMessage( WM_SETFONT, WVG_GetStockObject( DEFAULT_GUI_FONT ), 1 )
      ::hWndTT := WVG_CreateTooltipWindow( ::hWnd )
   ENDIF

   RETURN Self

//
