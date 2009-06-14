/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ Compatible xbpWindow Class
 *
 *                 Pritpal Bedi  <pritpal@vouchcac.com>
 *                              29May2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "apig.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/
 * To Switch Over from ASCALLBACK() to SET/GET_Prop() calls
 *
#if 0
   #define __BYASCALLBACK__
#else
   #define __BYSETPROP__
#endif

#ifndef __DBG_PARTS__
   #xtranslate hb_ToOutDebug( [<x,...>] ) =>
#endif

/*----------------------------------------------------------------------*/

CLASS XbpWindow  INHERIT  XbpPartHandler

   DATA     oWidget
   ACCESS   pWidget                               INLINE  IF( empty( ::oWidget ), NIL, QT_PTROF( ::oWidget ) )

   /*  CONFIGURATION */
   DATA     animate                               INIT  .F.
   DATA     clipChildren                          INIT  .F.
   DATA     clipParent                            INIT  .F.
   DATA     clipSiblings                          INIT  .T.
   DATA     group                                 INIT  0    /* XBP_NO_GROUP */
   DATA     sizeRedraw                            INIT  .F.
   DATA     tabStop                               INIT  .F.
   DATA     visible                               INIT  .T.

   /*  RUNTIME DATA */
   DATA     dropZone                              INIT  .F.
   DATA     helpLink
   DATA     tooltipText                           INIT  ""

   DATA     clr_FG
   DATA     clr_BG
   DATA     fnt_COMMPOUNDNAME
   DATA     fnt_hFont

   /*  CALLBACK SLOTS */
   DATA     sl_enter
   DATA     sl_leave
   DATA     sl_lbClick
   DATA     sl_lbDblClick
   DATA     sl_lbDown
   DATA     sl_lbUp
   DATA     sl_mbClick
   DATA     sl_mbDblClick
   DATA     sl_mbDown
   DATA     sl_mbUp
   DATA     sl_motion
   DATA     sl_rbClick
   DATA     sl_rbDblClick
   DATA     sl_rbDown
   DATA     sl_rbUp
   DATA     sl_wheel

   DATA     sl_helpRequest
   DATA     sl_keyboard
   DATA     sl_killInputFocus
   DATA     sl_move
   DATA     sl_paint
   DATA     sl_quit
   DATA     sl_resize
   DATA     sl_setInputFocus
   DATA     sl_dragEnter
   DATA     sl_dragMotion
   DATA     sl_dragLeave
   DATA     sl_dragDrop

   DATA     sl_close
   DATA     sl_setDisplayFocus
   DATA     sl_killDisplayFocus

EXPORTED:
   /*  LIFE CYCLE  */
   METHOD   init()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   /*  MANIPULATE  */
   METHOD   captureMouse()
   METHOD   disable()
   METHOD   enable()
   METHOD   hide()
   METHOD   invalidateRect()
   METHOD   lockPS()
   METHOD   lockUpdate()
   METHOD   setModalState()
   METHOD   setPointer()
   METHOD   setTrackPointer()
   METHOD   setPos()
   METHOD   setPosAndSize()
   METHOD   setSize()
   METHOD   show()
   METHOD   toBack()
   METHOD   toFront()
   METHOD   unlockPS()
   METHOD   winDevice()

   /*  SETTINGS  */
   DATA     hBrushBG
   METHOD   setColorBG( nRGB )
   METHOD   setColorFG( nRGB )
   METHOD   setFont()
   METHOD   setFontCompoundName()
   METHOD   setPresParam()

   /*  STATUS  */
   METHOD   currentPos()
   METHOD   currentSize()
   METHOD   getHWND()
   METHOD   getModalState()
   METHOD   hasInputFocus()

   DATA     is_hidden                             INIT   .F.
   DATA     is_enabled                            INIT   .T.
   METHOD   isEnabled()                           INLINE ::is_enabled
   METHOD   isVisible()                           INLINE !( ::is_hidden )

   /*  CALLBACKS  */
   METHOD   enter()                               SETGET
   METHOD   leave()                               SETGET
   METHOD   lbClick()                             SETGET
   METHOD   lbDblClick()                          SETGET
   METHOD   lbDown()                              SETGET
   METHOD   lbUp()                                SETGET
   METHOD   mbClick()                             SETGET
   METHOD   mbDblClick()                          SETGET
   METHOD   mbDown()                              SETGET
   METHOD   mbUp()                                SETGET
   METHOD   motion()                              SETGET
   METHOD   rbClick()                             SETGET
   METHOD   rbDblClick()                          SETGET
   METHOD   rbDown()                              SETGET
   METHOD   rbUp()                                SETGET
   METHOD   wheel()                               SETGET

   /*  OTHER MESSAGES */
   METHOD   helpRequest()                         SETGET
   METHOD   keyboard()                            SETGET
   METHOD   killInputFocus()                      SETGET
   METHOD   move()                                SETGET
   METHOD   paint()                               SETGET
   METHOD   quit()                                SETGET
   METHOD   resize()                              SETGET
   METHOD   setInputFocus()                       SETGET
   METHOD   dragEnter()                           SETGET
   METHOD   dragMotion()                          SETGET
   METHOD   dragLeave()                           SETGET
   METHOD   dragDrop()                            SETGET

   METHOD   close()                               SETGET
   METHOD   setDisplayFocus()                     SETGET
   METHOD   killDisplayFocus()                    SETGET


   DATA     title                                 INIT   " "
   DATA     icon                                  INIT   0
   DATA     closable                              INIT   .T.
   DATA     resizable                             INIT   .t.
   DATA     resizeMode                            INIT   0
   DATA     style                                 INIT   QWS_OVERLAPPEDWINDOW
   DATA     exStyle                               INIT   0
   DATA     lModal                                INIT   .f.
   DATA     pGTp
   DATA     pGT
   DATA     objType                               INIT   objTypeNone
   DATA     className                             INIT   ""

   METHOD   setFocus()
   METHOD   sendMessage()

   DATA     hWnd
   DATA     pWnd
   DATA     aPos                                  INIT   { 0,0 }
   DATA     aSize                                 INIT   { 0,0 }
   DATA     aPresParams                           INIT   {}
   DATA     lHasInputFocus                        INIT   .F.
   DATA     nFrameState                           INIT   0       /* normal */

   DATA     maxCol                                INIT   79
   DATA     maxRow                                INIT   24
   DATA     mouseMode                             INIT   1

   DATA     nID                                   INIT   0
   DATA     nControlID                            INIT   5000
   METHOD   createControl()
   METHOD   getControlID()                        INLINE ++::nControlID

   METHOD   Initialize()

   DATA     nOldProc                              INIT   0
   DATA     nWndProc

   DATA     oMenu
   METHOD   HandleEvent()                         INLINE ( 1 )
   METHOD   ControlWndProc()
   METHOD   findObjectByHandle( hWnd )


   METHOD   isDerivedFrom()
   METHOD   setWindowProcCallback()

   METHOD   grabEvent()

   DATA     oPalette

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

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

   ::XbpPartHandler:init( oParent, oOwner )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

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

   ::XbpPartHandler:create( oParent, oOwner )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::sSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible


   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:destroy()
   #if 0
   hb_ToOutDebug( "          %s:destroy() XbpWindow()", __objGetClsName( self ) )
   #endif

   IF Len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
      ::aChildren := {}
   ENDIF

   #ifdef __BYSETPROP__
   QTG_ReleaseWindowProcBlock( ::pWnd )
   #endif

   IF Qtc_IsWindow( ::hWnd )
      Qtc_DestroyWindow( ::hWnd )
   ENDIF

   #ifdef __BYASCALLBACK__
   IF !empty( ::nWndProc )
      hb_FreeCallBack( ::nWndProc )
   ENDIF
   #endif

   IF ::hBrushBG <> NIL
      Qtc_DeleteObject( ::hBrushBG )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:grabEvent( pEvent )
   LOCAL oEvent, nType

   nType := Qt_QEvent_type( pEvent )

   DO CASE
   CASE nType == QEvent_MouseMove
      oEvent := QMouseEvent()
      oEvent:pPtr := pEvent
      SetAppEvent( xbeM_Motion, { oEvent:x(), oEvent:y() }, oEvent:button(), self )
      oEvent:setAccepted( .t. )

   CASE nType == QEvent_KeyPress
      oEvent := QKeyEvent()
      oEvent:pPtr := pEvent
      SetAppEvent( xbeP_Keyboard, oEvent:key, oEvent:text, self )
      oEvent:setAccepted( .t. )

   ENDCASE

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD XbpWindow:SetWindowProcCallback()

   #ifdef __BYSETPROP__
   ::nOldProc := QTG_SetWindowProcBlock( ::pWnd, {|h,m,w,l| ::ControlWndProc( h,m,w,l ) } )
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:captureMouse()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:disable()

   IF Qtc_EnableWindow( ::hWnd, .f. )
      ::is_enabled := .f.
      RETURN .t.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD XbpWindow:enable()

   IF Qtc_EnableWindow( ::hWnd, .t. )
      ::is_enabled := .t.
      RETURN .t.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD XbpWindow:hide()

   IF hb_isObject( ::oWidget )
      ::oWidget:hide()
      ::is_hidden := .t.
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:invalidateRect( aRect )

   RETURN Qtc_InvalidateRect( ::hWnd, aRect )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lockPS()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lockUpdate()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setColorBG( nRGB )
   LOCAL cClass

   IF hb_isNumeric( nRGB )
      IF empty( ::oPalette )
         ::oPalette := QPalette():new()
      ENDIF

      cClass := upper( ::ClassName )
      DO CASE
      CASE cClass $ 'XBPPUSHBUTTON,XBPMENUBAR,XBPMENU,XBPTOOLBAR'
         ::oPalette:setColor( QPalette_Button    , QT_PTROF( QColor():new( nRGB ) ) )
      OTHERWISE
         ::oPalette:setColor( QPalette_Background, QT_PTROF( QColor():new( nRGB ) ) )
      ENDCASE

      ::oWidget:setPalette( QT_PTROF( ::oPalette ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setColorFG( nRGB )
   LOCAL cClass

   IF hb_isNumeric( nRGB )
      IF empty( ::oPalette )
         ::oPalette := QPalette():new()
      ENDIF

      cClass := upper( ::ClassName )

      DO CASE
      CASE cClass $ 'XBPPUSHBUTTON,XBPMENUBAR,XBPMENU'
         ::oPalette:setColor( QPalette_ButtonText, QT_PTROF( QColor():new( nRGB ) ) )
      OTHERWISE
         ::oPalette:setColor( QPalette_Foreground, QT_PTROF( QColor():new( nRGB ) ) )
         ::oPalette:setColor( QPalette_Text      , QT_PTROF( QColor():new( nRGB ) ) )
      ENDCASE

      ::oWidget:setPalette( QT_PTROF( ::oPalette ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setModalState()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPointer()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setTrackPointer()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPos( aPos, lPaint )

   IF hb_isArray( aPos )
      DEFAULT lPaint TO .T.
      ::oWidget:move( aPos[ 1 ], aPos[ 2 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPosAndSize( aPos, aSize, lPaint )

   IF hb_isArray( aPos ) .and. hb_isArray( aSize )
      DEFAULT lPaint TO .T.

      ::oWidget:resize( aSize[ 1 ], aSize[ 2 ] )
      ::oWidget:move( aPos[ 1 ], aPos[ 2 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setSize( aSize, lPaint )

   IF hb_isArray( aSize )
      DEFAULT lPaint TO .T.

      ::oWidget:resize( aSize[ 1 ], aSize[ 2 ] )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:isDerivedFrom( cClassORoObject )
   LOCAL lTrue := .f.
   LOCAL cCls := __ObjGetClsName( self )

   /* Compares without Xbp or Wvg prefixes  */

   IF hb_isChar( cClassORoObject )
      IF upper( substr( cClassORoObject,4 ) ) == upper( substr( cCls,4 ) )
         lTrue := .t.
      ENDIF

   ELSEIF hb_isObject( cClassORoObject )
      IF upper( substr( cClassORoObject:className,4 ) ) == upper( substr( cCls,4 ) )
         lTrue := .t.
      ENDIF
   ENDIF

   RETURN lTrue

/*----------------------------------------------------------------------*/

METHOD XbpWindow:show()

   ::oWidget:show()
   ::is_hidden      := .f.
   ::lHasInputFocus := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:toBack()

   RETURN Qtc_SetWindowPosToBack( ::hWnd )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:toFront()

   /*RETURN Win_SetForegroundWindow( ::hWnd ) */
   RETURN Qtc_SetWindowPosToTop( ::hWnd )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:unlockPS()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:winDevice()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setFont()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setFontCompoundName( xFont )
   LOCAL cOldFont, s, n, nPoint, cFont, cAttr, cFace
   LOCAL aAttr := { "normal","italic","bold" }

   cOldFont := ::fnt_COMMPOUNDNAME

   IF hb_isNumeric( cFont )

   ELSE
      IF !empty( xFont )
         cFont := xFont
         s := lower( cFont )
         n := ascan( aAttr, {|e| at( e, cFont ) > 0 } )
         IF n > 0
            cAttr := aAttr[ n ]
            n := at( cAttr, s )
            cFont := substr( cFont,1,n-1 )
         ELSE
            cAttr := "normal"
         ENDIF

         IF ( n := at( ".", cFont ) ) > 0
            nPoint := val( substr( cFont,1,n-1 ) )
            cFont  := substr( cFont,n+1 )
         ELSE
            nPoint := 0
         ENDIF

         cFace := alltrim( cFont )

         HB_SYMBOL_UNUSED( cFace )
         HB_SYMBOL_UNUSED( cAttr )
         HB_SYMBOL_UNUSED( nPoint )
      ENDIF
   ENDIF

   RETURN cOldFont

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPresParam()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:currentPos()

   RETURN { ::oWidget:x(), ::oWidget:y() }

/*----------------------------------------------------------------------*/

METHOD XbpWindow:currentSize()

   RETURN { ::oWidget:width(), ::oWidget:height() }

/*----------------------------------------------------------------------*/

METHOD XbpWindow:getHWND()

   RETURN QT_PTROF( ::oWidget )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:getModalState()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:hasInputFocus()

   RETURN Self

/*----------------------------------------------------------------------*/
 *                           Callback Methods
/*----------------------------------------------------------------------*/

METHOD XbpWindow:enter( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_enter )
      eval( ::sl_enter, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_enter := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:leave( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_leave )
      eval( ::sl_leave, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_leave := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbClick )
      eval( ::sl_lbClick, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbClick := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbDblClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbDblClick )
      eval( ::sl_lbDblClick, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbDblClick := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbDown( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbDown )
      eval( ::sl_lbDown, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbDown := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbUp( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbUp )
      eval( ::sl_lbUp, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbUp := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbClick )
      eval( ::sl_mbClick, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_mbClick := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbDblClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbDblClick )
      eval( ::sl_mbDblClick, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_mbDblClick := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbDown( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbDown )
      eval( ::sl_mbDown, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_mbDown := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbUp( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbUp )
      eval( ::sl_mbUp, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_mbUp := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:motion( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_motion )
      eval( ::sl_motion, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_motion := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbClick )
      eval( ::sl_rbClick, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_rbClick := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbDblClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbDblClick )
      eval( ::sl_rbDblClick, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_rbDblClick := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbDown( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbDown )
      eval( ::sl_rbDown, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_rbDown := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbUp( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbUp )
      eval( ::sl_rbUp, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_rbUp := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:wheel( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_wheel )
      eval( ::sl_wheel, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_wheel := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/
 *                           Other Messages
/*----------------------------------------------------------------------*/

METHOD XbpWindow:close( xParam )
   if ::objType == objTypeCrt
      if hb_isNil( xParam ) .and. hb_isBlock( ::sl_close )
         eval( ::sl_close, NIL, NIL, Self )
         RETURN Self
      endif

      if hb_isBlock( xParam ) .or. hb_isNil( xParam )
         ::sl_close := xParam
         RETURN NIL
      endif
   endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:helpRequest( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_helpRequest )
      eval( ::sl_helpRequest, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_helpRequest := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:keyboard( xParam )

   if hb_isNumeric( xParam ) .and. hb_isBlock( ::sl_keyboard )
      eval( ::sl_keyboard, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_keyboard := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:killDisplayFocus( xParam )
   if ::objType == objTypeCrt
      if hb_isNil( xParam ) .and. hb_isBlock( ::sl_killDisplayFocus )
         eval( ::sl_killDisplayFocus, NIL, NIL, Self )
         RETURN Self
      endif

      if hb_isBlock( xParam ) .or. hb_isNil( xParam )
         ::sl_killDisplayFocus := xParam
         RETURN NIL
      endif
   endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:killInputFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_killInputFocus )
      eval( ::sl_killInputFocus, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_killInputFocus := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:move( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_move )
      eval( ::sl_move, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_move := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:paint( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_paint )
      eval( ::sl_paint, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_paint := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:quit( xParam, xParam1 )

   if hb_isNumeric( xParam ) .and. hb_isBlock( ::sl_quit )
      eval( ::sl_quit, xParam, xParam1, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_quit := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:resize( xParam, xParam1 )

   if hb_isArray( xParam ) .and. hb_isArray( xParam1 ) .and. hb_isBlock( ::sl_resize )
      eval( ::sl_resize, xParam, xParam1, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) /*.or. hb_isNil( xParam )*/
      ::sl_resize := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setDisplayFocus( xParam )

   if ::objType == objTypeCrt
      if hb_isNil( xParam ) .and. hb_isBlock( ::setDisplayFocus )
         eval( ::setDisplayFocus, NIL, NIL, Self )
         RETURN Self
      endif

      if hb_isBlock( xParam ) .or. hb_isNil( xParam )
         ::setDisplayFocus := xParam
         RETURN NIL
      endif
   endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setInputFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_setInputFocus )
      eval( ::sl_setInputFocus, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_setInputFocus := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragEnter( xParam, xParam1 )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_dragEnter )
      eval( ::sl_dragEnter, xParam, xParam1, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_dragEnter := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragMotion( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_dragMotion )
      eval( ::sl_dragMotion, xParam, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_dragMotion := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragLeave( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_dragLeave )
      eval( ::sl_dragLeave, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_dragLeave := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragDrop( xParam, xParam1 )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_dragDrop )
      eval( ::sl_dragDrop, xParam, xParam1, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_dragDrop := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

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

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setFocus()

   ::sendMessage( QWM_ACTIVATE, 1, 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:sendMessage( nMessage, nlParam, nwParam )

   RETURN Qtc_SendMessage( ::hWnd, nMessage, nlParam, nwParam )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:findObjectByHandle( hWnd )
   LOCAL nObj

   IF len( ::aChildren ) > 0
      IF ( nObj := ascan( ::aChildren, {|o| o:hWnd == hWnd } ) ) > 0
         RETURN ::aChildren[ nObj ]
      ENDIF
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpWindow:createControl()
   LOCAL hWnd

   ::nID := ::oParent:GetControlId()

   hWnd := Qtc_CreateWindowEx( ::exStyle, ;
                               ::className, ;
                               "", ;                              /* window name */
                               ::style, ;
                               ::aPos[ 1 ], ::aPos[ 2 ],;
                               ::aSize[ 1 ], ::aSize[ 2 ],;
                               ::oParent:hWnd,;
                               ::nID,;                            /* hMenu       */
                               NIL,;                              /* hInstance   */
                               NIL )                              /* lParam      */


   IF ( hWnd <> 0 )
      ::hWnd := hWnd
      ::pWnd := QTC_n2p( hWnd )
      ::sendMessage( QWM_SETFONT, Qtc_GetStockObject( QDEFAULT_GUI_FONT ), 1 )
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/

METHOD XbpWindow:ControlWndProc( hWnd, nMessage, nwParam, nlParam )
   LOCAL nCtrlID, nNotifctn, hWndCtrl, nObj, aMenuItem, oObj, nReturn

   #if 1
   hb_ToOutDebug( "%s:wndProc( %i  %i  %i  %i )", __ObjGetClsName( self ), hWnd, nMessage, nwParam, nlParam )
   #endif

   SWITCH nMessage

   CASE QWM_ERASEBKGND
      IF ::objType == objTypeDA .and. !empty( ::hBrushBG )
         ::handleEvent( HB_GTE_CTLCOLOR, { nwParam, nlParam } )
      ENDIF
      EXIT

   CASE QWM_COMMAND
      nCtrlID   := Qtc_LOWORD( nwParam )
      nNotifctn := Qtc_HIWORD( nwParam )
      hWndCtrl  := nlParam

      IF hWndCtrl == 0                            /* It is menu */
         IF hb_isObject( ::oMenu )
            IF !empty( aMenuItem := ::oMenu:FindMenuItemById( nCtrlID ) )
               IF hb_isBlock( aMenuItem[ 2 ] )
                  Eval( aMenuItem[ 2 ], aMenuItem[ 1 ], NIL, aMenuItem[ 4 ] )

               ELSEIF hb_isBlock( aMenuItem[ 3 ] )
                  Eval( aMenuItem[ 3 ], aMenuItem[ 1 ], NIL, aMenuItem[ 4 ] )

               ENDIF
            ENDIF
         ENDIF
         RETURN 0
      ELSE
         IF ( nObj := ascan( ::aChildren, {|o| o:nID == nCtrlID } ) ) > 0
            nReturn := ::aChildren[ nObj ]:handleEvent( HB_GTE_COMMAND, { nNotifctn, nCtrlID, hWndCtrl } )
            IF hb_isNumeric( nReturn ) .and. nReturn == 0
               RETURN 0
            ENDIF
         ENDIF
      ENDIF
      EXIT

   CASE QWM_NOTIFY
      IF ( nObj := ascan( ::aChildren, {| o | o:nID == nwParam } ) ) > 0
         nReturn := ::aChildren[ nObj ]:handleEvent( HB_GTE_NOTIFY, { nwParam, nlParam } )
         IF hb_isNumeric( nReturn ) .and. nReturn == EVENT_HANDELLED
            RETURN EVENT_HANDELLED
         ENDIF
      ENDIF
      EXIT

   CASE QWM_CTLCOLORLISTBOX
   CASE QWM_CTLCOLORMSGBOX
   CASE QWM_CTLCOLOREDIT
   CASE QWM_CTLCOLORBTN
   CASE QWM_CTLCOLORDLG
   CASE QWM_CTLCOLORSCROLLBAR
   CASE QWM_CTLCOLORSTATIC

      oObj := ::findObjectByHandle( nlParam )
      IF hb_isObject( oObj )
         nReturn := oObj:handleEvent( HB_GTE_CTLCOLOR, { nwParam, nlParam } )

         IF nReturn == 1
            RETURN Qtc_CallWindowProc( ::nOldProc, hWnd, nMessage, nwParam, nlParam )
         ELSE
            RETURN nReturn
         ENDIF
      ENDIF
      EXIT

   CASE QWM_HSCROLL
      ::handleEvent( HB_GTE_HSCROLL, { Qtc_LoWord( nwParam ), Qtc_HiWord( nwParam ), nlParam } )
      RETURN 0

   CASE QWM_VSCROLL
      nReturn := ::handleEvent( HB_GTE_VSCROLL, { Qtc_LoWord( nwParam ), Qtc_HiWord( nwParam ), nlParam } )
      IF nReturn == EVENT_HANDELLED
         RETURN 0
      ENDIF
      EXIT

   CASE QWM_CAPTURECHANGED
      EXIT
#if 0
   CASE QWM_MOUSEMOVE
      IF ::objType == objTypeScrollBar
         IF !( ::lTracking )
            ::lTracking := Wvg_BeginMouseTracking( ::hWnd )
         ENDIF
      ENDIF
      EXIT

   CASE QWM_MOUSEHOVER
      IF ::objType == objTypeScrollBar
         IF ::oParent:objType == objTypeCrt
            WAPI_SetFocus( ::oParent:hWnd )
         ENDIF
         RETURN 0
      ENDIF
      EXIT

   CASE QWM_MOUSELEAVE
      IF ::objType == objTypeScrollBar
         ::lTracking := .f.
         IF ::oParent:objType == objTypeCrt
            WAPI_SetFocus( ::oParent:hWnd )
         ENDIF
      ENDIF
      EXIT
#endif
   END

   RETURN Qtc_CallWindowProc( ::nOldProc, hWnd, nMessage, nwParam, nlParam )

/*----------------------------------------------------------------------*/

#if 0
FUNCTION hb_toOut( ... )
   RETURN hb_ToOutDebug( ... )
#endif

/*----------------------------------------------------------------------*/

CLASS XbpObject

   METHOD INIT
   METHOD HandleEvent()  INLINE ( 1 )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpObject:INIT()
   RETURN Self

/*----------------------------------------------------------------------*/







