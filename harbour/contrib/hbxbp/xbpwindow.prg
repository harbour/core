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

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpWindow  INHERIT  XbpPartHandler

   CLASSDATA nProperty                            INIT  0
   DATA     qtProperty                            INIT  ""
   /* Called in the initializer - Unique in the application */
   METHOD   getProperty()                         INLINE  "PROP" + hb_ntos( ++::nProperty )
   /* After object is physically created, set unique property to 1 */
   METHOD   setQtProperty()

   DATA     cargo                                 INIT  ""
   DATA     styleSheet                            INIT  ""
   METHOD   setStyleSheet()

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
   DATA     lModal                                INIT   .f.

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

   METHOD   Initialize()

   DATA     nOldProc                              INIT   0
   DATA     nWndProc

   DATA     oMenu
   METHOD   HandleEvent()
   METHOD   grabEvent()

   METHOD   isDerivedFrom()

   DATA     oWidget
   ACCESS   pWidget                               INLINE  IF( empty( ::oWidget ), NIL, QT_PTROF( ::oWidget ) )
   ACCESS   pParent                               INLINE  IF( empty( ::oParent ), NIL, QT_PTROF( ::oParent:oWidget ) )
   DATA     oTabWidget
   DATA     aTabs                                 INIT  {}
   DATA     oPalette
   DATA     xDummy

   METHOD   connect()
   METHOD   connectEvent()
   METHOD   connectWindowEvents()
   DATA     aConnections                          INIT  {}
   DATA     aEConnections                         INIT  {}

   METHOD   setStyle()                            INLINE NIL

   DATA     lTrack                                INIT  .f.

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

   ::qtProperty  := ::getProperty()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL aPP, i, cClass := __objGetClsName( Self )

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

   aPP := Xbp_PresParam()
   FOR i := 1 TO len( ::aPresParams )
      Xbp_SetPresParam( aPP, ::aPresParams[ i,1 ], ::aPresParams[ i,2 ] )
   NEXT
   ::aPresParams := aPP

   DO CASE
   CASE cClass $ 'XBPDIALOG,XBPDRAWINGAREA'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_DIALOGBACKGROUND )
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_DIALOGBACKGROUND )
   CASE cClass $ 'XBPPUSHBUTTON'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_FGCLR         , XBPSYSCLR_BUTTONTEXT       )
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_BUTTONMIDDLE     )
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_BUTTONMIDDLE     )
   CASE cClass $ 'XBPTABPAGE'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_BUTTONMIDDLE     )
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_BUTTONMIDDLE     )
   CASE cClass $ 'XBPLISTBOX'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_ENTRYFIELD       )
   CASE cClass $ 'XBPSCROLLBAR'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_SCROLLBAR        )
   CASE cClass $ 'XBPSLE,XBPMLE'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_ENTRYFIELD       )
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_3DFACE           )
   CASE cClass $ 'XBPSPINBUTTON,XBPCOMBOBOX,XBPTREEVIEW'
      Xbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_ENTRYFIELD       )
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setQtProperty( cProperty )
   LOCAL oVariant := QVariant():new()

   DEFAULT cProperty TO "YES"

   oVariant:setValue( cProperty )

   ::oWidget:setProperty( ::qtProperty, QT_PTROF( oVariant ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:connect( pWidget, cSignal, bBlock )
   LOCAL lSuccess

   IF ( lSuccess := Qt_Connect_Signal( pWidget, cSignal, bBlock ) )
      aadd( ::aConnections, { pWidget, cSignal } )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpWindow:connectEvent( pWidget, nEvent, bBlock )
   LOCAL lSuccess

   IF ( lSuccess := Qt_Connect_Event( pWidget, nEvent, bBlock ) )
      aadd( ::aEConnections, { pWidget, nEvent } )
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpWindow:connectWindowEvents()

   //::oWidget:installEventFilter( SetEventFilter() )

   ::connectEvent( ::pWidget, QEvent_MouseMove          , {|o,e| ::grabEvent( QEvent_MouseMove          , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_MouseButtonPress   , {|o,e| ::grabEvent( QEvent_MouseButtonPress   , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_MouseButtonRelease , {|o,e| ::grabEvent( QEvent_MouseButtonRelease , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_MouseButtonDblClick, {|o,e| ::grabEvent( QEvent_MouseButtonDblClick, e, o ) } )
   ::connectEvent( ::pWidget, QEvent_Enter              , {|o,e| ::grabEvent( QEvent_Enter              , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_Leave              , {|o,e| ::grabEvent( QEvent_Leave              , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_Wheel              , {|o,e| ::grabEvent( QEvent_Wheel              , e, o ) } )
   //
   ::connectEvent( ::pWidget, QEvent_Move               , {|o,e| ::grabEvent( QEvent_Move               , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_Paint              , {|o,e| ::grabEvent( QEvent_Paint              , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_Resize             , {|o,e| ::grabEvent( QEvent_Resize             , e, o ) } )
   //
   ::connectEvent( ::pWidget, QEvent_FocusIn            , {|o,e| ::grabEvent( QEvent_FocusIn            , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_FocusOut           , {|o,e| ::grabEvent( QEvent_FocusOut           , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_DragEnter          , {|o,e| ::grabEvent( QEvent_DragEnter          , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_DragLeave          , {|o,e| ::grabEvent( QEvent_DragLeave          , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_DragMove           , {|o,e| ::grabEvent( QEvent_DragMove           , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_Drop               , {|o,e| ::grabEvent( QEvent_Drop               , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_WhatsThis          , {|o,e| ::grabEvent( QEvent_WhatsThis          , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_KeyPress           , {|o,e| ::grabEvent( QEvent_KeyPress           , e, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/
/*
:quit()
*/
METHOD XbpWindow:grabEvent( nEvent, pEvent, oXbp )
   LOCAL oEvent, nXbpKey, oP0, oP1, oObj_O, oObj_N
   LOCAL lRet := .t.

   HB_SYMBOL_UNUSED( oXbp )

   SWITCH ( nEvent )

   CASE QEvent_MouseMove                     // :motion()
      oEvent      := QMouseEvent():configure( pEvent )
      SetAppEvent( xbeM_Motion, { oEvent:x(), oEvent:y() }, NIL, self )
      EXIT
   CASE QEvent_MouseButtonPress              // :lbClick() :mbClick() :rbClick()
                                             // :lbDown() :mbDown() :rbDown()
      oEvent      := QMouseEvent():configure( pEvent )
      DO CASE
      CASE oEvent:button() == Qt_LeftButton
         SetAppEvent( xbeM_LbDown, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_MidButton
         SetAppEvent( xbeM_MbDown, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_RightButton
         SetAppEvent( xbeM_RbDown, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDCASE
      EXIT
   CASE QEvent_MouseButtonRelease            // :mbUp() :rbUp() :lbUp()
      oEvent      := QMouseEvent():configure( pEvent )
      DO CASE
      CASE oEvent:button() == Qt_LeftButton
         SetAppEvent( xbeM_LbUp, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_MidButton
         SetAppEvent( xbeM_MbUp, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_RightButton
         SetAppEvent( xbeM_RbUp, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDCASE
      EXIT
   CASE QEvent_MouseButtonDblClick           // :lbDblClick() :mbDblClick() :rbDblClick()
      oEvent      := QMouseEvent():configure( pEvent )
      DO CASE
      CASE oEvent:button() == Qt_LeftButton
         SetAppEvent( xbeM_LbDblClick, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_MidButton
         SetAppEvent( xbeM_MbDblClick, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_RightButton
         SetAppEvent( xbeM_RbDblClick, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDCASE
      EXIT
   CASE QEvent_Enter                         // :enter()
      IF ( ::lTrack )
         oEvent      := QMouseEvent():configure( pEvent )
         SetAppEvent( xbeM_Enter, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDIF
      lRet := .f.
      EXIT
   CASE QEvent_Leave                         // :leave()
      IF ( ::lTrack )
         oEvent      := QMouseEvent():configure( pEvent )
         SetAppEvent( xbeM_Leave, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDIF
      lRet := .f.
      EXIT
   CASE QEvent_Wheel                         // :wheel()
      oEvent      := QWheelEvent():configure( pEvent )
      SetAppEvent( xbeM_Wheel, { oEvent:x(), oEvent:y() }, { oEvent:buttons(), oEvent:delta() }, self )
      EXIT
   CASE QEvent_FocusIn                       // :setInputFocus()
      SetAppEvent( xbeP_SetInputFocus, NIL, NIL, Self )
      lRet := .f.
      EXIT
   CASE QEvent_FocusOut                      // :killInputFocus()
      SetAppEvent( xbeP_KillInputFocus, NIL, NIL, Self )
      lRet := .f.
      EXIT
   CASE QEvent_Paint                         // :paint()
      oEvent      := QPaintEvent():configure( pEvent )
      oObj_N      := QRect():configure( oEvent:rect() )
      SetAppEvent( xbeP_Paint, { oObj_N:left(), oObj_N:top(), oObj_N:right(), oObj_N:bottom() }, NIL, Self )
      lRet := .f.
      EXIT
   CASE QEvent_Move                          // :move()
      oEvent      := QMoveEvent():configure( pEvent )
      oP0         := QPoint():configure( oEvent:oldPos() )
      oP1         := QPoint():configure( oEvent:pos() )
      SetAppEvent( xbeP_Move, { oP0:x(), oP0:y() }, { oP1:x(), oP1:y() }, Self )
      EXIT
   CASE QEvent_Resize                        // :resize()
      oEvent      := QResizeEvent():configure( pEvent )
      oObj_O      := QSize():configure( oEvent:oldSize() )
      oObj_N      := QSize():configure( oEvent:size() )
      SetAppEvent( xbeP_Resize, { oObj_O:width(), oObj_O:height() }, { oObj_N:width(), oObj_N:height() }, Self )
      EXIT
   CASE QEvent_DragEnter                     // :dragEnter()
      oEvent      := QDragEnterEvent():configure( pEvent )
      SetAppEvent( xbeP_DragEnter, { oEvent:mouseButtons(), { oEvent:pos():x(), oEvent:pos():y() } }, /* oDragObj */, Self )
      EXIT
   CASE QEvent_DragLeave                     // :dragLeave()
      SetAppEvent( xbeP_DragLeave, NIL, NIL, Self )
      EXIT
   CASE QEvent_DragMove                      // :dragMotion()
      oEvent      := QDragEnterEvent():configure( pEvent )
      SetAppEvent( xbeP_DragMotion, { oEvent:mouseButtons(), { oEvent:pos():x(), oEvent:pos():y() } }, NIL, Self )
      EXIT
   CASE QEvent_Drop                          // :dragDrop()
      oEvent      := QDragEnterEvent():configure( pEvent )
      SetAppEvent( xbeP_DragDrop, { oEvent:mouseButtons(), { oEvent:pos():x(), oEvent:pos():y() } }, /* oDragObj */, Self )
      EXIT
   CASE QEvent_WhatsThis                     // :helpRequest()
      SetAppEvent( xbeP_HelpRequest, NIL, NIL, Self )
      EXIT
   CASE QEvent_KeyPress                      // :keyBoard()
      nXbpkey := XbpQKeyEventToAppEvent( pEvent )
      SetAppEvent( xbeP_Keyboard, nXbpKey, NIL, self )
      EXIT
   END SWITCH

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD XbpWindow:handleEvent( nEvent, mp1, mp2 )

   SWITCH ( nEvent )

   CASE xbeM_Motion
      ::motion( mp1, mp2 )
      EXIT
   CASE xbeM_Enter
      ::enter( mp1, mp2 )
      EXIT
   CASE xbeM_Leave
      ::leave( mp1, mp2 )
      EXIT
   CASE xbeM_LbDown
      ::lbDown( mp1, mp2 )
      EXIT
   CASE xbeM_MbDown
      ::mbDown( mp1, mp2 )
      EXIT
   CASE xbeM_RbDown
      ::rbDown( mp1, mp2 )
      EXIT
   CASE xbeM_LbUp
      ::lbUp( mp1, mp2 )
      EXIT
   CASE xbeM_MbUp
      ::mbUp( mp1, mp2 )
      EXIT
   CASE xbeM_RbUp
      ::rbUp( mp1, mp2 )
      EXIT
   CASE xbeM_LbClick
      ::lbClick( mp1, mp2 )
      EXIT
   CASE xbeM_MbClick
      ::mbClick( mp1, mp2 )
      EXIT
   CASE xbeM_RbClick
      ::rbClick( mp1, mp2 )
      EXIT
   CASE xbeM_LbDblClick
      ::lbDblClick( mp1, mp2 )
      EXIT
   CASE xbeM_MbDblClick
      ::mbDblClick( mp1, mp2 )
      EXIT
   CASE xbeM_RbDblClick
      ::rbDblClick( mp1, mp2 )
      EXIT
   CASE xbeM_LbMotion
      ::lbMotion( mp1, mp2 )
      EXIT
   CASE xbeM_MbMotion
      ::mbMotion( mp1, mp2 )
      EXIT
   CASE xbeM_RbMotion
      ::rbMotion( mp1, mp2 )
      EXIT
   CASE xbeM_Wheel
      ::wheel( mp1, mp2 )
      EXIT
   CASE xbeP_Move
      ::move( mp1, mp2 )
      EXIT
   CASE xbeP_Resize
      ::resize( mp1, mp2 )
      EXIT
   CASE xbeP_Paint
      ::paint( mp1, mp2 )
      EXIT
   CASE xbeP_HelpRequest
      ::helpRequest( mp1, mp2 )
      EXIT
   CASE xbeP_Activate
      ::activate( mp1, mp2 )
      EXIT
   CASE xbeP_ItemSelected
      ::itemSelected( mp1, mp2 )
      EXIT
   CASE xbeP_SetInputFocus
      ::setInputFocus( mp1, mp2 )
      EXIT
   CASE xbeP_KillInputFocus
      ::killInputFocus( mp1, mp2 )
      EXIT
   CASE xbeP_SetDisplayFocus
      ::setDisplayFocus( mp1, mp2 )
      EXIT
   CASE xbeP_KillDisplayFocus
      ::killDisplayFocus( mp1, mp2 )
      EXIT
   CASE xbeP_ClipboardChange
      ::clipBoardChange( mp1, mp2 )
      EXIT
   CASE xbeP_Measure
      ::measure( mp1, mp2 )
      EXIT
   CASE xbeP_Draw
      ::draw( mp1, mp2 )
      EXIT
   CASE xbeP_ItemMarked
      ::itemMarked( mp1, mp2 )
      EXIT
   CASE xbeP_MeasureItem
      ::measureItem( mp1, mp2 )
      EXIT
   CASE xbeP_DrawItem
      ::drawItem( mp1, mp2 )
      EXIT
   CASE xbeP_DragEnter
      ::dragEnter( mp1, mp2 )
      EXIT
   CASE xbeP_DragMotion
      ::dragMotion( mp1, mp2 )
      EXIT
   CASE xbeP_DragLeave
      ::dragLeave( mp1, mp2 )
      EXIT
   CASE xbeP_DragDrop
      ::dragDrop( mp1, mp2 )
      EXIT

   END SWITCH

   RETURN nil

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

//xbp_Debug( "Destroy: "+pad(__ObjGetClsName( self ),12)+ IF(empty(::cargo),'',str(::cargo) ) )
#if 0
   IF Len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
      ::aChildren := {}
   ENDIF
#endif
   IF len( ::aConnections ) > 0
      aeval( ::aConnections, {|e_| Qt_DisConnect_Signal( e_[ 1 ], e_[ 2 ] ) } )
      ::aConnections := {}
   ENDIF

   IF len( ::aEConnections ) > 0
      aeval( ::aEConnections, {|e_| Qt_DisConnect_Event( e_[ 1 ], e_[ 2 ] ) } )
      ::aEConnections := {}
      ::oWidget:removeEventFilter( SetEventFilter() )
   ENDIF

   IF Len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
      ::aChildren := {}
   ENDIF

   ::oWidget:close()

//xbp_Debug( "          Destroy: "+pad(__ObjGetClsName( self ),12)+ IF(empty(::cargo),'',str(::cargo) ) )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpWindow:captureMouse( lCapture )
   LOCAL lSuccess := .f.

   IF hb_isLogical( lCapture )
      IF lCapture
         ::oWidget:grabMouse()
      ELSE
         ::oWidget:releaseMouse()
      ENDIF
      lSuccess := .t.           /* QT cannot determine if it succeeded */
   ENDIF

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpWindow:disable()

   ::oWidget:setDisabled( .t. )
   ::is_enabled := ::oWidget:isEnabled()

   RETURN ! ::is_enabled

/*----------------------------------------------------------------------*/

METHOD XbpWindow:enable()

   ::oWidget:setEnabled( .t. )
   ::is_enabled := ::oWidget:isEnabled()

   RETURN ::is_enabled

/*----------------------------------------------------------------------*/

METHOD XbpWindow:hide()

   IF hb_isObject( ::oWidget )
      ::oWidget:hide()
   ENDIF
   ::is_hidden := ::oWidget:isHidden()

   RETURN ::is_hidden

/*----------------------------------------------------------------------*/

METHOD XbpWindow:invalidateRect( aRect )

   HB_SYMBOL_UNUSED( aRect )
   // TODO:

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lockPS()

   // TODO:

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lockUpdate()

   // TODO:

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setStyleSheet( cNewSheet )
   LOCAL cSheet := ::oWidget:styleSheet()

   ::oWidget:setStyleSheet( cSheet + " " + cNewSheet )

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION Xbp_RgbToName( nRgb )
   LOCAL oColor := QColor():new( nRGB )
   LOCAL cName := oColor:name

   RETURN '#'+substr( cName,6 ) + substr( cName,4,2 ) + substr( cName,2,2 )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setColorBG( nRGB )
   LOCAL oldRGB, cName, cQTName

   cName  := Xbp_RgbToName( nRGB )

   IF hb_isNumeric( nRGB )
      oldRGB  := Xbp_SetPresParam( ::aPresParams, XBP_PP_BGCLR, nRGB )
      cQTName := Xbp_XbpToQTName( __ObjGetClsName( self ) )

      IF empty( cQTName )
         ::setStyleSheet( "background-color: "+ cName +";" )
      ELSE
         ::setStyleSheet( "background-color: "+ cName +";" )
//         ::setStyleSheet( cQTName +'['+ ::qtProperty +'="YES"] '+ "{ background-color: "+ cName +" ; }" )
      ENDIF
   ELSE
      oldRGB := Xbp_SetPresParam( ::aPresParams, XBP_PP_BGCLR )
   ENDIF

   RETURN oldRGB

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setColorFG( nRGB )
   LOCAL oldRGB, cName

   cName := Xbp_RgbToName( nRGB )

   IF hb_isNumeric( nRGB )
      oldRGB := Xbp_SetPresParam( ::aPresParams, XBP_PP_FGCLR, nRGB )
      ::setStyleSheet( "color: "+ cName +";" )
   ELSE
      oldRGB := Xbp_SetPresParam( ::aPresParams, XBP_PP_FGCLR )
   ENDIF

   RETURN oldRGB

   #if 0
   LOCAL cClass  := __ObjGetClsName( self )

   IF hb_isNumeric( nRGB )
      IF empty( ::oPalette )
         ::oPalette := QPalette()
         ::oPalette:pPtr := ::oWidget:palette()
      ENDIF

      DO CASE
      CASE cClass $ 'XBPPUSHBUTTON,XBPMENUBAR,XBPMENU,XBPTOOLBAR,XBPTABPAGE'
         ::oPalette:setColor( QPalette_ButtonText, QT_PTROF( QColor():new( nRGB ) ) )
      OTHERWISE
         ::oPalette:setColor( QPalette_Foreground, QT_PTROF( QColor():new( nRGB ) ) )
         ::oPalette:setColor( QPalette_Text      , QT_PTROF( QColor():new( nRGB ) ) )
      ENDCASE

      ::oWidget:setPalette( QT_PTROF( ::oPalette ) )
   ENDIF

   LOCAL oColor := QColor():new( nRGB )
   Xbp_SetPresParam( ::aPresParams, XBP_PP_FGCLR, nRGB )
   ::setStyleSheet( "color: "+ oColor:name +";" )
   RETURN Self
   #endif

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setFont( oFont )
   LOCAL cAttr := ""

   // TODO:
   //::oWidget:setFont( QT_PTROF( oFont:oWidget ) ) /* Works but need to be refined */

   IF oFont:bold .and. oFont:italic
      cAttr := "bolditalic"
   ELSEIF oFont:bold
      cAttr := "bold"
   ELSEIF oFont:italic
      cAttr := "italic"
   ENDIF

   ::setFontCompoundName( oFont:compoundName + " " + cAttr )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setFontCompoundName( xFont )
   LOCAL cOldFont, s, n, nPoint, cFont, cFace, cCSS
   LOCAL aAttr   := { "bolditalic", "italic", "bold" }
   LOCAL cAttr   := "normal"
   LOCAL cWeight := "normal"

   cOldFont := Xbp_SetPresParam( ::aPresParams, XBP_PP_COMPOUNDNAME )

   IF hb_isNumeric( cFont )

   ELSE
      IF !empty( xFont )
         cFont := xFont
         s := lower( cFont )
         n := ascan( aAttr, {|e| at( e, cFont ) > 0 } )
         IF n > 0
            cAttr := aAttr[ n ]
            n     := at( cAttr, s )
            cFont := substr( cFont, 1, n-1 )
         ENDIF
         IF ( n := at( ".", cFont ) ) > 0
            nPoint := val( substr( cFont,1,n-1 ) )
            cFont  := substr( cFont,n+1 )
         ELSE
            nPoint := 0
         ENDIF
         cFace := alltrim( cFont )

         Xbp_SetPresParam( ::aPresParams, XBP_PP_COMPOUNDNAME, xFont )

         IF cAttr == "bolditalic"
            cAttr   := "italic"
            cWeight := "bold"
         ENDIF
         IF cAttr == "bold"
            cAttr := "normal"
            cWeight := "bold"
         ENDIF

         cCSS := 'font-family: "'+ cFace + '"; font-style: ' + cAttr + '; font-size: ' + ;
                                    hb_ntos( nPoint ) + 'pt; font-weight: ' + cWeight + ';'
         ::setStyleSheet( cCSS )
      ENDIF
   ENDIF

   RETURN cOldFont

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setModalState( nState )

   DO CASE
   CASE nState == XBP_DISP_MODELESS
      ::oWidget:setWindowModality( Qt_NonModal )
   CASE nState == XBP_DISP_APPMODAL
      ::oWidget:setWindowModality( Qt_ApplicationModal )
   CASE nState == XBP_DISP_SYSMODAL
      // TODO:
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPointer( cDllName, xResID, nType )

   HB_SYMBOL_UNUSED( cDllName )

   DEFAULT nType TO XBPWINDOW_POINTERTYPE_POINTER

   DO CASE
   CASE nType == XBPWINDOW_POINTERTYPE_POINTER
      // TODO:

   CASE nType == XBPWINDOW_POINTERTYPE_SYSPOINTER
      DO CASE
      CASE xResID == XBPSTATIC_SYSICON_DEFAULT   // Default mouse pointer

      CASE xResID == XBPSTATIC_SYSICON_ARROW     // Normal arrow
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_ArrowCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_WAIT      // Hour glass or clock
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_WaitCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_MOVE      // Move the window
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_OpenHandCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZE      // Change size (all directions)
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SizeAllCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZENWSE  // Change size (North west-South east)
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SizeFDiagCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZENESW  // Change size (North east-South west)
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SizeBDiagCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZEWE    // Change size (West-East)
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SizeHorCursor ) ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZENS    // Change size (North-South)
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SizeVerCursor ) ) )

      /* Possible Harbour-QT extensions - #deines yet to be finalized */

      CASE xResID == Qt_UpArrowCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_UpArrowCursor ) ) )

      CASE xResID == Qt_CrossCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_CrossCursor ) ) )

      CASE xResID == Qt_IBeamCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_IBeamCursor ) ) )

      CASE xResID == Qt_BlankCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_BlankCursor ) ) )

      CASE xResID == Qt_SplitVCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SplitVCursor ) ) )

      CASE xResID == Qt_SplitHCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_SplitHCursor ) ) )

      CASE xResID == Qt_PointingHandCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_PointingHandCursor ) ) )

      CASE xResID == Qt_ForbiddenCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_ForbiddenCursor ) ) )

      CASE xResID == Qt_ClosedHandCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_ClosedHandCursor ) ) )

      CASE xResID == Qt_WhatsThisCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_WhatsThisCursor ) ) )

      CASE xResID == Qt_BusyCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_BusyCursor ) ) )

      CASE xResID == Qt_BitmapCursor
         ::oWidget:setCursor( QT_PTROF( QCursor():new( Qt_BitmapCursor ) ) )

      ENDCASE

   CASE nType == XBPWINDOW_POINTERTYPE_ICON
      IF valtype( xResID ) == "C"   // Harbour compatibility
         IF file( xResID )
            #if 0  /* The original image size - but in practice pointer should be proper sized */
            ::oWidget:setCursor( QT_PTROF( QCursor():new( "QPixmap", QT_PTROF( QPixmap():new( xResID ) ) ) ) )
            #else
            ::oWidget:setCursor( QT_PTROF( QCursor():new( "QPixmap", QPixmap():new( xResID ):scaled( 24,24 ) ) ) )
            #endif
         ENDIF
      ENDIF

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setTrackPointer( lTrack )
   LOCAL lRet := .f.

   IF hb_isLogical( lTrack )
      ::lTrack := lTrack
      lRet := .T.
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPos( aPos, lPaint )

   DEFAULT aPos TO ::aPos

   IF hb_isArray( aPos )
      DEFAULT lPaint TO .T.
      ::oWidget:move( aPos[ 1 ], aPos[ 2 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPosAndSize( aPos, aSize, lPaint )

   DEFAULT aPos  TO ::aPos
   DEFAULT aSize TO ::aSize

   IF hb_isArray( aPos ) .and. hb_isArray( aSize )
      DEFAULT lPaint TO .T.

      ::oWidget:resize( aSize[ 1 ], aSize[ 2 ] )
      ::oWidget:move( aPos[ 1 ], aPos[ 2 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setSize( aSize, lPaint )

   DEFAULT aSize TO ::aSize

   IF hb_isArray( aSize )
      DEFAULT lPaint TO .T.

      ::oWidget:resize( aSize[ 1 ], aSize[ 2 ] )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:isDerivedFrom( cClassORoObject )
   LOCAL lTrue := .f.
   LOCAL cCls := __ObjGetClsName( Self )

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

   // TODO:

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:toFront()

   // TODO:

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:unlockPS()

   // TODO:

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:winDevice()

   // TODO:

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPresParam( aPPNew )
   LOCAL i, aPP

   aPP := aclone( ::aPresParams )

   IF hb_isArray( aPPNew )
      FOR i := 1 TO len( aPPNew )
         Xbp_SetPresParam( ::aPresParams, aPPNew[ i,1 ], aPPNew[ i,2 ] )
      NEXT
   ENDIF

   // Build Style Sheet

   RETURN aPP

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
   LOCAL nState

   nState := ::oWidget:windowModality()

   IF hb_bitAnd( nState, Qt_NonModal ) == Qt_NonModal
      RETURN XBP_DISP_MODELESS
   ELSEIF hb_bitAnd( nState, Qt_ApplicationModal ) == Qt_ApplicationModal
      RETURN XBP_DISP_APPMODAL
   ELSE
      // TODO:  XBP_DISP_SYSMODAL
   ENDIF

   RETURN -1

/*----------------------------------------------------------------------*/

METHOD XbpWindow:hasInputFocus()

   RETURN ::oWidget:hasFocus()

/*----------------------------------------------------------------------*/
 *                           Callback Methods
/*----------------------------------------------------------------------*/

METHOD XbpWindow:enter( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_enter )
      eval( ::sl_enter, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_enter := xParam
   endif

   RETURN ::sl_enter

/*----------------------------------------------------------------------*/

METHOD XbpWindow:leave( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_leave )
      eval( ::sl_leave, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_leave := xParam
   endif

   RETURN ::sl_leave

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbClick )
      eval( ::sl_lbClick, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_lbClick := xParam
   endif

   RETURN ::sl_lbClick

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbDblClick( xParam )

   IF hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbDblClick )
      eval( ::sl_lbDblClick, xParam, NIL, Self )
      RETURN NIL
   ENDIF

   IF hb_isBlock( xParam )
      ::sl_lbDblClick := xParam
   ENDIF

   RETURN ::sl_lbDblClick

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbDown( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbDown )
      eval( ::sl_lbDown, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_lbDown := xParam
   endif

   RETURN ::sl_lbDown

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbUp( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_lbUp )
      eval( ::sl_lbUp, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbUp := xParam
   endif

   RETURN ::sl_lbUp

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbClick )
      eval( ::sl_mbClick, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_mbClick := xParam
   endif

   RETURN ::sl_mbClick

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbDblClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbDblClick )
      eval( ::sl_mbDblClick, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_mbDblClick := xParam
   endif

   RETURN ::sl_mbDblClick

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbDown( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbDown )
      eval( ::sl_mbDown, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_mbDown := xParam
   endif

   RETURN ::sl_mbDown

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbUp( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_mbUp )
      eval( ::sl_mbUp, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_mbUp := xParam
   endif

   RETURN ::sl_mbUp

/*----------------------------------------------------------------------*/

METHOD XbpWindow:motion( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_motion )
      eval( ::sl_motion, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_motion := xParam
   endif

   RETURN ::sl_motion

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbClick )
      eval( ::sl_rbClick, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_rbClick := xParam
   endif

   RETURN ::sl_rbClick

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbDblClick( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbDblClick )
      eval( ::sl_rbDblClick, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_rbDblClick := xParam
   endif

   RETURN ::sl_rbDblClick

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbDown( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbDown )
      eval( ::sl_rbDown, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_rbDown := xParam
   endif

   RETURN ::sl_rbDown

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbUp( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_rbUp )
      eval( ::sl_rbUp, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_rbUp := xParam
   endif

   RETURN ::sl_rbUp

/*----------------------------------------------------------------------*/

METHOD XbpWindow:wheel( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_wheel )
      eval( ::sl_wheel, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_wheel := xParam
   endif

   RETURN ::sl_wheel

/*----------------------------------------------------------------------*/
 *                           Other Messages
/*----------------------------------------------------------------------*/

METHOD XbpWindow:close( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_close )
      RETURN eval( ::sl_close, NIL, NIL, Self )               /* NOTE: */
   endif

   if hb_isBlock( xParam )
      ::sl_close := xParam
   endif

   RETURN ::sl_close

/*----------------------------------------------------------------------*/

METHOD XbpWindow:helpRequest( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_helpRequest )
      eval( ::sl_helpRequest, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_helpRequest := xParam
   endif

   RETURN ::sl_helpRequest

/*----------------------------------------------------------------------*/

METHOD XbpWindow:keyboard( xParam )

   if hb_isNumeric( xParam ) .and. hb_isBlock( ::sl_keyboard )
      eval( ::sl_keyboard, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_keyboard := xParam
   endif

   RETURN ::sl_keyboard

/*----------------------------------------------------------------------*/

METHOD XbpWindow:killDisplayFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_killDisplayFocus )
      eval( ::sl_killDisplayFocus, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_killDisplayFocus := xParam
   endif

   RETURN ::sl_killDisplayFocus

/*----------------------------------------------------------------------*/

METHOD XbpWindow:killInputFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_killInputFocus )
      eval( ::sl_killInputFocus, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_killInputFocus := xParam
   endif

   RETURN ::sl_killInputFocus

/*----------------------------------------------------------------------*/

METHOD XbpWindow:move( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_move )
      eval( ::sl_move, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_move := xParam
   endif

   RETURN ::sl_move

/*----------------------------------------------------------------------*/

METHOD XbpWindow:paint( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_paint )
      eval( ::sl_paint, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_paint := xParam
   endif

   RETURN ::sl_paint

/*----------------------------------------------------------------------*/

METHOD XbpWindow:quit( xParam, xParam1 )

   if hb_isNumeric( xParam ) .and. hb_isBlock( ::sl_quit )
      eval( ::sl_quit, xParam, xParam1, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_quit := xParam
   endif

   RETURN ::sl_quit

/*----------------------------------------------------------------------*/

METHOD XbpWindow:resize( xParam, xParam1 )

   if hb_isArray( xParam ) .and. hb_isArray( xParam1 ) .and. hb_isBlock( ::sl_resize )
      eval( ::sl_resize, xParam, xParam1, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_resize := xParam
   endif

   RETURN ::sl_resize

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setDisplayFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_setDisplayFocus )
      eval( ::sl_setDisplayFocus, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_setDisplayFocus := xParam
   endif

   RETURN ::sl_setDisplayFocus

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setInputFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_setInputFocus )
      eval( ::sl_setInputFocus, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_setInputFocus := xParam
   endif

   RETURN ::sl_setInputFocus

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragEnter( xParam, xParam1 )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_dragEnter )
      eval( ::sl_dragEnter, xParam, xParam1, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_dragEnter := xParam
   endif

   RETURN ::sl_dragEnter

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragMotion( xParam )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_dragMotion )
      eval( ::sl_dragMotion, xParam, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_dragMotion := xParam
   endif

   RETURN ::sl_dragMotion

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragLeave( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_dragLeave )
      eval( ::sl_dragLeave, NIL, NIL, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_dragLeave := xParam
   endif

   RETURN ::sl_dragLeave

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragDrop( xParam, xParam1 )

   if hb_isArray( xParam ) .and. hb_isBlock( ::sl_dragDrop )
      eval( ::sl_dragDrop, xParam, xParam1, Self )
      RETURN NIL
   endif

   if hb_isBlock( xParam )
      ::sl_dragDrop := xParam
   endif

   RETURN ::sl_dragDrop

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

   ::oWidget:setFocus()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:sendMessage()// nMessage, nlParam, nwParam )

   RETURN self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS XbpObject

   METHOD INIT
   METHOD HandleEvent()  INLINE ( HBXBP_EVENT_UNHANDLED )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpObject:INIT()
   RETURN Self

/*----------------------------------------------------------------------*/
#if 0           /* A Template */
METHOD xbpWindow:setStyle()
   LOCAL s := "", txt_:={}

   aadd( txt_, ' ' )
   aadd( txt_, ' ' )

   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( s )

   RETURN self
#endif
/*----------------------------------------------------------------------*/

STATIC FUNCTION Xbp_PresParam()
   LOCAL aPP := {}

   aadd( aPP, { XBP_PP_FGCLR              , NIL } )
   aadd( aPP, { XBP_PP_BGCLR              , NIL } )
   aadd( aPP, { XBP_PP_HILITE_FGCLR       , NIL } )
   aadd( aPP, { XBP_PP_HILITE_BGCLR       , NIL } )
   aadd( aPP, { XBP_PP_DISABLED_FGCLR     , NIL } )
   aadd( aPP, { XBP_PP_DISABLED_BGCLR     , NIL } )
   aadd( aPP, { XBP_PP_BORDER_CLR         , NIL } )
   aadd( aPP, { XBP_PP_COMPOUNDNAME       , NIL } )
   aadd( aPP, { XBP_PP_FONT               , NIL } )
   aadd( aPP, { XBP_PP_ACTIVE_CLR         , NIL } )
   aadd( aPP, { XBP_PP_INACTIVE_CLR       , NIL } )
   aadd( aPP, { XBP_PP_ACTIVETEXT_FGCLR   , NIL } )
   aadd( aPP, { XBP_PP_ACTIVETEXT_BGCLR   , NIL } )
   aadd( aPP, { XBP_PP_INACTIVETEXT_FGCLR , NIL } )
   aadd( aPP, { XBP_PP_INACTIVETEXT_BGCLR , NIL } )
   aadd( aPP, { XBP_PP_CAPTION            , NIL } )
   aadd( aPP, { XBP_PP_ALIGNMENT          , NIL } )
   aadd( aPP, { XBP_PP_ORIGIN             , NIL } )

   RETURN aPP

/*----------------------------------------------------------------------*/

FUNCTION Xbp_SetPresParamIfNil( aPP, nParam, xValue )
   LOCAL n

   IF xValue != NIL
      IF ( n := ascan( aPP, {|e_| e_[ 1 ] == nParam } ) ) > 0
         IF aPP[ n,2 ] == NIL
            aPP[ n,2 ] := xValue
         ENDIF
      ENDIF
   ELSE
      aadd( aPP, { nParam, xValue } )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Xbp_SetPresParam( aPP, nParam, xValue )
   LOCAL oldValue, n

   IF ( n := ascan( aPP, {|e_| e_[ 1 ] == nParam } ) ) > 0
      oldValue := aPP[ n,2 ]
      IF xValue != NIL
         aPP[ n,2 ] := xValue
      ENDIF
   ELSE
      aadd( aPP, { nParam, xValue } )
   ENDIF

   RETURN oldValue

/*----------------------------------------------------------------------*/
