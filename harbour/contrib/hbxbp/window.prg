/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2008-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ Compatible xbpWindow Class
 *
 *                            Pritpal Bedi
 *                              29May2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "error.ch"
#include "inkey.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpWindow  INHERIT  XbpPartHandler

   CLASSDATA nProperty                            INIT   0

   DATA     qtProperty                            INIT   ""
   DATA     qtObject
   DATA     isViaQtObject                         INIT   .f.

   /*  CONFIGURATION */
   DATA     animate                               INIT   .F.
   DATA     clipChildren                          INIT   .F.
   DATA     clipParent                            INIT   .F.
   DATA     clipSiblings                          INIT   .T.
   DATA     group                                 INIT   XBP_NO_GROUP
   DATA     sizeRedraw                            INIT   .F.
   DATA     tabStop                               INIT   .F.
   DATA     visible                               INIT   .T.

   /*  RUNTIME DATA */
   DATA     dropZone                              INIT   .F.
   DATA     helpLink
   DATA     tooltipText                           INIT   ""

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

   DATA     hb_contextMenu
   DATA     hBrushBG
   DATA     is_hidden                             INIT   .F.
   DATA     is_enabled                            INIT   .T.
   DATA     icon                                  INIT   0
   DATA     closable                              INIT   .T.
   DATA     resizable                             INIT   .T.
   DATA     resizeMode                            INIT   0
   DATA     lModal                                INIT   .f.
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
   DATA     nOldProc                              INIT   0
   DATA     nWndProc
   DATA     oMenu
   DATA     oWidget
   DATA     cargo                                 INIT   ""
   DATA     styleSheet                            INIT   ""
   DATA     oTabWidget
   DATA     aTabs                                 INIT   {}
   DATA     oPalette
   DATA     xDummy
   DATA     aConnections                          INIT   {}
   DATA     aEConnections                         INIT   {}
   DATA     lTrack                                INIT   .f.
   DATA     aPP
   DATA     qLayout
   DATA     nLayout
   DATA     oFont
   DATA     aCSS                                  INIT   { { "", "" } }

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   setQtProperty( cProperty )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   grabEvent( nEvent, oEvent )
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   captureMouse( lCapture )
   METHOD   invalidateRect( aRect )
   METHOD   setFont( oFont )
   METHOD   setFontCompoundName( xFont )
   METHOD   setModalState( nState )
   METHOD   setPointer( cDllName, xResID, nType )
   METHOD   setTrackPointer( lTrack )
   METHOD   setPos( aPos, lPaint )
   METHOD   setPosAndSize( aPos, aSize, lPaint )
   METHOD   setSize( aSize, lPaint )
   METHOD   isDerivedFrom( cClassORoObject )
   METHOD   setPresParam( aPPNew )
   METHOD   setCSSAttribute( cAttr, cCSS )

   DATA     cTitle                                INIT    ""
   METHOD   title( cTitle )                       SETGET

   METHOD   enter( ... )                          SETGET
   METHOD   leave( ... )                          SETGET
   METHOD   lbClick( ... )                        SETGET
   METHOD   lbDblClick( ... )                     SETGET
   METHOD   lbDown( ... )                         SETGET
   METHOD   lbUp( ... )                           SETGET
   METHOD   mbClick( ... )                        SETGET
   METHOD   mbDblClick( ... )                     SETGET
   METHOD   mbDown( ... )                         SETGET
   METHOD   mbUp( ... )                           SETGET
   METHOD   motion( ... )                         SETGET
   METHOD   rbClick( ... )                        SETGET
   METHOD   rbDblClick( ... )                     SETGET
   METHOD   rbDown( ... )                         SETGET
   METHOD   rbUp( ... )                           SETGET
   METHOD   wheel( ... )                          SETGET
   METHOD   close( ... )                          SETGET
   METHOD   helpRequest( ... )                    SETGET
   METHOD   keyboard( ... )                       SETGET
   METHOD   killDisplayFocus( ... )               SETGET
   METHOD   killInputFocus( ... )                 SETGET
   METHOD   move( ... )                           SETGET
   METHOD   paint( ... )                          SETGET
   METHOD   quit( ... )                           SETGET
   METHOD   resize( ... )                         SETGET
   METHOD   setDisplayFocus( ... )                SETGET
   METHOD   setInputFocus( ... )                  SETGET
   METHOD   dragEnter( ... )                      SETGET
   METHOD   dragMotion( ... )                     SETGET
   METHOD   dragLeave( ... )                      SETGET
   METHOD   dragDrop( ... )                       SETGET
   METHOD   hbContextMenu( ... )                  SETGET

   ERROR    HANDLER OnError( ... )

   METHOD   Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   isEnabled()                           INLINE ::is_enabled
   METHOD   isVisible()                           INLINE !( ::is_hidden )
   METHOD   destroy()
   METHOD   disable()
   METHOD   enable()
   METHOD   hide()
   METHOD   lockPS()
   METHOD   lockUpdate()
   METHOD   show()
   METHOD   toBack()
   METHOD   toFront()
   METHOD   unlockPS()
   METHOD   winDevice()
   METHOD   setColorBG( nRGB )
   METHOD   setColorFG( nRGB )
   METHOD   currentPos()
   METHOD   currentSize()
   METHOD   getHWND()
   METHOD   getModalState()
   METHOD   hasInputFocus()
   METHOD   setFocus()
   METHOD   sendMessage()
   METHOD   connectWindowEvents()
   METHOD   disconnectWindowEvents()
   METHOD   clearSlots()

   /* Called in the initializer - Unique in the application */
   METHOD   getProperty()                         INLINE "PROP" + hb_ntos( ++::nProperty )

   METHOD   setStyle()                            INLINE NIL
   METHOD   className()                           INLINE __objGetClsName( Self )

   /* Harbour Extension */
   METHOD   hbLayout( nTypeLayout )               SETGET

   METHOD   postCreate()

   ACCESS   pParent                               INLINE iif( empty( ::oParent ), NIL, ::oParent:oWidget )

   PROTECTED:
   METHOD   setStyleSheet( cAttr, cCSS )
   METHOD   getCSS( nAttr, xValue )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO {}
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::aPP         := aclone( aPresParams )
   ::visible     := lVisible

   ::XbpPartHandler:init( oParent, oOwner )

   ::qtProperty  := ::getProperty()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL i, cClass := __objGetClsName( Self )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT lVisible    TO ::visible

   ::oParent     := oParent
   ::oOwner      := oOwner
   ::aPos        := aPos
   ::aSize       := aSize
   ::visible     := lVisible

   /* Important : 25 Nov 2009 */
   // DEFAULT ::oParent TO SetAppWindow()

   ::XbpPartHandler:create( oParent, oOwner )

   IF !empty( aPresParams )
      IF !empty( ::aPP )
         FOR i := 1 TO len( ::aPP )
            ::aPP[ i, 1 ] := NIL
            ::aPP[ i, 2 ] := NIL
            ::aPP[ i ]    := NIL
         NEXT
         ::aPP := NIL
      ENDIF
      ::aPP := aclone( aPresParams )
   ENDIF
   hbxbp_PresParam( @::aPresParams )
   IF !empty( ::aPP )
      FOR i := 1 TO len( ::aPP )
         hbxbp_SetPresParam( ::aPresParams, ::aPP[ i,1 ], ::aPP[ i,2 ] )
         ::aPP[ i,1 ] := NIL
         ::aPP[ i,2 ] := NIL
      NEXT
   ENDIF
   ::aPP := NIL

   /* Initialize CSS parameters */
   IF ( i := ascan( ::aPresParams, {|e_| e_[ 1 ] == XBP_PP_FGCLR } ) ) > 0
      IF ! ( ::aPresParams[ i, 2 ] == NIL )
         ::setCSSAttribute( "XBP_PP_FGCLR", ::getCSS( XBP_PP_FGCLR, ::aPresParams[ i, 2 ] ) )
      ENDIF
   ENDIF
   IF ( i := ascan( ::aPresParams, {|e_| e_[ 1 ] == XBP_PP_BGCLR } ) ) > 0
      IF ! ( ::aPresParams[ i, 2 ] == NIL )
         ::setCSSAttribute( "XBP_PP_BGCLR", ::getCSS( XBP_PP_BGCLR, ::aPresParams[ i, 2 ] ) )
      ENDIF
   ENDIF
   IF ( i := ascan( ::aPresParams, {|e_| e_[ 1 ] == XBP_PP_COMPOUNDNAME } ) ) > 0
      IF ! ( ::aPresParams[ i, 2 ] == NIL )
         ::setCSSAttribute( "XBP_PP_COMPOUNDNAME", ::getCSS( XBP_PP_COMPOUNDNAME, ::aPresParams[ i, 2 ] ) )
      ENDIF
   ENDIF

   DO CASE
   CASE cClass $ 'XBPDIALOG,XBPDRAWINGAREA'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_DIALOGBACKGROUND )
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_DIALOGBACKGROUND )
   CASE cClass $ 'XBPPUSHBUTTON'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_FGCLR         , XBPSYSCLR_BUTTONTEXT       )
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_BUTTONMIDDLE     )
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_BUTTONMIDDLE     )
   CASE cClass $ 'XBPTABPAGE'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_BUTTONMIDDLE     )
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_BUTTONMIDDLE     )
   CASE cClass $ 'XBPLISTBOX'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_ENTRYFIELD       )
   CASE cClass $ 'XBPSCROLLBAR'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_SCROLLBAR        )
   CASE cClass $ 'XBPSLE,XBPMLE'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_ENTRYFIELD       )
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_DISABLED_BGCLR, XBPSYSCLR_3DFACE           )
   CASE cClass $ 'XBPSPINBUTTON,XBPCOMBOBOX,XBPTREEVIEW'
      hbxbp_SetPresParamIfNil( ::aPresParams, XBP_PP_BGCLR         , XBPSYSCLR_ENTRYFIELD       )
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DEFAULT oParent     TO ::oParent
   DEFAULT oOwner      TO ::oOwner
   DEFAULT aPos        TO ::aPos
   DEFAULT aSize       TO ::aSize
   DEFAULT aPresParams TO ::aPresParams
   DEFAULT lVisible    TO ::visible

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setQtProperty( cProperty )
   HB_SYMBOL_UNUSED( cProperty )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:postCreate()

   ::status := iif( ! empty( ::oWidget ), XBP_STAT_CREATE, XBP_STAT_FAILURE )

   IF ! empty( ::toolTipText ) .AND. HB_ISSTRING( ::toolTipText )
      ::oWidget:setTooltip( ::toolTipText )
   ENDIF

   ::setStyleSheet()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:connectWindowEvents()

   ::oWidget:connect( QEvent_MouseButtonPress   , {|e| ::grabEvent( QEvent_MouseButtonPress   , e ) } )
   ::oWidget:connect( QEvent_MouseButtonRelease , {|e| ::grabEvent( QEvent_MouseButtonRelease , e ) } )
   ::oWidget:connect( QEvent_MouseMove          , {|e| ::grabEvent( QEvent_MouseMove          , e ) } )
   ::oWidget:connect( QEvent_MouseButtonDblClick, {|e| ::grabEvent( QEvent_MouseButtonDblClick, e ) } )
   ::oWidget:connect( QEvent_Enter              , {|e| ::grabEvent( QEvent_Enter              , e ) } )
   ::oWidget:connect( QEvent_Leave              , {|e| ::grabEvent( QEvent_Leave              , e ) } )
   ::oWidget:connect( QEvent_Wheel              , {|e| ::grabEvent( QEvent_Wheel              , e ) } )

   ::oWidget:connect( QEvent_FocusIn            , {|e| ::grabEvent( QEvent_FocusIn            , e ) } )
   ::oWidget:connect( QEvent_FocusOut           , {|e| ::grabEvent( QEvent_FocusOut           , e ) } )
   ::oWidget:connect( QEvent_DragEnter          , {|e| ::grabEvent( QEvent_DragEnter          , e ) } )
   ::oWidget:connect( QEvent_DragLeave          , {|e| ::grabEvent( QEvent_DragLeave          , e ) } )
   ::oWidget:connect( QEvent_DragMove           , {|e| ::grabEvent( QEvent_DragMove           , e ) } )
   ::oWidget:connect( QEvent_Drop               , {|e| ::grabEvent( QEvent_Drop               , e ) } )
   ::oWidget:connect( QEvent_WhatsThis          , {|e| ::grabEvent( QEvent_WhatsThis          , e ) } )
   ::oWidget:connect( QEvent_KeyPress           , {|e| ::grabEvent( QEvent_KeyPress           , e ) } )

   ::oWidget:connect( QEvent_ContextMenu        , {|e| ::grabEvent( QEvent_ContextMenu        , e ) } )

   ::oWidget:connect( QEvent_Move               , {|e| ::grabEvent( QEvent_Move               , e ) } )
*  ::oWidget:connect( QEvent_Paint              , {|e| ::grabEvent( QEvent_Paint              , e ) } )
*  ::oWidget:connect( QEvent_Resize             , {|e| ::grabEvent( QEvent_Resize             , e ) } )

      RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:disconnectWindowEvents()

   ::oWidget:disconnect( QEvent_MouseButtonPress    )
   ::oWidget:disconnect( QEvent_MouseButtonRelease  )
   ::oWidget:disconnect( QEvent_MouseMove           )
   ::oWidget:disconnect( QEvent_MouseButtonDblClick )
   ::oWidget:disconnect( QEvent_Enter               )
   ::oWidget:disconnect( QEvent_Leave               )
   ::oWidget:disconnect( QEvent_Wheel               )

   ::oWidget:disconnect( QEvent_FocusIn             )
   ::oWidget:disconnect( QEvent_FocusOut            )
   ::oWidget:disconnect( QEvent_DragEnter           )
   ::oWidget:disconnect( QEvent_DragLeave           )
   ::oWidget:disconnect( QEvent_DragMove            )
   ::oWidget:disconnect( QEvent_Drop                )
   ::oWidget:disconnect( QEvent_WhatsThis           )
   ::oWidget:disconnect( QEvent_KeyPress            )

   ::oWidget:disconnect( QEvent_ContextMenu         )

   ::oWidget:disconnect( QEvent_Move                )
*  ::oWidget:disconnect( QEvent_Paint               )
*  ::oWidget:disconnect( QEvent_Resize              )

      RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:destroy()

   ::aPresParams := NIL

   IF !empty( ::oOwner )
      ::oOwner:delOwned( Self )  /* Not a public function */
   ENDIF

   ::oParent := NIL
   ::oOwner  := NIL

   IF !empty( ::oTabWidget )
      ::oTabWidget := NIL
   ENDIF

   IF ! empty( ::aChildren )
      aeval( ::aChildren, {|o| o:destroy() } )
      ::aChildren := {}
   ENDIF

   ::XbpPartHandler:destroy()

   IF !empty( ::qtObject )
      ::qtObject:destroy()
   ENDIF

   IF !empty( ::qLayout )
      ::qLayout := NIL
   ENDIF

   ::oWidget := NIL
   HB_TRACE( HB_TR_DEBUG, "DESTROYED", __objGetClsName( Self ) )
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpWindow:clearSlots()
   LOCAL i

   ::sl_enter              := NIL
   ::sl_leave              := NIL
   ::sl_lbClick            := NIL
   ::sl_lbDblClick         := NIL
   ::sl_lbDown             := NIL
   ::sl_lbUp               := NIL
   ::sl_mbClick            := NIL
   ::sl_mbDblClick         := NIL
   ::sl_mbDown             := NIL
   ::sl_mbUp               := NIL
   ::sl_motion             := NIL
   ::sl_rbClick            := NIL
   ::sl_rbDblClick         := NIL
   ::sl_rbDown             := NIL
   ::sl_rbUp               := NIL
   ::sl_wheel              := NIL

   ::sl_helpRequest        := NIL
   ::sl_keyboard           := NIL
   ::sl_killInputFocus     := NIL
   ::sl_move               := NIL
   ::sl_paint              := NIL
   ::sl_quit               := NIL
   ::sl_resize             := NIL
   ::sl_setInputFocus      := NIL
   ::sl_dragEnter          := NIL
   ::sl_dragMotion         := NIL
   ::sl_dragLeave          := NIL
   ::sl_dragDrop           := NIL

   ::sl_close              := NIL
   ::sl_setDisplayFocus    := NIL
   ::sl_killDisplayFocus   := NIL

   IF !empty( ::aPresParams )
      FOR i := 1 TO len( ::aPresParams )
         ::aPresParams[ i,1 ] := NIL
         ::aPresParams[ i,2 ] := NIL
         ::aPresParams[ i ]   := NIL
      NEXT
   ENDIF
   ::aPresParams           := NIL
   ::aPresParams           := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:grabEvent( nEvent, oEvent )
   LOCAL nXbpKey, oP0, oP1, oObj_O, oObj_N

   HB_TRACE( HB_TR_DEBUG, nEvent, valtype( oEvent ), __objGetClsName( oEvent ) )

   SWITCH ( nEvent )

   CASE QEvent_MouseMove                     // :motion()
      SetAppEvent( xbeM_Motion, { oEvent:x(), oEvent:y() }, NIL, self )
      RETURN .f.
   CASE QEvent_MouseButtonPress              // :lbClick() :mbClick() :rbClick()
                                             // :lbDown() :mbDown() :rbDown()
      DO CASE
      CASE oEvent:button() == Qt_LeftButton
         SetAppEvent( xbeM_LbDown, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_MidButton
         SetAppEvent( xbeM_MbDown, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_RightButton
         SetAppEvent( xbeM_RbDown, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDCASE
      RETURN .f.
   CASE QEvent_MouseButtonRelease            // :mbUp() :rbUp() :lbUp()
      DO CASE
      CASE oEvent:button() == Qt_LeftButton
         SetAppEvent( xbeM_LbUp, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_MidButton
         SetAppEvent( xbeM_MbUp, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_RightButton
         SetAppEvent( xbeM_RbUp, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDCASE
      RETURN .f.
   CASE QEvent_MouseButtonDblClick           // :lbDblClick() :mbDblClick() :rbDblClick()
      DO CASE
      CASE oEvent:button() == Qt_LeftButton
         SetAppEvent( xbeM_LbDblClick, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_MidButton
         SetAppEvent( xbeM_MbDblClick, { oEvent:x(), oEvent:y() }, NIL, self )
      CASE oEvent:button() == Qt_RightButton
         SetAppEvent( xbeM_RbDblClick, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDCASE
      RETURN .f.
   CASE QEvent_ContextMenu                   //
      //SetAppEvent( xbeM_Context, { oEvent:globalX(), oEvent:globalY() }, NIL, self )
      ::hbContextMenu( { oEvent:globalX(), oEvent:globalY() } )
      EXIT
   CASE QEvent_Enter                         // :enter()
      IF ( ::lTrack )
         SetAppEvent( xbeM_Enter, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDIF
      RETURN .f.
   CASE QEvent_Leave                         // :leave()
      IF ( ::lTrack )
         SetAppEvent( xbeM_Leave, { oEvent:x(), oEvent:y() }, NIL, self )
      ENDIF
      RETURN .f.
   CASE QEvent_Wheel                         // :wheel()
      SetAppEvent( xbeM_Wheel, { oEvent:x(), oEvent:y() }, { oEvent:buttons(), oEvent:delta() }, self )
      RETURN .f.
   CASE QEvent_FocusIn                       // :setInputFocus()
      SetAppEvent( xbeP_SetInputFocus, NIL, NIL, Self )
      RETURN .f.
   CASE QEvent_FocusOut                      // :killInputFocus()
      SetAppEvent( xbeP_KillInputFocus, NIL, NIL, Self )
      RETURN .f.
   CASE QEvent_Paint                         // :paint()
      oObj_N      := oEvent:rect()
      SetAppEvent( xbeP_Paint, { oObj_N:left(), oObj_N:top(), oObj_N:right(), oObj_N:bottom() }, NIL, Self )
      RETURN .f.
   CASE QEvent_Move                          // :move()
      oP0         := oEvent:oldPos()
      oP1         := oEvent:pos()
      ::moveOwned( oP1:x() - oP0:x(), oP1:y() - oP0:y() )
   // SetAppEvent( xbeP_Move, { oP0:x(), oP0:y() }, { oP1:x(), oP1:y() }, Self )
      RETURN .f.
   CASE QEvent_Resize                        // :resize()
      oObj_O      := oEvent:oldSize()
      oObj_N      := oEvent:size()
      SetAppEvent( xbeP_Resize, { oObj_O:width(), oObj_O:height() }, { oObj_N:width(), oObj_N:height() }, Self )
      RETURN .f.
   CASE QEvent_DragEnter                     // :dragEnter()
      oObj_O      := oEvent:pos()
      SetAppEvent( xbeP_DragEnter, { oEvent:mouseButtons(), { oObj_O:x(), oObj_O:y() } }, /* oDragObj */, Self )
      RETURN .f.
   CASE QEvent_DragLeave                     // :dragLeave()
      SetAppEvent( xbeP_DragLeave, NIL, NIL, Self )
      RETURN .f.
   CASE QEvent_DragMove                      // :dragMotion()
      oObj_O      := oEvent:pos()
      SetAppEvent( xbeP_DragMotion, { oEvent:mouseButtons(), { oObj_O:x(), oObj_O:y() } }, NIL, Self )
      RETURN .f.
   CASE QEvent_Drop                          // :dragDrop()
      oObj_O      := oEvent:pos()
      SetAppEvent( xbeP_DragDrop, { oEvent:mouseButtons()  , { oObj_O:x(), oObj_O:y() } }, /* oDragObj */, Self )
      RETURN .f.
   CASE QEvent_WhatsThis                     // :helpRequest()
      SetAppEvent( xbeP_HelpRequest, NIL, NIL, Self )
      RETURN .f.
   CASE QEvent_KeyPress                      // :keyBoard()
      nXbpkey := hbxbp_QKeyEventToAppEvent( oEvent )
      SetAppEvent( xbeP_Keyboard, nXbpKey, NIL, self )
      RETURN .t.

   END SWITCH

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD XbpWindow:handleEvent( nEvent, mp1, mp2 )

   SWITCH ( nEvent )

   CASE xbeP_Keyboard
      ::keyboard( mp1 )
      EXIT
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

METHOD XbpWindow:onError( ... )
   LOCAL cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   RETURN ::oWidget:&cMsg( ... )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:captureMouse( lCapture )
   LOCAL lSuccess := .f.

   IF HB_ISLOGICAL( lCapture )
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

   IF HB_ISOBJECT( ::oWidget )
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

STATIC FUNCTION Xbp_RgbToName( nRgb )
   LOCAL cName := QColor( nRGB ):name()
   RETURN '#'+substr( cName,6 ) + substr( cName,4,2 ) + substr( cName,2,2 )

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setStyleSheet( cAttr, cCSS )
   LOCAL n, s := ""

   IF ! empty( cAttr )
      ::setCSSAttribute( cAttr, cCSS )
   ENDIF
   FOR n := 1 TO len( ::aCSS )
      s += ::aCSS[ n,2 ] + " "
   NEXT

   ::oWidget:setStyleSheet( "" )                /* Must Enforce It */
   ::oWidget:setStyleSheet( s )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setCSSAttribute( cAttr, cCSS )
   LOCAL n

   DEFAULT cCSS TO ""

   IF ( n := ascan( ::aCSS, {|e_| e_[ 1 ] == cAttr } ) ) == 0
      aadd( ::aCSS, { cAttr, cCSS } )
   ELSE
      ::aCSS[ n, 2 ] := cCSS
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:getCSS( nAttr, xValue )
   LOCAL s, n, nPoint, cFont, cFace, cCSS
   LOCAL aAttr   := { "bolditalic", "italic", "bold" }
   LOCAL cAttr   := "normal"
   LOCAL cWeight := "normal"

   SWITCH nAttr

   CASE XBP_PP_COMPOUNDNAME
      cFont := xValue
      s := lower( cFont )
      n := ascan( aAttr, {|e| at( e, cFont ) > 0 } )
      IF n > 0
         cAttr   := aAttr[ n ]
         n       := at( cAttr, s )
         cFont   := substr( cFont, 1, n-1 )
      ENDIF
      IF ( n := at( ".", cFont ) ) > 0
         nPoint  := val( substr( cFont,1,n-1 ) )
         cFont   := substr( cFont,n+1 )
      ELSE
         nPoint := 0
      ENDIF
      cFace := alltrim( cFont )

      IF cAttr == "bolditalic"
         cAttr   := "italic"
         cWeight := "bold"
      ENDIF
      IF cAttr == "bold"
         cAttr   := "normal"
         cWeight := "bold"
      ENDIF

      cCSS := 'font-family: "'+ cFace + '"; font-style: ' + cAttr + '; font-size: ' + ;
                                 hb_ntos( nPoint ) + 'pt; font-weight: ' + cWeight + ';'
      RETURN cCSS

   CASE XBP_PP_BGCLR
      RETURN "background-color: " + Xbp_RgbToName( xValue ) + ";"

   CASE XBP_PP_FGCLR
      RETURN "color: " + Xbp_RgbToName( xValue ) + ";"

   ENDSWITCH

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setColorBG( nRGB )
   LOCAL oldRGB := hbxbp_SetPresParam( ::aPresParams, XBP_PP_BGCLR )

   IF HB_ISNUMERIC( nRGB )
      hbxbp_SetPresParam( ::aPresParams, XBP_PP_BGCLR, nRGB )
      ::setCSSAttribute( "XBP_PP_BGCLR", ::getCSS( XBP_PP_BGCLR, nRGB ) )
      ::setStyleSheet()
   ENDIF

   RETURN oldRGB

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setColorFG( nRGB )
   LOCAL oldRGB := hbxbp_SetPresParam( ::aPresParams, XBP_PP_FGCLR )

   IF HB_ISNUMERIC( nRGB )
      hbxbp_SetPresParam( ::aPresParams, XBP_PP_FGCLR, nRGB )
      ::setCSSAttribute( "XBP_PP_FGCLR", ::getCSS( XBP_PP_FGCLR, nRGB ) )
      ::setStyleSheet()
   ENDIF

   RETURN oldRGB

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setFont( oFont )
   LOCAL cAttr := ""

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
   LOCAL cOldFont := hbxbp_SetPresParam( ::aPresParams, XBP_PP_COMPOUNDNAME )

   IF ! HB_ISNUMERIC( xFont )
      IF ! empty( xFont )
         ::setCSSAttribute( "XBP_PP_COMPOUNDNAME", ::getCSS( XBP_PP_COMPOUNDNAME, xFont ) )
         ::setStyleSheet()
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

   ::oWidget:hide()
   ::oWidget:show()

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
         ::oWidget:setCursor( QCursor( Qt_ArrowCursor        ) )

      CASE xResID == XBPSTATIC_SYSICON_WAIT      // Hour glass or clock
         ::oWidget:setCursor( QCursor( Qt_WaitCursor         ) )

      CASE xResID == XBPSTATIC_SYSICON_MOVE      // Move the window
         ::oWidget:setCursor( QCursor( Qt_OpenHandCursor     ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZE      // Change size (all directions)
         ::oWidget:setCursor( QCursor( Qt_SizeAllCursor      ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZENWSE  // Change size (North west-South east)
         ::oWidget:setCursor( QCursor( Qt_SizeFDiagCursor    ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZENESW  // Change size (North east-South west)
         ::oWidget:setCursor( QCursor( Qt_SizeBDiagCursor    ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZEWE    // Change size (West-East)
         ::oWidget:setCursor( QCursor( Qt_SizeHorCursor      ) )

      CASE xResID == XBPSTATIC_SYSICON_SIZENS    // Change size (North-South)
         ::oWidget:setCursor( QCursor( Qt_SizeVerCursor      ) )

      /* Possible Harbour-QT extensions - #deines yet to be finalized */

      CASE xResID == Qt_UpArrowCursor
         ::oWidget:setCursor( QCursor( Qt_UpArrowCursor      ) )

      CASE xResID == Qt_CrossCursor
         ::oWidget:setCursor( QCursor( Qt_CrossCursor        ) )

      CASE xResID == Qt_IBeamCursor
         ::oWidget:setCursor( QCursor( Qt_IBeamCursor        ) )

      CASE xResID == Qt_BlankCursor
         ::oWidget:setCursor( QCursor( Qt_BlankCursor        ) )

      CASE xResID == Qt_SplitVCursor
         ::oWidget:setCursor( QCursor( Qt_SplitVCursor       ) )

      CASE xResID == Qt_SplitHCursor
         ::oWidget:setCursor( QCursor( Qt_SplitHCursor       ) )

      CASE xResID == Qt_PointingHandCursor
         ::oWidget:setCursor( QCursor( Qt_PointingHandCursor ) )

      CASE xResID == Qt_ForbiddenCursor
         ::oWidget:setCursor( QCursor( Qt_ForbiddenCursor    ) )

      CASE xResID == Qt_ClosedHandCursor
         ::oWidget:setCursor( QCursor( Qt_ClosedHandCursor   ) )

      CASE xResID == Qt_WhatsThisCursor
         ::oWidget:setCursor( QCursor( Qt_WhatsThisCursor    ) )

      CASE xResID == Qt_BusyCursor
         ::oWidget:setCursor( QCursor( Qt_BusyCursor         ) )

      CASE xResID == Qt_BitmapCursor
         ::oWidget:setCursor( QCursor( Qt_BitmapCursor       ) )

      ENDCASE

   CASE nType == XBPWINDOW_POINTERTYPE_ICON
      IF valtype( xResID ) == "C"   // Harbour compatibility
         IF file( xResID )
            #if 0  /* The original image size - but in practice pointer should be proper sized */
            ::oWidget:setCursor( QCursor( "QPixmap", QPixmap( xResID ) ) )
            #else
            ::oWidget:setCursor( QCursor( "QPixmap", QPixmap( xResID ):scaled( 24,24 ) ) )
            #endif
         ENDIF
      ENDIF

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setTrackPointer( lTrack )
   LOCAL lRet := .f.

   IF HB_ISLOGICAL( lTrack )
      ::lTrack := lTrack
      lRet := .T.
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD XbpWindow:currentPos()

   RETURN { ::oWidget:x(), ::oWidget:y() }

/*----------------------------------------------------------------------*/

METHOD XbpWindow:currentSize()

   IF ::className() == "HBPAPPDESKTOP"
      RETURN { ::width(), ::height() }
   ELSE
      RETURN { ::oWidget:width(), ::oWidget:height() }
   ENDIF

   RETURN {}

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPos( aPos, lPaint )

   DEFAULT aPos TO ::aPos

   IF HB_ISARRAY( aPos )
      DEFAULT lPaint TO .T.
      ::oWidget:move( aPos[ 1 ], aPos[ 2 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setPosAndSize( aPos, aSize, lPaint )

   IF empty( ::qtObject )
      DEFAULT aPos  TO ::aPos
      DEFAULT aSize TO ::aSize

      IF HB_ISARRAY( aPos ) .and. HB_ISARRAY( aSize )
         DEFAULT lPaint TO .T.

         ::oWidget:move( aPos[ 1 ], aPos[ 2 ] )
         ::oWidget:resize( aSize[ 1 ], aSize[ 2 ] )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setSize( aSize, lPaint )

   DEFAULT aSize TO ::aSize

   IF HB_ISARRAY( aSize )
      DEFAULT lPaint TO .T.

      ::oWidget:resize( aSize[ 1 ], aSize[ 2 ] )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:isDerivedFrom( cClassORoObject )
   LOCAL lTrue := .f.
   LOCAL cCls := __ObjGetClsName( Self )

   /* Compares without Xbp or Wvg prefixes  */

   IF HB_ISSTRING( cClassORoObject )
      RETURN __clsParent( Self:classH, cClassORoObject )

   ELSEIF HB_ISOBJECT( cClassORoObject )
      IF upper( substr( cClassORoObject:className,4 ) ) == upper( substr( cCls,4 ) )
         lTrue := .t.
      ENDIF
   ENDIF

   IF !( lTrue )
      IF HB_ISOBJECT( ::oParent )
         lTrue := ::oParent:isDerivedFrom( cClassORoObject )
      ENDIF
   ENDIF

   RETURN lTrue

/*----------------------------------------------------------------------*/

METHOD XbpWindow:show()

   ::oWidget:show()
   ::oWidget:activateWindow()
   ::is_hidden      := .f.
   ::lHasInputFocus := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:toBack()

   ::oWidget:lower()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:toFront()

   ::oWidget:raise()

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
   LOCAL i

   IF HB_ISARRAY( aPPNew )
      FOR i := 1 TO len( aPPNew )
         hbxbp_SetPresParam( ::aPresParams, aPPNew[ i,1 ], aPPNew[ i,2 ] )
      NEXT
   ENDIF

   RETURN ::aPresParams

/*----------------------------------------------------------------------*/

METHOD XbpWindow:getHWND()
   LOCAL oError := ErrorNew()

   oError:severity    := ES_ERROR
   oError:genCode     := EG_UNSUPPORTED
   oError:subSystem   := "HBXBP"
   oError:subCode     := 7000
   oError:canRetry    := .F.
   oError:canDefault  := .F.
   oError:Args        := hb_AParams()
   oError:operation   := ProcName()

   Eval( ErrorBlock(), oError )

   RETURN NIL

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

METHOD XbpWindow:title( cTitle )
   LOCAL xTitle := ::cTitle

   IF HB_ISSTRING( cTitle )
      ::cTitle := cTitle
      IF HB_ISOBJECT( ::oWidget )
         ::oWidget:setWindowTitle( ::cTitle )
      ENDIF
   ENDIF

   RETURN xTitle

/*----------------------------------------------------------------------*/
 *                           Callback Methods
/*----------------------------------------------------------------------*/

METHOD XbpWindow:enter( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_enter := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_enter )
      eval( ::sl_enter, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:leave( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_leave := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_leave )
      eval( ::sl_leave, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_lbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_lbClick )
      eval( ::sl_lbClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbDblClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_lbDblClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_lbDblClick )
      eval( ::sl_lbDblClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbDown( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_lbDown := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_lbDown )
      eval( ::sl_lbDown, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:lbUp( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_lbUp := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_lbUp )
      eval( ::sl_lbUp, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_mbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_mbClick )
      eval( ::sl_mbClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbDblClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_mbDblClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_mbDblClick )
      eval( ::sl_mbDblClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbDown( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_mbDown := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_mbDown )
      eval( ::sl_mbDown, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:mbUp( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_mbUp := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_mbUp )
      eval( ::sl_mbUp, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:motion( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_motion := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_motion )
      eval( ::sl_motion, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_rbClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_rbClick )
      eval( ::sl_rbClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbDblClick( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_rbDblClick := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_rbDblClick )
      eval( ::sl_rbDblClick, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbDown( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_rbDown := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_rbDown )
      eval( ::sl_rbDown, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:rbUp( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_rbUp := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_rbUp )
      eval( ::sl_rbUp, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:wheel( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_wheel := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_wheel )
      eval( ::sl_wheel, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
 *                           Other Messages
/*----------------------------------------------------------------------*/

METHOD XbpWindow:close( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_close := a_[ 1 ]
   ELSEIF len( a_ ) == 0 .AND. HB_ISBLOCK( ::sl_close )
      eval( ::sl_close, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:helpRequest( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_helpRequest := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_helpRequest )
      eval( ::sl_helpRequest, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:keyboard( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_keyboard := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_keyboard )
      eval( ::sl_keyboard, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:killDisplayFocus( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_killDisplayFocus := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_killDisplayFocus )
      eval( ::sl_killDisplayFocus, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:killInputFocus( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_killInputFocus := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_killInputFocus )
      eval( ::sl_killInputFocus, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:move( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_move := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_move )
      eval( ::sl_move, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:paint( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_paint := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_paint )
      eval( ::sl_paint, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:quit( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_quit := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_quit )
      eval( ::sl_quit, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:resize( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_resize := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_resize )
      eval( ::sl_resize, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setDisplayFocus( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_setDisplayFocus := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_setDisplayFocus )
      eval( ::sl_setDisplayFocus, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:setInputFocus( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_setInputFocus := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_setInputFocus )
      eval( ::sl_setInputFocus, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragEnter( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_dragEnter := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_dragEnter )
      eval( ::sl_dragEnter, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragMotion( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_dragMotion := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::sl_dragMotion )
      eval( ::sl_dragMotion, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragLeave( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_dragLeave := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_dragLeave )
      eval( ::sl_dragLeave, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:dragDrop( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_dragDrop := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. HB_ISBLOCK( ::sl_dragDrop )
      eval( ::sl_dragDrop, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:hbContextMenu( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::hb_contextMenu := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. HB_ISBLOCK( ::hb_contextMenu )
      eval( ::hb_contextMenu, a_[ 1 ], NIL, Self )
   ENDIF
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

   ::oWidget:setFocus()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:sendMessage()// nMessage, nlParam, nwParam )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpWindow:hbLayout( nTypeLayout )
   LOCAL lApply := .f.
   LOCAL oldLayout, oXbp

   oldLayout := ::nLayout

   IF HB_ISNUMERIC( nTypeLayout ) .AND. nTypeLayout > 0 .AND. nTypeLayout <= HBPLAYOUT_TYPE_MAX
      ::nLayout := nTypeLayout
      lApply := .t.
   ELSEIF !empty( ::nLayout )
      lApply := .t.
   ENDIF

   IF lApply
      IF !empty( ::qLayout )
         ::qLayout := NIL
      ENDIF
      DO CASE
      CASE ::nLayout == HBPLAYOUT_TYPE_HORZBOX
         ::qLayout := QHBoxLayout()
      CASE ::nLayout == HBPLAYOUT_TYPE_VERTBOX
         ::qLayout := QVBoxLayout()
      CASE ::nLayout == HBPLAYOUT_TYPE_GRID
         ::qLayout := QGridLayout()
      CASE ::nLayout == HBPLAYOUT_TYPE_FORM
         ::qLayout := QFormLayout()
      ENDCASE
      ::oWidget:setLayout( ::qLayout )
      FOR EACH oXbp IN ::aChildren
         ::qLayout:addWidget( oXbp:oWidget )
      NEXT
   ENDIF

   RETURN oldLayout

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

STATIC FUNCTION hbxbp_PresParam( aPP )

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

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbxbp_SetPresParamIfNil( aPP, nParam, xValue )
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

FUNCTION hbxbp_SetPresParam( aPP, nParam, xValue )
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS HbpAppDesktop INHERIT XbpWindow

   METHOD init()
   METHOD create()
   METHOD width( nScreen )
   METHOD virtualWidth()
   METHOD height( nScreen )
   METHOD virtualHeight()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbpAppDesktop:init()

   ::xbpWindow:init()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpAppDesktop:create()

   ::oWidget := QDesktopWidget()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbpAppDesktop:width( nScreen )

   DEFAULT nScreen TO -1

   RETURN iif( nScreen == -1, ::oWidget:screenGeometry( ::oWidget:primaryScreen() ):right(), ;
                                                           ::oWidget:screenGeometry( nScreen ):right() )

/*----------------------------------------------------------------------*/

METHOD HbpAppDesktop:height( nScreen )

   DEFAULT nScreen TO -1

   RETURN iif( nScreen == -1, ::oWidget:screenGeometry( ::oWidget:primaryScreen() ):bottom(), ;
                                                           ::oWidget:screenGeometry( nScreen ):bottom() )

/*----------------------------------------------------------------------*/

METHOD HbpAppDesktop:virtualWidth()

   RETURN ::oWidget:width()

/*----------------------------------------------------------------------*/

METHOD HbpAppDesktop:virtualHeight()

   RETURN ::oWidget:height()

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
