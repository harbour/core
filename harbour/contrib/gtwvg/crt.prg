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

//                               EkOnkar
//                         ( The LORD is ONE )
//
//                    Xbase++ Compatible xbpCrt Class
//
//                 Pritpal Bedi  <bedipritpal@hotmail.com>
//                              08Nov2008

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgCrt  INHERIT  WvgWindow, WvgPartHandler

   VAR    oMenu

   /*  CONFIGURATION */
   VAR    alwaysOnTop                           INIT  .F.        /* Determines whether the dialog can be covered by other windows */
   VAR    border                                INIT  0          /* Border type for the XbpCrt window */
   VAR    clipChildren                          INIT  .F.
   VAR    closable                              INIT  .T.
   VAR    fontHeight                            INIT  16
   VAR    fontWidth                             INIT  10
   VAR    fontName                              INIT  "Courier New"
   VAR    gridMove                              INIT  .F.
   VAR    icon                                  INIT  0
   VAR    minMax                                INIT  .T.
   VAR    sysMenu                               INIT  .T.
   VAR    taskList                              INIT  .T.
   VAR    title                                 INIT  " "
   VAR    titleBar                              INIT  .T.
   VAR    visible                               INIT  .T.

   VAR    autoFocus                             INIT  .T.
   VAR    autoMark                              INIT  .T.
   VAR    dropFont                              INIT  .T.
   VAR    dropZone                              INIT  .F.
   VAR    helpLink                              INIT  NIL
   VAR    maxCol                                INIT  79
   VAR    maxRow                                INIT  24
   VAR    mouseMode                             INIT  1          /* Determines whether mouse coordinates are given as graphics or text coordinates.*/
   VAR    modalResult                           INIT  NIL        /* Specifies the result of a modal dialog.                                        */
   VAR    aSyncFlush                            INIT  .F.        /* Determines the display behavior of text-mode output.                           */
   VAR    tooltipText                           INIT  ""
   VAR    useShortCuts                          INIT  .F.        /* Enables shortcut keys for the system menu                                      */
   VAR    xSize                                 INIT  640 READONLY
   VAR    ySize                                 INIT  400 READONLY

   /*  GUI Specifics */
   VAR    animate                               INIT  .F.
   VAR    clipParent                            INIT  .F.
   VAR    clipSiblings                          INIT  .T.
   VAR    group                                 INIT  0          /* XBP_NO_GROUP */
   VAR    sizeRedraw                            INIT  .F.
   VAR    tabStop                               INIT  .F.

   /*  CALLBACK SLOTS */
   VAR    sl_enter
   VAR    sl_leave
   VAR    sl_lbClick
   VAR    sl_lbDblClick
   VAR    sl_lbDown
   VAR    sl_lbUp
   VAR    sl_mbClick
   VAR    sl_mbDblClick
   VAR    sl_mbDown
   VAR    sl_mbUp
   VAR    sl_motion
   VAR    sl_rbClick
   VAR    sl_rbDblClick
   VAR    sl_rbDown
   VAR    sl_rbUp
   VAR    sl_wheel

   VAR    sl_close
   VAR    sl_helpRequest
   VAR    sl_keyboard
   VAR    sl_killDisplayFocus                    /* only for CRT */
   VAR    sl_killInputFocus
   VAR    sl_move
   VAR    sl_paint                               /* only for gui dialogs */
   VAR    sl_quit
   VAR    sl_resize
   VAR    sl_setDisplayFocus                     /* only for CRT */
   VAR    sl_setInputFocus
   VAR    sl_dragEnter
   VAR    sl_dragMotion
   VAR    sl_dragLeave
   VAR    sl_dragDrop

   /*  HARUR implementation */
   VAR    resizable                             INIT  .T.
   VAR    resizeMode                            INIT  HB_GTI_RESIZEMODE_FONT
   VAR    style                                 INIT  ( WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU + WS_SIZEBOX + WS_MINIMIZEBOX + WS_MAXIMIZEBOX )
   VAR    exStyle                               INIT  0
   VAR    lModal                                INIT  .F.
   VAR    pGTp
   VAR    pGT
   VAR    objType                               INIT  objTypeCrt
   VAR    ClassName                             INIT  "WVGCRT"
   VAR    drawingArea
   VAR    hWnd
   VAR    aPos                                  INIT  { 0, 0 }
   VAR    aSize                                 INIT  { 24, 79 }
   VAR    aPresParams                           INIT  {}
   VAR    lHasInputFocus                        INIT  .F.
   VAR    nFrameState                           INIT  0  /* normal */

   VAR    isGT                                  INIT  .F.

   METHOD setTitle( cTitle )                    INLINE ::title := cTitle, hb_gtInfo( HB_GTI_WINTITLE, cTitle )
   METHOD getTitle()                            INLINE hb_gtInfo( HB_GTI_WINTITLE )
   METHOD showWindow()                          INLINE ::show()
   METHOD refresh()                             INLINE ::invalidateRect()

   /* LIFE CYCLE */
   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()

   /*  METHODS */
   METHOD currentPos()
   METHOD currentSize()
   METHOD captureMouse()
   METHOD disable()
   METHOD enable()
   METHOD getFrameState()
   METHOD getHWND()
   METHOD getModalState()
   METHOD hasInputFocus()
   METHOD hide()
   METHOD invalidateRect( nTop, nLeft, nBottom, nRight )
   METHOD isEnabled()
   METHOD isVisible()
   METHOD lockPS()
   METHOD lockUpdate()
   METHOD menuBar()
   METHOD setColorBG()
   METHOD setColorFG()
   METHOD setFont()
   METHOD setFontCompoundName()
   METHOD setFrameState( nState )
   METHOD setPresParam()
   METHOD setModalState()
   METHOD setPointer()
   METHOD setTrackPointer()
   METHOD setPos()
   METHOD setPosAndSize()
   METHOD setSize( aSize, lPaint )
   METHOD showModal()
   METHOD show()
   METHOD toBack()
   METHOD toFront()
   METHOD unlockPS()
   METHOD winDevice()

   /* MESSAGES  */
   METHOD enter( xParam )                       SETGET
   METHOD leave( xParam )                       SETGET
   METHOD lbClick( xParam )                     SETGET
   METHOD lbDblClick( xParam )                  SETGET
   METHOD lbDown( xParam )                      SETGET
   METHOD lbUp( xParam )                        SETGET
   METHOD mbClick( xParam )                     SETGET
   METHOD mbDblClick( xParam )                  SETGET
   METHOD mbDown( xParam )                      SETGET
   METHOD mbUp( xParam )                        SETGET
   METHOD motion( xParam )                      SETGET
   METHOD rbClick( xParam )                     SETGET
   METHOD rbDblClick( xParam )                  SETGET
   METHOD rbDown( xParam )                      SETGET
   METHOD rbUp( xParam )                        SETGET
   METHOD wheel( xParam )                       SETGET
   METHOD close( xParam )                       SETGET
   METHOD helpRequest( xParam )                 SETGET
   METHOD keyboard( xParam )                    SETGET
   METHOD killDisplayFocus( xParam )            SETGET
   METHOD killInputFocus( xParam )              SETGET
   METHOD move( xParam )                        SETGET
   METHOD paint( xParam )                       SETGET
   METHOD quit( xParam, xParam1 )               SETGET
   METHOD resize( xParam )                      SETGET
   METHOD setDisplayFocus( xParam )             SETGET
   METHOD setInputFocus( xParam )               SETGET
   METHOD dragEnter( xParam, xParam1 )          SETGET
   METHOD dragMotion( xParam )                  SETGET
   METHOD dragLeave( xParam )                   SETGET
   METHOD dragDrop( xParam, xParam1 )           SETGET

ENDCLASS

METHOD WvgCrt:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF HB_ISARRAY( aPos )
      ::aPos := aPos
   ENDIF
   IF HB_ISARRAY( aSize )
      ::aSize := aSize
   ENDIF
   IF HB_ISARRAY( aPresParams )
      ::aPresParams := aPresParams
   ENDIF
   IF HB_ISLOGICAL( lVisible )
      ::visible := lVisible
   ENDIF

   /*  Drawing Area of oCrt will point to itself */
   ::drawingArea := Self

   RETURN Self

METHOD WvgCrt:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   LOCAL lRowCol := .T.

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

   ::maxRow := ::aSize[ 1 ]
   ::maxCol := ::aSize[ 2 ]

   ::WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::lModal
      ::pGT  := hb_gtCreate( "WVG" )
      ::pGTp := hb_gtSelect( ::pGT )
   ELSE
      hb_gtReload( "WVG" )
      ::pGT := hb_gtSelect()
   ENDIF

   hb_gtInfo( HB_GTI_NOTIFIERBLOCKGUI, {| nEvent, ... | ::notifier( nEvent, ... )      } )
   hb_gtInfo( HB_GTI_NOTIFIERBLOCK   , {| nEvent, ... | ::notifierBlock( nEvent, ... ) } )

   IF ::lModal
      ::style := WS_POPUP + WS_CAPTION + WS_SYSMENU
      IF ::resizable
         ::style += WS_MINIMIZEBOX + WS_MAXIMIZEBOX + WS_THICKFRAME
      ENDIF
   ENDIF

   hb_gtInfo( HB_GTI_RESIZABLE, ::resizable )
   hb_gtInfo( HB_GTI_PRESPARAMS, { ::exStyle, ::style, ::aPos[ 1 ], ::aPos[ 2 ], ;
      ::maxRow + 1, ::maxCol + 1, ::pGTp, .F., lRowCol, HB_WNDTYPE_CRT } )
   hb_gtInfo( HB_GTI_SETFONT, { ::fontName, ::fontHeight, ::fontWidth } )

   IF HB_ISNUMERIC( ::icon )
      hb_gtInfo( HB_GTI_ICONRES, ::icon )
   ELSE
      IF ( ".ico" $ Lower( ::icon ) )
         hb_gtInfo( HB_GTI_ICONFILE, ::icon )
      ELSE
         hb_gtInfo( HB_GTI_ICONRES, ::icon )
      ENDIF
   ENDIF

   /* CreateWindow() be forced to execute */
   CLS
   ::hWnd := hb_gtInfo( HB_GTI_SPEC, HB_GTS_WINDOWHANDLE )
   ::setFocus()

   hb_gtInfo( HB_GTI_CLOSABLE  , ::closable  )
   hb_gtInfo( HB_GTI_WINTITLE  , ::title     )
   hb_gtInfo( HB_GTI_RESIZEMODE, iif( ::resizeMode == HB_GTI_RESIZEMODE_ROWS, HB_GTI_RESIZEMODE_ROWS, HB_GTI_RESIZEMODE_FONT ) )

   IF ::lModal
      hb_gtInfo( HB_GTI_DISABLE, ::pGTp )
   ENDIF

   IF ::visible
      hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_NORMAL )
      ::lHasInputFocus := .T.
   ENDIF

   RETURN Self

METHOD WvgCrt:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

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

METHOD WvgCrt:destroy()

   IF HB_ISOBJECT( ::oMenu )
      ::oMenu:destroy()
   ENDIF

   IF Len( ::aChildren ) > 0
      AEval( ::aChildren, {| o | o:destroy() } )
      ::aChildren := {}
   ENDIF

   IF ! ::isGT
      IF ::lModal
         hb_gtInfo( HB_GTI_ENABLE, ::pGTp )
         hb_gtSelect( ::pGTp )
         hb_gtInfo( HB_GTI_SETFOCUS, ::pGTp )
      ENDIF
      ::pGT  := NIL
      ::pGTp := NIL
   ENDIF

   ::WvgWindow:destroy()

   RETURN Self

METHOD WvgCrt:currentPos()

   RETURN Self

METHOD WvgCrt:currentSize()

   RETURN { hb_gtInfo( HB_GTI_SCREENWIDTH ), hb_gtInfo( HB_GTI_SCREENHEIGHT ) }

METHOD WvgCrt:captureMouse()

   RETURN Self

METHOD WvgCrt:disable()

   hb_gtInfo( HB_GTI_DISABLE, ::pGT )

   RETURN Self

METHOD WvgCrt:enable()

   hb_gtInfo( HB_GTI_ENABLE, ::pGT )

   RETURN Self

METHOD WvgCrt:getFrameState()

   IF Wvg_IsIconic( ::hWnd )
      RETURN WVGDLG_FRAMESTAT_MINIMIZED
   ENDIF
   IF Wvg_IsZoomed( ::hWnd )
      RETURN WVGDLG_FRAMESTAT_MAXIMIZED
   ENDIF

   RETURN WVGDLG_FRAMESTAT_NORMALIZED

METHOD WvgCrt:getHWND()

   RETURN ::hWnd

METHOD WvgCrt:getModalState()

   RETURN Self

METHOD WvgCrt:hasInputFocus()

   RETURN ::lHasInputFocus

METHOD WvgCrt:hide()

   hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, HB_GTS_SW_HIDE )

   RETURN Self

METHOD WvgCrt:invalidateRect( nTop, nLeft, nBottom, nRight )

   __defaultNIL( @nTop, 0 )
   __defaultNIL( @nLeft, 0 )
   __defaultNIL( @nBottom, MaxRow() )
   __defaultNIL( @nRight, MaxCol() )

   Wvt_InvalidateRect( nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvgCrt:isEnabled()

   RETURN Self

METHOD WvgCrt:isVisible()

   RETURN Self

METHOD WvgCrt:lockPS()

   RETURN Self

METHOD WvgCrt:lockUpdate()

   RETURN Self

METHOD WvgCrt:menuBar()

   IF ! HB_ISOBJECT( ::oMenu )
      ::oMenu := WvgMenuBar():New( self ):create()
   ENDIF

   RETURN ::oMenu

METHOD WvgCrt:setColorBG()

   RETURN Self

METHOD WvgCrt:setColorFG()

   RETURN Self

METHOD WvgCrt:setFont()

   RETURN Self

METHOD WvgCrt:setFontCompoundName()

   RETURN ""

METHOD WvgCrt:setFrameState( nState )

   LOCAL lSuccess := .F.

   DO CASE

   CASE nState == WVGDLG_FRAMESTAT_MINIMIZED
      lSuccess := ::sendMessage( WM_SYSCOMMAND, SC_MINIMIZE, 0 )

   CASE nState == WVGDLG_FRAMESTAT_MAXIMIZED
      lSuccess := ::sendMessage( WM_SYSCOMMAND, SC_MAXIMIZE, 0 )

   CASE nState == WVGDLG_FRAMESTAT_NORMALIZED
      lSuccess := ::sendMessage( WM_SYSCOMMAND, SC_RESTORE, 0 )

   ENDCASE

   RETURN lSuccess

METHOD WvgCrt:setModalState()

   RETURN Self

METHOD WvgCrt:setPointer()

   RETURN Self

METHOD WvgCrt:setTrackPointer()

   RETURN Self

METHOD WvgCrt:setPos()

   RETURN Self

METHOD WvgCrt:setPosAndSize()

   RETURN Self

METHOD WvgCrt:setPresParam()

   RETURN Self

METHOD WvgCrt:setSize( aSize, lPaint )

   IF HB_ISARRAY( aSize )
      __defaultNIL( @lPaint, .T. )

      hb_gtInfo( HB_GTI_SCREENHEIGHT, aSize[ 1 ] )
      hb_gtInfo( HB_GTI_SCREENWIDTH , aSize[ 2 ] )
   ENDIF

   RETURN Self

METHOD WvgCrt:show()

   hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_NORMAL )
   ::lHasInputFocus := .T.

   RETURN Self

METHOD WvgCrt:showModal()

   RETURN Self

METHOD WvgCrt:toBack()

   RETURN Self

METHOD WvgCrt:toFront()

   RETURN Wvg_SetWindowPosToTop( ::hWnd )

METHOD WvgCrt:unlockPS()

   RETURN Self

METHOD WvgCrt:winDevice()

   RETURN Self

METHOD WvgCrt:enter( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_enter )
      Eval( ::sl_enter, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_enter := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:leave( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_leave )
      Eval( ::sl_leave, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_leave := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:lbClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbClick )
      Eval( ::sl_lbClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:lbDblClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbDblClick )
      Eval( ::sl_lbDblClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbDblClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:lbDown( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbDown )
      Eval( ::sl_lbDown, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbDown := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:lbUp( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_lbUp )
      Eval( ::sl_lbUp, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_lbUp := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:mbClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbClick )
      Eval( ::sl_mbClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:mbDblClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbDblClick )
      Eval( ::sl_mbDblClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbDblClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:mbDown( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbDown )
      Eval( ::sl_mbDown, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbDown := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:mbUp( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_mbUp )
      Eval( ::sl_mbUp, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_mbUp := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:motion( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_motion )
      Eval( ::sl_motion, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_motion := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:rbClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbClick )
      Eval( ::sl_rbClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:rbDblClick( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbDblClick )
      Eval( ::sl_rbDblClick, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbDblClick := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:rbDown( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbDown )
      Eval( ::sl_rbDown, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbDown := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:rbUp( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_rbUp )
      Eval( ::sl_rbUp, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_rbUp := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:wheel( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_wheel )
      Eval( ::sl_wheel, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_wheel := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:close( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_close )
      Eval( ::sl_close, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_close := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:helpRequest( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_helpRequest )
      Eval( ::sl_helpRequest, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_helpRequest := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:keyboard( xParam )

   IF HB_ISNUMERIC( xParam ) .AND. HB_ISBLOCK( ::sl_keyboard )
      Eval( ::sl_keyboard, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_keyboard := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:killDisplayFocus( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_killDisplayFocus )
      Eval( ::sl_killDisplayFocus, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_killDisplayFocus := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:killInputFocus( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_killInputFocus )
      Eval( ::sl_killInputFocus, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_killInputFocus := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:move( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_move )
      Eval( ::sl_move, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_move := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:paint( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_paint )
      Eval( ::sl_paint, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_paint := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:quit( xParam, xParam1 )

   IF HB_ISNUMERIC( xParam ) .AND. HB_ISBLOCK( ::sl_quit )
      Eval( ::sl_quit, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_quit := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:resize( xParam )

   IF HB_ISBLOCK( xParam )/* .or. HB_ISNIL( xParam ) */
      ::sl_resize := xParam
      RETURN NIL
   ENDIF
   IF Empty( xParam )
      ::sendMessage( WM_SIZE, 0, 0 )
   ENDIF

   RETURN Self

METHOD WvgCrt:setDisplayFocus( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::setDisplayFocus )
      Eval( ::setDisplayFocus, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::setDisplayFocus := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:setInputFocus( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_setInputFocus )
      Eval( ::sl_setInputFocus, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_setInputFocus := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:dragEnter( xParam, xParam1 )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_dragEnter )
      Eval( ::sl_dragEnter, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragEnter := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:dragMotion( xParam )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_dragMotion )
      Eval( ::sl_dragMotion, xParam, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragMotion := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:dragLeave( xParam )

   IF HB_ISNIL( xParam ) .AND. HB_ISBLOCK( ::sl_dragLeave )
      Eval( ::sl_dragLeave, NIL, NIL, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragLeave := xParam
      RETURN NIL
   ENDIF

   RETURN Self

METHOD WvgCrt:dragDrop( xParam, xParam1 )

   IF HB_ISARRAY( xParam ) .AND. HB_ISBLOCK( ::sl_dragDrop )
      Eval( ::sl_dragDrop, xParam, xParam1, Self )
      RETURN Self
   ENDIF

   IF HB_ISBLOCK( xParam ) .OR. HB_ISNIL( xParam )
      ::sl_dragDrop := xParam
      RETURN NIL
   ENDIF

   RETURN Self
