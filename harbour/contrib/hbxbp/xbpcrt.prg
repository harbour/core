/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                    Xbase++ Compatible xbpCrt Class
 *
 *                 Pritpal Bedi  <bedipritpal@hotmail.com>
 *                              12Apr2011
 *
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "xbp.ch"
#include "appevent.ch"

#define HB_GTI_WIDGET          2001
#define HB_GTI_DRAWINGAREA     2002
#define HB_GTI_DISABLE         2003
#define HB_GTI_EVENTLOOP       2004

/*----------------------------------------------------------------------*/

CLASS XbpCrt  INHERIT  XbpWindow, XbpPartHandler

   DATA     oMenu

   /*  CONFIGURATION */
   DATA     alwaysOnTop                           INIT  .F.        /* Determines whether the dialog can be covered by other windows */
   DATA     border                                INIT  0          /* Border type for the XbpCrt window */
   DATA     clipChildren                          INIT  .F.
   DATA     closable                              INIT  .T.
   DATA     fontHeight                            INIT  16
   DATA     fontName                              INIT  "Courier New"
   DATA     fontWidth                             INIT  8
   DATA     gridMove                              INIT  .F.
   DATA     icon                                  INIT  0
   DATA     minMax                                INIT  .T.
   DATA     sysMenu                               INIT  .T.
   DATA     taskList                              INIT  .T.
   DATA     title                                 INIT  " "
   DATA     titleBar                              INIT  .T.
   DATA     visible                               INIT  .T.

   DATA     autoFocus                             INIT  .T.
   DATA     autoMark                              INIT  .T.
   DATA     dropFont                              INIT  .T.
   DATA     dropZone                              INIT  .F.
   DATA     helpLink                              INIT  NIL
   DATA     maxCol                                INIT  79
   DATA     maxRow                                INIT  24
   DATA     mouseMode                             INIT  1          /* Determines whether mouse coordinates are given as graphics or text coordinates.*/
   DATA     modalResult                           INIT  NIL        /* Specifies the result of a modal dialog.                                        */
   DATA     aSyncFlush                            INIT  .F.        /* Determines the display behavior of text-mode output.                           */
   DATA     tooltipText                           INIT  ""
   DATA     useShortCuts                          INIT  .F.        /* Enables shortcut keys for the system menu                                      */
   DATA     xSize                                 INIT  640 READONLY
   DATA     ySize                                 INIT  400 READONLY

   /*  GUI Specifics */
   DATA     animate                               INIT  .F.
   DATA     clipParent                            INIT  .F.
   DATA     clipSiblings                          INIT  .T.
   DATA     group                                 INIT  0          /* XBP_NO_GROUP */
   DATA     sizeRedraw                            INIT  .F.
   DATA     tabStop                               INIT  .F.

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

   DATA     sl_close
   DATA     sl_helpRequest
   DATA     sl_keyboard
   DATA     sl_killDisplayFocus                    /* only for CRT */
   DATA     sl_killInputFocus
   DATA     sl_move
   DATA     sl_paint                               /* only for gui dialogs */
   DATA     sl_quit
   DATA     sl_resize
   DATA     sl_setDisplayFocus                     /* only for CRT */
   DATA     sl_setInputFocus
   DATA     sl_dragEnter
   DATA     sl_dragMotion
   DATA     sl_dragLeave
   DATA     sl_dragDrop

   /*  HARBOUR implementation */
   DATA     resizable                             INIT  .t.
   DATA     resizeMode                            INIT  HB_GTI_RESIZEMODE_FONT
   DATA     style                                 INIT  0 // (WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU + WS_SIZEBOX + WS_MINIMIZEBOX + WS_MAXIMIZEBOX)
   DATA     exStyle                               INIT  0
   DATA     lModal                                INIT  .f.
   DATA     pGTp
   DATA     pGT
   DATA     objType                               INIT  0 //objTypeCrt
   DATA     ClassName                             INIT  "XBPCRT"
   DATA     drawingArea
   DATA     hWnd
   DATA     aPos                                  INIT  { 0,0 }
   DATA     aSize                                 INIT  { 24,79 }
   DATA     aPresParams                           INIT  {}
   DATA     lHasInputFocus                        INIT  .F.
   DATA     nFrameState                           INIT  0  /* normal */

   METHOD   setTitle( cTitle )                    INLINE ::title := cTitle, hb_gtInfo( HB_GTI_WINTITLE, cTitle )
   METHOD   getTitle()                            INLINE hb_gtInfo( HB_GTI_WINTITLE )
   METHOD   showWindow()                          INLINE ::show()
   METHOD   refresh()                             INLINE NIL //::invalidateRect()

   /*  LIFE CYCLE */
   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   setFocus()

   /*  METHODS */
   METHOD   currentPos()
   METHOD   currentSize()
   METHOD   captureMouse()
   METHOD   disable()
   METHOD   enable()
   METHOD   getFrameState()
   METHOD   getHWND()
   METHOD   getModalState()
   METHOD   hasInputFocus()
   METHOD   hide()
   METHOD   invalidateRect( nTop, nLeft, nBottom, nRight )
   METHOD   isEnabled()
   METHOD   isVisible()
   METHOD   lockPS()
   METHOD   lockUpdate()
   METHOD   menuBar()
   METHOD   setColorBG()
   METHOD   setColorFG()
   METHOD   setFont()
   METHOD   setFontCompoundName()
   METHOD   setFrameState( nState )
   METHOD   setPresParam()
   METHOD   setModalState()
   METHOD   setPointer()
   METHOD   setTrackPointer()
   METHOD   setPos()
   METHOD   setPosAndSize()
   METHOD   setSize( aSize, lPaint )
   METHOD   showModal()
   METHOD   show()
   METHOD   toBack()
   METHOD   toFront()
   METHOD   unlockPS()
   METHOD   winDevice()

   /* MESSAGES  */
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
   METHOD   close( xParam )                       SETGET
   METHOD   helpRequest( xParam )                 SETGET
   METHOD   keyboard( xParam )                    SETGET
   METHOD   killDisplayFocus( xParam )            SETGET
   METHOD   killInputFocus( xParam )              SETGET
   METHOD   move( xParam )                        SETGET
   METHOD   paint( xParam )                       SETGET
   METHOD   quit( xParam, xParam1 )               SETGET
   METHOD   resize( xParam )                      SETGET
   METHOD   setDisplayFocus( xParam )             SETGET
   METHOD   setInputFocus( xParam )               SETGET
   METHOD   dragEnter( xParam, xParam1 )          SETGET
   METHOD   dragMotion( xParam )                  SETGET
   METHOD   dragLeave( xParam )                   SETGET
   METHOD   dragDrop( xParam, xParam1 )           SETGET

   DATA     nFlags
   DATA     oMDI

   ENDCLASS

/*----------------------------------------------------------------------*/
 *                         Instance Initiation
/*----------------------------------------------------------------------*/

METHOD XbpCrt:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::resizeMode  := 0
   ::mouseMode   := 0

   ::drawingArea := XbpDrawingArea():new( self, , {0,0}, ::aSize, , .t. )

   RETURN Self

/*----------------------------------------------------------------------*/
 *                              Life Cycle
/*----------------------------------------------------------------------*/

METHOD XbpCrt:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::XbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::maxRow := ::aSize[ 1 ]
   ::maxCol := ::aSize[ 2 ]

   hb_gtReload( "QTC" )
   ::pGT := hb_gtSelect()

   /* Creates physical window */
   ? " "

   ::oWidget := hb_gtInfo( HB_GTI_WIDGET )
   ::drawingArea:oWidget := hb_gtInfo( HB_GTI_DRAWINGAREA )

   ::oWidget:setWindowTitle( ::title )

   hb_gtInfo( HB_GTI_CLOSABLE , ::closable  )
   hb_gtInfo( HB_GTI_RESIZABLE, ::resizable )

   //hb_gtInfo( HB_GTI_RESIZEMODE, iif( ::resizeMode == HB_GTI_RESIZEMODE_ROWS, HB_GTI_RESIZEMODE_ROWS, HB_GTI_RESIZEMODE_FONT ) )

   IF ! empty( ::toolTipText )
      ::oWidget:setTooltip( ::toolTipText )
   ENDIF
   IF hb_isChar( ::icon )
      ::oWidget:setWindowIcon( ::icon )
   ENDIF

   IF ::lModal
      hb_gtInfo( HB_GTI_DISABLE, ::pGTp )
   ENDIF
   IF ::visible
      ::oWidget:show()
      ::oWidget:setFocus()
      ::lHasInputFocus := .t.
   ENDIF

   ::nFlags := ::oWidget:windowFlags()
   IF __objGetClsName( ::oParent ) $ "XBPDRAWINGAREA"
      ::setParent( ::oParent )
   ENDIF


   // HB_GtInfo( HB_GTI_NOTIFIERBLOCK, {|nEvent, ...| ::notifier( nEvent, ... ) } )

#if 0
   hb_gtInfo( HB_GTI_PRESPARAMS, { ::exStyle, ::style, ::aPos[ 1 ], ::aPos[ 2 ], ;
                           ::maxRow+1, ::maxCol+1, ::pGTp, .F., lRowCol, HB_WNDTYPE_CRT } )
   hb_gtInfo( HB_GTI_SETFONT, { ::fontName, ::fontHeight, ::fontWidth } )
#endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

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

METHOD XbpCrt:destroy()

   ::oMDI := NIL

   IF hb_isObject( ::oMenu )
      ::oMenu:destroy()
   ENDIF

   IF Len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
   ENDIF

   ::pGT  := NIL
   ::pGTp := NIL

   RETURN Self

/*----------------------------------------------------------------------*/
 *                              Methods
/*----------------------------------------------------------------------*/

METHOD XbpCrt:currentPos()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:currentSize()

   RETURN { hb_gtInfo( HB_GTI_SCREENWIDTH ), hb_gtInfo( HB_GTI_SCREENHEIGHT ) }

/*----------------------------------------------------------------------*/

METHOD XbpCrt:captureMouse()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:disable()

   //hb_gtInfo( HB_GTI_DISABLE, ::pGT )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:enable()

   //hb_gtInfo( HB_GTI_ENABLE, ::pGT )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:getFrameState()
#if 0
   IF WVG_IsIconic( ::hWnd )
      RETURN WVGDLG_FRAMESTAT_MINIMIZED
   ENDIF
   IF WVG_IsZoomed( ::hWnd )
      RETURN WVGDLG_FRAMESTAT_MAXIMIZED
   ENDIF

   RETURN WVGDLG_FRAMESTAT_NORMALIZED
#endif
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpCrt:getHWND()

   RETURN ::hWnd

/*----------------------------------------------------------------------*/

METHOD XbpCrt:getModalState()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:hasInputFocus()

   RETURN ::lHasInputFocus

/*----------------------------------------------------------------------*/

METHOD XbpCrt:hide()

   //hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, HB_GTS_SW_HIDE )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:invalidateRect( nTop, nLeft, nBottom, nRight )

   DEFAULT nTop TO 0
   DEFAULT nLeft TO 0
   DEFAULT nBottom TO maxrow()
   DEFAULT nRight TO maxcol()

   //Wvt_InvalidateRect( nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:isEnabled()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:isVisible()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:lockPS()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:lockUpdate()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:menuBar()

   IF !( hb_isObject( ::oMenu ) )
      ::oMenu := XbpMenuBar():New( self ):create()
   ENDIF

   RETURN ::oMenu

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setColorBG()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setColorFG()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setFont()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setFontCompoundName()

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setFrameState( nState )
   Local lSuccess := .f.

   HB_SYMBOL_UNUSED( nState )
   DO CASE
#if 0
   CASE nState == XBPDLG_FRAMESTAT_MINIMIZED
      lSuccess := ::sendMessage( WM_SYSCOMMAND, SC_MINIMIZE, 0 )

   CASE nState == XBPDLG_FRAMESTAT_MAXIMIZED
      lSuccess := ::sendMessage( WM_SYSCOMMAND, SC_MAXIMIZE, 0 )

   CASE nState == XBPDLG_FRAMESTAT_NORMALIZED
      lSuccess := ::sendMessage( WM_SYSCOMMAND, SC_RESTORE, 0 )
#endif
   ENDCASE

   RETURN lSuccess
/*----------------------------------------------------------------------*/

METHOD XbpCrt:setModalState()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setPointer()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setTrackPointer()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setPos()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setPosAndSize()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setPresParam()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setSize( aSize, lPaint )

   if hb_isArray( aSize )
      DEFAULT lPaint TO .T.

      hb_gtInfo( HB_GTI_SCREENHEIGHT, aSize[ 1 ] )
      hb_gtInfo( HB_GTI_SCREENWIDTH , aSize[ 2 ] )
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:show()

   //Hb_GtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_NORMAL )
   ::lHasInputFocus := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:showModal()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:toBack()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:toFront()
   //RETURN WVG_SetWindowPosToTop( ::hWnd )
   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD XbpCrt:unlockPS()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:winDevice()

   RETURN Self

/*----------------------------------------------------------------------*/
 *                           Callback Methods
/*----------------------------------------------------------------------*/

METHOD XbpCrt:enter( xParam )

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

METHOD XbpCrt:leave( xParam )

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

METHOD XbpCrt:lbClick( xParam )

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

METHOD XbpCrt:lbDblClick( xParam )

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

METHOD XbpCrt:lbDown( xParam )

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

METHOD XbpCrt:lbUp( xParam )

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

METHOD XbpCrt:mbClick( xParam )

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

METHOD XbpCrt:mbDblClick( xParam )

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

METHOD XbpCrt:mbDown( xParam )

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

METHOD XbpCrt:mbUp( xParam )

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

METHOD XbpCrt:motion( xParam )

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

METHOD XbpCrt:rbClick( xParam )

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

METHOD XbpCrt:rbDblClick( xParam )

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

METHOD XbpCrt:rbDown( xParam )

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

METHOD XbpCrt:rbUp( xParam )

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

METHOD XbpCrt:wheel( xParam )

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

METHOD XbpCrt:close( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_close )
      eval( ::sl_close, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_close := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:helpRequest( xParam )

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

METHOD XbpCrt:keyboard( xParam )

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

METHOD XbpCrt:killDisplayFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::sl_killDisplayFocus )
      eval( ::sl_killDisplayFocus, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_killDisplayFocus := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:killInputFocus( xParam )

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

METHOD XbpCrt:move( xParam )

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

METHOD XbpCrt:paint( xParam )

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

METHOD XbpCrt:quit( xParam, xParam1 )

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

METHOD XbpCrt:resize( xParam )

   if hb_isBlock( xParam )/* .or. hb_isNil( xParam ) */
      ::sl_resize := xParam
      RETURN NIL
   endif
   IF empty( xParam )
      //::sendMessage( WM_SIZE, 0, 0 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setDisplayFocus( xParam )

   if hb_isNil( xParam ) .and. hb_isBlock( ::setDisplayFocus )
      eval( ::setDisplayFocus, NIL, NIL, Self )
      RETURN Self
   endif

   if hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::setDisplayFocus := xParam
      RETURN NIL
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCrt:setInputFocus( xParam )

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

METHOD XbpCrt:dragEnter( xParam, xParam1 )

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

METHOD XbpCrt:dragMotion( xParam )

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

METHOD XbpCrt:dragLeave( xParam )

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

METHOD XbpCrt:dragDrop( xParam, xParam1 )

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
 *                          HARBOUR SPECIFIC
/*----------------------------------------------------------------------*/
METHOD XbpCrt:SetFocus()

   //::sendMessage( WM_ACTIVATE, 1, 0 )
   /* ::sendMessage( WM_SETFOCUS, 0, 0 ) */

   RETURN Self
/*----------------------------------------------------------------------*/
