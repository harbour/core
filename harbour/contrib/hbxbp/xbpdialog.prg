/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                  Xbase++ Compatible xbpDialog Class
 *
 *                 Pritpal Bedi <pritpal@vouchcac.com>
 *                              29May2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpDialog FROM XbpWindow

   DATA     oMenu
   DATA     aRect

   DATA     drawingArea
   DATA     tasklist                              INIT  .t.
   DATA     oEventLoop

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   handleEvent( nEvent, mp1, mp2 )       VIRTUAL
   METHOD   exeBlock( nEvent, pEvent )

   METHOD   close()                               INLINE NIL

   METHOD   showModal()
   METHOD   setTitle( cTitle )                    INLINE ::title := cTitle, ::oWidget:setWindowTitle( cTitle )
   METHOD   getTitle()                            INLINE ::oWidget:windowTitle()

   METHOD   menuBar()
   METHOD   setFrameState( nState )
   METHOD   getFrameState()
   METHOD   calcClientRect()                      INLINE { 0, 0, ::oWidget:width(), ::oWidget:height() }
   METHOD   calcFrameRect()                       INLINE { ::oWidget:x(), ::oWidget:y(), ;
                                                           ::oWidget:x()+::oWidget:width(), ::oWidget:y()+::oWidget:height() }
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpDialog:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::resizeMode  := 0
   ::mouseMode   := 0

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::cargo := hb_threadId()                               /* To Be Removed */

   /* Thread specific event buffer */
   hbxbp_InitializeEventBuffer()

   IF !empty( ::qtObject )
      IF hb_isObject( ::qtObject )
         ::oWidget := ::qtObject
      ELSE
         ::oWidget := QMainWindow()
         ::oWidget:pPtr := hbqt_ptr( ::qtObject )
      ENDIF
      ::oWidget:setMouseTracking( .t. )
   ELSE
      #ifdef __QMAINWINDOW__
      ::oWidget := QMainWindow():new()
      #else
      ::oWidget := QMainWindow():configure( QT_HBQMainWindow( {|n,p| ::grabEvent( n,p ) }, hb_threadId() ) )
      #endif
   ENDIF

   IF !empty( ::title )
      ::oWidget:setWindowTitle( ::title )
   ENDIF
   IF hb_isChar( ::icon )
      ::oWidget:setWindowIcon( ::icon )
   ENDIF

   IF !empty( ::qtObject )
      ::drawingArea := XbpDrawingArea():new( self )
      ::drawingArea:qtObject := ::oWidget:centralWidget()
      ::drawingArea:create( self, , {0,0}, ::aSize, , .t. )
   ELSE
      ::drawingArea := XbpDrawingArea():new( self, , {0,0}, ::aSize, , .t. ):create()
      ::oWidget:setCentralWidget( ::drawingArea:oWidget )
   ENDIF

   //::setQtProperty()
   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   //SetAppWindow( Self )  /* This should never be done here, it is programmers responsibility */

   /* Install Event Loop per Dialog Basis */
   ::oEventLoop := QEventLoop():new( ::pWidget )
   hbxbp_SetEventLoop( ::oEventLoop )

   /* Instal Event Filter */
   ::oWidget:installEventFilter( ::pEvents )

   ::connectWindowEvents()
   //
   ::connectEvent( ::pWidget, QEvent_Close           , {|o,e| ::exeBlock( QEvent_Close           , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_WindowActivate  , {|o,e| ::exeBlock( QEvent_WindowActivate  , e, o ) } )
   ::connectEvent( ::pWidget, QEvent_WindowDeactivate, {|o,e| ::exeBlock( QEvent_WindowDeactivate, e, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::qtObjects := pQtObject

   ::create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:exeBlock( nEvent, pEvent )
   LOCAL lRet := .F.

   HB_SYMBOL_UNUSED( pEvent )

   DO CASE
   CASE nEvent == QEvent_WindowActivate
      SetAppEvent( xbeP_SetDisplayFocus, NIL, NIL, Self )
      lRet := .T.

   CASE nEvent == QEvent_WindowDeactivate
      SetAppEvent( xbeP_KillDisplayFocus, NIL, NIL, Self )
      lRet := .T.

   CASE nEvent == QEvent_Close
      IF hb_isBlock( ::sl_close )
         lRet := eval( ::sl_close, NIL, NIL, Self )
         IF lRet
            SetAppEvent( xbeP_Close, NIL, NIL, Self )
         ENDIF
      ELSE
         SetAppEvent( xbeP_Close, NIL, NIL, Self )
      ENDIF

   ENDCASE

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD XbpDialog:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:showModal()

   ::hide()
   ::oWidget:setWindowModality( 2 )
   ::show()

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD XbpDialog:setFrameState( nState )
   LOCAL lSuccess := .T.
   LOCAL nCurState := ::getFrameState()

   DO CASE
   CASE nState == XBPDLG_FRAMESTAT_MINIMIZED
      IF nCurState != XBPDLG_FRAMESTAT_MINIMIZED
         ::oWidget:setWindowState( Qt_WindowMinimized )
      ENDIF
   CASE nState == XBPDLG_FRAMESTAT_MAXIMIZED
      IF nCurState == XBPDLG_FRAMESTAT_MINIMIZED
         ::oWidget:show()
         ::oWidget:setWindowState( Qt_WindowMaximized )
      ELSEIF nCurState == XBPDLG_FRAMESTAT_NORMALIZED
         ::oWidget:setWindowState( Qt_WindowMaximized )
      ENDIF
   CASE nState == XBPDLG_FRAMESTAT_NORMALIZED
      IF nCurState != XBPDLG_FRAMESTAT_MINIMIZED
         ::oWidget:show()
      ENDIF
      ::oWidget:setWindowState( Qt_WindowNoState )
   ENDCASE

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpDialog:getFrameState()
   LOCAL nState := ::oWidget:windowState()

   IF ( hb_bitAnd( nState, Qt_WindowMinimized ) == Qt_WindowMinimized )
      RETURN XBPDLG_FRAMESTAT_MINIMIZED
   ELSEIF ( hb_bitAnd( nState, Qt_WindowMaximized ) == Qt_WindowMaximized )
      RETURN XBPDLG_FRAMESTAT_MAXIMIZED
   ENDIF

   RETURN XBPDLG_FRAMESTAT_NORMALIZED

/*----------------------------------------------------------------------*/

METHOD XbpDialog:menuBar()

   IF !( hb_isObject( ::oMenu ) )
      ::oMenu := XbpMenuBar():New( self ):create()
   ENDIF

   RETURN ::oMenu

/*----------------------------------------------------------------------*/
/*
 *                            XbpDrawingArea
 */
/*----------------------------------------------------------------------*/

CLASS XbpDrawingArea  INHERIT  XbpWindow

   DATA     caption                               INIT ""
   DATA     clipParent                            INIT .T.
   DATA     clipSiblings                          INIT .T.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::visible := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   HB_SYMBOL_UNUSED( lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, .T. )

   IF !empty( ::qtObject )
      ::oWidget := QWidget()
      ::oWidget:pPtr := hbqt_ptr( ::qtObject )
   ELSE
      ::oWidget := QWidget():new()
   ENDIF

   ::oWidget:setMouseTracking( .T. )
   ::oWidget:setFocusPolicy( Qt_StrongFocus )

   ::setQtProperty()  /* Using it for one-to-one style sheet management */

   ::oParent:addChild( SELF )

   RETURN Self

/*----------------------------------------------------------------------*/
