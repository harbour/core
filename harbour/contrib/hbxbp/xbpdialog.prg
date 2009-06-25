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
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpDialog FROM XbpWindow

   DATA     oMenu
   DATA     aRect

   DATA     drawingArea
   DATA     tasklist                              INIT  .t.
   DATA     oEventLoop

   METHOD   init()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   showModal()                           INLINE NIL
   METHOD   setTitle( cTitle )                    INLINE ::title := cTitle, ::oWidget:setWindowTitle( cTitle )
   METHOD   getTitle()                            INLINE ::oWidget:windowTitle()

   METHOD   menuBar()
   METHOD   setFrameState( nState )
   METHOD   getFrameState()
   METHOD   calcClientRect()                      INLINE { 0, 0, ::oWidget:width(), ::oWidget:height() }
   METHOD   calcFrameRect()                       INLINE { ::oWidget:x(), ::oWidget:y(), ;
                                                           ::oWidget:x()+::oWidget:width(), ::oWidget:y()+::oWidget:height() }

   METHOD   exeBlock()
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpDialog:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::resizeMode  := 0
   ::mouseMode   := 0

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   #if 1
   ::oWidget := QMainWindow():new()
   ::oWidget:setAttribute( Qt_WA_DeleteOnClose )
   #else
   ::oWidget := QMainWindow()
   ::oWidget:pPtr := Qt_MyMainWindow()
   #endif

   IF !empty( ::title )
      ::oWidget:setWindowTitle( ::title )
   ENDIF
   IF hb_isChar( ::icon )
      ::oWidget:setWindowIcon( ::icon )
   ENDIF

   ::drawingArea := XbpDrawingArea():new( self, , {0,0}, ::aSize, , .t. ):create()
   ::oWidget:setCentralWidget( QT_PTROF( ::drawingArea:oWidget ) )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   SetAppWindow( self )

   QT_QObject_InstallEventFilter( ::pWidget, SetEventFilter() )
   ::connectEvent( ::pWidget, QEvent_Close, {|o,e| ::exeBlock( QEvent_Close, e, o ) } )
   //
   QT_QObject_InstallEventFilter( ::drawingArea:pWidget, SetEventFilter() )
   ::connectEvent( ::drawingArea:pWidget, QEvent_KeyPress , {|o,e| ::exeBlock( QEvent_KeyPress , e, o ) } )
   ::connectEvent( ::drawingArea:pWidget, QEvent_MouseMove, {|o,e| ::exeBlock( QEvent_MouseMove, e, o ) } )
   ::connectEvent( ::drawingArea:pWidget, QEvent_FocusIn  , {|o,e| ::exeBlock( QEvent_FocusIn  , e, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:exeBlock( nEvent, p1, p2 )
   LOCAL oEvent
   LOCAL lRet := .f.

   HB_SYMBOL_UNUSED( p2 )

//hb_outDebug( str( nEvent ) )

   DO CASE
   CASE nEvent == QEvent_MouseMove
      oEvent := QMouseEvent()
      oEvent:pPtr := p1
      //SetAppEvent( xbeM_Motion, { oEvent:x(), oEvent:y() }, oEvent:button(), self )

   CASE nEvent == QEvent_KeyPress
      oEvent := QKeyEvent()
      oEvent:pPtr := p1
      SetAppEvent( xbeP_Keyboard, oEvent:key, oEvent:text, self )

   CASE nEvent == QEvent_Close
      IF hb_isBlock( ::sl_close )
         lRet := eval( ::sl_close, NIL, NIL, self )
         IF lRet
            SetAppEvent( xbeP_Close, NIL, NIL, self )
         ENDIF
      ELSE
         SetAppEvent( xbeP_Close, NIL, NIL, self )
      ENDIF

   CASE nEvent == QEvent_FocusIn
      IF hb_isBlock( ::sl_setInputFocus )
         eval( ::sl_setInputFocus, NIL, NIL, self )
      ENDIF

   ENDCASE

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD XbpDialog:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:destroy()

   #if 0
   IF hb_isObject( ::oMenu )
      ::oMenu:destroy()
   ENDIF

   IF len( ::aConnections ) > 0
      aeval( ::aConnections, {|e_| Qt_DisConnect_Signal( e_[ 1 ], e_[ 2 ] ) } )
      ::aConnections := {}
   ENDIF

   IF Len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
   ENDIF

   ::oWidget:close()
   #endif

   ::xbpWindow:destroy()

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpDialog:setFrameState( nState )
   LOCAL lSuccess := .f.

   HB_SYMBOL_UNUSED( nState )
   #if 0
   DO CASE

   CASE nState == XBPDLG_FRAMESTAT_MINIMIZED
      RETURN ( ::sendMessage( QWM_SYSCOMMAND, QSC_MINIMIZE, 0 ) <> 0 )

   CASE nState == XBPDLG_FRAMESTAT_MAXIMIZED
      RETURN ( ::sendMessage( QWM_SYSCOMMAND, QSC_MAXIMIZE, 0 ) <> 0 )

   CASE nState == XBPDLG_FRAMESTAT_NORMALIZED
      RETURN ( ::sendMessage( QWM_SYSCOMMAND, QSC_RESTORE, 0 ) <> 0 )

   ENDCASE
   #endif

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpDialog:getFrameState()

   #if 0
   IF Qtc_IsIconic( ::hWnd )
      RETURN XBPDLG_FRAMESTAT_MINIMIZED
   ENDIF
   IF Qtc_IsZoomed( ::hWnd )
      RETURN XBPDLG_FRAMESTAT_MAXIMIZED
   ENDIF
   #endif

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

   METHOD   new()
   METHOD   create()
   METHOD   handleEvent()
   //METHOD   Destroy()                             INLINE ::xbpWindow:destroy()
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::visible     := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   HB_SYMBOL_UNUSED( lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, .t. )

   ::oParent:addChild( SELF )

   #if 1
   ::oWidget := QWidget():new()// ::pParent )
   ::oWidget:setMouseTracking( .t. )
   ::oWidget:setFocusPolicy( 2 )
   #else
   ::oWidget := QWidget()
   ::oWidget:pPtr := QT_MyDrawingArea()
   #endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:handleEvent( nEvent, mp1, mp2  )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   RETURN ( 1 )

/*----------------------------------------------------------------------*/
