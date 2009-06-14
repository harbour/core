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
#include "apig.ch"
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
   METHOD   setTitle( cTitle )                    INLINE ::title := cTitle, hb_gtInfo( HB_GTI_WINTITLE, cTitle )
   METHOD   getTitle()                            INLINE hb_gtInfo( HB_GTI_WINTITLE )

   METHOD   menuBar()
   METHOD   setFrameState( nState )
   METHOD   getFrameState()
   METHOD   calcClientRect()                      INLINE ::aRect := Qtc_GetClientRect( ::hWnd ), ;
                                                         { 0, 0, ::aRect[ 3 ], ::aRect[ 4 ] }
   METHOD   calcFrameRect()                       INLINE ::aRect := Qtc_GetWindowRect( ::hWnd ),;
                                                         { ::aRect[ 1 ], ::aRect[ 2 ], ;
                                                         ::aRect[ 3 ]-::aRect[ 1 ], ::aRect[ 4 ]-::aRect[ 2 ] }
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpDialog:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::className   := "XbpDialog"
   ::resizeMode  := 0
   ::mouseMode   := 0
   ::objType     := objTypeDialog

   ::style       := QWS_THICKFRAME+QWS_OVERLAPPED+QWS_CAPTION+QWS_SYSMENU+QWS_MINIMIZEBOX+QWS_MAXIMIZEBOX

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QMainWindow()
   ::oWidget:pPtr := Qt_MyMainWindow()

   IF !empty( ::title )
      ::oWidget:setWindowTitle( ::title )
   ENDIF
   IF hb_isChar( ::icon )
      ::oWidget:setWindowIcon( ::icon )
   ENDIF

   ::drawingArea := XbpDrawingArea():new( self, , {0,0}, ::aSize, , .t. ):create()
   ::oWidget:setCentralWidget( QT_PTROF( ::drawingArea:oWidget ) )

   ::oWidget:resize( ::aSize[ 1 ], ::aSize[ 2 ] )
   ::oWidget:move( ::aPos[ 1 ], ::aPos[ 2 ] )

   IF ::visible
      ::lHasInputFocus := .t.
      ::oWidget:show()
   ENDIF

   ::oEventLoop := QEventLoop():new( QT_PTROF( ::drawingArea:oWidget ) )

   SetAppWindow( self )

   Qt_Connect_Signal( QT_PTROF( ::drawingArea:oWidget ), "keyPressEvent()" , {|o,pEvent| ::grabEvent( pEvent, o ) } )
   Qt_Connect_Signal( QT_PTROF( ::drawingArea:oWidget ), "mouseMoveEvent()", {|o,pEvent| ::grabEvent( pEvent, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDialog:destroy()

   ::oEventLoop:exit()

   IF hb_isObject( ::oMenu )
      ::oMenu:destroy()
   ENDIF

   IF Len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
   ENDIF

   ::oWidget:close()

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpDialog:setFrameState( nState )
   LOCAL lSuccess := .f.

   DO CASE

   CASE nState == XBPDLG_FRAMESTAT_MINIMIZED
      RETURN ( ::sendMessage( QWM_SYSCOMMAND, QSC_MINIMIZE, 0 ) <> 0 )

   CASE nState == XBPDLG_FRAMESTAT_MAXIMIZED
      RETURN ( ::sendMessage( QWM_SYSCOMMAND, QSC_MAXIMIZE, 0 ) <> 0 )

   CASE nState == XBPDLG_FRAMESTAT_NORMALIZED
      RETURN ( ::sendMessage( QWM_SYSCOMMAND, QSC_RESTORE, 0 ) <> 0 )

   ENDCASE

   RETURN lSuccess

/*----------------------------------------------------------------------*/

METHOD XbpDialog:getFrameState()

   IF Qtc_IsIconic( ::hWnd )
      RETURN XBPDLG_FRAMESTAT_MINIMIZED
   ENDIF
   IF Qtc_IsZoomed( ::hWnd )
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

   METHOD   new()
   METHOD   create()
   METHOD   destroy()
   METHOD   handleEvent()
   METHOD   setColorFG( nRGB )                    INLINE ::oParent:setColorFG( nRGB )
   METHOD   setColorBG( nRGB )                    INLINE ::oParent:setColorBG( nRGB )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::exStyle     := 0
   ::className   := "XbpDrawingArea"
   ::objType     := objTypeDA
   ::visible     := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   HB_SYMBOL_UNUSED( lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, .t. )

   ::oParent:addChild( SELF )

   ::oWidget := QWidget()
   ::oWidget:pPtr := QT_MyDrawingArea()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:handleEvent( nEvent, mp1, mp2  )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   RETURN ( 1 )

/*----------------------------------------------------------------------*/

METHOD XbpDrawingArea:destroy()

   hb_ToOutDebug( "          %s:destroy()", __objGetClsName( self ) )

   ::oWidget:close()

   RETURN NIL

/*----------------------------------------------------------------------*/
