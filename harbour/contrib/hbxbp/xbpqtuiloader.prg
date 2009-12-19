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
 *             Xbase++ Syntax Inspired XbpQtUiLoader Class
 *
 *                 Pritpal Bedi  <pritpal@vouchcac.com>
 *                              18Decy2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpQtUiLoader INHERIT XbpWindow

   DATA     file                                  INIT ""
   DATA     modal                                 INIT .t.
   DATA     qObj                                  INIT hb_hash()
   DATA     widgets                               INIT {}

   DATA     aSignals                              INIT {}
   DATA     aEvents                               INIT {}

   METHOD   new()
   METHOD   create()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()

   METHOD   loadUI()
   METHOD   loadContents()
   METHOD   loadWidgets()
   METHOD   signal()
   METHOD   event()


   ERROR HANDLER OnError()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF !empty( ::file ) .and. file( ::file )
      ::loadContents( ::file )

      ::oWidget := ::loadUI( ::file )

      IF !empty( ::oWidget )
         ::loadWidgets()
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:destroy()
   LOCAL a_

   FOR EACH a_ IN ::aSignals
      Qt_DisConnect_Signal( a_[ 1 ], a_[ 2 ] )
   NEXT

   FOR EACH a_ IN ::aEvents
      Qt_DisConnect_Event( a_[ 1 ], a_[ 2 ] )
   NEXT

   ::oWidget:hide()
   ::oWidget:close()
   ::oWidget:pPtr := 0

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:event( cWidget, nEvent, bBlock )

   IF hb_hHasKey( ::qObj, cWidget )
      IF Qt_Connect_Signal( ::qObj[ cWidget ], nEvent, bBlock )
         aadd( ::aEvents, { ::qObj[ cWidget ], nEvent } )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:signal( cWidget, cSignal, bBlock )

   IF hb_hHasKey( ::qObj, cWidget )
      IF Qt_Connect_Signal( ::qObj[ cWidget ], cSignal, bBlock )
         aadd( ::aSignals, { ::qObj[ cWidget ], cSignal } )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:loadWidgets()
   LOCAL a_, pPtr, bBlock, x, cBlock

   FOR EACH a_ IN ::widgets
      IF a_:__enumIndex() > 1
         IF type( a_[ 1 ] + "()" ) == "UI"
            cBlock := "{|| " + a_[ 1 ] + "() }"

            pPtr := Qt_findChild( ::oWidget, a_[ 2 ] )
            bBlock := &( cBlock )

            x := eval( bBlock )
            IF hb_isObject( x )
               x:pPtr := pPtr
               ::qObj[ a_[ 2 ] ] := x
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:loadContents( cUiFull )
   LOCAL cBuffer := memoread( cUiFull )
   LOCAL n, cClass, cWidget

   DO WHILE .t.
      IF ( n := at( "widget class=", cBuffer ) ) == 0
         EXIT
      ENDIF
      cBuffer := substr( cBuffer, n + 13 )

      n := at( "name=", cBuffer )
      cClass := alltrim( strtran( substr( cBuffer, 1, n-1 ), '"', "" ) )
      cBuffer := substr( cBuffer, n + 5 )

      n := at( ">", cBuffer )
      cWidget := alltrim( strtran( substr( cBuffer, 1, n-1 ), '"', "" ) )

      HBXBP_DEBUG( pad( cClass,30 ), cWidget )

      aadd( ::widgets, { cClass, cWidget } )
   ENDDO

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpQtUiLoader:loadUI( cUiFull )
   LOCAL qWidget, qUiLoader, qFile, pWidget

   qFile := QFile():new( cUiFull )
   IF qFile:open( 1 )
      qUiLoader  := QUiLoader():new()
      pWidget    := qUiLoader:load( qFile, IF( empty( ::oParent ), NIL, ::oParent:oWidget ) )
      DO CASE
      CASE ::widgets[ 1,1 ] == "QWidget"
         qWidget    := QWidget():configure( pWidget )
      CASE ::widgets[ 1,1 ] == "QDialog"
         qWidget    := QDialog():configure( pWidget )
      CASE ::widgets[ 1,1 ] == "QMainWindow"
         qWidget    := QMainWindow():configure( pWidget )
      OTHERWISE
         qWidget    := QWidget():configure( pWidget )
      ENDCASE
      qFile:close()
   ENDIF

   RETURN qWidget

/*----------------------------------------------------------------------*/

METHOD OnError( ... )
   LOCAL cMsg
   LOCAL xReturn

   cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   HBXBP_DEBUG( "OnError", cMsg )

   xReturn := ::oWidget:&cMsg( ... )

   RETURN xReturn

/*----------------------------------------------------------------------*/

