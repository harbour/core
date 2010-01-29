/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                     Harbour Parts HbQtUI Class
 *
 *                 Pritpal Bedi <pritpal@vouchcac.com>
 *                              28Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

/*----------------------------------------------------------------------*/

CLASS HbQtUI

   DATA     pSlots
   DATA     oWidget
   DATA     qObj                                  INIT hb_hash()
   DATA     widgets                               INIT {}

   DATA     aSignals                              INIT {}
   DATA     aEvents                               INIT {}

   METHOD   new( cFile, qParent )
   METHOD   destroy()

   METHOD   event( cWidget, nEvent, bBlock )
   METHOD   signal( cWidget, cSignal, bBlock )
   METHOD   loadWidgets()
   METHOD   loadContents( cUiFull )
   METHOD   loadUI( cUiFull, qParent )

   ERROR HANDLER OnError( ... )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbQtUI:new( cFile, qParent )

   IF !empty( cFile ) .and. file( cFile )
      hb_hCaseMatch( ::qObj, .f. )

      ::loadContents( cFile, qParent )

      ::oWidget := ::loadUI( cFile, qParent )

      IF !empty( ::oWidget )
         ::loadWidgets()
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtUI:destroy()
   LOCAL a_, qObj

   ::oWidget:hide()

   FOR EACH a_ IN ::aSignals
      Qt_Slots_disConnect( ::pSlots, a_[ 1 ], a_[ 2 ] )
   NEXT
   FOR EACH a_ IN ::aEvents
      Qt_Events_disConnect( ::pEvents, a_[ 1 ], a_[ 2 ] )
   NEXT

   FOR EACH qObj IN ::qObj
      qObj:pPtr := NIL
   NEXT

   ::oWidget:close()

   ::oWidget:pPtr := NIL
   ::oWidget      := NIL
   ::aEvents      := NIL
   ::aSignals     := NIL

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbQtUI:event( cWidget, nEvent, bBlock )

   IF hb_hHasKey( ::qObj, cWidget )
      IF Qt_Events_Connect( ::pEvents, ::qObj[ cWidget ], nEvent, bBlock )
         aadd( ::aEvents, { ::qObj[ cWidget ], nEvent } )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtUI:signal( cWidget, cSignal, bBlock )

   IF hb_hHasKey( ::qObj, cWidget )
      IF empty( ::pSlots )
         ::pSlots := QT_SLOTS_NEW()
      ENDIF
      IF Qt_Slots_Connect( ::pSlots, ::qObj[ cWidget ], cSignal, bBlock )
         aadd( ::aSignals, { ::qObj[ cWidget ], cSignal } )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtUI:loadWidgets()
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

METHOD HbQtUI:loadContents( cUiFull )
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
      cWidget := strtran( cWidget, "/", "" )

      aadd( ::widgets, { cClass, cWidget } )
   ENDDO

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbQtUI:loadUI( cUiFull, qParent )
   LOCAL oWidget, qUiLoader, qFile, pWidget
   #if 1
   LOCAL cBuffer

   /* This method allows to use ui components stored in a
    * database or embedded into sources as a text file stream
    */
   cBuffer := hb_memoRead( cUiFull )
   qFile := QBuffer():new()
   qFile:setData( cBuffer, len( cBuffer ) )
   #else
   qFile := QFile():new( cUiFull )
   #endif
   IF qFile:open( 1 )
      qUiLoader  := QUiLoader():new()
      pWidget    := qUiLoader:load( qFile, qParent )
      DO CASE
      CASE ::widgets[ 1,1 ] == "QWidget"
         oWidget    := QWidget():configure( pWidget )
      CASE ::widgets[ 1,1 ] == "QDialog"
         oWidget    := QDialog():configure( pWidget )
      CASE ::widgets[ 1,1 ] == "QMainWindow"
         oWidget    := QMainWindow():configure( pWidget )
      OTHERWISE
         oWidget    := QWidget():configure( pWidget )
      ENDCASE
      qFile:close()
      qFile:pPtr := NIL
      qUiLoader:pPtr := NIL
   ENDIF

   RETURN oWidget

/*----------------------------------------------------------------------*/

METHOD HbQtUI:OnError( ... )
   LOCAL cMsg, xReturn

   cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   IF left( cMsg, 2 ) == "Q_"
      xReturn := ::qObj[ substr( cMsg, 3 ) ]
   ELSE
      xReturn := ::oWidget:&cMsg( ... )
   ENDIF

   RETURN xReturn

/*----------------------------------------------------------------------*/

