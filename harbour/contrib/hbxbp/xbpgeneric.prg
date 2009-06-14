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
 *                     Xbase++ Compatible Function
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              08Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"

#include "xbp.ch"
#include "apig.ch"

/*----------------------------------------------------------------------*/

#define EVENT_BUFFER    200

THREAD STATIC ts_events
THREAD STATIC nEventIn  := 0
THREAD STATIC nEventOut := 0
THREAD STATIC ts_mutex

STATIC oDummy

/*----------------------------------------------------------------------*/

INIT PROCEDURE Qt_Start()
   qt_qapplication()
   RETURN

EXIT PROCEDURE Qt_End()
   qt_qapplication_quit()
   RETURN

/*----------------------------------------------------------------------*/
/*
 *  Internal to the XbpParts , Must NOT be called from Application Code
 */
FUNCTION SetAppEvent( nEvent, mp1, mp2, oXbp )

   //hb_mutexLock( ts_mutex )
   IF ++nEventIn > EVENT_BUFFER
      nEventIn := 1
   ENDIF

   ts_events[ nEventIn ] := { nEvent, mp1, mp2, oXbp }

   //hb_mutexUnLock( ts_mutex )
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION AppEvent( mp1, mp2, oXbp )
   LOCAL nEvent

   SetAppWindow():oEventLoop:processEvents()

   //hb_mutexLock( ts_mutex )

   IF ++nEventOut > EVENT_BUFFER
      nEventOut := 1
   ENDIF

   IF empty( ts_events[ nEventOut ] )
      nEvent := 0
      mp1    := NIL
      mp2    := NIL
      oXbp   := oDummy
   ELSE
      nEvent := ts_events[ nEventOut,1 ]
      mp1    := ts_events[ nEventOut,2 ]
      mp2    := ts_events[ nEventOut,3 ]
      oXbp   := ts_events[ nEventOut,4 ]
      ts_events[ nEventOut ] := NIL
   ENDIF

   //hb_mutexUnLock( ts_mutex )
   RETURN nEvent

/*----------------------------------------------------------------------*/

FUNCTION SetAppWindow( oXbp )
   LOCAL oldAppWindow

   THREAD STATIC oAppWindow

   IF empty( ts_mutex )
      ts_mutex := hb_mutexCreate()
   ENDIF
   IF empty( ts_events )
      ts_events := array( EVENT_BUFFER )
   ENDIF
   IF empty( oDummy )
      oDummy := XbpObject():new()
   ENDIF

   oldAppWindow := oAppWindow

   IF hb_isObject( oXbp )
      oAppWindow := oXbp
   ENDIF

   RETURN oldAppWindow

/*----------------------------------------------------------------------*/

FUNCTION SetAppFocus( oXbp )
   LOCAL oldXbpInFocus

   THREAD STATIC oXbpInFocus

   oldXbpInFocus := oXbpInFocus

   IF hb_isObject( oXbp )
      oXbpInFocus := oXbp
      oXbp:oWidget:setFocus()
   ENDIF

   RETURN oldXbpInFocus

/*----------------------------------------------------------------------*/

FUNCTION AppDesktop()

   STATIC oDeskTop

   IF oDeskTop == NIL
      oDeskTop := XbpWindow():new()
      oDeskTop:oWidget := QDesktopWidget():new()
   ENDIF

   RETURN oDeskTop

/*----------------------------------------------------------------------*/

FUNCTION MsgBox( cMsg, cTitle )
   LOCAL oMB

   DEFAULT cTitle TO ""

   oMB := QMessageBox():new()
   oMB:setInformativeText( cMsg )
   IF !empty( cTitle )
      oMB:setWindowTitle( cTitle )
   ENDIF
   oMB:show()

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION GraMakeRGBColor( aRGB )
   LOCAL nRGB

   IF hb_isArray( aRGB ) .and. len( aRGB ) == 3
      IF hb_isNumeric( aRGB[ 1 ] ) .and. ( aRGB[ 1 ] >= 0 ) .and. ( aRGB[ 1 ] <= 255 )
         IF hb_isNumeric( aRGB[ 2 ] ) .and. ( aRGB[ 2 ] >= 0 ) .and. ( aRGB[ 2 ] <= 255 )
            IF hb_isNumeric( aRGB[ 3 ] ) .and. ( aRGB[ 3 ] >= 0 ) .and. ( aRGB[ 3 ] <= 255 )
               nRGB := ( aRGB[ 1 ] + ( aRGB[ 2 ] * 256 ) + ( aRGB[ 3 ] * 256 * 256 ) ) + ( 256 * 256 * 256 )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN nRGB
