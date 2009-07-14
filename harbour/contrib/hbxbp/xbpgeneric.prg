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
#include "gra.ch"

#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define EVENT_BUFFER    200

/*----------------------------------------------------------------------*/

STATIC ts_mutex
STATIC oApp

THREAD STATIC aEventLoop := {}

//STATIC ts_events
THREAD STATIC ts_events

THREAD STATIC nEventIn  := 0
THREAD STATIC nEventOut := 0
THREAD STATIC oDummy

THREAD STATIC oAppWindow

THREAD STATIC sEventFilter
THREAD STATIC oEventLoop

/*----------------------------------------------------------------------*/

INIT PROCEDURE Qt_Start()

   ts_mutex  := hb_mutexCreate()
   oDummy    := XbpObject():new()

   qt_qapplication()
   oApp := QApplication():new()
   RETURN

/*----------------------------------------------------------------------*/

EXIT PROCEDURE Qt_End()
   qt_qapplication_quit()
   RETURN

/*----------------------------------------------------------------------*/
/*
 * Will be called from XbpDialog() | XbpCRT()
 */
FUNCTION InitializeEventBuffer()

   IF empty( ts_events )
      ts_events := array( EVENT_BUFFER )
      aeval( ts_events, {|e,i| e := e, ts_events[ i ] := { 0, NIL, NIL, NIL, -9999 } } )
   ENDIF
//xbp_debug( len( ts_events ) )
   RETURN nil

/*----------------------------------------------------------------------*/
/*
 *  Internal to the XbpParts , Must NOT be called from Application Code
 */
FUNCTION SetAppEvent( nEvent, mp1, mp2, oXbp )

   IF ++nEventIn > EVENT_BUFFER
      nEventIn := 1
   ENDIF

//xbp_debug( "SetAppEvent ... ", threadID(), nEventIn )

   ts_events[ nEventIn,1 ] := nEvent
   ts_events[ nEventIn,2 ] := mp1
   ts_events[ nEventIn,3 ] := mp2
   ts_events[ nEventIn,4 ] := oXbp
   ts_events[ nEventIn,5 ] := ThreadID()

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION AppEvent( mp1, mp2, oXbp, nTimeout )
   LOCAL nEvent //, n

   DEFAULT nTimeout TO 0

   IF ++nEventOut > EVENT_BUFFER
      nEventOut := 1
   ENDIF
//xbp_debug( "            AppEvent ... ", threadID(), nEventOut )
   DO WHILE .t.
      aEventLoop[ 1,1 ]:processEvents_1( 0, 200 )

      IF ts_events[ nEventOut,5 ] == ThreadID()
         nEvent := ts_events[ nEventOut,1 ]
         mp1    := ts_events[ nEventOut,2 ]
         mp2    := ts_events[ nEventOut,3 ]
         oXbp   := ts_events[ nEventOut,4 ]

         ts_events[ nEventOut,5 ] := -9999  /* an arbirary value never reached by ThreadID() */

         EXIT
      ENDIF

      hb_idleSleep( 0.01 )                  /* Releases CPU cycles */
   ENDDO
//( "..........................", threadID() )

   RETURN nEvent

/*----------------------------------------------------------------------*/

FUNCTION SetAppWindow( oXbp )
   LOCAL oldAppWindow

   //hb_outDebug( str( threadId() )+'  0' )

   oldAppWindow := oAppWindow

   IF hb_isObject( oXbp )
      oAppWindow := oXbp
   ENDIF

   //hb_outDebug( str( threadId() )+'  1' )

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

   DEFAULT cTitle TO "  "

   cMsg := strtran( cMsg, chr( 13 )+chr( 10 ), "<BR>" )
   cMsg := strtran( cMsg, chr( 13 ), "<BR>" )
   cMsg := strtran( cMsg, chr( 10 ), "<BR>" )

   oMB := QMessageBox():new()
   oMB:setText( "<b>"+ cMsg +"</b>" )
   oMB:setIcon( QMessageBox_Information )
   oMB:setParent( SetAppWindow():pWidget )
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )
   SetAppWindow():oWidget:setFocus()
   oMB:exec()

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

/*----------------------------------------------------------------------*/

FUNCTION SetEventFilter()
   LOCAL pEventFilter

   IF empty( sEventFilter )
      pEventFilter := QT_QEventFilter()
      IF hb_isPointer( pEventFilter )
         sEventFilter := pEventFilter
      ENDIF
   ENDIF

   RETURN sEventFilter

/*----------------------------------------------------------------------*/

FUNCTION AddEventLoop( oEventLoop )

   //hb_mutexLock( ts_mutex )

   aadd( aEventLoop, { oEventLoop, threadID() } )

   //hb_mutexUnLock( ts_mutex )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION RemoveEventLoop( oEventLoop )
   LOCAL n

   n := ascan( aEventLoop, {|o_| o_[ 1 ] == oEventLoop } )
   IF n > 0
      hb_mutexLock( ts_mutex )

      aEventLoop[ n,1 ] := NIL
      aEventLoop[ n,2 ] := 0

      hb_mutexUnLock( ts_mutex )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

#define CRLF   chr( 13 )+chr( 10 )

FUNCTION Xbp_Debug( ... )
   LOCAL s  := ""
   LOCAL aP := hb_aParams()

   aeval( aP, {|e| s += Xbp_XtoS( e ) + ' ' } )
   hb_outDebug( s )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Xbp_XtoS( xVar )
   LOCAL cType

   cType := valtype( xVar )
   DO CASE
   CASE cType == "N"
      RETURN str( xVar )
   CASE cType == "D"
      RETURN dtoc( xVar )
   CASE cType == "L"
      RETURN IF( xVar, "Yes", "No" )
   CASE cType == "M"
      RETURN xVar
   CASE cType == "C"
      RETURN xVar
   CASE cType == "A"
      RETURN "A:"+hb_ntos( len( xVar ) )
   CASE cType == "O"
      RETURN "<OBJECT>"
   OTHERWISE
      RETURN "<"+cType+">"
   ENDCASE

   RETURN xVar

/*----------------------------------------------------------------------*/

FUNCTION ConvertAFact( cMode, nFrom, xValue )
   LOCAL n, a_:= {}

   cMode := upper( cMode )

   DO CASE
   CASE cMode == "COLOR"
      aadd( a_, { GRA_CLR_WHITE     , Qt_white         } )
      aadd( a_, { GRA_CLR_BLACK     , Qt_black         } )
      aadd( a_, { GRA_CLR_BLUE      , Qt_blue          } )
      aadd( a_, { GRA_CLR_RED       , Qt_red           } )
      aadd( a_, { GRA_CLR_PINK      , Qt_magenta       } )
      aadd( a_, { GRA_CLR_GREEN     , Qt_green         } )
      aadd( a_, { GRA_CLR_CYAN      , Qt_cyan          } )
      aadd( a_, { GRA_CLR_YELLOW    , Qt_yellow        } )
      aadd( a_, { GRA_CLR_DARKGRAY  , Qt_darkGray      } )
      aadd( a_, { GRA_CLR_DARKBLUE  , Qt_darkBlue      } )
      aadd( a_, { GRA_CLR_DARKRED   , Qt_darkRed       } )
      aadd( a_, { GRA_CLR_DARKPINK  , Qt_darkMagenta   } )
      aadd( a_, { GRA_CLR_DARKGREEN , Qt_darkGreen     } )
      aadd( a_, { GRA_CLR_DARKCYAN  , Qt_darkCyan      } )
      aadd( a_, { GRA_CLR_BROWN     , Qt_darkYellow    } )
      aadd( a_, { GRA_CLR_PALEGRAY  , Qt_lightGray     } )

   CASE cMode == "RTFVERTICALALIGN"
      aadd( a_, {  0, QTextCharFormat_AlignNormal      } )
      aadd( a_, {  1, QTextCharFormat_AlignSuperScript } )
      aadd( a_, { -1, QTextCharFormat_AlignSubScript   } )

   CASE cMode == "RTFSELALIGNMENT"
      aadd( a_, { XBPRTF_ALIGN_LEFT  , Qt_AlignLeft    } )
      aadd( a_, { XBPRTF_ALIGN_RIGHT , Qt_AlignRight   } )
      aadd( a_, { XBPRTF_ALIGN_CENTER, Qt_AlignCenter  } )

   CASE cMode == "SOMEOTHER"

   ENDCASE

   IF ( n := ascan( a_, {|e_| e_[ nFrom ] == xValue } ) ) > 0
      RETURN a_[ n, IF( nFrom == 2, 1, 2 ) ]
   ENDIF

   RETURN xValue

/*----------------------------------------------------------------------*/
