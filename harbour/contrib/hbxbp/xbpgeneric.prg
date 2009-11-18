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
#include "appevent.ch"

/*----------------------------------------------------------------------*/

#define EVENT_BUFFER    200

/*----------------------------------------------------------------------*/

STATIC s_oDeskTop
STATIC s_mutex := hb_mutexCreate()
STATIC s_oApp

THREAD STATIC t_aEventLoop

THREAD STATIC t_events

THREAD STATIC t_nEventIn  := 0
THREAD STATIC t_nEventOut := 0
THREAD STATIC t_oDummy

THREAD STATIC t_oAppWindow

THREAD STATIC t_oEventLoop

/*----------------------------------------------------------------------*/

INIT PROCEDURE Qt_Start()

   Qt_MutexCreate()

   t_oDummy := XbpObject():new()

   s_oApp   := QApplication():new()

   RETURN

/*----------------------------------------------------------------------*/

EXIT PROCEDURE Qt_End()

   Qt_MutexDestroy()

   t_oDummy      := NIL
   t_oAppWindow  := NIL

   IF hb_isObject( s_oDeskTop )
      s_oDeskTop:oWidget:pPtr := 0
   endif

   s_oApp:quit()
   #if 0
   s_oApp:oWidget:pPtr := 0
   #endif
   RETURN

/*----------------------------------------------------------------------*/
/*
 * Will be called from XbpDialog() | XbpCRT()
 */
FUNCTION InitializeEventBuffer()

   IF empty( t_events )
      t_events := array( EVENT_BUFFER )
      aeval( t_events, {|e,i| e := e, t_events[ i ] := { 0, NIL, NIL, NIL } } )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION ClearEventBuffer()

   IF !empty( t_events )
      aeval( t_events, {|e,i| e := e, t_events[ i ] := NIL } )
      t_events := NIL
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION SetEventFilter()

   RETURN QT_QEventFilter()

/*----------------------------------------------------------------------*/

FUNCTION SetEventLoop( oELoop )

   t_oEventLoop := oELoop

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION PostAppEvent( nEvent, mp1, mp2, oXbp )
   LOCAL lSuccess := .T.

   SetAppEvent( nEvent, mp1, mp2, oXbp )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

/*
 *  Internal to the XbpParts , Must NOT be called from Application Code
 */
FUNCTION SetAppEvent( nEvent, mp1, mp2, oXbp )

   IF ++t_nEventIn > EVENT_BUFFER
      t_nEventIn := 1
   ENDIF

//HBXBP_DEBUG( 0, "SetAppEvent ... ", threadID(), nEvent, xbeP_Paint )

   t_events[ t_nEventIn, 1 ] := nEvent
   t_events[ t_nEventIn, 2 ] := mp1
   t_events[ t_nEventIn, 3 ] := mp2
   t_events[ t_nEventIn, 4 ] := oXbp

//HBXBP_DEBUG( 1, "SetAppEvent ... ", threadID(), nEvent )
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION AppEvent( mp1, mp2, oXbp, nTimeout )
   LOCAL nEvent
   //LOCAL nThreadID := ThreadID()

   //DEFAULT nTimeout TO 0
   HB_SYMBOL_UNUSED( nTimeOut )

   IF ++t_nEventOut > EVENT_BUFFER
      t_nEventOut := 1
   ENDIF
//HBXBP_DEBUG( "            AppEvent ... ", nThreadID, t_nEventOut )
   DO WHILE !empty( t_oEventLoop ) //.t.
      t_oEventLoop:processEvents( QEventLoop_AllEvents )

      IF !empty( t_events[ t_nEventOut, 4 ] )
         nEvent := t_events[ t_nEventOut, 1 ]
         mp1    := t_events[ t_nEventOut, 2 ]
         mp2    := t_events[ t_nEventOut, 3 ]
         oXbp   := t_events[ t_nEventOut, 4 ]
         t_events[ t_nEventOut, 4 ] := NIL
         EXIT
      ENDIF
      hb_releaseCPU()
   ENDDO
//HBXBP_DEBUG( "..........................", threadID() )

   RETURN nEvent

/*----------------------------------------------------------------------*/

FUNCTION SetAppWindow( oXbp )
   LOCAL oldAppWindow

   oldAppWindow := t_oAppWindow

   IF hb_isObject( oXbp )
      t_oAppWindow := oXbp
   ENDIF

   RETURN oldAppWindow

/*----------------------------------------------------------------------*/

FUNCTION SetAppFocus( oXbp )
   LOCAL oldXbpInFocus

   THREAD STATIC t_oXbpInFocus

   oldXbpInFocus := t_oXbpInFocus

   IF hb_isObject( oXbp )
      t_oXbpInFocus := oXbp
      oXbp:oWidget:setFocus()
   ENDIF

   RETURN oldXbpInFocus

/*----------------------------------------------------------------------*/

FUNCTION AppDesktop()

   IF s_oDeskTop == NIL
      s_oDeskTop := XbpWindow():new()
      s_oDeskTop:oWidget := QDesktopWidget():new()
   ENDIF

   RETURN s_oDeskTop

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

   CASE cMode == "ALIGNMENT"
      aadd( a_, { XBPALIGN_TOP    , Qt_AlignTop        } )
      aadd( a_, { XBPALIGN_BOTTOM , Qt_AlignBottom     } )
      aadd( a_, { XBPALIGN_LEFT   , Qt_AlignLeft       } )
      aadd( a_, { XBPALIGN_RIGHT  , Qt_AlignRight      } )
      aadd( a_, { XBPALIGN_VCENTER, Qt_AlignVCenter    } )
      aadd( a_, { XBPALIGN_HCENTER, Qt_AlignHCenter    } )

   CASE cMode == "BRWNAVIGATE"
      aadd( a_, { xbeK_DOWN , QAbstractItemView_MoveDown      } )
      aadd( a_, { xbeK_UP   , QAbstractItemView_MoveUp        } )
      aadd( a_, { xbeK_RIGHT, QAbstractItemView_MoveRight     } )
      aadd( a_, { xbeK_LEFT , QAbstractItemView_MoveLeft      } )
      aadd( a_, { xbeK_HOME , QAbstractItemView_MoveHome      } )
      aadd( a_, { xbeK_END  , QAbstractItemView_MoveEnd       } )
      aadd( a_, { xbeK_PGUP , QAbstractItemView_MovePageUp    } )
      aadd( a_, { xbeK_PGDN , QAbstractItemView_MovePageDown  } )

   CASE cMode == "SOMEOTHER"

   ENDCASE

   IF ( n := ascan( a_, {|e_| e_[ nFrom ] == xValue } ) ) > 0
      RETURN a_[ n, IF( nFrom == 2, 1, 2 ) ]
   ENDIF

   RETURN xValue

/*----------------------------------------------------------------------*/

#if defined( __HB_DEBUG__ )

PROCEDURE _HBXBP_DEBUG( ... )
   LOCAL cString := ""

   //AEval( hb_AParams(), {| x | cString += ValType( x ) + ":" + iif( ISARRAY( x ), "[" + hb_ntos( Len( x ) ) + "]", hb_ValToStr( x ) ) + " " } )
   AEval( hb_AParams(), {| x | cString += iif( ISARRAY( x ), "[" + hb_ntos( Len( x ) ) + "]", hb_ValToStr( x ) ) + "   " } )

   #if defined( __PLATFORM__WINDOWS ) .AND. defined( __HB_WINDEBUG__ )
      wapi_OutputDebugString( cString )
   #else
      hb_TraceString( cString )
   #endif

   RETURN

#endif

/*----------------------------------------------------------------------*/
