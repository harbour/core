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
STATIC s_oApp

THREAD STATIC t_events

THREAD STATIC t_nEventIn  := 0
THREAD STATIC t_nEventOut := 0

THREAD STATIC t_oAppWindow

THREAD STATIC t_oEventLoop

THREAD STATIC t_qtSlots
THREAD STATIC t_qtEvents

THREAD STATIC t_oXbpInFocus

/*----------------------------------------------------------------------*/

INIT PROCEDURE hbxbp_Start()

   hbqt_errorsys()

   s_oApp := QApplication():new()

   RETURN

/*----------------------------------------------------------------------*/

EXIT PROCEDURE hbxbp_End()

   t_oAppWindow := NIL

   IF hb_isObject( s_oDeskTop )
      s_oDeskTop:oWidget:pPtr := 0
   endif

   s_oApp:quit()

   RETURN

/*----------------------------------------------------------------------*/

FUNCTION hbxbp_getSlotsPtr()
   RETURN t_qtSlots

/*----------------------------------------------------------------------*/

FUNCTION hbxbp_getEventsPtr()
   RETURN t_qtEvents

/*----------------------------------------------------------------------*/
/*
 * Will be called from XbpDialog() | XbpCRT()
 */
FUNCTION hbxbp_InitializeEventBuffer()

   IF empty( t_qtSlots )
      t_qtSlots := Qt_Slots_New()
   ENDIF

   IF empty( t_qtEvents )
      t_qtEvents := Qt_Events_New()
   ENDIF

   IF empty( t_events )
      t_events := array( EVENT_BUFFER )
      aeval( t_events, {|e,i| HB_SYMBOL_UNUSED( e ), t_events[ i ] := { 0, NIL, NIL, NIL } } )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbxbp_ClearEventBuffer()

   IF !empty( t_events )
      aeval( t_events, {|e,i| HB_SYMBOL_UNUSED( e ), t_events[ i ] := NIL } )
      t_events := NIL
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbxbp_SetEventLoop( oELoop )

   t_oEventLoop := oELoop

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION PostAppEvent( nEvent, mp1, mp2, oXbp )

   SetAppEvent( nEvent, mp1, mp2, oXbp )

   RETURN .T.

/*----------------------------------------------------------------------*/

/*
 *  Internal to the XbpParts, must NOT be called from application code
 */
FUNCTION SetAppEvent( nEvent, mp1, mp2, oXbp )

   IF ++t_nEventIn > EVENT_BUFFER
      t_nEventIn := 1
   ENDIF

//HBXBP_DEBUG( 0, "SetAppEvent ... ", hb_threadId(), nEvent, xbeP_Paint )

   t_events[ t_nEventIn, 1 ] := nEvent
   t_events[ t_nEventIn, 2 ] := mp1
   t_events[ t_nEventIn, 3 ] := mp2
   t_events[ t_nEventIn, 4 ] := oXbp

//HBXBP_DEBUG( 1, "SetAppEvent ... ", hb_threadId(), nEvent )
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION AppEvent( mp1, mp2, oXbp, nTimeout )
   LOCAL nEvent
   //LOCAL nThreadID := hb_threadId()

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
//HBXBP_DEBUG( "..........................", hb_threadId() )

   RETURN nEvent

/*----------------------------------------------------------------------*/

FUNCTION SetAppWindow( oXbp )
   LOCAL oldAppWindow := t_oAppWindow

   IF hb_isObject( oXbp )
      t_oAppWindow := oXbp
   ENDIF

   RETURN oldAppWindow

/*----------------------------------------------------------------------*/

FUNCTION SetAppFocus( oXbp )
   LOCAL oldXbpInFocus

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

   cMsg := strtran( cMsg, chr( 13 ) + chr( 10 ), "<br />" )
   cMsg := strtran( cMsg, chr( 13 ), "<br />" )
   cMsg := strtran( cMsg, chr( 10 ), "<br />" )

   oMB := QMessageBox():new()
   oMB:setText( /* "<b>" + */ cMsg /* + "</b>" */ )
   oMB:setIcon( QMessageBox_Information )
   IF hb_isObject( SetAppWindow() )
      oMB:setParent( SetAppWindow():pWidget )
   ENDIF
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

FUNCTION hbxbp_ConvertAFactFromXBP( cMode, xValue )

   SWITCH Upper( cMode )
   CASE "COLOR"
      SWITCH xValue
      CASE GRA_CLR_WHITE     ; RETURN Qt_white
      CASE GRA_CLR_BLACK     ; RETURN Qt_black
      CASE GRA_CLR_BLUE      ; RETURN Qt_blue
      CASE GRA_CLR_RED       ; RETURN Qt_red
      CASE GRA_CLR_PINK      ; RETURN Qt_magenta
      CASE GRA_CLR_GREEN     ; RETURN Qt_green
      CASE GRA_CLR_CYAN      ; RETURN Qt_cyan
      CASE GRA_CLR_YELLOW    ; RETURN Qt_yellow
      CASE GRA_CLR_DARKGRAY  ; RETURN Qt_darkGray
      CASE GRA_CLR_DARKBLUE  ; RETURN Qt_darkBlue
      CASE GRA_CLR_DARKRED   ; RETURN Qt_darkRed
      CASE GRA_CLR_DARKPINK  ; RETURN Qt_darkMagenta
      CASE GRA_CLR_DARKGREEN ; RETURN Qt_darkGreen
      CASE GRA_CLR_DARKCYAN  ; RETURN Qt_darkCyan
      CASE GRA_CLR_BROWN     ; RETURN Qt_darkYellow
      CASE GRA_CLR_PALEGRAY  ; RETURN Qt_lightGray
      ENDSWITCH
      EXIT

   CASE "RTFVERTICALALIGN"
      SWITCH xValue
      CASE  0; RETURN QTextCharFormat_AlignNormal
      CASE  1; RETURN QTextCharFormat_AlignSuperScript
      CASE -1; RETURN QTextCharFormat_AlignSubScript
      ENDSWITCH
      EXIT

   CASE "RTFSELALIGNMENT"
      SWITCH xValue
      CASE XBPRTF_ALIGN_LEFT  ; RETURN Qt_AlignLeft
      CASE XBPRTF_ALIGN_RIGHT ; RETURN Qt_AlignRight
      CASE XBPRTF_ALIGN_CENTER; RETURN Qt_AlignCenter
      ENDSWITCH
      EXIT

   CASE "ALIGNMENT"
      SWITCH xValue
      CASE XBPALIGN_TOP    ; RETURN Qt_AlignTop
      CASE XBPALIGN_BOTTOM ; RETURN Qt_AlignBottom
      CASE XBPALIGN_LEFT   ; RETURN Qt_AlignLeft
      CASE XBPALIGN_RIGHT  ; RETURN Qt_AlignRight
      CASE XBPALIGN_VCENTER; RETURN Qt_AlignVCenter
      CASE XBPALIGN_HCENTER; RETURN Qt_AlignHCenter
      ENDSWITCH
      EXIT

   CASE "BRWNAVIGATE"
      SWITCH xValue
      CASE xbeK_DOWN ; RETURN QAbstractItemView_MoveDown
      CASE xbeK_UP   ; RETURN QAbstractItemView_MoveUp
      CASE xbeK_RIGHT; RETURN QAbstractItemView_MoveRight
      CASE xbeK_LEFT ; RETURN QAbstractItemView_MoveLeft
      CASE xbeK_HOME ; RETURN QAbstractItemView_MoveHome
      CASE xbeK_END  ; RETURN QAbstractItemView_MoveEnd
      CASE xbeK_PGUP ; RETURN QAbstractItemView_MovePageUp
      CASE xbeK_PGDN ; RETURN QAbstractItemView_MovePageDown
      ENDSWITCH
      EXIT

   ENDSWITCH

   RETURN xValue

/*----------------------------------------------------------------------*/

FUNCTION hbxbp_Just()
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION HbXbp_SetCodec( cCodec )
   IF !empty( cCodec )
      hbqt_SetCodecForCStrings( cCodec )
   ENDIF
   RETURN NIL

/*----------------------------------------------------------------------*/
