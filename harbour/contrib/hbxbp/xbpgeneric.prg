/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                    Xbase++ Compatible Functions
 *
 *                            Pritpal Bedi
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
STATIC s_hLastEvent := {=>}

THREAD STATIC t_events

THREAD STATIC t_nEventIn  := 0
THREAD STATIC t_nEventOut := 0

THREAD STATIC t_oAppWindow

THREAD STATIC t_oEventLoop

THREAD STATIC t_oXbpInFocus

/*----------------------------------------------------------------------*/

INIT PROCEDURE hbxbp_Start()

   hbqt_errorsys()

   RETURN

/*----------------------------------------------------------------------*/

EXIT PROCEDURE hbxbp_End()
   HB_TRACE( HB_TR_DEBUG, "................................... EXIT PROCEDURE hbxbp_End()    begin " )

   t_oAppWindow := NIL

   IF hb_isObject( s_oDeskTop )
      s_oDeskTop:oWidget := NIL
   ENDIF

   HB_TRACE( HB_TR_DEBUG, "................................... EXIT PROCEDURE hbxbp_End()    end   " )
   RETURN

/*----------------------------------------------------------------------*/
/*
 * Will be called from XbpDialog() | XbpCRT()
 */
FUNCTION hbxbp_InitializeEventBuffer()

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
   LOCAL oLoop := t_oEventLoop

   IF PCount() == 1
      IF hb_isObject( oELoop ) .OR. oELoop == NIL
         t_oEventLoop := oELoop
      ENDIF
   ENDIF

   RETURN oLoop

/*----------------------------------------------------------------------*/

FUNCTION AppType()

   RETURN APPTYPE_PM

/*----------------------------------------------------------------------*/

FUNCTION PostAppEvent( nEvent, mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp2 )

   DEFAULT oXbp TO SetAppWindow()

   IF nEvent == xbeP_Keyboard
      QApplication():sendEvent( oXbp:oWidget, QKeyEvent( QEvent_KeyPress, hbxbp_appEventToQKeyEvent( mp1 ), hbxbp_appEventModifier( mp1 ) ) )
   ELSE
      SetAppEvent( nEvent, mp1, mp2, oXbp )
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/

FUNCTION LastAppEvent( mp1, mp2, oXbp, nThreadID )
   LOCAL nEvent

   DEFAULT nThreadID TO hb_threadID()

   IF hb_hHasKey( s_hLastEvent, nThreadID )
      nEvent := s_hLastEvent[ nThreadID ] [ 1 ]
      mp1    := s_hLastEvent[ nThreadID ] [ 2 ]
      mp2    := s_hLastEvent[ nThreadID ] [ 3 ]
      oXbp   := s_hLastEvent[ nThreadID ] [ 4 ]
   ENDIF

   RETURN nEvent

/*----------------------------------------------------------------------*/

FUNCTION NextAppEvent( mp1, mp2, oXbp )
   LOCAL nEvent, n

   n := t_nEventOut + 1
   IF n > EVENT_BUFFER
      n := 1
   ENDIF

   IF !empty( t_events[ n, 4 ] )
      //
      nEvent := t_events[ n, 1 ]
      mp1    := t_events[ n, 2 ]
      mp2    := t_events[ n, 3 ]
      oXbp   := t_events[ n, 4 ]
   ENDIF

   RETURN nEvent

/*----------------------------------------------------------------------*/
/*
 *  Internal to the XbpParts, must NOT be called from application code
 */
FUNCTION SetAppEvent( nEvent, mp1, mp2, oXbp )

   IF empty( t_events )
      hbxbp_InitializeEventBuffer()
   ENDIF

   IF ++t_nEventIn > EVENT_BUFFER
      t_nEventIn := 1
   ENDIF

//HB_TRACE( HB_TR_DEBUG, "SetAppEvent ... ", t_nEventIn, nEvent, oXbp:className(), oXbp:title )

   t_events[ t_nEventIn, 1 ] := nEvent
   t_events[ t_nEventIn, 2 ] := mp1
   t_events[ t_nEventIn, 3 ] := mp2
   t_events[ t_nEventIn, 4 ] := iif( empty( oXbp ), t_oAppWindow, oXbp )

//HB_TRACE( HB_TR_DEBUG, 1, "SetAppEvent ... ", hb_threadId(), nEvent )
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION AppEvent( mp1, mp2, oXbp, nTimeout )
   LOCAL nEvent := 0
   LOCAL nThreadID := hb_threadId()

   //DEFAULT nTimeout TO 0
   HB_SYMBOL_UNUSED( nTimeOut )

   IF ++t_nEventOut > EVENT_BUFFER
      t_nEventOut := 1
   ENDIF
//HB_TRACE( HB_TR_DEBUG, "            AppEvent ... ", nThreadID, t_nEventOut )
   DO WHILE !empty( t_oEventLoop ) //.t.
      t_oEventLoop:processEvents( QEventLoop_AllEvents )

      IF !empty(   t_events[ t_nEventOut, 4 ] )
         //
         nEvent := t_events[ t_nEventOut, 1 ]
         mp1    := t_events[ t_nEventOut, 2 ]
         mp2    := t_events[ t_nEventOut, 3 ]
         oXbp   := t_events[ t_nEventOut, 4 ]
         //
         t_events[ t_nEventOut, 4 ] := NIL
         EXIT
      ENDIF
      hb_releaseCPU()
   ENDDO

   s_hLastEvent[ nThreadID ] := { nEvent, mp1, mp2, oXbp }

//HB_TRACE( HB_TR_DEBUG, "..........................", hb_threadId() )

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
      oXbp:setFocus()
   ENDIF

   RETURN oldXbpInFocus

/*----------------------------------------------------------------------*/

FUNCTION AppDesktop()

   IF s_oDeskTop == NIL
      s_oDeskTop := HbpAppDesktop():new():create()
   ENDIF

   RETURN s_oDeskTop

/*----------------------------------------------------------------------*/

FUNCTION MsgBox( cMsg, cTitle )
   LOCAL oMB

   DEFAULT cTitle TO "  "

   cMsg := strtran( cMsg, chr( 13 ) + chr( 10 ), "<br />" )
   cMsg := strtran( cMsg, chr( 13 ), "<br />" )
   cMsg := strtran( cMsg, chr( 10 ), "<br />" )

   oMB := QMessageBox()
   oMB:setText( /* "<b>" + */ cMsg /* + "</b>" */ )
   oMB:setIcon( QMessageBox_Information )
   IF hb_isObject( SetAppWindow() )
      oMB:setParent( SetAppWindow():oWidget )
   ENDIF
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )

   oMB:exec()

   RETURN nil

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

FUNCTION hbxbp_getNextID( cString )

   STATIC hIDs := {=>}

   IF ! hb_hHasKey( hIDs, cString )
      hIDs[ cString ] := 0
   ENDIF

   RETURN cString + "_" + hb_ntos( ++hIDs[ cString ] )

/*----------------------------------------------------------------------*/
/*
   ConfirmBox( [<oOwner>], <cMessage>, [<cTitle>], ;
            [<nButtons>],[<nStyle>],[<nStartBtn>] ) --> nAction
*/
FUNCTION ConfirmBox( oOwner, cMessage, cTitle, nButtons, nStyle, nStartBtn )
   LOCAL oMB, nRet
   LOCAL qEnter, qCancel, qYes, qNo, qRetry, qIgnore, qAbort, qHelp, qClk, qOk

   DEFAULT oOwner    TO SetAppWindow()
   DEFAULT cTitle    TO ""
   DEFAULT nButtons  TO XBPMB_OKCANCEL
   DEFAULT nStyle    TO XBPMB_NOICON
   DEFAULT nStartBtn TO XBPMB_DEFBUTTON1

   cMessage := strtran( cMessage, chr( 13 ) + chr( 10 ), "<br />" )
   cMessage := strtran( cMessage, chr( 13 ), "<br />" )
   cMessage := strtran( cMessage, chr( 10 ), "<br />" )

   oMB := QMessageBox()
   //
   oMB:setText( /* "<b>" + */ cMessage /* + "</b>" */ )

   SWITCH nButtons
   CASE XBPMB_OK
      qOk := oMB:addButton( "Ok", QMessageBox_AcceptRole )
      EXIT
   CASE XBPMB_OKCANCEL
      qOk     := oMB:addButton( "Ok"    , QMessageBox_AcceptRole )
      qCancel := oMB:addButton( "Cancel", QMessageBox_AcceptRole )
      oMB:setDefaultButton( iif( nStartBtn == XBPMB_DEFBUTTON1, qOk, qCancel ) )
      EXIT
   CASE XBPMB_RETRYCANCEL
      qRetry  := oMB:addButton( "Retry" , QMessageBox_AcceptRole )
      qCancel := oMB:addButton( "Cancel", QMessageBox_AcceptRole )
      oMB:setDefaultButton( iif( nStartBtn == XBPMB_DEFBUTTON1, qRetry, qCancel ) )
      EXIT
   CASE XBPMB_ABORTRETRYIGNORE
      qAbort  := oMB:addButton( "Abort" , QMessageBox_AcceptRole )
      qRetry  := oMB:addButton( "Retry" , QMessageBox_AcceptRole )
      qIgnore := oMB:addButton( "Ignore", QMessageBox_AcceptRole )
      oMB:setDefaultButton( iif( nStartBtn == XBPMB_DEFBUTTON1, qAbort, ;
                                 iif( nStartBtn == XBPMB_DEFBUTTON2, qRetry, qIgnore ) ) )
      EXIT
   CASE XBPMB_YESNO
      qYes := oMB:addButton( "Yes" , QMessageBox_AcceptRole )
      qNo  := oMB:addButton( "No"  , QMessageBox_AcceptRole )
      oMB:setDefaultButton( iif( nStartBtn == XBPMB_DEFBUTTON1, qYes, qNo ) )
      EXIT
   CASE XBPMB_YESNOCANCEL
      qYes    := oMB:addButton( "Yes"   , QMessageBox_AcceptRole )
      qNo     := oMB:addButton( "No"    , QMessageBox_AcceptRole )
      qCancel := oMB:addButton( "Cancel", QMessageBox_AcceptRole )
      oMB:setDefaultButton( iif( nStartBtn == XBPMB_DEFBUTTON1, qYes, ;
                                 iif( nStartBtn == XBPMB_DEFBUTTON2, qNo, qCancel ) ) )
      EXIT
   CASE XBPMB_CANCEL
      qCancel := oMB:addButton( "Cancel", QMessageBox_AcceptRole )
      EXIT
   CASE XBPMB_ENTER
      qEnter := oMB:addButton( "Enter", QMessageBox_AcceptRole )
      EXIT
   CASE XBPMB_ENTERCANCEL
      qEnter  := oMB:addButton( "Enter" , QMessageBox_AcceptRole )
      qCancel := oMB:addButton( "Cancel", QMessageBox_AcceptRole )
      oMB:setDefaultButton( iif( nStartBtn == XBPMB_DEFBUTTON1, qEnter, qCancel ) )
      EXIT
   CASE XBPMB_HELP
      qHelp := oMB:addButton( "Help", QMessageBox_AcceptRole )
      EXIT
   OTHERWISE
      oMB:setStandardButtons( QMessageBox_Ok )
      EXIT
   ENDSWITCH

   IF hb_bitAnd( nStyle, XBPMB_INFORMATION )  == XBPMB_INFORMATION
      oMB:setIcon( QMessageBox_Information )
   ELSEIF hb_bitAnd( nStyle, XBPMB_QUESTION ) == XBPMB_QUESTION
      oMB:setIcon( QMessageBox_Question )
   ELSEIF hb_bitAnd( nStyle, XBPMB_WARNING )  == XBPMB_WARNING
      oMB:setIcon( QMessageBox_Warning )
   ELSEIF hb_bitAnd( nStyle, XBPMB_CRITICAL ) == XBPMB_CRITICAL
      oMB:setIcon( QMessageBox_Critical )
   ENDIF

   IF hb_isObject( oOwner )
      oMB:setParent( oOwner:oWidget )
   ENDIF
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )

   nRet := oMB:exec()
   qClk := oMB:clickedButton()

   SWITCH nButtons
   CASE XBPMB_OK
      nRet := iif( hbqt_IsEqual( qClk, qOk ), XBPMB_RET_OK, -1 )
      EXIT
   CASE XBPMB_OKCANCEL
      nRet := iif( hbqt_IsEqual( qClk, qOk ), XBPMB_RET_OK, XBPMB_RET_CANCEL )
      EXIT
   CASE XBPMB_RETRYCANCEL
      nRet := iif( hbqt_IsEqual( qClk, qRetry ), XBPMB_RET_RETRY, XBPMB_RET_CANCEL )
      EXIT
   CASE XBPMB_ABORTRETRYIGNORE
      nRet := iif( hbqt_IsEqual( qClk, qAbort ), XBPMB_RET_ABORT, ;
                   iif( hbqt_IsEqual( qClk, qRetry ), XBPMB_RET_RETRY, XBPMB_RET_CANCEL ) )
      EXIT
   CASE XBPMB_YESNO
      nRet := iif( hbqt_IsEqual( qClk, qYes ), XBPMB_RET_YES, XBPMB_RET_NO )
      EXIT
   CASE XBPMB_YESNOCANCEL
      nRet := iif( hbqt_IsEqual( qClk, qYes ), XBPMB_RET_YES, ;
                   iif( hbqt_IsEqual( qClk, qNo ), XBPMB_RET_NO, XBPMB_RET_CANCEL ) )
      EXIT
   CASE XBPMB_CANCEL
      nRet := iif( hbqt_IsEqual( qClk, qCancel ), XBPMB_RET_CANCEL, -1 )
      EXIT
   CASE XBPMB_ENTER
      nRet := iif( hbqt_IsEqual( qClk, qEnter ), XBPMB_RET_ENTER, -1 )
      EXIT
   CASE XBPMB_ENTERCANCEL
      nRet := iif( hbqt_IsEqual( qClk, qEnter ), XBPMB_RET_ENTER, XBPMB_RET_CANCEL )
      EXIT
   CASE XBPMB_HELP
      nRet := iif( hbqt_IsEqual( qClk, qHelp ), XBPMB_RET_OK, -1 )
      EXIT
   ENDSWITCH

   RETURN nRet

/*----------------------------------------------------------------------*/
