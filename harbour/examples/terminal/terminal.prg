/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                       Terminal Server Application
//
//                   Pritpal Bedi (pritpal@vouchcac.com)
//                               13 Feb 2009
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include "common.ch"

//#include "wvtwin.ch"

#define WSABASEERR              10000
#define WSAECONNABORTED         (WSABASEERR+53)
#define WSAECONNRESET           (WSABASEERR+54)

//----------------------------------------------------------------------//

#define TOP                             t_[ 1 ]
#define LFT                             t_[ 2 ]
#define BTM                             t_[ 3 ]
#define RGT                             t_[ 4 ]

#define ENDBLOCK                        "|/END\|"

#define TIMER_RECEIVE                   1001
#define TIMER_SEND                      1002
#define TIMER_PING                      1010

#define SND_SCREEN                      1    //  Through Timer Only
#define SND_CODEBLOCK                   2    //  Application
#define SND_CLOCKINFO                   3
#define SND_CLOCKONOFF                  4
#define SND_MUSIC                       5

//----------------------------------------------------------------------//

STATIC s_srvrSocket
STATIC s_commSocket
STATIC s_lSendingClient := .F.
STATIC s_mutexSend := hb_mutexCreate()

Function RmtSvrInitialize( cServerInfo, nTimeoutClient, nTimeRefresh )
   Local lExit    := .t.

   s_srvrSocket := NIL
   s_commSocket := NIL

   DEFAULT nTimeoutClient TO 60     // 60 SECONDS
   DEFAULT nTimeRefresh   TO .5     // 0.5 SECONDS

   nTimeRefresh := 0.1

   if !empty( cServerInfo )
      if RmtSvrInitAsServer( cServerInfo, @s_srvrSocket, nTimeOutClient*1000 )

         if RmtSvrAcceptClient( s_srvrSocket, @s_commSocket )
            // Very Important Factor   20-50   No more
            //
            Hb_INetTimeout( s_commSocket, 10 )

            lExit := .f.

            RmtSvrRunning( .t. )

            hb_threadStart( @Thread_Receive(), 0.05 )
            hb_threadStart( @Thread_Send()   , nTimeRefresh )
            hb_threadStart( @Thread_Ping()   , 3 )

//          Wvt_SetTimer( TIMER_RECEIVE, 50   )  // 50 ok   1/20 of a second
//          Wvt_SetTimer( TIMER_SEND   , nTimeRefresh*1000 )
//          Wvt_SetTimer( TIMER_PING   , 3000 )
         endif
      endif
   endif

   if lExit
      if s_srvrSocket != NIL
         if Hb_INetErrorCode( s_srvrSocket ) == 0
            Hb_InetClose( s_srvrSocket )
         endif
      endif
      if s_commSocket != NIL
         if Hb_INetErrorCode( s_commSocket ) == 0
            Hb_InetClose( s_commSocket )
         endif
      endif

      Hb_INetCleanUp()
   endif

   Return !( lExit )

//----------------------------------------------------------------------//

Static Function RmtSvrInitAsServer( cServerInfo, Socket, nTimeoutClient )
   Local lRet := .f.

   Hb_INetInit()

   Socket := Hb_INetServer( val( cServerInfo ) )
   if Hb_InetErrorCode( Socket ) == 0
      lRet := .t.
   endif

   if lRet
      // Wait for 1 minutes maximum : W A T C H  INI Controlled
      //
      Hb_INetTimeout( Socket, nTimeoutClient )
TrmDebug( "SERVER: Connection Established!", hb_INetPort( Socket ) )
   else
TrmDebug( "SERVER: Connection Failed" )
   endif

   Return lRet

//----------------------------------------------------------------------//
//
//                    Waits for 2 minutes each try
//
Static Function RmtSvrAcceptClient( Socket, pClientSocket )
   Local lRet := .t.
// Local i := 0

   Do While .t.
      pClientSocket := Hb_InetAccept( Socket )

      if pClientSocket != NIL
         lRet := .t.
      endif

      exit

//TrmDebug( "SvrConnectClient()", i++, "TRY..." )
   enddo

   if lRet
TrmDebug( "CLIENT: Connection Established!", hb_INetPort( pClientSocket ) )
   else
TrmDebug( "CLIENT: Connection TimedOut!" )
   endif

   Return .t.

//----------------------------------------------------------------------//

Function RmtSvrSendClient( nMode, xData )
   Local cCurs, nError, nBytesSent, nBytesToSend, t_, cOdd, cEvn, cOdd0, cEvn0
   Local lSendCurs   := .f.
   Local lSendScrn   := .f.
   Local cData       := ""

   static cCursor    := "     "
   static cSOdd      := "     "
   static cSEvn      := "     "
   static n          := 0
   static nScreen    := 0

   n++

   if RmtSvrRunning()
      if !( s_lSendingClient )
         s_lSendingClient := .t.

         do case
         case nMode == SND_SCREEN
            cCurs := hb_ntos( Row() ) +";"+ ;
                     hb_ntos( Col() ) +";"+ ;
                     hb_ntos( Set( _SET_CURSOR ) ) +";"

            if !( cCurs == cCursor )
               lSendCurs := .t.
               cCursor   := cCurs
            endif

            t_:= xData
            DEFAULT t_ TO { 0, 0, maxrow( .t. ), maxcol( .t. ) }

            cOdd := ""
            cEvn := ""

            GETSCREENATTRIB( TOP,LFT,BTM,RGT,@cOdd,@cEvn )

            if !( cSOdd == cOdd ) .or. !( cSEvn == cEvn )
               lSendScrn := .t.
               cSOdd     := cOdd
               cSEvn     := cEvn
            endif

            if lSendScrn
               nScreen++

               cOdd0 := hb_zcompress( cOdd )
               cEvn0 := hb_zcompress( cEvn )

               cData := '<SCR>'    +;
                        hb_ntos( TOP ) +';'+ hb_ntos( LFT ) +';'+ hb_ntos( BTM ) +';'+ hb_ntos( RGT ) +';'+;
                        hb_ntos( len( cOdd0 ) ) +';'+ ;
                        hb_ntos( len( cEvn0 ) ) +';'+ ;
                        '</E?>'    +;
                        cOdd0      +;
                        cEvn0      +;
                        '</E?>'    +;
                        '</SCR>'   +;
                        '<CRS>' + cCurs + '</CRS>' +;
                        '<ID>' + hb_ntos( nScreen )+ '</ID>'

            elseif lSendCurs
               cData := '<CRS>'+ cCurs +'</CRS>'

            endif

         case nMode == SND_CODEBLOCK
            cData := '<BLK>'+ xData +'</BLK>'

         case nMode == SND_CLOCKINFO
            cData := '<CLK_INFO>'+ xData +'</CLK_INFO>'

         case nMode == SND_CLOCKONOFF
            cData := '<CLK_ONOFF>'+ if( xData, 'TRUE','FALSE' ) +'</CLK_ONOFF>'

         case nMode == SND_MUSIC
            cData := '<MUSIC>'+ upper( xData ) +'</MUSIC>'

         endcase

         if len( cData ) > 0
            cData        += ENDBLOCK
            nBytesToSend := len( cData )
            nBytesSent   := hb_INetSendAll( s_commSocket, cData, nBytesToSend )

            if nBytesSent != nBytesToSend
               nError := hb_INetErrorCode( s_commSocket )
TrmDebug( n,'E','VouchServer - SvrSendClient : ', nError, nBytesSent, nBytesToSend )

               do case
               case ascan( { -2, WSAECONNABORTED, WSAECONNRESET }, nError ) > 0
TrmDebug( n,'Q','VouchServer - SvrSendClient : ', nError, nBytesSent, nBytesToSend )
                  DbCloseAll()
                  Quit

               otherwise

               endcase
            endif
         endif

         s_lSendingClient := .f.
      endif
   endif

   Return nil

//----------------------------------------------------------------------//

Static Function RmtSvrReceiveClient()
   Local cKey, nBytes, nError

   static lInProcess := .f.

   if !lInProcess
      if hb_INetDataReady( s_commSocket ) > 0
         lInProcess := .t.

         cKey := hb_INetRecvLine( s_commSocket, @nBytes )

         if nBytes > 0
            hb_KeyPut( Val( cKey ) )

         elseif nBytes == 1

         else
            nError := hb_INetErrorCode( s_commSocket )
            if ascan( { -2, WSAECONNABORTED, WSAECONNRESET }, nError ) > 0
TrmDebug( 'VouchAsServer - Quitting : Error =', hb_INetErrorCode( s_commSocket ), 'nBytes =', nBytes )
               DbCloseAll()
               Quit
            endif
         endif

         lInProcess := .f.
      endif
   endif

   Return nil

//----------------------------------------------------------------------//
//  Required as this receive all timer events
//
Function Wvt_Timer( wParam )

   switch wParam

   case TIMER_RECEIVE
      RmtSvrReceiveClient()
      exit

   case TIMER_SEND
      RmtSvrSendClient( 1, NIL )
      exit

   case TIMER_PING
      if !( s_lSendingClient )
         hb_INetSendAll( s_commSocket, ENDBLOCK )
      endif
      exit

   end

   Return 0

STATIC PROCEDURE Thread_Receive( nWait )

   DO WHILE .T.
      RmtSvrReceiveClient()
      hb_idleSleep( nWait )
   ENDDO

   RETURN

STATIC PROCEDURE Thread_Send( nWait )

   DO WHILE .T.
      hb_mutexLock( s_mutexSend )
      RmtSvrSendClient( 1, NIL )
      hb_mutexUnlock( s_mutexSend )
      hb_idleSleep( nWait )
   ENDDO

   RETURN

STATIC PROCEDURE Thread_Ping( nWait )

   DO WHILE .T.
      hb_mutexLock( s_mutexSend )
      hb_INetSendAll( s_commSocket, ENDBLOCK )
      hb_mutexUnlock( s_mutexSend )
      hb_idleSleep( nWait )
   ENDDO

   RETURN

//----------------------------------------------------------------------//

Function RmtSvrSetInfo( cnInfo )
   Local xInfo

   static aInfo := { NIL,NIL,NIL }

   if valtype( cnInfo ) == 'C'     // To Retrieve it will be N
      aInfo[ 1 ] := val( cnInfo )  // Port to Use

   elseif valtype( cnInfo ) == 'N'
      xInfo := aInfo[ cnInfo ]

   endif

   Return xInfo

//----------------------------------------------------------------------//

Function RmtSvrRunning( lYes )
   Local sYes
   static oYes := .f.
   sYes := oYes

   if valtype( lYes ) == 'L'
      oYes := lYes
   endif

   return sYes


//----------------------------------------------------------------------//

Function TrmStr2A( cStr, cDel )
   Local a_:={}, n
   Local nlen

   nLen := len( cDel )

   do while .t.
      if ( n := at( cDel, cStr ) ) == 0
         exit
      endif

      aadd( a_, substr( cStr,1,n-1 ) )

      cStr := substr( cStr,n+nLen )
   enddo

   Return a_

//----------------------------------------------------------------------//

Function TrmDebug( p1,p2,p3,p4,p5,p6,p7,p8,p9,p10 )
   Local cDebug := ''

   if p1 != nil
      cDebug += TrmXtoS( p1 )
   endif
   if p2 != nil
      cDebug += '   ' + TrmXtoS( p2 )
   endif
   if p3 != nil
      cDebug += '   ' + TrmXtoS( p3 )
   endif
   if p4 != nil
      cDebug += '   ' + TrmXtoS( p4 )
   endif
   if p5 != nil
      cDebug += '   ' + TrmXtoS( p5 )
   endif
   if p6 != nil
      cDebug += '   ' + TrmXtoS( p6 )
   endif
   if p7 != nil
      cDebug += '   ' + TrmXtoS( p7 )
   endif
   if p8 != nil
      cDebug += '   ' + TrmXtoS( p8 )
   endif
   if p9 != nil
      cDebug += '   ' + TrmXtoS( p9 )
   endif
   if p10 != nil
      cDebug += '   ' + TrmXtoS( p10 )
   endif

#if defined( __PLATFORM__WINDOWS )
   wapi_OutputDebugString( cDebug )
#endif

   Return nil

//----------------------------------------------------------------------//

FUNCTION TrmXtoS( xVar )

   SWITCH ValType( xVar )
   CASE "C"
      RETURN xVar
   CASE "N"
      RETURN Str( xVar )
   CASE "D"
      RETURN DToC( xVar )
   CASE "L"
      RETURN iif( xVar, "T", "F" )
   ENDSWITCH

   RETURN "NIL"

//----------------------------------------------------------------------//

Function TrmDummy()
   Return nil

//----------------------------------------------------------------------//

STATIC PROCEDURE GETSCREENATTRIB( nT, nL, nB, nR, cOdd, cEvn )
   LOCAL s := SaveScreen( nT, nL, nB, nR )
   cOdd := CharOdd( s )
   cEvn := CharEven( s )
   RETURN
