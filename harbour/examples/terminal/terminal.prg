/*
 * $Id$
 */

/*
 * Harbour Project source code:
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
#include "wvtwin.ch"
#include "terminal.ch"

//----------------------------------------------------------------------//

#define TOP                             t_[ 1 ]
#define LFT                             t_[ 2 ]
#define BTM                             t_[ 3 ]
#define RGT                             t_[ 4 ]

#define ENDBLOCK                        "|/END\|"

#define NTRIM( n )                      ltrim( str( n ) )

#define TIMER_RECEIVE                   1001
#define TIMER_SEND                      1002
#define TIMER_PING                      1010

#define SND_SCREEN                      1    //  Through Timer Only
#define SND_CODEBLOCK                   2    //  Application
#define SND_CLOCKINFO                   3
#define SND_CLOCKONOFF                  4
#define SND_MUSIC                       5

//----------------------------------------------------------------------//

STATIC srvrSocket
STATIC commSocket
STATIC lSendingClient := .f.

Function RmtSvrInitialize( cServerInfo, nTimeoutClient, nTimeRefresh )
   Local lExit    := .t.
   Local nTimeOut := 50   // PICK FROM EXTERNASL SOURCE

   srvrSocket := NIL
   commSocket := NIL

   DEFAULT nTimeoutClient TO 60     // 60 SECONDS
   DEFAULT nTimeRefresh   TO .5     // 0.5 SECONDS

   nTimeRefresh := 0.1

   if !empty( cServerInfo )
      if RmtSvrInitAsServer( cServerInfo, @srvrSocket, nTimeOutClient*1000 )

         if RmtSvrAcceptClient( srvrSocket, @commSocket )
            // Very Important Factor   20-50   No more
            //
            Hb_INetTimeout( commSocket, 10 )

            lExit := .f.

            RmtSvrRunning( .t. )

            Wvt_SetTimer( TIMER_RECEIVE, 50   )  // 50 ok   1/20 of a second
            Wvt_SetTimer( TIMER_SEND   , nTimeRefresh*1000 )
            Wvt_SetTimer( TIMER_PING   , 3000 )
         endif
      endif
   endif

   if lExit
      if srvrSocket != NIL
         if Hb_INetErrorCode( srvrSocket ) == 0
            Hb_InetClose( srvrSocket )
         endif
      endif
      if commSocket != NIL
         if Hb_INetErrorCode( commSocket ) == 0
            Hb_InetClose( commSocket )
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
TrmDebug( "SERVER: Connection Established!", INetPort( Socket ) )
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
   Local i := 0

   Do While .t.
      pClientSocket := Hb_InetAccept( Socket )

      if pClientSocket != NIL
         lRet := .t.
      endif

      exit

TrmDebug( "SvrConnectClient()", i++, "TRY..." )
   enddo

   if lRet
TrmDebug( "CLIENT: Connection Established!", INetPort( pClientSocket ) )
   else
TrmDebug( "CLIENT: Connection TimedOut!" )
   endif

   Return .t.

//----------------------------------------------------------------------//

Function RmtSvrSendClient( nMode, xData )
   Local cScr, cCurs, nError, nBytesSent, nBytesToSend, t_, cOdd, cEvn, cOdd0, cEvn0
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
      if !( lSendingClient )
         lSendingClient := .t.

         do case
         case nMode == SND_SCREEN
            cCurs := NTRIM( Row() ) +";"+ ;
                     NTRIM( Col() ) +";"+ ;
                     NTRIM( Set( _SET_CURSOR ) ) +";"

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

               cOdd0 := hb_compress( cOdd )
               cEvn0 := hb_compress( cEvn )

               cData := '<SCR>'    +;
                        NTRIM( TOP ) +';'+ NTRIM( LFT ) +';'+ NTRIM( BTM ) +';'+ NTRIM( RGT ) +';'+;
                        NTRIM( len( cOdd0 ) ) +';'+ ;
                        NTRIM( len( cEvn0 ) ) +';'+ ;
                        '</E?>'    +;
                        cOdd0      +;
                        cEvn0      +;
                        '</E?>'    +;
                        '</SCR>'   +;
                        '<CRS>' + cCurs + '</CRS>' +;
                        '<ID>' + NTRIM( nScreen )+ '</ID>'

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
            nBytesSent   := INetSendAll( commSocket, cData, nBytesToSend )

            if nBytesSent <> nBytesToSend
               nError := INetErrorCode( commSocket )
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

         lSendingClient := .f.
      endif
   endif

   Return nil

//----------------------------------------------------------------------//

Static Function RmtSvrReceiveClient()
   Local cKey, nBytes, nError

   static lInProcess := .f.

   if !lInProcess
      if INetDataReady( commSocket ) > 0
         lInProcess := .t.

         cKey := INetRecvLine( commSocket, @nBytes )

         if nBytes > 0
            Wvt_Keyboard( val( cKey ) )

         elseif nBytes == 1

         else
            nError := INetErrorCode( commSocket )
            if ascan( { -2, WSAECONNABORTED, WSAECONNRESET }, nError ) > 0
TrmDebug( 'VouchAsServer - Quitting : Error =', INetErrorCode( commSocket ), 'nBytes =', nBytes )
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
      if !( lSendingClient )
         INetSendAll( commSocket, ENDBLOCK )
      endif
      exit

   end

   Return ( 0 )

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

   if p1 <> nil
      cDebug += TrmXtoS( p1 )
   endif
   if p2 <> nil
      cDebug += '   ' + TrmXtoS( p2 )
   endif
   if p3 <> nil
      cDebug += '   ' + TrmXtoS( p3 )
   endif
   if p4 <> nil
      cDebug += '   ' + TrmXtoS( p4 )
   endif
   if p5 <> nil
      cDebug += '   ' + TrmXtoS( p5 )
   endif
   if p6 <> nil
      cDebug += '   ' + TrmXtoS( p6 )
   endif
   if p7 <> nil
      cDebug += '   ' + TrmXtoS( p7 )
   endif
   if p8 <> nil
      cDebug += '   ' + TrmXtoS( p8 )
   endif
   if p9 <> nil
      cDebug += '   ' + TrmXtoS( p9 )
   endif
   if p10 <> nil
      cDebug += '   ' + TrmXtoS( p10 )
   endif

   OutputDebugString( cDebug )

   Return nil

//----------------------------------------------------------------------//

Function TrmXtoS( xVar )
   Local cVar := ''
   Local cType := valtype( xVar )

   do case
   case cType == 'C'
      cVar := xVar

   case cType == 'N'
      cVar := str( xVar )

   case cType == 'D'
      cVar := dtoc( xVar )

   case cType == 'L'
      cVar := if( xVar, 'T','F' )

   otherwise
      cVar := 'NIL'

   endcase

   Return cVar

//----------------------------------------------------------------------//

Function TrmDummy()
   Return nil

//----------------------------------------------------------------------//
