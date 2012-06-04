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
//                             Terminal Server
//
//                   Pritpal Bedi (pritpal@vouchcac.com)
//                               13 Feb 2009
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include "inkey.ch"
#include "setcurs.ch"

//----------------------------------------------------------------------//

static g_nUserCount
static g_nTotalCount
static MutexDB
static MutexCount

Static nMaxCol

//----------------------------------------------------------------------//

Function Main( cPort )
   LOCAL socket
   LOCAL nKey

   nMaxCol := maxcol()+1

   IF Empty( cPort ) .OR. Val( cPort ) == 0
      cPort := "8085"
   ENDIF

   g_nUserCount  := 0
   g_nTotalCount := 0

   MutexDB       := HB_MutexCreate()
   MutexCount    := HB_MutexCreate()

   SetColor( "W+/N" )
   SetCursor( SC_NONE )

   BuildScreen()

   hb_InetInit()

   Socket := hb_InetServer( val( cPort ) )

   DispOutAt( 3, 0, padc( "Waiting for connections on port " + cPort, nMaxCol ), "W+/N" )

   hb_ThreadStart( @ViewUpdate()    , Socket )
   hb_ThreadStart( @AcceptIncoming(), Socket )

   DO WHILE .T.
      nKey := inkey(0)

      if nKey == K_CTRL_F12
         //hb_ThreadStop( hView )

         // closing the socket will release the accept() request
         //hb_InetClose( Socket )

         //hb_ThreadStop( hAccept )
         EXIT
      ENDIF
   ENDDO

   DispOutAt( maxrow()-2, 0, padc( "Please kill the window if not closed automatically",nMaxCol ), "W+/N" )

   hb_InetClose( Socket )
   hb_InetCleanup()

   RETURN nil

//----------------------------------------------------------------------//
//
//                         Server Socket manager
//
PROCEDURE AcceptIncoming( Socket )
   LOCAL pClientSocket

   DO WHILE .T.
      pClientSocket := hb_InetAccept( Socket )

      IF pClientSocket != NIL
         hb_mutexLock( MutexCount )
         g_nUserCount++
         g_nTotalCount++
         hb_mutexUnlock( MutexCount )

         hb_ThreadStart( @ServeClient(), pClientSocket )
         hb_gcAll( .T. )
      ELSE
         ? "Catched error ", hb_InetErrorCode( Socket ), hb_InetErrorDesc( Socket )
         //EXIT
      ENDIF
   ENDDO

   RETURN

//----------------------------------------------------------------------//
//
//                      Service incoming connection
//
PROCEDURE ServeClient( Socket )
   LOCAL cRequest, cReply, cReq, cCmdLine, lExit
   LOCAL nLength      := 0
   LOCAL nn           := 0
   LOCAL a_

   static nServerPort := 45000

   do while .t.
      lExit := .f.
      BEGIN SEQUENCE
         *** First of all, we must take the request of the user
         cRequest := alltrim( hb_InetRecvLine( Socket, @nLength ) )

      RECOVER
         lExit := .t.

      END SEQUENCE

      if lExit
         EXIT
      endif

      if nLength < 0
         exit

      elseif nLength > 1
         a_:= hb_aTokens( cRequest, '|' )

         cReq := upper( a_[ 1 ] )

         do case
         case cReq == 'AR'
            nServerPort++
            SvrExecuteAPP( 'trm_app.exe', hb_ntos( nServerPort++ ), hb_dirBase() )
            inkey( 5 )
            cReply := 'CONNECT;' + 'localhost' + ';' + hb_ntos( nServerPort++ ) + ';'

         case cReq == 'VOUCH'
            nServerPort++

            cCmdLine := a_[ 3 ] +'   '+ ltrim( str( nServerPort ) )
            SvrExecuteAPP( a_[ 2 ], cCmdLine, a_[ 4 ] )
            cReply := 'CONNECT;' + 'localhost' + ';' + hb_ntos( nServerPort ) + ';' + a_[ 2 ] + ';'

         case cReq == 'ARCONNECTED'
            // No further info required, close connection
            EXIT

         case cReq == 'FORM'
#if 0
            oXmlDoc := TXmlDocument():new( '<?xml version="1.0"?>' )

            oXmlNode := TXmlNode():New( ,'form', { 'name' => 'CLIENT' } )
            oXmlDoc:oRoot:addBelow( oXmlNode )

            cFields := 'Code,C,8,0,@!,10,20,,^'+;
                       'Name,C,20,0,@!,12,20,,^'+;
                       'Salary,N,10,2,@Z 9999999.99,14,20,,^'+;
                       'Job,C,20,0,,16,20,,^'+;
                       'Birth,D,8,0,,18,20,,^'

            oXmlNode := TXmlNode():New( ,'fields', { 'fields' => cFields } )
            oXmlDoc:oRoot:addBelow( oXmlNode )

            cData := 'C01|JOHNY WALKER|200|DRUMMER|01/01/1956|'
            oXmlNode := TXmlNode():New( ,'data', { 'data' => cData } )
            oXmlDoc:oRoot:addBelow( oXmlNode )

            cReply := 'FORM;'+oXmlDoc:ToString( 1 )

            /*
            cReply := 'FORM;'+;
                      '<NAME>'+;
                      'CLIENT'+;
                      '</NAME>'+;
                      '<FORM>'+;
                      'Code,C,8,0,@!,10,20,,^'+;
                      'Name,C,20,0,@!,12,20,,^'+;
                      'Salary,N,10,2,@Z 9999999.99,14,20,,^'+;
                      'Job,C,20,0,,16,20,,^'+;
                      'Birth,D,8,0,,18,20,,^'+;
                      '</FORM>'+;
                      '<DATA>'+;
                      'C01|JOHNY WALKER|200|DRUMMER|01/01/1956|'+;
                      '</DATA>'

              */
#endif
         case cReq == 'SCREEN'
            cReply := 'SCREEN;'+SaveScreen( 0,0,MAXROW(),MAXCOL() )

         case cReq == 'INFO'
            cReply := 'INFO;' + hb_ntos( hb_INetPort( Socket ) ) + ';' + hb_INetAddress( Socket ) + ';'

         otherwise
            cReply := 'GENERIC;' + 'Request # '+ hb_ntos( ++nn, 10, 0 )

         endcase

         DispOutAt( 15, 0,  " REQ: " + Substr( cRequest, 0 , 75 ) + Space( 80 ),'W+/BG' )
         DispOutAt( 16, 0,  " RLY: " + Substr( cReply, 0 , 75 ) + Space( 80 ), 'W+/B' )

         cReply += chr(13)+chr(10)

         hb_InetSend( Socket, @cReply )

      else
         //ThreadSleep( 50 )
         inkey( 0.05 )

      endif

   enddo

   hb_InetClose( Socket )

   RETURN

//----------------------------------------------------------------------//

Function SvrExecuteApp( cAppln, cParams, cDirectory )

#if defined( __PLATFORM__WINDOWS )
   RETURN wapi_ShellExecute( NIL, "open", cAppln, cParams, cDirectory )
#else
   HB_SYMBOL_UNUSED( cDirectory )
   RETURN hb_run( cAppln + " " + cParams )
#endif

//----------------------------------------------------------------------//
//
//                        Managing visual updates
//
PROCEDURE ViewUpdate( Socket )
   LOCAL nProgress := 0

   DO WHILE .T.
      HB_MutexLock( MutexCount )

      Looping( @nProgress, 5, 39 )

      DispOutAt(  8, 5, "Main socket status : " + Str( hb_InetErrorCode( Socket ) ) +" :"+;
                                                                hb_InetErrorDesc( Socket ) )

      DispOutAt(  9, 5, "Connected Users    : " + Str( g_nUserCount  ) )
      DispOutAt( 10, 5, "Total users        : " + Str( g_nTotalCount ) )

      HB_MutexUnlock( MutexCount )

      inkey( 0.1 )
   ENDDO

   RETURN

//----------------------------------------------------------------------//

PROCEDURE Progress( nProgress, nDrow, nDcol )

   DispOutAt( nDrow, nDcol, "[ ]" )

   DO CASE
   CASE nProgress == 0
      DispOutAt( nDrow, nDcol+1, "-" )
   CASE nProgress == 1
      DispOutAt( nDrow, nDcol+1, "\" )
   CASE nProgress == 2
      DispOutAt( nDrow, nDcol+1, "|" )
   CASE nProgress == 3
      DispOutAt( nDrow, nDcol+1, "/" )
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   RETURN

//----------------------------------------------------------------------//

PROCEDURE Looping( nProgress,  nDrow, nDcol )

   IF nProgress > 3 .OR. nProgress < 0
      nProgress := 0
   ENDIF

   DispOutAt( nDrow, nDcol, "[ ]" )

   DO CASE
   CASE nProgress == 0
      DispOutAt( nDrow, nDcol+1, "-" )
   CASE nProgress == 1
      DispOutAt( nDrow, nDcol+1, "\" )
   CASE nProgress == 2
      DispOutAt( nDrow, nDcol+1, "|" )
   CASE nProgress == 3
      DispOutAt( nDrow, nDcol+1, "/" )
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   RETURN

//----------------------------------------------------------------------//

Function BuildScreen()

   CLEAR SCREEN

   DispOutAt( 0,0, padc( "Welcome to   V o u c h   Server", maxcol()+1 ), 'w+/r' )

   DispOutAt( maxrow(),0,padc( 'Press CTRL+F12 to QUIT',maxcol()+1 ), 'w+/r' )

   Return nil

//----------------------------------------------------------------------//

Function uiDebug( p1,p2,p3,p4,p5,p6,p7,p8,p9,p10 )
   Local cDebug := ''

   if p1 != nil
      cDebug += uiXtos( p1 )
   endif
   if p2 != nil
      cDebug += '   ' + uiXtos( p2 )
   endif
   if p3 != nil
      cDebug += '   ' + uiXtos( p3 )
   endif
   if p4 != nil
      cDebug += '   ' + uiXtos( p4 )
   endif
   if p5 != nil
      cDebug += '   ' + uiXtos( p5 )
   endif
   if p6 != nil
      cDebug += '   ' + uiXtos( p6 )
   endif
   if p7 != nil
      cDebug += '   ' + uiXtos( p7 )
   endif
   if p8 != nil
      cDebug += '   ' + uiXtos( p8 )
   endif
   if p9 != nil
      cDebug += '   ' + uiXtos( p9 )
   endif
   if p10 != nil
      cDebug += '   ' + uiXtos( p10 )
   endif

#if defined( __PLATFORM__WINDOWS )
   wapi_OutputDebugString( cDebug )
#endif

   Return nil

//----------------------------------------------------------------------//

FUNCTION uiXtos( xVar )

   SWITCH ValType( xVar )
   CASE "C"
      RETURN xVar
   CASE "N"
      RETURN Str( xVar )
   CASE "D"
      RETURN DToC( xVar )
   CASE "L"
      RETURN iif( xVar, "Yes", "No " )
   ENDSWITCH

   RETURN "NIL"
