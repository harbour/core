/*
 * xHarbour Project source code:
 * Remote Procedure Call code
 * Client class
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbclass.ch"

#include "hbrpc.ch"

CREATE CLASS TRPCClient

   VAR aServers
   VAR aFunctions

   VAR nUdpPort
   VAR nTcpPort

   /* asyncrhonous mode */
   VAR lAsyncMode
   /* block to be called at scan completion */
   VAR bOnScanComplete
   /*block called when there is a progress in the scan */
   VAR bOnScanServersProgress
   VAR bOnScanFunctionsProgress

   /* block to be called at function error */
   VAR bOnFunctionProgress
   /* block to be called at function success */
   VAR bOnFunctionReturn
   /* block to be called at function failure */
   VAR bOnFunctionFail


   METHOD New( cNetwork, nTcpPort, nUdpPort ) CONSTRUCTOR
   METHOD Destroy()

   /* Connection */
   METHOD Connect( cServer, cUserId, cPassword )
   METHOD Disconnect()

   /* Network scan functions */
   METHOD ScanServers( cName )
   METHOD ScanFunctions( cFunc, cSerial )
   METHOD ScanAgain()            INLINE ::UDPAccept()
   METHOD StopScan()

   /* Function call */
   METHOD CheckServer( cRemote )    // Checks if a server is ready on tcp
   METHOD SetLoopMode( nMethod, xData, nEnd, nStep )
   METHOD Call( ... )  // variable parameters
   METHOD CallAgain()            INLINE ::TCPAccept()
   METHOD StopCall()

   METHOD SetPeriodCallback( ... )
   METHOD ClearPeriodCallback()


   /* Accessors */
   METHOD SetEncryption( cKey )
   METHOD IsEncrypted()             INLINE ::cCryptKey != NIL
   METHOD GetStatus()               INLINE ::nStatus
   METHOD SetTimeout( nTime )
   METHOD GetTimeout()
   METHOD GetResult()               INLINE ::oResult
   METHOD FoundServers()            INLINE Len( ::aServers ) != 0
   METHOD FoundFunctions()          INLINE Len( ::aFunctions ) != 0

   METHOD HasError()                INLINE ::nErrorCode != 0 .OR. ::TcpHasError() .OR. ::UdpHasError()
   METHOD GetErrorCode()            INLINE ::nErrorCode

   METHOD TcpHasError()             INLINE iif( Empty( ::skTCP ), .F., hb_inetErrorCode( ::skTCP ) > 0 )
   METHOD GetTcpErrorCode()         INLINE iif( Empty( ::skTCP ), 0, hb_inetErrorCode( ::skTCP ) )
   METHOD GetTcpErrorDesc()         INLINE iif( Empty( ::skTCP ), "", hb_inetErrorDesc( ::skTCP ) )

   METHOD UdpHasError()             INLINE iif( Empty( ::skUDP ), .F., hb_inetErrorCode( ::skUDP ) > 0 )
   METHOD UdpGetErrorCode()         INLINE iif( Empty( ::skUDP ), 0, hb_inetErrorCode( ::skUDP ) )
   METHOD UdpGetErrorDesc()         INLINE iif( Empty( ::skUDP ), "", hb_inetErrorDesc( ::skUDP ) )
   /* Used to retreive data from scans */
   METHOD GetFunctionName( xId )
   METHOD GetServerName( xId )
   METHOD GetServerAddress( xId )

   HIDDEN:
   // Automatic initialization of inet support
   CLASS VAR lInit INIT hb_inetInit()

   VAR mtxBusy INIT hb_mutexCreate()

   VAR nStatus
   // This RPC protocol breaking error code
   VAR nErrorCode

   /* Network data */
   VAR cServer
   VAR cNetwork
   VAR skUdp
   VAR skTcp

   /* Timeout system */
   VAR nTimeout        INIT -1
   VAR nTimeLimit      INIT -1
   VAR caPerCall

   VAR nUdpTimeBegin   INIT 0
   VAR thUdpAccept     INIT NIL

   VAR nTcpTimeBegin   INIT 0
   VAR thTcpAccept     INIT NIL

   /* XHB RPC Loop system */
   VAR nLoopMode
   VAR aLoopData
   VAR nLoopStart
   VAR nLoopEnd
   VAR nLoopStep

   /* Encryption data */
   VAR bEncrypted
   VAR cCryptKey

   /* Last connection result */
   VAR oResult

   /* Encryption system */
   METHOD Encrypt( cDataIn )
   METHOD Decrypt( cDataIn )
   METHOD BuildChallengePwd( cPassword )
   METHOD ManageChallenge()

   /* Network negotiation system */
   METHOD StartScan()
   METHOD UDPAccept()
   METHOD UDPParse( cData, nLen )
   METHOD TCPAccept()
   METHOD TCPParse( cCode )
   METHOD clearTCPBuffer()

   /* internal network send call */
   METHOD SendCall( cFunction, aParams )

   /* event handlers */
   METHOD OnScanComplete()
   METHOD OnScanServersProgress( aLoc )
   METHOD OnScanFunctionsProgress( aLoc )

   METHOD OnFunctionFail( nReason, cReason )
   METHOD OnFunctionReturn( oReturn )
   METHOD OnFunctionProgress( nProgress, oData )

ENDCLASS

METHOD New( cNetwork, nTcpPort, nUdpPort ) CLASS TRPCClient

   ::nStatus := RPC_STATUS_NONE // not connected
   ::nErrorCode := 0 // no RPC error
   ::cServer := NIL // no server

   ::nUdpPort := iif( nUdpPort == NIL, 1139, nUdpPort )
   ::nTcpPort := iif( nTcpPort == NIL, 1140, nTcpPort )

   ::skTcp := hb_inetCreate()
   ::skUdp := hb_inetDGram( .T. )
   ::lAsyncMode := .F.
   ::aServers := {}
   ::aFunctions := {}
   ::cNetwork := cNetwork
   ::bEncrypted := .F.

   ::nLoopMode := RPC_LOOP_NONE

   RETURN Self

METHOD Destroy() CLASS TRPCClient

   hb_mutexLock( ::mtxBusy )

   ::Disconnect()
   IF hb_threadID( ::thUdpAccept ) != 0
      hb_threadQuitRequest( ::thUdpAccept )
      ::thUdpAccept := NIL
   ENDIF
   IF hb_threadID( ::thTcpAccept ) != 0
      hb_threadQuitRequest( ::thTcpAccept )
      ::thTcpAccept := NIL
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD SetEncryption( cKey )

   IF ! Empty( cKey )
      ::bEncrypted := .T.
      ::cCryptKey := cKey
   ELSE
      ::bEncrypted := .F.
   ENDIF

   RETURN .T.

METHOD ScanServers( cName ) CLASS TRPCClient

   // do not allow asynchronous mode without timeout
   IF ! ::lAsyncMode .AND. ( ::nTimeout == NIL .OR. ::nTimeOut <= 0 )
      RETURN .F.
   ENDIF

   hb_mutexLock( ::mtxBusy )
   ::aServers := {}
   hb_mutexUnlock( ::mtxBusy )

   hb_inetDGramSend( ::skUDP, ::cNetwork, ::nUdpPort, "XHBR00" + hb_Serialize( cName ) )
   ::StartScan()

   RETURN .F.

METHOD CheckServer( cRemote )

   LOCAL cData, skRemote, nLen, cData2

   cData := "XHBR00"
   IF cRemote == NIL
      cRemote := ::cNetwork
   ENDIF
   skRemote := hb_inetConnect( cRemote, ::nTcpPort )
   IF hb_inetErrorCode( skRemote ) == 0
      hb_inetTimeout( skRemote, 10000 )
      hb_inetSendAll( skRemote, cData )
      cData := Space( 256 )
      hb_inetRecvAll( skRemote, @cData, 6 + 9 )
      IF hb_inetErrorCode( skRemote ) == 0
         cData2 := Space( 256 )
         nLen := hb_GetLen8( SubStr( cData, 8, 8 ) )
         hb_inetRecvAll( skRemote, @cData2, nLen )
         IF hb_inetErrorCode( skRemote ) == 0
            cData := SubStr( cData + cData2, 7 )
            cData2 := hb_Deserialize( cData )
            AAdd( ::aServers, { hb_inetAddress( skRemote ), cData2 } )
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD ScanFunctions( cFunc, cSerial ) CLASS TRPCClient

   // do not allow asynchronous mode without timeout
   IF ! ::lAsyncMode .AND. ( ::nTimeOut == NIL .OR. ::nTimeOut <= 0 )
      RETURN .F.
   ENDIF

   IF cSerial == NIL
      cSerial := "00000000.0"
   ENDIF
   hb_mutexLock( ::mtxBusy )
   ::aFunctions := {}
   ::aServers := {}
   hb_mutexUnlock( ::mtxBusy )

   hb_inetDGramSend( ::skUDP, ::cNetwork, ::nUdpPort, ;
      "XHBR01" + hb_Serialize( cFunc ) + hb_Serialize( cSerial ) )
   ::StartScan()

   RETURN .F.

METHOD StartScan()

   // We don't accept sync call without timeout

   IF ::lAsyncMode
      // in async mode, force scanning stop
      ::StopScan()
   ENDIF

   ::nUDPTimeBegin := Int( Seconds() * 1000 )

   // in async mode, just launch the listener
   IF ::lAsyncMode
      hb_mutexLock( ::mtxBusy )
      ::thUdpAccept := StartThread( Self, "UDPAccept" )
      hb_mutexUnlock( ::mtxBusy )
   ELSE
      ::UDPAccept()
   ENDIF

   RETURN .T.

METHOD UDPAccept() CLASS TRPCClient

   LOCAL nTime, nDatalen, cData

   cData := Space( 1400 )
   // set default socket timeout
   IF ::nTimeout >= 0
      hb_inetTimeout( ::skUDP, ::nTimeout )
   ELSE
      hb_inetClearTimeout( ::skUdp )
   ENDIF

   DO WHILE .T.
      nDatalen := hb_inetDGramRecv( ::skUDP, @cData, 1400 )

      IF nDataLen > 0 .AND. ::UDPParse( cData, nDatalen )
         EXIT
      ENDIF

      IF ::nTimeout >= 0
         nTime := Int( Seconds() * 1000 )
         // a little tollerance must be added for double roundings
         // in the double Int() functions
         IF nTime - ::nUDPTimeBegin >= ::nTimeout - 5
            EXIT
         ENDIF
      ENDIF

   ENDDO

   ::OnScanComplete()
   // signal that this thread is no longer active
   hb_mutexLock( ::mtxBusy )
   ::thUdpAccept := NIL
   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD UDPParse( cData, nLen ) CLASS TRPCClient

   LOCAL cCode, cSer, cFunc, cName
   LOCAL aLoc

   IF nLen < 12
      RETURN .F.
   ENDIF

   cCode := SubStr( cData, 1, 6 )

   DO CASE
      /* XHRB00 - server scan */
   CASE cCode == "XHBR10"
      cData := SubStr( cData, 7 )
      cData := hb_Deserialize( cData, 512 )
      // deserialization error checking
      IF cData != NIL
         aLoc := { hb_inetAddress( ::skUDP ), cData }
         AAdd( ::aServers, aLoc )
         RETURN ::OnScanServersProgress( aLoc )
      ELSE
         RETURN .F.
      ENDIF


   CASE cCode == "XHBR11"
      cData := SubStr( cData, 7 )
      cSer := hb_DeserialBegin( cData )
      cName := hb_DeserialNext( @cSer, 64 )
      cFunc := hb_DeserialNext( @cSer, 64 )
      IF cName != NIL .AND. cFunc != NIL
         aLoc := { hb_inetAddress( ::skUDP ), cName, cFunc }
         AAdd( ::aFunctions, aLoc )
         RETURN ::OnScanFunctionsProgress( aLoc )
      ELSE
         RETURN .F.
      ENDIF

   ENDCASE

   RETURN .F.

METHOD StopScan() CLASS TRPCClient

   hb_mutexLock( ::mtxBusy )
   IF hb_threadID( ::thUDPAccept ) != 0
      hb_threadQuitRequest( ::thUDPAccept )
      ::thUDPAccept := NIL
      hb_mutexUnlock( ::mtxBusy )
      ::OnScanComplete()
   ELSE
      hb_mutexUnlock( ::mtxBusy )
   ENDIF

   RETURN .T.

METHOD Connect( cServer, cUserId, cPassword ) CLASS TRPCClient

   LOCAL cAuth, cReply := Space( 8 )

   hb_inetConnect( cServer, ::nTcpPort, ::skTcp  )

   IF hb_inetErrorCode( ::skTcp ) == 0
      ::nStatus := RPC_STATUS_CONNECTED // Connected
      IF ::bEncrypted
         cAuth := ::BuildChallengePwd( cPassword )
         cAuth := cUserId + ":" + cAuth
         hb_inetSendAll( ::skTcp, "XHBR93" + hb_CreateLen8( Len( cAuth ) ) + cAuth )
      ELSE
         cAuth := cUserId + ":" + cPassword
         hb_inetSendAll( ::skTcp, "XHBR90" + hb_CreateLen8( Len( cAuth ) ) + cAuth )
      ENDIF

      IF hb_inetErrorCode( ::skTcp ) == 0
         IF ! ::bEncrypted
            hb_inetRecvAll( ::skTcp, @cReply )
            IF hb_inetErrorCode( ::skTcp ) == 0 .AND. cReply == "XHBR91OK"
               ::nStatus := RPC_STATUS_LOGGED // Logged in
               RETURN .T.
            ENDIF
         ELSE
            RETURN ::ManageChallenge()
         ENDIF

      ENDIF
   ENDIF

   ::skTcp := NIL
   ::nStatus := RPC_STATUS_NONE

   RETURN .F.

METHOD BuildChallengePwd( cPassword ) CLASS TRPCClient

   LOCAL nLen := 10 + Int( hb_Random( 1, 60 ) )
   LOCAL nCount
   LOCAL cRet := ""

   FOR nCount := 1 TO nLen
      cRet += hb_BChar( Int( hb_Random( 2, 254 ) ) )
   NEXT
   cRet += "PASSWORD:" + cPassword + ":"

   DO WHILE hb_BLen( cRet ) < 100
      cRet += hb_BChar( Int( hb_Random( 2, 254 ) ) )
   ENDDO

   RETURN ::Encrypt( cRet )

METHOD ManageChallenge() CLASS TRPCClient

   LOCAL cCode, cLen, nLen
   LOCAL cData, nChallenge

   cCode := Space( 6 )
   IF hb_inetRecvAll( ::skTCP, @cCode ) != 6
      RETURN .F.
   ENDIF

   IF !( cCode == "XHBR94" )
      RETURN .F.
   ENDIF

   cLen := Space( 8 )
   IF hb_inetRecvAll( ::skTCP, @cLen ) != 8
      RETURN .F.
   ENDIF

   nLen := hb_GetLen8( cLen )
   cData := Space( nLen )
   IF hb_inetRecvAll( ::skTCP, @cData, nLen ) != nLen
      RETURN .F.
   ENDIF

   cData := hb_Decrypt( cData, ::cCryptKey )
   nChallenge := hb_Checksum( cData )
   hb_inetSendAll( ::skTCP, "XHBR95" + hb_CreateLen8( nChallenge ) )
#if 0
   IF hb_inetErrorCode( ::skTCP ) != 0
      RETURN .F.
   ENDIF
#endif

   cCode := Space( 8 )
   hb_inetRecvAll( ::skTCP, @cCode )
   IF hb_inetErrorCode( ::skTCP ) != 0 .OR. !( cCode == "XHBR91OK" )
      RETURN .F.
   ENDIF
   /* SUCCESS! */
   ::nStatus := RPC_STATUS_LOGGED

   RETURN .T.

METHOD Disconnect() CLASS TRPCClient

   IF ::nStatus >= RPC_STATUS_LOGGED
      hb_mutexLock( ::mtxBusy )
      ::nStatus := RPC_STATUS_NONE
      hb_inetSendAll( ::skTcp, "XHBR92" )
      hb_inetClose( ::skTcp )
      hb_mutexUnlock( ::mtxBusy )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD SetLoopMode( nMethod, xData, nEnd, nStep ) CLASS TRPCClient

   IF nMethod == RPC_LOOP_NONE
      ::nLoopMode := RPC_LOOP_NONE
      ::aLoopData := NIL
      RETURN .T.
   ENDIF

   IF HB_ISARRAY( xData )
      ::aLoopData := xData
   ELSE
      IF HB_ISNUMERIC( xData )
         // this is to allow garbage collecting
         ::aLoopData := NIL
         ::nLoopStart := xData
         ::nLoopEnd := nEnd
         IF HB_ISNUMERIC( nStep )
            ::nLoopStep := nStep
         ELSE
            ::nLoopStep := 1
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   ::nLoopMode := nMethod

   RETURN .T.

METHOD ClearTCPBuffer() CLASS TRPCClient

   LOCAL cDummy := Space( 512 )

   IF ::skTCP == NIL .OR. ::nStatus < RPC_STATUS_LOGGED
      RETURN .F.
   ENDIF

   DO WHILE hb_inetDataReady( ::skTCP ) > 0
      // hb_inetRecv reads only the available data
      hb_inetRecv( ::skTCP, @cDummy )
   ENDDO

   RETURN .T.

METHOD Call( ... ) CLASS TRPCClient

   LOCAL oCalling
   LOCAL cFunction, aParams
   LOCAL nCount

   IF PCount() == 0
      RETURN NIL
   ENDIF

   ::oResult := NIL

   // do not allow asynchronous mode without timeout
   IF ! ::lAsyncMode .AND. ( ::nTimeOut == NIL .OR. ::nTimeOut <= 0 )
      RETURN NIL
   ENDIF

   oCalling := hb_PValue( 1 )
   IF HB_ISARRAY( oCalling )
      cFunction := oCalling[ 1 ]
      hb_ADel( oCalling, 1, .T. )
      aParams := oCalling
   ELSE
      cFunction := oCalling
      aParams := Array( PCount() - 1 )
      FOR nCount := 2 TO PCount()
         aParams[ nCount - 1 ] := hb_PValue( nCount )
      NEXT
   ENDIF

   // clear eventual pending data
   ::ClearTcpBuffer()

   // The real call
   hb_mutexLock( ::mtxBusy )
   // already active or not already connected
   IF hb_threadID( ::thTcpAccept ) != 0 .OR. ::skTCP == NIL .OR. ::nStatus < RPC_STATUS_LOGGED
      hb_mutexUnlock( ::mtxBusy )
      RETURN NIL
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   ::nStatus := RPC_STATUS_WAITING // waiting for a reply

   // send the call through the socket
   IF ! ::SendCall( cFunction, aParams )
      RETURN .F.
   ENDIF

   // in async mode, just launch the listener
   IF ::lAsyncMode
      hb_mutexLock( ::mtxBusy )
      ::thTCPAccept := StartThread( Self, "TCPAccept" )
      hb_mutexUnlock( ::mtxBusy )
   ELSE
      ::TCPAccept()
   ENDIF

   RETURN ::oResult

METHOD SetPeriodCallback( ... ) CLASS TRPCClient

   LOCAL caCalling
   LOCAL nCount

   IF PCount() < 3
      // TODO set an error
      RETURN .F.
   ENDIF

   hb_mutexLock( ::mtxBusy )
   ::nTimeout := hb_PValue( 1 )
   ::nTimeLimit := hb_PValue( 2 )

   caCalling := hb_PValue( 3 )
   IF ! HB_ISARRAY( caCalling )
      caCalling := Array( PCount() - 2 )
      FOR nCount := 3 TO PCount()
         caCalling[ nCount - 2 ] := hb_PValue( nCount )
      NEXT
   ENDIF
   ::caPerCall := caCalling

   IF ::skTCP != NIL
      hb_inetTimeout( ::skTCP, ::nTimeout )
      hb_inetTimeLimit( ::skTCP, ::nTimeLimit )
      hb_inetPeriodCallback( ::skTCP, caCalling )
   ENDIF

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD ClearPeriodCallback() CLASS TRPCClient

   hb_mutexLock( ::mtxBusy )

   ::nTimeout := -1
   ::nTimeLimit := -1
   ::caPerCall := NIL

   IF ::skTCP != NIL
      hb_inetClearTimeout( ::skTCP )
      hb_inetClearTimeLimit( ::skTCP )
      hb_inetClearPeriodCallback( ::skTCP )
   ENDIF

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD SetTimeout( nTime ) CLASS TRPCClient

   hb_mutexLock( ::mtxBusy )

   ::nTimeout := nTime
   hb_inetTimeout( ::skTCP, ::nTimeout )

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD GetTimeout()

   LOCAL nRet

   hb_mutexLock( ::mtxBusy )
   nRet := ::nTimeout
   hb_mutexUnlock( ::mtxBusy )

   RETURN nRet

METHOD StopCall() CLASS TRPCClient

   IF ::nStatus != RPC_STATUS_WAITING
      RETURN .F.
   ENDIF

   // clear eventual pending data
   ::ClearTcpBuffer()

   // send cancelation request
   hb_inetSendAll( ::skTCP, "XHBR29" )

   // Stops waiting for a result
   hb_mutexLock( ::mtxBusy )
   IF hb_threadID( ::thTCPAccept ) != 0
      hb_threadQuitRequest( ::thTCPAccept )
      ::thTCPAccept := NIL
      ::nStatus := RPC_STATUS_LOGGED
      hb_mutexUnlock( ::mtxBusy )
      ::OnFunctionReturn( NIL )
   ELSE
      hb_mutexUnlock( ::mtxBusy )
   ENDIF

   RETURN .T.

METHOD SendCall( cFunction, aParams ) CLASS TRPCClient

   LOCAL cData := "", nLen
   LOCAL nReq, cType

   SWITCH ::nLoopMode
   CASE RPC_LOOP_NONE
      nReq := 0
      cType := ""
      EXIT

   CASE RPC_LOOP_ALLDATA
      nReq := 2
      cType := "A"
      EXIT

   CASE RPC_LOOP_SUMMARY
      nReq := 2
      cType := "C"
      EXIT

   CASE RPC_LOOP_CONFIRMATION
      nReq := 2
      cType := "E"
      EXIT
   ENDSWITCH

   IF ::aLoopData == NIL .AND. ::nLoopMode > RPC_LOOP_NONE
      cData := hb_Serialize( ::nLoopStart ) + hb_Serialize( ::nLoopEnd ) + ;
         hb_Serialize( ::nLoopStep )
   ENDIF

   cData += hb_Serialize( cFunction ) + hb_Serialize( aParams )

   IF ::aLoopData != NIL
      cData += hb_Serialize( ::aLoopData )
      nReq += 2
   ENDIF

   nLen := hb_BLen( cData )
   IF nLen > 512
      cData := hb_Compress( cData )
      cData := "XHBR2" + hb_ntos( nReq + 1 ) + ;
         hb_CreateLen8( nLen ) + hb_CreateLen8( hb_BLen( cData ) ) + ;
         cType + ::Encrypt( cData )
   ELSE
      cData := "XHBR2" + hb_ntos( nReq ) + hb_CreateLen8( nLen ) + ;
         cType + ::Encrypt( cData )
   ENDIF

   hb_inetSendAll( ::skTCP,  cData )

   RETURN hb_inetErrorCode( ::skTCP ) == 0

METHOD TCPAccept() CLASS TRPCClient

   LOCAL nTime := 0
   LOCAL cCode
   LOCAL nTimeLimit

   // TcpAccept can also be called standalone, without the
   // support of call(). So, we must set the waiting state.
   hb_mutexLock( ::mtxBusy )

   ::nErrorCode := 0
   ::nStatus := RPC_STATUS_WAITING

   hb_mutexUnlock( ::mtxBusy )

   cCode := Space( 6 )
   ::nTCPTimeBegin := Int( Seconds() * 1000 )
   nTimeLimit := Max( ::nTimeout, ::nTimeLimit )


   DO WHILE .T.
      IF hb_inetRecvAll( ::skTCP, @cCode, 6 ) <= 0
         EXIT
      ENDIF

      IF ! ::TCPParse( cCode )
         EXIT
      ENDIF

      IF nTimeLimit >= 0
         nTime := Int( Seconds() * 1000 )
         // a little tollerance must be added for double roundings
         // in the double Int() functions
         IF nTime - ::nTCPTimeBegin >= nTimeLimit - 5
            EXIT
         ENDIF
      ENDIF
   ENDDO

   hb_mutexLock( ::mtxBusy )

   // NOT waiting anymore
   ::nStatus := RPC_STATUS_LOGGED
   ::thTcpAccept := NIL

   IF ::caPerCall == NIL .AND. hb_inetErrorCode( ::skTCP ) != -1 .AND. ;
         nTime - nTimeLimit < nTimeLimit - 5
      IF hb_inetErrorCode( ::skTCP ) != 0
         ::nStatus := RPC_STATUS_ERROR
      ENDIF
   ENDIF

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD TCPParse( cCode ) CLASS TRPCClient

   LOCAL nDataLen, cData, nOrigLen
   LOCAL cDataLen := Space( 8 ), cOrigLen := Space( 8 )
   LOCAL cProgress := Space( 10 ), nProgress
   LOCAL lContinue := .F.

   ::nErrorCode := 0

   DO CASE
      /* Warn error codes */
   CASE cCode == "XHBR40"
      cData := Space( 2 )
      hb_inetRecvAll( ::skTCP, @cData, 2 )
      ::nErrorCode := Val( cData )
      ::OnFunctionFail( ::nErrorCode, "No description for now" )

      /* We have a reply */
   CASE cCode == "XHBR30"
      IF hb_inetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
         nDataLen := hb_GetLen8( cDataLen )
         cData := Space( nDataLen )
         IF hb_inetRecvAll( ::skTCP, @cData, nDataLen ) == nDataLen
            ::oResult := hb_Deserialize( ::Decrypt( cData ), nDataLen )
            IF ::oResult != NIL
               ::OnFunctionReturn( ::oResult )
            ENDIF
            // TODO: rise an error if ::oResult is NIL
         ENDIF
      ENDIF

      /* We have a reply */
   CASE cCode == "XHBR31"
      IF hb_inetRecvAll( ::skTCP, @cOrigLen ) == Len( cOrigLen )
         nOrigLen := hb_GetLen8( cOrigLen )
         IF hb_inetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
            nDataLen := hb_GetLen8( cDataLen )
            cData := Space( nDataLen )
            IF hb_inetRecvAll( ::skTCP, @cData ) == nDataLen
               cData := hb_Uncompress( nOrigLen, ::Decrypt( cData ) )
               IF ! Empty( cData )
                  ::oResult := hb_Deserialize( cData, nDataLen )
                  IF ::oResult != NIL
                     ::OnFunctionReturn( ::oResult )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      /* We have a progress */
   CASE cCode == "XHBR33"
      IF hb_inetRecvAll( ::skTCP, @cProgress, 10 ) == 10
         nProgress := hb_Deserialize( cProgress, 10 )
         IF nProgress != NIL
            lContinue := .T.
            ::OnFunctionProgress( nProgress )
         ENDIF
      ENDIF

      /* We have a progress with data*/
   CASE cCode == "XHBR34"
      IF hb_inetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
         nProgress := hb_Deserialize( cProgress, Len( cProgress ) )
         IF nProgress != NIL .AND. hb_inetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
            nDataLen := hb_GetLen8( cDataLen )
            cData := Space( nDataLen )
            IF hb_inetRecvAll( ::skTCP, @cData ) == nDataLen
               ::oResult := hb_Deserialize( ::Decrypt( cData ), nDataLen )
               IF ::oResult != NIL
                  lContinue := .T.
                  ::OnFunctionProgress( nProgress, ::oResult  )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      /* We have a progress with compressed data*/
   CASE cCode == "XHBR35"
      IF hb_inetRecvAll( ::skTCP, @cProgress ) == Len( cProgress )
         nProgress := hb_Deserialize( cProgress, Len( cProgress ) )
         IF nProgress != NIL .AND. hb_inetRecvAll( ::skTCP, @cOrigLen ) == Len( cOrigLen )
            nOrigLen := hb_GetLen8( cOrigLen )
            IF hb_inetRecvAll( ::skTCP, @cDataLen ) == Len( cDataLen )
               nDataLen := hb_GetLen8( cDataLen )
               cData := Space( nDataLen )
               IF hb_inetRecvAll( ::skTCP, @cData ) == nDataLen
                  cData := hb_Uncompress( nOrigLen, cData )
                  IF ! Empty( cData )
                     ::oResult := hb_Deserialize( ::Decrypt( cData ), nDataLen )
                     IF ::oResult != NIL
                        lContinue := .T.
                        ::OnFunctionProgress( nProgress, ::oResult )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDCASE

   RETURN lContinue

/***********************************
* Utility functions
************************************/

METHOD GetFunctionName( xId ) CLASS TRPCClient

   LOCAL cData, nPos

   IF HB_ISARRAY( xID )
      cData := xId[ 3 ]
   ELSEIF Len( ::aFunctions ) > 0
      cData := ::aFunctions[ xId ][ 3 ]
   ELSE
      cData := ""
   ENDIF

   IF ! Empty( cData )
      nPos := At( "(", cData )
      cData := SubStr( cData, 1, nPos - 1 )
   ENDIF

   RETURN cData

METHOD GetServerName( xId ) CLASS TRPCClient

   LOCAL cData

   IF HB_ISARRAY( xID )
      cData := xId[ 2 ]
   ELSE
      IF Len( ::aFunctions ) > 0
         cData := ::aFunctions[ xId ][ 2 ]
      ELSEIF Len( ::aServers ) > 0
         cData := ::aServers[ xId ][ 2 ]
      ELSE
         cData := ""
      ENDIF
   ENDIF

   RETURN cData

METHOD GetServerAddress( xId ) CLASS TRPCClient

   LOCAL cData

   IF HB_ISARRAY( xID )
      cData := xId[ 1 ]
   ELSE
      IF ! Empty( ::aFunctions )
         cData := ::aFunctions[ xId ][ 1 ]
      ELSEIF ! Empty( ::aServers )
         cData := ::aServers[ xId ][ 1 ]
      ELSE
         cData := ""
      ENDIF
   ENDIF

   RETURN cData

METHOD Encrypt( cDataIn ) CLASS TRPCClient

   IF ::bEncrypted
      RETURN hb_Crypt( cDataIn, ::cCryptKey )
   ENDIF

   RETURN cDataIn

METHOD Decrypt( cDataIn ) CLASS TRPCClient

   IF ::bEncrypted
      RETURN hb_Decrypt( cDataIn, ::cCryptKey )
   ENDIF

   RETURN cDataIn


/***********************************
* Event handlers
************************************/

METHOD OnScanComplete() CLASS TRPCClient

   IF ::bOnScanComplete != NIL
      RETURN Eval( ::bOnScanComplete )
   ENDIF

   RETURN .T.

METHOD OnScanServersProgress( aLoc ) CLASS TRPCClient

   IF ::bOnScanServersProgress != NIL
      RETURN Eval( ::bOnScanServersProgress, aLoc )
   ENDIF

   RETURN .T.

METHOD OnScanFunctionsProgress( aLoc ) CLASS TRPCClient

   IF ::bOnScanFunctionsProgress != NIL
      RETURN Eval( ::bOnScanFunctionsProgress, aLoc )
   ENDIF

   RETURN .T.

METHOD OnFunctionFail( nReason, cReason ) CLASS TRPCClient

   IF ::bOnFunctionFail != NIL
      RETURN Eval( ::bOnFunctionFail, nReason, cReason )
   ENDIF

   RETURN .T.

METHOD OnFunctionReturn( oReturn ) CLASS TRPCClient

   IF ::bOnFunctionReturn != NIL
      RETURN Eval( ::bOnFunctionReturn, oReturn )
   ENDIF

   RETURN .T.

METHOD OnFunctionProgress( nProgress, oData ) CLASS TRPCClient

   IF ::bOnFunctionProgress != NIL
      RETURN Eval( ::bOnFunctionProgress, nProgress, oData )
   ENDIF

   RETURN .T.
