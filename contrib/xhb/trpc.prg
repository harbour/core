/*
 * xHarbour Project source code:
 * Remote Procedure Call code
 * xHarbour part
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

/*
   XHB Remote procedure call protocol

   NOTES:
   All packets begin with the string "XHBR??" where ?? are 2 readable characters
   containing the packet type. In the protocol, a field in "" means a serialized
   string. A field in {} means a serialized array. A field in '' means a literal
   set of bytes (characters).

   Serialized strings: 4 chars lenght in network order, and then the string.
   Function serial numbers: "AAAAMMDD.C", where c is a developer defined character.

   UDP requests:
   00 - Server scan
      + "Server Name" ( valid regex or nothing )

   01 - Function scan
     + "Function Name" (valid regex)
     + "Minimum Serial" (00000000.0 for all)

   UDP replies:
   10 - Server available
      + "server name"
   11 - Function available
      +"server name"
      +"function description" as returned by TRPCFunction::Describe()

   12 - Too many functions in function request



   TCP requests:

   As UDP +

   20 - Function call
     <LEN8> - raw data length
     "Function name" + { Param1, ... Param N }

   21 - Compressed function call
     <LEN8> - Original data length
     <LEN8> - compressed data length
     * follows compressed data containing serialized name + params

   22 - Loop Function Call
     <LEN8> - Raw data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     Numeric BEGIN
     Numeric END
     Numeric STEP
     "Function name" + { Param1, ... Param N }
     Note: the parameter called $1 is the loop indicator

   23 - Loop Function Call / Compressed
     <LEN8> - Original data length
     <LEN8> - compressed data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     * follows compressed data containing:
     Numeric BEGIN
     Numeric END
     Numeric STEP
     "Function name" + { Param1, ... Param N }
     Note: the parameter called $1 is the loop indicator

   24 - Foreach function call
     <LEN8> - raw data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     "Function name" + { Param1, ... Param N }
     +Array containing the elements
     Note: the parameter called $1 is substitued with the foreach

   25 - Foreach function call / Compressed
     <LEN8> - Original data length
     <LEN8> - compressed data length
     "A" or "C" or "E": send all results/ send compressed result/
         send confirmation at end
     * follows compressed data containing:
     "Function name" + { Param1, ... Param N }
     +Array containing the elements
     Note: the parameter called $1 is substitued with the foreach

   29 - Cancelation request

   TCP REPLIES:

   30 - Function result
      + <LEN8> - raw data len
      + Serialized result

   31 - Compressed result
     + <LEN8> - Original data length
     + <LEN8> -compressed data len
     + Compressed data containing serialized result

   33 - Progress
     + Serialized progress number (0 to 100 float)

   34 - Progress with raw data
     + Serialized progress number (0 to 100 float) (10 chars)
     + <LEN8> - raw data len
     + Uncompressed progress data

   35 - Progress with compressed data
     + Serialized progress number (0 to 100 float) (10 chars)
     + <LEN8> - Original data length
     + <LEN8> - compressed data lenght
     + Compressed progress data


   40 - Function call error
      00 - Function not present
      01 - Not enough level
      02 - wrong parameters

      10 - Internal function error
         <LEN8> (function specific error code)+ cErrorDesc:45
      11 - busy, retry later

      20 - Protocol error

   90 - LOGIN
      <LEN8> + USERID:PASSWORD
   91 - LOGIN STATUS
      'OK'
      'NO'
   92 - GOODBYE

   93 - Encripted login
      <LEN8> Total length
      'USERID:ENCRYPTED( Random data + PASSWORD:pwd: + Random data)'

   94 - Challenge
      <LEN8> Total length
      'ENCRYPT(CHALLENGE DATA)'

   95 - Challenge reply
      <NUM8> - the CRC32 checksum of challenge.


*/

#include "hbclass.ch"

#include "hbrpc.ch"

/************************************
* RPC FUNCTION
*************************************/

CREATE CLASS TRPCFunction

   VAR cName
   VAR aParameters
   VAR cReturn
   VAR cSerial
   VAR nAuthLevel

   VAR oExecutable
   VAR oMethod

   VAR aCall

   CLASS VAR cPattern INIT hb_regexComp( "^C:[0-9]{1,6}$|^A$|^O$|^D$|^N$|^NI$" )

   METHOD New( cFname, cSerial, nAuthLevel, oExec, oMeth ) CONSTRUCTOR
   METHOD SetCallable( oExec, oMeth )
   METHOD CheckTypes( aParams )
   METHOD CheckParam( cParam )
   METHOD Describe()
   METHOD Run( aParams, oClient )

ENDCLASS

METHOD New( cFname, cSerial, nAuthLevel, oExec, oMeth ) CLASS TRPCFunction

   LOCAL cParam
   LOCAL aParams, aFuncDef

   // Analyze the function definition
   aFuncDef := hb_regex( "^([a-zA-Z0-9_-]+)\(([^)]*)\) *(-->)? *(.*)$", cFname )
   IF Empty( aFuncDef )
      Alert( "Invalid function defintion" )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   ::cName := aFuncDef[ 2 ]
   cParam := aFuncDef[ 3 ]
   ::cReturn := iif( Len( aFuncDef ) == 4, aFuncDef[ 4 ], aFuncDef[ 5 ] )

   // analyze parameter list
   IF Len( RTrim( cParam ) ) > 0
      aParams := hb_ATokens( cParam, "," )
      ::aParameters := {}
      FOR EACH cParam IN aParams
         cParam := AllTrim( Upper( cParam ) )
         ::CheckParam( cParam )
         AAdd( ::aParameters, cParam )
      NEXT
   ELSE
      ::aParameters := {}
   ENDIF

   // Analyze function definition return
   ::CheckParam( ::cReturn )

   // Analyze function serial number
   IF ! hb_regexMatch( "[0-9]{8}\..", cSerial )
      Alert( "Serial value not valid" )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   // analyze function authorization level
   IF nAuthLevel < 1
      Alert( "Authorization level must be at least 1" )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   ::cSerial := cSerial
   ::nAuthLevel := nAuthLevel

   // Set now Executable object if given
   IF oExec != NIL
      ::SetCallable( oExec, oMeth )
   ENDIF

   RETURN Self

METHOD SetCallable( oExec, oMeth ) CLASS TRPCFunction

   // If the callable is an object, we need to store the method
   IF HB_ISOBJECT( oExec )
      ::aCall := Array( Len( ::aParameters ) + 3 )
      ::aCall[ 2 ] := oMeth
   ELSE
      ::aCall := Array( Len( ::aParameters ) + 2 )
   ENDIF

   ::aCall[ 1 ] := oExec

   RETURN .T.

METHOD Run( aParams, oClient ) CLASS TRPCFunction

   LOCAL nStart, nCount, xRet

   IF ! ::CheckTypes( aParams )
      RETURN NIL
   ENDIF

   nStart := iif( HB_ISOBJECT( ::aCall[ 1 ] ), 3, 2 )

   FOR nCount := 1 TO Len( aParams )
      ::aCall[ nStart ] := aParams[ nCount ]
      nStart++
   NEXT

   ::aCall[ nStart ] := oClient

   xRet := hb_ExecFromArray( ::aCall )

   RETURN xRet

METHOD CheckParam( cParam ) CLASS TRPCFunction

   IF ! hb_regexMatch( ::cPattern, cParam )
      Alert( "TRPCFunction:CheckParam() wrong parameter specification: " + cParam )
      QUIT
   ENDIF

   RETURN .T.

METHOD CheckTypes( aParams ) CLASS TRPCFunction

   LOCAL oElem, i := 0

   IF ! HB_ISARRAY( aParams )
      RETURN .F.
   ENDIF

   IF Len( aParams ) != Len( ::aParameters )
      RETURN .F.
   ENDIF

   FOR EACH oElem in ::aParameters
      i++
      IF !( ValType( aParams[ i ] ) == oElem[ 1 ] )
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

METHOD Describe() CLASS TRPCFunction

   LOCAL cRet := ::cName + "("
   LOCAL nCount

   IF Len( ::aParameters ) > 0
      FOR nCount := 1 TO Len( ::aParameters ) - 1
         cRet += ::aParameters[ nCount ] + ","
      NEXT
      cRet += ATail( ::aParameters )
   ENDIF

   cRet += ")-->" + ::cReturn

   RETURN cRet + "/" + ::cSerial


/***********************************************************
* Connection manager class; this manages a single connection
************************************************************/

CREATE CLASS TRPCServeCon

   /* back reference to the parent to get callback blocks */
   VAR oServer

   /* Socket, mutex and thread */
   VAR skRemote
   VAR mtxBusy
   VAR thSelf INIT NIL

   /* Assigned authorization level */
   VAR nAuthLevel

   /* User ID */
   VAR cUserId

   /* Allow progress ? */
   VAR lAllowProgress

   METHOD New( oParent, skIn ) CONSTRUCTOR
   METHOD Destroy()

   /* Managing async */
   METHOD Start()
   METHOD Stop()
   METHOD Run()

   /* Utilty */
   METHOD SendProgress( nProgress, oData )
   METHOD IsCanceled()        INLINE ::lCanceled

   METHOD GetStatus()         INLINE ::nStatus
   HIDDEN:
   /* Current status */
   VAR  nStatus              INIT RPCS_STATUS_NONE
   /* Is this connection encrypted? */
   VAR bEncrypted
   /* crc for challenge handshake */
   VAR nChallengeCRC
   /* Temporary supposed user in challenge */
   VAR cChallengeUserid
   VAR cCryptKey

   /* Function execution data */
   VAR thFunction   INIT NIL
   VAR lCanceled    INIT  .F.

   METHOD RecvAuth( lEncrypt )
   METHOD RecvChallenge()
   METHOD RecvFunction( bComp, bMode )
   METHOD FuncCall( cData )
   METHOD FuncLoopCall( cMode, cData )
   METHOD FuncForeachCall( cMode, cData )
   METHOD LaunchChallenge( cUserid, cPassword )
   METHOD LaunchFunction( cFuncName, aParams, nMode, aDesc )
   METHOD FunctionRunner( cFuncName, oFunc, nMode, aParams, aDesc )
   METHOD SendResult( oRet, cFuncName )

   METHOD Encrypt( cDataIn )
   METHOD Decrypt( cDataIn )

ENDCLASS

METHOD New( oParent, skIn ) CLASS TRPCServeCon

   ::oServer := oParent
   ::skRemote := skIn
   ::mtxBusy := hb_mutexCreate()
   ::bEncrypted := .F.
   ::nAuthLevel := 0
   ::nChallengeCRC := -1

   RETURN Self

METHOD Destroy() CLASS TRPCServeCon

   hb_mutexLock( ::mtxBusy )
   // Eventually wait for the function to terminate
   IF ::thFunction != NIL
      ::lCanceled := .T.
      hb_mutexUnlock( ::mtxBusy )
      hb_threadJoin( ::thFunction )
      hb_mutexLock( ::mtxBusy )
   ENDIF

   ::skRemote := NIL
   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD Start() CLASS TRPCServeCon

   LOCAL lRet := .F.

   hb_mutexLock( ::mtxBusy )
   IF ::thSelf == NIL
      ::thSelf := StartThread( Self, "RUN" )
      lRet := .T.
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN lRet

METHOD Stop() CLASS TRPCServeCon

   LOCAL lRet := .F.

   hb_mutexLock( ::mtxBusy )
   IF hb_threadID( ::thSelf ) != 0
      hb_threadQuitRequest( ::thSelf )
      lRet := .T.
      hb_mutexUnlock( ::mtxBusy )
      hb_threadJoin( ::thSelf )
      ::thSelf := NIL
   ELSE
      hb_mutexUnlock( ::mtxBusy )
   ENDIF

   RETURN lRet

METHOD Run() CLASS TRPCServeCon

   LOCAL cCode := Space( 6 )
   LOCAL lBreak := .F.
   LOCAL aData
   LOCAL nSafeStatus

   DO WHILE hb_inetErrorCode( ::skRemote ) == 0 .AND. ! lBreak

      /* Get the request code */
      hb_inetRecvAll( ::skRemote, @cCode, 6 )
      IF hb_inetErrorCode( ::skRemote ) != 0
         EXIT
      ENDIF

      hb_mutexLock( ::mtxBusy )
      nSafeStatus := ::nStatus
      hb_mutexUnlock( ::mtxBusy )

      DO CASE

         /* Check for TCP server scan */
      CASE cCode == "XHBR00"
         hb_inetSendAll( ::skRemote, ;
            "XHBR10" + hb_Serialize( ::oServer:cServerName ) )

         /* Read autorization request */
      CASE cCode == "XHBR90"
         IF nSafeStatus == RPCS_STATUS_NONE
            lBreak := ! ::RecvAuth( .F. )
            IF ! lBreak
               nSafeStatus := RPCS_STATUS_LOGGED
            ENDIF
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Read encrypted autorization request */
      CASE cCode == "XHBR93"
         IF nSafeStatus == RPCS_STATUS_NONE
            lBreak := ! ::RecvAuth( .T. )
            IF ! lBreak
               nSafeStatus := RPCS_STATUS_CHALLENGE
            ENDIF
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Challeng reply */
      CASE cCode == "XHBR95"
         IF nSafeStatus == RPCS_STATUS_CHALLENGE
            lBreak := ! ::RecvChallenge()
            IF ! lBreak
               nSafeStatus := RPCS_STATUS_LOGGED
            ENDIF
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Close connection */
      CASE cCode == "XHBR92"
         ::oServer:OnClientLogout( Self )
         lBreak := .T.

         /* Execute function */
      CASE cCode == "XHBR20"
         IF nSafeStatus == RPCS_STATUS_LOGGED
            aData := ::RecvFunction( .F., .F. )
            IF aData != NIL
               lBreak := ! ::FuncCall( aData[ 2 ] )
            ELSE
               lBreak := .T.
            ENDIF
         ELSEIF nSafeStatus == RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_BUSY
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF


         /* Execute function */
      CASE cCode == "XHBR21"
         IF nSafeStatus == RPCS_STATUS_LOGGED
            aData := ::RecvFunction( .T., .F. )
            IF aData != NIL
               lBreak := ! ::FuncCall( aData[ 2 ] )
            ELSE
               lBreak := .T.
            ENDIF
         ELSEIF nSafeStatus == RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_BUSY
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Loop function */
      CASE cCode == "XHBR22"
         IF nSafeStatus == RPCS_STATUS_LOGGED
            aData := ::RecvFunction( .F., .T. )
            IF aData != NIL
               lBreak := ! ::FuncLoopCall( aData[ 1 ], aData[ 2 ] )
            ELSE
               lBreak := .T.
            ENDIF
         ELSEIF nSafeStatus == RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_BUSY
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Loop function - compressed */
      CASE cCode == "XHBR23"
         IF nSafeStatus == RPCS_STATUS_LOGGED
            aData := ::RecvFunction( .T., .T. )
            IF aData != NIL
               lBreak := ! ::FuncLoopCall( aData[ 1 ], aData[ 2 ] )
            ELSE
               lBreak := .T.
            ENDIF
         ELSEIF nSafeStatus == RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_BUSY
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Foreach function */
      CASE cCode == "XHBR24"
         IF nSafeStatus == RPCS_STATUS_LOGGED
            aData := ::RecvFunction( .F., .T. )
            IF aData != NIL
               lBreak := ! ::FuncForeachCall( aData[ 1 ], aData[ 2 ] )
            ELSE
               lBreak := .T.
            ENDIF
         ELSEIF nSafeStatus == RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_BUSY
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Foreach function - compressed*/
      CASE cCode == "XHBR25"
         IF nSafeStatus == RPCS_STATUS_LOGGED
            aData := ::RecvFunction( .T., .T. )
            IF aData  != NIL
               lBreak := ! ::FuncForeachCall( aData[ 1 ], aData[ 2 ] )
            ELSE
               lBreak := .T.
            ENDIF
         ELSEIF nSafeStatus == RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_BUSY
         ELSE
            nSafeStatus := RPCS_STATUS_ERROR
         ENDIF

         /* Function execution cancelation request */
      CASE cCode == "XHBR29"
            /* Note: even if the function is already terminated in the
               meanwhile, and the -real- status is not RUNNING anymore,
               there is no problem here. The cancelation request will
               be reset at next function call, and the caller must ignore
               any pending data before the "cancel" call */
         IF nSafeStatus != RPCS_STATUS_RUNNING
            nSafeStatus := RPCS_STATUS_ERROR
         ELSE
            hb_mutexLock( ::mtxBusy )
            ::lCanceled := .T.
            hb_mutexUnlock( ::mtxBusy )
            hb_inetSendAll( ::skRemote, "XHBR34" )
         ENDIF

      OTHERWISE
         lBreak := .T.
      ENDCASE

      /* Analisys of the nSafeStatus code */
      DO CASE
      CASE nSafeStatus == RPCS_STATUS_BUSY
         hb_inetSendAll( ::skRemote, "XHBR4011" )

      CASE nSafeStatus == RPCS_STATUS_ERROR
         hb_inetSendAll( ::skRemote, "XHBR4020" )

         /* Update real status only if not in error case */
      OTHERWISE
            /* The running status is set (in this thread) indipendently
               by the function launcher, if everything is fine */
         hb_mutexLock( ::mtxBusy )
         IF ::nStatus != RPCS_STATUS_RUNNING
            ::nStatus := nSafeStatus
         ENDIF
         hb_mutexUnlock( ::mtxBusy )
      ENDCASE

   ENDDO

   // signaling termination of this thread
   ::oServer:Terminating( Self )
   // Destroy resources just before termination
   ::Destroy()

   RETURN .T.

METHOD RecvAuth( lEncrypt ) CLASS TRPCServeCon

   LOCAL cLength := Space( 8 ), nLen, nPos
   LOCAL cUserID, cPassword
   LOCAL cReadIn

   IF hb_inetRecvAll( ::skRemote, @cLength, 8 ) != 8
      RETURN .F.
   ENDIF

   nLen := hb_GetLen8( cLength )

   IF ( lEncrypt .AND. nLen > 128 ) .OR. ( ! lEncrypt .AND. nLen > 37 )
      RETURN .F.
   ENDIF

   cReadIn := Space( nLen )
   IF hb_inetRecvAll( ::skRemote, @cReadin, nLen ) != nLen
      RETURN .F.
   ENDIF

   nPos := hb_BAt( ":", cReadin )
   IF nPos == 0
      RETURN .F.
   ENDIF

   cUserID := hb_BSubStr( cReadin, 1, nPos - 1 )
   cPassword := hb_BSubStr( cReadin, nPos + 1 )

   IF ! lEncrypt
      ::nAuthLevel := ::oServer:Authorize( cUserid, cPassword )
      IF ::nAuthLevel == 0
         hb_inetSendAll( ::skRemote, "XHBR91NO" )
         RETURN .F.
      ENDIF

      hb_inetSendAll( ::skRemote, "XHBR91OK" )
      IF hb_inetErrorCode( ::skRemote ) != 0
         RETURN .F.
      ENDIF
      ::cUserId := cUserId
      ::oServer:OnClientLogin( Self )
      RETURN .T.
   ENDIF

   RETURN ::LaunchChallenge( cUserid, cPassword )

METHOD LaunchChallenge( cUserid, cPassword ) CLASS TRPCServeCon

   LOCAL cChallenge, nCount

   ::cCryptKey := ::oServer:AuthorizeChallenge( cUserid, cPassword )
   IF Empty( ::cCryptKey )
      RETURN .F.
   ENDIF

   ::cChallengeUserid := cUserid

   /* Let's generate the sequence */
   cChallenge := ""
   FOR nCount := 1 TO 255
      cChallenge += hb_BChar( hb_Random( 0, 255 ) )
   NEXT

   ::nChallengeCRC := hb_Checksum( cChallenge )
   cChallenge := hb_Crypt( cChallenge, ::cCryptKey )

   hb_inetSendAll( ::skRemote, "XHBR94" + hb_CreateLen8( hb_BLen( cChallenge ) ) + cChallenge )

   IF hb_inetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

   RETURN .T.


METHOD RecvChallenge() CLASS TRPCServeCon

   LOCAL cNumber := Space( 8 )

   IF hb_inetRecvAll( ::skRemote, @cNumber ) != 8
      RETURN .F.
   ENDIF

   IF ::nChallengeCRC != hb_GetLen8( cNumber )
      RETURN .F.
   ENDIF

   hb_inetSendAll( ::skRemote, "XHBR91OK" )
   IF hb_inetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

   ::nAuthLevel := ::oServer:Authorize( ::cChallengeUserid )
   /* It is always possible that the user has been deleted in the meanwhile */
   IF ::nAuthLevel == 0
      RETURN .F.
   ENDIF

   ::cUserId := ::cChallengeUserid
   ::bEncrypted := .T.
   ::oServer:OnClientLogin( Self )

   RETURN .T.

METHOD RecvFunction( bComp, bMode ) CLASS TRPCServeCon

   LOCAL cLength := Space( 8 ), nLen, nComp
   LOCAL cMode := " "
   LOCAL cData

   /* Original lenght of data */
   IF hb_inetRecvAll( ::skRemote, @cLength, 8 ) != 8
      RETURN NIL
   ENDIF

   nLen := hb_GetLen8( cLength )
   IF nLen > 65000
      RETURN NIL
   ENDIF

   /* compressed lenght */
   IF bComp
      IF hb_inetRecvAll( ::skRemote, @cLength, 8 ) != 8
         RETURN NIL
      ENDIF

      nComp := hb_GetLen8( cLength )
   ELSE
      nComp := nLen
   ENDIF

   /* Mode */
   IF bMode
      IF hb_inetRecvAll( ::skRemote, @cMode ) != 1
         RETURN NIL
      ENDIF
   ENDIF

   /* Get data */
   cData := Space( nComp )
   IF hb_inetRecvAll( ::skRemote, @cData ) != nComp
      RETURN NIL
   ENDIF

   /* Eventually decrypt it */
   IF ::bEncrypted
      cData := ::Decrypt( cData )
   ENDIF

   /* Eventually uncompress it */
   IF bComp
      cData := hb_Uncompress( nLen, cData )
   ENDIF

   RETURN { cMode, cData }

METHOD FuncCall( cData ) CLASS TRPCServeCon

   LOCAL cSer, cFuncName, aParams

   /* Deserialize all elements */
   cSer := hb_DeserialBegin( cData )
   IF cSer == NIL
      RETURN .F.
   ENDIF
   cFuncName := hb_DeserialNext( @cSer )
   aParams := hb_DeserialNext( @cSer )

   IF aParams == NIL
      RETURN .F.
   ENDIF

   ::oServer:OnClientRequest( Self, 20, { cFuncName, aParams } )

   RETURN ::LaunchFunction( cFuncName, aParams, 0 )

METHOD FuncLoopCall( cMode, cData ) CLASS TRPCServeCon

   LOCAL nBegin, nEnd, nStep
   LOCAL cSer
   LOCAL cFuncName, aParams

   /* Deserialize all elements */
   cSer := hb_DeserialBegin( cData )
   IF Empty( cSer )
      RETURN .F.
   ENDIF
   nBegin := hb_DeserialNext( @cSer )
   nEnd := hb_DeserialNext( @cSer )
   nStep := hb_DeserialNext( @cSer )
   cFuncName := hb_DeserialNext( @cSer )
   aParams := hb_DeserialNext( @cSer )

   IF aParams == NIL
      RETURN .F.
   ENDIF

   ::oServer:OnClientRequest( Self, 22, { cFuncName, aParams, cMode, nBegin, nEnd, nStep } )

   RETURN ::LaunchFunction( cFuncName, aParams, 1, { cMode, nBegin, nEnd, nStep } )

METHOD FuncForeachCall( cMode, cData ) CLASS TRPCServeCon

   LOCAL cSer
   LOCAL cFuncName, aParams
   LOCAL aItems

   /* Deserialize all elements */
   cSer := hb_DeserialBegin( cData )
   IF Empty( cSer )
      RETURN .F.
   ENDIF

   cFuncName := hb_DeserialNext( @cSer )
   aParams := hb_DeserialNext( @cSer )
   aItems := hb_DeserialNext( @cSer )

   IF aItems  == NIL
      RETURN .F.
   ENDIF

   ::oServer:OnClientRequest( Self, 24, { cFuncName, aParams, aItems } )

   RETURN ::LaunchFunction( cFuncName, aParams, 2, { cMode, aItems } )

METHOD LaunchFunction( cFuncName, aParams, nMode, aDesc ) CLASS TRPCServeCon

   LOCAL oFunc

   // Check for function existance
   oFunc := ::oServer:Find( cFuncName )
   IF Empty( oFunc )
      // signal error
      ::oServer:OnFunctionError( Self, cFuncName, 0 )
      hb_inetSendAll( ::skRemote, "XHBR4000" )
      RETURN .T.
   ENDIF

   // check for level
   IF oFunc:nAuthLevel > ::nAuthLevel
      // signal error
      ::oServer:OnFunctionError( Self, cFuncName, 1 )
      hb_inetSendAll( ::skRemote, "XHBR4001" )
      RETURN .T.
   ENDIF

   // check for parameters
   IF aParams == NIL .OR. ! oFunc:CheckTypes( aParams )
      // signal error
      ::oServer:OnFunctionError( Self, cFuncName, 2 )
      hb_inetSendAll( ::skRemote, "XHBR4002" )
      RETURN .T.
   ENDIF

   hb_mutexLock( ::mtxBusy )
   // allow progress indicator by default
   ::lAllowProgress := .T.
   // setting the cancel indicator as false
   ::lCanceled := .F.
   // Set the running status
   ::nStatus := RPCS_STATUS_RUNNING
   ::thFunction := StartThread( Self, "FunctionRunner", ;
      cFuncName, oFunc, nMode, aParams, aDesc )
   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD FunctionRunner( cFuncName, oFunc, nMode, aParams, aDesc ) CLASS TRPCServeCon

   LOCAL nCount
   LOCAL oRet, oElem, aRet
   LOCAL aSubst, nSubstPos

   // ? "TH:", ::thFunction
   DO CASE

   CASE nMode == 0  // just run the function
      oRet := oFunc:Run( aParams, Self )
      // Notice: SendResult checks for lCanceled before really sending

   CASE nMode == 1 // run in loop
      aSubst := AClone( aParams )
      nSubstPos := AScan( aParams, {| x | HB_ISSTRING( x ) .AND. x == "$." } )

      SWITCH aDesc[ 1 ]
      CASE "A" // all results
         FOR nCount := aDesc[ 2 ] TO aDesc[ 3 ] STEP aDesc[ 4 ]
            IF nSubstPos > 0
               aSubst[ nSubstPos ] := nCount
            ENDIF
            oRet := oFunc:Run( aSubst, Self )
            ::SendResult( oRet, cFuncName )
         NEXT
         oRet := "Done"
         EXIT

      CASE "C" // Vector of all results
         aRet := {}
         ::lAllowProgress := .F.
         FOR nCount := aDesc[ 2 ] TO aDesc[ 3 ] STEP aDesc[ 4 ]
            IF nSubstPos > 0
               aSubst[ nSubstPos ] := nCount
            ENDIF
            oRet :=  oFunc:Run( aSubst, Self )
            IF oRet == NIL
               ::SendResult( NIL, cFuncName )
               EXIT
            ENDIF
            AAdd( aRet, oRet )
         NEXT
         IF oRet != NIL
            oRet := aRet
         ENDIF
         EXIT

      CASE "E" // Just send confirmation at end
         ::lAllowProgress := .F.
         FOR nCount := aDesc[ 2 ] TO aDesc[ 3 ] STEP aDesc[ 4 ]
            IF nSubstPos > 0
               aSubst[ nSubstPos ] := nCount
            ENDIF
            oRet := oFunc:Run( aSubst, Self )
            IF oRet == NIL
               ::SendResult( NIL, cFuncName )
               EXIT
            ENDIF
         NEXT
         IF oRet != NIL
            oRet := "Done"
         ENDIF
         EXIT
      ENDSWITCH

   CASE nMode == 2 // Run in a foreach loop
      aSubst := AClone( aParams )
      nSubstPos := AScan( aParams, {| x | HB_ISSTRING( x ) .AND. x == "$." } )

      SWITCH aDesc[ 1 ]
      CASE "A" // all results
         FOR EACH oElem IN  aDesc[ 2 ]
            IF nSubstPos > 0
               aSubst[ nSubstPos ] := oElem
            ENDIF
            oRet := oFunc:Run( aSubst, Self )
            ::SendResult( oRet, cFuncName )
         NEXT
         oRet := "Done"
         EXIT

      CASE "C" // Vector of all results
         aRet := {}
         ::lAllowProgress := .F.
         FOR EACH oElem IN  aDesc[ 2 ]
            IF nSubstPos > 0
               aSubst[ nSubstPos ] := oElem
            ENDIF
            oRet := oFunc:Run( aSubst, Self )
            IF oRet == NIL
               ::SendResult( NIL, cFuncName )
               EXIT
            ENDIF
            AAdd( aRet, oRet )
         NEXT
         IF oRet != NIL
            oRet := aRet
         ENDIF
         EXIT

      CASE "E" // Just send confirmation at end
         ::lAllowProgress := .F.
         FOR EACH oElem IN aDesc[ 2 ]
            IF nSubstPos > 0
               aSubst[ nSubstPos ] := oElem
            ENDIF
            oRet := oFunc:Run( aSubst, Self )
            IF oRet == NIL
               EXIT
            ENDIF
         NEXT
         EXIT
      ENDSWITCH
   ENDCASE

   // Now we can signal that execution terminated
   hb_mutexLock( ::mtxBusy )
   ::nStatus := RPCS_STATUS_LOGGED
   hb_mutexUnlock( ::mtxBusy )
   // The execution of the function terminates BEFORE the sending of
   // the last data or the confirmation data, even if the thread
   // has still something to do.
   ::SendResult( oRet, cFuncName )

   // Signal that the thread is no longer alive
   // Should not be needed!
   /*
   hb_mutexLock( ::mtxBusy )
   ::thFunction := -1
   hb_mutexUnlock( ::mtxBusy )
   */

   RETURN .T.

METHOD SendResult( oRet, cFuncName )

   LOCAL cData, cOrigLen, cCompLen

   // Ignore requests to send result if function is canceled
   hb_mutexLock( ::mtxBusy )
   IF ::lCanceled
      hb_mutexUnlock( ::mtxBusy )
      ::oServer:OnFunctionCanceled( Self, cFuncName )
      RETURN .T. // as if it were done
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   IF oRet == NIL
      ::oServer:OnFunctionError( Self, cFuncName, 10 )
      hb_inetSendAll( ::skRemote, "XHBR4010" )
   ELSE
      cData := hb_Serialize( oRet )
      cOrigLen := hb_CreateLen8( Len( cData ) )
      ::oServer:OnFunctionReturn( Self, cData )
      // should we compress it ?

      IF Len( cData ) > 512
         cData := hb_Compress( cData )
         cCompLen := hb_CreateLen8( Len( cData ) )
         hb_inetSendAll( ::skRemote, "XHBR31" + cOrigLen + cCompLen + ::Encrypt( cData ) )
      ELSE
         hb_inetSendAll( ::skRemote, "XHBR30" + cOrigLen + ::Encrypt( cData ) )
      ENDIF
   ENDIF

   IF hb_inetErrorCode( ::skRemote ) != 0
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD SendProgress( nProgress, oData ) CLASS TRPCServeCon

   LOCAL cOrigLen, cCompLen, lRet := .T.
   LOCAL cData

   // Ignore if told so
   hb_mutexLock( ::mtxBusy )
   IF ! ::lAllowProgress .OR. ::lCanceled
      hb_mutexUnlock( ::mtxBusy )
      RETURN .T.
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   ::oServer:OnFunctionProgress( Self, nProgress, oData )
   IF Empty( oData )
      hb_inetSendAll( ::skRemote, "XHBR33" + hb_Serialize( nProgress ) )
   ELSE
      cData := hb_Serialize( oData )
      cOrigLen := hb_CreateLen8( Len( cData ) )
      // do we should compress it ?
      IF Len( cData ) > 512
         cData := hb_Compress( cData )
         cCompLen := hb_CreateLen8( Len( cData ) )
         hb_inetSendAll( ::skRemote, "XHBR35" + hb_Serialize( nProgress ) + ;
            cOrigLen + cCompLen + ::Encrypt( cData ) )
      ELSE
         hb_inetSendAll( ::skRemote, "XHBR34" + hb_Serialize( nProgress ) + ;
            cOrigLen + ::Encrypt( cData ) )
      ENDIF
   ENDIF

   IF hb_inetErrorCode( ::skRemote ) != 0
      lRet := .F.
   ENDIF

   RETURN lRet

METHOD Encrypt( cDataIn ) CLASS TRPCServeCon

   IF ::bEncrypted
      RETURN hb_Crypt( cDataIn, ::cCryptKey )
   ENDIF

   RETURN cDataIn

METHOD Decrypt( cDataIn ) CLASS TRPCServeCon

   IF ::bEncrypted
      RETURN hb_Decrypt( cDataIn, ::cCryptKey )
   ENDIF

   RETURN cDataIn

/************************************
* RPC SERVICE
*************************************/

CREATE CLASS TRPCService

   VAR cServerName INIT "RPCGenericServer"
   VAR aFunctions
   CLASS VAR lInit INIT hb_inetInit()

   VAR nUdpPort INIT 1139
   VAR nTcpPort INIT 1140
   VAR cBindAddress INIT NIL
   VAR thAccept INIT 0
   VAR thUdp INIT 0
   VAR aServing INIT {}
   VAR mtxBusy INIT hb_mutexCreate()

   VAR skUdp
   VAR skServer

   /* Code blocks corresponding to event handlers */
   VAR bAuthorize
   VAR bGetEncryption
   VAR bOnFunctionScan
   VAR bOnServerScan
   VAR bOnClientConnect
   VAR bOnClientLogin
   VAR bOnClientRequest
   VAR bOnFunctionProgress
   VAR bOnFunctionError
   VAR bOnFunctionReturn
   VAR bOnFunctionCanceled
   VAR bOnClientLogout
   VAR bOnClientTerminate

   METHOD New() CONSTRUCTOR

   /* Block run on client connection request */
   VAR bConnection

   /* Function management */
   METHOD Add( xFunction, cVersion, nLevel, oExec, oMethod )
   METHOD Run( cName, aParams )
   METHOD Describe( cName )
   METHOD Find( cName )
   METHOD Remove( cName )

   /* General services */
   METHOD Start( lStartUdp )
   METHOD Stop()
   METHOD StartService( skIn )
   METHOD Terminating( oConnection )

   /* Tcp services */
   METHOD Accept()

   /* UDP services */
   METHOD UdpListen()
   METHOD UDPParseRequest( cData, nPacketLen )
   METHOD UDPInterpretRequest( cData, nPacketLen, cRes )

   /* Utility */
   METHOD AuthorizeChallenge( cUserId, cData )

   /* to be overloaded */
   METHOD Authorize( cUserid, cPassword )
   /* Provide encryption key for a user */
   METHOD GetEncryption( cUserId )
   METHOD OnFunctionScan()
   METHOD OnServerScan()
   METHOD OnClientConnect( oClient )
   METHOD OnClientLogin( oClient )
   METHOD OnClientRequest( oClient, nRequest, cData )
   METHOD OnFunctionProgress( oClient, nProgress, aData )
   METHOD OnFunctionError( oClient, cFunction, nError )
   METHOD OnFunctionReturn( oClient, aData )
   METHOD OnFunctionCanceled( oClient, cFuncName )
   METHOD OnClientLogout( oClient )
   METHOD OnClientTerminate( oClient )

ENDCLASS

METHOD New() CLASS TRPCService

   ::aFunctions := {}

   RETURN Self

METHOD Add( xFunction, cVersion, nLevel, oExec, oMethod )

   LOCAL nElem, lRet := .F.
   LOCAL oFunction

   IF HB_ISSTRING( xFunction )
      oFunction := TRPCFunction():New( xFunction, cVersion, nLevel, oExec, oMethod )
   ELSE
      oFunction := xFunction
   ENDIF

   hb_mutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {| x | oFunction:cName == x:cName } )
   IF nElem == 0
      AAdd( ::aFunctions, oFunction )
      lRet := .T.
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN lRet

METHOD Find( cName ) CLASS TRPCService

   LOCAL nElem
   LOCAL oRet := NIL

   hb_mutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {| x | Upper( cName ) == Upper( x:cName ) } )
   IF nElem != 0
      oRet := ::aFunctions[ nElem ]
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN oRet

METHOD Remove( cName ) CLASS TRPCService

   LOCAL nElem
   LOCAL lRet := .F.

   hb_mutexLock( ::mtxBusy )
   nElem := AScan( ::aFunctions, {| x | cName == x:cName } )
   IF nElem != 0
      hb_ADel( ::aFunctions, nElem, .T. )
      lRet := .T.
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN lRet

METHOD Run( cName, aParams ) CLASS TRPCService

   LOCAL oFunc := ::Find( cName )
   LOCAL oRet := NIL

   hb_mutexLock( ::mtxBusy )
   IF ! Empty( oFunc )
      oRet := oFunc:Run( aParams )
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN oRet

METHOD Describe( cName ) CLASS TRPCService

   LOCAL oFunc := ::Find( cName )
   LOCAL cRet := NIL

   hb_mutexLock( ::mtxBusy )
   IF ! Empty( oFunc )
      cRet := oFunc:Describe()
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN cRet

METHOD Start( lStartUdp ) CLASS TRPCService

   IF Empty( ::cBindAddress )
      ::skServer := hb_inetServer( ::nTcpPort )
      ::skUdp := hb_inetDGramBind( ::nUdpPort )
   ELSE
      ::skServer := hb_inetServer( ::nTcpPort, ::cBindAddress )
      ::skUdp := hb_inetDGramBind( ::nUdpPort, ::cBindAddress )
   ENDIF

   ::thAccept := StartThread( Self, "Accept" )

   IF lStartUdp != NIL .AND. lStartUdp
      ::thUdp := StartThread( Self, "UdpListen" )
   ELSE
      ::thUdp := NIL
   ENDIF

   RETURN .T.

METHOD Stop() CLASS TRPCService

   LOCAL oElem

   hb_mutexLock( ::mtxBusy )
   IF hb_threadID( ::thAccept ) == 0
      hb_mutexUnlock( ::mtxBusy )
      RETURN .F.
   ENDIF

   hb_inetClose( ::skServer )
   // closing the socket will make their infinite loops to terminate.
   hb_threadQuitRequest( ::thAccept )
   hb_threadJoin( ::thAccept )
   IF hb_threadID( ::thUDP ) != 0
      hb_inetClose( ::skUdp )
      hb_threadQuitRequest( ::thUdp )
      hb_threadJoin( ::thUdp )
   ENDIF

   FOR EACH oElem IN ::aServing
      IF hb_threadID( oElem:thSelf ) != 0
         hb_threadQuitRequest( oElem:thSelf )
         hb_threadJoin( oElem:thSelf )
      ENDIF
   NEXT
   ASize( ::aServing, 0 )

   // now destroy all the allocated resources
   ::skServer := NIL
   ::skUdp := NIL

   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD Accept() CLASS TRPCService

   LOCAL skIn

   DO WHILE .T.
      skIn := hb_inetAccept( ::skServer )
      // todo: better sync
      IF hb_inetstatus( ::skServer ) < 0
         EXIT
      ENDIF
      IF skIn != NIL
         ::StartService( skIn )
      ENDIF
   ENDDO

   RETURN .T.

METHOD StartService( skIn ) CLASS TRPCService

   LOCAL oService

   hb_mutexLock( ::mtxBusy )
   oService := TRPCServeCon():New( Self, skIn )
   AAdd( ::aServing, oService )
   oService:Start()
   hb_mutexUnlock( ::mtxBusy )
   ::OnClientConnect( oService )

   RETURN .T.

METHOD UDPListen() CLASS TRPCService

   LOCAL cData := Space( 1000 )
   LOCAL nPacketLen

   DO WHILE .T.
      nPacketLen := hb_inetDGramRecv( ::skUdp, @cData, 1000 )
      IF hb_inetstatus( ::skUdp ) < 0
         EXIT
      ENDIF
      ::UDPParseRequest( cData, nPacketLen )
   ENDDO

   RETURN .T.

METHOD UDPParseRequest( cData, nPacketLen ) CLASS TRPCService

   LOCAL cToSend

   IF ::UDPInterpretRequest( cData, nPacketLen, @cToSend )
      hb_inetDGramSend( ::skUdp, ;
         hb_inetAddress( ::skUdp ), hb_inetPort( ::skUdp ), cToSend )
      RETURN .T.
   ENDIF

   RETURN .F.

METHOD UDPInterpretRequest( cData, nPacketLen, cRes ) CLASS TRPCService

   LOCAL cCode, cMatch, cNumber, cSerial
   LOCAL oFunc

   IF nPacketLen < 6
      RETURN .F.
   ENDIF

   cCode := hb_BSubStr( cData, 1, 6 )

   DO CASE
   /* XHRB00 - server scan */
   CASE cCode == "XHBR00"
      IF ! ::OnServerScan()
         RETURN .F.
      ENDIF
      IF nPacketLen > 6
         cMatch := hb_Deserialize( hb_BSubStr( cData, 7 ) )
         IF hb_regexMatch( cMatch, ::cServerName )
            cRes := "XHBR10" + hb_Serialize( ::cServerName )
         ENDIF
      ELSE
         cRes := "XHBR10" + hb_Serialize( ::cServerName )
      ENDIF
      RETURN .T.

      /* XRB01 - Function scan */
   CASE cCode == "XHBR01"
      IF ! ::OnFunctionScan()
         RETURN .F.
      ENDIF
      /* minimal length to be valid */
      IF nPacketLen > 24
         cSerial := hb_DeserialBegin( hb_BSubStr( cData, 7 ) )
         cMatch := hb_DeserialNext( @cSerial )
         cNumber := NIL
         IF ! Empty( cMatch )
            cMatch := hb_regexComp( cMatch )
            cNumber := hb_DeserialNext( @cSerial )
         ELSE
            cMatch := hb_regexComp( ".*" )
         ENDIF

         IF Empty( cNumber )
            cNumber := "00000000.0"
         ENDIF

         FOR EACH oFunc IN ::aFunctions
            IF hb_regexMatch( cMatch, oFunc:cName ) .AND. cNumber <= oFunc:cSerial
               cRes := "XHBR11" + hb_Serialize( ::cServerName ) + hb_Serialize( ofunc:Describe() )
               RETURN .T.
            ENDIF
         NEXT
      ENDIF

      /* If we don't have the function, we cannot reply */
      RETURN .F.

   ENDCASE

   /* Ignore malformed requests. */

   RETURN .F.

METHOD Terminating( oConnection ) CLASS TRPCService

   LOCAL nToken

   ::OnClientTerminate( oConnection )
   hb_mutexLock( ::mtxBusy )
   nToken := AScan( ::aServing, {| x | x == oConnection } )
   IF nToken > 0
      hb_ADel( ::aServing, nToken, .T. )
   ENDIF
   hb_mutexUnlock( ::mtxBusy )

   RETURN .T.

METHOD AuthorizeChallenge( cUserId, cData ) CLASS TRPCService

   LOCAL cKey, nPos, cMarker := "PASSWORD:"

   cKey := ::GetEncryption( cUserId )
   IF Empty( cKey )
      RETURN NIL
   ENDIF

   cData := hb_Decrypt( cData, cKey )
   nPos := At( cMarker, cData )
   IF nPos == 0
      RETURN NIL
   ENDIF

   cData := SubStr( cData, nPos + Len( cMarker ) )
   nPos := At( ":", cData )
   IF nPos == 0
      RETURN NIL
   ENDIF

   cData := SubStr( cData, 1, nPos - 1 )

   IF ::Authorize( cUserId, cData ) > 0
      RETURN cKey
   ENDIF

   RETURN NIL

/* Default authorization will ALWAYS return 1 if a bAuthorize block is not provided */
/* IF cPassword is NIL, must return the level of the given userid */

METHOD Authorize( cUserid, cPassword ) CLASS TRPCService

   IF ::bAuthorize != NIL
      RETURN Eval( ::bAuthorize, cUserid, cPassword )
   ENDIF

   RETURN 1

/* By default, do not provide an encryption key for any user */

METHOD GetEncryption( cUserId ) CLASS TRPCService

   IF ::bGetEncryption != NIL
      RETURN Eval( ::bGetEncryption, cUserId )
   ENDIF

   RETURN NIL

METHOD OnFunctionScan() CLASS TRPCService

   IF ::bOnFunctionScan != NIL
      RETURN Eval( ::bOnFunctionScan, Self )
   ENDIF

   RETURN .T.

METHOD OnServerScan() CLASS TRPCService

   IF ::bOnServerScan != NIL
      RETURN Eval( ::bOnServerScan, Self )
   ENDIF

   RETURN .T.

METHOD OnClientConnect( oClient ) CLASS TRPCService

   IF ::bOnClientConnect != NIL
      RETURN Eval( ::bOnClientConnect, oClient )
   ENDIF

   RETURN .T.

METHOD OnClientLogin( oClient ) CLASS TRPCService

   IF ::bOnClientLogin != NIL
      Eval( ::bOnClientLogin, oClient )
   ENDIF

   RETURN .T.

METHOD OnClientRequest( oClient, nRequest, cData ) CLASS TRPCService

   IF ::bOnClientRequest != NIL
      RETURN Eval( ::bOnClientRequest, oClient, nRequest, cData )
   ENDIF

   RETURN .T.

METHOD OnFunctionProgress( oClient, nProgress, aData ) CLASS TRPCService

   IF ::bOnFunctionProgress != NIL
      RETURN Eval( ::bOnFunctionProgress, oClient, nProgress, aData )
   ENDIF

   RETURN .T.

METHOD OnFunctionError( oClient, cFunction, nError ) CLASS TRPCService

   IF ::bOnFunctionError != NIL
      RETURN Eval( ::bOnFunctionError, oClient, cFunction, nError )
   ENDIF

   RETURN .T.

METHOD OnFunctionReturn( oClient, aData ) CLASS TRPCService

   IF ::bOnFunctionReturn != NIL
      RETURN Eval( ::bOnFunctionReturn, oClient, aData )
   ENDIF

   RETURN .T.

METHOD OnFunctionCanceled( oClient, cFuncName ) CLASS TRPCService

   IF ::bOnFunctionCanceled != NIL
      RETURN Eval( ::bOnFunctionCanceled, oClient, cFuncName )
   ENDIF

   RETURN .T.

METHOD OnClientLogout( oClient ) CLASS TRPCService

   IF ::bOnClientLogout != NIL
      RETURN Eval( ::bOnClientLogout, oClient )
   ENDIF

   RETURN .T.

METHOD OnClientTerminate( oClient ) CLASS TRPCService

   IF ::bOnClientTerminate != NIL
      RETURN Eval( ::bOnClientTerminate, oClient )
   ENDIF

   RETURN .T.
