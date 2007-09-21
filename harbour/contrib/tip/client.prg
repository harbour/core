/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
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

/* 2004-01-13
  Enhaced tip cliente to conenct to secure smtp servers by Luiz Rafael Culik
*/

/* 2007-03-29, Hannes Ziegler
   Adapted all :new() method(s) so that tIPClient becomes the
   abstract super class for TIpClientFtp, TIpClientHttp, TIpClientPop and TIpClientSmtp

   Added Methods :INetErrorDesc(), :lastErrorCode() and :lastErrorMessage()
   Removed method :data() since it calls an undeclared method :getOk()
   :data() is used in TIpClientSmtp

   Fixed bug in :readToFile()

*/

/* 2007-06-01, Toninho@fwi
   Added data ::nWrite to work like ::nRead
*/

#include "hbcompat.ch"
#include "hbclass.ch"
#include "error.ch"
#include "fileio.ch"
#include "tip.ch"
#include "common.ch"
/**
* Inet Client class
*/

CLASS tIPClient

   CLASSDATA   bInitSocks  INIT .F.
   CLASSDATA   cCRLF       INIT HB_InetCRLF()
   DATA oUrl                                 // url to wich to connect
   DATA oCredentials                         // credential needed to access the service
   DATA nStatus                              // basic status
   DATA SocketCon
   Data lTrace
   Data nHandle

   /* Input stream length */
   DATA nLength
   /* Input stream data read by the app*/
   DATA nRead
   /* Last physical read amount */
   DATA nLastRead

   DATA nDefaultPort
   DATA nConnTimeout
   DATA bInitialized

   DATA cReply
   DATA nAccessMode
   DATA nWrite
   DATA nLastWrite

   DATA bEof
   DATA isOpen INIT .F.

   /** Gauge control; it can be a codeblock or a function pointer. */
   DATA exGauge

   DATA Cargo

   METHOD New( oUrl, lTrace, oCredentials )
   METHOD Open()

   METHOD Read( iLen )
   METHOD ReadToFile( cFile, nMode, nSize )
   METHOD Write( cData, iLen, bCommit )
   METHOD Commit()
   METHOD WriteFromFile( cFile )
   METHOD Reset()
   METHOD Close()
/*   METHOD Data( cData ) */                   // commented: calls undeclared METHOD :getOk

   METHOD lastErrorCode() INLINE ::nLastError
   METHOD lastErrorMessage(SocketCon) INLINE ::INetErrorDesc(SocketCon)

   PROTECTED:
   DATA nLastError INIT 0

   /* Methods to log data if needed */
   METHOD InetRecv( SocketCon, cStr1, len)
   METHOD InetRecvLine( SocketCon, nLen, size )
   METHOD InetRecvAll( SocketCon, cStr1, len )
   METHOD InetCount( SocketCon )
   METHOD InetSendAll( SocketCon,  cData, nLen )
   METHOD InetErrorCode(SocketCon)
   METHOD InetErrorDesc(SocketCon)
   METHOD InetConnect( cServer, nPort, SocketCon )

   METHOD Log()

ENDCLASS


METHOD New( oUrl, lTrace, oCredentials ) CLASS tIPClient
   LOCAL oErr

   Default lTrace to .F.

   IF .not. ::bInitSocks
      HB_InetInit()
      ::bInitSocks := .T.
   ENDIF

   IF HB_IsString( oUrl )
      oUrl := tUrl():New( oUrl )
   ENDIF

   IF .NOT. oURL:cProto $ "ftp,http,pop,smtp"
      oErr := ErrorNew()
      oErr:Args          := { Self, oURL:cProto }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "unsupported protocol"
      oErr:GenCode       := EG_UNSUPPORTED
      oErr:Operation     := ::className()+":new()"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1081
      oErr:SubSystem     := "BASE"
      Eval( ErrorBlock(), oErr )
   ENDIF

   ::oUrl         := oUrl
   ::oCredentials := oCredentials
   ::nStatus      := 0
   ::bInitialized := .F.
   ::nWrite       := 0
   ::nLastWrite   := 0
   ::nLength      := -1
   ::nRead        := 0
   ::nLastRead    := 0
   ::bEof         := .F.
   ::lTrace       := lTrace

RETURN self



METHOD Open( cUrl ) CLASS tIPClient

   LOCAL nPort

   IF HB_IsString( cUrl )
      ::oUrl := tUrl():New( cUrl )
   ENDIF

   IF ::oUrl:nPort == -1
      nPort := ::nDefaultPort
   ELSE
      nPort := ::oUrl:nPort
   ENDIF

   ::SocketCon := HB_InetCreate()

   HB_InetTimeout( ::SocketCon, ::nConnTimeout )
   ::InetConnect( ::oUrl:cServer, nPort, ::SocketCon )

   IF ::InetErrorCode( ::SocketCon ) != 0
      RETURN .F.
   ENDIF

   ::isOpen := .T.
RETURN .T.



METHOD Close() CLASS tIPClient

   local nRet:=-1

   IF .not. Empty( ::SocketCon )

      nRet := HB_InetClose( ::SocketCon )

      ::SocketCon:=nil
      ::isOpen := .F.
   ENDIF

RETURN(nRet)



METHOD Reset() CLASS tIPClient
   ::bInitialized := .F.
   ::bEof := .F.
RETURN .T.



METHOD Commit() CLASS tIPClient
RETURN .T.



METHOD Read( nLen ) CLASS tIPClient
   LOCAL cStr0, cStr1

   IF ::nLength > 0 .and. ::nLength == ::nRead
      RETURN NIL
   ENDIF

   IF Empty( nLen ) .or. nLen < 0 .or.( ::nLength > 0 .and. nLen > ::nLength - ::nRead )
      nLen := ::nLength - ::nRead
   ENDIF

   IF Empty( nLen ) .or. nLen < 0
      // read till end of stream
      cStr1 := Space( 1024 )
      cStr0 := ""
      ::nLastRead := ::InetRecv( ::SocketCon, @cStr1, 1024 )
      DO WHILE ::nLastRead > 0
         ::nRead += ::nLastRead
         cStr0 += Substr( cStr1, 1, ::nLastRead )
         ::nLastRead := ::InetRecv( ::SocketCon, @cStr1, 1024 )
      ENDDO
      ::bEof := .T.
   ELSE
      // read an amount of data
      cStr0 := Space( nLen )

      // S.R. if len of file is less than 1024 HB_InetRecvAll return 0
      //      ::nLastRead := HB_InetRecvAll( ::SocketCon, @cStr0, nLen )

      ::InetRecvAll( ::SocketCon, @cStr0, nLen )
      ::nLastRead := ::InetCount( ::SocketCon )
      ::nRead += ::nLastRead

      IF ::nLastRead != nLen
         ::bEof := .T.
         cStr0 := Substr( cStr0, 1, ::nLastRead )
         // S.R.         RETURN NIL
      ENDIF

      IF ::nRead == ::nLength
         ::bEof := .T.
      ENDIF

   ENDIF
RETURN cStr0



METHOD ReadToFile( cFile, nMode, nSize ) CLASS tIPClient
   LOCAL nFout
   LOCAL cData
   LOCAL nSent

   IF Empty ( nMode )
      nMode := FC_NORMAL
   ENDIF

   nSent := 0

   IF !Empty( ::exGauge )
      HB_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
   ENDIF

   ::nRead   := 0
   ::nStatus := 1

   DO WHILE ::InetErrorCode( ::SocketCon ) == 0 .and. .not. ::bEof
      cData := ::Read( 1024 )
      IF cData == NIL
         IF nFout != NIL
            Fclose( nFout )
         ENDIF
         IF ::InetErrorCode( ::SocketCon ) > 0
            RETURN .F.
         ELSE
            RETURN .T.
         ENDIF
      ENDIF
      IF nFout == NIL
         nFout := Fcreate( cFile, nMode )
         IF nFout < 0
            ::nStatus := 0
            RETURN .F.
         ENDIF
      ENDIF

      IF Fwrite( nFout, cData ) < 0
         Fclose( nFout )
         RETURN .F.
      ENDIF

      nSent += Len( cData )
      IF !Empty( ::exGauge )
         HB_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
      ENDIF

   ENDDO

   IF nSent > 0
      ::Commit()
   Endif

   ::nStatus := 2
   Fclose( nFout )
RETURN .T.



METHOD WriteFromFile( cFile ) CLASS tIPClient
   LOCAL nFin
   LOCAL cData
   LOCAL nLen
   LOCAL nSize, nSent

   ::nWrite  := 0
   ::nStatus := 0
   nFin := Fopen( cFile, FO_READ )
   IF nFin < 0
      RETURN .F.
   ENDIF
   nSize := FSeek( nFin, 0, 2 )
   FSeek( nFin, 0 )


   // allow initialization of the gauge
   nSent := 0
   IF ! Empty( ::exGauge )
      HB_ExecFromArray( ::exGauge, {nSent, nSize, Self} )
   ENDIF

   ::nStatus := 1
   cData := Space( 1024 )
   nLen := Fread( nFin, @cData, 1024 )

   DO WHILE nLen > 0
      IF ::Write( @cData, nLen ) != nLen
         Fclose( nFin )
         RETURN .F.
      ENDIF
      nSent += nLen
      IF ! Empty( ::exGauge )
         HB_ExecFromArray( ::exGauge, {nSent, nSize, Self} )
      ENDIF
      nLen := Fread( nFin, @cData, 1024 )
   ENDDO

   // it may happen that the file has lenght 0
   IF nSent > 0
      ::Commit()
   ENDIF

   ::nStatus := 2
   Fclose( nFin )
RETURN .T.


/*
HZ: METHOD :getOk() is not declared in TIpClient

METHOD Data( cData ) CLASS tIPClient
   ::InetSendall( ::SocketCon, "DATA" + ::cCRLF )
   IF .not. ::GetOk()
      RETURN .F.
   ENDIF
   ::InetSendall(::SocketCon, cData + ::cCRLF + "." + ::cCRLF )
RETURN ::GetOk()
*/


METHOD Write( cData, nLen, bCommit ) CLASS tIPClient

   IF Empty( nLen )
      nLen := Len( cData )
   ENDIF

   ::nLastWrite := ::InetSendall( ::SocketCon,  cData , nLen )

   IF .not. Empty( bCommit ) .and. bCommit

      ::Commit()

   ENDIF

   ::nWrite += ::nLastWrite

RETURN ::nLastWrite



METHOD InetSendAll( SocketCon, cData, nLen ) CLASS tIPClient

   Local nRet

   IF Empty( nLen )
      nLen := Len( cData )
   ENDIF

   nRet := HB_InetSendAll( SocketCon, cData, nLen )

   if ::lTrace

      ::Log( SocketCon, nlen, cData, nRet )

   endif

Return nRet



METHOD InetCount( SocketCon ) CLASS tIPClient

   Local nRet

   nRet := HB_InetCount( SocketCon )

   if ::lTrace

      ::Log( SocketCon, nRet )

   endif

Return nRet



METHOD InetRecv( SocketCon, cStr1, len ) CLASS tIPClient

   Local nRet

   nRet := HB_InetRecv( SocketCon, @cStr1, len )

   if ::lTrace

      ::Log( SocketCon, "", len, iif( nRet >= 0, cStr1, nRet ) )

   endif

Return nRet



METHOD InetRecvLine( SocketCon, nLen, size ) CLASS tIPClient

   Local cRet

   cRet := HB_InetRecvLine( SocketCon, @nLen, size )

   if ::lTrace

      ::Log( SocketCon, "", size, cRet )

   endif

Return cRet



METHOD InetRecvAll( SocketCon, cStr1, len ) CLASS tIPClient

   Local nRet

   nRet := HB_InetRecvAll( SocketCon, @cStr1, len )

   if ::lTrace

      ::Log( SocketCon, "", len, iif( nRet >= 0, cStr1, nRet ) )

   endif

Return nRet



METHOD InetErrorCode( SocketCon ) CLASS tIPClient

   Local nRet

   ::nLastError := nRet := HB_InetErrorCode( SocketCon )

   if ::lTrace

      ::Log( SocketCon, nRet )

   endif

Return nRet


METHOD InetErrorDesc( SocketCon ) CLASS tIPClient
   LOCAL cMsg := ""

   DEFAULT SocketCon TO ::SocketCon

   IF .not. Empty( SocketCon )

      cMsg := HB_InetErrorDesc( SocketCon )

   ENDIF
RETURN cMsg


/* BROKEN, should test number of parameters and act accordingly, see doc\inet.txt */
METHOD InetConnect( cServer, nPort, SocketCon ) CLASS tIPClient

   HB_InetConnect( cServer, nPort, SocketCon )

   if ::lTrace

      ::Log( cServer, nPort, SocketCon )

   endif

Return Nil


/* Called from another method with list of parameters and, as last parameter, return code
   of function being logged.
   Example, I want to log MyFunc( a, b, c ) which returns m,
            ::Log( a, b, c, m )
*/
METHOD Log( ... ) CLASS tIPClient

   LOCAL xVar
   LOCAL cMsg := DToS( Date() ) + "-" + Time() + Space( 2 ) + ;
                 SubStr( ProcName( 1 ), Rat( ":", ProcName( 1 ) ) ) +;
                 "( "

   for each xVar in hb_aParams()

      // Preserves CRLF on result
      if xVar:__enumIndex() < PCount()
         cMsg += StrTran( StrTran( AllTrim( CStr( xVar ) ), Chr( 13 ) ), Chr( 10 ) )
      else
         cMsg += CStr( xVar )
      endif

      cMsg += iif ( xVar:__enumIndex() < PCount() - 1, ", ", "" )

      if xVar:__enumIndex() == PCount() - 1
         cMsg += " )" + hb_OsNewLine() + ">> "

      elseif xVar:__enumIndex() == PCount()
         cMsg += " <<" + hb_OsNewLine() + hb_OsNewLine()

      endif

   next

   fWrite( ::nHandle, cMsg )

RETURN Self
