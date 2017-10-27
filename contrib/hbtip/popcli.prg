/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 2007 Hannes Ziegler <hz AT knowlexbase.com> (countMail(), retrieveAll())
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/* Inet service manager: pop3 */

CREATE CLASS TIPClientPOP INHERIT TIPClient

   METHOD New( oUrl, xTrace, oCredentials )
   METHOD Open( cUrl )
   METHOD OpenDigest( cUrl )
   METHOD Close( lAutoQuit )
   METHOD Delete( nId )
   METHOD List()
   METHOD Noop()                 // Can be called repeatedly to keep-alive the connection
   METHOD Retrieve( nId, nLen )
   METHOD Rset()
   METHOD Stat()
   METHOD Top( nMsgId )          // Get Headers of mail (no body) to be able to quickly handle a message
   METHOD Quit()
   METHOD UIDL( nMsgId )         // Returns Unique ID of message n or list of unique IDs of all message inside maildrop
   METHOD countMail()
   METHOD GetOK()
   METHOD Read( nLen )
   METHOD retrieveAll( lDelete )

   METHOD getTop( nMsgId )
   METHOD getMessageRaw( nMsgId )
   METHOD getBody( nMsgId )
   METHOD getSubject( nMsgId )

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials ) CLASS TIPClientPOP

   ::super:new( oUrl, iif( hb_defaultValue( xTrace, .F. ), "pop3", xTrace ), oCredentials )

   ::nDefaultPort := iif( ::oUrl:cProto == "pop3s" .OR. ::oUrl:cProto == "pops", 995, 110 )
   ::nConnTimeout := 10000

   RETURN Self

METHOD Open( cUrl ) CLASS TIPClientPOP

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF Empty( ::oUrl:cUserid ) .OR. Empty( ::oUrl:cPassword )
      RETURN .F.
   ENDIF

   IF ::GetOk()
      ::inetSendAll( ::SocketCon, "USER " + ::oUrl:cUserid + ::cCRLF )
      IF ::GetOK()
         ::inetSendAll( ::SocketCon, "PASS " + ::oUrl:cPassword + ::cCRLF )
         IF ::GetOK()
            ::isOpen := .T.
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD OpenDigest( cUrl ) CLASS TIPClientPOP

   LOCAL nPos, nPos2, cDigest

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF Empty( ::oUrl:cUserid ) .OR. Empty( ::oUrl:cPassword )
      RETURN .F.
   ENDIF

   IF ::GetOk()
      IF ( nPos := At( "<", ::cReply ) ) > 0
         IF ( nPos2 := hb_At( ">", ::cReply, nPos + 1 ) ) > nPos
            cDigest := hb_MD5( SubStr( ::cReply, nPos, ( nPos2 - nPos ) + 1 ) + ::oUrl:cPassword )
            ::inetSendAll( ::SocketCon, "APOP " + ::oUrl:cUserid + " " + cDigest + ::cCRLF )
            IF ::GetOK()
               ::isOpen := .T.
               RETURN .T.
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

METHOD Close( lAutoQuit ) CLASS TIPClientPOP

   ::InetTimeOut( ::SocketCon )

   IF hb_defaultValue( lAutoQuit, .T. )
      ::Quit()
   ENDIF

   RETURN ::super:Close()

METHOD Delete( nId ) CLASS TIPClientPOP

   ::inetSendAll( ::SocketCon, "DELE " + hb_ntos( Int( nId ) ) + ::cCRLF )

   RETURN ::GetOk()

METHOD List() CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   ::inetSendAll( ::SocketCon, "LIST" + ::cCRLF )
   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   cRet := ""
   DO WHILE ! cStr == "." .AND. ::inetErrorCode( ::SocketCon ) == 0
      cStr := ::inetRecvLine( ::SocketCon, @nPos, 256 )
      IF ! HB_ISSTRING( cStr ) .OR. cStr == "."
         ::bEof := .T.
      ELSE
         cRet += cStr + ::cCRLF
      ENDIF
   ENDDO

   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD Noop() CLASS TIPClientPOP

   ::inetSendAll( ::SocketCon, "NOOP" + ::cCRLF )

   RETURN ::GetOk()

METHOD Retrieve( nId, nLen ) CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cRet, nRetLen, cBuffer, nRead
   LOCAL cEOM := ::cCRLF + "." + ::cCRLF        // End Of Mail

   IF ! ::bInitialized
      ::inetSendAll( ::SocketCon, "RETR " + hb_ntos( Int( nId ) ) + ::cCRLF )
      IF ! ::GetOk()
         ::bEof := .T.
         RETURN NIL
      ENDIF
      ::bInitialized := .T.
   ENDIF

   cRet := ""
   nRetLen := 0
   /* 2004-05-04 - <maurilio.longo@libero.it>
      Instead of receiving a single char at a time until after we have the full mail, let's receive as
      much as we can and stop when we reach EOM (end of mail :)) sequence. This way is _a lot_ faster
    */
   DO WHILE ::inetErrorCode( ::SocketCon ) == 0 .AND. ! ::bEof

      cBuffer := Space( 1024 )

      nRead := ::inetRecv( ::SocketCon, @cBuffer, hb_BLen( cBuffer ) )

      cRet += hb_BLeft( cBuffer, nRead )

      /* 2005-11-24 - <maurilio.longo@libero.it>
                      "- Len( cEOM )" to be sure to always find a full EOM,
                      otherwise if response breaks EOM in two, it will never
                      be found
       */
      IF ( nPos := hb_BAt( cEOM, cRet, Max( nRetLen - hb_BLen( cEOM ), 1 ) ) ) > 0
         // Remove ".CRLF"
         cRet := hb_BLeft( cRet, nPos + 1 )
         ::bEof := .T.

      ELSEIF HB_ISNUMERIC( nLen ) .AND. nLen < hb_BLen( cRet )  /* FIXME: might break UTF-8 chars */
         EXIT
      ELSE
         nRetLen += nRead
      ENDIF
   ENDDO

   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   // Remove byte-stuffed termination octet(s) if any

   RETURN StrTran( cRet, ::cCRLF + "..", ::cCRLF + "." )

METHOD Rset() CLASS TIPClientPOP

   ::inetSendAll( ::SocketCon, "RSET" + ::cCRLF )

   RETURN ::GetOk()

METHOD Stat() CLASS TIPClientPOP

   LOCAL nRead

   ::inetSendAll( ::SocketCon, "STAT" + ::cCRLF )

   RETURN ::inetRecvLine( ::SocketCon, @nRead, 128 )

METHOD Top( nMsgId ) CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   ::inetSendAll( ::SocketCon, "TOP " + hb_ntos( Int( nMsgId ) ) + " 0" + ::cCRLF )
   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   cRet := ""
   DO WHILE ! cStr == "." .AND. ::inetErrorCode( ::SocketCon ) == 0
      cStr := ::inetRecvLine( ::SocketCon, @nPos, 512 )
      IF ! HB_ISSTRING( cStr ) .OR. cStr == "."
         ::bEof := .T.
      ELSE
         cRet += cStr + ::cCRLF
      ENDIF
   ENDDO

   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD Quit() CLASS TIPClientPOP

   ::inetSendAll( ::SocketCon, "QUIT" + ::cCRLF )

   RETURN ::GetOk()

METHOD UIDL( nMsgId ) CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   IF HB_ISNUMERIC( nMsgId ) .AND. nMsgId >= 1
      ::inetSendAll( ::SocketCon, "UIDL " + hb_ntos( Int( nMsgId ) ) + ::cCRLF )
   ELSE
      ::inetSendAll( ::SocketCon, "UIDL" + ::cCRLF )
   ENDIF

   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   IF Empty( nMsgId )
      cRet := ""
      DO WHILE ! cStr == "." .AND. ::inetErrorCode( ::SocketCon ) == 0
         cStr := ::inetRecvLine( ::SocketCon, @nPos, 256 )
         IF ! HB_ISSTRING( cStr ) .OR. cStr == "."
            ::bEof := .T.
         ELSE
            cRet += cStr + ::cCRLF
         ENDIF
      ENDDO
   ELSE
      // +OK Space( 1 ) nMsg Space( 1 ) UID
      RETURN SubStr( ::cReply, RAt( Space( 1 ), ::cReply ) + 1 )
   ENDIF

   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD countMail() CLASS TIPClientPop

   LOCAL cStat

   IF ::isOpen
      ::reset()
      cStat := ::Stat()
      IF HB_ISSTRING( cStat ) .AND. hb_LeftEq( cStat, "+OK" )
         RETURN Val( SubStr( cStat, 4, hb_At( " ", cStat, 5 ) - 4 ) )
      ENDIF
   ENDIF

   RETURN -1

METHOD GetOk() CLASS TIPClientPOP

   ::cReply := ::inetRecvLine( ::SocketCon,, 128 )

   RETURN ::inetErrorCode( ::SocketCon ) == 0 .AND. ;
      HB_ISSTRING( ::cReply ) .AND. hb_LeftEq( ::cReply, "+" )

/* QUESTION: This method will return logical, NIL or string
             Is it really intended that way? [vszakats] */
METHOD Read( nLen ) CLASS TIPClientPOP

   /* Decide what to read */
   IF Empty( ::oUrl:cFile )
      RETURN ::List()  /* return NIL or string */
   ELSEIF Val( ::oUrl:cFile ) < 0
      RETURN ::Delete( -Val( ::oUrl:cFile ) ) .AND. ::Quit()  /* return logical */
   ENDIF

   RETURN ::Retrieve( Val( ::oUrl:cFile ), nLen )  /* return NIL or string */

METHOD retrieveAll( lDelete ) CLASS TIPClientPOP

   LOCAL aMails, oMail

   IF ::isOpen

      hb_default( @lDelete, .F. )

      FOR EACH oMail IN aMails := Array( ::countMail() )
         ::reset()

         oMail := TIPMail():new()
         oMail:fromString( ::retrieve( oMail:__enumIndex() ) )

         IF lDelete
            ::reset()
            ::delete( oMail:__enumIndex() )
         ENDIF
      NEXT
   ENDIF

   RETURN aMails

METHOD getTop( nMsgId ) CLASS TIPClientPOP

   LOCAL nPos, cStr, xRet

   ::inetSendAll( ::SocketCon, "TOP " + hb_ntos( Int( nMsgId ) ) + " 0" + ::cCRLF )
   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   xRet := cStr := ""
   DO WHILE ! cStr == "." .AND. ::inetErrorCode( ::SocketCon ) == 0
      cStr := ::inetRecvLine( ::SocketCon, @nPos, 1024 )
      IF HB_ISSTRING( cStr ) .AND. ! cStr == "."
         xRet += cStr + ::cCRLF
      ENDIF
   ENDDO

   RETURN xRet

METHOD getMessageRaw( nMsgId ) CLASS TIPClientPOP

   LOCAL cLine, nBytes, xRet

   ::inetSendAll( ::SocketCon, "RETR " + hb_ntos( Int( nMsgId ) ) + ::cCRLF )
   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   xRet := ""
   DO WHILE ::inetErrorCode( ::SocketCon ) == 0
      cLine := ::inetRecvLine( ::SocketCon, @nBytes, 8192 )
      IF nBytes <= 0 .OR. ! HB_ISSTRING( cLine ) .OR. cLine == "."
         EXIT
      ENDIF
      xRet += cLine + ::cCRLF
   ENDDO

   RETURN xRet

METHOD getBody( nMsgId ) CLASS TIPClientPOP

   LOCAL xRet, n, n1, i, nBoundary, cBoundary, aMsg

   IF Empty( aMsg := ::getMessageRaw( nMsgId, .T. ) )
      RETURN NIL
   ENDIF

   xRet := ""

   IF ( nBoundary := AScan( aMsg, {| cLine | n1 := hb_AtI( "boundary=", cLine ), n1 > 0 } ) ) > 0
      cBoundary := AllTrim( StrTran( SubStr( aMsg[ nBoundary ], n1 + 1 ), '"' ) )
   ENDIF

   IF ! Empty( cBoundary )
      IF ( n := AScan( aMsg, {| cLine | cBoundary $ cLine }, nBoundary + 1 ) ) > 0 .AND. ;
         ( n1 := AScan( aMsg, {| cLine | cBoundary $ cLine }, n + 1 ) ) > 0  // This must not happen, but
         FOR i := n + 3 TO n1 - 1
            xRet += aMsg[ i ] + ::cCRLF
         NEXT
      ENDIF
   ELSEIF ( n := AScan( aMsg, {| cLine | Empty( cLine ) } ) ) > 0
      FOR i := n + 1 TO Len( aMsg )
         xRet += aMsg[ i ] + ::cCRLF
      NEXT
   ENDIF

   RETURN xRet

METHOD getSubject( nMsgId ) CLASS TIPClientPOP

   LOCAL cHeader

   FOR EACH cHeader IN ::getTop( nMsgId, .T. )
      IF hb_LeftEqI( cHeader, "subject: " )
         RETURN SubStr( cHeader, Len( "subject: " ) + 1 )
      ENDIF
   NEXT

   RETURN NIL
