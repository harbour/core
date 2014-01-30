/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://harbour-project.org
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

/* 2007-04-10, Hannes Ziegler <hz AT knowlexbase.com>
   Added method :countMail()
   Added method :retrieveAll()
 */

#include "hbclass.ch"

/* Inet service manager: pop3 */

CREATE CLASS TIPClientPOP FROM TIPClient

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

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials ) CLASS TIPClientPOP

   ::super:new( oUrl, iif( HB_ISLOGICAL( xTrace ) .AND. xTrace, "pop3", xTrace ), oCredentials )

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

   ::inetSendAll( ::SocketCon, "DELE " + hb_ntos( nId ) + ::cCRLF )

   RETURN ::GetOk()

METHOD List() CLASS TIPClientPOP

   LOCAL nPos
   LOCAL cStr, cRet

   ::inetSendAll( ::SocketCon, "LIST" + ::cCRLF )
   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   cRet := ""
   DO WHILE !( cStr == "." ) .AND. ::inetErrorCode( ::SocketCon ) == 0
      cStr := ::inetRecvLine( ::SocketCon, @nPos, 256 )
      IF !( cStr == "." )
         cRet += cStr + ::cCRLF
      ELSE
         ::bEof := .T.
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
      ::inetSendAll( ::SocketCon, "RETR " + hb_ntos( nId ) + ::cCRLF )
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

      nRead := ::inetRecv( ::SocketCon, @cBuffer, 1024 )

      cRet += Left( cBuffer, nRead )

      /* 2005-11-24 - <maurilio.longo@libero.it>
                      "- Len( cEOM )" to be sure to always find a full EOM,
                      otherwise if response breaks EOM in two, it will never
                      be found
       */
      IF ( nPos := hb_At( cEOM, cRet, Max( nRetLen - Len( cEOM ), 1 ) ) ) != 0
         // Remove ".CRLF"
         cRet := Left( cRet, nPos + 1 )
         ::bEof := .T.

      ELSEIF ! Empty( nLen ) .AND. nLen < Len( cRet )
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

   ::inetSendAll( ::SocketCon, "TOP " + hb_ntos( nMsgId ) + " 0" + ::cCRLF )
   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   cRet := ""
   DO WHILE !( cStr == "." ) .AND. ::inetErrorCode( ::SocketCon ) == 0
      cStr := ::inetRecvLine( ::SocketCon, @nPos, 512 )
      IF !( cStr == "." )
         cRet += cStr + ::cCRLF
      ELSE
         ::bEof := .T.
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

   IF ! Empty( nMsgId )
      ::inetSendAll( ::SocketCon, "UIDL " + hb_ntos( nMsgId ) + ::cCRLF )
   ELSE
      ::inetSendAll( ::SocketCon, "UIDL" + ::cCRLF )
   ENDIF

   IF ! ::GetOk()
      RETURN NIL
   ENDIF

   IF ! Empty( nMsgId )
      // +OK Space( 1 ) nMsg Space( 1 ) UID
      RETURN SubStr( ::cReply, RAt( Space( 1 ), ::cReply ) + 1 )
   ELSE
      cRet := ""
      DO WHILE !( cStr == "." ) .AND. ::inetErrorCode( ::SocketCon ) == 0
         cStr := ::inetRecvLine( ::SocketCon, @nPos, 256 )
         IF !( cStr == "." )
            cRet += cStr + ::cCRLF
         ELSE
            ::bEof := .T.
         ENDIF
      ENDDO
   ENDIF

   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN NIL
   ENDIF

   RETURN cRet

METHOD countMail() CLASS TIPClientPop

   LOCAL cStat

   IF ::isOpen
      ::reset()
      cStat := ::stat()
      IF hb_LeftIs( cStat, "+OK" )
         RETURN Val( SubStr( cStat, 4, hb_At( " ", cStat, 5 ) - 4 ) )
      ENDIF
   ENDIF

   RETURN -1

METHOD GetOk() CLASS TIPClientPOP

   LOCAL nLen

   ::cReply := ::inetRecvLine( ::SocketCon, @nLen, 128 )
   IF ::inetErrorCode( ::SocketCon ) != 0 .OR. ! hb_LeftIs( ::cReply, "+" )
      RETURN .F.
   ENDIF

   RETURN .T.

/* QUESTION: This method will return .T./.F./NIL or string
             Is it really intended that way? [vszakats] */
METHOD Read( nLen ) CLASS TIPClientPOP

   /* Set what to read for */
   IF Empty( ::oUrl:cFile )
      RETURN ::List()
   ENDIF

   IF Val( ::oUrl:cFile ) < 0
      IF ::Delete( -Val( ::oUrl:cFile ) )
         RETURN ::Quit()
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN ::Retrieve( Val( ::oUrl:cFile ), nLen )

METHOD retrieveAll( lDelete )

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
