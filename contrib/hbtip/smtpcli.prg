/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour) (SSL support)
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

/* 2007-04-12, Hannes Ziegler <hz AT knowlexbase.com>
   Added method :sendMail()
*/

#include "hbclass.ch"

#include "tip.ch"

CREATE CLASS TIPClientSMTP FROM TIPClient

   METHOD New( oUrl, xTrace, oCredentials, cClientHost )
   METHOD Open( cUrl, lTLS )
   METHOD Close()
   METHOD Write( cData, nLen, bCommit )
   METHOD Mail( cFrom )
   METHOD Rcpt( cTo )
   METHOD Data( cData )
   METHOD Commit()
   METHOD Quit()
   METHOD GetOK()
   METHOD SendMail( oTIpMail )

   /* Methods for smtp server that require login */
   METHOD OpenSecure( cUrl, lTLS )
   METHOD Auth( cUser, cPass ) // Auth by login method
   METHOD AuthPlain( cUser, cPass ) // Auth by plain method
   METHOD ServerSuportSecure( lAuthPlain, lAuthLogin )

   HIDDEN:

   VAR isAuth INIT .F.
   VAR cClientHost

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials, cClientHost ) CLASS TIPClientSMTP

   ::super:new( oUrl, iif( HB_ISLOGICAL( xTrace ) .AND. xTrace, "smtp", xTrace ), oCredentials )

   ::nDefaultPort := iif( ::oUrl:cProto == "smtps", 465, 25 )
   ::nConnTimeout := 50000
   ::nAccessMode := TIP_WO  // a write only
   ::cClientHost := cClientHost

   RETURN Self

METHOD Open( cUrl, lTLS ) CLASS TIPClientSMTP

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF ! ::GetOk()
      RETURN .F.
   ENDIF

   hb_default( @lTLS, .F. )

   IF lTLS
      ::inetSendAll( ::SocketCon, "STARTTLS" + ::cCRLF )
      IF ::GetOk()
         ::EnableTLS( .T. )
      ENDIF
   ENDIF

   ::inetSendAll( ::SocketCon, "HELO " + iif( Empty( ::cClientHost ), "TIPClientSMTP", ::cClientHost ) + ::cCRLF )

   RETURN ::GetOk()

METHOD OpenSecure( cUrl, lTLS ) CLASS TIPClientSMTP

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF ! ::GetOk()
      RETURN .F.
   ENDIF

   hb_default( @lTLS, .F. )

   IF lTLS
      ::inetSendAll( ::SocketCon, "STARTTLS" + ::cCRLF )
      IF ::GetOk()
         ::EnableTLS( .T. )
      ENDIF
   ENDIF

   ::inetSendAll( ::SocketCon, "EHLO " + iif( Empty( ::cClientHost ), "TIPClientSMTP", ::cClientHost ) + ::cCRLF )

   RETURN ::GetOk()

METHOD GetOk() CLASS TIPClientSMTP

   ::cReply := ::inetRecvLine( ::SocketCon,, 512 )
   IF ::inetErrorCode( ::SocketCon ) != 0 .OR. ! HB_ISSTRING( ::cReply ) .OR. Left( ::cReply, 1 ) == "5"
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Close() CLASS TIPClientSMTP

   ::InetTimeOut( ::SocketCon )
   ::Quit()

   RETURN ::super:Close()

METHOD Commit() CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, ::cCRLF + "." + ::cCRLF )

   RETURN ::GetOk()

METHOD Quit() CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "QUIT" + ::cCRLF )
   ::isAuth := .F.

   RETURN ::GetOk()

METHOD Mail( cFrom ) CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "MAIL FROM: <" + cFrom + ">" + ::cCRLF )

   RETURN ::GetOk()

METHOD Rcpt( cTo ) CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "RCPT TO: <" + cTo + ">" + ::cCRLF )

   RETURN ::GetOk()

METHOD Data( cData ) CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "DATA" + ::cCRLF )
   IF ! ::GetOk()
      RETURN .F.
   ENDIF
   ::inetSendAll( ::SocketCon, cData + ::cCRLF + "." + ::cCRLF )

   RETURN ::GetOk()

METHOD Auth( cUser, cPass ) CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "AUTH LOGIN" + ::cCRLF )
   IF ::GetOk()
      ::inetSendAll( ::SocketCon, hb_base64Encode( StrTran( cUser, "&at;", "@" ) ) + ::cCRLF  )
      IF ::GetOk()
         ::inetSendAll( ::SocketCon, hb_base64Encode( cPass ) + ::cCRLF )
         IF ::GetOk()
            RETURN ::isAuth := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN ::isAuth := .F.

METHOD AuthPlain( cUser, cPass ) CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "AUTH PLAIN" + hb_base64Encode( hb_BChar( 0 ) + cUser + hb_BChar( 0 ) + cPass ) + ::cCRLF )

   RETURN ::isAuth := ::GetOk()

METHOD Write( cData, nLen, bCommit ) CLASS TIPClientSMTP

   LOCAL cRcpt

   IF ! ::bInitialized

      IF Empty( ::oUrl:cFile )  // GD user id not needed if we did not auth
         RETURN -1
      ENDIF

      IF ! ::Mail( ::oUrl:cUserid )
         RETURN -1
      ENDIF

      FOR EACH cRcpt IN hb_regexSplit( ",", ::oUrl:cFile )
         IF ! ::Rcpt( cRcpt )
            RETURN -1
         ENDIF
      NEXT

      ::inetSendAll( ::SocketCon, "DATA" + ::cCRLF )
      IF ! ::GetOk()
         RETURN -1
      ENDIF
      ::bInitialized := .T.
   ENDIF

   ::nLastWrite := ::super:Write( cData, nLen, bCommit )

   RETURN ::nLastWrite

METHOD ServerSuportSecure( /* @ */ lAuthPlain, /* @ */ lAuthLogin ) CLASS TIPClientSMTP

   lAuthLogin := .F.
   lAuthPlain := .F.

   IF ::OpenSecure()
      DO WHILE .T.
         ::GetOk()
         IF ::cReply == NIL
            EXIT
         ELSEIF "LOGIN" $ ::cReply
            lAuthLogin := .T.
         ELSEIF "PLAIN" $ ::cReply
            lAuthPlain := .T.
         ENDIF
      ENDDO
      ::Close()
   ENDIF

   RETURN lAuthLogin .OR. lAuthPlain

METHOD SendMail( oTIpMail ) CLASS TIPClientSmtp

   LOCAL cTo

   IF ! ::isOpen
      RETURN .F.
   ENDIF

   IF ! ::isAuth
      ::Auth( ::oUrl:cUserId, ::oUrl:cPassword )
      IF ! ::isAuth
         RETURN .F.
      ENDIF
   ENDIF

   ::mail( oTIpMail:getFieldPart( "From" ) )

   cTo := oTIpMail:getFieldPart( "To" )
   cTo := StrTran( cTo, e"\r\n" )
   cTo := StrTran( cTo, Chr( 9 ) )
   cTo := StrTran( cTo, " " )

   FOR EACH cTo IN hb_regexSplit( ",", cTo )
      ::rcpt( cTo )
   NEXT

   RETURN ::data( oTIpMail:toString() )
