/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 2007 Hannes Ziegler <hz AT knowlexbase.com> (sendMail())
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour) (SSL support)
 * Copyright 2015 Jean Lefebvre (STARTTLS support)
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

#include "tip.ch"

CREATE CLASS TIPClientSMTP INHERIT TIPClient

   VAR lAuthLOGIN INIT .F.
   VAR lAuthPLAIN INIT .F.
   VAR lTLS       INIT .F.

   METHOD New( oUrl, xTrace, oCredentials, cClientHost )
   METHOD Open( cUrl, lSSL )
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
   METHOD OpenSecure( cUrl, lSSL )
   METHOD Auth( cUser, cPass )      /* Auth by login method */
   METHOD AuthPlain( cUser, cPass ) /* Auth by plain method */
   METHOD ServerSuportSecure( lAuthPlain, lAuthLogin )
   METHOD StartTLS()
   METHOD DetectSecurity()

   HIDDEN:

   VAR isAuth INIT .F.
   VAR cClientHost

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials, cClientHost ) CLASS TIPClientSMTP

   ::super:new( oUrl, iif( hb_defaultValue( xTrace, .F. ), "smtp", xTrace ), oCredentials )

   ::nDefaultPort := iif( ::oUrl:cProto == "smtps", 465, 25 )
   ::nConnTimeout := 50000
   ::nAccessMode := TIP_WO  // a write only
   ::cClientHost := cClientHost

   RETURN Self

METHOD Open( cUrl, lSSL ) CLASS TIPClientSMTP

   LOCAL lOk

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF ! ::GetOk()
      RETURN .F.
   ENDIF

   IF hb_defaultValue( lSSL, .F. )
      ::EnableSSL( .T. )
      ::lAuthLogin := .T.
      ::lAuthPlain := .T.
   ENDIF

   ::inetSendAll( ::SocketCon, "HELO " + iif( Empty( ::cClientHost ), "TIPClientSMTP", ::cClientHost ) + ::cCRLF )

   DO WHILE .T.
      IF !( lOk := ::GetOk() ) .OR. ::cReply == NIL .OR. ;
         hb_LeftEq( ::cReply, "250 " )
         EXIT
      ENDIF
   ENDDO

   RETURN lOk

METHOD OpenSecure( cUrl, lSSL ) CLASS TIPClientSMTP

   LOCAL lOk

   IF ! ::super:Open( cUrl )
      RETURN .F.
   ENDIF

   IF ! ::GetOk()
      RETURN .F.
   ENDIF

   hb_default( @lSSL, .F. )

   IF lSSL
      ::EnableSSL( .T. )
      ::lAuthLogin := .T.
      ::lAuthPlain := .T.
   ENDIF

   ::inetSendAll( ::SocketCon, "EHLO " + iif( Empty( ::cClientHost ), "TIPClientSMTP", ::cClientHost ) + ::cCRLF )

   lOk := ::DetectSecurity()

   IF lOk .AND. ! lSSL .AND. ::lTLS
      lOk := ::StartTLS()
   ENDIF

   RETURN lOk

METHOD GetOk() CLASS TIPClientSMTP

   ::cReply := ::inetRecvLine( ::SocketCon,, 512 )
   IF ::inetErrorCode( ::SocketCon ) != 0 .OR. ! HB_ISSTRING( ::cReply ) .OR. hb_LeftEq( ::cReply, "5" )
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD StartTLS() CLASS TIPClientSMTP

   ::inetSendAll( ::SocketCon, "STARTTLS" + ::cCRLF )

   IF ::GetOk() .AND. ::lHasSSL
      ::EnableSSL( .T. )
      __tip_SSLConnectFD( ::ssl, ::SocketCon )
      ::inetSendAll( ::SocketCon, "EHLO " + iif( Empty( ::cClientHost ), "TIPClientSMTP", ::cClientHost ) + ::cCRLF )
      RETURN ::DetectSecurity()
   ENDIF

   RETURN .F.

METHOD DetectSecurity() CLASS TIPClientSMTP

   LOCAL lOk

   DO WHILE .T.
      IF !( lOk := ::GetOk() ) .OR. ;
         ::cReply == NIL
         EXIT
      ENDIF
      IF "LOGIN" $ ::cReply
         ::lAuthLogin := .T.
      ENDIF
      IF "PLAIN" $ ::cReply
         ::lAuthPlain := .T.
      ENDIF
      IF ::HasSSL() .AND. "STARTTLS" $ ::cReply
         ::lTLS := .T.
         ::lAuthLogin := .T.
         ::lAuthPlain := .T.
      ENDIF

      IF hb_LeftEq( ::cReply, "250 " )
         EXIT
      ENDIF
   ENDDO

   RETURN lOk

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

   ::inetSendAll( ::SocketCon, "AUTH PLAIN " + hb_base64Encode( hb_BChar( 0 ) + cUser + hb_BChar( 0 ) + cPass ) + ::cCRLF )

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

   RETURN ::super:Write( cData, nLen, bCommit )

METHOD ServerSuportSecure( /* @ */ lAuthPlain, /* @ */ lAuthLogin ) CLASS TIPClientSMTP

   IF ::OpenSecure()
      lAuthLogin := ::lAuthLogin
      lAuthPlain := ::lAuthPlain
      ::Close()
   ELSE
      lAuthLogin := .F.
      lAuthPlain := .F.
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

   FOR EACH cTo IN hb_regexSplit( ",", hb_StrReplace( oTIpMail:getFieldPart( "To" ), ;
                                                      { tip_CRLF(), Chr( 9 ), " " } ) )
      ::rcpt( cTo )
   NEXT

   RETURN ::data( oTIpMail:toString() )
