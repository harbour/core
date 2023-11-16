/*
 * tip_MailSend() (This version started from Luiz's original work on SendMail())
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes and Patrick Mast
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

#if defined( HB_LEGACY_LEVEL4 )
FUNCTION hb_SendMail( ... )
   RETURN tip_MailSend( ... )
#endif

/*
   cServer     -> Required. IP or domain name of the mail server
   nPort       -> Optional. Port used my email server
   cFrom       -> Required. Email address of the sender
   xTo         -> Required. Character string or array of email addresses to send the email to
   xCC         -> Optional. Character string or array of email addresses for CC (Carbon Copy)
   xBCC        -> Optional. Character string or array of email addresses for BCC (Blind Carbon Copy)
   cBody       -> Optional. The body message of the email as text, or the filename of the HTML message to send.
   cSubject    -> Optional. Subject of the sending email
   aFiles      -> Optional. Array of attachments to the email to send
   cUser       -> Required. User name for the POP3 server
   cPass       -> Required. Password for cUser
   cPopServer  -> Required. POP3 server name or address
   nPriority   -> Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low
   lRead       -> Optional. If set to .T., a confirmation request is send. Standard setting is .F.
   xTrace      -> Optional. If set to .T., a log file is created (smtp-<n>.log). Standard setting is .F.
                            If a block is passed, it will be called for each log event with the message a string, no param on session close.
   lPopAuth    -> Optional. Do POP3 authentication before sending mail.
   lNoAuth     -> Optional. Disable authentication methods
   nTimeOut    -> Optional. Number os ms to wait default 10000 (10s)
   cReplyTo    -> Optional.
   lSSL        -> Optional. Need SSL at connect time (TLS need this param set to False)
   cSMTPPass   -> Optional.
   cCharset    -> Optional.
   cEncoding   -> Optional.
   cClientHost -> Optional. Domain name of the SMTP client in the format smtp.example.net OR
                            client IP surrounded by brackets as [127.0.0.1] for IPv4 or as [ipv6:address] (f.e. '[ipv6:::1]') for IPv6
                            Note: This parameter is optional for backwards compatibility, but should be provided to comply with RFC 2812.
 */
FUNCTION tip_MailSend( cServer, nPort, cFrom, xTo, xCC, xBCC, cBody, cSubject, ;
      aFiles, cUser, cPass, cPopServer, nPriority, lRead, ;
      xTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo, ;
      lSSL, cSMTPPass, cCharset, cEncoding, cClientHost )

   LOCAL cTmp
   LOCAL cTo
   LOCAL cCC
   LOCAL cBCC
   LOCAL tmp

   LOCAL oInMail
   LOCAL oUrl
   LOCAL oUrl1

   LOCAL lBodyHTML
   LOCAL lAuthTLS      := .F.
   LOCAL lConnect      := .F.
   LOCAL oPop

   /* consider any empty values invalid */
   IF Empty( cServer )
      cServer := NIL
   ENDIF
   IF Empty( nPort )
      nPort := NIL
   ENDIF

   hb_default( @cServer, "localhost" )
   hb_default( @cUser, "" )
   hb_default( @cPass, "" )
   hb_default( @nPort, 25 )
   hb_default( @lPopAuth, .T. )
   hb_default( @lNoAuth, .F. )
   hb_default( @nTimeOut, 10000 )
   hb_default( @lSSL, .F. )
   hb_default( @cSMTPPass, cPass )

   // cTo
   DO CASE
   CASE HB_ISARRAY( xTo )
      FOR tmp := Len( xTo ) TO 1 STEP -1
         IF Empty( xTo[ tmp ] )
            hb_ADel( xTo, tmp, .T. )
         ENDIF
      NEXT
      IF Empty( xTo )
         RETURN .F.
      ENDIF
      cTo := ""
      FOR EACH cTmp IN xTo
         cTo += tip_GetRawEmail( AllTrim( cTmp ) )
         IF ! cTmp:__enumIsLast()
            cTo += ","
         ENDIF
      NEXT
   CASE HB_ISSTRING( xTo )
      cTo := tip_GetRawEmail( AllTrim( xTo ) )
   ENDCASE

   // CC (Carbon Copy)
   DO CASE
   CASE HB_ISARRAY( xCC )
      FOR tmp := Len( xCC ) TO 1 STEP -1
         IF Empty( xCC[ tmp ] )
            hb_ADel( xCC, tmp, .T. )
         ENDIF
      NEXT
      cCC := ""
      FOR EACH cTmp IN xCC
         cCC += tip_GetRawEmail( AllTrim( cTmp ) )
         IF ! cTmp:__enumIsLast()
            cCC += ","
         ENDIF
      NEXT
   CASE HB_ISSTRING( xCC )
      cCC := tip_GetRawEmail( AllTrim( xCC ) )
   ENDCASE

   // BCC (Blind Carbon Copy)
   DO CASE
   CASE HB_ISARRAY( xBCC )
      FOR tmp := Len( xBCC ) TO 1 STEP -1
         IF Empty( xBCC[ tmp ] )
            hb_ADel( xBCC, tmp, .T. )
         ENDIF
      NEXT
      cBCC := ""
      FOR EACH cTmp IN xBCC
         cBCC += tip_GetRawEmail( AllTrim( cTmp ) )
         IF ! cTmp:__enumIsLast()
            cBCC += ","
         ENDIF
      NEXT
   CASE HB_ISSTRING( xBCC )
      cBCC := tip_GetRawEmail( AllTrim( xBCC ) )
   ENDCASE

   cUser := StrTran( cUser, "@", "&at;" )

   IF HB_ISSTRING( cPopServer ) .AND. lPopAuth

      BEGIN SEQUENCE WITH __BreakBlock()
         oUrl1 := TUrl():New( iif( lSSL, "pop3s://", "pop://" ) + cUser + ":" + cPass + "@" + cPopServer + "/" )
         oUrl1:cUserid := StrTran( cUser, "&at;", "@" )
         oPop := TIPClientPOP():New( oUrl1, xTrace )
      RECOVER
         RETURN .F.
      END SEQUENCE

      IF oPop:Open()
         oPop:Close()
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   BEGIN SEQUENCE WITH __BreakBlock()
      oUrl := TUrl():New( iif( lSSL, "smtps://", "smtp://" ) + cUser + iif( Empty( cSMTPPass ), "", ":" + cSMTPPass ) + "@" + cServer )
   RECOVER
      RETURN .F.
   END SEQUENCE

   oUrl:nPort   := nPort
   oUrl:cUserid := StrTran( cUser, "&at;", "@" )

   oUrl:cFile := ;
      cTo + ;
      iif( Empty( cCC ), "", "," + cCC ) + ;
      iif( Empty( cBCC ), "", "," + cBCC )

   BEGIN SEQUENCE WITH __BreakBlock()
      oInmail := TIPClientSMTP():New( oUrl, xTrace,, cClientHost )
   RECOVER
      RETURN .F.
   END SEQUENCE

   oInmail:nConnTimeout := nTimeOut

   IF ! lNoAuth
      IF oInMail:OpenSecure( , lSSL )

         lAuthTLS := oInMail:lTLS

         IF ( oInMail:lAuthLogin .AND. oInMail:Auth( cUser, cSMTPPass ) ) .OR. ;
            ( oInMail:lAuthPlain .AND. oInMail:AuthPlain( cUser, cSMTPPass ) )
            lConnect := .T.
         ENDIF
      ENDIF
      IF ! lConnect
         oInMail:Close()
         BEGIN SEQUENCE WITH __BreakBlock()
            oInmail := TIPClientSMTP():New( oUrl, xTrace,, cClientHost )
         RECOVER
            RETURN .F.
         END SEQUENCE

         oInmail:nConnTimeout := nTimeOut
      ENDIF
   ENDIF

   IF ! lConnect
      IF ! oInMail:Open( NIL, lAuthTLS )
         oInmail:Close()
         RETURN .F.
      ENDIF
   ENDIF

   /* If the string is an existing HTML filename, load it. */
   SWITCH Lower( hb_FNameExt( cBody ) )
   CASE ".htm"
   CASE ".html"
      IF hb_vfExists( cBody )
         cBody := MemoRead( cBody )
         lBodyHTML := .T.
         EXIT
      ENDIF
   OTHERWISE
      lBodyHTML := .F.
   ENDSWITCH

   oInMail:oUrl:cUserid := tip_GetRawEmail( cFrom )

   IF ( tmp := ( oInMail:Write( tip_MailAssemble( cFrom, xTo, xCC, cBody, ;
      cSubject, aFiles, nPriority, lRead, cReplyTo, cCharset, ;
      cEncoding, lBodyHTML ) ) > 0 ) )

      oInMail:Commit()
   ENDIF
   oInMail:Close()

   RETURN tmp
