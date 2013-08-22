/*
 * Harbour Project source code:
 * tip_MailSend() (This version started from Luiz's original work on SendMail())
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes and Patrick Mast
 * Copyright 2009 Viktor Szakats (harbour syenar.net) (SSL support)
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

#if defined( HB_LEGACY_LEVEL4 )
FUNCTION hb_SendMail( ... )
   RETURN tip_MailSend( ... )
#endif

/*
   cServer     -> Required. IP or domain name of the mail server
   nPort       -> Optional. Port used my email server
   cFrom       -> Required. Email address of the sender
   xTo         -> Required. Character string or array of email addresses to send the email to
   xCC         -> Optional. Character string or array of email adresses for CC (Carbon Copy)
   xBCC        -> Optional. Character string or array of email adresses for BCC (Blind Carbon Copy)
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
   lNoAuth     -> Optional. Disable Autentication methods
   nTimeOut    -> Optional. Number os ms to wait default 10000 (10s)
   cReplyTo    -> Optional.
   cClientHost -> Optional. Domain name of the SMTP client in the format smtp.example.com OR client IP surrounded by brackets as in [200.100.100.5]
                            Note: This parameter is optional for backwards compatibility, but should be provided to comply with RFC 2812.
*/
FUNCTION tip_MailSend( cServer, nPort, cFrom, xTo, xCC, xBCC, cBody, cSubject, ;
      aFiles, cUser, cPass, cPopServer, nPriority, lRead, ;
      xTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo, ;
      lTLS, cSMTPPass, cCharset, cEncoding, cClientHost )

   LOCAL cTmp
   LOCAL cTo
   LOCAL cCC
   LOCAL cBCC
   LOCAL tmp

   LOCAL oInMail
   LOCAL oUrl
   LOCAL oUrl1

   LOCAL lBodyHTML     := .F.
   LOCAL lConnectPlain := .F.
   LOCAL lReturn       := .T.
   LOCAL lAuthLogin    := .F.
   LOCAL lAuthPlain    := .F.
   LOCAL lAuthTLS      := .F.
   LOCAL lConnect      := .T.
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
   hb_default( @lTLS, .F. )
   hb_default( @cSMTPPass, cPass )

   // cTo
   IF HB_ISARRAY( xTo )
      FOR tmp := Len( xTo ) TO 1 STEP -1
         IF Empty( xTo[ tmp ] )
            hb_ADel( xTo, tmp, .T. )
         ENDIF
      NEXT
      cTo := ""
      cTmp := ""
      IF Len( xTo ) > 1
         FOR EACH cTo IN xTo
            IF cTo:__enumIndex() != 1
               cTmp += tip_GetRawEmail( AllTrim( cTo ) ) + ","
            ENDIF
         NEXT
         cTmp := SubStr( cTmp, 1, Len( cTmp ) - 1 )
      ENDIF
      cTo := tip_GetRawEmail( AllTrim( xTo[ 1 ] ) )
      IF Len( cTmp ) > 0
         cTo += "," + cTmp
      ENDIF
   ELSEIF HB_ISSTRING( xTo )
      cTo := tip_GetRawEmail( AllTrim( xTo ) )
   ENDIF

   // CC (Carbon Copy)
   IF HB_ISARRAY( xCC )
      FOR tmp := Len( xCC ) TO 1 STEP -1
         IF Empty( xCC[ tmp ] )
            hb_ADel( xCC, tmp, .T. )
         ENDIF
      NEXT
      cCC := ""
      IF Len( xCC ) > 0
         FOR EACH cTmp IN xCC
            cCC += tip_GetRawEmail( AllTrim( cTmp ) ) + ","
         NEXT
         cCC := SubStr( cCC, 1, Len( cCC ) - 1 )
      ENDIF
   ELSEIF HB_ISSTRING( xCC )
      cCC := tip_GetRawEmail( AllTrim( xCC ) )
   ENDIF

   // BCC (Blind Carbon Copy)
   IF HB_ISARRAY( xBCC )
      FOR tmp := Len( xBCC ) TO 1 STEP -1
         IF Empty( xBCC[ tmp ] )
            hb_ADel( xBCC, tmp, .T. )
         ENDIF
      NEXT
      cBCC := ""
      IF Len( xBCC ) > 0
         FOR EACH cTmp IN xBCC
            cBCC += tip_GetRawEmail( AllTrim( cTmp ) ) + ","
         NEXT
         cBCC := SubStr( cBCC, 1, Len( cBCC ) - 1 )
      ENDIF
   ELSEIF HB_ISSTRING( xBCC )
      cBCC := tip_GetRawEmail( AllTrim( xBCC ) )
   ENDIF

   cUser := StrTran( cUser, "@", "&at;" )

   IF cPopServer != NIL .AND. lPopAuth
      BEGIN SEQUENCE
         oUrl1 := TUrl():New( iif( lTLS, "pop3s://", "pop://" ) + cUser + ":" + cPass + "@" + cPopServer + "/" )
         oUrl1:cUserid := StrTran( cUser, "&at;", "@" )
         oPop := TIPClientPOP():New( oUrl1, xTrace )
         IF oPop:Open()
            oPop:Close()
         ELSE
            lReturn := .F.
         ENDIF
      RECOVER
         lReturn := .F.
      END SEQUENCE
   ENDIF

   IF ! lReturn
      RETURN .F.
   ENDIF

   BEGIN SEQUENCE
      oUrl := TUrl():New( iif( lTLS, "smtps://", "smtp://" ) + cUser + iif( Empty( cSMTPPass ), "", ":" + cSMTPPass ) + "@" + cServer )
   RECOVER
      lReturn := .F.
   END SEQUENCE

   IF ! lReturn
      RETURN .F.
   ENDIF

   oUrl:nPort   := nPort
   oUrl:cUserid := StrTran( cUser, "&at;", "@" )

   oUrl:cFile := cTo + iif( Empty( cCC ), "", "," + cCC ) + iif( Empty( cBCC ), "", "," + cBCC )

   BEGIN SEQUENCE
      oInmail := TIPClientSMTP():New( oUrl, xTrace, NIL, cClientHost )
   RECOVER
      lReturn := .F.
   END SEQUENCE

   IF ! lReturn
      RETURN .F.
   ENDIF

   oInmail:nConnTimeout := nTimeOut

   IF ! lNoAuth

      IF oInMail:OpenSecure()

         DO WHILE .T.
            IF ! oInMail:GetOk()
               EXIT
            ENDIF
            IF oInMail:cReply == NIL
               EXIT
            ELSEIF "LOGIN" $ oInMail:cReply
               lAuthLogin := .T.
            ELSEIF "PLAIN" $ oInMail:cReply
               lAuthPlain := .T.
            ELSEIF oInMail:HasSSL() .AND. "STARTTLS" $ oInMail:cReply
               lAuthTLS := .T.
            ELSEIF Left( oInMail:cReply, 4 ) == "250 "
               EXIT
            ENDIF
         ENDDO

         IF lAuthLogin
            IF ! oInMail:Auth( cUser, cSMTPPass )
               lConnect := .F.
            ELSE
               lConnectPlain := .T.
            ENDIF
         ENDIF

         IF lAuthPlain .AND. ! lConnect
            IF ! oInMail:AuthPlain( cUser, cSMTPPass )
               lConnect := .F.
            ENDIF
         ELSE
            IF ! lConnectPlain
               lConnect := .F.
            ENDIF
         ENDIF
      ELSE
         lConnect := .F.
      ENDIF
   ELSE
      lConnect := .F.
   ENDIF

   IF ! lConnect

      IF ! lNoAuth
         oInMail:Close()
      ENDIF

      BEGIN SEQUENCE
         oInmail := TIPClientSMTP():New( oUrl, xTrace, NIL, cClientHost )
      RECOVER
         lReturn := .F.
      END SEQUENCE

      oInmail:nConnTimeout := nTimeOut

      IF ! oInMail:Open( NIL, lAuthTLS )
         oInmail:Close()
         RETURN .F.
      ENDIF

      DO WHILE .T.
         IF Left( oInMail:cReply, 4 ) == "250 "
            EXIT
         ENDIF
         IF ! oInMail:GetOk()
            EXIT
         ENDIF
         IF oInMail:cReply == NIL
            EXIT
         ENDIF
      ENDDO

   ENDIF

   /* If the string is an existing HTML filename, load it. */
   IF ( Lower( hb_FNameExt( cBody ) ) == ".htm" .OR. ;
        Lower( hb_FNameExt( cBody ) ) == ".html" ) .AND. ;
      hb_FileExists( cBody )
      cBody := MemoRead( cBody )
      lBodyHTML := .T.
   ENDIF

   oInMail:oUrl:cUserid := tip_GetRawEmail( cFrom )

   oInMail:Write( tip_MailAssemble( cFrom, xTo, xCC, cBody, cSubject, aFiles, nPriority, lRead, cReplyTo, cCharset, cEncoding, lBodyHTML ) )
   oInMail:Commit()
   oInMail:Close()

   RETURN lReturn

#if defined( HB_LEGACY_LEVEL4 )
FUNCTION hb_MailAssemble( ... )
   RETURN tip_MailAssemble( ... )
#endif

FUNCTION tip_MailAssemble( ;
      cFrom, ;       /* Required. Email address of the sender */
      xTo, ;         /* Required. Character string or array of email addresses to send the email to */
      xCC, ;         /* Optional. Character string or array of email adresses for CC (Carbon Copy) */
      cBody, ;       /* Optional. The body message of the email as text, or the filename of the HTML message to send. */
      cSubject, ;    /* Optional. Subject of the sending email */
      aFiles, ;      /* Optional. Array of attachments to the email to send */
      nPriority, ;   /* Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low */
      lRead, ;       /* Optional. If set to .T., a confirmation request is send. Standard setting is .F. */
      cReplyTo, ;    /* Optional. */
      cCharset, ;    /* Optional. */
      cEncoding, ;   /* Optional. */
      lBodyHTML )    /* Optional. */

   LOCAL oMail
   LOCAL oAttach
   LOCAL aThisFile
   LOCAL cMimeText
   LOCAL cFile
   LOCAL cFileCP
   LOCAL cData
   LOCAL cContentType

   LOCAL cCharsetCP
   LOCAL tmp

   IF Empty( cFrom ) .OR. ! HB_ISSTRING( cFrom )
      RETURN .F.
   ENDIF
   IF Empty( xTo ) .OR. ( ! HB_ISSTRING( xTo ) .AND. ! HB_ISARRAY( xTo ) )
      RETURN .F.
   ENDIF

   hb_default( @cBody, "" )
   hb_default( @cSubject, "" )
   hb_default( @aFiles, {} )
   hb_default( @nPriority, 3 )
   hb_default( @lRead, .F. )
   hb_default( @cReplyTo, "" )
   hb_default( @cCharset, "UTF-8" )
   hb_default( @cEncoding, "quoted-printable" )
   hb_default( @lBodyHTML, .F. )

   /* NOTE: Either extend the conversion table from std CP ID
            to Harbour CP ID, or wait until Harbour supports std CP IDs.
            Either way, for now it only works for UTF-8. [vszakats] */
   IF Upper( cCharset ) == "UTF-8"
      cCharsetCP := "UTF8"
   ENDIF

   /* add ending EOL to body, if there wasn't any */
   IF !( Right( cBody, 2 ) == Chr( 13 ) + Chr( 10 ) )
      cBody += Chr( 13 ) + Chr( 10 )
   ENDIF

   /* Convert input to the CP of the e-mail */
   IF ! Empty( cCharsetCP )
      IF HB_ISSTRING( xTo )
         xTo := hb_Translate( xTo,, cCharsetCP )
      ELSEIF HB_ISARRAY( xTo )
         FOR EACH tmp IN xTo
            tmp := hb_Translate( tmp,, cCharsetCP )
         NEXT
      ENDIF
      IF HB_ISSTRING( xCC )
         xCC := hb_Translate( xCC,, cCharsetCP )
      ELSEIF HB_ISARRAY( xCC )
         FOR EACH tmp IN xCC
            tmp := hb_Translate( tmp,, cCharsetCP )
         NEXT
      ENDIF
      cFrom := hb_Translate( cFrom,, cCharsetCP )
      cBody := hb_Translate( cBody,, cCharsetCP )
      cSubject := hb_Translate( cSubject,, cCharsetCP )
   ENDIF

   /* Start assembling mail */

   oMail := TIPMail():new()
   oMail:SetEncoder( cEncoding )
   oMail:SetCharset( cCharset )
   oMail:SetHeader( cSubject, cFrom, xTo, xCC )
   oMail:hHeaders[ "Date" ] := tip_TimeStamp()
   IF ! Empty( cReplyTo )
      oMail:hHeaders[ "Reply-to" ] := cReplyTo
   ENDIF

   cContentType := iif( lBodyHTML, "text/html", "text/plain" ) + "; charset=" + cCharset

   IF Empty( aFiles )
      oMail:hHeaders[ "Content-Type" ] := cContentType
      oMail:SetBody( cBody )
   ELSE
      oAttach := TIPMail():new()
      oAttach:SetEncoder( cEncoding )
      oAttach:SetCharset( cCharset )
      oAttach:hHeaders[ "Content-Type" ] := cContentType
      oAttach:SetBody( cBody )
      oMail:Attach( oAttach )
   ENDIF

   FOR EACH aThisFile IN aFiles

      IF HB_ISSTRING( aThisFile )
         cFile := aThisFile
         cData := hb_MemoRead( cFile )
      ELSEIF HB_ISARRAY( aThisFile ) .AND. Len( aThisFile ) >= 2
         cFile := aThisFile[ 1 ]
         IF HB_ISSTRING( aThisFile[ 2 ] )
            cData := aThisFile[ 2 ]
            hb_default( @cFile, "unnamed" )
         ELSE
            IF ! HB_ISSTRING( cFile )
               LOOP /* No filename and no content. */
            ELSE
               cData := hb_MemoRead( cFile )
            ENDIF
         ENDIF
      ELSE
         LOOP
      ENDIF

      cData += Chr( 13 ) + Chr( 10 )

      cFileCP := iif( Empty( cCharsetCP ), cFile, hb_Translate( cFile,, cCharsetCP ) )

      oAttach := TIPMail():New()
      oAttach:SetCharset( cCharset )
      oAttach:SetEncoder( iif( IsBinaryType( cFile ), "base64", cEncoding ) )
      // Some e-mail clients use Content-Type to check for filename
      cMimeText := tip_FileNameMimeType( cFile )
      IF cMimeText == "unknown"
         cMimeText := "text/plain"  /* TOFIX: Such fallback doesn't seem right. [vszakats] */
      ENDIF
      cMimeText += "; name=" + '"' + hb_FNameNameExt( cFileCP ) + '"'
      IF Lower( hb_FNameExt( cFile ) ) == ".html" .OR. ;
         Lower( hb_FNameExt( cFile ) ) == ".htm"
         cMimeText += "; charset=" + cCharset
      ENDIF
      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      // But usually, original filename is set here
      oAttach:hHeaders[ "Content-Disposition" ] := "attachment; filename=" + '"' + hb_FNameNameExt( cFileCP ) + '"'
      oAttach:SetBody( cData )
      oMail:Attach( oAttach )
   NEXT

   IF lRead
      oMail:hHeaders[ "Disposition-Notification-To" ] := tip_GetRawEmail( cFrom )
   ENDIF

   IF nPriority != 3
      oMail:hHeaders[ "X-Priority" ] := Str( nPriority, 1 )
   ENDIF

   RETURN oMail:ToString()

/* TOFIX: Decide based on mime type instead. */
STATIC FUNCTION IsBinaryType( cFileName )
   RETURN Empty( hb_FNameExt( cFileName ) ) .OR. ;
      "|" + SubStr( hb_FNameExt( Lower( cFileName ) ), 2 ) + "|" $ "|" + ;
      "3dm|3dmf|aab|aam|aas|adr|afl|ai|aif|aifc|aiff|alt|arj|asd|" + ;
      "asf|asn|asp|asx|asz|au|avi|axs|bcpio|bin|bin|cdf|cdx|chat|" + ;
      "che|cht|class|cnc|cod|coda|con|cpi|cpio|csh|csm|css|cu|" + ;
      "dbf|dbt|dcr|dig|dir|doc|dsf|dst|dus|dvi|dwf|dwg|dxf|dxr|" + ;
      "ebk|eps|es|etf|evy|exe|fh4|fh5|fhc|fif|fpt|fpx|frl|" + ;
      "gif|gsd|gsm|gtar|gz|hdf|hqx|ica|ief|ins|ips|ips|ipx|ivr|" + ;
      "jpe|jpeg|jpg|js|latex|lha|lzh|lzx|m3u|man|map|mbd|mcf|" + ;
      "mems|mfp|mid|midi|mif|mol|mov|movie|mp2|mp3|mpe|mpeg|mpg|" + ;
      "mpire|mpl|n2p|nc|npx|nsc|ntx|oda|ofml|page|pbm|pdb|pdf|" + ;
      "pfr|pgm|pgp|php3|phtml|pnm|pot|ppm|pps|ppt|ppz|pqf|pqi|" + ;
      "ps|ptlk|push|qd3|qd3d|qrt|qt|ra|ram|ras|rgb|rip|rmf|rmf|" + ;
      "roff|rpm|rrf|rtc|rtf|sca|sh|shar|shw|sit|sml|smp|snd|spl|" + ;
      "spr|sprite|src|stk|stream|sv4cpio|sv4crc|svf|svh|svr|swa|" + ;
      "swf|t|talk|tar|tbk|tcl|tex|texi|texinfo|tif|tiff|tlk|tmv|" + ;
      "tr|tsi|tsp|ustar|vbd|vcd|vgm|vgp|vgx|viv|vivo|vmd|vmf|vox|" + ;
      "vqe|vqf|vql|vrt|vts|vtts|waf|wan|wav|wi|wid|wis|wlt|wri|" + ;
      "wrl|wrz|wtx|wtx|xbm|xdr|xls|xlt|xml|xpm|xwd|z|zip|zpa|"
