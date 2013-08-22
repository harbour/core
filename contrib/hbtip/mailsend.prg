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
      cMimeText := GetMimeType( cFile ) + "; name=" + hb_FNameNameExt( cFileCP )
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

STATIC FUNCTION IsBinaryType( cFileName )
   RETURN Empty( hb_FNameExt( cFileName ) ) .OR. hb_regexLike( ".+\.(" + ;
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
      "wrl|wrz|wtx|wtx|xbm|xdr|xls|xlt|xml|xpm|xwd|z|zip|zpa" + ")", ;
      Lower( cFileName ) )

STATIC FUNCTION GetMimeType( cFileName )

   #translate ( <exp1> LIKE <exp2> )  => hb_regexLike( <exp2>, <exp1> )

   cFileName := Lower( cFileName )

   DO CASE
   CASE ( cFileName LIKE ".+\.(3dmf|3dm|qd3d|qd3)" ); RETURN "x-world/x-3dmf"
   CASE ( cFileName LIKE ".+\.(aam|aas)" ); RETURN "application/x-authorware-map"
   CASE ( cFileName LIKE ".+\.(aif|aiff|aifc)" ); RETURN "audio/x-aiff"
   CASE ( cFileName LIKE ".+\.(ai|eps|ps)" ); RETURN "application/postscript"
   CASE ( cFileName LIKE ".+\.(asf|asx)" ); RETURN "video/x-ms-asf"
   CASE ( cFileName LIKE ".+\.(asn|asz|asd)" ); RETURN "application/astound"
   CASE ( cFileName LIKE ".+\.(au|snd)" ); RETURN "audio/basic"
   CASE ( cFileName LIKE ".+\.(bin|class|lha|lzh|lzx|dbf)" ); RETURN "application/octet-stream"
   CASE ( cFileName LIKE ".+\.(cht|dus)" ); RETURN "audio/x-dspeech"
   CASE ( cFileName LIKE ".+\.(coda|page)" ); RETURN "application/x-coda"
   CASE ( cFileName LIKE ".+\.(cu|csm)" ); RETURN "application/x-cu-seeme"
   CASE ( cFileName LIKE ".+\.(dcr|dir|dxr|swa)" ); RETURN "application/x-director"
   CASE ( cFileName LIKE ".+\.(fh5|fh4|fhc)" ); RETURN "image/x-freehand"
   CASE ( cFileName LIKE ".+\.(gsm|gsd)" ); RETURN "audio/gsm"
   CASE ( cFileName LIKE ".+\.(htm|html)" ); RETURN "text/html"
   CASE ( cFileName LIKE ".+\.(jpeg|jpg|jpe)" ); RETURN "image/jpeg"
   CASE ( cFileName LIKE ".+\.(mid|midi)" ); RETURN "audio/x-midi"
   CASE ( cFileName LIKE ".+\.(mpeg|mpg|mpe)" ); RETURN "video/mpeg"
   CASE ( cFileName LIKE ".+\.(mpl|mpire)" ); RETURN "application/x-mpire"
   CASE ( cFileName LIKE ".+\.(nc|cdf)" ); RETURN "application/x-netcdf"
   CASE ( cFileName LIKE ".+\.(pot|pps|ppt|ppz)" ); RETURN "application/mspowerpoint"
   CASE ( cFileName LIKE ".+\.(qt|mov)" ); RETURN "video/quicktime"
   CASE ( cFileName LIKE ".+\.(ram|ra)" ); RETURN "audio/x-pn-realaudio"
   CASE ( cFileName LIKE ".+\.(sml|ofml)" ); RETURN "application/fml"
   CASE ( cFileName LIKE ".+\.(svf|dwg|dxf)" ); RETURN "image/vnd"
   CASE ( cFileName LIKE ".+\.(talk|spc)" ); RETURN "text/x-speech"
   CASE ( cFileName LIKE ".+\.(texinfo|texi)" ); RETURN "application/x-texinfo"
   CASE ( cFileName LIKE ".+\.(tiff|tif)" ); RETURN "image/tiff"
   CASE ( cFileName LIKE ".+\.(t|tr|roff)" ); RETURN "application/x-troff"
   CASE ( cFileName LIKE ".+\.(vgm|vgx|xdr)" ); RETURN "video/x-videogram"
   CASE ( cFileName LIKE ".+\.(viv|vivo)" ); RETURN "video/vnd.vivo"
   CASE ( cFileName LIKE ".+\.(vqf|vql)" ); RETURN "audio/x-twinvq"
   CASE ( cFileName LIKE ".+\.(wan|waf)" ); RETURN "plugin/wanimate"
   CASE ( cFileName LIKE ".+\.(wrl|wrz)" ); RETURN "x-world/x-vrml"
   CASE ( cFileName LIKE ".+\.aab" ); RETURN "application/x-authorware-bin"
   CASE ( cFileName LIKE ".+\.adr" ); RETURN "application/x-msaddr"
   CASE ( cFileName LIKE ".+\.afl" ); RETURN "video/animaflex"
   CASE ( cFileName LIKE ".+\.alt" ); RETURN "application/x-up-alert"
   CASE ( cFileName LIKE ".+\.arj" ); RETURN "application/x-arj"
   CASE ( cFileName LIKE ".+\.asp" ); RETURN "application/x-asap"
   CASE ( cFileName LIKE ".+\.avi" ); RETURN "video/x-msvideo"
   CASE ( cFileName LIKE ".+\.axs" ); RETURN "application/olescript"
   CASE ( cFileName LIKE ".+\.bcpio" ); RETURN "application/x-bcpio"
   CASE ( cFileName LIKE ".+\.bin" ); RETURN "application/x-macbinary"
   CASE ( cFileName LIKE ".+\.ccs" ); RETURN "text/ccs"
   CASE ( cFileName LIKE ".+\.chat" ); RETURN "application/x-chat"
   CASE ( cFileName LIKE ".+\.che" ); RETURN "application/x-up-cacheop"
   CASE ( cFileName LIKE ".+\.cnc" ); RETURN "application/x-cnc"
   CASE ( cFileName LIKE ".+\.cod" ); RETURN "image/cis-cod"
   CASE ( cFileName LIKE ".+\.con" ); RETURN "application/x-connector"
   CASE ( cFileName LIKE ".+\.cpi" ); RETURN "image/cpi"
   CASE ( cFileName LIKE ".+\.cpio" ); RETURN "application/x-cpio"
   CASE ( cFileName LIKE ".+\.csh" ); RETURN "application/x-csh"
   CASE ( cFileName LIKE ".+\.css" ); RETURN "application/x-pointplus"
   CASE ( cFileName LIKE ".+\.dig" ); RETURN "multipart/mixed"
   CASE ( cFileName LIKE ".+\.doc" ); RETURN "application/msword"
   CASE ( cFileName LIKE ".+\.dsf" ); RETURN "image/x-mgx-dsf"
   CASE ( cFileName LIKE ".+\.dst" ); RETURN "application/tajima"
   CASE ( cFileName LIKE ".+\.dvi" ); RETURN "application/x-dvi"
   CASE ( cFileName LIKE ".+\.dwf" ); RETURN "drawing/x-dwf"
   CASE ( cFileName LIKE ".+\.ebk" ); RETURN "application/x-expandedbook"
   CASE ( cFileName LIKE ".+\.es" ); RETURN "audio/echospeech"
   CASE ( cFileName LIKE ".+\.etf" ); RETURN "image/x-etf"
   CASE ( cFileName LIKE ".+\.etx" ); RETURN "text/x-setext"
   CASE ( cFileName LIKE ".+\.evy" ); RETURN "application/x-envoy"
   CASE ( cFileName LIKE ".+\.exe" ); RETURN "application/exe"
   CASE ( cFileName LIKE ".+\.fif" ); RETURN "image/fif"
   CASE ( cFileName LIKE ".+\.fpx" ); RETURN "image/x-fpx"
   CASE ( cFileName LIKE ".+\.frl" ); RETURN "application/freeloader"
   CASE ( cFileName LIKE ".+\.gif" ); RETURN "image/gif"
   CASE ( cFileName LIKE ".+\.gtar" ); RETURN "application/x-gtar"
   CASE ( cFileName LIKE ".+\.gz" ); RETURN "application/gzip"
   CASE ( cFileName LIKE ".+\.hdf" ); RETURN "application/x-hdf"
   CASE ( cFileName LIKE ".+\.hdml" ); RETURN "text/x-hdml"
   CASE ( cFileName LIKE ".+\.hqx" ); RETURN "application/mac-binhex40"
   CASE ( cFileName LIKE ".+\.ica" ); RETURN "application/x-ica"
   CASE ( cFileName LIKE ".+\.ief" ); RETURN "image/ief"
   CASE ( cFileName LIKE ".+\.ins" ); RETURN "application/x-NET-Install"
   CASE ( cFileName LIKE ".+\.ips" ); RETURN "application/x-ipscript"
   CASE ( cFileName LIKE ".+\.ipx" ); RETURN "application/x-ipix"
   CASE ( cFileName LIKE ".+\.ivr" ); RETURN "i-world/i-vrml"
   CASE ( cFileName LIKE ".+\.js" ); RETURN "application/x-javascript"
   CASE ( cFileName LIKE ".+\.latex" ); RETURN "application/x-latex"
   CASE ( cFileName LIKE ".+\.m3u" ); RETURN "audio/x-mpegurl"
   CASE ( cFileName LIKE ".+\.man" ); RETURN "application/x-troff-man"
   CASE ( cFileName LIKE ".+\.map" ); RETURN "application/x-httpd-imap"
   CASE ( cFileName LIKE ".+\.mbd" ); RETURN "application/mbedlet"
   CASE ( cFileName LIKE ".+\.mcf" ); RETURN "image/vasa"
   CASE ( cFileName LIKE ".+\.me" ); RETURN "application/x-troff-me"
   CASE ( cFileName LIKE ".+\.mfp" ); RETURN "application/mirage"
   CASE ( cFileName LIKE ".+\.mif" ); RETURN "application/x-mif"
   CASE ( cFileName LIKE ".+\.mol" ); RETURN "chemical/x-mdl-molfile"
   CASE ( cFileName LIKE ".+\.movie" ); RETURN "video/x-sgi-movie"
   CASE ( cFileName LIKE ".+\.mp2" ); RETURN "audio/x-mpeg"
   CASE ( cFileName LIKE ".+\.mp3" ); RETURN "audio/x-mpeg"
   CASE ( cFileName LIKE ".+\.ms" ); RETURN "application/x-troff-ms"
   CASE ( cFileName LIKE ".+\.n2p" ); RETURN "application/n2p"
   CASE ( cFileName LIKE ".+\.npx" ); RETURN "application/x-netfpx"
   CASE ( cFileName LIKE ".+\.nsc" ); RETURN "application/x-nschat"
   CASE ( cFileName LIKE ".+\.oda" ); RETURN "application/oda"
   CASE ( cFileName LIKE ".+\.pbm" ); RETURN "image/x-portable-bitmap"
   CASE ( cFileName LIKE ".+\.pdb" ); RETURN "chemical/x-pdb"
   CASE ( cFileName LIKE ".+\.pdf" ); RETURN "application/pdf"
   CASE ( cFileName LIKE ".+\.pfr" ); RETURN "application/font-tdpfr"
   CASE ( cFileName LIKE ".+\.pgm" ); RETURN "image/x-portable-graymap"
   CASE ( cFileName LIKE ".+\.pgp" ); RETURN "application/x-pgp-plugin"
   CASE ( cFileName LIKE ".+\.pgr" ); RETURN "text/parsnegar-document"
   CASE ( cFileName LIKE ".+\.php3" ); RETURN "application/x-httpd-php3"
   CASE ( cFileName LIKE ".+\.phtml" ); RETURN "application/x-httpd-php"
   CASE ( cFileName LIKE ".+\.pnm" ); RETURN "image/x-portable-anymap"
   CASE ( cFileName LIKE ".+\.ppm" ); RETURN "image/x-portable-pixmap"
   CASE ( cFileName LIKE ".+\.pqf" ); RETURN "application/x-cprplayer"
   CASE ( cFileName LIKE ".+\.pqi" ); RETURN "application/cprplayer"
   CASE ( cFileName LIKE ".+\.ptlk" ); RETURN "application/listenup"
   CASE ( cFileName LIKE ".+\.push" ); RETURN "multipart/x-mixed-replace"
   CASE ( cFileName LIKE ".+\.qrt" ); RETURN "application/quest"
   CASE ( cFileName LIKE ".+\.ras" ); RETURN "image/x-cmu-raster"
   CASE ( cFileName LIKE ".+\.rgb" ); RETURN "image/x-rgb"
   CASE ( cFileName LIKE ".+\.rip" ); RETURN "image/rip"
   CASE ( cFileName LIKE ".+\.rmf" ); RETURN "audio/x-rmf"
   CASE ( cFileName LIKE ".+\.rpm" ); RETURN "audio/x-pn-realaudio-plugin"
   CASE ( cFileName LIKE ".+\.rrf" ); RETURN "application/x-InstallFromTheWeb"
   CASE ( cFileName LIKE ".+\.rtc" ); RETURN "application/rtc"
   CASE ( cFileName LIKE ".+\.rtf" ); RETURN "application/rtf"
   CASE ( cFileName LIKE ".+\.rtx" ); RETURN "text/richtext"
   CASE ( cFileName LIKE ".+\.sca" ); RETURN "application/x-supercard"
   CASE ( cFileName LIKE ".+\.sh" ); RETURN "application/x-sh"
   CASE ( cFileName LIKE ".+\.shar" ); RETURN "application/x-shar"
   CASE ( cFileName LIKE ".+\.shw" ); RETURN "application/presentations"
   CASE ( cFileName LIKE ".+\.sit" ); RETURN "application/x-stuffit"
   CASE ( cFileName LIKE ".+\.smp" ); RETURN "application/studiom"
   CASE ( cFileName LIKE ".+\.spl" ); RETURN "application/futuresplash"
   CASE ( cFileName LIKE ".+\.spr" ); RETURN "application/x-sprite"
   CASE ( cFileName LIKE ".+\.sprite" ); RETURN "application/x-sprite"
   CASE ( cFileName LIKE ".+\.src" ); RETURN "application/x-wais-source"
   CASE ( cFileName LIKE ".+\.stk" ); RETURN "application/hstu"
   CASE ( cFileName LIKE ".+\.stream" ); RETURN "audio/x-qt-stream"
   CASE ( cFileName LIKE ".+\.sv4cpio" ); RETURN "application/x-sv4cpio"
   CASE ( cFileName LIKE ".+\.sv4crc" ); RETURN "application/x-sv4crc"
   CASE ( cFileName LIKE ".+\.svh" ); RETURN "image/svh"
   CASE ( cFileName LIKE ".+\.svr" ); RETURN "x-world/x-svr"
   CASE ( cFileName LIKE ".+\.swf" ); RETURN "application/x-shockwave-flash"
   CASE ( cFileName LIKE ".+\.talk" ); RETURN "application/talker"
   CASE ( cFileName LIKE ".+\.tar" ); RETURN "application/x-tar"
   CASE ( cFileName LIKE ".+\.tbk" ); RETURN "application/toolbook"
   CASE ( cFileName LIKE ".+\.tcl" ); RETURN "application/x-tcl"
   CASE ( cFileName LIKE ".+\.tex" ); RETURN "application/x-tex"
   CASE ( cFileName LIKE ".+\.tlk" ); RETURN "application/x-tlk"
   CASE ( cFileName LIKE ".+\.tmv" ); RETURN "application/x-Parable-Thing"
   CASE ( cFileName LIKE ".+\.tsi" ); RETURN "audio/tsplayer"
   CASE ( cFileName LIKE ".+\.tsp" ); RETURN "application/dsptype"
   CASE ( cFileName LIKE ".+\.tsv" ); RETURN "text/tab-separated-values"
   CASE ( cFileName LIKE ".+\.txt" ); RETURN "text/plain"
   CASE ( cFileName LIKE ".+\.ustar" ); RETURN "application/x-ustar"
   CASE ( cFileName LIKE ".+\.vbd" ); RETURN "application/activexdocument"
   CASE ( cFileName LIKE ".+\.vcd" ); RETURN "application/x-cdlink"
   CASE ( cFileName LIKE ".+\.vgp" ); RETURN "video/x-videogram-plugin"
   CASE ( cFileName LIKE ".+\.vmd" ); RETURN "application/vocaltec-media-desc"
   CASE ( cFileName LIKE ".+\.vmf" ); RETURN "application/vocaltec-media-file"
   CASE ( cFileName LIKE ".+\.vox" ); RETURN "audio/voxware"
   CASE ( cFileName LIKE ".+\.vqe" ); RETURN "audio/x-twinvq-plugin"
   CASE ( cFileName LIKE ".+\.vrt" ); RETURN "x-world/x-vrt"
   CASE ( cFileName LIKE ".+\.vts" ); RETURN "workbook/formulaone"
   CASE ( cFileName LIKE ".+\.vtts" ); RETURN "workbook/formulaone"
   CASE ( cFileName LIKE ".+\.wav" ); RETURN "audio/x-wav"
   CASE ( cFileName LIKE ".+\.wi" ); RETURN "image/wavelet"
   CASE ( cFileName LIKE ".+\.wid" ); RETURN "application/x-DemoShield"
   CASE ( cFileName LIKE ".+\.wis" ); RETURN "application/x-InstallShield"
   CASE ( cFileName LIKE ".+\.wlt" ); RETURN "application/x-mswallet"
   CASE ( cFileName LIKE ".+\.wri" ); RETURN "application/write"
   CASE ( cFileName LIKE ".+\.wtx" ); RETURN "audio/wtx"
   CASE ( cFileName LIKE ".+\.wtx" ); RETURN "audio/x-wtx"
   CASE ( cFileName LIKE ".+\.xbm" ); RETURN "image/x-xbitmap"
   CASE ( cFileName LIKE ".+\.xls" ); RETURN "application/xls"
   CASE ( cFileName LIKE ".+\.xlt" ); RETURN "application/xlt"
   CASE ( cFileName LIKE ".+\.xml" ); RETURN "text/xml"
   CASE ( cFileName LIKE ".+\.xpm" ); RETURN "image/x-xpixmap"
   CASE ( cFileName LIKE ".+\.xwd" ); RETURN "image/x-xwindowdump"
   CASE ( cFileName LIKE ".+\.z" ); RETURN "application/x-compress"
   CASE ( cFileName LIKE ".+\.zip" ); RETURN "application/zip"
   CASE ( cFileName LIKE ".+\.zpa" ); RETURN "application/pcphoto"
   ENDCASE

   RETURN "text/plain"
