/*
 * Harbour Project source code:
 * hb_SendMail() (This version of hb_SendMail() started from Luiz's original work on SendMail())
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

#translate ( <exp1> LIKE <exp2> )   => ( hb_regexLike( (<exp2>), (<exp1>) ) )

FUNCTION hb_SendMail( cServer, nPort, cFrom, xTo, xCC, xBCC, cBody, cSubject, ;
      aFiles, cUser, cPass, cPopServer, nPriority, lRead, ;
      xTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo, ;
      lTLS, cSMTPPass, cCharset, cEncoding, cClientHost )
   /*
   cServer    -> Required. IP or domain name of the mail server
   nPort      -> Optional. Port used my email server
   cFrom      -> Required. Email address of the sender
   xTo        -> Required. Character string or array of email addresses to send the email to
   xCC        -> Optional. Character string or array of email adresses for CC (Carbon Copy)
   xBCC       -> Optional. Character string or array of email adresses for BCC (Blind Carbon Copy)
   cBody      -> Optional. The body message of the email as text, or the filename of the HTML message to send.
   cSubject   -> Optional. Subject of the sending email
   aFiles     -> Optional. Array of attachments to the email to send
   cUser      -> Required. User name for the POP3 server
   cPass      -> Required. Password for cUser
   cPopServer -> Required. POP3 server name or address
   nPriority  -> Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low
   lRead      -> Optional. If set to .T., a confirmation request is send. Standard setting is .F.
   xTrace     -> Optional. If set to .T., a log file is created (smtp-<n>.log). Standard setting is .F.
                           If a block is passed, it will be called for each log event with the message a string, no param on session close.
   lPopAuth   -> Optional. Do POP3 authentication before sending mail.
   lNoAuth    -> Optional. Disable Autentication methods
   nTimeOut   -> Optional. Number os ms to wait default 10000 (10s)
   cReplyTo   -> Optional.
   cClientHost-> Optional. Domain name of the SMTP client in the format smtp.example.com OR client IP surrounded by brackets as in [200.100.100.5]
                           Note: This parameter is optional for backwards compatibility, but should be provided to comply with RFC 2812.
   */

   LOCAL cTmp
   LOCAL cTo
   LOCAL cCC
   LOCAL cBCC
   LOCAL tmp

   LOCAL oInMail
   LOCAL oUrl
   LOCAL oUrl1

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

   oInMail:oUrl:cUserid := tip_GetRawEmail( cFrom )

   oInMail:Write( hb_MailAssemble( cFrom, xTo, xCC, cBody, cSubject, aFiles, nPriority, lRead, cReplyTo, cCharset, cEncoding ) )
   oInMail:Commit()
   oInMail:Close()

   RETURN lReturn

FUNCTION hb_MailAssemble( cFrom, xTo, xCC, cBody, cSubject, ;
      aFiles, nPriority, lRead, ;
      cReplyTo, ;
      cCharset, cEncoding )
   /*
   cFrom      -> Required. Email address of the sender
   xTo        -> Required. Character string or array of email addresses to send the email to
   xCC        -> Optional. Character string or array of email adresses for CC (Carbon Copy)
   cBody      -> Optional. The body message of the email as text, or the filename of the HTML message to send.
   cSubject   -> Optional. Subject of the sending email
   aFiles     -> Optional. Array of attachments to the email to send
   nPriority  -> Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low
   lRead      -> Optional. If set to .T., a confirmation request is send. Standard setting is .F.
   cReplyTo   -> Optional.
   */

   LOCAL cBodyTemp
   LOCAL oMail
   LOCAL oAttach
   LOCAL aThisFile
   LOCAL cMimeText
   LOCAL cFile
   LOCAL cFname
   LOCAL cFext
   LOCAL cData

   hb_default( @aFiles, {} )
   hb_default( @nPriority, 3 )
   hb_default( @lRead, .F. )
   hb_default( @cReplyTo, "" )
   hb_default( @cCharset, "ISO-8859-1" )
   hb_default( @cEncoding, "quoted-printable" )

   IF !( ( ".htm" $ Lower( cBody ) .OR. ".html" $ Lower( cBody ) ) .AND. hb_FileExists( cBody ) )
      IF !( Right( cBody, 2 ) == Chr( 13 ) + Chr( 10 ) )
         cBody += Chr( 13 ) + Chr( 10 )
      ENDIF
   ENDIF

   oMail := TIPMail():new()
   oMail:SetEncoder( cEncoding )
   oMail:SetCharset( cCharset )
   oMail:SetHeader( cSubject, cFrom, xTo, xCC )
   oMail:hHeaders[ "Date" ] := tip_TimeStamp()
   IF ! Empty( cReplyTo )
      oMail:hHeaders[ "Reply-to" ] := cReplyTo
   ENDIF

   IF ! Empty( aFiles )
      oAttach := TIPMail():new()
      oAttach:SetEncoder( cEncoding )
      oAttach:SetCharset( cCharset )

      IF ( ".htm" $ Lower( cBody ) .OR. ".html" $ Lower( cBody ) ) .AND. hb_FileExists( cBody )
         cMimeText := "text/html; charset=" + cCharset
         oAttach:hHeaders[ "Content-Type" ] := cMimeText
         cBodyTemp := cBody
         cBody     := MemoRead( cBodyTemp ) + Chr( 13 ) + Chr( 10 )
      ELSE
         oAttach:hHeaders[ "Content-Type" ] := "text/plain; charset=" + cCharset
      ENDIF
      oAttach:SetBody( cBody )
      oMail:Attach( oAttach )
   ELSE
      IF ( ".htm" $ Lower( cBody ) .OR. ".html" $ Lower( cBody ) ) .AND. hb_FileExists( cBody )
         cMimeText := "text/html ; charset=" + cCharset
         oMail:hHeaders[ "Content-Type" ] := cMimeText
         cBodyTemp := cBody
         cBody     := MemoRead( cBodyTemp ) + Chr( 13 ) + Chr( 10 )
      ELSE
         oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=" + cCharset
      ENDIF
      oMail:SetBody( cBody )
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

      oAttach := TIPMail():New()
      oAttach:SetCharset( cCharset )

      hb_FNameSplit( cFile,, @cFname, @cFext )
      cFile := Lower( cFile )

      IF ( cFile LIKE ".+\.(vbd|asn|asz|asd|pqi|tsp|exe|sml|ofml)" ) .OR. ;
         ( cFile LIKE ".+\.(pfr|frl|spl|gz||stk|ips|ptlk|hqx|mbd)" ) .OR. ;
         ( cFile LIKE ".+\.(mfp|pot|pps|ppt|ppz|doc|n2p|bin|class)" ) .OR. ;
         ( cFile LIKE ".+\.(lha|lzh|lzx|dbf|cdx|dbt|fpt|ntx|oda)" ) .OR. ;
         ( cFile LIKE ".+\.(axs|zpa|pdf|ai|eps|ps|shw|qrt|rtc|rtf)" ) .OR. ;
         ( cFile LIKE ".+\.(smp|dst|talk|tbk|vmd|vmf|wri|wid|rrf)" ) .OR. ;
         ( cFile LIKE ".+\.(wis|ins|tmv|arj|asp|aabaam|aas|bcpio)" ) .OR. ;
         ( cFile LIKE ".+\.(vcd|chat|cnc|coda|page|z|con|cpio|pqf)" ) .OR. ;
         ( cFile LIKE ".+\.(csh|cu|csm|dcr|dir|dxr|swa|dvi|evy|ebk)" ) .OR. ;
         ( cFile LIKE ".+\.(gtar|hdf|map|phtml|php3|ica|ipx|ips|js)" ) .OR. ;
         ( cFile LIKE ".+\.(latex|bin|mif|mpl|mpire|adr|wlt|nc|cdf)" ) .OR. ;
         ( cFile LIKE ".+\.(npx|nsc|pgp|css|sh||shar|swf|spr|sprite)" ) .OR. ;
         ( cFile LIKE ".+\.(sit|sca|sv4cpio|sv4crc|tar|tcl|tex)" ) .OR. ;
         ( cFile LIKE ".+\.(texinfo|texi|tlk|t|tr|roff|man|mems)" ) .OR. ;
         ( cFile LIKE ".+\.(alt|che|ustar|src|xls|xlt|zip|au|snd)" ) .OR. ;
         ( cFile LIKE ".+\.(es|gsm|gsd|rmf|tsi|vox|wtx|aif|aiff)" ) .OR. ;
         ( cFile LIKE ".+\.(aifc|cht|dus|mid|midi|mp3|mp2|m3u|ram)" ) .OR. ;
         ( cFile LIKE ".+\.(ra|rpm|stream|rmf|vqf|vql|vqe|wav|wtx)" ) .OR. ;
         ( cFile LIKE ".+\.(mol|pdb|dwf|ivr|cod|cpi|fif|gif|ief)" ) .OR. ;
         ( cFile LIKE ".+\.(jpeg|jpg|jpe|rip|svh|tiff|tif|mcf|svf)" ) .OR. ;
         ( cFile LIKE ".+\.(dwg|dxf|wi|ras|etf|fpx|fh5|fh4|fhc|dsf)" ) .OR. ;
         ( cFile LIKE ".+\.(pnm|pbm|pgm|ppm|rgb|xbm|xpm|xwd|dig)" ) .OR. ;
         ( cFile LIKE ".+\.(push|wan|waf||afl|mpeg|mpg|mpe|qt|mov)" ) .OR. ;
         ( cFile LIKE ".+\.(viv|vivo|asf|asx|avi|movie|vgm|vgx)" ) .OR. ;
         ( cFile LIKE ".+\.(xdr|vgp|vts|vtts|3dmf|3dm|qd3d|qd3)" ) .OR. ;
         ( cFile LIKE ".+\.(svr|wrl|wrz|vrt|xml)" ) .OR. Empty( cFExt )
         oAttach:SetEncoder( "base64" )
      ELSE
         oAttach:SetEncoder( cEncoding )
      ENDIF

      cMimeText := hb_SetMimeType( cFile, cFname, cFext )
      // Some EMAIL readers use Content-Type to check for filename

      IF ".html" $ Lower( cFext ) .OR. ".htm" $ Lower( cFext )
         cMimeText += "; charset=" + cCharset
      ENDIF

      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      // But usually, original filename is set here
      oAttach:hHeaders[ "Content-Disposition" ] := "attachment; filename=" + '"' + cFname + cFext + '"'
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

FUNCTION hb_SetMimeType( cFile, cFname, cFext )

   cFile := Lower( cFile )

   DO CASE
   CASE ( cFile LIKE ".+\.vbd" ); RETURN "application/activexdocument; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(asn|asz|asd)" ); RETURN "application/astound; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pqi" ); RETURN "application/cprplayer; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tsp" ); RETURN "application/dsptype; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.exe" ); RETURN "application/exe; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(sml|ofml)" ); RETURN "application/fml; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pfr" ); RETURN "application/font-tdpfr; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.frl" ); RETURN "application/freeloader; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.spl" ); RETURN "application/futuresplash; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.gz" ); RETURN "application/gzip; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.stk" ); RETURN "application/hstu; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ips" ); RETURN "application/ips; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ptlk" ); RETURN "application/listenup; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.hqx" ); RETURN "application/mac-binhex40; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mbd" ); RETURN "application/mbedlet; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mfp" ); RETURN "application/mirage; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(pot|pps|ppt|ppz)" ); RETURN "application/mspowerpoint; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.doc" ); RETURN "application/msword; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.n2p" ); RETURN "application/n2p; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(bin|class|lha|lzh|lzx|dbf)" ); RETURN "application/octet-stream; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.oda" ); RETURN "application/oda; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.axs" ); RETURN "application/olescript; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.zpa" ); RETURN "application/pcphoto; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pdf" ); RETURN "application/pdf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(ai|eps|ps)" ); RETURN "application/postscript; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.shw" ); RETURN "application/presentations; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.qrt" ); RETURN "application/quest; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rtc" ); RETURN "application/rtc; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rtf" ); RETURN "application/rtf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.smp" ); RETURN "application/studiom; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dst" ); RETURN "application/tajima; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.talk" ); RETURN "application/talker; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tbk" ); RETURN "application/toolbook; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vmd" ); RETURN "application/vocaltec-media-desc; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vmf" ); RETURN "application/vocaltec-media-file; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wri" ); RETURN "application/write; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wid" ); RETURN "application/x-DemoShield; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rrf" ); RETURN "application/x-InstallFromTheWeb; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wis" ); RETURN "application/x-InstallShield; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ins" ); RETURN "application/x-NET-Install; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tmv" ); RETURN "application/x-Parable-Thing; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.arj" ); RETURN "application/x-arj; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.asp" ); RETURN "application/x-asap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.aab" ); RETURN "application/x-authorware-bin; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(aam|aas)" ); RETURN "application/x-authorware-map; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.bcpio" ); RETURN "application/x-bcpio; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vcd" ); RETURN "application/x-cdlink; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.chat" ); RETURN "application/x-chat; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cnc" ); RETURN "application/x-cnc; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(coda|page)" ); RETURN "application/x-coda; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.z" ); RETURN "application/x-compress; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.con" ); RETURN "application/x-connector; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cpio" ); RETURN "application/x-cpio; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pqf" ); RETURN "application/x-cprplayer; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.csh" ); RETURN "application/x-csh; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(cu|csm)" ); RETURN "application/x-cu-seeme; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(dcr|dir|dxr|swa)" ); RETURN "application/x-director; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dvi" ); RETURN "application/x-dvi; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.evy" ); RETURN "application/x-envoy; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ebk" ); RETURN "application/x-expandedbook; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.gtar" ); RETURN "application/x-gtar; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.hdf" ); RETURN "application/x-hdf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.map" ); RETURN "application/x-httpd-imap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.phtml" ); RETURN "application/x-httpd-php; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.php3" ); RETURN "application/x-httpd-php3; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ica" ); RETURN "application/x-ica; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ipx" ); RETURN "application/x-ipix; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ips" ); RETURN "application/x-ipscript; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.js" ); RETURN "application/x-javascript; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.latex" ); RETURN "application/x-latex; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.bin" ); RETURN "application/x-macbinary; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mif" ); RETURN "application/x-mif; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(mpl|mpire)" ); RETURN "application/x-mpire; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.adr" ); RETURN "application/x-msaddr; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wlt" ); RETURN "application/x-mswallet; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(nc|cdf)" ); RETURN "application/x-netcdf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.npx" ); RETURN "application/x-netfpx; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.nsc" ); RETURN "application/x-nschat; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pgp" ); RETURN "application/x-pgp-plugin; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.css" ); RETURN "application/x-pointplus; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sh" ); RETURN "application/x-sh; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.shar" ); RETURN "application/x-shar; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.swf" ); RETURN "application/x-shockwave-flash; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.spr" ); RETURN "application/x-sprite; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sprite" ); RETURN "application/x-sprite; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sit" ); RETURN "application/x-stuffit; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sca" ); RETURN "application/x-supercard; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sv4cpio" ); RETURN "application/x-sv4cpio; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sv4crc" ); RETURN "application/x-sv4crc; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tar" ); RETURN "application/x-tar; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tcl" ); RETURN "application/x-tcl; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tex" ); RETURN "application/x-tex; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(texinfo|texi)" ); RETURN "application/x-texinfo; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tlk" ); RETURN "application/x-tlk; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(t|tr|roff)" ); RETURN "application/x-troff; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.man" ); RETURN "application/x-troff-man; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.me" ); RETURN "application/x-troff-me; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ms" ); RETURN "application/x-troff-ms; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.alt" ); RETURN "application/x-up-alert; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.che" ); RETURN "application/x-up-cacheop; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ustar" ); RETURN "application/x-ustar; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.src" ); RETURN "application/x-wais-source; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xls" ); RETURN "application/xls; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xlt" ); RETURN "application/xlt; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.zip" ); RETURN "application/zip; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(au|snd)" ); RETURN "audio/basic; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.es" ); RETURN "audio/echospeech; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(gsm|gsd)" ); RETURN "audio/gsm; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rmf" ); RETURN "audio/rmf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tsi" ); RETURN "audio/tsplayer; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vox" ); RETURN "audio/voxware; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wtx" ); RETURN "audio/wtx; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(aif|aiff|aifc)" ); RETURN "audio/x-aiff; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(cht|dus)" ); RETURN "audio/x-dspeech; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(mid|midi)" ); RETURN "audio/x-midi; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mp3" ); RETURN "audio/x-mpeg; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mp2" ); RETURN "audio/x-mpeg; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.m3u" ); RETURN "audio/x-mpegurl; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(ram|ra)" ); RETURN "audio/x-pn-realaudio; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rpm" ); RETURN "audio/x-pn-realaudio-plugin; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.stream" ); RETURN "audio/x-qt-stream; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rmf" ); RETURN "audio/x-rmf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(vqf|vql)" ); RETURN "audio/x-twinvq; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vqe" ); RETURN "audio/x-twinvq-plugin; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wav" ); RETURN "audio/x-wav; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wtx" ); RETURN "audio/x-wtx; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mol" ); RETURN "chemical/x-mdl-molfile; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pdb" ); RETURN "chemical/x-pdb; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dwf" ); RETURN "drawing/x-dwf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ivr" ); RETURN "i-world/i-vrml; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cod" ); RETURN "image/cis-cod; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cpi" ); RETURN "image/cpi; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.fif" ); RETURN "image/fif; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.gif" ); RETURN "image/gif; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ief" ); RETURN "image/ief; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(jpeg|jpg|jpe)" ); RETURN "image/jpeg; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rip" ); RETURN "image/rip; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.svh" ); RETURN "image/svh; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(tiff|tif)" ); RETURN "image/tiff; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mcf" ); RETURN "image/vasa; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(svf|dwg|dxf)" ); RETURN "image/vnd; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wi" ); RETURN "image/wavelet; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ras" ); RETURN "image/x-cmu-raster; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.etf" ); RETURN "image/x-etf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.fpx" ); RETURN "image/x-fpx; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(fh5|fh4|fhc)" ); RETURN "image/x-freehand; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dsf" ); RETURN "image/x-mgx-dsf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pnm" ); RETURN "image/x-portable-anymap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pbm" ); RETURN "image/x-portable-bitmap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pgm" ); RETURN "image/x-portable-graymap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ppm" ); RETURN "image/x-portable-pixmap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rgb" ); RETURN "image/x-rgb; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xbm" ); RETURN "image/x-xbitmap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xpm" ); RETURN "image/x-xpixmap; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xwd" ); RETURN "image/x-xwindowdump; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dig" ); RETURN "multipart/mixed; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.push" ); RETURN "multipart/x-mixed-replace; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(wan|waf)" ); RETURN "plugin/wanimate; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ccs" ); RETURN "text/ccs; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(htm|html)" ); RETURN "text/html; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pgr" ); RETURN "text/parsnegar-document; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xml" ); RETURN "text/xml; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.txt" ); RETURN "text/plain; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rtx" ); RETURN "text/richtext; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tsv" ); RETURN "text/tab-separated-values; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.hdml" ); RETURN "text/x-hdml; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.etx" ); RETURN "text/x-setext; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(talk|spc)" ); RETURN "text/x-speech; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.afl" ); RETURN "video/animaflex; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(mpeg|mpg|mpe)" ); RETURN "video/mpeg; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(qt|mov)" ); RETURN "video/quicktime; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(viv|vivo)" ); RETURN "video/vnd.vivo; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(asf|asx)" ); RETURN "video/x-ms-asf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.avi" ); RETURN "video/x-msvideo; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.movie" ); RETURN "video/x-sgi-movie; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(vgm|vgx|xdr)" ); RETURN "video/x-videogram; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vgp" ); RETURN "video/x-videogram-plugin; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vts" ); RETURN "workbook/formulaone; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vtts" ); RETURN "workbook/formulaone; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(3dmf|3dm|qd3d|qd3)" ); RETURN "x-world/x-3dmf; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.svr" ); RETURN "x-world/x-svr; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(wrl|wrz)" ); RETURN "x-world/x-vrml; name=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vrt" ); RETURN "x-world/x-vrt; name=" + cFname + cFext
   ENDCASE

   RETURN "text/plain; name=" + cFname + cFext
