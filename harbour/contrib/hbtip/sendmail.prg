/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * hb_SendMail() (This version of hb_SendMail() started from Luiz's original work on SendMail())
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes & Patrick Mast
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu) (SSL support)
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

#include "common.ch"

#translate ( <exp1> LIKE <exp2> )   => ( hb_regexLike( (<exp2>), (<exp1>) ) )

FUNCTION hb_SendMail( cServer, nPort, cFrom, xTo, xCC, xBCC, cBody, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, bTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo, lTLS, cSMTPPass, cCharset, cEncoding )
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
   bTrace     -> Optional. If set to .T., a log file is created (smtp-<nNr>.log). Standard setting is NIL.
                           If a block is passed, it will be called for each log event with the message a string, no param on session close.
   lPopAuth   -> Optional. Do POP3 authentication before sending mail.
   lNoAuth    -> Optional. Disable Autentication methods
   nTimeOut   -> Optional. Number os ms to wait default 20000 (20s)
   cReplyTo   -> Optional.
   */

   LOCAL oInMail
   LOCAL cBodyTemp
   LOCAL oUrl
   LOCAL oMail
   LOCAL oAttach
   LOCAL aThisFile
   LOCAL cMimeText
   LOCAL cFile
   LOCAL cFname
   LOCAL cFext
   LOCAL cData
   LOCAL oUrl1

   LOCAL cTmp          := ""
   LOCAL cTo           := ""
   LOCAL cCC           := ""
   LOCAL cBCC          := ""

   LOCAL lConnectPlain := .F.
   LOCAL lReturn       := .T.
   LOCAL lAuthLogin    := .F.
   LOCAL lAuthPlain    := .F.
   LOCAL lAuthTLS      := .F.
   LOCAL lConnect      := .T.
   LOCAL oPop

   LOCAL cFromRaw := tip_GetRawEMail( cFrom )

   IF ! ISCHARACTER( cServer ) .OR. Empty( cServer )
      cServer := "localhost"
   ENDIF
   IF ! ISCHARACTER( cUser )
      cUser := ""
   ENDIF
   IF ! ISCHARACTER( cPass )
      cPass := ""
   ENDIF
   IF ! ISNUMBER( nPort ) .OR. Empty( nPort )
      nPort := 25
   ENDIF
   IF ! ISARRAY( aFiles )
      aFiles := {}
   ENDIF
   IF ! ISNUMBER( nPriority )
      nPriority := 3
   ENDIF
   IF ! ISLOGICAL( lRead )
      lRead := .F.
   ENDIF
   IF ! ISLOGICAL( lPopAuth )
      lPopAuth := .T.
   ENDIF
   IF ! ISLOGICAL( lNoAuth )
      lNoAuth := .F.
   ENDIF
   IF ! ISNUMBER( nTimeOut )
      nTimeOut := 10000
   ENDIF
   IF ! ISCHARACTER( cReplyTo )
      cReplyTo := ""
   ENDIF
   IF ! ISLOGICAL( lTLS )
      lTLS := .F.
   ENDIF
   IF ! ISCHARACTER( cSMTPPass )
      cSMTPPass := cPass
   ENDIF
   IF ! ISCHARACTER( cCharset )
      cCharset := "ISO-8859-1"
   ENDIF
   IF ! ISCHARACTER( cEncoding )
      cEncoding := "quoted-printable"
   ENDIF

   cUser := StrTran( cUser, "@", "&at;" )

   IF !( ( ".htm" $ Lower( cBody ) .OR. ".html" $ Lower( cBody ) ) .AND. hb_FileExists( cBody ) )

      IF !( Right( cBody, 2 ) == Chr( 13 ) + Chr( 10 ) )
         cBody += Chr( 13 ) + Chr( 10 )
      ENDIF

   ENDIF

   // cTo
   IF ISARRAY( xTo )
      IF Len( xTo ) > 1
         FOR EACH cTo IN xTo
            IF cTo:__enumIndex() != 1
               IF ! Empty( cTo )
                  cTmp += tip_GetRawEMail( AllTrim( cTo ) ) + ","
               ENDIF
            ENDIF
         NEXT
         cTmp := SubStr( cTmp, 1, Len( cTmp ) - 1 )
      ENDIF
      cTo := tip_GetRawEMail( AllTrim( xTo[ 1 ] ) )
      IF Len( cTmp ) > 0
         cTo += "," + cTmp
      ENDIF
   ELSEIF ISCHARACTER( xTo )
      cTo := tip_GetRawEMail( AllTrim( xTo ) )
   ENDIF


   // CC (Carbon Copy)
   IF ISARRAY( xCC )
      IF Len( xCC ) > 0
         FOR EACH cTmp IN xCC
            IF ! Empty( cTmp )
               cCC += tip_GetRawEMail( AllTrim( cTmp ) ) + ","
            ENDIF
         NEXT
         cCC := SubStr( cCC, 1, Len( cCC ) - 1 )
      ENDIF
   ELSEIF ISCHARACTER( xCC )
      cCC := tip_GetRawEMail( AllTrim( xCC ) )
   ENDIF


   // BCC (Blind Carbon Copy)
   IF ISARRAY( xBCC )
      IF Len( xBCC ) > 0
         FOR EACH cTmp IN xBCC
            IF ! Empty( cTmp )
               cBCC += tip_GetRawEMail( AllTrim( cTmp ) ) + ","
            ENDIF
         NEXT
         cBCC := SubStr( cBCC, 1, Len( cBCC ) - 1 )
      ENDIF
   ELSEIF ISCHARACTER( xBCC )
      cBCC := tip_GetRawEMail( AllTrim( xBCC ) )
   ENDIF

   IF cPopServer != NIL .AND. lPopAuth
      BEGIN SEQUENCE
         oUrl1 := tUrl():New( iif( lTLS, "pop3s://" , "pop://" ) + cUser + ":" + cPass + "@" + cPopServer + "/" )
         oUrl1:cUserid := StrTran( cUser, "&at;", "@" )
         opop:= tIPClientPOP():New( oUrl1, bTrace )
         IF oPop:Open()
            oPop:Close()
         ENDIF
      RECOVER
         lReturn := .F.
      END SEQUENCE
   ENDIF

   IF ! lReturn
      RETURN .F.
   ENDIF

   BEGIN SEQUENCE
      oUrl := tUrl():New( iif( lTLS, "smtps://", "smtp://" ) + cUser + iif( Empty( cSMTPPass ), "", ":" + cSMTPPass ) + "@" + cServer )
   RECOVER
      lReturn := .F.
   END SEQUENCE

   IF ! lReturn
      RETURN .F.
   ENDIF

   oUrl:nPort   := nPort
   oUrl:cUserid := StrTran( cUser, "&at;", "@" )

   oMail := tipMail():new()
   oMail:SetEncoder( cEncoding )
   oMail:SetCharset( cCharset )
   oMail:SetHeader( cSubject, cFrom, xTo, xCC, xBCC )
   oMail:hHeaders[ "Date" ] := tip_Timestamp()
   IF ! Empty( cReplyTo )
      oMail:hHeaders[ "Reply-to" ] := cReplyTo
   ENDIF

   IF ! Empty( aFiles )
      oAttach := tipMail():new()
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

   oUrl:cFile := cTo + iif( Empty( cCC ), "", "," + cCC ) + iif( Empty( cBCC ), "", "," + cBCC )

   BEGIN SEQUENCE
      oInmail := tIPClientSMTP():New( oUrl, bTrace )
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
#if defined( HB_HAS_OPENSSL )
            ELSEIF "STARTTLS" $ oInMail:cReply
               lAuthTLS := .T.
#endif
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
         oInmail := tIPClientSMTP():New( oUrl, bTrace )
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

   oInMail:oUrl:cUserid := cFromRaw

   FOR EACH aThisFile IN aFiles

      IF ISCHARACTER( aThisFile )
         cFile := aThisFile
         cData := hb_MemoRead( cFile )
      ELSEIF ISARRAY( aThisFile ) .AND. Len( aThisFile ) >= 2
         cFile := aThisFile[ 1 ]
         IF ISCHARACTER( aThisFile[ 2 ] )
            cData := aThisFile[ 2 ]
            IF ! ISCHARACTER( cFile )
               cFile := "unnamed"
            ENDIF
         ELSE
            IF ! ISCHARACTER( cFile )
               LOOP /* No filename and no content. */
            ELSE
               cData := hb_MemoRead( cFile )
            ENDIF
         ENDIF
      ELSE
         LOOP
      ENDIF

      cData += Chr( 13 ) + Chr( 10 )

      oAttach := TipMail():New()
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
         ( cFile LIKE ".+\.(svr|wrl|wrz|vrt)" ) .OR. Empty( cFExt )
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
      oAttach:hHeaders[ "Content-Disposition" ] := "attachment; filename=" + cFname + cFext
      oAttach:SetBody( cData )
      oMail:Attach( oAttach )
   NEXT

   IF lRead
      oMail:hHeaders[ "Disposition-Notification-To" ] := cFromRaw
   ENDIF

   IF nPriority != 3
      oMail:hHeaders[ "X-Priority" ] := Str( nPriority, 1 )
   ENDIF

   oInMail:Write( oMail:ToString() )
   oInMail:Commit()
   oInMail:Close()

   RETURN lReturn

//-------------------------------------------------------------//

FUNCTION hb_SetMimeType( cFile, cFname, cFext )

   cFile := Lower( cFile )

   DO CASE
   CASE ( cFile LIKE ".+\.vbd" ); RETURN "application/activexdocument=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(asn|asz|asd)" ); RETURN "application/astound=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pqi" ); RETURN "application/cprplayer=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tsp" ); RETURN "application/dsptype=" + cFname + cFext
   CASE ( cFile LIKE ".+\.exe" ); RETURN "application/exe=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(sml|ofml)" ); RETURN "application/fml=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pfr" ); RETURN "application/font-tdpfr=" + cFname + cFext
   CASE ( cFile LIKE ".+\.frl" ); RETURN "application/freeloader=" + cFname + cFext
   CASE ( cFile LIKE ".+\.spl" ); RETURN "application/futuresplash =" + cFname + cFext
   CASE ( cFile LIKE ".+\.gz" ); RETURN "application/gzip =" + cFname + cFext
   CASE ( cFile LIKE ".+\.stk" ); RETURN "application/hstu =" + cFname + cFext
   CASE ( cFile LIKE ".+\.ips" ); RETURN "application/ips=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ptlk" ); RETURN "application/listenup =" + cFname + cFext
   CASE ( cFile LIKE ".+\.hqx" ); RETURN "application/mac-binhex40 =" + cFname + cFext
   CASE ( cFile LIKE ".+\.mbd" ); RETURN "application/mbedlet=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mfp" ); RETURN "application/mirage=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(pot|pps|ppt|ppz)" ); RETURN "application/mspowerpoint =" + cFname + cFext
   CASE ( cFile LIKE ".+\.doc" ); RETURN "application/msword=" + cFname + cFext
   CASE ( cFile LIKE ".+\.n2p" ); RETURN "application/n2p=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(bin|class|lha|lzh|lzx|dbf)" ); RETURN "application/octet-stream =" + cFname + cFext
   CASE ( cFile LIKE ".+\.oda" ); RETURN "application/oda=" + cFname + cFext
   CASE ( cFile LIKE ".+\.axs" ); RETURN "application/olescript=" + cFname + cFext
   CASE ( cFile LIKE ".+\.zpa" ); RETURN "application/pcphoto=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pdf" ); RETURN "application/pdf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(ai|eps|ps)" ); RETURN "application/postscript=" + cFname + cFext
   CASE ( cFile LIKE ".+\.shw" ); RETURN "application/presentations=" + cFname + cFext
   CASE ( cFile LIKE ".+\.qrt" ); RETURN "application/quest=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rtc" ); RETURN "application/rtc=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rtf" ); RETURN "application/rtf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.smp" ); RETURN "application/studiom=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dst" ); RETURN "application/tajima=" + cFname + cFext
   CASE ( cFile LIKE ".+\.talk" ); RETURN "application/talker=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tbk" ); RETURN "application/toolbook =" + cFname + cFext
   CASE ( cFile LIKE ".+\.vmd" ); RETURN "application/vocaltec-media-desc=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vmf" ); RETURN "application/vocaltec-media-file=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wri" ); RETURN "application/write=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wid" ); RETURN "application/x-DemoShield =" + cFname + cFext
   CASE ( cFile LIKE ".+\.rrf" ); RETURN "application/x-InstallFromTheWeb=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wis" ); RETURN "application/x-InstallShield=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ins" ); RETURN "application/x-NET-Install=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tmv" ); RETURN "application/x-Parable-Thing=" + cFname + cFext
   CASE ( cFile LIKE ".+\.arj" ); RETURN "application/x-arj=" + cFname + cFext
   CASE ( cFile LIKE ".+\.asp" ); RETURN "application/x-asap=" + cFname + cFext
   CASE ( cFile LIKE ".+\.aab" ); RETURN "application/x-authorware-bin =" + cFname + cFext
   CASE ( cFile LIKE ".+\.(aam|aas)" ); RETURN "application/x-authorware-map =" + cFname + cFext
   CASE ( cFile LIKE ".+\.bcpio" ); RETURN "application/x-bcpio=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vcd" ); RETURN "application/x-cdlink =" + cFname + cFext
   CASE ( cFile LIKE ".+\.chat" ); RETURN "application/x-chat=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cnc" ); RETURN "application/x-cnc=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(coda|page)" ); RETURN "application/x-coda=" + cFname + cFext
   CASE ( cFile LIKE ".+\.z" ); RETURN "application/x-compress=" + cFname + cFext
   CASE ( cFile LIKE ".+\.con" ); RETURN "application/x-connector=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cpio" ); RETURN "application/x-cpio=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pqf" ); RETURN "application/x-cprplayer=" + cFname + cFext
   CASE ( cFile LIKE ".+\.csh" ); RETURN "application/x-csh=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(cu|csm)" ); RETURN "application/x-cu-seeme=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(dcr|dir|dxr|swa)" ); RETURN "application/x-director=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dvi" ); RETURN "application/x-dvi=" + cFname + cFext
   CASE ( cFile LIKE ".+\.evy" ); RETURN "application/x-envoy=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ebk" ); RETURN "application/x-expandedbook=" + cFname + cFext
   CASE ( cFile LIKE ".+\.gtar" ); RETURN "application/x-gtar=" + cFname + cFext
   CASE ( cFile LIKE ".+\.hdf" ); RETURN "application/x-hdf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.map" ); RETURN "application/x-httpd-imap =" + cFname + cFext
   CASE ( cFile LIKE ".+\.phtml" ); RETURN "application/x-httpd-php=" + cFname + cFext
   CASE ( cFile LIKE ".+\.php3" ); RETURN "application/x-httpd-php3 =" + cFname + cFext
   CASE ( cFile LIKE ".+\.ica" ); RETURN "application/x-ica=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ipx" ); RETURN "application/x-ipix=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ips" ); RETURN "application/x-ipscript=" + cFname + cFext
   CASE ( cFile LIKE ".+\.js" ); RETURN "application/x-javascript =" + cFname + cFext
   CASE ( cFile LIKE ".+\.latex" ); RETURN "application/x-latex=" + cFname + cFext
   CASE ( cFile LIKE ".+\.bin" ); RETURN "application/x-macbinary=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mif" ); RETURN "application/x-mif=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(mpl|mpire)" ); RETURN "application/x-mpire=" + cFname + cFext
   CASE ( cFile LIKE ".+\.adr" ); RETURN "application/x-msaddr =" + cFname + cFext
   CASE ( cFile LIKE ".+\.wlt" ); RETURN "application/x-mswallet=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(nc|cdf)" ); RETURN "application/x-netcdf =" + cFname + cFext
   CASE ( cFile LIKE ".+\.npx" ); RETURN "application/x-netfpx =" + cFname + cFext
   CASE ( cFile LIKE ".+\.nsc" ); RETURN "application/x-nschat =" + cFname + cFext
   CASE ( cFile LIKE ".+\.pgp" ); RETURN "application/x-pgp-plugin =" + cFname + cFext
   CASE ( cFile LIKE ".+\.css" ); RETURN "application/x-pointplus=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sh" ); RETURN "application/x-sh =" + cFname + cFext
   CASE ( cFile LIKE ".+\.shar" ); RETURN "application/x-shar=" + cFname + cFext
   CASE ( cFile LIKE ".+\.swf" ); RETURN "application/x-shockwave-flash=" + cFname + cFext
   CASE ( cFile LIKE ".+\.spr" ); RETURN "application/x-sprite =" + cFname + cFext
   CASE ( cFile LIKE ".+\.sprite" ); RETURN "application/x-sprite =" + cFname + cFext
   CASE ( cFile LIKE ".+\.sit" ); RETURN "application/x-stuffit=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sca" ); RETURN "application/x-supercard=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sv4cpio" ); RETURN "application/x-sv4cpio=" + cFname + cFext
   CASE ( cFile LIKE ".+\.sv4crc" ); RETURN "application/x-sv4crc =" + cFname + cFext
   CASE ( cFile LIKE ".+\.tar" ); RETURN "application/x-tar=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tcl" ); RETURN "application/x-tcl=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tex" ); RETURN "application/x-tex=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(texinfo|texi)" ); RETURN "application/x-texinfo=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tlk" ); RETURN "application/x-tlk=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(t|tr|roff)" ); RETURN "application/x-troff=" + cFname + cFext
   CASE ( cFile LIKE ".+\.man" ); RETURN "application/x-troff-man=" + cFname + cFext
   CASE ( cFile LIKE ".+\.me" ); RETURN "application/x-troff-me=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ms" ); RETURN "application/x-troff-ms=" + cFname + cFext
   CASE ( cFile LIKE ".+\.alt" ); RETURN "application/x-up-alert=" + cFname + cFext
   CASE ( cFile LIKE ".+\.che" ); RETURN "application/x-up-cacheop =" + cFname + cFext
   CASE ( cFile LIKE ".+\.ustar" ); RETURN "application/x-ustar=" + cFname + cFext
   CASE ( cFile LIKE ".+\.src" ); RETURN "application/x-wais-source=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xls" ); RETURN "application/xls=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xlt" ); RETURN "application/xlt=" + cFname + cFext
   CASE ( cFile LIKE ".+\.zip" ); RETURN "application/zip=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(au|snd)" ); RETURN "audio/basic=" + cFname + cFext
   CASE ( cFile LIKE ".+\.es" ); RETURN "audio/echospeech =" + cFname + cFext
   CASE ( cFile LIKE ".+\.(gsm|gsd)" ); RETURN "audio/gsm=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rmf" ); RETURN "audio/rmf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tsi" ); RETURN "audio/tsplayer=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vox" ); RETURN "audio/voxware=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wtx" ); RETURN "audio/wtx=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(aif|aiff|aifc)" ); RETURN "audio/x-aiff =" + cFname + cFext
   CASE ( cFile LIKE ".+\.(cht|dus)" ); RETURN "audio/x-dspeech=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(mid|midi)" ); RETURN "audio/x-midi =" + cFname + cFext
   CASE ( cFile LIKE ".+\.mp3" ); RETURN "audio/x-mpeg =" + cFname + cFext
   CASE ( cFile LIKE ".+\.mp2" ); RETURN "audio/x-mpeg =" + cFname + cFext
   CASE ( cFile LIKE ".+\.m3u" ); RETURN "audio/x-mpegurl=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(ram|ra)" ); RETURN "audio/x-pn-realaudio =" + cFname + cFext
   CASE ( cFile LIKE ".+\.rpm" ); RETURN "audio/x-pn-realaudio-plugin=" + cFname + cFext
   CASE ( cFile LIKE ".+\.stream" ); RETURN "audio/x-qt-stream=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rmf" ); RETURN "audio/x-rmf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(vqf|vql)" ); RETURN "audio/x-twinvq=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vqe" ); RETURN "audio/x-twinvq-plugin=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wav" ); RETURN "audio/x-wav=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wtx" ); RETURN "audio/x-wtx=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mol" ); RETURN "chemical/x-mdl-molfile=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pdb" ); RETURN "chemical/x-pdb=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dwf" ); RETURN "drawing/x-dwf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ivr" ); RETURN "i-world/i-vrml=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cod" ); RETURN "image/cis-cod=" + cFname + cFext
   CASE ( cFile LIKE ".+\.cpi" ); RETURN "image/cpi=" + cFname + cFext
   CASE ( cFile LIKE ".+\.fif" ); RETURN "image/fif=" + cFname + cFext
   CASE ( cFile LIKE ".+\.gif" ); RETURN "image/gif=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ief" ); RETURN "image/ief=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(jpeg|jpg|jpe)" ); RETURN "image/jpeg=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rip" ); RETURN "image/rip=" + cFname + cFext
   CASE ( cFile LIKE ".+\.svh" ); RETURN "image/svh=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(tiff|tif)" ); RETURN "image/tiff=" + cFname + cFext
   CASE ( cFile LIKE ".+\.mcf" ); RETURN "image/vasa=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(svf|dwg|dxf)" ); RETURN "image/vnd=" + cFname + cFext
   CASE ( cFile LIKE ".+\.wi" ); RETURN "image/wavelet=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ras" ); RETURN "image/x-cmu-raster=" + cFname + cFext
   CASE ( cFile LIKE ".+\.etf" ); RETURN "image/x-etf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.fpx" ); RETURN "image/x-fpx=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(fh5|fh4|fhc)" ); RETURN "image/x-freehand =" + cFname + cFext
   CASE ( cFile LIKE ".+\.dsf" ); RETURN "image/x-mgx-dsf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pnm" ); RETURN "image/x-portable-anymap=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pbm" ); RETURN "image/x-portable-bitmap=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pgm" ); RETURN "image/x-portable-graymap =" + cFname + cFext
   CASE ( cFile LIKE ".+\.ppm" ); RETURN "image/x-portable-pixmap=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rgb" ); RETURN "image/x-rgb=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xbm" ); RETURN "image/x-xbitmap=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xpm" ); RETURN "image/x-xpixmap=" + cFname + cFext
   CASE ( cFile LIKE ".+\.xwd" ); RETURN "image/x-xwindowdump=" + cFname + cFext
   CASE ( cFile LIKE ".+\.dig" ); RETURN "multipart/mixed=" + cFname + cFext
   CASE ( cFile LIKE ".+\.push" ); RETURN "multipart/x-mixed-replace=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(wan|waf)" ); RETURN "plugin/wanimate=" + cFname + cFext
   CASE ( cFile LIKE ".+\.ccs" ); RETURN "text/ccs =" + cFname + cFext
   CASE ( cFile LIKE ".+\.(htm|html)" ); RETURN "text/html=" + cFname + cFext
   CASE ( cFile LIKE ".+\.pgr" ); RETURN "text/parsnegar-document=" + cFname + cFext
   CASE ( cFile LIKE ".+\.txt" ); RETURN "text/plain=" + cFname + cFext
   CASE ( cFile LIKE ".+\.rtx" ); RETURN "text/richtext=" + cFname + cFext
   CASE ( cFile LIKE ".+\.tsv" ); RETURN "text/tab-separated-values=" + cFname + cFext
   CASE ( cFile LIKE ".+\.hdml" ); RETURN "text/x-hdml=" + cFname + cFext
   CASE ( cFile LIKE ".+\.etx" ); RETURN "text/x-setext=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(talk|spc)" ); RETURN "text/x-speech=" + cFname + cFext
   CASE ( cFile LIKE ".+\.afl" ); RETURN "video/animaflex=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(mpeg|mpg|mpe)" ); RETURN "video/mpeg=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(qt|mov)" ); RETURN "video/quicktime=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(viv|vivo)" ); RETURN "video/vnd.vivo=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(asf|asx)" ); RETURN "video/x-ms-asf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.avi" ); RETURN "video/x-msvideo=" + cFname + cFext
   CASE ( cFile LIKE ".+\.movie" ); RETURN "video/x-sgi-movie=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(vgm|vgx|xdr)" ); RETURN "video/x-videogram=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vgp" ); RETURN "video/x-videogram-plugin =" + cFname + cFext
   CASE ( cFile LIKE ".+\.vts" ); RETURN "workbook/formulaone=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vtts" ); RETURN "workbook/formulaone=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(3dmf|3dm|qd3d|qd3)" ); RETURN "x-world/x-3dmf=" + cFname + cFext
   CASE ( cFile LIKE ".+\.svr" ); RETURN "x-world/x-svr=" + cFname + cFext
   CASE ( cFile LIKE ".+\.(wrl|wrz)" ); RETURN "x-world/x-vrml=" + cFname + cFext
   CASE ( cFile LIKE ".+\.vrt" ); RETURN "x-world/x-vrt=" + cFname + cFext
   ENDCASE

   RETURN "text/plain;filename=" + cFname + cFext
