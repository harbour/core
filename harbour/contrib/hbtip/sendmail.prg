/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * HB_SendMail()
 * ( This version of HB_SendMail() started from Luiz's original work on SendMail() )
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes & Patrick Mast
 *
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

#translate ( <exp1> LIKE <exp2> )   => ( HB_REGEXLIKE( (<exp2>), (<exp1>) ) )

FUNCTION HB_SendMail( cServer, nPort, cFrom, aTo, aCC, aBCC, cBody, cSubject, aFiles, cUser, cPass, cPopServer, nPriority, lRead, lTrace, lPopAuth, lNoAuth, nTimeOut, cReplyTo )
   /*
   cServer    -> Required. IP or domain name of the mail server
   nPort      -> Optional. Port used my email server
   cFrom      -> Required. Email address of the sender
   aTo        -> Required. Character string or array of email addresses to send the email to
   aCC        -> Optional. Character string or array of email adresses for CC (Carbon Copy)
   aBCC       -> Optional. Character string or array of email adresses for BCC (Blind Carbon Copy)
   cBody      -> Optional. The body message of the email as text, or the filename of the HTML message to send.
   cSubject   -> Optional. Subject of the sending email
   aFiles     -> Optional. Array of attachments to the email to send
   cUser      -> Required. User name for the POP3 server
   cPass      -> Required. Password for cUser
   cPopServer -> Required. Pop3 server name or address
   nPriority  -> Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low
   lRead      -> Optional. If set to .T., a confirmation request is send. Standard setting is .F.
   lTrace     -> Optional. If set to .T., a log file is created (sendmail<nNr>.log). Standard setting is .F.
   lNoAuth    -> Optional. Disable Autentication methods
   nTimeOut   -> Optional. Number os ms to wait default 20000 (20s)
   cReplyTo   -> Optional.
   */

   LOCAL oInMail, cBodyTemp, oUrl, oMail, oAttach, aThisFile, cMimeText,;
         cFile, cFname, cFext, cData, oUrl1

   LOCAL cTmp          := ""
   LOCAL cTo           := ""
   LOCAL cCC           := ""
   LOCAL cBCC          := ""

   LOCAL lConnectPlain := .F.
   LOCAL lReturn       := .T.
   LOCAL lAuthLogin    := .F.
   LOCAL lAuthPlain    := .F.
   LOCAL lConnect      := .T.
   LOCAL oPop

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
   IF ! ISLOGICAL( lTrace )
      lTrace := .F.
   ENDIF
   IF ! ISLOGICAL( lPopAuth )
      lPopAuth := .T.
   ENDIF
   IF ! ISLOGICAL( lNoAuth )
      lNoAuth := .F.
   ENDIF
   IF ! ISNUMBER( nTimeOut )
      nTimeOut := 100
   ENDIF
   IF ! ISCHARACTER( cReplyTo )
      cReplyTo := ""
   ENDIF

   cUser := StrTran( cUser, "@", "&at;" )

   IF !( (".htm" $ Lower( cBody ) .OR. ".html" $ Lower( cBody ) ) .AND. hb_FileExists(cBody) )

      IF !( Right( cBody, 2 ) == HB_OSNewLine() )
         cBody += HB_OsNewLine()
      ENDIF

   ENDIF

   // cTo
   IF ISARRAY( aTo )
      IF Len( aTo ) > 1
         FOR EACH cTo IN aTo
            IF cTo:__enumIndex() != 1
               IF !Empty( cTo )
                  cTmp += cTo + ","
               ENDIF
            ENDIF
         NEXT
         cTmp := SubStr( cTmp, 1, Len( cTmp ) - 1 )
      ENDIF
      cTo := aTo[ 1 ]
      IF Len( cTmp ) > 0
         cTo += "," + cTmp
      ENDIF
   ELSEIF ISCHARACTER( aTo )
      cTo := AllTrim( aTo )
   ENDIF


   // CC (Carbon Copy)
   IF ISARRAY( aCC )
      IF Len( aCC ) > 0
         FOR EACH cTmp IN aCC
            IF !Empty( cTmp )
               cCC += cTmp + ","
            ENDIF
         NEXT
         cCC := SubStr( cCC, 1, Len( cCC ) - 1 )
      ENDIF
   ELSEIF ISCHARACTER(aCC)
      cCC := AllTrim( aCC )
   ENDIF


   // BCC (Blind Carbon Copy)
   IF ISARRAY(aBCC)
      IF Len(aBCC) > 0
         FOR EACH cTmp IN aBCC
            IF !Empty( cTmp )
               cBCC += cTmp + ","
            ENDIF
         NEXT
         cBCC := SubStr( cBCC, 1, Len( cBCC ) - 1 )
      ENDIF
   ELSEIF ISCHARACTER(aBCC)
      cBCC := AllTrim( aBCC )
   ENDIF

   IF cPopServer != NIL .AND. lPopAuth
      BEGIN SEQUENCE
         oUrl1 := tUrl():New( "pop://" + cUser + ":" + cPass + "@" + cPopServer + "/" )
         oUrl1:cUserid := Strtran( cUser, "&at;", "@" )
         opop:= tIPClientPOP():New( oUrl1, lTrace )
         IF oPop:Open()
            oPop:Close()
         ENDIF
      RECOVER
         lReturn := .F.
      END SEQUENCE

   ENDIF

   IF !lReturn
      RETURN .F.
   ENDIF

   BEGIN SEQUENCE
      oUrl := tUrl():New( "smtp://" + cUser + "@" + cServer + "/" + cTo )
   RECOVER
      lReturn := .F.
   END SEQUENCE

   IF !lReturn
      RETURN .F.
   ENDIF

   oUrl:nPort   := nPort
   oUrl:cUserid := Strtran( cUser, "&at;", "@" )

   oMail   := tipMail():new()
   oAttach := tipMail():new()
   oAttach:SetEncoder( "7-bit" )

   IF (".htm" $ Lower( cBody ) .OR. ".html" $ Lower( cBody ) ) .AND. hb_FileExists(cBody)
      cMimeText := "text/html ; charset=ISO-8859-1"
      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      cBodyTemp := cBody
      cBody     := MemoRead( cBodyTemp ) + chr( 13 ) + chr( 10 )

   ELSE
      oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=iso8851"
   ENDIF

   oAttach:SetBody( cBody )
   oMail:Attach( oAttach )
   oUrl:cFile := cTo + iif( Empty(cCC), "", "," + cCC ) + iif( Empty(cBCC), "", "," + cBCC)

   oMail:hHeaders[ "Date" ] := tip_Timestamp()
   oMail:hHeaders[ "From" ] := cFrom

   IF !Empty(cCC)
      oMail:hHeaders[ "Cc" ] := cCC
   ENDIF
   IF !Empty(cBCC)
      oMail:hHeaders[ "Bcc" ] := cBCC
   ENDIF
   IF !Empty(cReplyTo)
      oMail:hHeaders[ "Reply-To" ] := cReplyTo
   ENDIF

   BEGIN SEQUENCE
      oInmail := tIPClientSMTP():New( oUrl, lTrace)
   RECOVER
      lReturn := .F.
   END SEQUENCE

   IF !lReturn
      RETURN .F.
   ENDIF

   oInmail:nConnTimeout:= nTimeOut

   IF !lNoAuth

      IF oInMail:OpenSecure()

         DO WHILE .T.
            oInMail:GetOk()
            IF oInMail:cReply == NIL
               EXIT
            ELSEIF "LOGIN" $ oInMail:cReply
               lAuthLogin := .T.
            ELSEIF "PLAIN" $ oInMail:cReply
               lAuthPlain := .T.
            ENDIF
         ENDDO

         IF lAuthLogin
            IF !oInMail:Auth( cUser, cPass )
               lConnect := .F.
            ELSE
               lConnectPlain  := .T.
            ENDIF
         ENDIF

         IF lAuthPlain .AND. !lConnect
            IF !oInMail:AuthPlain( cUser, cPass )
               lConnect := .F.
            ENDIF
         ELSE
            IF !lConnectPlain
               oInmail:Getok()
               lConnect := .F.
            ENDIF
         ENDIF
      ELSE
         lConnect := .F.
      ENDIF
   ELSE
      lConnect := .F.
   ENDIF

   IF !lConnect

      if !lNoAuth
         oInMail:Close()
      endif

      BEGIN SEQUENCE
         oInmail := tIPClientsmtp():New( oUrl, lTrace)
      RECOVER
         lReturn := .F.
      END SEQUENCE

      oInmail:nConnTimeout:=nTimeOut


      IF !oInMail:Open()
         oInmail:Close()
         RETURN .F.
      ENDIF

      WHILE .T.
         oInMail:GetOk()
         IF oInMail:cReply == NIL
            EXIT
         ENDIF
      ENDDO

   ENDIF

   oInMail:oUrl:cUserid := cFrom
   oMail:hHeaders[ "To" ]      := cTo
   oMail:hHeaders[ "Subject" ] := cSubject

   FOR EACH aThisFile IN AFiles

      IF ISCHARACTER( aThisFile )
         cFile := aThisFile
         cData := Memoread( cFile ) + chr( 13 ) + chr( 10 )
      ELSEIF ISARRAY( aThisFile ) .AND. Len( aThisFile ) >= 2
         cFile := aThisFile[ 1 ]
         cData := aThisFile[ 2 ] + chr( 13 ) + chr( 10 )
      ELSE
         lReturn := .F.
         EXIT
      ENDIF

      oAttach := TipMail():New()

      HB_FNameSplit( cFile,, @cFname, @cFext )
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
         ( cFile LIKE ".+\.(svr|wrl|wrz|vrt)" ) .OR. Empty(cFExt)
         oAttach:SetEncoder( "base64" )
      ELSE
         oAttach:SetEncoder( "7-bit" )
      ENDIF

      cMimeText := HB_SetMimeType( cFile, cFname, cFext )
      // Some EMAIL readers use Content-Type to check for filename

      IF ".html" $ lower( cFext) .OR. ".htm" $ lower( cFext )
         cMimeText += "; charset=ISO-8859-1"
      ENDIF

      oAttach:hHeaders[ "Content-Type" ] := cMimeText
      // But usually, original filename is set here
      oAttach:hHeaders[ "Content-Disposition" ] := "attachment; filename=" + cFname + cFext
      oAttach:SetBody( cData )
      oMail:Attach( oAttach )

   NEXT

   IF lRead
      oMail:hHeaders[ "Disposition-Notification-To" ] := cFrom
   ENDIF

   IF nPriority != 3
      oMail:hHeaders[ "X-Priority" ] := Str( nPriority, 1 )
   ENDIF

   oInmail:Write( oMail:ToString() )
   oInMail:Commit()
   oInMail:Close()

RETURN lReturn


//-------------------------------------------------------------//


FUNCTION HB_SetMimeType( cFile, cFname, cFext )

   cFile := Lower( cFile )

   IF     ( cFile LIKE ".+\.vbd" ); RETURN "application/activexdocument="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(asn|asz|asd)" ); RETURN "application/astound="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pqi" ); RETURN "application/cprplayer=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tsp" ); RETURN "application/dsptype="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.exe" ); RETURN "application/exe="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(sml|ofml)" ); RETURN "application/fml="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pfr" ); RETURN "application/font-tdpfr=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.frl" ); RETURN "application/freeloader=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.spl" ); RETURN "application/futuresplash =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.gz" ); RETURN "application/gzip =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.stk" ); RETURN "application/hstu =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ips" ); RETURN "application/ips="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ptlk" ); RETURN "application/listenup =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.hqx" ); RETURN "application/mac-binhex40 =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mbd" ); RETURN "application/mbedlet="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mfp" ); RETURN "application/mirage=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(pot|pps|ppt|ppz)" ); RETURN "application/mspowerpoint =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.doc" ); RETURN "application/msword=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.n2p" ); RETURN "application/n2p="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(bin|class|lha|lzh|lzx|dbf)" ); RETURN "application/octet-stream =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.oda" ); RETURN "application/oda="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.axs" ); RETURN "application/olescript=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.zpa" ); RETURN "application/pcphoto="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pdf" ); RETURN "application/pdf="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(ai|eps|ps)" ); RETURN "application/postscript=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.shw" ); RETURN "application/presentations=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.qrt" ); RETURN "application/quest=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rtc" ); RETURN "application/rtc="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rtf" ); RETURN "application/rtf="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.smp" ); RETURN "application/studiom="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.dst" ); RETURN "application/tajima=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.talk" ); RETURN "application/talker=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tbk" ); RETURN "application/toolbook =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vmd" ); RETURN "application/vocaltec-media-desc="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vmf" ); RETURN "application/vocaltec-media-file="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wri" ); RETURN "application/write=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wid" ); RETURN "application/x-DemoShield =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rrf" ); RETURN "application/x-InstallFromTheWeb="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wis" ); RETURN "application/x-InstallShield="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ins" ); RETURN "application/x-NET-Install=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tmv" ); RETURN "application/x-Parable-Thing="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.arj" ); RETURN "application/x-arj=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.asp" ); RETURN "application/x-asap=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.aab" ); RETURN "application/x-authorware-bin =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(aam|aas)" ); RETURN "application/x-authorware-map =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.bcpio" ); RETURN "application/x-bcpio="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vcd" ); RETURN "application/x-cdlink =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.chat" ); RETURN "application/x-chat=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.cnc" ); RETURN "application/x-cnc=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(coda|page)" ); RETURN "application/x-coda=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.z" ); RETURN "application/x-compress=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.con" ); RETURN "application/x-connector="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.cpio" ); RETURN "application/x-cpio=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pqf" ); RETURN "application/x-cprplayer="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.csh" ); RETURN "application/x-csh=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(cu|csm)" ); RETURN "application/x-cu-seeme=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(dcr|dir|dxr|swa)" ); RETURN "application/x-director=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.dvi" ); RETURN "application/x-dvi=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.evy" ); RETURN "application/x-envoy="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ebk" ); RETURN "application/x-expandedbook=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.gtar" ); RETURN "application/x-gtar=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.hdf" ); RETURN "application/x-hdf=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.map" ); RETURN "application/x-httpd-imap =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.phtml" ); RETURN "application/x-httpd-php="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.php3" ); RETURN "application/x-httpd-php3 =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ica" ); RETURN "application/x-ica=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ipx" ); RETURN "application/x-ipix=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ips" ); RETURN "application/x-ipscript=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.js" ); RETURN "application/x-javascript =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.latex" ); RETURN "application/x-latex="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.bin" ); RETURN "application/x-macbinary="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mif" ); RETURN "application/x-mif=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(mpl|mpire)" ); RETURN "application/x-mpire="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.adr" ); RETURN "application/x-msaddr =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wlt" ); RETURN "application/x-mswallet=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(nc|cdf)" ); RETURN "application/x-netcdf =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.npx" ); RETURN "application/x-netfpx =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.nsc" ); RETURN "application/x-nschat =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pgp" ); RETURN "application/x-pgp-plugin =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.css" ); RETURN "application/x-pointplus="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.sh" ); RETURN "application/x-sh =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.shar" ); RETURN "application/x-shar=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.swf" ); RETURN "application/x-shockwave-flash=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.spr" ); RETURN "application/x-sprite =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.sprite" ); RETURN "application/x-sprite =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.sit" ); RETURN "application/x-stuffit=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.sca" ); RETURN "application/x-supercard="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.sv4cpio" ); RETURN "application/x-sv4cpio=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.sv4crc" ); RETURN "application/x-sv4crc =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tar" ); RETURN "application/x-tar=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tcl" ); RETURN "application/x-tcl=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tex" ); RETURN "application/x-tex=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(texinfo|texi)" ); RETURN "application/x-texinfo=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tlk" ); RETURN "application/x-tlk=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(t|tr|roff)" ); RETURN "application/x-troff="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.man" ); RETURN "application/x-troff-man="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.me" ); RETURN "application/x-troff-me=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ms" ); RETURN "application/x-troff-ms=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.alt" ); RETURN "application/x-up-alert=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.che" ); RETURN "application/x-up-cacheop =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ustar" ); RETURN "application/x-ustar="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.src" ); RETURN "application/x-wais-source=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.xls" ); RETURN "application/xls="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.xlt" ); RETURN "application/xlt="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.zip" ); RETURN "application/zip="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(au|snd)" ); RETURN "audio/basic="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.es" ); RETURN "audio/echospeech =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(gsm|gsd)" ); RETURN "audio/gsm=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rmf" ); RETURN "audio/rmf=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tsi" ); RETURN "audio/tsplayer=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vox" ); RETURN "audio/voxware=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wtx" ); RETURN "audio/wtx=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(aif|aiff|aifc)" ); RETURN "audio/x-aiff =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(cht|dus)" ); RETURN "audio/x-dspeech="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(mid|midi)" ); RETURN "audio/x-midi =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mp3" ); RETURN "audio/x-mpeg =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mp2" ); RETURN "audio/x-mpeg =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.m3u" ); RETURN "audio/x-mpegurl="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(ram|ra)" ); RETURN "audio/x-pn-realaudio =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rpm" ); RETURN "audio/x-pn-realaudio-plugin="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.stream" ); RETURN "audio/x-qt-stream=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rmf" ); RETURN "audio/x-rmf="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(vqf|vql)" ); RETURN "audio/x-twinvq=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vqe" ); RETURN "audio/x-twinvq-plugin=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wav" ); RETURN "audio/x-wav="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wtx" ); RETURN "audio/x-wtx="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mol" ); RETURN "chemical/x-mdl-molfile=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pdb" ); RETURN "chemical/x-pdb=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.dwf" ); RETURN "drawing/x-dwf=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ivr" ); RETURN "i-world/i-vrml=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.cod" ); RETURN "image/cis-cod=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.cpi" ); RETURN "image/cpi=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.fif" ); RETURN "image/fif=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.gif" ); RETURN "image/gif=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ief" ); RETURN "image/ief=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(jpeg|jpg|jpe)" ); RETURN "image/jpeg=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rip" ); RETURN "image/rip=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.svh" ); RETURN "image/svh=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(tiff|tif)" ); RETURN "image/tiff=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.mcf" ); RETURN "image/vasa=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(svf|dwg|dxf)" ); RETURN "image/vnd=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.wi" ); RETURN "image/wavelet=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ras" ); RETURN "image/x-cmu-raster=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.etf" ); RETURN "image/x-etf="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.fpx" ); RETURN "image/x-fpx="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(fh5|fh4|fhc)" ); RETURN "image/x-freehand =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.dsf" ); RETURN "image/x-mgx-dsf="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pnm" ); RETURN "image/x-portable-anymap="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pbm" ); RETURN "image/x-portable-bitmap="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pgm" ); RETURN "image/x-portable-graymap =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ppm" ); RETURN "image/x-portable-pixmap="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rgb" ); RETURN "image/x-rgb="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.xbm" ); RETURN "image/x-xbitmap="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.xpm" ); RETURN "image/x-xpixmap="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.xwd" ); RETURN "image/x-xwindowdump="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.dig" ); RETURN "multipart/mixed="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.push" ); RETURN "multipart/x-mixed-replace=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(wan|waf)" ); RETURN "plugin/wanimate="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.ccs" ); RETURN "text/ccs =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(htm|html)" ); RETURN "text/html=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.pgr" ); RETURN "text/parsnegar-document="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.txt" ); RETURN "text/plain=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.rtx" ); RETURN "text/richtext=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.tsv" ); RETURN "text/tab-separated-values=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.hdml" ); RETURN "text/x-hdml="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.etx" ); RETURN "text/x-setext=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(talk|spc)" ); RETURN "text/x-speech=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.afl" ); RETURN "video/animaflex="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(mpeg|mpg|mpe)" ); RETURN "video/mpeg=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(qt|mov)" ); RETURN "video/quicktime="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(viv|vivo)" ); RETURN "video/vnd.vivo=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(asf|asx)" ); RETURN "video/x-ms-asf=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.avi" ); RETURN "video/x-msvideo="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.movie" ); RETURN "video/x-sgi-movie=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(vgm|vgx|xdr)" ); RETURN "video/x-videogram=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vgp" ); RETURN "video/x-videogram-plugin =" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vts" ); RETURN "workbook/formulaone="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vtts" ); RETURN "workbook/formulaone="+cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(3dmf|3dm|qd3d|qd3)" ); RETURN "x-world/x-3dmf=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.svr" ); RETURN "x-world/x-svr=" + cFname + cFext
   ELSEIF ( cFile LIKE ".+\.(wrl|wrz)" ); RETURN "x-world/x-vrml=" +cFname + cFext
   ELSEIF ( cFile LIKE ".+\.vrt" ); RETURN "x-world/x-vrt=" + cFname + cFext
   ENDIF

RETURN "text/plain;filename=" + cFname + cFext
