/*
 * Harbour Project source code:
 * tip_MailAssemble() (This version started from Luiz's original work on SendMail())
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes and Patrick Mast
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

   /* Attempt to convert to selected charset if it's supported
      by Harbour (and linked to app). */
   IF Upper( cCharset ) == "UTF-8"
      cCharsetCP := "UTF8"
   ELSEIF hb_cdpExists( Lower( cCharset ) )
      cCharsetCP := hb_cdpUniID( Lower( cCharset ) )
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

      cMimeText := NIL

      IF HB_ISSTRING( aThisFile )
         cFile := aThisFile
         cData := hb_MemoRead( cFile )
      ELSEIF HB_ISARRAY( aThisFile ) .AND. Len( aThisFile ) >= 2
         cFile := aThisFile[ 1 ]
         IF HB_ISSTRING( aThisFile[ 2 ] )
            cData := aThisFile[ 2 ]
            hb_default( @cFile, "unnamed" )
         ELSEIF HB_ISSTRING( cFile )
            cData := hb_MemoRead( cFile )
         ELSE
            LOOP /* No filename and no content. */
         ENDIF
         IF Len( aThisFile ) >= 3 .AND. HB_ISSTRING( aThisFile[ 3 ] )
            cMimeText := aThisFile[ 3 ]
         ENDIF
      ELSE
         LOOP
      ENDIF

      hb_default( @cMimeText, tip_FileNameMimeType( cFile ) )

      cFileCP := iif( Empty( cCharsetCP ), cFile, hb_Translate( cFile,, cCharsetCP ) )

      oAttach := TIPMail():New()
      oAttach:SetCharset( cCharset )
      oAttach:SetEncoder( iif( Left( cMimeText, Len( "text/" ) ) == "text/", cEncoding, "base64" ) )
      // Some e-mail clients use Content-Type to check for filename
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
      oMail:hHeaders[ "X-Priority" ] := hb_ntos( Int( nPriority ) )
   ENDIF

   RETURN oMail:ToString()
