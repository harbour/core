/*
 * tip_MailAssemble() (This version started from Luiz's original work on SendMail())
 *
 * Copyright 2007 Luiz Rafael Culik Guimaraes and Patrick Mast
 * Copyright 2009-2017 Viktor Szakats (vszakats.net/harbour) (SSL support)
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
FUNCTION hb_MailAssemble( ... )
   RETURN tip_MailAssemble( ... )
#endif

FUNCTION tip_MailAssemble( ;
      cFrom, ;       /* Required. Email address of the sender */
      xTo, ;         /* Required. Character string or array of email addresses to send the email to */
      xCC, ;         /* Optional. Character string or array of email addresses for CC (Carbon Copy) */
      cBody, ;       /* Optional. The body message of the email as text, or the filename of the HTML message to send. */
      cSubject, ;    /* Optional. Subject of the sending email */
      aFiles, ;      /* Optional. Array of attachments to the email to send */
      nPriority, ;   /* Optional. Email priority: 1=High, 3=Normal (Standard), 5=Low */
      lRead, ;       /* Optional. If set to .T., a confirmation request is send. Standard setting is .F. */
      cReplyTo, ;    /* Optional. */
      cCharset, ;    /* Optional. */
      cEncoding, ;   /* Optional. */
      lBodyHTML, ;   /* Optional. */
      bSMIME )       /* Optional. S/MIME signing/encrypting callback */

   LOCAL oMail
   LOCAL oAttach
   LOCAL aThisFile
   LOCAL cMimeType
   LOCAL cFile
   LOCAL cData
   LOCAL cContentType
   LOCAL nAttr

   LOCAL cCharsetCP
   LOCAL tmp

   IF Empty( cFrom ) .OR. ! HB_ISSTRING( cFrom )
      RETURN ""
   ENDIF
   IF Empty( xTo ) .OR. ( ! HB_ISSTRING( xTo ) .AND. ! HB_ISARRAY( xTo ) )
      RETURN ""
   ENDIF

   hb_default( @cBody, "" )
   hb_default( @cSubject, "" )
   hb_default( @aFiles, {} )
   nPriority := Int( hb_defaultValue( nPriority, 3 ) )
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

   cContentType := iif( lBodyHTML, "text/html", "text/plain" ) + "; charset=" + cCharset

   /* add ending EOL to body, if there wasn't any */
   IF ! Right( cBody, 2 ) == Chr( 13 ) + Chr( 10 )
      cBody += Chr( 13 ) + Chr( 10 )
   ENDIF

   /* Convert input to the CP of the email */
   IF ! Empty( cCharsetCP )
      xTo := s_TransCP( xTo, cCharsetCP )
      xCC := s_TransCP( xCC, cCharsetCP )
      cFrom := s_TransCP( cFrom, cCharsetCP )
      cBody := s_TransCP( cBody, cCharsetCP )
      cSubject := s_TransCP( cSubject, cCharsetCP )
   ENDIF

   oMail := TIPMail():New()
   oMail:SetEncoder( cEncoding )
   oMail:SetCharset( cCharset )
   IF Empty( aFiles )
      oMail:hHeaders[ "Content-Type" ] := cContentType
      oMail:SetBody( cBody )
   ELSE
      oAttach := TIPMail():New()
      oAttach:SetEncoder( cEncoding )
      oAttach:SetCharset( cCharset )
      oAttach:hHeaders[ "Content-Type" ] := cContentType
      oAttach:SetBody( cBody )
      oMail:Attach( oAttach )

      FOR EACH aThisFile IN aFiles

         cMimeType := NIL
         nAttr := 0

         DO CASE
         CASE HB_ISSTRING( aThisFile )
            cFile := aThisFile
            cData := hb_MemoRead( cFile )
            hb_vfAttrGet( cFile, @nAttr )
         CASE HB_ISARRAY( aThisFile ) .AND. Len( aThisFile ) >= 2
            cFile := aThisFile[ 1 ]
            IF HB_ISSTRING( aThisFile[ 2 ] )
               cData := aThisFile[ 2 ]
               hb_default( @cFile, "unnamed" )
            ELSEIF HB_ISSTRING( cFile )
               cData := hb_MemoRead( cFile )
               hb_vfAttrGet( cFile, @nAttr )
            ELSE
               LOOP  /* No filename and no content. */
            ENDIF
            IF Len( aThisFile ) >= 3 .AND. HB_ISSTRING( aThisFile[ 3 ] )
               cMimeType := aThisFile[ 3 ]
            ENDIF
         OTHERWISE
            LOOP
         ENDCASE

         IF cMimeType == NIL
            cMimeType := tip_FileNameMimeType( cFile, "application/octet-stream" )
         ENDIF
         cFile := s_TransCP( cFile, cCharsetCP )

         oAttach := TIPMail():New()
         oAttach:SetCharset( cCharset )
         oAttach:SetEncoder( iif( hb_LeftEq( cMimeType, "text/" ), cEncoding, "base64" ) )

         IF cMimeType == "text/html"
            cMimeType += "; charset=" + cCharset
            IF ! Right( cData, 2 ) == Chr( 13 ) + Chr( 10 )
               cData += Chr( 13 ) + Chr( 10 )
            ENDIF
         ENDIF
         // Some email clients use Content-Type to check for filename
         cMimeType += "; name=" + '"' + hb_FNameNameExt( cFile ) + '"'
         IF ( nAttr := __tip_FAttrToUmask( nAttr ) ) != 0
            cMimeType += "; x-unix-mode=" + '"' + hb_NumToHex( nAttr, 4 ) + '"'
         ENDIF
         oAttach:hHeaders[ "Content-Type" ] := cMimeType
         // Usually, original filename is set here
         oAttach:hHeaders[ "Content-Disposition" ] := "attachment; filename=" + '"' + hb_FNameNameExt( cFile ) + '"'
         oAttach:SetBody( cData )
         oMail:Attach( oAttach )
      NEXT
   ENDIF

   IF HB_ISEVALITEM( bSMIME ) .AND. ;
      HB_ISSTRING( tmp := Eval( bSMIME, oMail:ToString() ) )

      oMail := TIPMail():New()
      oMail:SetCharset( cCharset )
   ELSE
      tmp := NIL
   ENDIF

   oMail:SetHeader( cSubject, cFrom, xTo, xCC )
   oMail:hHeaders[ "Date" ] := tip_TimeStamp()
   IF ! Empty( cReplyTo )
      oMail:hHeaders[ "Reply-to" ] := cReplyTo
   ENDIF
   IF lRead
      oMail:hHeaders[ "Disposition-Notification-To" ] := tip_GetRawEmail( cFrom )
   ENDIF
   IF nPriority != 3
      oMail:hHeaders[ "X-Priority" ] := hb_ntos( nPriority )
   ENDIF

   RETURN iif( HB_ISSTRING( tmp ), oMail:HeadersToString() + tmp, oMail:ToString() )

STATIC FUNCTION s_TransCP( xData, cCP )

   LOCAL tmp

   IF ! Empty( cCP )
      DO CASE
      CASE HB_ISSTRING( xData )
         RETURN hb_Translate( xData,, cCP )
      CASE HB_ISARRAY( xData )
         FOR EACH tmp IN xData
            tmp := hb_Translate( tmp,, cCP )
         NEXT
      ENDCASE
   ENDIF

   RETURN xData
