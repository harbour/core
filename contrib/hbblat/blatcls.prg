/*
 * BLAT wrapper library interface code.
 *
 * Copyright 2007-2012 Francesco Saverio Giudice <info@fsgiudice.com>
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

#include "hbblat.ch"
#include "hbclass.ch"

CREATE CLASS HBBlat

   PROTECTED:
   VAR nBlatError              AS NUMERIC INIT BLAT_SUCCESS    // Will contains numerical error returned from BLAT
   VAR cBlatError              AS STRING  INIT ""              // Will contains error string returned from BLAT
   VAR nError                  AS NUMERIC INIT BLAT_SUCCESS
   VAR cError                  AS STRING  INIT ""
   VAR aErrors                 AS ARRAY   INIT {}
   VAR cCommand                AS STRING  INIT ""
   VAR cVersion                AS STRING  INIT "0.2"
   VAR cBlatVersion            AS STRING  INIT "2.7.6"
   VAR lChecked                AS LOGICAL INIT .F.

   EXPORTED:
   // Installation
   VAR lSaveSettings           AS LOGICAL INIT .F.

   // store common settings to the Windows Registry.  Takes the
   // same parameters as -install, and is only for SMTP settings.
   METHOD SaveSettings( ... )  INLINE ::Install( "SMTP", ... )
#if 0
   VAR lInstall                AS LOGICAL INIT .F.
#endif
   METHOD Install( cService, cServerAddress, cSenderEmailAddress, nTries, nPort, cProfile, cUsername, cPassword ) VIRTUAL // TODO

   // Basics
   VAR cOptionFile             AS STRING
   VAR cTO                     AS STRING                       // recipient list (comma separated)
   VAR cTOFile                 AS STRING                       // recipient list filename
   VAR cCC                     AS STRING                       // carbon copy recipient list (comma separated)
   VAR cCCFile                 AS STRING                       // cc recipient list filename
   VAR cBCC                    AS STRING                       // blind carbon copy recipient list (comma separated)
   VAR cBCCFile                AS STRING                       // bcc recipient list filename
   VAR nMaxNames               AS NUMERIC INIT 0               // send to groups of x number of recipients
   VAR lToUndiscloseRecipients AS LOGICAL INIT .F.             // set To: header to Undisclosed Recipients if not using the ::cTo and ::cCC options
   VAR cSubject                AS STRING                       // subject line
   VAR lSuppressSubject        AS LOGICAL INIT .F.             // suppress subject line if it is not defined
   VAR cSubjectFile            AS STRING                       // file containing subject line
   VAR cBody                   AS STRING                       // message body
   VAR cBodyFile               AS STRING                       // file containing the message body
   VAR cSignatureFile          AS STRING                       // text file containing your email signature
   VAR cTagFile                AS STRING                       // text file containing taglines, to be randomly chosen
   VAR cPostScriptumFile       AS STRING                       // final message text, possibly for unsubscribe instructions

   // Registry Overrides
   VAR cProfile                AS STRING                       // Send using cProfile profile (using server, user, and port)
   VAR lShowProfiles           AS LOGICAL INIT .F.             // list all profiles in the Registry
   VAR cServerSMTP             AS STRING                       // specify SMTP server to be used (optionally, addr:port)
   VAR cServerNNTP             AS STRING                       // specify NNTP server to be used (optionally, addr:port)
   VAR cServerPOP3             AS STRING                       // specify POP3 server to be used (optionally, addr:port) when POP3 access is required before sending email
   VAR cServerIMAP             AS STRING                       // specify IMAP server to be used (optionally, addr:port) when IMAP access is required before sending email
   VAR cMailFrom               AS STRING
   VAR cFrom                   AS STRING
   VAR cReplyTo                AS STRING
   VAR cReturnPath             AS STRING
   VAR cSender                 AS STRING
   VAR nPortSMTP               AS NUMERIC                      // Blat default 25
   VAR nPortNNTP               AS NUMERIC                      // Blat default 119
   VAR nPortPOP3               AS NUMERIC                      // Blat default 110
   VAR nPortIMAP               AS NUMERIC                      // Blat default 143
   VAR cUserAUTH               AS STRING
   VAR cPasswordAUTH           AS STRING
   VAR cUserPOP3               AS STRING
   VAR cPasswordPOP3           AS STRING
   VAR cUserIMAP               AS STRING
   VAR cPasswordIMAP           AS STRING
   VAR lNoMD5                  AS LOGICAL INIT .F.             // if .T. Do NOT use CRAM-MD5 authentication.  Use this in cases where the server's CRAM-MD5 is broken, such as Network Solutions

   // Miscellaneous RFC header switches
   VAR cOrganization           AS STRING
   VAR cUserAgent              AS STRING
   VAR cXHeader                AS STRING
   VAR lNoBlatHomePage         AS LOGICAL INIT .T.
   VAR lNoBlatXHeader          AS LOGICAL INIT .T.
   VAR lRequestDisposition     AS LOGICAL INIT .F.
   VAR lRequestReturnReceipt   AS LOGICAL INIT .F.
   VAR cCharSet                AS STRING
   VAR cUserHeader1            AS STRING
   VAR cUserHeader2            AS STRING
   VAR cDSN                    AS STRING                 // Delivery Status Notifications (RFC 3461): n: never, s: successful, f: failure, d: delayed - can be used together, however N takes precedence
   VAR lEHBase64               AS LOGICAL INIT .F.       // use base64 for encoding headers, if necessary
   VAR lEHQuoted               AS LOGICAL INIT .F.       // use quoted-printable for encoding headers, if necessary
   VAR lLowPriority            AS LOGICAL INIT .F.
   VAR lHighPriority           AS LOGICAL INIT .F.
   VAR nSensitivity            AS NUMERIC INIT -1        // set message sensitity 0 for personal, 1 for private, 2 for company-confidential

   // Attachment and encoding options
   VAR aAttachBinFiles         AS ARRAY
   VAR cAttachBinFiles         AS STRING

   VAR aAttachTextFiles        AS ARRAY
   VAR cAttachTextFiles        AS STRING

   VAR aAttachInlineFiles      AS ARRAY
   VAR cAttachInlineFiles      AS STRING

   VAR aHtmlEmbeddedFiles      AS ARRAY
   VAR cHtmlEmbeddedFiles      AS STRING                 // embed file(s) in HTML.  Object tag in HTML must specify content-id using cid: tag.  eg: <img src="cid:image.jpg">

   VAR cAttachListBinFile      AS STRING
   VAR cAttachListTextFile     AS STRING
   VAR cAttachListEmbeddedFile AS STRING

   VAR lSendBinaryBase64       AS LOGICAL INIT .F.
   VAR lSendBinaryUUEncoded    AS LOGICAL INIT .F.
   VAR lSendEnrichedText       AS LOGICAL INIT .F.
   VAR lUnicode                AS LOGICAL INIT .F.
   VAR lHtml                   AS LOGICAL INIT .F.
   VAR cAlternateText          AS STRING
   VAR cAlternateTextFile      AS STRING
   VAR lMime                   AS LOGICAL INIT .F.       // MIME Quoted-Printable Content-Transfer-Encoding
   VAR lAskFor8BitMime         AS LOGICAL INIT .F.       // ask for 8bit data support when sending MIME
   VAR nMultipartSize          AS NUMERIC                // send multipart messages, breaking attachments on <size> KB boundaries, where <size> is per 1000 bytes
   VAR lNoMultipartMessage     AS LOGICAL INIT .F.       // do not allow multipart messages
   VAR cContentType            AS STRING                 // use cContentType in the ContentType header for attachments that do not have a registered content type for the extension. For example: -contenttype "text/calendar"

   // NNTP specific options
   VAR cGroups                 AS STRING                 // list of newsgroups (comma separated)

   // Other options
   VAR lXtndXmit               AS LOGICAL INIT .F.       // Attempt to use POP3 to transmit when accessing POP3 first
   VAR lHelp                   AS LOGICAL INIT .F.       // displays this help (also -?, /?, -help or /help)
   VAR lQuiet                  AS LOGICAL INIT .T.       // suppresses all output to the screen
   VAR lDebug                  AS LOGICAL INIT .F.       // echoes server communications to a log file or screen (overrides -q if echoes to the screen)
   VAR cLogFile                AS STRING                 // log everything but usage to <file>
   VAR lLogTimestamp           AS LOGICAL INIT .F.       // when -log is used, a timestamp is added to each log line
   VAR lLogOverwrite           AS LOGICAL INIT .F.       // when -log is used, overwrite the log file
   VAR nTimeout                AS NUMERIC INIT 60        // set timeout to 'n' seconds.  Blat will wait 'n' seconds for server responses
   VAR nTry                    AS NUMERIC INIT 1         // -1 == INFINITE, how many times blat should try to send (1 to 'INFINITE')
   VAR lBinary                 AS LOGICAL INIT .F.       // do not convert ASCII | (pipe, 0x7c) to CrLf in the message body
   VAR cHostname               AS STRING                 // select the hostname used to send the message via SMTP this is typically your local machine name
   VAR lRaw                    AS LOGICAL INIT .F.       // do not add CR/LF after headers
   VAR nDelay                  AS NUMERIC INIT 1         // wait x seconds between messages being sent when used with -maxnames or -multipart
   VAR cCommentChar            AS STRING                 // use this character to mark the start of commments in options files and recipient list files.  The default is ;
   VAR lSuperDebug             AS LOGICAL INIT .F.       // hex/ascii dump the data between Blat and the server
   VAR lSuperDebugText         AS LOGICAL INIT .F.       // ascii dump the data between Blat and the server

   // Other
   VAR lIgnoreErrors           AS LOGICAL INIT .F.

   // Methods
   METHOD Send()
   METHOD Command( cCommand )  VIRTUAL
   METHOD Error()
   METHOD ErrorString()
   METHOD GetErrors()
   METHOD Check()
   METHOD BlatError()
   METHOD BlatErrorString()
   METHOD TranslateBlatError( nErr )
   METHOD GetCommand()
   METHOD GetVersion()         INLINE ::cVersion
   METHOD GetBlatVersion()     INLINE ::cBlatVersion

   PROTECTED:

   METHOD AddOptFile( cValue, cOption, nError )

ENDCLASS

METHOD Send() CLASS HBBlat

   ::Check()

   IF ::nError == BLAT_SUCCESS
      IF ( ::nBlatError := hb_blatSend( ::cCommand ) ) != 0
         ::nError := ::nBlatError
         ::cError := BLAT_TEXT_ERROR
      ELSE
         ::cError := BLAT_TEXT_SUCCESS
      ENDIF
      ::cBlatError := ::TranslateBlatError( ::nBlatError )
   ENDIF

   RETURN ::nError

METHOD PROCEDURE Check() CLASS HBBlat

   // Not using registry, so every parameter has to be explicity set
   // No control apart from existence

   IF ! ::lChecked

      ::cCommand := ""

      // --- The Basics ---

      IF ! HB_ISSTRING( ::cTO ) .AND. ! HB_ISSTRING( ::cTOFile ) .AND. ! ::lToUndiscloseRecipients
         ::nError := BLAT_ERR_MISSING_TO
         ::cError := ::TranslateBlatError( ::nError )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ELSEIF HB_ISSTRING( ::cTO )
         ::cCommand += " -to " + '"' + ::cTo + '"'
      ENDIF

      IF ! ::AddOptFile( ::cTOFile, "-tf", BLAT_ERR_MISSING_TOFILE )
         RETURN
      ENDIF

      IF HB_ISSTRING( ::cCC )
         ::cCommand += " -cc " + '"' + ::cCC + '"'
      ENDIF

      IF ! ::AddOptFile( ::cCCFile, "-cf", BLAT_ERR_MISSING_CCFILE )
         RETURN
      ENDIF

      IF HB_ISSTRING( ::cBCC )
         ::cCommand += " -bcc " + '"' + ::cBCC + '"'
      ENDIF

      IF ! ::AddOptFile( ::cBCCFile, "-bf", BLAT_ERR_MISSING_BCCFILE )
         RETURN
      ENDIF

      IF HB_ISNUMERIC( ::nMaxNames ) .AND. ::nMaxNames >= 1
         ::cCommand += " -maxNames " + hb_ntos( ::nMaxNames )
      ENDIF

      IF ::lToUndiscloseRecipients
         ::cCommand += " -ur"
      ENDIF

      IF HB_ISSTRING( ::cSubject )
         ::cCommand += " -subject " + '"' + ::cSubject + '"'
      ENDIF

      IF ::lSuppressSubject
         ::cCommand += " -ss"
      ENDIF

      IF ! ::AddOptFile( ::cSubjectFile, "-sf", BLAT_ERR_MISSING_SUBJECTFILE )
         RETURN
      ENDIF

      IF ! ::AddOptFile( ::cBodyFile, "-bodyf", BLAT_ERR_MISSING_BODYFILE )
         RETURN
      ENDIF

      IF ! HB_ISSTRING( ::cBody ) .AND. ! HB_ISSTRING( ::cBodyFile )
         ::nError := BLAT_ERR_MISSING_BODY
         ::cError := ::TranslateBlatError( ::nError )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ELSEIF HB_ISSTRING( ::cBody )
         ::cCommand += " -body " + '"' + ::cBody + '"'
      ELSEIF hb_vfExists( ::cBodyFile )
         ::cCommand += " -bodyF " + '"' + ::cBodyFile + '"'
      ELSE
         ::nError := BLAT_ERR_MISSING_BODYFILE
         ::cError := ::TranslateBlatError( ::nError ) + ": " + ::cBodyFile
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ENDIF

      IF ! ::AddOptFile( ::cSignatureFile, "-sig", BLAT_ERR_MISSING_SIGNATUREFILE )
         RETURN
      ENDIF

      IF ! ::AddOptFile( ::cTagFile, "-tag", BLAT_ERR_MISSING_TAGFILE )
         RETURN
      ENDIF

      IF ! ::AddOptFile( ::cPostScriptumFile, "-ps", BLAT_ERR_MISSING_PSFILE )
         RETURN
      ENDIF

      // --- Registry overrides ---

      IF HB_ISSTRING( ::cProfile )
         ::cCommand += " -p " + '"' + ::cProfile + '"'
      ENDIF

      IF ::lShowProfiles
         ::cCommand += " -profile"
      ENDIF

      IF HB_ISSTRING( ::cServerSMTP )
         ::cCommand += " -server " + ::cServerSMTP
      ELSE
         ::nError := BLAT_ERR_MISSING_SERVERSMTP
         ::cError := ::TranslateBlatError( ::nError )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ENDIF

      IF HB_ISSTRING( ::cServerNNTP )
         ::cCommand += " -serverNNTP " + ::cServerNNTP
      ENDIF

      IF HB_ISSTRING( ::cServerPOP3 )
         ::cCommand += " -serverPOP3 " + ::cServerPOP3
      ENDIF

      IF HB_ISSTRING( ::cServerIMAP )
         ::cCommand += " -serverIMAP " + ::cServerIMAP
      ENDIF

      IF HB_ISSTRING( ::cMailFrom )
         ::cCommand += " -mailfrom " + '"' + ::cMailFrom + '"'
      ENDIF

      IF HB_ISSTRING( ::cFrom )
         ::cCommand += " -f " + '"' + ::cFrom + '"'
      ELSE
         ::nError := BLAT_ERR_MISSING_FROM
         ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_FROM )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ENDIF

      IF HB_ISSTRING( ::cReplyTo )
         ::cCommand += " -replyto " + '"' + ::cReplyTo + '"'
      ENDIF

      IF HB_ISSTRING( ::cReturnPath )
         ::cCommand += " -returnpath " + '"' + ::cReturnPath + '"'
      ENDIF

      IF HB_ISSTRING( ::cSender )
         ::cCommand += " -sender " + '"' + ::cSender + '"'
      ENDIF

      IF HB_ISNUMERIC( ::nPortSMTP ) .AND. ::nPortSMTP >= 1
         ::cCommand += " -portSMTP " + hb_ntos( ::nPortSMTP )
      ENDIF

      IF HB_ISNUMERIC( ::nPortNNTP ) .AND. ::nPortNNTP >= 1
         ::cCommand += " -portNNTP " + hb_ntos( ::nPortNNTP )
      ENDIF

      IF HB_ISNUMERIC( ::nPortPOP3 ) .AND. ::nPortPOP3 >= 1
         ::cCommand += " -portPOP3 " + hb_ntos( ::nPortPOP3 )
      ENDIF

      IF HB_ISNUMERIC( ::nPortIMAP ) .AND. ::nPortIMAP >= 1
         ::cCommand += " -portIMAP " + hb_ntos( ::nPortIMAP )
      ENDIF

      IF HB_ISSTRING( ::cUserAUTH )
         ::cCommand += " -u " + '"' + ::cUserAUTH + '"'
      ENDIF

      IF HB_ISSTRING( ::cPasswordAUTH )
         ::cCommand += " -pw " + '"' + ::cPasswordAUTH + '"'
      ENDIF

      IF HB_ISSTRING( ::cUserPOP3 )
         ::cCommand += " -pu " + '"' + ::cUserPOP3 + '"'
      ENDIF

      IF HB_ISSTRING( ::cPasswordPOP3 )
         ::cCommand += " -ppw " + '"' + ::cPasswordPOP3 + '"'
      ENDIF

      IF HB_ISSTRING( ::cUserIMAP )
         ::cCommand += " -iu " + '"' + ::cUserIMAP + '"'
      ENDIF

      IF HB_ISSTRING( ::cPasswordIMAP )
         ::cCommand += " -ipw " + '"' + ::cPasswordIMAP + '"'
      ENDIF

      IF ::lNoMD5
         ::cCommand += " -nomd5"
      ENDIF

      // --- Miscellaneous RFC header switches ---

      IF HB_ISSTRING( ::cOrganization )
         ::cCommand += " -organization " + '"' + ::cOrganization + '"'
      ENDIF

      IF HB_ISSTRING( ::cUserAgent )
         ::cCommand += " -ua " + '"' + ::cUserAgent + '"'
      ENDIF

      IF HB_ISSTRING( ::cXHeader )
         ::cCommand += " -x " + '"' + ::cXHeader + '"'
      ENDIF

      IF ::lNoBlatXHeader
         ::cCommand += " -noh2"
      ENDIF

      // No Blat Home Page
      // Add only if ::lNoBlatXHeader is .F.
      IF ! ::lNoBlatXHeader
         IF ::lNoBlatHomePage
            ::cCommand += " -noh"
         ENDIF
      ENDIF

      IF ::lRequestDisposition
         ::cCommand += " -d"
      ENDIF

      IF ::lRequestReturnReceipt
         ::cCommand += " -r"
      ENDIF

      IF HB_ISSTRING( ::cCharSet )
         ::cCommand += " -charset " + ::cCharSet
      ENDIF

      IF HB_ISSTRING( ::cUserHeader1 )
         ::cCommand += " -a1 " + '"' + ::cUserHeader1 + '"'
      ENDIF

      IF HB_ISSTRING( ::cUserHeader2 )
         ::cCommand += " -a2 " + '"' + ::cUserHeader2 + '"'
      ENDIF

      IF HB_ISSTRING( ::cDSN )
         IF "n" $ Lower( ::cDSN ) .OR. ;
            "s" $ Lower( ::cDSN ) .OR. ;
            "f" $ Lower( ::cDSN ) .OR. ;
            "d" $ Lower( ::cDSN )
            ::cCommand += " -dsn " + ::cDSN
         ELSE
            ::nError := BLAT_ERR_WRONG_DSN
            ::cError := ::TranslateBlatError( ::nError ) + ": " + ::cDSN
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ENDIF
      ENDIF

      IF ::lEHBase64
         ::cCommand += " -hdrencb"
      ENDIF

      IF ::lEHQuoted
         ::cCommand += " -hdrencq"
      ENDIF

      // if both == .F., priority is standard
      DO CASE
      CASE ::lHighPriority
         ::cCommand += " -priority 1"
      CASE ::lLowPriority
         ::cCommand += " -priority 0"
      ENDCASE

      IF HB_ISNUMERIC( ::nSensitivity ) .AND. AScan( { 0, 1, 2 }, ::nSensitivity ) > 0
         ::cCommand += " -sensitivity " + hb_ntos( ::nSensitivity )
      ENDIF

      // --- Attachment and encoding options ---

      IF ! Empty( ::aAttachBinFiles )
         IF HB_ISSTRING( ::cAttachBinFiles )
            ::cAttachBinFiles += ","
         ELSE
            ::cAttachBinFiles := ""
         ENDIF
         ::cAttachBinFiles += ArrayToString( ::aAttachBinFiles )
      ENDIF
      IF HB_ISSTRING( ::cAttachBinFiles )
         ::cCommand += " -attach " + "'" + ::cAttachBinFiles + '"'
      ENDIF

      IF ! Empty( ::aAttachTextFiles )
         IF HB_ISSTRING( ::cAttachTextFiles )
            ::cAttachTextFiles += ","
         ELSE
            ::cAttachTextFiles := ""
         ENDIF
         ::cAttachTextFiles += ArrayToString( ::aAttachTextFiles )
      ENDIF
      IF HB_ISSTRING( ::cAttachTextFiles )
         ::cCommand += " -attacht " + '"' + ::cAttachTextFiles + '"'
      ENDIF

      IF ! Empty( ::aAttachInlineFiles )
         IF HB_ISSTRING( ::cAttachInlineFiles )
            ::cAttachInlineFiles += ","
         ELSE
            ::cAttachInlineFiles := ""
         ENDIF
         ::cAttachInlineFiles += ArrayToString( ::aAttachInlineFiles )
      ENDIF
      IF HB_ISSTRING( ::cAttachInlineFiles )
         ::cCommand += " -attachi " + '"' + ::cAttachInlineFiles + '"'
      ENDIF

      IF ! Empty( ::aHtmlEmbeddedFiles )
         IF HB_ISSTRING( ::cHtmlEmbeddedFiles )
            ::cHtmlEmbeddedFiles += ","
         ELSE
            ::cHtmlEmbeddedFiles := ""
         ENDIF
         ::cHtmlEmbeddedFiles += ArrayToString( ::aHtmlEmbeddedFiles )
      ENDIF
      IF HB_ISSTRING( ::cHtmlEmbeddedFiles )
         ::cCommand += " -embed " + '"' + ::cHtmlEmbeddedFiles + '"'
      ENDIF

      IF ! ::AddOptFile( ::cAttachListBinFile, "-af", BLAT_ERR_MISSING_ATTACHLISTBINFILE )
         RETURN
      ENDIF
      IF ! ::AddOptFile( ::cAttachListTextFile, "-atf", BLAT_ERR_MISSING_ATTACHLISTTEXTFILE )
         RETURN
      ENDIF
      IF ! ::AddOptFile( ::cAttachListEmbedded, "-aef", BLAT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE )
         RETURN
      ENDIF

      IF ::lSendBinaryBase64
         ::cCommand += " -base64"
      ENDIF

      IF ::lSendBinaryUUEncoded
         ::cCommand += " -uuencode"
      ENDIF

      IF ::lSendEnrichedText
         ::cCommand += " -enriched"
      ENDIF

      IF ::lUnicode
         ::cCommand += " -unicode"
      ENDIF

      IF ::lHtml
         ::cCommand += " -html"
      ENDIF

      IF HB_ISSTRING( ::cAlternateText )
         ::cCommand += " -alttext " + '"' + ::cAlternateText + '"'
      ENDIF

      IF ! ::AddOptFile( ::cAlternateTextFile, "-alttextf", BLAT_ERR_MISSING_ALTERNATETEXTFILE )
         RETURN
      ENDIF

      IF ::lMime
         ::cCommand += " -mime"
      ENDIF

      IF ::lAskFor8BitMime
         ::cCommand += " -8bitmime"
      ENDIF

      IF HB_ISNUMERIC( ::nMultipartSize ) .AND. ::nMultipartSize >= 1
         ::cCommand += " -multipart " + hb_ntos( ::nMultipartSize )
      ENDIF

      IF ::lNoMultipartMessage
         ::cCommand += " -nomps"
      ENDIF

      IF HB_ISSTRING( ::cContentType )
         ::cCommand += " -contentType " + ::cContentType
      ENDIF

      // --- NNTP specific options ---

      IF HB_ISSTRING( ::cGroups )
         ::cCommand += " -groups " + '"' + ::cGroups + '"'
      ENDIF

      // --- Other options ---

      IF ::lXtndXmit
         ::cCommand += " -xtndxmit"
      ENDIF

      IF ::lQuiet
         ::cCommand += " -q"
      ENDIF

      IF ::lDebug
         ::cCommand += " -debug"
      ENDIF

      IF HB_ISSTRING( ::cLogFile )
         ::cCommand += " -log " + '"' + ::cLogFile + '"'
      ENDIF

      IF ::lLogTimestamp
         ::cCommand += " -timestamp"
      ENDIF

      IF ::lLogOverwrite
         ::cCommand += " -overwritelog"
      ENDIF

      IF HB_ISNUMERIC( ::nTimeout ) .AND. ::nTimeout >= 1
         ::cCommand += " -ti " + hb_ntos( ::nTimeout )
      ENDIF

      IF HB_ISNUMERIC( ::nTry )
         IF ::nTry == BLAT_TRY_INFINITE_KEY
            ::cCommand += " -try " + BLAT_TRY_INFINITE_VALUE
         ELSEIF ::nTry >= 0
            ::cCommand += " -try " + hb_ntos( ::nTry )
         ENDIF
      ENDIF

      IF ::lBinary
         ::cCommand += " -binary"
      ENDIF

      IF HB_ISSTRING( ::cHostname )
         ::cCommand += " -hostname " + '"' + ::cHostname + '"'
      ENDIF

      IF ::lRaw
         ::cCommand += " -raw"
      ENDIF

      IF HB_ISNUMERIC( ::nDelay ) .AND. ::nDelay >= 1
         ::cCommand += " -delay " + hb_ntos( ::nDelay )
      ENDIF

      IF HB_ISSTRING( ::cCommentChar )
         ::cCommand += " -comment " + '"' + ::cCommentChar + '"'
      ENDIF

      IF ::lSuperDebug
         ::cCommand += " -superdebug"
      ENDIF

      IF ::lSuperDebugText
         ::cCommand += " -superdebugT"
      ENDIF

      ::lChecked := .T.
   ENDIF

   RETURN

METHOD AddOptFile( cValue, cOption, nError ) CLASS HBBlat

   IF HB_ISSTRING( cValue )
      IF hb_vfExists( cValue )
         ::cCommand += " " + cOption + " " + '"' + cValue + '"'
      ELSE
         ::nError := nError
         ::cError := ::TranslateBlatError( ::nError ) + ": " + cValue
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN .F.
         ENDIF
      ENDIF
   ENDIF

   RETURN .T.

METHOD GetCommand() CLASS HBBlat

   IF ! ::lChecked
      ::Check()
   ENDIF

   RETURN ::cCommand

METHOD Error() CLASS HBBlat
   RETURN ::nError

METHOD ErrorString() CLASS HBBlat
   RETURN ::cError

METHOD GetErrors() CLASS HBBlat
   RETURN ::aErrors

METHOD BlatError() CLASS HBBlat
   RETURN ::nBlatError

METHOD BlatErrorString() CLASS HBBlat
   RETURN ::cBlatError

METHOD TranslateBlatError( nErr ) CLASS HBBlat

   LOCAL nPos

   /* BLAT_TEXT_ERR_UNKNOWN has to be first error */
   /* TODO: add function that returns language error array */
   LOCAL aErrors := { ;
      { BLAT_ERR_UNKNOWN                        , BLAT_TEXT_ERR_UNKNOWN                        }, ;
      { BLAT_SUCCESS                            , BLAT_TEXT_SUCCESS                            }, ;
      { BLAT_ERR_MESSAGE_NOT_ACCEPTED           , BLAT_TEXT_ERR_MESSAGE_NOT_ACCEPTED           }, ;
      { BLAT_ERR_MISSING_FROM                   , BLAT_TEXT_ERR_MISSING_FROM                   }, ;
      { BLAT_ERR_MISSING_TO                     , BLAT_TEXT_ERR_MISSING_TO                     }, ;
      { BLAT_ERR_MISSING_TOFILE                 , BLAT_TEXT_ERR_MISSING_TOFILE                 }, ;
      { BLAT_ERR_MISSING_BODY                   , BLAT_TEXT_ERR_MISSING_BODY                   }, ;
      { BLAT_ERR_MISSING_BODYFILE               , BLAT_TEXT_ERR_MISSING_BODYFILE               }, ;
      { BLAT_ERR_MISSING_SERVERSMTP             , BLAT_TEXT_ERR_MISSING_SERVERSMTP             }, ;
      { BLAT_ERR_MISSING_SUBJECTFILE            , BLAT_TEXT_ERR_MISSING_SUBJECTFILE            }, ;
      { BLAT_ERR_MISSING_CCFILE                 , BLAT_TEXT_ERR_MISSING_CCFILE                 }, ;
      { BLAT_ERR_MISSING_BCCFILE                , BLAT_TEXT_ERR_MISSING_BCCFILE                }, ;
      { BLAT_ERR_MISSING_PSFILE                 , BLAT_TEXT_ERR_MISSING_PSFILE                 }, ;
      { BLAT_ERR_MISSING_ATTACHLISTBINFILE      , BLAT_TEXT_ERR_MISSING_ATTACHLISTBINFILE      }, ;
      { BLAT_ERR_MISSING_ATTACHLISTTEXTFILE     , BLAT_TEXT_ERR_MISSING_ATTACHLISTTEXTFILE     }, ;
      { BLAT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE , BLAT_TEXT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE }, ;
      { BLAT_ERR_MISSING_ALTERNATETEXTFILE      , BLAT_TEXT_ERR_MISSING_ALTERNATETEXTFILE      }, ;
      { BLAT_ERR_MISSING_SIGNATUREFILE          , BLAT_TEXT_ERR_MISSING_SIGNATUREFILE          }, ;
      { BLAT_ERR_MISSING_TAGFILE                , BLAT_TEXT_ERR_MISSING_TAGFILE                }, ;
      { BLAT_ERR_WRONG_DSN                      , BLAT_TEXT_ERR_WRONG_DSN                      }, ;
      { BLAT_ERR_LOGICAL_EXPECTED               , BLAT_TEXT_ERR_LOGICAL_EXPECTED               }, ;
      { BLAT_ERR_STRING_EXPECTED                , BLAT_TEXT_ERR_STRING_EXPECTED                } }

   IF ( nPos := AScan( aErrors, {| e | e[ 1 ] == nErr }, 2 ) ) == 0
      nPos := 1
   ENDIF

   RETURN aErrors[ nPos ][ 2 ]

STATIC FUNCTION ArrayToString( aArray )

   LOCAL cString := ""
   LOCAL cElem

   FOR EACH cElem IN aArray
      cString += ;
         iif( '"' $ cElem, "'" + cElem + "'", '"' + cElem + '"' ) + ;
         iif( cElem:__enumIsLast(), "", "," )
   NEXT

   RETURN cString
