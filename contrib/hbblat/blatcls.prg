/*
 * Harbour Project source code:
 * BLAT wrapper library interface code.
 *
 * Copyright 2007-2012 Francesco Saverio Giudice <info@fsgiudice.com>
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
   //VAR lInstall                AS LOGICAL INIT .F.
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
   VAR cDSN                    AS STRING                 // Delivery Status Notifications (RFC 3461): n = never, s = successful, f = failure, d = delayed - can be used together, however N takes precedence
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

ENDCLASS

METHOD Send() CLASS HBBlat

   ::Check()

   IF ::nError == BLAT_SUCCESS
      ::nBlatError := hb_blatSend( ::cCommand )
      IF ::nBlatError != 0
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

      // The Basics ----------------------------------

      // to
      IF ! HB_ISSTRING( ::cTO ) .AND. ! HB_ISSTRING( ::cTOFile ) .AND. ! ::lToUndiscloseRecipients
         ::nError := BLAT_ERR_MISSING_TO
         ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_TO )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ELSE
         IF HB_ISSTRING( ::cTO )
            ::cCommand += " -to " + ::cTo
         ENDIF
      ENDIF

      // tofile - optional
      IF HB_ISSTRING( ::cTOFile )
         IF ! hb_FileExists( ::cTOFile )
            ::nError := BLAT_ERR_MISSING_TOFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_TOFILE ) + ": " + ::cToFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -tf " + ::cToFile
         ENDIF
      ENDIF

      // cc - optional
      IF HB_ISSTRING( ::cCC )
         ::cCommand += " -cc " + ::cCC
      ENDIF

      // ccfile - optional
      IF HB_ISSTRING( ::cCCFile )
         IF ! hb_FileExists( ::cCCFile )
            ::nError := BLAT_ERR_MISSING_CCFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_CCFILE ) + ": " + ::cCCFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -cf " + ::cCCFile
         ENDIF
      ENDIF

      // bcc - optional
      IF HB_ISSTRING( ::cBCC )
         ::cCommand += " -bcc " + ::cBCC
      ENDIF

      // bccfile - optional
      IF HB_ISSTRING( ::cBCCFile )
         IF ! hb_FileExists( ::cBCCFile )
            ::nError := BLAT_ERR_MISSING_BCCFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_BCCFILE ) + ": " + ::cBCCFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -bf " + ::cBCCFile
         ENDIF
      ENDIF

      // nMaxNames
      IF HB_ISNUMERIC( ::nMaxNames ) .AND. ::nMaxNames > 0
         ::cCommand += " -maxNames " + hb_ntos( ::nMaxNames )
      ENDIF

      // lToUndiscloseRecipients
      IF ::lToUndiscloseRecipients
         ::cCommand += " -ur"
      ENDIF

      // cSubject - optional
      IF HB_ISSTRING( ::cSubject )
         ::cCommand += ' -subject "' + ::cSubject + '"'
      ENDIF

      // lSuppressSubject
      IF ::lSuppressSubject
         ::cCommand += " -ss"
      ENDIF

      // cSubjectFile - optional
      IF HB_ISSTRING( ::cSubjectFile )
         IF ! hb_FileExists( ::cSubjectFile )
            ::nError := BLAT_ERR_MISSING_SUBJECTFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_SUBJECTFILE ) + ": " + ::cSubjectFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -sf " + ::cSubjectFile
         ENDIF
      ENDIF

      // bodyfile - optional
      IF HB_ISSTRING( ::cBodyFile )
         IF ! hb_FileExists( ::cBodyFile )
            ::nError := BLAT_ERR_MISSING_BODYFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_BODYFILE ) + ": " + ::cBodyFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -bodyF " + ::cBodyFile
         ENDIF
      ENDIF

      // body
      IF ! HB_ISSTRING( ::cBody ) .AND. ! HB_ISSTRING( ::cBodyFile )
         ::nError := BLAT_ERR_MISSING_BODY
         ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_BODY )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ELSE
         IF HB_ISSTRING( ::cBody )
            ::cCommand += ' -body "' + ::cBody + '"'
         ELSE
            IF ! hb_FileExists( ::cBodyFile )
               ::nError := BLAT_ERR_MISSING_BODYFILE
               ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_BODYFILE ) + ": " + ::cBodyFile
               AAdd( ::aErrors, { ::nError, ::cError } )
               IF ! ::lIgnoreErrors
                  RETURN
               ENDIF
            ELSE
               ::cCommand += " -bodyF " + ::cBodyFile
            ENDIF
         ENDIF
      ENDIF

      // cSignatureFile
      IF HB_ISSTRING( ::cSignatureFile )
         IF ! hb_FileExists( ::cSignatureFile )
            ::nError := BLAT_ERR_MISSING_SIGNATUREFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_SIGNATUREFILE ) + ": " + ::cSignatureFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -sig " + ::cSignatureFile
         ENDIF
      ENDIF

      // cTagFile
      IF HB_ISSTRING( ::cTagFile )
         IF ! hb_FileExists( ::cTagFile )
            ::nError := BLAT_ERR_MISSING_TAGFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_TAGFILE ) + ": " + ::cTagFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -tag " + ::cTagFile
         ENDIF
      ENDIF

      // cPostScriptumFile - optional
      IF HB_ISSTRING( ::cPostScriptumFile )
         IF ! hb_FileExists( ::cPostScriptumFile )
            ::nError := BLAT_ERR_MISSING_PSFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_PSFILE ) + ": " + ::cPostScriptumFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -ps " + ::cPostScriptumFile
         ENDIF
      ENDIF

      // Registry overrides ------------------------------

      // cProfile
      IF HB_ISSTRING( ::cProfile )
         ::cCommand += " -p " + ::cProfile
      ENDIF

      // lShowProfiles
      IF ::lShowProfiles
         ::cCommand += " -profile"
      ENDIF

      // cServerSMTP
      IF ! HB_ISSTRING( ::cServerSMTP )
         ::nError := BLAT_ERR_MISSING_SERVERSMTP
         ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_SERVERSMTP )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ELSE
         ::cCommand += " -server " + ::cServerSMTP
      ENDIF

      // cServerNNTP
      IF HB_ISSTRING( ::cServerNNTP )
         ::cCommand += " -serverNNTP " + ::cServerNNTP
      ENDIF

      // cServerPOP3
      IF HB_ISSTRING( ::cServerPOP3 )
         ::cCommand += " -serverPOP3 " + ::cServerPOP3
      ENDIF

      // cServerIMAP
      IF HB_ISSTRING( ::cServerIMAP )
         ::cCommand += " -serverIMAP " + ::cServerIMAP
      ENDIF

      // cMailFrom
      IF HB_ISSTRING( ::cMailFrom )
         ::cCommand += " -mailfrom " + ::cMailFrom
      ENDIF

      // cFrom
      IF ! HB_ISSTRING( ::cFrom )
         ::nError := BLAT_ERR_MISSING_FROM
         ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_FROM )
         AAdd( ::aErrors, { ::nError, ::cError } )
         IF ! ::lIgnoreErrors
            RETURN
         ENDIF
      ELSE
         ::cCommand += " -f " + ::cFrom
      ENDIF

      // cReplyTo
      IF HB_ISSTRING( ::cReplyTo )
         ::cCommand += " -replyto " + ::cReplyTo
      ENDIF

      // cReplyTo
      IF HB_ISSTRING( ::cReplyTo )
         ::cCommand += " -replyto " + ::cReplyTo
      ENDIF

      // cReturnPath
      IF HB_ISSTRING( ::cReturnPath )
         ::cCommand += " -returnpath " + ::cReturnPath
      ENDIF

      // cSender
      IF HB_ISSTRING( ::cSender )
         ::cCommand += " -sender " + ::cSender
      ENDIF

      // nPortSMTP
      IF HB_ISNUMERIC( ::nPortSMTP ) .AND. ::nPortSMTP > 0
         ::cCommand += " -portSMTP " + hb_ntos( ::nPortSMTP )
      ENDIF

      // nPortNNTP
      IF HB_ISNUMERIC( ::nPortNNTP ) .AND. ::nPortNNTP > 0
         ::cCommand += " -portNNTP " + hb_ntos( ::nPortNNTP )
      ENDIF

      // nPortPOP3
      IF HB_ISNUMERIC( ::nPortPOP3 ) .AND. ::nPortPOP3 > 0
         ::cCommand += " -portPOP3 " + hb_ntos( ::nPortPOP3 )
      ENDIF

      // nPortIMAP
      IF HB_ISNUMERIC( ::nPortIMAP ) .AND. ::nPortIMAP > 0
         ::cCommand += " -portIMAP " + hb_ntos( ::nPortIMAP )
      ENDIF

      // cUserAUTH
      IF HB_ISSTRING( ::cUserAUTH )
         ::cCommand += " -u " + ::cUserAUTH
      ENDIF

      // cPasswordAUTH
      IF HB_ISSTRING( ::cPasswordAUTH )
         ::cCommand += " -pw " + ::cPasswordAUTH
      ENDIF

      // cUserPOP3
      IF HB_ISSTRING( ::cUserPOP3 )
         ::cCommand += " -pu " + ::cUserPOP3
      ENDIF

      // cPasswordPOP3
      IF HB_ISSTRING( ::cPasswordPOP3 )
         ::cCommand += " -ppw " + ::cPasswordPOP3
      ENDIF

      // cUserIMAP
      IF HB_ISSTRING( ::cUserIMAP )
         ::cCommand += " -iu " + ::cUserIMAP
      ENDIF

      // cPasswordIMAP
      IF HB_ISSTRING( ::cPasswordIMAP )
         ::cCommand += " -ipw " + ::cPasswordIMAP
      ENDIF

      // lNoMD5
      IF ::lNoMD5
         ::cCommand += " -nomd5"
      ENDIF

      // Miscellaneous RFC header switches ----------------------

      // cOrganization
      IF HB_ISSTRING( ::cOrganization )
         ::cCommand += " -organization " + ::cOrganization
      ENDIF

      // cUserAgent
      IF HB_ISSTRING( ::cUserAgent )
         ::cCommand += " -ua " + ::cUserAgent
      ENDIF

      // cXHeader
      IF HB_ISSTRING( ::cXHeader )
         ::cCommand += " -x " + ::cXHeader
      ENDIF

      // NoBlatHeader
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

      // lRequestDisposition
      IF ::lRequestDisposition
         ::cCommand += " -d"
      ENDIF

      // lRequestReturnReceipt
      IF ::lRequestReturnReceipt
         ::cCommand += " -r"
      ENDIF

      // cCharSet
      IF HB_ISSTRING( ::cCharSet )
         ::cCommand += " -charset " + ::cCharSet
      ENDIF

      // cUserHeader1
      IF HB_ISSTRING( ::cUserHeader1 )
         ::cCommand += " -a1 " + ::cUserHeader1
      ENDIF

      // cUserHeader2
      IF HB_ISSTRING( ::cUserHeader2 )
         ::cCommand += " -a2 " + ::cUserHeader2
      ENDIF

      // cDSN
      IF HB_ISSTRING( ::cDSN )
         IF "n" $ Lower( ::cDSN ) .OR. "s" $ Lower( ::cDSN ) .OR. ;
            "f" $ Lower( ::cDSN ) .OR. "d" $ Lower( ::cDSN )
            ::cCommand += " -dsn " + ::cDSN
         ELSE
            ::nError := BLAT_ERR_WRONG_DSN
            ::cError := ::TranslateBlatError( BLAT_ERR_WRONG_DSN ) + ": " + ::cDSN
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ENDIF
      ENDIF

      // lEHBase64
      IF ::lEHBase64
         ::cCommand += " -hdrencb"
      ENDIF

      // lEHQuoted
      IF ::lEHQuoted
         ::cCommand += " -hdrencq"
      ENDIF

      // lHighPriority / lLowPriority - if both == .F., priority is standard
      IF ::lHighPriority
         ::cCommand += " -priority 1"
      ELSEIF ::lLowPriority
         ::cCommand += " -priority 0"
      ENDIF

      // nSensitivity
      IF HB_ISNUMERIC( ::nSensitivity ) .AND. AScan( { 0, 1, 2 }, ::nSensitivity ) > 0
         ::cCommand += " -sensitivity " + hb_ntos( ::nSensitivity )
      ENDIF

      // Attachment and encoding options --------

      // Attach Binary Files
      IF ! Empty( ::aAttachBinFiles )
         IF HB_ISSTRING( ::cAttachBinFiles )
            ::cAttachBinFiles += ","
         ELSE
            ::cAttachBinFiles := ""
         ENDIF
         ::cAttachBinFiles += ArrayToString( ::aAttachBinFiles )
      ENDIF
      IF HB_ISSTRING( ::cAttachBinFiles )
         ::cCommand += " -attach " + ::cAttachBinFiles
      ENDIF

      // Attach Text Files
      IF ! Empty( ::aAttachTextFiles )
         IF HB_ISSTRING( ::cAttachTextFiles )
            ::cAttachTextFiles += ","
         ELSE
            ::cAttachTextFiles := ""
         ENDIF
         ::cAttachTextFiles += ArrayToString( ::aAttachTextFiles )
      ENDIF
      IF HB_ISSTRING( ::cAttachTextFiles )
         ::cCommand += " -attacht " + ::cAttachTextFiles
      ENDIF

      // Attach INLINE Files
      IF ! Empty( ::aAttachInlineFiles )
         IF HB_ISSTRING( ::cAttachInlineFiles )
            ::cAttachInlineFiles += ","
         ELSE
            ::cAttachInlineFiles := ""
         ENDIF
         ::cAttachInlineFiles += ArrayToString( ::aAttachInlineFiles )
      ENDIF
      IF HB_ISSTRING( ::cAttachInlineFiles )
         ::cCommand += " -attachi " + ::cAttachInlineFiles
      ENDIF

      // Attach Html Embedded Files
      IF ! Empty( ::aHtmlEmbeddedFiles )
         IF HB_ISSTRING( ::cHtmlEmbeddedFiles )
            ::cHtmlEmbeddedFiles += ","
         ELSE
            ::cHtmlEmbeddedFiles := ""
         ENDIF
         ::cHtmlEmbeddedFiles += ArrayToString( ::aHtmlEmbeddedFiles )
      ENDIF
      IF HB_ISSTRING( ::cHtmlEmbeddedFiles )
         ::cCommand += " -embed " + ::cHtmlEmbeddedFiles
      ENDIF

      // cAttachListBinFile - optional
      IF HB_ISSTRING( ::cAttachListBinFile )
         IF ! hb_FileExists( ::cAttachListBinFile )
            ::nError := BLAT_ERR_MISSING_ATTACHLISTBINFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_ATTACHLISTBINFILE ) + ": " + ::cAttachListBinFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -af " + ::cAttachListBinFile
         ENDIF
      ENDIF

      // cAttachListTextFile - optional
      IF HB_ISSTRING( ::cAttachListTextFile )
         IF ! hb_FileExists( ::cAttachListTextFile )
            ::nError := BLAT_ERR_MISSING_ATTACHLISTTEXTFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_ATTACHLISTTEXTFILE ) + ": " + ::cAttachListTextFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -atf " + ::cAttachListTextFile
         ENDIF
      ENDIF

      // cAttachListEmbeddedFile - optional
      IF HB_ISSTRING( ::cAttachListEmbeddedFile )
         IF ! hb_FileExists( ::cAttachListEmbeddedFile )
            ::nError := BLAT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_ATTACHLISTEMBEDDEDFILE ) + ": " + ::cAttachListEmbeddedFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -aef " + ::cAttachListEmbeddedFile
         ENDIF
      ENDIF

      // lSendBinaryBase64 - optional
      IF ::lSendBinaryBase64
         ::cCommand += " -base64"
      ENDIF

      // lSendBinaryUUEncoded - optional
      IF ::lSendBinaryUUEncoded
         ::cCommand += " -uuencode"
      ENDIF

      // lSendEnrichedText - optional
      IF ::lSendEnrichedText
         ::cCommand += " -enriched"
      ENDIF

      // lUnicode - optional
      IF ::lUnicode
         ::cCommand += " -unicode"
      ENDIF

      // lHtml - optional
      IF ::lHtml
         ::cCommand += " -html"
      ENDIF

      // cAlternateText - optional
      IF HB_ISSTRING( ::cAlternateText )
         ::cCommand += " -alttext " + ::cAlternateText
      ENDIF

      // cAlternateTextFile - optional
      IF HB_ISSTRING( ::cAlternateTextFile )
         IF ! hb_FileExists( ::cAlternateTextFile )
            ::nError := BLAT_ERR_MISSING_ALTERNATETEXTFILE
            ::cError := ::TranslateBlatError( BLAT_ERR_MISSING_ALTERNATETEXTFILE ) + ": " + ::cAlternateTextFile
            AAdd( ::aErrors, { ::nError, ::cError } )
            IF ! ::lIgnoreErrors
               RETURN
            ENDIF
         ELSE
            ::cCommand += " -alttextf " + ::cAlternateTextFile
         ENDIF
      ENDIF

      // lMime - optional
      IF ::lMime
         ::cCommand += " -mime"
      ENDIF

      // lAskFor8BitMime - optional
      IF ::lAskFor8BitMime
         ::cCommand += " -8bitmime"
      ENDIF

      // nMultipartSize - optional
      IF HB_ISNUMERIC( ::nMultipartSize ) .AND. ::nMultipartSize > 0
         ::cCommand += " -multipart " + hb_ntos( ::nMultipartSize )
      ENDIF

      // lNoMultipartMessage - optional
      IF ::lNoMultipartMessage
         ::cCommand += " -nomps"
      ENDIF

      // cContentType - optional
      IF HB_ISSTRING( ::cContentType )
         ::cCommand += " -contentType " + ::cContentType
      ENDIF

      // NNTP specific options ------------------

      // cGroups - optional
      IF HB_ISSTRING( ::cGroups )
         ::cCommand += " -groups " + ::cGroups
      ENDIF

      // Other options --------------------------

      // lXtndXmit
      IF ::lXtndXmit
         ::cCommand += " -xtndxmit"
      ENDIF

#if 0
      /* NOT IMPLEMENTED */
      // lHelp
      IF ::lHelp
         ::cCommand += " -h"
      ENDIF
#endif

      // lQuiet
      IF ::lQuiet
         ::cCommand += " -q"
      ENDIF

      // lDebug
      IF ::lDebug
         ::cCommand += " -debug"
      ENDIF

      // cLogFile
      IF HB_ISSTRING( ::cLogFile )
         ::cCommand += " -log " + ::cLogFile
      ENDIF

      // lLogTimestamp
      IF ::lLogTimestamp
         ::cCommand += " -timestamp"
      ENDIF

      // lLogOverwrite
      IF ::lLogOverwrite
         ::cCommand += " -overwritelog"
      ENDIF

      // nTimeout
      IF HB_ISNUMERIC( ::nTimeout ) .AND. ::nTimeout > 0
         ::cCommand += " -ti " + hb_ntos( ::nTimeout )
      ENDIF

      // nTry
      IF HB_ISNUMERIC( ::nTry )
         IF ::nTry == BLAT_TRY_INFINITE_KEY
            ::cCommand += " -try " + BLAT_TRY_INFINITE_VALUE
         ELSEIF ::nTry >= 0
            ::cCommand += " -try " + hb_ntos( ::nTry )
         ENDIF
      ENDIF

      // lBinary
      IF ::lBinary
         ::cCommand += " -binary"
      ENDIF

      // cHostname
      IF HB_ISSTRING( ::cHostname )
         ::cCommand += " -hostname " + ::cHostname
      ENDIF

      // lRaw
      IF ::lRaw
         ::cCommand += " -raw"
      ENDIF

      // nDelay
      IF HB_ISNUMERIC( ::nDelay ) .AND. ::nDelay > 0
         ::cCommand += " -delay " + hb_ntos( ::nDelay )
      ENDIF

      // cCommentChar
      IF HB_ISSTRING( ::cCommentChar )
         ::cCommand += " -comment " + ::cCommentChar
      ENDIF

      // lSuperDebug
      IF ::lSuperDebug
         ::cCommand += " -superdebug"
      ENDIF

      // lSuperDebugT
      IF ::lSuperDebugText
         ::cCommand += " -superdebugT"
      ENDIF

      // Check done
      ::lChecked := .T.
   ENDIF

   RETURN

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
      { BLAT_ERR_UNKNONW                        , BLAT_TEXT_ERR_UNKNOWN                        }, ;
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

   nPos := AScan( aErrors, {| e | e[ 1 ] == nErr }, 2 )
   IF nPos == 0
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
