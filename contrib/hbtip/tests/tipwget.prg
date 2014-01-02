/*
 * TEST of TIP libs (for higher level URI interface)
 *
 * Usage: This file is similar to a wget command
 *
 * Without the filename, tipwget will be in demo mode,
 * just demostrating it is working
 *
 * With the filename, data will be stored to the file or
 * retrieved from the file and sent to Internet.
 *
 * Usage of URI.
 * HTTP Protocol
 *   http://<sitename>/<path>?<query>
 *   - at the moment HTTP URI is not able to send data,
 *     (f.e. a form)
 *
 * POP Protocol
 *    pop://<username>:<password>@<popserver>/[-][MsgNum]
 *    - Without MsgNum, you get the list of messages
 *    - With MsgNum get Message MsgNum
 *    - With -MsgNum deletes message MsgNum
 *
 * SMTP Protocol
 *    smtp://<mail-from>@<server>/RCPT
 *    - (You have to provide a filename)
 *    - use &at; in mail-from message
 *    - Send the mail in filename (that must include
 *      headers) to RCPT f.e.
 *      stmp://user&at;example.com@smtp.example.com/gian@niccolai.ws
 *
 *      NOTE: In Unix, to use '&' from command line you have to surround
 *      the URL with "", f.e. "smtp://...&at;...@server/dest"
 *
 * FTP Protocol
 *    ftp://user:passwd@<ftpserver>/[<path>]
 *    - without path, get the list of files (use path/ to get the list of
 *      files in a dir.
 *    - with path, get a file. If the target file (second param) starts with '+'
 *      it will be sent instead of being retrieved.
 */

#require "hbssl"
#require "hbtip"

REQUEST __HBEXTERN__HBSSL__

#include "hbclass.ch"
#include "tip.ch"

PROCEDURE Main( cUrl, cFile )

   LOCAL bWrite := .F.
   LOCAL oUrl, oClient
   LOCAL cData

   ? "Harbour - TIP (class based internet client protocol) test"

   IF ! HB_ISSTRING( cUrl ) .OR. Empty( cUrl )
      ? hb_StrFormat( "Usage: %1$s <URI> [dumpToOrFromFileName]", hb_ProgName() )
      RETURN
   ENDIF

   oUrl := TUrl():New( cUrl )
   IF Empty( oUrl )
      ? "Invalid URL", cUrl
      RETURN
   ENDIF

   SWITCH Lower( oUrl:cProto )
   CASE "ftp"
      oClient := TIPClientFTP():New( oUrl )
      EXIT
   CASE "http"
   CASE "https"
      oClient := TIPClientHTTP():New( oUrl )
      EXIT
   CASE "pop"
   CASE "pops"
      oClient := TIPClientPOP():New( oUrl )
      EXIT
   CASE "smtp"
   CASE "smtps"
      oClient := TIPClientSMTP():New( oUrl )
      EXIT
   ENDSWITCH

   IF Empty( oClient )
      ? "Invalid URL", cUrl
      RETURN
   ENDIF
   oClient:nConnTimeout := 2000 /* 20000 */

   oUrl:cUserid := StrTran( oUrl:cUserid, "&at;", "@" )

   ? "Connecting to", oUrl:cProto + "://" + oUrl:cServer
   IF oClient:Open()
      IF Empty( oClient:cReply )
         ? "Connection status: <connected>"
      ELSE
         ? "Connection status:", oClient:cReply
      ENDIF

      IF ! Empty( cFile ) .AND. Left( cFile, 1 ) == "+"
         cFile := SubStr( cFile, 2 )
         bWrite := .T.
      ENDIF

      ?
      oClient:exGauge := {| done, size | ShowGauge( done, size ) }
#if 0
      /* Can be also: */
      oClient:exGauge := {| done, size, oConnection | dothing( done, size, oConnection ) }
#endif

      IF oClient:nAccessMode == TIP_WO .OR. ( oClient:nAccessMode == TIP_RW .AND. bWrite )
         IF oClient:WriteFromFile( cFile )
            ? "Data successfully sent"
         ELSE
            ? "Error: Data not sent", oClient:lastErrorMessage()
         ENDIF
      ELSE
         IF Empty( cFile )
            cData := oClient:Read()
            IF ! Empty( cData )
               ? "First 80 characters:", RTrim( Left( cData, 80 ) )
            ELSE
               ? "Error: file can't be retrieved", oClient:lastErrorMessage()
            ENDIF
         ELSE
            IF oClient:ReadToFile( cFile )
               ? "File", cFile, "written."
               ? "Server replied", oClient:cReply
            ELSE
               ? "Error: Generic error in writing", cFile
            ENDIF
         ENDIF
      ENDIF

      oClient:Close()
      IF Empty( oClient:cReply )
         ? "Done: (no goodbye message)"
      ELSE
         ? "Done:", oClient:cReply
      ENDIF
   ELSE
      ? "Can't open URI", cUrl
      IF ! Empty( oClient:cReply )
         ? oClient:cReply
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE ShowGauge( nSent, nSize )

   hb_default( @nSize, 0 )

   SetPos( Row(), 0 )
   ?? "Sending:", nSent, "/", nSize

   RETURN
