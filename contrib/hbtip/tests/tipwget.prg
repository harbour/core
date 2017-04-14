/* TEST of TIP libs (for higher level URI interface)
 *
 * Usage: This file is similar to a wget command
 *
 * Without the filename, tipwget will be in demo mode,
 * just demostrating it is working
 *
 * With the filename, data will be stored to the file or
 * retrieved from the file and sent to internet.
 *
 * Usage of URI.
 * HTTP[S] Protocol
 *   http[s]://<sitename>/<path>?<query>
 *   - at the moment HTTP URI is not able to send data,
 *     (f.e. a form)
 *
 * POP[S] Protocol
 *    pop[s]://<username>:<password>@<popserver>/[-][MsgNum]
 *    - Without MsgNum, you get the list of messages
 *    - With MsgNum get Message MsgNum
 *    - With -MsgNum deletes message MsgNum
 *
 * SMTP[S] Protocol
 *    smtp[s]://<mail-from>@<server>/RCPT
 *    - (You have to provide a filename)
 *    - use &at; in mail-from message
 *    - Send the mail in filename (that must include
 *      headers) to RCPT f.e.
 *      stmp[s]://user&at;example.com@smtp.example.com/gian@niccolai.ws
 *
 *      NOTE: In Unix, to use '&' from command-line you have to surround
 *      the URL with "", f.e. "smtp[s]://...&at;...@server/dest"
 *
 * FTP[S] Protocol
 *    ftp[s]://user:passwd@<ftpserver>/[<path>]
 *    - without path, get the list of files (use path/ to get the list of
 *      files in a dir.
 *    - with path, get a file. If the target file (second param) starts with '+'
 *      it will be sent instead of being retrieved.
 */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

#include "hbclass.ch"
#include "inkey.ch"
#include "tip.ch"

PROCEDURE Main( cURL, cFile )

   LOCAL bWrite := .F.
   LOCAL oURL, oClient
   LOCAL cData

   ? "Harbour - TIP (class based internet client protocol) test"

   IF ! HB_ISSTRING( cURL ) .OR. Empty( cURL )
      ? hb_StrFormat( "Usage: %1$s <URI> [dumpToOrFromFileName]", hb_ProgName() )
      RETURN
   ENDIF

   IF Empty( oURL := TUrl():New( cURL ) )
      ? "Invalid URL", cURL
      RETURN
   ENDIF

   SWITCH Lower( oURL:cProto )
   CASE "ftp"
      oClient := TIPClientFTP():New( oURL )
      EXIT
   CASE "http"
   CASE "https"
      oClient := TIPClientHTTP():New( oURL )
      EXIT
   CASE "pop"
   CASE "pops"
      oClient := TIPClientPOP():New( oURL )
      EXIT
   CASE "smtp"
   CASE "smtps"
      oClient := TIPClientSMTP():New( oURL )
      EXIT
   ENDSWITCH

   IF Empty( oClient )
      ? "Invalid URL", cURL
      RETURN
   ENDIF
   oClient:nConnTimeout := 2000 /* 20000 */

   oURL:cUserid := StrTran( oURL:cUserid, "&at;", "@" )

   ? "Connecting to", oURL:cProto + "://" + oURL:cServer
   IF oClient:Open()
      ? "Connection status:", iif( Empty( oClient:cReply ), "<connected>", oClient:cReply )

      IF HB_ISSTRING( cFile ) .AND. hb_LeftEq( cFile, "+" )
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
         IF HB_ISSTRING( cFile )
            IF ( cData := oClient:Read() ) == ""
               ? "Error: file could not be retrieved", oClient:lastErrorMessage()
            ELSE
               ? "First 80 characters:", hb_ValToExp( hb_BLeft( cData, 80 ) ) )
            ENDIF
         ELSEIF oClient:ReadToFile( cFile )
            ? "File", cFile, "written."
            ? "Server replied", oClient:cReply
         ELSE
            ? "Error: Generic error in writing", cFile
         ENDIF
      ENDIF

      oClient:Close()
      ? "Done:", iif( Empty( oClient:cReply ), "(no goodbye message)", oClient:cReply )
   ELSE
      ? "Could not open URI", cURL
      IF ! Empty( oClient:cReply )
         ? oClient:cReply
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION ShowGauge( nSent, nSize )

   SetPos( Row(), 0 )
   ?? "Sending:", nSent, "/", nSize

   RETURN hb_keyStd( Inkey() ) != K_ESC
