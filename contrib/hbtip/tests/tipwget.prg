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

   CLS
   @ 1, 6 SAY "Harbour - TIP (class based internet client protocol) test"

   IF ! HB_ISSTRING( cUrl ) .OR. Empty( cUrl )
      @ 4, 5 SAY hb_StrFormat( "Usage: %1$s <URI> [dumpToOrFromFileName]", hb_ProgName() )
      Terminate()
      RETURN
   ENDIF

   oUrl := TUrl():New( cUrl )
   IF Empty( oUrl )
      @ 4, 5 SAY "Invalid URL " + cUrl
      Terminate()
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
      @ 4, 5 SAY "Invalid URL " + cUrl
      Terminate()
      RETURN
   ENDIF
   oClient:nConnTimeout := 2000 /* 20000 */

   oUrl:cUserid := StrTran( oUrl:cUserid, "&at;", "@" )

   @ 4, 5 SAY "Connecting to " + oUrl:cProto + "://" + oUrl:cServer
   IF oClient:Open()
      IF Empty( oClient:cReply )
         @ 5, 5 SAY "Connection status: <connected>"
      ELSE
         @ 5, 5 SAY "Connection status: " + oClient:cReply
      ENDIF

      IF ! Empty( cFile ) .AND. Left( cFile, 1 ) == "+"
         cFile := SubStr( cFile, 2 )
         bWrite := .T.
      ENDIF

      IF oClient:nAccessMode == TIP_WO .OR. ( oClient:nAccessMode == TIP_RW .AND. bWrite )
         oClient:exGauge := {| done, size | ShowGauge( done, size ) }
#if 0
         /* Can be also: */
         oClient:exGauge := {| done, size, oConnection | dothing( done, size, oConnection ) }
#endif
         IF oClient:WriteFromFile( cFile )
            @ 7, 5 SAY "Data successfully sent"
         ELSE
            @ 7, 5 SAY "Error: Data not sent " + oClient:lastErrorMessage()
         ENDIF
      ELSE
         IF Empty( cFile )
            cData := oClient:Read()
            IF ! Empty( cData )
               @ 7, 5 SAY "First 80 characters:"
               ? RTrim( SubStr( cData, 1, 80 ) )
            ELSE
               @ 7, 5 SAY "Error: file can't be retrieved " + oClient:lastErrorMessage()
            ENDIF
         ELSE
            IF oClient:ReadToFile( cFile )
               @ 7, 5 SAY "File " + cFile + " written."
               @ 8, 5 SAY "Server replied " + oClient:cReply
            ELSE
               @ 7, 5 SAY "Error: Generic error in writing " + cFile
            ENDIF
         ENDIF
      ENDIF

      oClient:Close()
      IF Empty( oClient:cReply )
         @ 22, 5 SAY "Done: (no goodbye message)"
      ELSE
         @ 22, 5 SAY "Done: " + oClient:cReply
      ENDIF
   ELSE
      @ 5, 5 SAY "Can't open URI " + cUrl
      IF ! Empty( oClient:cReply )
         @ 6, 5 SAY oClient:cReply
      ENDIF
   ENDIF

   Terminate()

   RETURN

STATIC PROCEDURE Terminate()

   @ MaxRow() - 1, 0 SAY PadC( "Program done - Press a key to terminate", MaxCol() + 1 )
   Inkey( 0 )
   @ MaxRow(), 0

   RETURN

STATIC PROCEDURE ShowGauge( nSent, nSize )

   @ 6, 5 SAY "Sending: " + Replicate( hb_UTF8ToStr( "░" ), 60 )
   /* nSent may be zero */
   IF nSent > 0
      @ 6, 14 SAY Replicate( hb_UTF8ToStr( "█" ), 60 * nSent / nSize )
   ENDIF

   RETURN
