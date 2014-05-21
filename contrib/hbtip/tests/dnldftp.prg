/* Download a file from an FTP server */

#require "hbtip"

#include "directry.ch"

PROCEDURE Main( cMask )

   LOCAL lRetVal := .T.

   LOCAL aFiles
   LOCAL cUrl
   LOCAL oUrl
   LOCAL oFTP
   LOCAL cUser
   LOCAL cServer
   LOCAL cPassword
   LOCAL aFile

   cServer   := "ftp.example.com"  /* change this to the real name or IP of your FTP server */
   cUser     := "ftpuser"          /* change this to a valid user on the server */
   cPassword := "ftppass"          /* change this to a valid password for the user */
   cUrl      := "ftp://" + cUser + ":" + cPassword + "@" + cServer

   /* fetch files to transfer */
   aFiles := { { hb_defaultValue( cMask, __FILE__ ), 1, 2, 3 } }

   IF ! Empty( aFiles )

      oUrl              := TUrl():New( cUrl )
      oFTP              := TIPClientFTP():New( oUrl, .T. )
      oFTP:nConnTimeout := 20000
      oFTP:bUsePasv     := .T.

      /* Comprobamos si el usuario contiene una @ para forzar el userid */
      IF "@" $ cUser
         oFTP:oUrl:cServer   := cServer
         oFTP:oUrl:cUserID   := cUser
         oFTP:oUrl:cPassword := cPassword
      ENDIF

      IF oFTP:Open( cUrl )
         FOR EACH aFile IN aFiles
            IF oFtp:DownloadFile( aFile[ F_NAME ] )
               lRetVal := .T.
            ELSE
               lRetVal := .F.
               EXIT
            ENDIF
         NEXT
         oFTP:Close()
      ELSE
         ? "Could not connect to FTP server", oURL:cServer
         IF oFTP:SocketCon == NIL
            ? "Connection not initialized"
         ELSEIF hb_inetErrorCode( oFTP:SocketCon ) == 0
            ? "Server response:", oFTP:cReply
         ELSE
            ? "Error in connection:", hb_inetErrorDesc( oFTP:SocketCon )
         ENDIF
         lRetVal := .F.
      ENDIF
   ENDIF

   ErrorLevel( iif( lRetVal, 0, 1 ) )

   RETURN
