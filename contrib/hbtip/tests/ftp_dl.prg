/* Download a file from an FTP server */

#require "hbtip"

#include "directry.ch"

PROCEDURE Main( cURL, cMask )

   LOCAL lRetVal := .T.

   LOCAL aFiles, aFile
   LOCAL oFTP, oURL

   /* fetch files to transfer */
   IF ! Empty( aFiles := { { hb_defaultValue( cMask, __FILE__ ),,, } } )

      hb_default( @cURL, "ftp://user:pass@ftp.example.com" )

      oURL              := TUrl():New( cUrl )
      oFTP              := TIPClientFTP():New( oURL, .T. )
      oFTP:nConnTimeout := 20000
      oFTP:bUsePasv     := .T.

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
