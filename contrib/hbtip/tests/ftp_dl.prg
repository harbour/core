/* Download a file from an FTP server */

#require "hbtip"

PROCEDURE Main( cURL, ... )

   LOCAL lRetVal := .T.

   LOCAL aFiles, cFile
   LOCAL oFTP, oURL

   /* fetch files to transfer */
   IF ! Empty( aFiles := { ... } )

      hb_default( @cURL, "ftp://user:pass@ftp.example.com" )

      oURL := TUrl():New( cURL )

      oFTP := TIPClientFTP():New( oURL, .T. )
      oFTP:nConnTimeout := 20000
      oFTP:bUsePasv := .T.

      IF oFTP:Open( cURL )
         FOR EACH cFile IN aFiles
            ? "Filename:", cFile
            IF ! oFtp:DownloadFile( cFile )
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
