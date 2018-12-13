/* TIP FTP advanced operations test */

#require "hbtip"

#include "directry.ch"

PROCEDURE Main( cURL )

   LOCAL oFTP, oURL, aFile

   IF Empty( oURL := TUrl():New( cURL ) )
      ? "Invalid URL", cURL
      RETURN
   ENDIF

   IF ! oURL:cProto == "ftp"
      ? "This is a 'DELE' test for FTP."
      ? "Use an FTP address with a file that you can delete."
      RETURN
   ENDIF

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   oFTP := TIPClientFTP():New( oURL )
   oFTP:nConnTimeout := 20000
   ? "Connecting with", oURL:cServer
   IF oFTP:Open( cURL )
      ? "Connection established"
      ? "File listing"
      FOR EACH aFile IN oFTP:ListFiles()
         IF aFile:__enumIndex() > 10
            ? "Skipping the rest..."
            EXIT
         ENDIF
         ? aFile[ F_DATE ], Str( aFile[ F_SIZE ], 10 ), aFile[ F_NAME ]
      NEXT
      IF oFTP:CWD( oURL:cPath )
         ? "CWD success"
         IF ! Empty( oURL:cFile )
            ? "Deleting", oURL:cPath
            IF oFTP:Dele( oURL:cFile )
               ? "DELE success"
            ELSE
               ? "DELE failure (server reply:", oFTP:cReply + ")"
            ENDIF
         ENDIF
      ELSE
         ? "CWD failure (server reply:", oFTP:cReply + ")"
      ENDIF

      oFTP:Close()
   ELSE
      ? "Could not connect with", oURL:cServer
      IF oFTP:SocketCon == NIL
         ? "Connection not initiated"
      ELSEIF hb_inetErrorCode( oFTP:SocketCon ) == 0
         ? "Server replied:", oFTP:cReply
      ELSE
         ? "Error in connection:", hb_inetErrorDesc( oFTP:SocketCon )
      ENDIF
   ENDIF

   ? "Done"

   RETURN
