/* TIP FTP advanced operations Test */

#require "hbtip"

#include "directry.ch"

PROCEDURE Main( cUrl )

   LOCAL oFTP, oURL, aFile

   IF Empty( oURL := TUrl():New( cUrl ) )
      ? "Invalid URL", cUrl
      RETURN
   ENDIF

   IF !( oURL:cProto == "ftp" )
      ? "This is a 'DELE' test for FTP."
      ? "Use an FTP address with a file that you can delete."
      RETURN
   ENDIF

   oFTP := TIPClientFTP():New( oURL )
   oFTP:nConnTimeout := 20000
   ? "Connecting with", oURL:cServer
   IF oFTP:Open( cUrl )
      ? "Connection established"
      ? "File listing"
      FOR EACH aFile IN oFTP:ListFiles()
         IF aFile:__enumIndex() > 10
            ? "Skipping the rest..."
            EXIT
         ENDIF
         ? aFile[ F_DATE ], Str( aFile[ F_SIZE ], 10 ), aFile[ F_NAME ]
      NEXT
      ? "Deleting", oURL:cPath
      IF oFTP:CWD( oURL:cPath )
         ? "CWD success"
         IF oFTP:Dele( oURL:cFile )
            ? "DELE success"
         ELSE
            ? "DELE failure (server reply:", oFTP:cReply + ")"
         ENDIF
      ELSE
         ? "CWD Faliure (server reply:", oFTP:cReply + ")"
      ENDIF

      oFTP:Close()
   ELSE
      ? "Can't connect with", oURL:cServer
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
