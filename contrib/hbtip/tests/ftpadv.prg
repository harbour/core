/* TIP FTP advanced operations Test */

#require "hbtip"

PROCEDURE Main( cUrl )

   LOCAL oCon, oUrl

   oUrl := TUrl():New( cUrl )
   IF Empty( oUrl )
      ? "Invalid URL", cUrl
      RETURN
   ENDIF

   IF !( oUrl:cProto == "ftp" )
      ? "This is a 'DELE' test for FTP."
      ? "Use an FTP address with a file that you can delete."
      RETURN
   ENDIF

   oCon := TIPClientFTP():New( oUrl )
   oCon:nConnTimeout := 20000
   ? "Connecting with", oUrl:cServer
   IF oCon:Open( cUrl )
      ? "Connection established"
      ? "Deleting", oUrl:cPath
      IF oCon:CWD( oUrl:cPath )
         ? "CWD success"
         IF oCon:Dele( oUrl:cFile )
            ? "DELE success"
         ELSE
            ? "DELE failure (server reply:", oCon:cReply + ")"
         ENDIF
      ELSE
         ? "CWD Faliure (server reply:", oCon:cReply + ")"
      ENDIF

      oCon:Close()
   ELSE
      ? "Can't connect with", oUrl:cServer
      IF oCon:SocketCon == NIL
         ? "Connection not initiated"
      ELSEIF hb_inetErrorCode( oCon:SocketCon ) == 0
         ? "Server replied:", oCon:cReply
      ELSE
         ? "Error in connection:", hb_inetErrorDesc( oCon:SocketCon )
      ENDIF
   ENDIF

   ? "Done"

   RETURN
