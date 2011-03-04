/*
 * $Id$
 */

/******************************************
 * TIP test
 * FTP Advanced operations Test
 ******************************************/

PROCEDURE MAIN( cUrl )

   LOCAL oCon, oUrl

   oUrl := tURL():New( cUrl )
   IF Empty( oUrl )
      ? "Invalid url " + cUrl
      ?
      QUIT
   ENDIF

   IF oUrl:cProto != "ftp"
      ? 'This is a "DELE" test for ftp.'
      ? 'Use an ftp address with a file that you can delete.'
      ?
      QUIT
   END

   oCon := TipClientFtp():New( oUrl )
   oCon:nConnTimeout := 20000
   ? "Connecting with", oUrl:cServer
   IF oCon:Open( cUrl )
      ? "Connection eshtablished"
      ? "Deleting", oUrl:cPath
      IF oCon:CWD( oUrl:cPath )
         ? "CWD success"
         IF oCon:Dele( oUrl:cFile )
            ? "DELE success"
         ELSE
            ? "DELE Faliure (server reply:", oCon:cReply + ")"
         ENDIF
      ELSE
         ? "CWD Faliure (server reply:", oCon:cReply + ")"
      ENDIF

      oCon:Close()
   ELSE
      ? "Can't connect with", oUrl:cServer
      IF oCon:SocketCon == NIL
         ? "Connection not initiated"
      ELSEIF hb_InetErrorCode( oCon:SocketCon ) == 0
         ? "Server sayed:", oCon:cReply
      ELSE
         ? "Error in connection:", hb_InetErrorDesc( oCon:SocketCon )
      ENDIF
   END

   ? "Done"
   ?

   RETURN
