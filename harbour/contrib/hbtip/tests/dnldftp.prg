/*
 * $Id$
 */

/*
 * Download an file from an ftp server
 */

#require "hbtip"

PROCEDURE Main( cFile )

   ? TRP20FTPEnv( cFile )

   RETURN

/**********************************************************************
 *
 *     Static Function TRP20FTPEnv()
 *
 **********************************************************************/

STATIC FUNCTION TRP20FTPEnv( cCarpeta )

   LOCAL aFiles
   LOCAL cUrl
   LOCAL cStr
   LOCAL lRetVal  := .T.
   LOCAL oUrl
   LOCAL oFTP
   LOCAL cUser
   LOCAL cServer
   LOCAL cPassword
   LOCAL cFile     := ""

   cServer   := "ftpserver"   /* change ftpserver to the real name  or ip of your ftp server */
   cUser     := "ftpuser"     /* change ftpuser to an valid user on ftpserer */
   cPassword := "ftppass"     /* change ftppass  to an valid password for ftpuser */
   cUrl      := "ftp://" + cUser + ":" + cPassword + "@" + cServer

   /* Leemos ficheros a enviar */
   aFiles := { { cCarpeta, 1, 2, 3 } }
   /* aFiles := Directory( cCarpeta ) */

   IF Len( aFiles ) > 0

      oUrl              := tUrl():New( cUrl )
      oFTP              := tIPClientFtp():New( oUrl, .T. )
      oFTP:nConnTimeout := 20000
      oFTP:bUsePasv     := .T.

      /* Comprobamos si el usuario contiene una @ para forzar el userid */
      IF At( "@", cUser ) > 0
         oFTP:oUrl:cServer   := cServer
         oFTP:oUrl:cUserID   := cUser
         oFTP:oUrl:cPassword := cPassword
      ENDIF

      IF oFTP:Open( cUrl )
         FOR EACH cFile IN afiles
            IF ! oFtp:DownloadFile( cFile[ 1 ] )
               lRetVal := .F.
               EXIT
            ELSE
               lRetVal := .T.
            ENDIF
         NEXT
         oFTP:Close()
      ELSE
         cStr := "Could not connect to FTP server " + oURL:cServer
         IF oFTP:SocketCon == NIL
            cStr += hb_eol() + "Connection not initialized"
         ELSEIF hb_inetErrorCode( oFTP:SocketCon ) == 0
            cStr += hb_eol() + "Server response:" + " " + oFTP:cReply
         ELSE
            cStr += hb_eol() + "Error in connection:" + " " + hb_inetErrorDesc( oFTP:SocketCon )
         ENDIF
         ? cStr
         lRetVal := .F.
      ENDIF
   ENDIF

   RETURN lRetVal
