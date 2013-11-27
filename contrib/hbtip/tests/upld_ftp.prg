/*
 * Send an file or list of files to ftp server
 */

#require "hbtip"

#include "directry.ch"

PROCEDURE Main( cMask )

   ? TRP20FTPEnv( cMask  )

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

   cServer   := "ftp.example.com"   /* change ftpserver to the real name  or ip of your ftp server */
   cUser     := "ftpuser"           /* change ftpuser to an valid user on ftpserer */
   cPassword := "ftppass"           /* change ftppass  to an valid password for ftpuser */
   cUrl      := "ftp://" + cUser + ":" + cPassword + "@" + cServer

   /* Leemos ficheros a enviar */
   aFiles := Directory( cCarpeta )

   IF Len( aFiles ) > 0

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
         FOR EACH cFile IN afiles
            ? "Filename: " + cFile[ F_NAME ]
            IF ! oFtp:UploadFile( cFile[ F_NAME ] )
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
