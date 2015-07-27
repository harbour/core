/* TIP Base64 (and other) encoding
 *
 * This test writes data to standard output, and is
 * compiled only under GTCGI. This allow to test the
 * input/output file against other OS encoded/decoded data
 */

#require "hbtip"

#include "fileio.ch"

PROCEDURE Main( ... )

   LOCAL oEncoder, cEncoder := "base64"
   LOCAL lDecode := .F., lHelp := .F.
   LOCAL cData, nLen, cBuffer

   LOCAL fhndInput, hInput := hb_GetStdIn()
   LOCAL fhndOutput, hOutput := hb_GetStdOut()

   /* Parameter parsing */
   FOR EACH cData IN hb_AParams()

      SWITCH Lower( cData )
      CASE "-h"
         lHelp := .T.
         EXIT
      CASE "-d"
         lDecode := .T.
         EXIT
      CASE "-q"
         cEncoder := "quoted-printable"
         EXIT
      CASE "-u"
         cEncoder := "url"
         EXIT
      OTHERWISE
         IF hb_vfExists( cData ) .AND. hInput == hb_GetStdIn()
            IF ( fhndInput := hb_vfOpen( cData ) ) != NIL
               hInput := hb_vfHandle( fhndInput )
            ENDIF
         ELSEIF hOutput == hb_GetStdOut()
            IF ( fhndOutput := hb_vfOpen( cData, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
               hOutput := hb_vfHandle( fhndOutput )
            ENDIF
         ELSE
            ? "Wrong parameter", cData
            ?
            lHelp := .T.
         ENDIF
      ENDSWITCH
   NEXT

   /* Providing help */
   IF lHelp
      ? "Usage:"
      ? "base64 [<] file-to-encode [>] encoded-file"
      ? "base64 -d [<] encoded-file [>] file-to-decode"
      ? "base64 -q [-d]  to use quoted printable encoding/decoding"
      ? "base64 -u [-d]  to use URL encoding/decoding."
      ? "input/output redirection is optional"
      ?
      RETURN
   ENDIF

   /* Reading input stream */
   cData := ""
   cBuffer := Space( 1024 )
   DO WHILE ( nLen := FRead( hInput, @cBuffer, hb_BLen( cBuffer ) ) ) > 0
      cData += hb_BLeft( cBuffer, nLen )
   ENDDO
   IF fhndInput != NIL
      hb_vfClose( fhndInput )
   ENDIF

   /* Encoding/decoding */
   oEncoder := TIPEncoder():New( cEncoder )
   IF lDecode
      cData := oEncoder:Decode( cData )
   ELSE
      cData := oEncoder:Encode( cData )
   ENDIF

   /* Writing stream */
   FWrite( hOutput, cData )
   IF fhndOutput != NIL
      hb_vfClose( fhndOutput )
   ENDIF

   RETURN
