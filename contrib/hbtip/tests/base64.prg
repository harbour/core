/*
 * TIP Base64 (and other) encoding
 *
 * This test writes data to standard output, and is
 * compiled only under GTCGI. This allow to test the
 * input/output file against other OS encoded/decoded data
 */

#require "hbtip"

PROCEDURE Main( ... )

   LOCAL oEncoder
   LOCAL cData
   LOCAL cBuffer := Space( 1024 )
   LOCAL nLen
   LOCAL lHelp := .F., lDecode := .F., lQp := .F., lUrl := .F.

   LOCAL hInput := hb_GetStdIn()
   LOCAL hOutput := hb_GetStdOut()

   /* Parameter parsing */
   FOR nLen := 1 TO PCount()

      cData := Lower( hb_PValue( nLen ) )

      SWITCH cData
      CASE "-h"
         lHelp := .T.
         EXIT
      CASE "-d"
         lDecode := .T.
         EXIT
      CASE "-q"
         lQp := .T.
         EXIT
      CASE "-u"
         lUrl := .T.
         EXIT
      OTHERWISE
         IF hb_FileExists( cData ) .AND. hInput == hb_GetStdIn()
            hInput := FOpen( cData )
         ELSEIF hOutput == hb_GetStdOut()
            hOutput := FCreate( cData )
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

   /* Selecting the encoder */
   IF lUrl
      oEncoder := TIPEncoder():New( "url" )
   ELSEIF lQp
      oEncoder := TIPEncoder():New( "quoted-printable" )
   ELSE
      oEncoder := TIPEncoder():New( "base64" )
   ENDIF

   /* Reading input stream */
   cData := ""
   DO WHILE ( nLen := FRead( hInput, @cBuffer, hb_BLen( cBuffer ) ) ) > 0
      cData += hb_BLeft( cBuffer, nLen )
   ENDDO
   IF hInput != hb_GetStdIn()
      FClose( hInput )
   ENDIF

   /* Encoding/decoding */
   IF lDecode
      cData := oEncoder:Decode( cData )
   ELSE
      cData := oEncoder:Encode( cData )
   ENDIF

   /* Writing stream */
   FWrite( hOutput, cData )
   IF hOutput != hb_GetStdOut()
      FClose( hOutput )
   ENDIF

   RETURN
