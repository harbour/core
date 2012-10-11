/*
 * $Id$
 */

/******************************************
* TIP test
* BASE64 (and other) encoding
*
* This test writes data to standard output, and is
* compiled only under GTCGI; this allow to test the
* input/output file against other OS encoded/decoded data
*
* Usage:
* base64 < file-to-encode >encoded-file
* base64 -d  < encoded-file  >file-to-decode
* base64 -q [-d]  to use quoted printable encoding/decoding.
* base64 -u [-d]  to use url encoding/decoding.
*****/

#require "hbtip"

#define hSTDIN 0
#define hSTDOUT 1

PROCEDURE Main( ... )

   LOCAL oEncoder
   LOCAL cData
   LOCAL cBuffer := Space( 1024 )
   LOCAL nLen
   LOCAL lHelp := .F. , lDecode := .F. , lQp := .F. , lUrl := .F.
   LOCAL hInput := hSTDIN
   LOCAL hOutput := hSTDOUT

   /* Parameter parsing */
   FOR nLen := 1 TO PCount()
      cData := Lower( hb_PValue( nLen ) )
      DO CASE
      CASE cData == '-h'
         lHelp := .T.

      CASE cData == '-d'
         lDecode := .T.

      CASE cData == '-q'
         lQp := .T.

      CASE cData == '-u'
         lUrl := .T.

         OTHERWISE
         IF hb_FileExists( cData ) .AND. hInput == hSTDIN
            hInput := FOpen( cData )
         ELSEIF hOutput == hSTDOUT
            hOutput := FCreate( cData )
         ELSE
            ? "Wrong parameter", cData
            ?
            lHelp := .T.
            EXIT
         ENDIF
      ENDCASE
   NEXT

   /* Providing help */
   IF lHelp
      ? "Usage:"
      ? "base64test [<] file-to-encode [>] encoded-file"
      ? "base64test -d [<] encoded-file  [>] file-to-decode"
      ? "base64test -q [-d]  to use quoted printable encoding/decoding"
      ? "base64test -u [-d]  to use url encoding/decoding."
      ? "input & output-redirection is optional"
      ?
      QUIT
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
   nLen := FRead( hInput, @cBuffer, 1024 )
   DO WHILE nLen > 0
      IF nLen < 1024
         cData += hb_BLeft( cBuffer, nLen )
      ELSE
         cData += cBuffer
      ENDIF
      nLen := FRead( hInput, @cBuffer, 1024 )
   ENDDO
   IF hInput != hSTDIN
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
   IF hOutput != hSTDOUT
      FClose( hOutput )
   ENDIF

   RETURN
