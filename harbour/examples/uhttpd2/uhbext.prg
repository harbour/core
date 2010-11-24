/*
 * $Id$
 */

/************************************************************
*
* Functions candidates to be a part of Harbour's core or RTL
*
*************************************************************/

FUNCTION split( cSeparator, cString )

   LOCAL aRet := {}
   LOCAL nI

   DO WHILE ( nI := At( cSeparator, cString ) ) > 0
      AAdd( aRet, Left( cString, nI - 1 ) )
      cString := SubStr( cString, nI + Len( cSeparator ) )
   ENDDO
   AAdd( aRet, cString )

   RETURN aRet

FUNCTION join( cSeparator, aData )

   LOCAL cRet := ""
   LOCAL nI

   FOR nI := 1 TO Len( aData )

      IF nI > 1
         cRet += cSeparator
      ENDIF

      IF     ValType( aData[ nI ] ) $ "CM" ; cRet += aData[ nI ]
      ELSEIF ValType( aData[ nI ] ) == "N" ; cRet += hb_ntos( aData[ nI ] )
      ELSEIF ValType( aData[ nI ] ) == "D" ; cRet += iif( ! Empty( aData[ nI ] ), DToC( aData[ nI ] ), "" )
      ELSE
      ENDIF
   NEXT

   RETURN cRet
