DECLARE FUNCTION nMyFunc( ) AS NUMERIC

FUNCTION Hex2Dec( lVar AS LOGICAL )

   LOCAL nVar AS NUMERIC, cVar AS CHARACTER, lVar2 AS LOGICAL, nNoType := 3

   nVar := .T.

   nVar := 1

	nVar := 'A'

   cVar := 2

   cVar := 'B'

   cVar := 2

   lVar := .T.

   lVar := nNoType

   cVar := nVar

	NondDeclared := 2

   cVar := {|x,y,z| nMyFunc( 3 ) }

	nVar := 8 + cVar

	IF 1

   ENDIF

RETURN NIL

Function nMyFunc( nParam )

return nParam * 2
