//
// $Id$
//

// This file is OK to have warnings.
#ifdef __HARBOUR__
   #pragma -es0
#endif

DECLARE FUNCTION nMyFunc( ) AS NUMERIC

FUNCTION Main()

   LOCAL n AS NUMERIC

   ? "This is a compiler test."

   n := nMyFunc()

   cVar := {|n AS NUMERIC , c AS CHARACTER, d AS DATE| n := 'A', c := 1, d := 0, .T. }

   RETURN NIL

FUNCTION Hex2Dec( lVar AS LOGICAL )

   LOCAL nVar AS NUMERIC, cVar AS CHARACTER, lVar2 AS LOGICAL, nNoType := 3
   PRIVATE cMemVar1 AS CHARACTER

        nVar := .T.

   nVar := 1

        nVar := 'A'

        cVar := 2

   cVar := 'B'

        cVar := 2

   lVar := .T.

     lVar := nNoType

        cVar := nVar

   M->cMemVar1 := 2

   NondDeclared := 2

   cVar := {|n AS NUMERIC , c AS CHARACTER, d AS DATE| n := nMyFunc( n,c,d ), c := 2  }

        nVar := 8 + cVar

        IF 1

   ENDIF

RETURN NIL

Function nMyFunc( nParam )

return nParam * 2
