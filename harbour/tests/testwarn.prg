//
// $Id$
//

// This file is OK to have warnings.
#ifdef __HARBOUR__
   #pragma -es0
#endif

DECLARE FUNCTION nMyFunc( cVar AS STRING, nVar AS NUMERIC ) AS NUMERIC

DECLARE FUNCTION cOtherFunc( ) AS CHAR

DECLARE FUNCTION cOtherFunc( ) AS CHAR

DECLARE FUNCTION seconds() AS NUM

DECLARE FUNCTION int( n AS NUMERIC ) AS NUMERIC

Function Main()
  Local n As Numeric, lVar AS LOGICAL

  n := IIF( lVar, 'A', 3 )
  n := 2
  n := 'a'
  n := seconds() + 2
  n := int( seconds() + 2 )

Return( NIL )

FUNCTION Main1()

   LOCAL n AS NUMERIC, cVar AS CHARACTER, a[5,5,5] AS ARRAY

   n := &SomeFun( 2, 3 )

   n := ExtFun()

   cVar := cOtherFunc( 3 )

   n := nMyFunc( a, cVar ) + 3

   n := &(cVar)

   n := "&SomeVar"

   n := &Var.1

   n := V&SomeVar.1

   n[2] := 4

   cVar := {|nb AS NUMERIC , cb AS CHARACTER, db AS DATE| n := .F., nb := 'A', cb := 1, db := 0, n := 'wrong type', 0 }

   ? "This is a compiler test."

   n := 'C is Wrong Type for n'

   n := {1,2,3}

   n := a

   IIF( n, 2, 3 )

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
