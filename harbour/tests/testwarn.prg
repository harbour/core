//
// $Id$
//

// This file is OK to have warnings.
#ifdef __HARBOUR__
   #pragma -es0
#else

   #TRANSLATE AS ARRAY OF CHAR =>
   #TRANSLATE AS ARRAY OF CHARACTER =>
   #TRANSLATE AS ARRAY OF STRING =>

   #TRANSLATE AS ARRAY OF NUM =>
   #TRANSLATE AS ARRAY OF NUMERIC =>

   #TRANSLATE AS ARRAY OF DATE =>
   #TRANSLATE AS ARRAY OF ARRAY  =>
   #TRANSLATE AS ARRAY OF BLOCK =>
   #TRANSLATE AS ARRAY OF OBJECT =>

   #TRANSLATE AS ARRAY OF BOOL =>
   #TRANSLATE AS ARRAY OF BOOLEAN =>
   #TRANSLATE AS ARRAY OF LOGICAL =>

   #TRANSLATE AS ARRAY OF VAR =>
   #TRANSLATE AS ARRAY OF VARIANT =>


   #TRANSLATE AS CHAR =>
   #TRANSLATE AS CHARACTER =>
   #TRANSLATE AS STRING =>

   #TRANSLATE AS NUM =>
   #TRANSLATE AS NUMERIC =>

   #TRANSLATE AS DATE =>
   #TRANSLATE AS ARRAY =>
   #TRANSLATE AS BLOCK =>
   #TRANSLATE AS OBJECT =>

   #TRANSLATE AS BOOL =>
   #TRANSLATE AS BOOLEAN =>
   #TRANSLATE AS LOGICAL =>

   #TRANSLATE AS VAR =>
   #TRANSLATE AS VARIANT =>

   #COMMAND DECLARE FUNCTION <*x*> =>
#endif

DECLARE FUNCTION nMyFunc( cVar AS STRING, nVar AS NUMERIC ) AS NUMERIC

DECLARE FUNCTION cOtherFunc( @cVar as char, optional nVar as num, optional other as variant ) AS CHAR

DECLARE FUNCTION cOtherFunc( ) AS CHAR

DECLARE FUNCTION seconds() AS NUM

DECLARE FUNCTION int( n AS NUMERIC ) AS NUMERIC

DECLARE FUNCTION TEST AS NUMERIC

DECLARE CLASS MyClass                              ;
        Has METHOD   nMyFunc( nVal As Num ) As Num ;
        Has METHOD   nMyFunc( nVal As Num ) As Num ;
        Has Data     cMyData    ;
        Has Method   FinalMethod

DECLARE CLASS MyClass                              ;
        Has METHOD   nMyFunc( nVal As Num ) As Num

FIELD a AS CHAR
FIELD b AS CHAR

MEMVAR Var1 AS CHAR

STATIC lGlobal AS LOGICAL

PROCEDURE THEMAIN( optional )

  STATIC lStatic := 0, oMyObj As Object From CLASS WrongClass
  LOCAL cVar AS CHAR := [declare function]

  oMyObj:MyMethod( 2, 3, 4 )

  FIELD b AS NUM
  USE TEMP

  MEMVAR a AS NUM
  MEMVAR Var1 AS NUM

  PRIVATE TEST AS CHAR

  DO Optional WITH Var1
  DO Optional WITH 1
  DO Optional WITH "something"

  FOR Conter := Optional TO 10
    ? "For with End"
  End

  IF optional
     ? 'Ok'
  ENDIF

  x := cOtherFunc( 'A' )
  x := cOtherFunc( @Test )
  x := cOtherFunc( 'A', 'A', 'A' )

  M->TEST := "TEST"   //OK - no warnings here

  a := 'A'

  b := 'a'

  if lStatic
     Var1 := .F.
  endif

  IF lGlobal
     Var1 := .T.
  ENDIF

RETURN

PROCEDURE SOMEPROC()

  PRIVATE TEST AS NUMERIC

  M->TEST := 1		//incorrect warning is printed here

  FOR M->TEST := 1 TO M->TEST + 10
    ? "Incorrect warnings for FOR/NEXT"
  NEXT

  REPLACE a WITH 1

  //M->public_var := 0    //core dumps (GPF) on Linux

  b := 0

  Var1 := 1

  if lGlobal = 0
  endif

RETURN
PROC MAIN1()

  PRIVATE OTHER, TEST AS CHAR

  Var1 := M->TEST

  Var2 := Test()

  ? Var1 + 2

  M->TEST := 1
  M->TEST  := "incorrect warning"

  Test[ 1 ][ 2 ] := "incorrect warning"

RETURN

Function Test()
return .t.

Function Main2()
  Local n As Numeric, lVar AS LOGICAL

  n := IIF( lVar, 'A', 3 )
  n := 2
  n := 'a'
  n := seconds() + 2
  n := int( seconds() + 2 )

Return( NIL )

FUNCTION Main3()

   LOCAL n AS NUMERIC, cVar AS CHARACTER, a[5,5,5] AS ARRAY OF Char

   cVar := a[1]

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

*/
