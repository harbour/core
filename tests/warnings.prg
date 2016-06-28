// This file is OK to have warnings.
#ifdef __HARBOUR__
   #pragma -es0
#else
   #xtranslate AS ARRAY [OF <type>] =>
   #xtranslate AS STRING =>
   #xtranslate AS CLASS <ClassName> =>
   #xtranslate AS NUMERIC =>
   #xtranslate AS DATE =>
   #xtranslate AS CODEBLOCK =>
   #xtranslate AS OBJECT =>
   #xtranslate AS LOGICAL =>
   #xtranslate AS USUAL =>

   #xtranslate OPTIONAL =>

   #xcommand DECLARE <*x*> =>
#endif

DECLARE nMyFunc( cVar AS STRING, @nVar AS NUMERIC ) AS NUMERIC

DECLARE cOtherFunc( ) AS STRING

DECLARE cOtherFunc( @cVar AS STRING, OPTIONAL nVar AS NUMERIC, OPTIONAL other /* AS VARIANT */ ) AS STRING

DECLARE Seconds() AS NUMERIC

DECLARE Int( n AS NUMERIC ) AS NUMERIC

DECLARE TEST() AS NUMERIC

DECLARE MyClass ;
        nMyFunc( nVal AS NUMERIC) AS NUMERIC

DECLARE MyClass ;
        nMyFunc( nVal AS NUMERIC ) AS NUMERIC ;
        nMyFunc( nVal AS NUMERIC ) AS NUMERIC ;
        cMyData ;
        aInstances AS ARRAY OF OBJECT MyClass ;
        oNext( oInstance AS CLASS MyClass ) AS CLASS MyClass

DECLARE OtherClass ;
        nMyFunc( nVal AS NUMERIC ) AS NUMERIC ;
        nMyFunc( nVal AS NUMERIC ) AS NUMERIC ;
        cMyData ;
        aInstances AS ARRAY OF OBJECT MyClass ;
        oNext( oInstance AS CLASS OtherClass ) AS CLASS MyClass

FIELD a AS STRING
FIELD b AS STRING

MEMVAR Var1 AS STRING

STATIC s_lGlobal AS LOGICAL

PROCEDURE Main( OPTIONAL )

   STATIC s_lStatic := 0, s_oMyObj AS CLASS WrongClass

   LOCAL cVar AS STRING := [declare function]

   LOCAL a As STRING, oB AS CLASS MyClass, c AS STRING, oD AS CLASS OtherClass

   FIELD b AS NUMERIC

   MEMVAR Var1 AS NUMERIC

   PRIVATE TEST AS STRING

   USE temp

   s_oMyObj:MyMethod( 2, 3, 4 )

   a := b:nMyFunc( 2, 3 )
   a := b:nMyFunc( 2 )

   a := oB:oNext( 1 ):cMyData
   a := oB:oNext( c ):cMyData2
   a := oB:oNext( d ):cMyData
   a := oB:oNext( oD ):cMyData

   a := oB:aInstances[ 1 ]:oNext:cMyData2
   a := oB:aInstances[ 1 ]:oNext:cMyData

   x := cOtherFunc( "A" )
   x := cOtherFunc( @Test )
   x := cOtherFunc( "A", "A", "A" )

   M->TEST := "TEST"

   a := "A"

   oB := "a"

   IF s_lStatic
      Var1 := .F.
   ENDIF

   IF s_lGlobal
      Var1 := .T.
   ENDIF

   RETURN

PROCEDURE SOMEPROC()  /* must be a public function */

   PRIVATE TEST AS NUMERIC

   M->TEST := 1

   FOR M->TEST := 1 TO M->TEST + 10
      ? "Correct warnings for FOR/NEXT"
   NEXT

   REPLACE a WITH 1

   M->public_var := 0
   b := 0
   Var1 := 1

   IF s_lGlobal == 0
      ? "s_lGlobal is NOT Numeric"
   ENDIF

   RETURN

PROCEDURE Main1()  /* must be a public function */

   PRIVATE OTHER, TEST AS STRING

   Var1 := M->TEST
   Var2 := Test()

   ? Var1 + 2

   M->TEST := 1
   M->TEST := "No Warning"

   Test[ 1 ][ 2 ] := "Correct warning"

   RETURN

STATIC FUNCTION Test()
   RETURN .T.

FUNCTION Test2()  /* must be a public function */

   LOCAL n AS NUMERIC, lVar AS LOGICAL

   n := iif( lVar, "A", 3 ) // iif() needs to be completed.
   n := 2
   n := "a"
   n := Seconds() + 2
   n := Int( Seconds() + 2 )

   RETURN NIL

FUNCTION Test3()  /* must be a public function */

   LOCAL n AS NUMERIC, cVar AS STRING, a[ 5, 5, 5 ] AS ARRAY OF STRING

   cVar := a[ 1 ]

   n := &SomeFun( 2, 3 )
   n := ExtFun()
   cVar := cOtherFunc( 3 )
   n := nMyFunc( a, cVar ) + 3
   n := &(cVar)
   n := "&SomeVar"
   n := &Var.1
   n := V&SomeVar.1
   n[ 2 ] := 4

   cVar := {| nb AS NUMERIC, cb AS STRING, db AS DATE | n := .F., nb := "A", cb := 1, db := 0, n := "wrong type", 0 }

   ? "This is a compiler test."

   n := "C is Wrong Type for n"
   n := { 1, 2, 3 }
   n := a
   iif( n, 2, 3 )

   RETURN NIL

FUNCTION SomeTest( lVar AS LOGICAL )  /* must be a public function */

   LOCAL nVar AS NUMERIC, cVar AS STRING, lVar2 AS LOGICAL, nNoType := 3
   PRIVATE cMemVar1 AS STRING

   nVar := .T.
   nVar := 1
   nVar := "A"
   cVar := 2
   cVar := "B"
   cVar := 2
   lVar := .T.
   lVar := nNoType
   cVar := nVar
   M->cMemVar1 := 2
   NondDeclared := 2
   cVar := {| n AS NUMERIC, c AS STRING, d AS DATE | n := nMyFunc( n, c, d ), c := 2  }
   nVar := 8 + cVar

   IF 1

   ENDIF

   RETURN NIL

STATIC FUNCTION nMyFunc( cVar AS STRING, nVar AS NUMERIC )

   nVar := Val( cVar )

   RETURN nVar + 1

STATIC FUNCTION cOtherFunc( )
   RETURN "Hello"

STATIC FUNCTION ExtFun()
   RETURN 1
