PROCEDURE Main

   LOCAL   cLocal
   PRIVATE cName

   CLEAR SCREEN

   Alert( "Testinf PP as Interpreter... " )

   USE test
   IF ! File( "test" + IndexExt() )
      INDEX on FIELD->First TO First
   ELSE
      SET INDEX TO First
   ENDIF

   GO TOP

   cName := FIELD->First + FIELD->Last

   IF cName == FIELD->First + FIELD->Last
      ? "Ok"
   ELSE
      ? "Err"
   ENDIF

   DO CASE
      CASE cName == First // Not exact!
        ? "Err"

      CASE cName = First // But still equal
        ? "Ok"

      OTHERWISE
        ? "Err"
   ENDCASE

   REPLACE First WITH "From PP"

   ? FIELD->First

   cLocal := "in main"
   ? Test()
   ? cLocal
   ? cName

   ? cFromTest
   ? TestPrv

RETURN

FUNCTION Test

    PRIVATE TestPrv
    PUBLIC cFromTest

    ? cName
    ? cLocal

    M->TestPrv := "Private of Test"
    Test2()

RETURN ProcName()

PROCEDURE Test2

   ? ProcName(), ProcLine(), M->testPrv

RETURN
