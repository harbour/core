
#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL nI, aStruct := { ;
      { "CHARACTER", "C", 25, 0 }, ;
      { "NUMERIC",   "N",  8, 0 }, ;
      { "DOUBLE",    "N",  8, 2 }, ;
      { "DATE",      "D",  8, 0 }, ;
      { "LOGICAL",   "L",  1, 0 }, ;
      { "MEMO1",     "M", 10, 0 }, ;
      { "MEMO2",     "M", 10, 0 } }

   REQUEST DBFCDX

   dbCreate( "testdbf", aStruct, "DBFCDX", .T., "MYALIAS" )

   ? "[" + MYALIAS->MEMO1 + "]"
   ? "[" + MYALIAS->MEMO2 + "]"
   ? "-"
   MYALIAS->( dbAppend() )
   MYALIAS->MEMO1 := "Hello world!"
   MYALIAS->MEMO2 := "Harbour power"
   ? "[" + MYALIAS->MEMO1 + "]"
   ? "[" + MYALIAS->MEMO2 + "]"
   MYALIAS->( dbAppend() )
   MYALIAS->MEMO1 := "This is a test for field MEMO1."
   MYALIAS->MEMO2 := "This is a test for field MEMO2."
   ? "[" + MYALIAS->MEMO1 + "]"
   ? "[" + MYALIAS->MEMO2 + "]"
   MYALIAS->NUMERIC := 90
   MYALIAS->DOUBLE := 120.138
   ? "[" + Str( MYALIAS->DOUBLE ) + "]"
   ? "[" + Str( MYALIAS->NUMERIC ) + "]"

   ? ""
   ? "Press any key..."
   Inkey( 0 )

   ? ""
   ? "Append 50 records with memos..."
   FOR nI := 1 TO 50
      MYALIAS->( dbAppend() )
      MYALIAS->MEMO1 := "This is a very long string. " + ;
         "This may seem silly however strings like this are still " + ;
         "used. Not by good programmers though, but I've seen " + ;
         "stuff like this used for Copyright messages and other " + ;
         "long text. What is the point to all of this you'd say. " + ;
         "Well I am coming to the point right now, the constant " + ;
         "string is limited to 256 characters and this string is " + ;
         "a lot bigger. Do you get my drift ? If there is somebody " + ;
         "who has read this line upto the very end: Esto es un " + ;
         "sombrero grande ridiculo." + Chr( 13 ) + Chr( 10 ) + ;
         "/" + Chr( 13 ) + Chr( 10 ) + "[;-)" + Chr( 13 ) + Chr( 10 ) + ;
         "\"
   NEXT
   MYALIAS->( dbCommit() )

   ? "Records before ZAP:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[ 1 ][ 2 ], ;
      Directory( "testdbf.fpt" )[ 1 ][ 2 ]
   MYALIAS->( __dbZap() )
   MYALIAS->( dbCommit() )
   ? "Records after ZAP:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[ 1 ][ 2 ], ;
      Directory( "testdbf.fpt" )[ 1 ][ 2 ]
   ? "Value of fields MEMO1, MEMO2, DOUBLE and NUMERIC:"
   ? "[" + MYALIAS->MEMO1 + "]"
   ? "[" + MYALIAS->MEMO2 + "]"
   ? "[" + Str( MYALIAS->DOUBLE ) + "]"
   ? "[" + Str( MYALIAS->NUMERIC ) + "]"
   ? "Press any key..."
   Inkey( 0 )
   dbCloseAll()

   dbCreate( "testdbf", aStruct, , .T., "MYALIAS" )

   FOR nI := 1 TO 10
      MYALIAS->( dbAppend() )
      MYALIAS->NUMERIC := nI
      ? "Adding a record", nI
      IF nI == 3 .OR. nI == 7
         MYALIAS->( dbDelete() )
         ? "Deleting record", nI
      ENDIF
   NEXT
   MYALIAS->( dbCommit() )

   ? ""
   ? "With SET DELETED OFF"
   ? "Press any key..."
   Inkey( 0 )

   MYALIAS->( dbGoTop() )
   DO WHILE ! MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   ENDDO

   SET DELETED ON
   ? ""
   ? "With SET DELETED ON"
   ? "Press any key..."
   Inkey( 0 )

   MYALIAS->( dbGoTop() )
   DO WHILE ! MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   ENDDO

   ? ""
   ? "With SET DELETED ON"
   ? "and  SET FILTER TO MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8"
   ? "Press any key..."
   Inkey( 0 )

   MYALIAS->( dbSetFilter( {|| MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8 }, ;
      "MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8" ) )
   MYALIAS->( dbGoTop() )
   DO WHILE ! MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   ENDDO

   SET DELETED OFF
   ? ""
   ? "With SET DELETED OFF"
   ? "and  SET FILTER TO MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8"
   ? "Press any key..."
   Inkey( 0 )

   MYALIAS->( dbSetFilter( {|| MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8 }, ;
      "MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8" ) )
   MYALIAS->( dbGoTop() )
   DO WHILE ! MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   ENDDO

   ? "dbFilter() => " + dbFilter()
   ? ""

   ? "Testing __dbPack()"
   ? "Records before PACK:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[ 1 ][ 2 ], ;
      Directory( "testdbf.dbt" )[ 1 ][ 2 ]
   SET FILTER TO
   MYALIAS->( __dbPack() )
   MYALIAS->( dbCommit() )
   ? "Records after PACK:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[ 1 ][ 2 ], ;
      Directory( "testdbf.dbt" )[ 1 ][ 2 ]
   ? "Press any key..."
   Inkey( 0 )
   ? "Value of fields:"
   MYALIAS->( dbGoTop() )
   DO WHILE ! MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   ENDDO
   ? ""

   ? "Open test.dbf and LOCATE FOR TESTDBF->SALARY > 145000"
   ? "Press any key..."
   Inkey( 0 )
   dbUseArea( , , "test", "TESTDBF" )
   LOCATE for TESTDBF->SALARY > 145000
   DO WHILE TESTDBF->( Found() )
      ? TESTDBF->FIRST, TESTDBF->LAST, TESTDBF->SALARY
      CONTINUE
   ENDDO
   ? ""
   ? "LOCATE FOR TESTDBF->MARRIED .AND. TESTDBF->FIRST > 'S'"
   ? "Press any key..."
   Inkey( 0 )
   dbUseArea( , , "test", "TESTDBF" )
   LOCATE for TESTDBF->MARRIED .AND. TESTDBF->FIRST > 'S'
   DO WHILE TESTDBF->( Found() )
      ? TESTDBF->FIRST, TESTDBF->LAST, TESTDBF->MARRIED
      CONTINUE
   ENDDO

   dbCloseAll()
   hb_dbDrop( "testdbf",, "DBFCDX" )

   RETURN
