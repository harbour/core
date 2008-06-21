/*
 * $Id$
 */

function main()

   local nI, aStruct := { { "CHARACTER", "C", 25, 0 }, ;
                          { "NUMERIC",   "N",  8, 0 }, ;
                          { "DOUBLE",    "N",  8, 2 }, ;
                          { "DATE",      "D",  8, 0 }, ;
                          { "LOGICAL",   "L",  1, 0 }, ;
                          { "MEMO1",     "M", 10, 0 }, ;
                          { "MEMO2",     "M", 10, 0 } }

   REQUEST DBFCDX

   dbCreate( "testdbf", aStruct, "DBFCDX", .t., "MYALIAS" )

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
   InKey( 0 )

   ? ""
   ? "Append 50 records with memos..."
   for nI := 1 to 50
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
                        "sombrero grande rid¡culo." + Chr( 13 ) + Chr( 10 ) + ;
                        "/" + Chr( 13 ) + Chr( 10 ) + "[;-)" + Chr( 13 ) + Chr( 10 )+ ;
                        "\"
   next
   MYALIAS->( dbCommit() )

   ? "Records before ZAP:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[1][2], ;
      Directory( "testdbf.fpt" )[1][2]
   MYALIAS->( __dbZap() )
   MYALIAS->( dbCommit() )
   ? "Records after ZAP:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[1][2], ;
      Directory( "testdbf.fpt" )[1][2]
   ? "Value of fields MEMO1, MEMO2, DOUBLE and NUMERIC:"
   ? "[" + MYALIAS->MEMO1 + "]"
   ? "[" + MYALIAS->MEMO2 + "]"
   ? "[" + Str( MYALIAS->DOUBLE ) + "]"
   ? "[" + Str( MYALIAS->NUMERIC ) + "]"
   ? "Press any key..."
   InKey( 0 )
   dbCloseAll()

   dbCreate( "testdbf", aStruct,, .t., "MYALIAS" )

   for nI := 1 to 10
      MYALIAS->( dbAppend() )
      MYALIAS->NUMERIC := nI
      ? "Adding a record", nI
      if nI == 3 .or. nI == 7
         MYALIAS->( dbDelete() )
         ? "Deleting record", nI
      endif
   next
   MYALIAS->( dbCommit() )

   ? ""
   ? "With SET DELETED OFF"
   ? "Press any key..."
   InKey( 0 )

   MYALIAS->( dbGoTop() )
   do while !MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   enddo

   SET DELETED ON
   ? ""
   ? "With SET DELETED ON"
   ? "Press any key..."
   InKey( 0 )

   MYALIAS->( dbGoTop() )
   do while !MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   enddo

   ? ""
   ? "With SET DELETED ON"
   ? "and  SET FILTER TO MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8"
   ? "Press any key..."
   InKey( 0 )

   MYALIAS->( dbSetFilter( { || MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8 }, ;
                           "MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8" ) )
   MYALIAS->( dbGoTop() )
   do while !MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   enddo

   SET DELETED OFF
   ? ""
   ? "With SET DELETED OFF"
   ? "and  SET FILTER TO MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8"
   ? "Press any key..."
   InKey( 0 )

   MYALIAS->( dbSetFilter( { || MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8 }, ;
                           "MYALIAS->NUMERIC > 2 .AND. MYALIAS->NUMERIC < 8" ) )
   MYALIAS->( dbGoTop() )
   do while !MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   enddo

   ? "dbFilter() => " + dbFilter()
   ? ""

   ? "Testing __dbPack()"
   ? "Records before PACK:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[1][2], ;
      Directory( "testdbf.dbt" )[1][2]
   SET FILTER TO
   MYALIAS->( __dbPack() )
   MYALIAS->( dbCommit() )
   ? "Records after PACK:", MYALIAS->( LastRec() )
   ? "Size of files (data and memo):", Directory( "testdbf.dbf" )[1][2], ;
      Directory( "testdbf.dbt" )[1][2]
   ? "Press any key..."
   InKey( 0 )
   ? "Value of fields:"
   MYALIAS->( dbGoTop() )
   do while !MYALIAS->( Eof() )
      ? MYALIAS->NUMERIC
      MYALIAS->( dbSkip() )
   enddo
   ? ""

   ? "Open test.dbf and LOCATE FOR TESTDBF->SALARY > 145000"
   ? "Press any key..."
   InKey( 0 )
   dbUseArea( ,, "test", "TESTDBF" )
   locate for TESTDBF->SALARY > 145000
   do while TESTDBF->( Found() )
      ? TESTDBF->FIRST, TESTDBF->LAST, TESTDBF->SALARY
      continue
   enddo
   ? ""
   ? "LOCATE FOR TESTDBF->MARRIED .AND. TESTDBF->FIRST > 'S'"
   ? "Press any key..."
   InKey( 0 )
   dbUseArea( ,, "test", "TESTDBF" )
   locate for TESTDBF->MARRIED .AND. TESTDBF->FIRST > 'S'
   do while TESTDBF->( Found() )
      ? TESTDBF->FIRST, TESTDBF->LAST, TESTDBF->MARRIED
      continue
   enddo

return nil

