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
   MYALIAS->MEMO1 := "111"
   MYALIAS->MEMO2 := "222"
   ? "[" + MYALIAS->MEMO1 + "]"
   ? "[" + MYALIAS->MEMO2 + "]"
   MYALIAS->NUMERIC := 90
   MYALIAS->DOUBLE := 120.138
   ? "[" + Str( MYALIAS->DOUBLE ) + "]"
   ? "[" + Str( MYALIAS->NUMERIC ) + "]"

   dbCloseAll()

   ? ""
   ? "Press any key..."
   InKey( 0 )

   dbCreate( "testdbf", aStruct, "DBFCDX", .t., "MYALIAS" )

   for nI := 1 to 10
      MYALIAS->( dbAppend() )
      MYALIAS->NUMERIC := nI
      ? "Adding a record", nI
      if nI == 3 .or. nI == 7
         MYALIAS->( dbDelete() )
         ? "Deleting record", nI
      endif
   next

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

