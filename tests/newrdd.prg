#ifndef __HARBOUR__
#include "clipper.ch"
#endif

REQUEST _DBF

PROCEDURE Main()

   LOCAL nI, aArray

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   rddSetDefault( "DBF" )

   SET EXCLUSIVE OFF
   CLS

   dbUseArea( .T., "DBF", "test", "TESTDBF", .T., .F. )
   ? "RecCount:", TESTDBF->( RecCount() )
   ? "Used:", TESTDBF->( Used() )
   ? "Select:", TESTDBF->( Select() )
   ? "RecSize:", TESTDBF->( RecSize() )
   ? "RecNo:", TESTDBF->( RecNo() )
   ? "NetErr:", TESTDBF->( NetErr() )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   aArray := rddList( 0 )
   ? "Rdd's: "
   FOR nI := 1 TO Len( aArray )
      ?? aArray[ nI ], ""
   NEXT
   ? "RddName:", TESTDBF->( rddName() )
   ? "lUpdate:", TESTDBF->( LUpdate() )
   ? "Header:", TESTDBF->( Header() )
   ? "FieldPos( 'LAST' ):", TESTDBF->( FieldPos( "LAST" ) )
   ? "FieldName( 2 ):", TESTDBF->( FieldName( 2 ) )
   ? "Dbf():", TESTDBF->( Dbf() )
   ? "Alias( 1 ):", Alias( 1 )
   ? "dbTableExt():", TESTDBF->( dbTableExt() )
   aArray := TESTDBF->( dbStruct() )
   ? "dbStruct:"
   FOR nI := 1 TO Len( aArray )
      ? PadR( aArray[ nI ][ 1 ], 10 ), aArray[ nI ][ 2 ], aArray[ nI ][ 3 ], aArray[ nI ][ 4 ]
   NEXT

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "dbGoTop():"
   dbGoTop()
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( -1 ):"
   dbSkip( -1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( -1 ):"
   dbSkip( -1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( 1 ):"
   dbSkip( 1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( 1 ):"
   dbSkip( 1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbGoTop():"
   dbGoTop()
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( 1 ):"
   dbSkip( 1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( 1 ):"
   dbSkip( 1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( -1 ):"
   dbSkip( -1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( -1 ):"
   dbSkip( -1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   Inkey( 0 )
   CLS

   ? "dbGoBottom():"
   dbGoBottom()
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( 1 ):"
   dbSkip( 1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( 1 ):"
   dbSkip( 1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( -1 ):"
   dbSkip( -1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "dbSkip( -1 ):"
   dbSkip( -1 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "dbGoto( 490 ):"
   dbGoto( 490 )
   ? "Bof Eof Found Deleted RecNo:", TESTDBF->( Bof() ), TESTDBF->( Eof() ), ;
      TESTDBF->( Found() ), TESTDBF->( Deleted() ), TESTDBF->( Deleted() ), ;
      TESTDBF->( RecNo() )
   ? "FCount:", TESTDBF->( FCount() )
   FOR nI := 1 TO TESTDBF->( FCount() )
      ? "FieldGet( " + hb_ntos( nI ) + " ):", TESTDBF->( FieldGet( nI ) )
   NEXT

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "WHILE ! TESTDBF->( Eof() )"
   ? "   ? TESTDBF->FIRST, TESTDBF->( RecNo() )"
   ? "   TESTDBF->( dbSkip() )"
   ? "ENDDO"
   ? ""
   WHILE ! TESTDBF->( Eof() )
      ? TESTDBF->FIRST, TESTDBF->( RecNo() )
      TESTDBF->( dbSkip() )
   ENDDO

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "SET FILTER TO TESTDBF->AGE == 21"
   ? "? TESTDBF->( dbFilter() )"
   ? "TESTDBF->( dbGoTop() )"
   ? "WHILE ! TESTDBF->( Eof() )"
   ? "   ? TESTDBF->FIRST, TESTDBF->AGE, TESTDBF->( RecNo() )"
   ? "   TESTDBF->( dbSkip() )"
   ? "ENDDO"
   ? "SET FILTER TO"
   ? ""
   SET FILTER TO TESTDBF->AGE == 21
   ? TESTDBF->( dbFilter() )
   TESTDBF->( dbGoTop() )
   WHILE ! TESTDBF->( Eof() )
      ? TESTDBF->FIRST, TESTDBF->AGE, TESTDBF->( RecNo() )
      TESTDBF->( dbSkip() )
   ENDDO
   SET FILTER TO

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "TESTDBF->( Found() )"
   ? "LOCATE FOR TESTDBF->AGE == 23"
   ? "WHILE TESTDBF->( Found() )"
   ? "   ? TESTDBF->FIRST, TESTDBF->AGE, TESTDBF->( RecNo() )"
   ? "   CONTINUE"
   ? "ENDDO"
   TESTDBF->( Found() )
   LOCATE FOR TESTDBF->AGE == 23
   WHILE TESTDBF->( Found() )
      ? TESTDBF->FIRST, TESTDBF->AGE, TESTDBF->( RecNo() )
      CONTINUE
   ENDDO

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? "TESTDBF->( dbEval( {|| QOut( TESTDBF->FIRST, TESTDBF->AGE ) }, ;"
   ? "                   {|| TESTDBF->AGE == 23 } ) )"
   ? ""
   ? "dbCommit()"
   TESTDBF->( dbEval( {|| QOut( TESTDBF->FIRST, TESTDBF->AGE ) }, ;
      {|| TESTDBF->AGE == 23 } ) )
   TESTDBF->( dbCommit() )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? 'dbCreate( "newrdd", { ;'
   ? '   { "First_Name", "C", 20, 0 }, ;'
   ? '   { "Age",        "N",  3, 0 }, ;'
   ? '   { "Date",       "D",  8, 0 }, ;'
   ? '   { "Rate",       "N",  6, 2 }, ;'
   ? '   { "Memo",       "M", 10, 0 }, ;'
   ? '   { "Student",    "L",  1, 0 } },, .T., "newrdd" )'
   ? 'SET CENTURY ON'
   dbCreate( "newrdd", { ;
      { "First_Name", "C", 20, 0 }, ;
      { "Age",        "N",  3, 0 }, ;
      { "Date",       "D",  8, 0 }, ;
      { "Rate",       "N",  6, 2 }, ;
      { "Memo",       "M", 10, 0 }, ;
      { "Student",    "L",  1, 0 } }, "DBFCDX", .T., "newrdd" )
   SET CENTURY ON
   ? "lUpdate:", NEWRDD->( LUpdate() )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? 'Select( "TESTDBF" )'
   ? "SET FILTER TO TESTDBF->SALARY > 120000"
   ? "TESTDBF->( dbGoTop() )"
   ? "WHILE ! TESTDBF->( Eof() )"
   ? "   NEWRDD->( dbAppend() )"
   ? "   NEWRDD->FIRST_NAME := TESTDBF->FIRST"
   ? "   NEWRDD->AGE := TESTDBF->AGE"
   ? "   NEWRDD->DATE := TESTDBF->HIREDATE"
   ? "   NEWRDD->RATE := Val( Right( hb_ntos( Seconds() ), 5 ) )"
   ? "   NEWRDD->MEMO := TESTDBF->FIRST + Chr( 13 ) + Chr( 10 ) + ;"
   ? "                   TESTDBF->LAST + Chr( 13 ) + Chr( 10 ) + ;"
   ? "                   TESTDBF->STREET"
   ? "   NEWRDD->STUDENT := TESTDBF->MARRIED"
   ? "   TESTDBF->( dbSkip() )"
   ? "ENDDO"
   ? "SET FILTER TO"
   ? "? NEWRDD->( RecCount() )"
   ? "NEWRDD->( dbGoTop() )"
   ? "NEWRDD->( dbRLock( 1 ) )"
   ? "NEWRDD->( dbDelete() )"
   ? "? NEWRDD->( Deleted() )"
   ? "NEWRDD->( dbGoBottom() )"
   ? "NEWRDD->( dbRLock() )"
   ? "NEWRDD->( dbDelete() )"
   ? "? NEWRDD->( Deleted() )"
   ? "NEWRDD->( dbRLock( 3 ) )"
   Select( "TESTDBF" )
   SET FILTER TO TESTDBF->SALARY > 120000
   TESTDBF->( dbGoTop() )
   WHILE ! TESTDBF->( Eof() )
      NEWRDD->( dbAppend() )
      NEWRDD->FIRST_NAME := TESTDBF->FIRST
      NEWRDD->AGE := TESTDBF->AGE
      NEWRDD->DATE := TESTDBF->HIREDATE
      NEWRDD->RATE := Val( Right( hb_ntos( Seconds() ), 5 ) )
      NEWRDD->MEMO := TESTDBF->FIRST + Chr( 13 ) + Chr( 10 ) + ;
         TESTDBF->LAST + Chr( 13 ) + Chr( 10 ) + ;
         TESTDBF->STREET
      NEWRDD->STUDENT := TESTDBF->MARRIED
      TESTDBF->( dbSkip() )
   ENDDO
   SET FILTER TO
   ? NEWRDD->( RecCount() )
   NEWRDD->( dbGoTop() )
   NEWRDD->( dbRLock( 1 ) )
   NEWRDD->( dbDelete() )
   ? NEWRDD->( Deleted() )
   NEWRDD->( dbGoBottom() )
   NEWRDD->( dbRLock() )
   NEWRDD->( dbDelete() )
   ? NEWRDD->( Deleted() )
   NEWRDD->( dbRLock( 3 ) )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   aArray := NEWRDD->( dbRLockList() )
   ? "aArray := NEWRDD->( dbRLockList() )"
   ? "FOR nI := 1 TO Len( aArray )"
   ? "   ? aArray[ nI ]"
   ? "NEXT"
   ? "dbRLockList(): "
   FOR nI := 1 TO Len( aArray )
      ? aArray[ nI ]
   NEXT

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? 'NEWRDD->( dbCloseArea() )'
   ? 'dbUseArea( .T., "DBF", "newrdd", "NEWRDD", .F., .F. )'
   ? 'nI := 1'
   ? 'NEWRDD->( __dbPack( {|| QOut( nI ), nI++ } ) )'
   ? '? "RecCount:", NEWRDD->( RecCount() )'
   ? ""
   NEWRDD->( dbCloseArea() )
   dbUseArea( .T., "DBFCDX", "newrdd", "NEWRDD", .F., .F. )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   nI := 1
   NEWRDD->( __dbPack( {|| QOut( nI ), nI++ } ) )
   ? "RecCount:", NEWRDD->( RecCount() )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? '? "RecCount:", NEWRDD->( RecCount() )'
   ? 'NEWRDD->( __dbZap() )'
   ? '? "RecCount:", NEWRDD->( RecCount() )'
   ? ""
   ? "RecCount:", NEWRDD->( RecCount() )
   NEWRDD->( __dbZap() )
   ? "RecCount:", NEWRDD->( RecCount() )

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   ? 'NEWRDD->( dbCloseArea() )'
   ? 'SORT ON FIRST /DC, AGE /D TO NEWRDD'
   ? 'dbUseArea( .T., "DBF", "newrdd", "NEWRDD", .F., .F. )'
   ? '? "RecCount:", NEWRDD->( RecCount() )'
   ? 'FOR nI := 1 TO 8'
   ? '   ? NEWRDD->FIRST, NEWRDD->AGE'
   ? '   NEWRDD->( dbSkip() )'
   ? 'NEXT'
   ? '? "..."'
   ? 'NEWRDD->( dbGoBottom() )'
   ? 'NEWRDD->( dbSkip( -8 ) )'
   ? 'FOR nI := 1 TO 8'
   ? '   ? NEWRDD->FIRST, NEWRDD->AGE'
   ? '   NEWRDD->( dbSkip() )'
   ? 'NEXT'

   ? "Press any key to continue..."
   Inkey( 0 )
   CLS

   NEWRDD->( dbCloseArea() )
   Select( "TESTDBF" )
   SORT ON FIRST /DC, AGE /D TO newrdd

   dbUseArea( .T., "DBF", "newrdd", "NEWRDD", .F., .F. )
   ? "RecCount:", NEWRDD->( RecCount() )
   FOR nI := 1 TO 8
      ? NEWRDD->FIRST, NEWRDD->AGE
      NEWRDD->( dbSkip() )
   NEXT
   ? "..."
   NEWRDD->( dbGoBottom() )
   NEWRDD->( dbSkip( -8 ) )
   FOR nI := 1 TO 8
      ? NEWRDD->FIRST, NEWRDD->AGE
      NEWRDD->( dbSkip() )
   NEXT

   dbCloseAll()

   hb_dbDrop( "newrdd",, "DBFCDX" )

   RETURN
