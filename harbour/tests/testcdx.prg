/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL aStruct := { ;
      { "CHARACTER", "C", 25, 0 }, ;
      { "NUMERIC",   "N",  8, 0 }, ;
      { "DOUBLE",    "N",  8, 2 }, ;
      { "DATE",      "D",  8, 0 }, ;
      { "MEMO",      "M", 10, 0 }, ;
      { "LOGICAL",   "L",  1, 0 } }

   CLS
   dbUseArea( .T. , "DBFCDX", "test", "TESTDBF", .T. , .F. )
   dbCreate( "testcdx", aStruct, "DBFCDX", .T. , "TESTCDX" )

   ? "RddName:", rddName()
// ? "Press any key to continue..."
// InKey( 0 )
   Select( "TESTDBF" )
   SET FILTER TO TESTDBF->SALARY > 140000
   TESTDBF->( dbGoTop() )
// WHILE !TESTDBF->( Eof() )
//    TESTCDX->( dbAppend() )
//    TESTCDX->CHARACTER := TESTDBF->FIRST
//    TESTCDX->NUMERIC := TESTDBF->SALARY
//    TESTCDX->MEMO := TESTDBF->FIRST + Chr( 13 ) + Chr( 10 ) + ;
//                     TESTDBF->LAST + Chr( 13 ) + Chr( 10 ) + ;
//                     TESTDBF->STREET
//    TESTDBF->( dbSkip() )
// ENDDO

   ? TESTCDX->( RecCount() )
   TESTCDX->( dbGoTop() )
   ? TESTCDX->( EOF() )
   WHILE !TESTCDX->( EOF() )
      ? TESTCDX->( RecNo() ), TESTCDX->NUMERIC
      ? TESTCDX->MEMO
      TESTCDX->( dbSkip() )
//    ? "Press any key to continue..."
//    InKey( 0 )
   ENDDO

   FErase( "testcdx.cdx" )

   Select( "TESTCDX" )
   ordCreate( "testcdx", "Character", "CHARACTER", FIELD->CHARACTER, .F. )

   dbCloseAll()
   hb_dbDrop( "testcdx",, "DBFCDX" )

   RETURN
