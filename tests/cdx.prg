
#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL aStruct := { ;
      { "CHARACTER", "C", 25, 0 }, ;
      { "NUMERIC",   "N",  8, 0 }, ;
      { "DOUBLE",    "N",  8, 2 }, ;
      { "DATE",      "D",  8, 0 }, ;
      { "MEMO",      "M", 10, 0 }, ;
      { "LOGICAL",   "L",  1, 0 } }

   CLS
   dbUseArea( .T., "DBFCDX", "test", "TESTDBF", .T., .F. )
   dbCreate( "testcdx", aStruct, "DBFCDX", .T., "TESTCDX" )

   ? "RddName:", rddName()
   ? "Press any key to continue..."
   Inkey( 0 )
   Select( "TESTDBF" )
   SET FILTER TO TESTDBF->SALARY > 140000
   TESTDBF->( dbGoTop() )
   WHILE ! TESTDBF->( Eof() )
      TESTCDX->( dbAppend() )
      TESTCDX->CHARACTER := TESTDBF->FIRST
      TESTCDX->NUMERIC := TESTDBF->SALARY
      TESTCDX->MEMO := TESTDBF->FIRST + hb_eol() + ;
                       TESTDBF->LAST + hb_eol() + ;
                       TESTDBF->STREET
      TESTDBF->( dbSkip() )
   ENDDO

   ? TESTCDX->( RecCount() )
   TESTCDX->( dbGoTop() )
   ? TESTCDX->( Eof() )
   WHILE ! TESTCDX->( Eof() )
      ? TESTCDX->( RecNo() ), TESTCDX->NUMERIC
      ? TESTCDX->MEMO
      TESTCDX->( dbSkip() )
      ? "Press any key to continue..."
      Inkey( 0 )
   ENDDO

   hb_dbDrop( "testcdx.cdx" )

   Select( "TESTCDX" )
   ordCreate( "testcdx", "Character", "FIELD->CHARACTER", {|| FIELD->CHARACTER }, .F. )

   dbCloseAll()
   hb_dbDrop( "testcdx",, "DBFCDX" )

   RETURN
