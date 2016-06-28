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
   dbUseArea( .T., "DBFCDX", "test.dbf", "TESTDBF", .T., .F. )
   dbCreate( "testcdx.dbf", aStruct, "DBFCDX", .T., "TESTCDX" )

   ? "rddName():", rddName()
   WAIT
   dbSelectArea( "TESTDBF" )
   SET FILTER TO TESTDBF->SALARY > 140000
   TESTDBF->( dbGoTop() )
   DO WHILE ! TESTDBF->( Eof() )
      TESTCDX->( dbAppend() )
      TESTCDX->CHARACTER := TESTDBF->FIRST
      TESTCDX->NUMERIC := TESTDBF->SALARY
      TESTCDX->MEMO := ;
         TESTDBF->FIRST + hb_eol() + ;
         TESTDBF->LAST + hb_eol() + ;
         TESTDBF->STREET
      TESTDBF->( dbSkip() )
   ENDDO

   ? TESTCDX->( LastRec() )
   TESTCDX->( dbGoTop() )
   ? TESTCDX->( Eof() )
   DO WHILE ! TESTCDX->( Eof() )
      ? TESTCDX->( RecNo() ), TESTCDX->NUMERIC
      ? TESTCDX->MEMO
      TESTCDX->( dbSkip() )
      WAIT
   ENDDO

   hb_dbDrop( "testcdx.cdx" )

   dbSelectArea( "TESTCDX" )
   ordCreate( "testcdx", "Character", "FIELD->CHARACTER", {|| FIELD->CHARACTER }, .F. )

   dbCloseAll()
   hb_dbDrop( "testcdx.dbf",, "DBFCDX" )

   RETURN
