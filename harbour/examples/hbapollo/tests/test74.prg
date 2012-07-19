/*
 * $Id$
 */
/*
   sx_EvalLogical()
   sx_EvalNumeric()
   sx_EvalString()
   sx_EvalDate()
   sx_EvalTest()
*/

#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aStruct1 := { ;
      { "PART_NO"   , "C" , 10, 0 }, ;
      { "PART_NAME" , "C", 10, 0 }, ;
      { "PRICE"     , "N", 10, 2 }, ;
      { "DATE_SOLD" , "D", 8, 0 }, ;
      { "COST"      , "N", 10, 2 }, ;
      { "NOTES"     , "M", 10, 0 } }
   LOCAL d

   SET RDD SDENSX
   SET DATE "dd/mm/yyyy"
   CREATE TABLE "TEST1" STRUCT aStruct1
   USE "test1"
   APPEND BLANK
   REPLACE PRICE WITH 20
   REPLACE COST WITH 10
   REPLACE PART_NO WITH "PART"
   REPLACE PART_NAME WITH "0010"
   REPLACE DATE_SOLD WITH DATE()

   ? [sx_EvalLogical( "Upper(PART_NO)='PART'" ) =>], sx_EvalLogical( "Upper(PART_NO)='PART'" )
   ? 'sx_EvalNumeric( "PRICE-COST" ) =>', sx_EvalNumeric( "PRICE-COST" )
   ? 'sx_EvalString( "ALLTRIM(PART_NO) + ALLTRIM(PART_NAME)" ) =>', sx_EvalString( "ALLTRIM(PART_NO) + ALLTRIM(PART_NAME)" )
   ? 'sx_EvalString( "DATE_SOLD + 30" ) =>', sx_EvalString( "DATE_SOLD + 30" )
   ? 'sx_EvalDate( "DATE_SOLD + 30" ) =>', sx_EvalDate( "DATE_SOLD + 30" )
   ?
   ? [sx_EvalTest( "Upper(PART_NO)='PART'" ) =>], sx_EvalTest( "Upper(PART_NO)='PART'" )
   ? 'sx_EvalTest( "PRICE-COST" ) =>', sx_EvalTest( "PRICE-COST" )
   ? 'sx_EvalTest( "ALLTRIM(PART_NO) + ALLTRIM(PART_NAME)" ) =>', sx_EvalTest( "ALLTRIM(PART_NO) + ALLTRIM(PART_NAME)" )
   ? 'sx_EvalTest( "DATE_SOLD + 30" ) =>', sx_EvalTest( "DATE_SOLD + 30" )
