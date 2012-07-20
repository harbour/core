/*
 * $Id$
 */
/*
   sx_FLock()
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
   LOCAL nCount, i, cFieldName

   SET RDD SDENSX
   SET DATE ANSI
   CREATE TABLE "TEST1" STRUCT aStruct1
   USE "test1"
   APPEND BLANK

   ? sx_Flock()
