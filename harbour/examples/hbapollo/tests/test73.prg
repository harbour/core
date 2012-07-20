/*
 * $Id$
 */
/*
   sx_Empty()/sx_IsNull()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aStruct1 := { { "PART_NO","C",10,0 }, { "PRICE","N",10,2 }, { "NOTES","M",10,0 } }

   SET RDD SDEFOX
   SET DATE ANSI
   CREATE TABLE "TEST1" STRUCT aStruct1
   USE "test1"
   APPEND BLANK
   REPLACE PRICE WITH 20
   ? sx_Empty( "PART_NO" )
   ? sx_IsNull( "PART_NO" )
   ? sx_Empty( "PRICE" )
   ? sx_IsNull( "PRICE" )
