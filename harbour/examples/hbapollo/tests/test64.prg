/*
 * $Id$
 */
/*
   sx_BaseDate(), sx_BaseName()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aInfo, i, j
   LOCAL s, t
   LOCAL aStruct1 := { { "PART_NO","C",10,0 }, { "PRICE","N",10,2 }, { "NOTES","M",10,0 } }
   LOCAL aStruct2 := { { "CUST_NAME","C",20,0 }, { "AMOUNT","N",12,0 }, { "DUEDATE","D",8,0 }, { "PAID","L",1,0 } }

   SET DATE "dd/mm/yyyy"
   CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDEFOX
   CREATE TABLE "TEST2" STRUCT aStruct2 RDD SDENSX
   USE "test1" ALIAS ONE READONLY RDD SDEFOX
   USE "test2" ALIAS two EXCLUSIVE RDD SDENSX

   ? 'sx_BaseDate()      =', sx_BaseDate()
   ? 'sx_BaseDate(1)     =', sx_BaseDate( 1 )
   ? 'sx_BaseDate(2)     =', sx_BaseDate( 2 )
   ? 'sx_BaseDate("ONE") =', sx_BaseDate( "ONE" )
   ? 'sx_BaseDate("TWO") =', sx_BaseDate( "TWO" )
   ?
   ? 'sx_BaseName(1)     =', sx_BaseName( 1 )
   ? 'sx_BaseName(2)     =', sx_BaseName( 2 )
   ? 'sx_BaseName("ONE") =', sx_BaseName( "ONE" )
   ? 'sx_BaseName("TWO") =', sx_BaseName( "TWO" )
