/*
 * $Id$
 */
/*
   sx_DbInfo() With Argument
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

   aInfo := sx_DbInfo( "ONE" )
   ? 'SX_DBINFO( "ONE" ) ....'
   ? REPL( "-", 76 )
   FOR s := 1 TO LEN ( aInfo )
      ? "Work Area   :", aInfo[s][1]
      ? "DBF Name    :", aInfo[s][2]
      ? "Alias       :", aInfo[s][3]
      ? "Shared      :", aInfo[s][4]
      ? "Readonly    :", aInfo[s][5]
      ? "RDE Type    :", aInfo[s][6]
      ? "Open Mode   :", aInfo[s][7]
      ? "RDD         :", aInfo[s][8]
      ? "CommitLevel :", aInfo[s][9]
      ? "RecSize     :", aInfo[s][10]
      ? "No of Fields:", aInfo[s][11]
      ? "Field Properties:"
      FOR t := 1 TO LEN ( aInfo[s][12] )
         ? " ", padr( aInfo[s][12][t][1], 20 ), aInfo[s][12][t][2], aInfo[s][12][t][3], aInfo[s][12][t][4]
      NEXT
   NEXT
   ?
   aInfo := sx_DbInfo( 2 )
   ? 'SX_DBINFO( 2 ) ....'
   ? REPL( "-", 76 )
   FOR s := 1 TO LEN ( aInfo )
      ? "Work Area   :", aInfo[s][1]
      ? "DBF Name    :", aInfo[s][2]
      ? "Alias       :", aInfo[s][3]
      ? "Shared      :", aInfo[s][4]
      ? "Readonly    :", aInfo[s][5]
      ? "RDE Type    :", aInfo[s][6]
      ? "Open Mode   :", aInfo[s][7]
      ? "RDD         :", aInfo[s][8]
      ? "CommitLevel :", aInfo[s][9]
      ? "RecSize     :", aInfo[s][10]
      ? "No of Fields:", aInfo[s][11]
      ? "Field Properties:"
      FOR t := 1 TO LEN ( aInfo[s][12] )
         ? " ", padr( aInfo[s][12][t][1], 20 ), aInfo[s][12][t][2], aInfo[s][12][t][3], aInfo[s][12][t][4]
      NEXT
   NEXT
   CLOSE ALL
