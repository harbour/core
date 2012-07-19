/*
 * $Id$
 */
/*
   sx_DbInfo() Without Arguments
*/

#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aInfo, i, j
   LOCAL s, t
   LOCAL aStruct1 := { { "PART_NO","C",10,0 }, { "PRICE","N",10,2 }, { "NOTES","M",10,0 } }
   LOCAL aStruct2 := { { "CUST_NAME","C",20,0 }, { "AMOUNT","N",12,0 }, { "DUEDATE","D",8,0 }, { "PAID","L",1,0 } }

   SET RDD SDEFOX
   SET DATE "dd/mm/yyyy"
   CREATE TABLE "TEST1" STRUCT aStruct1
   CREATE TABLE "TEST2" STRUCT aStruct2
   USE "test1" ALIAS ONE readonly
   USE "test2" ALIAS two exclusive

   aInfo := sx_DbInfo()
   ? 'SX_DBINFO() ....'
   ?
   FOR j := 1 TO LEN( aInfo )
      ? REPL( "-", 76 )
      ? "Work Area   :", aInfo[j][1]
      ? "DBF Name    :", aInfo[j][2]
      ? "Alias       :", aInfo[j][3]
      ? "Shared      :", aInfo[j][4]
      ? "Readonly    :", aInfo[j][5]
      ? "RDE Type    :", aInfo[j][6]
      ? "Open Mode   :", aInfo[j][7]
      ? "RDD         :", aInfo[j][8]
      ? "CommitLevel :", aInfo[j][9]
      ? "RecSize     :", aInfo[j][10]
      ? "No of Fields:", aInfo[j][11]
      ? "Field Properties:"
      FOR s := 1 TO LEN ( aInfo[j][12] )
         ? " ", padr( aInfo[j][12][s][1], 20 ), aInfo[j][12][s][2], aInfo[j][12][s][3], aInfo[j][12][s][4]
      NEXT
   NEXT
   CLOSE ALL
