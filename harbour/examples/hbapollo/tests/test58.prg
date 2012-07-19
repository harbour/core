/*
 * $Id$
 */
/*
  sx_GetRecord()
  sx_GetRecordEx()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "c:\windows\temp\sixtest.dbf"
   LOCAL aStruct := { ;
      { "FIRST", "C", 25, 0 }, ;
      { "LAST", "C", 25, 0 }, ;
      { "HIREDATE", "D", 8, 0 } }
   LOCAL j, n := seconds(), nArea, i
   LOCAL aGet, nTagCount, aTagInfo
   LOCAL cGetRecord

   SET RDD SDENTX
   SET EPOCH 1950
   SET DATE "DD-MM-YYYY"
   SET TRIM ON

   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS VAR nArea EXCLUSIVE
   APPEND BLANK

   REPLACE FIRST WITH "First_Name"
   REPLACE LAST WITH "Last_LastName"
   REPLACE HIREDATE WITH Date()
   sx_delete()
   cGetRecord := sx_GetRecord()
   ?
   ? '-----------------------------------'
   ? 'sx_GetRecord() and sx_GetRecordEx()'
   ? '-----------------------------------'
   ? 'cGetRecord := sx_GetRecord()'
   ? 'cGetRecord =', cGetRecord
   ? 'Vaptype(cGetRecord) =', ValType( cGetRecord )

   ?
   ? 'cGetRecord := sx_GetRecordEx()'
   cGetRecord := sx_GetRecordEx()
   ? 'Valtype(cGetRecord) =', ValType( cGetRecord )
   ? 'len(cGetRecord) =', padl( len( cGetRecord ), 1 )
   for i := 1 TO len( cGetRecord )
      ? "cGetRecord[" + padl( i, 1 ) + "] =", cGetRecord[i], "[" + ValType( cGetRecord[i] ) + "]"
   next
