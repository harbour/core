/*
 * $Id$
 */
/*
   sx_ReplaceBlob()/sx_BlobToFile()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aInfo, i, j
   LOCAL s, t
   LOCAL aStruct1 := { { "NO","C",5,0 }, { "BINARY","M",10,0 } }

   SET DATE "dd/mm/yyyy"
   CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDEFOX
   USE "test1" ALIAS ONE RDD SDEFOX
   APPEND BLANK
   REPLACE NO WITH "001"
   IF File( "C:\WINDOWS\SYSTEM.INI" )
      sx_ReplaceBLOB( "BINARY", "C:\WINDOWS\SYSTEM.INI" )
      sx_Commit()
      ? sx_BLOBToFile( "BINARY", "test.ini" )
   ELSE
      ? "File C:\WINDOWS\SYSTEM.INI required for testing. Not found."
   ENDIF
