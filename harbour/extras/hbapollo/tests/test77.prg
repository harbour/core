/*
 * $Id$
 */
/*
   sx_GetBlobLength()
   sx_ReplaceBlob()/sx_BlobToFile()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aInfo, i, j
   LOCAL s, t
   LOCAL aStruct1 := { { "NO","C",5,0 }, { "BINARY","M",10,0 } }

   SET DATE ANSI
   CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDEFOX
   USE "test1" ALIAS ONE RDD SDEFOX
   APPEND BLANK
   REPLACE NO WITH "001"
   IF File( GetEnv( "WINDIR" ) + "\SYSTEM.INI" )
      sx_ReplaceBLOB( "BINARY", GetEnv( "WINDIR" ) + "\SYSTEM.INI" )
      sx_Commit()
      ? 'sx_GetBlobLength( "BINARY" ) =>', sx_GetBlobLength( "BINARY" )
      ? 'sx_BLOBToFile( "BINARY", "test.ini" ) =>', sx_BLOBToFile( "BINARY", "test.ini" )
   ELSE
      ? "File " + GetEnv( "WINDIR" ) + "\SYSTEM.INI required for testing. Not found."
   ENDIF
