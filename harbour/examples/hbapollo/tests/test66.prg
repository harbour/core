/*
 * $Id$
 */
/*
   sx_ReplaceBitmap()/sx_BlobToFile()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aInfo, i, j
   LOCAL s, t
   LOCAL aStruct1 := { { "NO","C",5,0 }, { "BINARY","M",10,0 } }

   SET DATE "dd/mm/yyyy"
   CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDENSX
   USE "test1" ALIAS ONE RDD SDENSX
   //CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDEFOX
   //use "test1" alias ONE RDD SDEFOX
   APPEND BLANK
   REPLACE NO WITH "001"
   IF file( "C:\WINDOWS\ACD Wallpaper.bmp" )
      sx_ReplaceBitmap( "BINARY", "C:\WINDOWS\ACD Wallpaper.bmp" )
      sx_Commit()
      ? sx_BLOBToFile( "BINARY", "test.bmp" )
   ELSE
      ? "File C:\WINDOWS\setup.bmp required for testing. Not found!"
   ENDIF
