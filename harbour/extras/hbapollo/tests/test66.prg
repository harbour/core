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

   SET DATE ANSI
   CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDENSX
   USE "test1" ALIAS ONE RDD SDENSX
   //CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDEFOX
   //use "test1" alias ONE RDD SDEFOX
   APPEND BLANK
   REPLACE NO WITH "001"
   IF file( GetEnv( "WINDIR" ) + "\ACD Wallpaper.bmp" )
      sx_ReplaceBitmap( "BINARY", GetEnv( "WINDIR" ) + ""\ACD Wallpaper.bmp" )
      sx_Commit()
      ? sx_BLOBToFile( "BINARY", "test.bmp" )
   ELSE
      ? "File " + GetEnv( "WINDIR" ) + "\ACD Wallpaper.bmp required for testing. Not found!"
   ENDIF
