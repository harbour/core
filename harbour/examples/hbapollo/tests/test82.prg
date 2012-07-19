/*
 * $Id$
 */
/*
   sx_ReplaceArray()/sx_GetValueArray()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aValue, i, j
   LOCAL aStruct1 := { { "NO","C",5,0 }, { "BINARY","M",10,0 } }

   SET DATE "dd/mm/yyyy"
   CREATE TABLE "TEST1" STRUCT aStruct1 RDD SDEFOX
   USE "test1" ALIAS ONE RDD SDEFOX
   APPEND BLANK
   REPLACE NO WITH "001"
   ? sx_ReplaceArray( "BINARY", aStruct1 )
   sx_Commit()

   aValue := sx_GetValueArray ( "BINARY" )

   FOR i := 1 TO Len( aValue )
      ? "Element: ", ltrim( str( i ) )
      FOR j := 1 TO Len( aValue [i] )
         ? aValue[i][j], "[" + ValType( aValue[i][j] ) + "]"
      NEXT
      ?
   NEXT
