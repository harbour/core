/*
 * $Id$
 */
/*
  sx_GetValueEx( nArea | cArea )
  sx_PutValueEx( aValues, nArea | cArea  )
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR", "C", 25, 0 }, ;
      { "MYDATE", "D", 8, 0 }, ;
      { "MYNUM0", "N", 10, 0 }, ;
      { "MYNUM2", "N", 10, 2 }, ;
      { "MYNUM3", "N", 10, 3 }, ;
      { "MYMEMO", "M", 10, 0 }, ;
      { "MYLOGIC", "L", 1, 0 } }
   LOCAL j, n := seconds(), nArea
   LOCAL aGet

   // SET CENTURY ON
   // Automatically ON woth the following date format
   SET EPOCH 1950
   SET DATE ANSI

   // Set Trim On to RTRIM String Value of FieldGets
   // Default is OFF
   SET TRIM ON

   CREATE DBF cFile STRUCT aStruct RDD SDENSX
   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE
   APPEND BLANK

   REPLACE MYCHAR  WITH "Harbour Power"
   REPLACE MYDATE  WITH DATE()
   REPLACE MYNUM0  WITH 10000
   REPLACE MYNUM2  WITH 250.25
   REPLACE MYNUM3  WITH 10000.123
   REPLACE MYLOGIC WITH .T.
   REPLACE MYMEMO  WITH "This is some text but you can use MEMOREAD()"

   COMMIT

   aGet := sx_GetValueEx()
   ?
   ? 'aGet := sx_GetValueEx()'
   ?
   ? 'Len( aGet ) =', padl( Len( aGet ), 2 )
   ?
   FOR J := 1 TO LEN( AGET )
      ? "aGet[" + ltrim( str( j ) ) + "] =", aGet[j], "[" + ValType( aGet[j] ) + "]"
   NEXT
   ?
   ? 'sx_LastRec() =', padl( sx_LastRec(), 2 )
   ? 'Append Blank and sx_PutValueEx( aGet ) ... Press any key ...'
   PAUSE
   APPEND BLANK
   sx_putvalueex( aGet )
   ?
   ? 'sx_LastRec() =', padl( sx_LastRec(), 2 ), "... Press any key ..."
   PAUSE

   BROWSE
