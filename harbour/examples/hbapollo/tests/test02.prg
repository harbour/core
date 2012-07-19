/*
 * $Id$
 */
/*
  Demo Creating DBF and Append Blank Records
  Replacing and Retrieving Data
  Using SDENSX
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

   // SET CENTURY ON
   // Automatically ON woth the following date format
   SET EPOCH 1950
   SET DATE "DD-MM-YYYY"
   SET( 4, "DD-MM-YYYY" )

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

   ? "---------------------------------------------"
   ? "Test Appending, Replacing and Retrieving Data"
   ? "---------------------------------------------"
   ? "Area  : ", nArea
   ? "RDD   : " + sx_rddDriver( nArea )
   ? 'FieldGet( MYCHAR )  = ', FieldGet( MYCHAR )   , "[" + ValType( FieldGet( MYCHAR ) )   + "]"
   ? 'FieldGet( MYDATE )  = ', FieldGet( MYDATE )   , "[" + ValType( FieldGet( MYDATE ) )   + "]"
   ? 'FieldGet( MYNUM0 )  = ', FieldGet( MYNUM0 )   , "[" + ValType( FieldGet( MYNUM0 ) )   + "]"
   ? 'FieldGet( MYNUM2 )  = ', FieldGet( MYNUM2 )   , "[" + ValType( FieldGet( MYNUM2 ) )   + "]"
   ? 'FieldGet( MYNUM3 )  = ', FieldGet( MYNUM3 )   , "[" + ValType( FieldGet( MYNUM3 ) )   + "]"
   ? 'FieldGet( MYMEMO )  = ', FieldGet( MYMEMO )   , "[" + ValType( FieldGet( MYMEMO ) )   + "]"
   ? 'FieldGet( MYLOGIC ) = ', FieldGet( MYLOGIC )  , "[" + ValType( FieldGet( MYLOGIC ) )  + "]"
   ? 'FieldGet( UNEXIST ) = ', FieldGet( UNEXIST )  , "[" + ValType( FieldGet( UNEXIST ) )  + "]"

   ?
   ? "Now Get All Field as String ... Press any key ... "
   PAUSE
   ?
   ? 'FieldGetStr( MYCHAR )  = ', FieldGetStr( MYCHAR )   , "[" + ValType( FieldGetStr( MYCHAR ) )   + "]"
   ? 'FieldGetStr( MYDATE )  = ', FieldGetStr( MYDATE )   , "[" + ValType( FieldGetStr( MYDATE ) )   + "]"
   ? 'FieldGetStr( MYNUM0 )  = ', FieldGetStr( MYNUM0 )   , "[" + ValType( FieldGetStr( MYNUM0 ) )   + "]"
   ? 'FieldGetStr( MYNUM2 )  = ', FieldGetStr( MYNUM2 )   , "[" + ValType( FieldGetStr( MYNUM2 ) )   + "]"
   ? 'FieldGetStr( MYNUM3 )  = ', FieldGetStr( MYNUM3 )   , "[" + ValType( FieldGetStr( MYNUM3 ) )   + "]"
   ? 'FieldGetStr( MYMEMO )  = ', FieldGetStr( MYMEMO )   , "[" + ValType( FieldGetStr( MYMEMO ) )   + "]"
   ? 'FieldGetStr( MYLOGIC ) = ', FieldGetStr( MYLOGIC )  , "[" + ValType( FieldGetStr( MYLOGIC ) )  + "]"
   ? 'FieldGetStr( UNEXIST ) = ', FieldGetStr( UNEXIST )  , "[" + ValType( FieldGetStr( UNEXIST ) )  + "]"

   ?
   ? "Now Get Date Field as DTOS ... Press any key ... "
   PAUSE
   ?
   ? 'FieldGetDTOS( MYDATE ) = ', FieldGetDTOS( MYDATE )  , "[" + ValType( FieldGetDTOS( MYDATE ) )  + "]"

   ?
   ? "Now Get Date Field as Julian Date ... Press any key ... "
   PAUSE
   ?
   ? 'FieldGetJulian( MYDATE ) = ', FieldGetJulian( "MYDATE" ), "[" + ValType( FieldGetJulian( MYDATE ) )  + "]"

   CLOSE DATABASE
