/*
 * $Id$
 */
/*
   sx_GetByte       ()
   sx_GetDateJulian ()
   sx_GetDateString ()
   sx_GetDouble     ()
   sx_GetInteger    ()
   sx_GetLogical    ()
   sx_GetLong       ()
   sx_GetMemo       ()
   sx_GetString     ()
   sx_GetTrimString ()
   sx_GetVariant    ()
*/

#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aStruct1 := { ;
      { "PART_NO"   , "C", 20, 0 }, ;
      { "PART_NAME" , "C", 10, 0 }, ;
      { "PRICE"     , "N", 10, 2 }, ;
      { "DATE_SOLD" , "D", 8, 0 }, ;
      { "COST"      , "N", 10, 2 }, ;
      { "SOLD"      , "L", 1, 0 }, ;
      { "NOTES"     , "M", 10, 0 } }
   LOCAL xVar

   SET RDD SDEFOX
   SET DATE ANSI
   CREATE TABLE "TEST1" STRUCT ASTRUCT1
   USE "TEST1"
   APPEND BLANK
   REPLACE PART_NO WITH "MY_PARTNO"
   REPLACE DATE_SOLD WITH DATE()
   REPLACE PRICE WITH 25.30
   REPLACE SOLD WITH .F.
   REPLACE NOTES WITH "This is some notes for testing purposes"
   COMMIT

   ? 'sx_GetString( "PART_NO" ) =>', xVar := sx_GetString( "PART_NO" ), '[' + ValType( xVar ) + ']'
   ? 'sx_GetTrimString( "PART_NO" ) =>', xVar := sx_GetTrimString( "PART_NO" ), '[' + ValType( xVar ) + ']'
   ? 'sx_GetByte( "PART_NO" ) =>', xVar := sx_GetByte( "PART_NO" ), '[' + ValType( xVar ) + ']'
   ? 'sx_GetDateJulian ( "DATE_SOLD" ) =>', xVar := sx_GetDateJulian ( "DATE_SOLD" ), '[' + ValType( xVar ) + ']'
   ? 'sx_GetDateString ("DATE_SOLD" ) =>', xVar := sx_GetDateString ( "DATE_SOLD" ), '[' + Valtype( xVar ) + ']'
   ? 'sx_GetValue ("DATE_SOLD" ) =>', xVar := sx_GetValue ( "DATE_SOLD" ), '[' + Valtype( xVar ) + ']'
   ? 'sx_GetDouble( "PRICE" ) =>', xVar := sx_GetDouble( "PRICE" ), '[' + Valtype( xVar ) + ']'
   ? 'sx_GetInteger( "PRICE" ) =>', xVar := sx_GetInteger( "PRICE" ), '[' + Valtype( xVar ) + ']'
   ? 'sx_GetLong( "PRICE" ) =>', xVar := sx_GetLong( "PRICE" ), '[' + Valtype( xVar ) + ']'
   ? 'sx_GetLogical( "SOLD" ) =>', xVar := sx_GetLogical( "SOLD" ), '[' + Valtype( xVar ) + ']'
   ? 'sx_GetMemo( "NOTES" ) =>', xVar := sx_GetMemo( "NOTES" ), '[' + Valtype( xVar ) + ']'
