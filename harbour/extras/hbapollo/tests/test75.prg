/*
 * $Id$
 */
/*
   sx_FieldCount    ()
   sx_FieldDecimals ()
   sx_FieldName     ()
   sx_FieldNum      ()
   sx_FieldType     ()
   sx_FieldWidth    ()
*/

#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL aStruct1 := { ;
      { "PART_NO"   , "C" , 10, 0 }, ;
      { "PART_NAME" , "C", 10, 0 }, ;
      { "PRICE"     , "N", 10, 2 }, ;
      { "DATE_SOLD" , "D", 8, 0 }, ;
      { "COST"      , "N", 10, 2 }, ;
      { "NOTES"     , "M", 10, 0 } }
   LOCAL nCount, i, cFieldName

   SET RDD SDENSX
   SET DATE ANSI
   CREATE TABLE "TEST1" STRUCT aStruct1
   USE "test1"
   APPEND BLANK

   ? 'nCount := sx_FieldCount() =>', nCount := sx_FieldCount()

   FOR i := 1 TO nCount
      ? 'sx_FieldName(' + ltrim( str( i ) ) + ')  =', cFieldName := sx_FieldName( i )
      ? 'sx_FieldType(' + cFieldName + ')     =', sx_FieldType( cFieldName )
      ? 'sx_FieldNum(' + cFieldName + ')      =', sx_FieldNum( cFieldName )
      ? 'sx_FieldWidth(' + cFieldName + ')    =', sx_FieldWidth( cFieldName )
      ? 'sx_FieldDecimals(' + cFieldName + ') =', sx_FieldDecimals( cFieldName )
      ?
   NEXT

   FOR i := 1 TO nCount
      ? 'sx_FieldName(' + ltrim( str( i ) ) + ')     =', cFieldName := sx_FieldName( i )
      ? 'sx_FieldType(' + ltrim( str( i ) ) + ')     =', sx_FieldType( i )
      ? 'sx_FieldNum(' + ltrim( str( i ) ) + ')      =', sx_FieldNum( i )
      ? 'sx_FieldWidth(' + ltrim( str( i ) ) + ')    =', sx_FieldWidth( i )
      ? 'sx_FieldDecimals(' + ltrim( str( i ) ) + ') =', sx_FieldDecimals( i )
      ?
   NEXT
