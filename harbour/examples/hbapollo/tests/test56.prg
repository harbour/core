/*
 * $Id$
 */
/*
  sx_TagInfo()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "FIRST", "C", 25, 0 }, ;
      { "LAST", "C", 25, 0 }, ;
      { "HIREDATE", "D", 8, 0 } }
   LOCAL j, n := seconds(), nArea, i
   LOCAL aGet, nTagCount, aTagInfo

   SET RDD SDEFOX
   SET EPOCH 1950
   SET DATE ANSI
   SET TRIM ON

   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS VAR nArea EXCLUSIVE
   APPEND BLANK

   sx_IndexTag ( "MYTEST", "FIRST_TAG", "FIRST", 0, .F. , "LEFT(FIRST,1)='A'" )
   sx_IndexTag ( "MYTEST", "LAST_TAG", "LAST", 0, .T. , "RIGHT(LAST,1)='Z'" )
   sx_IndexTag ( "MYTEST", "DATE_TAG", "DTOS(HIREDATE)", 0, .T. , )
   ?
   ? 'sx_TagCount() =', sx_TagCount()
   ?
   ? 'aTagInfo := TagInfo()'
   aTagInfo := TagInfo()
   for j := 1 TO len( aTagInfo )
      ? 'TagName          :', aTagInfo[ j, 1 ]
      ? 'Index Expression :', aTagInfo[ j, 2 ]
      ? 'Index Condition  :', aTagInfo[ j, 3 ]
      ? 'Index Type       :', aTagInfo[ j, 4 ]
      ? 'Descending       :', aTagInfo[ j, 5 ]
      ? 'RYO              :', aTagInfo[ j, 6 ]
      ? 'Index File Name  :', aTagInfo[ j, 7 ]
      ? 'Index Key Field  :', aTagInfo[ j, 8 ]
      ?
   next

   ? 'aTagInfo := sx_TagInfo(2)'
   aTagInfo := sx_TagInfo( 2 )
   for j := 1 TO len( aTagInfo )
      ? 'TagName          :', aTagInfo[ j, 1 ]
      ? 'Index Expression :', aTagInfo[ j, 2 ]
      ? 'Index Condition  :', aTagInfo[ j, 3 ]
      ? 'Index Type       :', aTagInfo[ j, 4 ]
      ? 'Descending       :', aTagInfo[ j, 5 ]
      ? 'RYO              :', aTagInfo[ j, 6 ]
      ? 'Index File Name  :', aTagInfo[ j, 7 ]
      ? 'Index Key Field  :', aTagInfo[ j, 8 ]
      ?
   next

   ? 'aTagInfo := sx_TagInfo( "FIRST_TAG" )'
   aTagInfo := sx_TagInfo( "FIRST_TAG" )
   for j := 1 TO len( aTagInfo )
      ? 'TagName          :', aTagInfo[ j, 1 ]
      ? 'Index Expression :', aTagInfo[ j, 2 ]
      ? 'Index Condition  :', aTagInfo[ j, 3 ]
      ? 'Index Type       :', aTagInfo[ j, 4 ]
      ? 'Descending       :', aTagInfo[ j, 5 ]
      ? 'RYO              :', aTagInfo[ j, 6 ]
      ? 'Index File Name  :', aTagInfo[ j, 7 ]
      ? 'Index Key Field  :', aTagInfo[ j, 8 ]
      ?
   next

FUNCTION TagInfo( nTag )

   RETURN sx_TagInfo( nTag )
