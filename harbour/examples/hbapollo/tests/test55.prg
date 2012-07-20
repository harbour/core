/*
 * $Id$
 */
/*
  sx_IndexClose()
  This function is only valid for RDD SDENSXDBT
  After sx_IndexClose(), workarea is into 0 index, must sx_setOrder()
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
   LOCAL aGet, nTagCount

   // SET ERRORLEVEL 0
   SET RDD SDENSXDBT
   SET EPOCH 1950
   SET DATE ANSI
   SET TRIM ON

   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS VAR nArea EXCLUSIVE
   APPEND BLANK

   REPLACE MYCHAR  WITH "Harbour Power"
   REPLACE MYDATE  WITH DATE()
   REPLACE MYNUM0  WITH 10000
   REPLACE MYNUM2  WITH 250.25
   REPLACE MYNUM3  WITH 10000.123
   REPLACE MYLOGIC WITH .T.
   REPLACE MYMEMO  WITH "This is some text but you can use MEMOREAD()"

   COMMIT

   INDEX ON MYCHAR TO "TESTNTX"
   ?
   ? '---------------------------------------------'
   ? 'INDEX ON MYCHAR TO "TESTNTX"'
   ? 'Sx_SetOrder(1) =', padl( Sx_SetOrder( 1 ), 1 )
   ? 'sx_IndexOrd() =', padl( sx_IndexOrd(), 1 )
   ? 'Sx_IndexKey() =', Sx_IndexKey()
   ? 'Sx_IndexKeyField() =', Sx_IndexKeyField()
   ? '---------------------------------------------'
   INDEX ON DTOS( MYDATE ) TO "TESTNTX1"
   ? 'INDEX ON DTOS(MYDATE) TO "TESTNTX1"'
   ? 'Sx_IndexKey() =', Sx_IndexKey()
   ? 'Sx_IndexKeyField() =', Sx_IndexKeyField()
   ? '---------------------------------------------'
   ? 'Sx_Tagcount() =', padl( nTagCount := Sx_Tagcount(), 1 )
   FOR j := 1 TO nTagCount
      ? 'sx_TagName(' + ltrim( str( j ) ) + ')   =', sx_TagName( j )
      ? 'sx_IndexName(' + ltrim( str( j ) ) + ') =', sx_IndexName( j )
   NEXT
   ? '---------------------------------------------'
   ? 'sx_IndexOrd() =', padl( sx_IndexOrd(), 1 )
   ? '---------------------------------------------'
   ? 'sx_IndexClose() =', sx_IndexClose()
   ? 'Sx_Tagcount()   =', padl( nTagCount := Sx_Tagcount(), 1 )
   ? 'Sx_SetOrder(1)  =', padl( Sx_SetOrder( 1 ), 1 )
   FOR j := 1 TO nTagCount
      ? 'sx_TagName(' + ltrim( str( j ) ) + ')   =', sx_TagName( j )
      ? 'sx_IndexName(' + ltrim( str( j ) ) + ') =', sx_IndexName( j )
   NEXT
   ? 'sx_IndexOrd() =', padl( sx_IndexOrd(), 1 )
   ? 'Sx_IndexKey() =', Sx_IndexKey()
   ? 'Sx_IndexKeyField() =', Sx_IndexKeyField()
   ? '---------------------------------------------'
   ? 'sx_IndexClose() =', sx_IndexClose()
   ? 'Sx_Tagcount()   =', padl( nTagCount := Sx_Tagcount(), 1 )
   // Should be runtime error here because no index is open
   ? 'Sx_SetOrder(1)  =', padl( Sx_SetOrder( 1 ), 1 )
