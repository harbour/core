/*
 * $Id$
 */
/*
  sx_FieldCount    ( nArea | cAlias )
  sx_FieldDecimals ( cFieldName, nArea | cAlias )
  sx_FieldName     ( iFieldNum, nArea | cAlias )
  sx_FieldNum      ( cFieldName, nArea | cAlias  )
  sx_FieldOffset   ( cFieldName, nArea | cAlias  )
  sx_FieldType     ( cFieldName, nArea | cAlias  )
  sx_FieldWidth    ( cFieldName, nArea | cAlias  )
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile1   := "TEST1.DBF"
   LOCAL cFile2   := "TEST2.DBF"
   LOCAL aStruct1 := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYLOGIC"   , "L", 1, 0 }, ;
      { "MYNUM1"    , "N", 8, 0 }, ;
      { "MYNUM2"    , "N", 7, 2 }, ;
      { "MYMEMO"    , "M", 10, 0 } }
   LOCAL aStruct2 := { ;
      { "YOURCHAR"  , "C", 25, 0 }, ;
      { "YOURDATE"  , "D", 8, 0 }, ;
      { "YOURLOGIC" , "L", 1, 0 }, ;
      { "YOURNUM0"  , "N", 10, 3 }, ;
      { "YOURNUM1"  , "N", 8, 0 }, ;
      { "YOURNUM2"  , "N", 9, 2 }, ;
      { "YOURMEMO"  , "M", 10, 0 } }
   LOCAL MYFIELD, YOURFIELD, I, J

   SX_RDDSETDEFAULT( "SDEFOX" )

   SET EPOCH 1950
   SET DATE ANSI

   CREATE DBF cFile1 STRUCT aStruct1
   USE cFile1 ALIAS MYALIAS EXCLUSIVE

   CREATE DBF cFile2 STRUCT aStruct2
   USE cFile2 ALIAS YOURALIAS EXCLUSIVE

   i := sx_FieldCount( "MYALIAS" )
   ?
   ? 'sx_FieldCount( "MYALIAS" ) = ', padl( i, 4 )
   ?
   for j := 1 TO i
      myField := sx_FieldName ( j, "MYALIAS" )
      ? padr( myField, 10 ), ;
         padl( sx_FieldNum      ( myfield, "MYALIAS" ), 4 ), ;
         padl( sx_FieldType     ( myfield, "MYALIAS" ), 3 ), ;
         padl( sx_FieldWidth    ( myfield, "MYALIAS" ), 4 ), ;
         padl( sx_FieldDecimals ( myfield, "MYALIAS" ), 4 ), ;
         padl( sx_FieldOffset   ( myfield, "MYALIAS" ), 4 )
   next
   ?
   i := sx_FieldCount( "YOURALIAS" )
   ? 'sx_FieldCount( "YOURALIAS" ) = ', padl( i, 2 )
   ?
   for j := 1 TO i
      myField := sx_FieldName ( j, "YOURALIAS" )
      ? padr( myField, 10 ), ;
         padl( sx_FieldNum      ( myfield, "YOURALIAS" ), 4 ), ;
         padl( sx_FieldType     ( myfield, "YOURALIAS" ), 3 ), ;
         padl( sx_FieldWidth    ( myfield, "YOURALIAS" ), 4 ), ;
         padl( sx_FieldDecimals ( myfield, "YOURALIAS" ), 4 ), ;
         padl( sx_FieldOffset   ( myfield, "YOURALIAS" ), 4 )
   next
   CLOSE ALL
