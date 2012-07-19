/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   SX_DBEVAL ........ ANOTHER TESTS ... COUNT TO ... NEXT n
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR" , "C", 15, 0 }, ;
      { "MYDATE" , "D", 8, 0 }, ;
      { "MYNUM"  , "N", 8, 0 }, ;
      { "MYMEMO" , "M", 10, 0 }, ;
      { "MYLOGIC", "L", 1, 0 } }
   LOCAL j, n := seconds(), nArea, cPad
   LOCAL nIndex, cOldColor, nCount

   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   CREATE DBF cFile STRUCT aStruct RDD SDENSX

   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE

   ? "---------------------------------"
   ? "Polupating DBF with 10000 Records"
   ? "---------------------------------"
   ? "Area  : ", nArea
   ? "RDD   : " + sx_rddDriver( nArea )
   ? "Start : ", n

   FOR j := 1 TO 10000
      APPEND BLANK
      cPad := PADL( j, 5, "0" )
      FieldPut( MYCHAR, "NAME_" + cPad )
      FieldPut( MYDATE,  date() + j )
      FieldPut( MYNUM,   j )
      FieldPut( MYMEMO,  "This is Record Number " + cPad )
      FieldPut( MYLOGIC, j % 2 == 0 )
   NEXT

   COMMIT

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now testing sx_DBEval with the following expressions :'
   ?
   ? 'COUNT TO nCount FOR sx_GetValue("MYDATE") >= CTOD("01/01/2008")   ;'
   ? '                .AND. sx_GetValue("MYDATE") <= CTOD("31/12/2008") ;'
   ? '                NEXT 365'
   ?
   ? 'Press any key ...'
   PAUSE
   n := seconds()
   ?
   ? 'Working .....'
   GO TOP
   COUNT TO nCount FOR sx_GetValue( "MYDATE" ) >= CTOD( "01/01/2008" )  ;
      .AND. sx_GetValue( "MYDATE" ) <= CTOD( "31/12/2008" ) ;
      NEXT 365
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'nCount =', nCount
   CLOSE ALL
