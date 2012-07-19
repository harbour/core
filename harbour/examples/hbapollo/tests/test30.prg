/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   SX_DBEVAL ........ AVERAGE TO ...
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 0 } }
   LOCAL j, n := seconds(), nArea, cPad
   LOCAL nIndex, cOldColor, nAverage1, nAverage2

   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   CREATE DBF cFile STRUCT aStruct RDD SDENSX

   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE

   ? "--------------------------------"
   ? "Polupating DBF with 1000 Records"
   ? "--------------------------------"
   ? "Area  : ", nArea
   ? "RDD   : " + sx_rddDriver( nArea )
   ? "Start : ", n

   FOR j := 1 TO 1000
      APPEND BLANK
      cPad := PADL( j, 5, "0" )
      FieldPut( MYCHAR, "NAME_" + cPad )
      FieldPut( MYDATE,  date() + j  )
      FieldPut( MYNUMBER1,   j * 10 )
      FieldPut( MYNUMBER2,   j * 20 )
   NEXT

   COMMIT

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now testing sx_DBEval with the following expressions :'
   ?
   ? 'AVERAGE MYNUMBER1, MYNUMBER2 TO nAverage1, nAverage2'
   ?
   ? 'Press any key ...'
   PAUSE
   n := seconds()
   ?
   ? 'Working .....'
   GO TOP
   AVERAGE MYNUMBER1, MYNUMBER2 TO nAverage1, nAverage2
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'nAverage1 =', nAverage1
   ? 'nAverage2 =', nAverage2
   ?
   ? 'Now testing sx_DBEval with the following expressions :'
   ?
   ? 'AVERAGE MYNUMBER1, MYNUMBER2 TO nAverage1, nAverage2 NEXT 100'
   ?
   ? 'Press any key ...'
   PAUSE
   n := seconds()
   ?
   ? 'Working .....'
   GO TOP
   AVERAGE MYNUMBER1, MYNUMBER2 TO nAverage1, nAverage2 NEXT 100
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'nAverage1 =', nAverage1
   ? 'nAverage2 =', nAverage2

   ?
   ? 'Now testing sx_DBEval with the following expressions :'
   ?
   ? 'AVERAGE MYNUMBER1, MYNUMBER2 TO nAverage1, nAverage2 ;'
   ? '    FOR sx_GetValue("MYDATE") <= CTOD("31/12/2008")'
   ?
   ? 'Press any key ...'
   PAUSE
   n := seconds()
   ?
   ? 'Working .....'
   GO TOP
   AVERAGE MYNUMBER1, MYNUMBER2 TO nAverage1, nAverage2 ;
      FOR sx_GetValue( "MYDATE" ) <= CTOD( "31/12/2008" )
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'nAverage1 =', nAverage1
   ? 'nAverage2 =', nAverage2

   CLOSE ALL
