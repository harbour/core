/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   SX_DBEVAL ........ ANOTHER TESTS ...
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
   LOCAL nIndex, cOldColor

   SET DELETED ON
   SET COMMITLEVEL 2
   SET EPOCH 1950
   SET DATE "dd/mm/yyyy"

   CREATE DBF cFile STRUCT aStruct RDD SDENSX

   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE

   ? "---------------------------------"
   ? "Polupating DBF with 10000 Records"
   ? "---------------------------------"
   ? "Area  : ", nArea
   ? "RDD   : " + sx_rddDriver( nArea )
   ? "Start : ", n

   APPEND BLANK 10000
   j := 0
   SX_DBEVAL( {||;
      cPad := PADL( ++j, 5, "0" ), ;
      SX_REPLACEEX( { ;
      { "MYDATE", date() + j }, ;
      { "MYCHAR", "NAME_" + cPad }, ;
      { "MYNUM",  j }, ;
      { "MYMEMO", "This is Record Number " + cPad }, ;
      { "MYLOGIC", j % 2 == 0 } } ) } )

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now will browse DBF file ... Press any key ...'
   PAUSE
   BROWSE

   cls
   ?
   ? 'Now testing sx_DBEval with the following expressions :'
   ?
   ? 'DELETE FOR !sx_GetValue("MYLOGIC")'
   ?
   ? 'Press any key ...'
   PAUSE
   n := seconds()
   ?
   ? 'Working .....'

   DELETE FOR !sx_GetValue( "MYLOGIC" )

   COMMIT

   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now will browse DBF file ... Press any key ...'
   PAUSE

   BROWSE

   CLOSE ALL
