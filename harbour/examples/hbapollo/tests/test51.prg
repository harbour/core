/*
 * $Id$
 */
/*
   Testing Sx_CommitLevel()
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN( nLevel )

   IF nLevel == NIL
      nLevel := 2
   ELSE
      nLevel := val( nLevel )
      IF nLevel < 0 .OR. nLevel > 2
         ? 'Usage : TEST51 <nLevel>, where nLevel: 0, 1 or 2'
         RETURN
      ENDIF
   ENDIF

   ?
   ? 'Set Commit Level To ' + ltrim( str( nLevel ) ), sx_CommitLevel( nLevel )
   ?
   ? 'Appending 10,000 Records Using Commit Level ' + ltrim( str( nLevel ) )
   ? 'Working ...'
   ?
   Test_1()
   ?
   Test_2()
   ?
   GOTOP
   CLS
   ?
   ?
   ?
   ?
   ? 'BROWSE ... Press any key ...'
   PAUSE
   CLS
   BROWSE
   CLOSE ALL
   CLS

PROC Test_2( nCommit )

   LOCAL n

   sx_Gotop()

   ? 'Now On RecNo =>', ltrim( str( sx_RecNo() ) ), "Press any key ..."
   PAUSE

   n := seconds()
   WHILE !sx_Eof()
      sx_Skip()
   ENDDO

   ?
   ? "Skipping ..."
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now On RecNo =>', ltrim( str( sx_RecNo() ) )
   ?

FUNCTION Test_1( nCommit )

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 10, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 2 } }
   LOCAL j, n := seconds(), nArea, cPad
   LOCAL cApplication

   SET EPOCH 1950
   SET DATE ANSI

   CREATE DBF cFile STRUCT aStruct RDD SDENTX
   USE cFile ALIAS MYALIAS RDD SDENTX VAR nArea EXCLUSIVE
   APPEND BLANK 10000
   GO TOP
   ? "----------------------------------"
   ? "Polupating DBF with 10,000 Records"
   ? "----------------------------------"
   ? "Area         : ", nArea
   ? "RDD          : " + sx_rddDriver( nArea )
   ? "Commit Level : ", sx_GetCommitLevel( nArea )
   ? "Start        : ", n
   j := 0
   WHILE !sx_Eof()
      cPad := PADL( ++ j, 5, "0" )
      sx_Replace( "MYCHAR",   "NAME_" + cPad )
      sx_Replace( "MYDATE",    date() + j    )
      sx_Replace( "MYNUMBER1", j * 10        )
      sx_Replace( "MYNUMBER2", j * 20 / 3    )
      sx_skip( 1 )
   ENDDO

   ? "End          : ", seconds()
   ? "Time         : ", seconds() - n

   RETURN NIL
