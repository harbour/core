/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   ... dbedit ...... :)
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { { "MYCHAR","C",10,0 }, { "MYDATE","D",8,0 }, { "MYNUM","N",8,0 }, { "MYLOGIC","L",1,0 } }
   LOCAL j, n := seconds(), nArea
   LOCAL nIndex

   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   IF FILE( "sixtest.nsx" )
      FERASE( "sixtest.nsx" )
   ENDIF

   CREATE DBF cFile STRUCT aStruct RDD SDENSX

   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE

   ? "------------------------------------------------------"
   ? "Test Appending 10,000 Blank Records and Creating Index"
   ? "------------------------------------------------------"
   ? "Area  : ", nArea
   ? "RDD   : " + sx_rddDriver( nArea )
   ? "Start : ", n

   FOR j := 1 TO 10000
      APPEND BLANK
      FieldPut( MYCHAR,   "NAME_" + PADL( j, 5, "0" ) )
      FieldPut( MYDATE, date() + j )
      FieldPut( MYNUM,  j )
      FieldPut( MYLOGIC,  j % 2 == 0 )
   NEXT

   COMMIT

   ?
   ? 'INDEX ON UPPER(MYCHAR) TO SIXTEST'
   ?
   INDEX ON UPPER( MYCHAR ) TO SIXTEST
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n, "Quick enough ?"
   ?
   ? "Now CLOSE ALL and ReOpen DBF ... Press any key ..."

   PAUSE

   CLOSE ALL

   // ReOpen Database
   USE cFile ALIAS MYALIAS RDD SDENSX VAR nArea EXCLUSIVE
   // Automatically Open index file if it has same name with DBF
   // Must explisitely set order 1 because we opened one index file
   ?
   ? 'sx_Alias          = ', sx_Alias()
   ? 'sx_SetOrder( 1 )  = ', sx_SetOrder( 1 ), ' was previous order'
   ? 'sx_IndexName( 1 ) = ', sx_IndexName( 1 )
   ?
   ? "Now will call SX_DBEDIT() ... Press any key ..."
   ?

   PAUSE
   cls
   GO TOP
   sx_dbedit()

   CLOSE ALL
