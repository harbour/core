/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { { "TEST","C",10,0 }, { "MYDATE","C",8,0 }, { "MYNUM","N",8,0 }, { "MYLOGIC","L",1,0 } }
   LOCAL j, n := seconds(), nArea

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
      FieldPut( TEST, "NAME_" + PADL( j, 5, "0" ) )
   NEXT

   COMMIT

   ?
   ? 'INDEX ON UPPER(TEST) TO SIXTEST'
   ?
   INDEX ON UPPER( TEST ) TO SIXTEST

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Seek( "NAME_07567" ) =', Seek( "NAME_07567" )
   ? 'Found()              =', Found()
   ? 'Eof()                =', Eof()
   ? 'RecNo()              =', RecNo()
   ?
   ? 'Seek( "NOTEXIST" )   =', Seek( "NOTEXIST" )
   ? 'Found()              =', Found()
   ? 'Eof()                =', Eof()
   ? 'RecNo()              =', RecNo()

   CLOSE ALL
