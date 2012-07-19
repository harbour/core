/*
 * $Id$
 */
/*
   index file properties .....
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { { "MYCHAR","C",10,0 }, { "MYDATE","D",8,0 }, { "MYNUM","N",8,0 }, { "MYLOGIC","L",1,0 } }
   LOCAL j, n := seconds(), nArea
   LOCAL nOrd

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
   ? 'INDEX ON UPPER(MYCHAR) TO SIXTEST FOR MYNUM > 5000'
   ?
   INDEX ON UPPER( MYCHAR ) TO SIXTEST FOR MYNUM > 5000
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n, "Quick enough ?"
   ?
   ? 'sx_IndexCondition()   = ', sx_IndexCondition()
   ? 'sx_IndexKey()         = ', sx_IndexKey()
   ? 'sx_IndexKeyField()    = ', sx_IndexKeyField()
   ? 'nOrd := sx_IndexOrd() = ', nOrd := sx_IndexOrd()
   ? 'sx_IndexName( nOrd )  = ', sx_IndexName( nOrd )
   ? 'sx_IndexType()        = ', sx_IndexType()
   ?
   ? 'sx_IndexType() returns one of the followings:'
   ? 'INDEX_STANDARD            1'
   ? 'INDEX_STANDARD_UNIQUE     2'
   ? 'INDEX_CONDITIONAL         3'
   ? 'INDEX_CONDITIONAL_UNIQUE  4'
