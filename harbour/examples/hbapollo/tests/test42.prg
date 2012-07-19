/*
 * $Id$
 */
/*
   Enhanced DBEval, and alias assignment
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile1  := "test_1.dbf"
   LOCAL cFile2  := "test_2.dbf"
   LOCAL cFile3  := "test_3.dbf"

   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 0 } }

   SX_RDDSETDEFAULT( "SDEFOX" )
   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   CREATE DBF cFile1 STRUCT aStruct
   USE cFile1 ALIAS MYALIAS_1 EXCLUSIVE

   CREATE DBF cFile2 STRUCT aStruct
   USE cFile2 ALIAS MYALIAS_2 EXCLUSIVE

   CREATE DBF cFile3 STRUCT aStruct
   USE cFile3 ALIAS MYALIAS_3 EXCLUSIVE

   APPEND BLANK TO MYALIAS_1 100
   APPEND BLANK TO MYALIAS_2 200
   APPEND BLANK TO MYALIAS_3 300

   ? 'sx_LastRec("MYALIAS_1")=', sx_LastRec( "MYALIAS_1" )
   ? 'sx_LastRec("MYALIAS_2")=', sx_LastRec( "MYALIAS_2" )
   ? 'sx_LastRec("MYALIAS_3")=', sx_LastRec( "MYALIAS_3" )

   ? 'GOTO 100 ALIAS MYALIAS_1'
   GOTO 100 ALIAS MYALIAS_1

   ? 'REPLACE MYCHAR ALIAS MYALIAS_1 WITH "my_alias_3"'
   REPLACE MYCHAR ALIAS MYALIAS_1 WITH "my_alias_3"

   // SX_REPLACE("MYCHAR","My_Alias_3","MYALIAS_1")

   ? 'DELETE FOR sx_RecNo() <= 50 AREA MYALIAS_1'
   ? 'DELETE FOR sx_RecNo() <= 70 AREA MYALIAS_2'
   ? 'SX_GETVALUE("MYCHAR","MYALIAS_1")=', SX_GETVALUE( "MYCHAR", "MYALIAS_1" )

   ? 'sx_Recno("MYALIAS_1") =', sx_Recno( "myalias_1" )

   DELETE FOR sx_RecNo() <= 50 AREA MYALIAS_1
   DELETE FOR sx_RecNo() <= 70 AREA MYALIAS_2

   ? 'PACK MYALIAS_1'
   ? 'PACK MYALIAS_2'

   PACK MYALIAS_1
   PACK MYALIAS_2

   ? 'sx_LastRec("MYALIAS_1")=', sx_LastRec( "MYALIAS_1" )
   ? 'sx_LastRec("MYALIAS_2")=', sx_LastRec( "MYALIAS_2" )
   ? 'sx_LastRec("MYALIAS_3")=', sx_LastRec( "MYALIAS_3" )

   ? 'ZAP MYALIAS_1'
   ? 'ZAP MYALIAS_2'

   ZAP MYALIAS_1
   ZAP MYALIAS_2

   ? 'sx_LastRec("MYALIAS_1")=', sx_LastRec( "MYALIAS_1" )
   ? 'sx_LastRec("MYALIAS_2")=', sx_LastRec( "MYALIAS_2" )
   ? 'sx_LastRec("MYALIAS_3")=', sx_LastRec( "MYALIAS_3" )
