/*
 * $Id$
 */
/*
   Append Many Blank Records at Once
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
   LOCAL j, n := seconds()

   SX_RDDSETDEFAULT( "SDEFOX" )
   SET CENTURY ON
   SET DATE ANSI
   ?
   ? 'Using APPEND BLANK 10000 ... Press any key ...'
   PAUSE

   n := seconds()
   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS
   APPEND BLANK 10000

   ? "---------------------------------"
   ? "Polupating DBF with 10000 Records"
   ? "---------------------------------"
   ? "RDD   : " + sx_rddDriver( )
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'sx_RecCount() =', sx_RecCount()

   CLOSE ALL

   ?
   ? 'Using conventional FOR-NEXT LOOP ... Press any key ...'
   PAUSE

   n := seconds()
   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS

   ? "---------------------------------"
   ? "Polupating DBF with 10000 Records"
   ? "---------------------------------"
   ? "RDD   : " + sx_rddDriver()
   ? "Start : ", n

   FOR j := 1 TO 10000
      APPEND BLANK TO MYALIAS
   NEXT

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'sx_RecCount() =', sx_RecCount()

   CLOSE ALL
