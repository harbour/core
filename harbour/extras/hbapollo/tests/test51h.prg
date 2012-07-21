/*
 * $Id$
 */
/*
  For comparison with Harbour/xHarbour
  Using Harbour RDD ....
*/

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 10, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 2 } }
   LOCAL cApplication

   DBCREATE( ( CFILE ), ASTRUCT )
   USE SIXTEST ALIAS MYALIAS EXCLUSIVE
   TEST_1()
   TEST_2()

   //---

PROC TEST_2()

   LOCAL n

   DBGotop()

   ? 'Now On RecNo =>', ltrim( str( RecNo() ) ), "Press any key ..."
   INKEY( 0 )

   n := seconds()
   WHILE !Eof()
      DBSkip()
   ENDDO

   ?
   ? "Skipping ..."
   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now On RecNo =>', ltrim( str( RecNo() ) )
   ?
   ? 'BROWSE ... Press any key ..'
   dbgotop()
   Browse()

   //---

PROC TEST_1()

   LOCAL j, n := seconds(), nArea, cPad

   ? "----------------------------------"
   ? "Polupating DBF with 10,000 Records"
   ? "----------------------------------"
   ? "Start : ", n

   FOR j := 1 TO 10000
      APPEND BLANK
      cPad := PADL( j, 5, "0" )
      MYALIAS -> MYCHAR := "NAME_" + cPad
      MYALIAS -> MYDATE :=  date() + j
      MYALIAS -> MYNUMBER1 :=   j * 10
      MYALIAS -> MYNUMBER2 :=   j * 20 / 3
   NEXT

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?

   RETURN
