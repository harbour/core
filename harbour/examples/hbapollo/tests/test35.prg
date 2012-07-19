/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   COPY TO DELIMITED WITH FIELDS FOR NEXT .....
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "c:\windows\temp\sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 0 }, ;
      { "MYMEMO"    , "M", 10, 0 } }   // Memo field will not be printed
   LOCAL j, n := seconds(), nArea, cPad
   LOCAL nIndex, cOldColor, nAverage1, nAverage2, cApplication

   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   IF File( "c:\windows\temp\myText.Txt" )
      FErase( "c:\windows\temp\myText.Txt" )
   ENDIF

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
      FieldPut( MYMEMO,      "My Memo" ) // Memo Field Will Not Be Printed
   NEXT

   COMMIT

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?
   ? 'Now will COPY TO ... Press any key ...'
   PAUSE
   ?
   n := seconds()
   ? 'COPY TO mytext.txt FIELDS MYCHAR, MYDATE, MYNUMBER1, MYNUMBER2, MYMEMO ;'
   ? '   FOR sx_GetValue( "MYDATE" ) <= CTOD("31/12/2008") ;'
   ? '   NEXT 100 DELIMITED WITH CHR(9)'
   ? 'Please note that MEMO field will not be printed'

   GO TOP
   // if no FIELDS clause is used, all fields will be printed
   // COPY TO mytext.txt ;
   // DELIMITED clause should come last
   COPY TO c:\windows\temp\mytext.txt FIELDS MYCHAR, MYDATE, MYNUMBER1, MYNUMBER2, MYMEMO ;
      FOR sx_GetValue( "MYDATE" ) <= CTOD( "31/12/2008" ) ;
      NEXT 100 DELIMITED WITH CHR( 9 )

   ?
   ? "Start : ", n
   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n
   ?

   CLOSE ALL

   // if file("c:\windows\temp\mytext.txt")
   //   ? 'File c:\windows\temp\mytext.txt created ...'
   // endif

   IF !empty( cApplication := appReg( "txt" ) )
      ? 'Now will browse text file ... Press any key ...'
      PAUSE
      IF File( "c:\windows\temp\myText.Txt" )
         __Run( cApplication + " " + "c:\windows\temp\mytext.txt" )
      ENDIF
   ENDIF
