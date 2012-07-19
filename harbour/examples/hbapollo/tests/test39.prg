/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   LOCATE ...
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

   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   IF File( "myText.Txt" )
      FErase( "myText.Txt" )
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
   NEXT

   COMMIT

   ? "End   : ", seconds()
   ? "Time  : ", seconds() - n

   ?
   ? 'GO TOP'
   ? [sx_locate( 'MYCHAR = "NAME_00100"', .F., .F. )]
   GO TOP
   // Syntax : SX_LOCATE( cExpression, lBackward, lContinue ) => RecNo()
   // Locate forward
   ? '->', sx_locate( 'MYCHAR = "NAME_00100"', .F. , .F. )
   ?
   ? [sx_locate( 'MYCHAR = "NAME_00035"', .T., .F. )]
   ? '->', sx_locate( 'MYCHAR = "NAME_00035"', .T. , .F. )
   ?

   ? 'GO BOTTOM'
   ? [sx_locate( 'MYCHAR = "NAME_00100"', .T., .F. )]
   GO BOTTOM
   // Locate backward
   ? '->', sx_locate( 'MYCHAR = "NAME_00100"', .T. , .F. )
   ?
   ? [sx_locate( 'MYCHAR = "NAME_00800"', .F., .F. )]
   ? '->', sx_locate( 'MYCHAR = "NAME_00800"', .F. , .F. )
