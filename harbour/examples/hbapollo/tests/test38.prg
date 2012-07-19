/*
 * $Id$
 */
/*
   Copying records to a new DBF files from current work area ....
   SX_DBSTRUCT() ...
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "c:\windows\temp\sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 2 }, ;
      { "MYLOGICAL" , "L", 1, 0 }, ;
      { "MYMEMO"    , "M", 10, 0 } }
   LOCAL j, aStructure

   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   IF File( "c:\windows\temp\myText.Txt" )
      FErase( "c:\windows\temp\myText.Txt" )
   ENDIF

   CREATE DBF cFile STRUCT aStruct

   USE cFile ALIAS MYALIAS
   ?
   ? 'SX_DBSTRUCT() Test ...'
   ?
   ? 'len( aStructure ) :=', ltrim( str( len(aStructure := sx_dbstruct() ) ) )
   ?
   for j := 1 TO len( aStructure )
      ? padr( aStructure[j][1], 16 ) + aStructure[j][2] + str( aStructure[j][3] ) + str( aStructure[j][4] )
   next

   CLOSE ALL
