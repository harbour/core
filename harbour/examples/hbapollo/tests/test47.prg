/*
 * $Id$
 */
/*
  How to Encrypt/Decrypt DBF on The Fly
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile1   := "TEST1.DBF"
   LOCAL aStruct1 := { ;
      { "MYCHAR1"   , "C", 15, 0 }, ;
      { "MYCHAR2"   , "C", 15, 0 }, ;
      { "MYCHAR3"   , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYLOGIC"   , "L", 1, 0 }, ;
      { "MYNUM1"    , "N", 8, 0 }, ;
      { "MYNUM2"    , "N", 7, 2 }, ;
      { "MYMEMO"    , "M", 10, 0 } }

   CLS
   SX_RDDSETDEFAULT( "SDEFOX" )
   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   ?
   ? 'Creating DBF ...'
   CREATE DBF cFile1 STRUCT aStruct1
   ? 'Opening File ...'
   USE cFile1 ALIAS MYALIAS EXCLUSIVE
   ? 'Setting Password ...'
   SX_SETPASSWORD( "HARBOUR" ) // Set Password On Current Area - MAX 8 characters
   ? 'Append Blank ...'
   APPEND BLANK
   ? 'Update Record ...'
   REPLACE MYCHAR1 WITH "THIS IS MYCHAR1", ;
      MYCHAR2 WITH "THIS IS MYCHAR2", ;
      MYCHAR3 WITH "THIS IS MYCHAR3", ;
      MYDATE  WITH  DATE(), ;
      MYLOGIC WITH  .T. , ;
      MYNUM1  WITH  100, ;
      MYNUM2  WITH  200.50, ;
      MYMEMO  WITH  "Harbour Power!"
   ?
   ? 'Now BROWSE .. Press any key ...'
   PAUSE
   CLS
   BROWSE
   CLOSE ALL
   CLS
   ?
   ? 'Browse with Wrong PASSWORD .. Press any key ...'
   PAUSE
   USE cFile1 ALIAS MYALIAS EXCLUSIVE
   SX_SETPASSWORD( "XHARBOUR" ) // Wrong Password
   CLS
   BROWSE
   CLOSE ALL

   CLS
   ?
   ? 'Browse with Correct PASSWORD .. Press any key ...'
   PAUSE
   USE cFile1 ALIAS MYALIAS EXCLUSIVE
   SX_SETPASSWORD( "HARBOUR" ) // Correct Password
   CLS
   BROWSE

   CLOSE ALL
