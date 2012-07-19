/*
 * $Id$
 */
/*
  Manipulation of Database Field Values
  sx_EvalTest()
  sx_EvalString()
  sx_EvalNumeric()
  sx_EvalLogical()
*/
#include "sixapi.ch"

#define EVAL_CHARACTER   1
#define EVAL_NUMERIC     2
#define EVAL_LOGICAL     3
#define EVAL_DATESTRING  4

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile := "TEST.DBF"
   LOCAL i
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYLOGIC"   , "L", 1, 0 }, ;
      { "MYNUMBER1" , "N", 8, 0 }, ;
      { "MYNUMBER2" , "N", 8, 0 } }

   SX_RDDSETDEFAULT( "SDEFOX" )
   SET EPOCH 1950
   SET DATE "DD/MM/YYYY"

   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS EXCLUSIVE
   APPEND BLANK
   REPLACE MYCHAR WITH "JUST TEST", ;
      MYDATE WITH DATE(), ;
      MYLOGIC WITH .T. , ;
      MYNUMBER1 WITH 111 , ;
      MYNUMBER2 WITH 222

   //--- sxApi way ....
   ? 'sxApi way ...'
   ?
   ? "TEST" $ sx_GetValue( "MYCHAR" )
   ? IF( sx_GetValue( "MYLOGIC" ), "TRUE", "FALSE" )
   ? sx_GetValue( "MYNUMBER1" ) + sx_GetValue( "MYNUMBER2" )
   ? sx_GetValue( "MYDATE" ) + 30
   ?
   //--- Apollo way ...
   ? 'Apollo way ...'
   ?
   ? sx_EvalLogical( '"TEST" $  MYCHAR' )
   ? sx_EvalNumeric( 'MYNUMBER1 + MYNUMBER2' )
   ? sx_EvalString( 'MYCHAR + " " + DTOS( MYDATE )' )
   ? STOD( sx_EvalString( 'MYDATE + 30' ) )
   ?
   ? 'sx_Evaltest() ....'
   ?
   ? sx_EvalTest( 'MYCHAR + " " + DTOS( MYDATE )' )
   ? sx_EvalTest( 'MYNUMBER1 + MYNUMBER2' )
   ? sx_EvalTest( '"TEST" $  MYCHAR' )
   ? sx_EvalTest( 'MYDATE + 30' )

   CLOSE ALL
