/*
 * $Id$
 */

FUNCTION Main()

   USE TEST

   REPLACE Age WITH 1
   ? FIELD->Age

   //REPLACE 1->Age WITH 2 // Todo: complete support in harbour.y - AliasAddInt()
   //? FIELD->Age

   //REPLACE 1.5->Age WITH 3 // Will produce "Invalid alias expression"
   //? FIELD->Age

   REPLACE TEST->Age WITH 4
   ? FIELD->Age

   TEST->Age := 5
   ? FIELD->Age

   TEST->( FieldPut( FieldPos( 'AGE' ), 6 ) )
   ? FIELD->Age

   dbCloseArea()

   SELE 2

   USE test

// ? ("0")->FIRST
   ? ("B")->FIRST
   ? ("2")->FIRST
   ? 2->FIRST
   ? B->FIRST

   Inkey( 0 )

// ? ("0")->FIRST
   ? SELECT()
   ? SELECT( 1 )
   ? SELECT( 2 )

   ? "0", SELECT( "0" )
   ? "1", SELECT( "1" )
   ? "2", SELECT( "2" )
   ? "A", SELECT( "A" )
   ? "B", SELECT( "B" )
   ? "C", SELECT( "C" )
   ? "D", SELECT( "D" )
   ? "E", SELECT( "E" )
   ? "F", SELECT( "F" )
   ? "G", SELECT( "G" )
   ? "H", SELECT( "H" )
   ? "I", SELECT( "I" )
   ? "J", SELECT( "J" )
   ? "K", SELECT( "K" )
   ? "L", SELECT( "L" )
   ? "M", SELECT( "M" )
   ? "N", SELECT( "N" )
   ? "O", SELECT( "O" )
   ? "P", SELECT( "P" )
   ? "Q", SELECT( "Q" )
   ? "R", SELECT( "R" )
   ? "S", SELECT( "S" )
   ? "T", SELECT( "T" )
   ? "U", SELECT( "U" )
   ? "V", SELECT( "V" )
   ? "W", SELECT( "W" )
   ? "X", SELECT( "X" )
   ? "Y", SELECT( "Y" )
   ? "Z", SELECT( "Z" )

   Inkey( 0 )

   ? ""  , dbSelectArea()     , SELECT()
   ? ""  , dbSelectArea( NIL ), SELECT()
   ? ""  , dbSelectArea( "" ) , SELECT()
   ? " " , dbSelectArea( " " ), SELECT()
   ? "0" , dbSelectArea( "0" ), SELECT()
   ? "1" , dbSelectArea( "1" ), SELECT()
   ? "2" , dbSelectArea( "2" ), SELECT()
   ? "A" , dbSelectArea( "A" ), SELECT()
   ? "B" , dbSelectArea( "B" ), SELECT()
   ? "C" , dbSelectArea( "C" ), SELECT()
   ? "D" , dbSelectArea( "D" ), SELECT()
   ? "E" , dbSelectArea( "E" ), SELECT()
   ? "F" , dbSelectArea( "F" ), SELECT()
   ? "G" , dbSelectArea( "G" ), SELECT()
   ? "H" , dbSelectArea( "H" ), SELECT()
   ? "I" , dbSelectArea( "I" ), SELECT()
   ? "J" , dbSelectArea( "J" ), SELECT()
   ? "K" , dbSelectArea( "K" ), SELECT()
   ? "L" , dbSelectArea( "L" ), SELECT()
   ? "M" , dbSelectArea( "M" ), SELECT()
   ? "Z" , dbSelectArea( "Z" ), SELECT()
   ? "AA", dbSelectArea( "AA" ), SELECT()

return NIL
