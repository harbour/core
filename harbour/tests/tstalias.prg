/*
 * $Id$
 */

PROCEDURE Main()

   USE test

   REPLACE Age WITH 1
   ? FIELD->Age

// REPLACE 1->Age WITH 2 // Todo: complete support in harbour.y - AliasAddInt()
// ? FIELD->Age

// REPLACE 1.5->Age WITH 3 // Will produce "Invalid alias expression"
// ? FIELD->Age

   REPLACE TEST->Age WITH 4
   ? FIELD->Age

   TEST->Age := 5
   ? FIELD->Age

   TEST->( FieldPut( FieldPos( "AGE" ), 6 ) )
   ? FIELD->Age

   dbCloseArea()

   SELE 2

   USE test

// ? ( "0" )->FIRST
   ? ( "B" )->FIRST
   ? ( "2" )->FIRST
   ? 2->FIRST
   ? B->FIRST

   Inkey( 0 )

// ? ( "0" )->FIRST
   ? Select()
   ? Select( 1 )
   ? Select( 2 )

   ? "0", Select( "0" )
   ? "1", Select( "1" )
   ? "2", Select( "2" )
   ? "A", Select( "A" )
   ? "B", Select( "B" )
   ? "C", Select( "C" )
   ? "D", Select( "D" )
   ? "E", Select( "E" )
   ? "F", Select( "F" )
   ? "G", Select( "G" )
   ? "H", Select( "H" )
   ? "I", Select( "I" )
   ? "J", Select( "J" )
   ? "K", Select( "K" )
   ? "L", Select( "L" )
   ? "M", Select( "M" )
   ? "N", Select( "N" )
   ? "O", Select( "O" )
   ? "P", Select( "P" )
   ? "Q", Select( "Q" )
   ? "R", Select( "R" )
   ? "S", Select( "S" )
   ? "T", Select( "T" )
   ? "U", Select( "U" )
   ? "V", Select( "V" )
   ? "W", Select( "W" )
   ? "X", Select( "X" )
   ? "Y", Select( "Y" )
   ? "Z", Select( "Z" )

   Inkey( 0 )

   ? ""  , dbSelectArea()     , Select()
   ? ""  , dbSelectArea( NIL ), Select()
   ? ""  , dbSelectArea( "" ) , Select()
   ? " " , dbSelectArea( " " ), Select()
   ? "0" , dbSelectArea( "0" ), Select()
   ? "1" , dbSelectArea( "1" ), Select()
   ? "2" , dbSelectArea( "2" ), Select()
   ? "A" , dbSelectArea( "A" ), Select()
   ? "B" , dbSelectArea( "B" ), Select()
   ? "C" , dbSelectArea( "C" ), Select()
   ? "D" , dbSelectArea( "D" ), Select()
   ? "E" , dbSelectArea( "E" ), Select()
   ? "F" , dbSelectArea( "F" ), Select()
   ? "G" , dbSelectArea( "G" ), Select()
   ? "H" , dbSelectArea( "H" ), Select()
   ? "I" , dbSelectArea( "I" ), Select()
   ? "J" , dbSelectArea( "J" ), Select()
   ? "K" , dbSelectArea( "K" ), Select()
   ? "L" , dbSelectArea( "L" ), Select()
   ? "M" , dbSelectArea( "M" ), Select()
   ? "Z" , dbSelectArea( "Z" ), Select()
   ? "AA", dbSelectArea( "AA" ), Select()

   RETURN
