#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   COPY FILE test.dbf TO test1.dbf

   USE test1.dbf ALIAS test

   REPLACE Age WITH 1
   ? FIELD->Age

   REPLACE 1->Age WITH 2
   ? FIELD->Age

#ifdef _COMMENT_
   REPLACE 1.5->Age WITH 3  // RTE: "Invalid alias expression"
   ? FIELD->Age
#endif

   REPLACE TEST->Age WITH 4
   ? FIELD->Age

   TEST->Age := 5
   ? FIELD->Age

   TEST->( FieldPut( FieldPos( "AGE" ), 6 ) )
   ? FIELD->Age

   dbCloseArea()

   dbSelectArea( 2 )

   USE test1.dbf ALIAS test

#ifdef _COMMENT_
   ? ( "0" )->FIRST  // RTE
#endif
   ? ( "B" )->FIRST
   ? ( "2" )->FIRST
   ? 2->FIRST
   ? B->FIRST

   WAIT

#ifdef _COMMENT_
   ? ( "0" )->FIRST  // RTE
#endif
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

   WAIT

   ? ""  , dbSelectArea()     , Select()
   ? ""  , dbSelectArea( NIL ), Select()
#ifdef _COMMENT_
   ? ""  , dbSelectArea( "" ) , Select()  // RTE
   ? " " , dbSelectArea( " " ), Select()  // RTE
#endif
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
#ifdef _COMMENT_
   ? "L" , dbSelectArea( "L" ), Select()  // RTE
#endif
   ? "M" , dbSelectArea( "M" ), Select()
#ifdef _COMMENT_
   ? "Z" , dbSelectArea( "Z" ), Select()  // RTE
   ? "AA", dbSelectArea( "AA" ), Select()  // RTE
#endif

   dbCloseAll()

   hb_dbDrop( "test1.dbf" )

   RETURN
