/*
 * $Id$
 */

#require "hbodbc"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL aOrders
   LOCAL nOp
   LOCAL dsFunctions

   LOCAL i

   dsFunctions := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   SET COLOR TO "W+/B"
   CLS

   DO WHILE .T.

      @  0,  0 SAY PadC( "- TODBC Demonstration -", 80 ) COLOR "B/W"

      dsFunctions:SetSQL( "SELECT * FROM test" )
      dsFunctions:Open()

      @  3, 24 TO Len( dsFunctions:Fields ) + 4, 55

      aOrders := {}
      FOR i := 1 TO Len( dsFunctions:Fields )

         AAdd( aOrders, dsFunctions:Fields[ i ] :FieldName )
         @ i + 3, 25 PROMPT PadC( "ORDER BY " + aOrders[ i ], 30 )

      NEXT

      MENU TO nOp

      IF nOp == 0
         EXIT
      ENDIF

      dsFunctions:Close()

      dsFunctions:SetSQL( "SELECT * FROM test ORDER BY " + aOrders[ nOp ] )
      dsFunctions:Open()

      FOR i := 11 TO 24
         @ i, 0 SAY Replicate( " ", 80 )
      NEXT

      @ 10, 0 TO 10, 79
      @ 24, 0 TO 24, 79
      @ 12, 0 TO 12, 79
      @ 11, 0 SAY ""

      @ 11, 2         SAY "Statement:"     COLOR "GR+/B"
      @ 11, Col() + 1 SAY dsFunctions:cSQL

      @ 14, 5 SAY " " + ;
         PadR( dsFunctions:FieldByName( "First" ):FieldName, 3 ) + "   " + ;
         PadR( dsFunctions:FieldByName( "Last" ):FieldName, 15 ) + "   " + ;
         PadR( dsFunctions:FieldByName( "Street" ):FieldName, 2 ) + "   " + ;
         PadR( dsFunctions:FieldByName( "City" ):FieldName, 40 ) ;
         COLOR "B/W"

      DO WHILE ! dsFunctions:Eof()
         ? "      " + ;
            PadR( dsFunctions:FieldByName( "First" ):Value, 3 ), "|", ;
            PadR( dsFunctions:FieldByName( "Last" ):Value, 15 ), "|", ;
            PadR( dsFunctions:FieldByName( "Street" ):Value, 2 ), "|", ;
            PadR( dsFunctions:FieldByName( "City" ):Value, 40 )
         dsFunctions:Skip()
      ENDDO

      dsFunctions:Close()

   ENDDO
   dsFunctions:Destroy()

   RETURN
