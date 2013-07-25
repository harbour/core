#require "hbodbc"

PROCEDURE Main()

   LOCAL dsFunctions := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   LOCAL aOrders
   LOCAL nOp
   LOCAL i

   SET COLOR TO "W+/B"
   CLS

   DO WHILE .T.

      @  0,  0 SAY PadC( "- TODBC Demonstration -", MaxCol() + 1 ) COLOR "B/W"

      dsFunctions:SetSQL( "SELECT * FROM test" )
      dsFunctions:Open()

      @  3, 24 TO Len( dsFunctions:Fields ) + 4, 55

      aOrders := {}
      FOR i := 1 TO Len( dsFunctions:Fields )
         AAdd( aOrders, dsFunctions:Fields[ i ]:FieldName )
         @ i + 3, 25 PROMPT PadC( "ORDER BY " + aOrders[ i ], 30 )
      NEXT

      MENU TO nOp

      IF nOp == 0
         EXIT
      ENDIF

      dsFunctions:Close()

      dsFunctions:SetSQL( "SELECT * FROM test ORDER BY " + aOrders[ nOp ] )
      dsFunctions:Open()

      @ 14, 5 SAY " " + ;
         PadR( dsFunctions:FieldByName( "First" ):FieldName, 10 ) + "   " + ;
         PadR( dsFunctions:FieldByName( "Last" ):FieldName, 10 ) + "   " + ;
         PadR( dsFunctions:FieldByName( "Street" ):FieldName, 25 ) + "   " + ;
         PadR( dsFunctions:FieldByName( "City" ):FieldName, 40 ) ;
         COLOR "B/W"

      dsFunctions:Skip() /* TOFIX: To avoid first record to return NILs. bug in TODBC? */

      DO WHILE ! dsFunctions:Eof()
         ? "      " + ;
            PadR( dsFunctions:FieldByName( "First" ):Value, 10 ), "|", ;
            PadR( dsFunctions:FieldByName( "Last" ):Value, 10 ), "|", ;
            PadR( dsFunctions:FieldByName( "Street" ):Value, 25 ), "|", ;
            PadR( dsFunctions:FieldByName( "City" ):Value, 40 )
         dsFunctions:Skip()
      ENDDO

      @ MaxRow(), 0
      @ MaxRow(), 1         SAY "Statement:" COLOR "GR+/B"
      @ MaxRow(), Col() + 1 SAY dsFunctions:cSQL

      dsFunctions:Close()

   ENDDO
   dsFunctions:Destroy()

   RETURN
