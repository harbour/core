#require "hbodbc"

PROCEDURE Main()

   LOCAL dsFunctions := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   LOCAL aOrders
   LOCAL nOp
   LOCAL fld

   SetColor( "W+/B" )
   CLS

   DO WHILE .T.

      @  0,  0 SAY PadC( "- TODBC Demonstration -", MaxCol() + 1 ) COLOR "B/W"

      dsFunctions:SetSQL( "SELECT * FROM test" )
      dsFunctions:Open()

      @  3, 24 TO Len( dsFunctions:Fields ) + 4, 55

      aOrders := {}
      FOR EACH fld IN dsFunctions:Fields
         AAdd( aOrders, fld:FieldName )
         @  3 + Len( aOrders ), 25 PROMPT PadR( "ORDER BY " + ATail( aOrders ), 30 )
      NEXT

      MENU TO nOp

      IF nOp == 0
         EXIT
      ENDIF

      dsFunctions:Close()

      dsFunctions:SetSQL( "SELECT * FROM test ORDER BY " + aOrders[ nOp ] )
      dsFunctions:Open()

      @ MaxRow(), 0
      @ MaxRow(), 1 SAY "Statement:" COLOR "GR+/B"
      @ MaxRow(), Col() + 1 SAY dsFunctions:cSQL

      BrowseODBC( 1, 0, MaxRow() - 1, MaxCol(), dsFunctions )

      dsFunctions:Close()
   ENDDO

   dsFunctions:Destroy()

   RETURN
