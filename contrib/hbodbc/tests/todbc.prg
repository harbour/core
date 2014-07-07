#require "hbodbc"

PROCEDURE Main()

   LOCAL oDS := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   LOCAL aOrders
   LOCAL nChoice
   LOCAL fld

   SetColor( "W+/B" )
   CLS

   @ 0, 0 SAY PadC( "TODBC Demonstration", MaxCol() + 1 ) COLOR "B/W"

   oDS:SetSQL( "SELECT * FROM test" )
   oDS:Open()

   aOrders := {}
   FOR EACH fld IN oDS:Fields
      AAdd( aOrders, fld:FieldName )
   NEXT

   oDS:Close()

   DO WHILE .T.

      @ 3, 24 TO Len( aOrders ) + 4, 55
      @ 3, 35 SAY " " + "ORDER BY" + " "
      @ MaxRow(), 0

      IF ( nChoice := AChoice( 4, 25, Len( aOrders ) + 4, 54, aOrders,,, nChoice ) ) == 0
         EXIT
      ENDIF

      oDS:SetSQL( "SELECT * FROM test ORDER BY " + aOrders[ nChoice ] )
      oDS:Open()

      @ MaxRow(), 1 SAY "Statement:" COLOR "GR+/B"
      @ MaxRow(), Col() + 1 SAY oDS:cSQL

      hb_odbcBrowse( 1, 0, MaxRow() - 1, MaxCol(), oDS )

      oDS:Close()
   ENDDO

   oDS:Destroy()

   RETURN
