
*+--------------------------------------------------------------------
*+
*+    Function Main()
*+
*+--------------------------------------------------------------------
*+
FUNCTION Main()

   LOCAL aOrders
   LOCAL nOp
   LOCAL dsFunctions

//   LOCAL cExePath := substr( cargv(), 1, rat( "\", cargv() ) - 1 )
   LOCAL cConStr  := ;
           "DBQ=" + "harbour.mdb;" + ;
           "Driver={Microsoft Access Driver (*.mdb)}"

   dsFunctions := TODBC():New( cConStr )

   set COLOR TO "W+/B"
   CLS

   WHILE .T.

      @ 00, 00 SAY padc( "þ TODBC Demonstration þ", 80 ) COLOR "B/W"

      dsFunctions:SetSQL( "SELECT * FROM Functions" )
      dsFunctions:Open()

      @ 03, 24 TO len( dsFunctions:Fields ) + 4, 55

      aOrders := {}
      FOR i := 1 TO len( dsFunctions:Fields )

         aadd( aOrders, dsFunctions:Fields[ i ] :FieldName )
         @ i + 3, 25 PROMPT padc( "ORDER BY " + aOrders[ i ], 30 )

      NEXT

      MENU TO nOp

      IF nOp == 0
         EXIT
      ENDIF

      dsFunctions:Close()

      dsFunctions:SetSQL( "SELECT * FROM Functions ORDER BY " + aOrders[ nOp ] )
      dsFunctions:Open()

      FOR i := 11 TO 24
         @ i, 00 SAY REPL( " ", 80 )
      NEXT

      @ 10, 00 TO 10, 79
      @ 24, 00 TO 24, 79
      @ 12, 00 TO 12, 79
      @ 11, 00 SAY ""

      @ 11, 02        SAY "Statement:"     COLOR "GR+/B"
      @ 11, col() + 1 SAY dsFunctions:cSQL

      @ 14, 05 SAY " " + padr( dsFunctions:FieldByName( "Code" ) :FieldName, 3 ) + "   " + ;
              padr( dsFunctions:FieldByName( "Function" ) :FieldName, 15 ) + "   " + ;
              padr( dsFunctions:FieldByName( "State" ) :FieldName, 2 ) + "   " + ;
              padr( dsFunctions:FieldByName( "Comments" ) :FieldName, 40 ) ;
              COLOR "B/W"

      WHILE !dsFunctions:Eof()
         ? "      " + padr( dsFunctions:FieldByName( "Code" ) :Value, 3 ), "³", ;
                            padr( dsFunctions:FieldByName( "Function" ) :Value, 15 ), "³", ;
                            padr( dsFunctions:FieldByName( "State" ) :Value, 2 ), "³", ;
                            padr( dsFunctions:FieldByName( "Comments" ) :Value, 40 )
         dsFunctions:Skip()
      ENDDO

      dsFunctions:Close()

   ENDDO
   dsFunctions:Destroy()

RETURN ( NIL )

*+ EOF: ODBCDEMO.PRG
