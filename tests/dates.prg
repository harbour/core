// Testing Harbour dates management.

PROCEDURE Main()

   LOCAL dDate, dDate2, cMask, cDate

   ? "Testing Harbour dates management on", Date()

   TestCentury()

   ?
   ? "dDate := CToD( '02/04/1999' ) =>", dDate := CToD( "02/04/1999" )

   ? "ValType( dDate ) =", ValType( dDate )

   ? "Day( dDate ) =", Day( dDate )
   ? "Month( dDate ) =", Month( dDate )
   ? "Year( dDate ) =", Year( dDate )
   ?

   ? "dDate + 5 =", dDate2 := dDate + 5
   ? "dDate - 5 =", dDate - 5
   ?

   ? "dDate2 - dDate =", dDate2 - dDate

   ?
   ? dDate, DToS( dDate )

   ? "19990429", hb_SToD( "19990429" )

   ?
   Set( _SET_EPOCH, 1950 )
   cMask := "dd/mm/yyyy"
   cDate := "02/04/49"
   Set( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )

   ?
   cMask := "mm/dd/yyyy"
   Set( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )

   ?
   cMask := "yyyy/mm/dd"
   Set( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )
   ?
   ? "49/02/04", cMask, CToD( "49/02/04" )

   TestCentury()

   ?
   cMask := "yyyy/dd/mm"
   Set( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )
   ?
   ? "49/02/04", cMask, CToD( "49/02/04" )

   ?
   cMask := "ddd/mmm/yy"
   Set( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )

   RETURN

STATIC PROCEDURE TestCentury()

   ?
   ? __SetCentury()
   __SetCentury( "ON" )
   ?? __SetCentury()
   __SetCentury( "OFF" )
   ?? __SetCentury()
   __SetCentury( "GIBBERISH" )
   ?? __SetCentury()
   __SetCentury( .T. )
   ?? __SetCentury()
   __SetCentury( 5 )
   ?? __SetCentury()

   RETURN
