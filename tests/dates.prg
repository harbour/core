// Testing Harbour dates management

PROCEDURE Main()

   LOCAL dDate, dDate2, cMask, cDate

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? "Testing Harbour dates management on", Date()

   TestCentury()

   ?
   ? "dDate := CToD( '1999-02-04' ) =>", dDate := CToD( "1999-02-04" )

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
   Set( _SET_DATEFORMAT, cMask := "dd/mm/yyyy" )
   dDate := CToD( cDate := "02/04/49" )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )

   ?
   Set( _SET_DATEFORMAT, cMask := "mm/dd/yyyy" )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )

   ?
   Set( _SET_DATEFORMAT, cMask := "yyyy/mm/dd" )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )
   ?
   ? "49/02/04", cMask, CToD( "49/02/04" )

   TestCentury()

   ?
   Set( _SET_DATEFORMAT, cMask := "yyyy/dd/mm" )
   dDate := CToD( cDate )
   ? cDate, cMask, dDate, DToS( dDate ), DToC( dDate )
   ?
   ? "49/02/04", cMask, CToD( "49/02/04" )

   ?
   Set( _SET_DATEFORMAT, cMask := "ddd/mmm/yy" )
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
