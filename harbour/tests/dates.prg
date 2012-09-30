/*
 * $Id$
 */

// Testing Harbour dates management.

#include "set.ch"

PROCEDURE Main()

   LOCAL dDate, dDate2, cMask, cDate

   OutStd( hb_eol(),  "Testing Harbour dates management on", Date() )

   TestCentury()

   OutStd( hb_eol(), "" )
   OutStd( hb_eol(), "dDate := CToD( '02/04/1999' ) =>", dDate := CToD( "02/04/1999" ) )

   OutStd( hb_eol(), "ValType( dDate ) =", ValType( dDate ) )

   OutStd( hb_eol(), "Day( dDate ) =", Day( dDate ) )
   OutStd( hb_eol(), "Month( dDate ) =", Month( dDate ) )
   OutStd( hb_eol(), "Year( dDate ) =", Year( dDate ), hb_eol() )

   OutStd( hb_eol(), "dDate + 5 =", dDate2 := dDate + 5 )
   OutStd( hb_eol(), "dDate - 5 =", dDate - 5, hb_eol() )

   OutStd( hb_eol(), "dDate2 - dDate =", dDate2 - dDate )

   OutStd( hb_eol(), "" )
   OutStd( hb_eol(), dDate, DToS( dDate ) )

   OutStd( hb_eol(), "19990429", SToD( "19990429" ) )

   OutStd( hb_eol(), "" )
   SET( _SET_EPOCH, 1950 )
   cMask := "dd/mm/yyyy"
   cDate := "02/04/49"
   SET( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   OutStd( hb_eol(), cDate, cMask, dDate, DToS( dDate ), DToC( dDate ) )

   OutStd( hb_eol(), "" )
   cMask := "mm/dd/yyyy"
   SET( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   OutStd( hb_eol(), cDate, cMask, dDate, DToS( dDate ), DToC( dDate ) )

   OutStd( hb_eol(), "" )
   cMask := "yyyy/mm/dd"
   SET( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   OutStd( hb_eol(), cDate, cMask, dDate, DToS( dDate ), DToC( dDate ) )
   OutStd( hb_eol(), "" )
   OutStd( hb_eol(), "49/02/04", cMask, CToD( "49/02/04" ) )

   TestCentury( hb_eol() )

   OutStd( hb_eol(), "" )
   cMask := "yyyy/dd/mm"
   SET( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   OutStd( hb_eol(), cDate, cMask, dDate, DToS( dDate ), DToC( dDate ) )
   OutStd( hb_eol(), "" )
   OutStd( hb_eol(), "49/02/04", cMask, CToD( "49/02/04" ) )

   OutStd( hb_eol(), "" )
   cMask := "ddd/mmm/yy"
   SET( _SET_DATEFORMAT, cMask )
   dDate := CToD( cDate )
   OutStd( hb_eol(), cDate, cMask, dDate, DToS( dDate ), DToC( dDate ) )

   RETURN

PROCEDURE TestCentury()

   OutStd( hb_eol(), "" )
   OutStd( hb_eol(), __SetCentury() )
   __SetCentury( "ON" )
   OutStd( __SetCentury() )
   __SetCentury( "OFF" )
   OutStd( __SetCentury() )
   __SetCentury( "GIBBERISH" )
   OutStd( __SetCentury() )
   __SetCentury( .T. )
   OutStd( __SetCentury() )
   __SetCentury( 5 )
   OutStd( __SetCentury() )

   RETURN
