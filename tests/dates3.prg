// Testing Harbour date management

PROCEDURE Main()

   LOCAL dDate, i

   Set( _SET_DATEFORMAT, "dd/mm/yyyy" )
   dDate := hb_SToD( "19990525" )

   ? dDate, DoW( dDate )

   ? LastMonday( dDate )

   dDate += 3
   ? dDate, DoW( dDate )

   dDate += 4
   ? dDate, DoW( dDate )

   Set( _SET_DATEFORMAT, "mm/dd/yyyy" )
   dDate := hb_SToD( "19990525" )

   ? dDate, DoW( dDate )

   ? LastMonday( dDate )

   dDate += 3
   ? dDate, DoW( dDate )

   dDate += 4
   ? dDate, DoW( dDate )

   ?
   dDate := Date()
   FOR i := 1 TO 7
      ? dDate, DoW( dDate )
      dDate++
   NEXT
   ? hb_SToD(), DoW( hb_SToD() )

   RETURN

// Like NG's sample

STATIC FUNCTION LastMonday( dDate )
   RETURN dDate - DoW( dDate ) + 2
