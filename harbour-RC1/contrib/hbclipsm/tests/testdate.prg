/*
 * $Id$
 */

function Test()

   local dDate
   local aDate

   SET DATE FORMAT "dd/mm/yyyy"
   dDate := CTOD( "19/07/71" )

   SET CENTURY OFF
   ? "[" + MDY( dDate ) + "]"
   ? "[" + DMY( dDate ) + "]"
   ? DateAsAge( dDate )
   aDate := DateAsArray( dDate )
   ? aDate[1], aDate[2], aDate[3]
   ? ArrayAsDate( aDate )
   ? DateIsLeap( dDate )
   ? NtoD( aDate[2], aDate[3], aDate[1] )
   ? AddMonth( dDate, 12 )
   ? AddMonth( dDate, 18 )

   SET CENTURY ON
   ? "[" + MDY( dDate ) + "]"
   ? "[" + DMY( dDate ) + "]"
   ? DateAsAge( dDate )
   aDate := DateAsArray( dDate )
   ? aDate[1], aDate[2], aDate[3]
   ? ArrayAsDate( aDate )
   ? DateIsLeap( dDate )
   ? NtoD( aDate[2], aDate[3], aDate[1] )
   ? AddMonth( dDate, 12 )
   ? AddMonth( dDate, 18 )

return nil
