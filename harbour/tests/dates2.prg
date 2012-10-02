/*
 * $Id$
 */

#include "set.ch"

PROCEDURE Main()

   LOCAL dDate := hb_SToD( "19990430" )

   ? Set( _SET_DATEFORMAT ), dDate
   Set( _SET_DATEFORMAT, "yyy/mm/ddd" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "on" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "off" )
   ? Set( _SET_DATEFORMAT ), dDate
   Set( _SET_DATEFORMAT, "yyy/m/d/yyy" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "on" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "off" )
   ? Set( _SET_DATEFORMAT ), dDate
   Set( _SET_DATEFORMAT, "m/d/y/m/d" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "on" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "off" )
   ? Set( _SET_DATEFORMAT ), dDate
   Set( _SET_DATEFORMAT, "mmmm/ddddd" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "on" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "off" )
   ? Set( _SET_DATEFORMAT ), dDate
   Set( _SET_DATEFORMAT, "mmmmm/dd" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "on" )
   ? Set( _SET_DATEFORMAT ), dDate
   __SetCentury( "off" )
   ? Set( _SET_DATEFORMAT ), dDate

   RETURN
