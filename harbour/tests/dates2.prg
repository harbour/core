/*
 * $Id$
 */

#include "set.ch"

PROCEDURE Main()

   LOCAL dDate := hb_SToD( "19990430" )

   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   SET( _SET_DATEFORMAT, "yyy/mm/ddd" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "on" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "off" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   SET( _SET_DATEFORMAT, "yyy/m/d/yyy" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "on" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "off" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   SET( _SET_DATEFORMAT, "m/d/y/m/d" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "on" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "off" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   SET( _SET_DATEFORMAT, "mmmm/ddddd" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "on" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "off" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   SET( _SET_DATEFORMAT, "mmmmm/dd" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "on" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )
   __SetCentury( "off" )
   OutStd( Set( _SET_DATEFORMAT ), dDate, hb_eol() )

   RETURN
