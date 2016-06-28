/* Copyright 2013-2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbmisc"

PROCEDURE Main()

   LOCAL tTime

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   IF Empty( tTime := hb_ntp_GetTimeUTC( "0.europe.pool.ntp.org" ) )
      ? "Network not available"
   ENDIF
   ? "UTC    time:", tTime
   ? "Local  time:", tTime + hb_UTCOffset() / 86400
   ? "System time:", hb_DateTime()

   RETURN
