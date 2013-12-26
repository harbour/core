/* Copyright 2013 Viktor Szakats (vszakats.net/harbour) */

#require "hbmisc"

PROCEDURE Main()

   LOCAL tTime

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ?? "UTC    time:", tTime := hb_ntp_GetTimeUTC( "0.europe.pool.ntp.org" )
   ?
   ?? "Local  time:", tTime + hb_UTCOffset() / 86400
   ?
   ?? "System time:", hb_DateTime()
   ?

   RETURN
