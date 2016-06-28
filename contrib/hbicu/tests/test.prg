/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbicu"

PROCEDURE Main()

   ? u_errorName( 0 /* U_ZERO_ERROR */ )
   ? hb_u_getVersion()

   RETURN
