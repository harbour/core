/*
 * Copyright 2009-2010 Viktor Szakats (harbour syenar.net)
 * www - http://www.harbour-project.org
 */

#require "hbsms"

PROCEDURE Main( cPort )

#if   defined( __PLATFORM__WINDOWS )
   hb_default( @cPort, "\\.\COM22" )
#elif defined( __PLATFORM__LINUX )
   hb_default( @cPort, "/dev/ttyS1" )
#elif defined( __PLATFORM__DARWIN )
   hb_default( @cPort, "/dev/cu.myport-COM1-1" )
#endif

   ? "start"
   ? sms_Send( cPort, "555555555", "test msg", .T. )
   ? "end"

   RETURN
