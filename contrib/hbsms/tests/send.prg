/* Copyright 2009-2010 Viktor Szakats (vszakats.net/harbour) */

#require "hbsms"

#if   defined( __PLATFORM__WINDOWS )
   #define _DEF_PORT_  "\\.\COM22"
#elif defined( __PLATFORM__LINUX )
   #define _DEF_PORT_  "/dev/ttyS1"
#elif defined( __PLATFORM__DARWIN )
   #define _DEF_PORT_  "/dev/cu.myport-COM1-1"
#else
   #define _DEF_PORT_  ""
#endif

PROCEDURE Main( cPort )

   ? "start"
   ? sms_Send( hb_defaultValue( cPort, _DEF_PORT_ ), "555555555", "test msg", .T. )
   ? "end"

   RETURN
