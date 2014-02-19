/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

#require "hbfoxpro"

PROCEDURE Main()

   #if defined( __PLATFORM__WINDOWS )

   ? __fox_DynCall( "DECLARE INTEGER MessageBeep IN user32.dll" )
   ? __fox_DynCall( "DECLARE LONG GetCurrentProcessId IN kernel32.dll" )

   #endif

   WAIT

   RETURN
