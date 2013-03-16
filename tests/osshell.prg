//NOTEST             // It is very frustrating if this one is auto-tested

//
// This program shells to OS
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL cShell

#if defined( __PLATFORM__UNIX )
   cShell := GetEnv( "SHELL" )
#else
   cShell := GetEnv( "COMSPEC" )
#endif

   ? "About to shell to OS.."
   ! ( cShell )
   ? "Hey, I am back !"

   RETURN
