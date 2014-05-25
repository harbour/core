//NOTEST             // It is very frustrating if this one is auto-tested

// This program shells to OS
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain

#if defined( __PLATFORM__UNIX )
   #define _SHELLENV_  "SHELL"
#else
   #define _SHELLENV_  "COMSPEC"
#endif

PROCEDURE Main()

   ? "About to shell to OS..."
   ?
   ! ( GetEnv( _SHELLENV_ ) )
   ? "Hey, I am back !"

   RETURN
