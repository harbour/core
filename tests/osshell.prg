//NOTEST             // It is very frustrating if this one is auto-tested

/* Written by Eddie Runia <eddie@runia.com>. Placed in the public domain. */

/* This program shells to OS */

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
