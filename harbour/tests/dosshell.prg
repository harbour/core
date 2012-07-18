//NOTEST             // It is very frustrating if this one is auto-tested
/*
 * $Id$
 */

//
// DosShell
//
// This program shell to DOS
//
// Warning : DOS only
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL cOs    := Upper( OS() )
   LOCAL cShell := GetEnv( "COMSPEC" )

   IF At( "WINDOWS", cOs ) != 0 .OR. At( "DOS", cOs ) != 0 ;
         .OR. At( "OS/2", cOs ) != 0
      ? "About to shell to DOS.."
      ! ( cShell )
      ? "Hey, I am back !"
   ELSE
      ? "Sorry this program is for Windows, DOS, and OS/2 only"
   ENDIF

   RETURN
