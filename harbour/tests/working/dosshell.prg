//NOTEST             // It is very frustrating if this one is auto-tested
//
// DosShell
//
// This program shell to DOS
//
// Warning : DOS only
//
// Copyright (C) 1999  Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//

function Main()

   local cOs    := Upper( OS() )
   local cShell := GetEnv("COMSPEC")

   if (at( "WINDOWS", cOs ) != 0) .or. at( "DOS", cOs ) != 0
      ? "About to shell to DOS.."
      ! (cShell)
      ? "Hey, I am back !"
   else
      ? "Sorry this program is for Windows and DOS only"
   endif
return nil
