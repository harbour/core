//NOTEST             // It is very frustrating if this one is auto-tested
//
// DosShell
//
// This program shell to DOS
//
// Warning : DOS only
//
function Main()

   local cOs := Upper( OS() )

   if at( "WINDOWS", cOs ) != 0 .or. at( "DOS", cOs ) != 0
      QOut( "About to shell to DOS.." )
      __Run( GetEnv("COMSPEC") )
      QOut( "Hey, I am back !" )
   else
      QOut( "Sorry this program is for Windows and DOS only" )
   endif
return nil
