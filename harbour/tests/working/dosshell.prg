//NOTEST             // It is very frustrating if this one is auto-tested
//
// DosShell
//
// This program shell to DOS
//
// Warning : DOS only
//
#command ? [<xArg,...>] => QOut( <xArg> )
#command ! <cCommand>   => __Run( <cCommand> )

function Main()

   local cOs := Upper( OS() )

   if (at( "WINDOWS", cOs ) != 0) .or. at( "DOS", cOs ) != 0
      ? "About to shell to DOS.."
      ! GetEnv("COMSPEC")
      ? "Hey, I am back !"
   else
      ? "Sorry this program is for Windows and DOS only"
   endif
return nil
