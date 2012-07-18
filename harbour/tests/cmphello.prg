/*
 * $Id$
 */

//
// Compile Hello
//
// This program compiles hello.prg
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL cOs := Upper( OS() )

   QOut( "About to compile Hello.prg" )
   QOut()
   IF At( "WINDOWS", cOs ) != 0 .OR. At( "DOS", cOs ) != 0 .OR. ;
         At( "OS/2", cOs ) != 0                    // OS/2, DOS, Windows version
      __Run( "..\bin\harbour.exe hello.prg /gHRB" )
   ELSE                                            // Unix / Linux version
      __Run( "../bin/harbour hello.prg /gHRB" )
   ENDIF
   QOut( "Finished compiling" )

   RETURN
