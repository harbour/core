//
// $Id$
//

// Test program for new class that reads a file one line at a time
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "fileio.ch"

PROCEDURE Main( cFile )
LOCAL oFile := TFileRead():New( cFile )
LOCAL cOs := UPPER( OS() ), cNewLine

   IF "OS/2" $ cOs .OR. "WIN" $ cOS .OR. "DOS" $cOS
      cNewLine := CHR( 13 ) + CHR( 10 )
   ELSE
      cNewLine := CHR( 10 )
   END IF

   oFile:Open()
   IF oFile:Error()
      QOUT( oFile:ErrorMsg( "FileRead: " ) )
   ELSE
      WHILE oFile:MoreToRead()
         OUTSTD( oFile:ReadLine() )
         OUTSTD( cNewLine )
      END WHILE
      oFile:Close()
   END IF
QUIT
