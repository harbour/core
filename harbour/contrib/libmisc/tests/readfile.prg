//
// $Id$
//

// Test program for new class that reads a file one line at a time
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

#include "fileio.ch"

PROCEDURE Main( cFile )
LOCAL oFile := TFileRead():New( cFile )
LOCAL cNewLine := HB_OSNewLine()

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
