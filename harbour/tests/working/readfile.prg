//
// $Id$
//

// Test program for new class that reads a file one line at a time
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/
#ifdef __HARBOUR__
 #define NEW_LINE CHR( 10 )
#else
 #define NEW_LINE CHR( 13 ) + CHR( 10 )
#endif
#include "fileio.ch"

PROCEDURE Main( cFile )
LOCAL oFile := TFileRead():New( cFile )

   oFile:Open()
   IF oFile:Error()
      QOUT( oFile:ErrorMsg( "FileRead: " ) )
   ELSE
      WHILE oFile:MoreToRead()
         OUTSTD( oFile:ReadLine() )
         OUTSTD( NEW_LINE )
      END WHILE
      oFile:Close()
   END IF
QUIT
