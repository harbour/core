/*
 * $Id$
 */

/* Test program for new class that reads a file one line at a time */

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

#include "fileio.ch"

PROCEDURE Main( cFile )
   LOCAL oFile := TFileRead():New( cFile )
   LOCAL cNewLine := hb_eol()

   oFile:Open()
   IF oFile:Error()
      QOUT( oFile:ErrorMsg( "FileRead: " ) )
   ELSE
      DO WHILE oFile:MoreToRead()
         OUTSTD( oFile:ReadLine() )
         OUTSTD( cNewLine )
      ENDDO
      oFile:Close()
   ENDIF

   RETURN
