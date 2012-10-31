/*
 * $Id$
 */

/* Test program for new class that reads a file one line at a time */

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

#require "hbmisc"

PROCEDURE Main( cFile )

   LOCAL oFile := TFileRead():New( cFile )

   oFile:Open()
   IF oFile:Error()
      ? oFile:ErrorMsg( "FileRead: " )
   ELSE
      DO WHILE oFile:MoreToRead()
         ? oFile:ReadLine()
      ENDDO
      oFile:Close()
   ENDIF

   RETURN
