/*
 * $Id$
 */

#require "hbziparc"

#include "simpleio.ch"

PROCEDURE Main( cZip, ... )

   LOCAL a, b, c

   SET DATE TO ANSI
   SET CENTURY ON

   ? hb_ZipFile( cZip, hb_AParams() )

   a := hb_GetFilesInZip( cZip, .T. )

   FOR EACH b IN a
      ?
      FOR EACH c IN b
         ?? c, ""
      NEXT
   NEXT

   RETURN
