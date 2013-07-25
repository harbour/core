#pragma -b+

#include "hbclass.ch"

PROCEDURE Main

   LOCAL o := Some():New()

   AltD()

   ? o:Test()

   RETURN

CREATE CLASS Some

   ACCESS Test
   ASSIGN Test() INLINE ::Test()

END CLASS

METHOD Test()

   LOCAL a := 1

   a++

   RETURN a
