#pragma -b+

#include "hbclass.ch"

PROCEDURE Main

   LOCAL o := Some():New()
   LOCAL h := { "a" => 10, "b" => 20 }

   AltD()

   ? o:Test()
   ? h[ "a" ]

   RETURN

CREATE CLASS Some STATIC

   ACCESS Test
   ASSIGN Test() INLINE ::Test()

END CLASS

METHOD Test()

   LOCAL a := 1

   a++

   RETURN a
