/* Comparing hash retrieval method speeds */

#define _ITER  1000000

PROCEDURE Main()

   LOCAL h := { "a" => 1, "b" => 2, "c" => 3, "d" => 4, "e" => 5, "f" => 6 }
   LOCAL t, c, p, x

   t := hb_milliSeconds()
   FOR c := 1 TO _ITER
       IF ( p := hb_HPos( h, "d" ) ) > 0
          x := hb_HValueAt( h, p )
       ENDIF
   NEXT
   ? hb_milliSeconds() - t

   t := hb_milliSeconds()
   FOR c := 1 TO _ITER
       IF "d" $ h
          x := h[ "d" ]
       ENDIF
   NEXT
   ? hb_milliSeconds() - t

   t := hb_milliSeconds()
   FOR c := 1 TO _ITER
       hb_HGetRef( h, "d", @x )
   NEXT
   ? hb_milliSeconds() - t

   RETURN
