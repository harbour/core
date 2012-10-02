/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL   GetList := {}, cVar := "Hello"
   MEMVAR  aVar, nIndex, cMacro, cEarly, cEarly2, cLate
   PRIVATE aVar := { "World", "Again" }, nIndex := 1, cMacro := "cEarly", cEarly := { "Early" }, cLate := "Late!", cEarly2 := { "Early2" }

   CLS

   ? "2nd GET should say 'Early'."

   @ 10, 10 SAY "cVar            :" GET cVar PICTURE "@K!"
   @ 12, 10 SAY "cMacro[1]       :" GET &cMacro[ 1 ]
   @ 14, 10 SAY "cMacro.2[1]     :" GET &cMacro.2[ 1 ]
   @ 16, 10 SAY "cEarly[1]       :" GET cEarly[ 1 ]
// @ 14,10 SAY "cMacro         :" GET &( cMacro )[ 1 ]
   nIndex := 2
   @ 18, 10 SAY "aVar            :" GET aVar[ nIndex ]
   @ 20, 10 SAY "Picture of GET-1:" GET GetList[ 1 ]:Picture
   nIndex := 3
   cMacro := "cLate"
   READ

   CLS

/* Clipper Error "Get contains complex macro"
   ? "This GET should say 'Late!'."
   cMacro := "cEarly"
   @ 10, 10 SAY "cMacro          :" GET &( cMacro )
   cMacro := "cLate"
   READ
*/

   RETURN
