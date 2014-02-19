#require "hbnf"

#include "inkey.ch"

PROCEDURE Main()

   LOCAL nSickHrs := 0
   LOCAL nPersHrs := 0
   LOCAL nVacaHrs := 0
   LOCAL GetList  := {}

   SET SCOREBOARD OFF
   CLS

   SetKey( K_ALT_A, {|| ft_Adder() } )      // Make <ALT-A> call FT_Adder

   // Simple sample of program data entry!

   @ 12,  5 SAY "Please enter the total Sick, Personal, and Vacation hours."
   @ 15, 22 SAY "Sick hrs."
   @ 15, 40 SAY "Pers. hrs."
   @ 15, 60 SAY "Vaca. hrs."
   @ MaxRow() - 2, 0 SAY PadC( "Press <ALT-A> to Pop - Up the Adder.", MaxCol() + 1 )
   @ MaxRow() - 1, 0 SAY PadC( "Press <ESC> to Quit the adder Demo.", MaxCol() + 1 )
   DO WHILE .T.                                 // Get the sick, personal, and vaca
      @ 16, 24 GET nSickHrs PICTURE "9999.999"  // Normally I have a VALID()
      @ 16, 43 GET nPersHrs PICTURE "9999.999"  // to make sure the value is
      @ 16, 63 GET nVacaHrs PICTURE "9999.999"  // within the allowable range.
      CLEAR TYPEAHEAD                           // SIMPLE example <g>.
      READ
      IF LastKey() == K_ESC                     // <ESC> - ABORT
         CLEAR TYPEAHEAD
         EXIT
      ENDIF
   ENDDO

   SET KEY K_ALT_A                     // Reset <ALT-A>

   RETURN
