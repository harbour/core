// Test for regular expression functions
// This allows to use a fine tune regex to use them in programs
// Giancarlo Niccolai

#include "inkey.ch"

PROCEDURE Main()

   LOCAL pCompiled
   LOCAL nRow
   LOCAL aMatch, cMatch

   LOCAL GetList := {}

   LOCAL cRegex := Space( 60 )
   LOCAL cSentence := Space( 120 )

   CLS

   @ 2, 15 SAY "Regular expression test"

   @ 4, 5 SAY "Insert regular expression(s) and strings to test for."
   @ 5, 5 SAY "Press <Esc> to exit"

   DO WHILE LastKey() != K_ESC

      @ 8, 5 SAY " REGEX:" GET cRegex PICTURE "@S30"
      @ 9, 5 SAY "PHRASE:" GET cSentence PICTURE "@S60"
      READ
      IF LastKey() != K_ESC

         @ 12, 5 CLEAR TO MaxRow(), MaxCol()

         IF Empty( pCompiled := hb_regexComp( RTrim( cRegex ) ) )
            @ 12, 5 SAY "Invalid REGEX expression"
            LOOP
         ENDIF

         IF ( aMatch := hb_regex( pCompiled, RTrim( cSentence ) ) ) != NIL
            @ 12, 5 SAY "MATCHES:"
            nRow := 13
            FOR EACH cMatch IN aMatch
               @ nRow++, 5 SAY ">" + cMatch
            NEXT
         ELSE
            @ 12, 5 SAY "No matches"
         ENDIF
      ENDIF
   ENDDO

   CLS

   RETURN
