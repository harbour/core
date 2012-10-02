/*
 * $Id$
 */

#require "hbnf"

// ADD PARAMETER "CENTURY" ON COMMAND LINES TO TEST 4-DIGIT YEARS

PROCEDURE Main( cCent )

   LOCAL  lCentOn := .F. , cDate
   MEMVAR getlist

   IF HB_ISSTRING( cCent ) .AND. "CENT" $ Upper( cCent )
      SET CENTURY ON
      lCentOn := .T.
   ENDIF

   DO WHILE .T.
      CLEAR
      @ 2, 10 SAY "Date to Test"

      IF lCentOn
         cDate := Space( 10 )
         @ 2, 24 GET cDate PICTURE "##/##/####"
      ELSE
         cDate := Space( 8 )
         @ 2, 24 GET cDate PICTURE "##/##/##"
      ENDIF
      READ

      IF Empty( cDate )
         EXIT
      ENDIF

      IF Left( DToC( CToD( cDate ) ), 1 ) == " "
         Tone( 800, 1 )
         @ 4, 24 SAY "INVALID DATE"
         Inkey( 2 )
         LOOP
      ENDIF

      @ 4, 10 SAY "Is Day Number " + Str( FT_DOY( CToD( cDate ) ), 3 )

      @ 6, 10 SAY "Is in Week Number " + Str( FT_WOY( CToD( cDate ) ), 2 )
      @ 7, 0
      WAIT
   ENDDO

   CLEAR

   RETURN
