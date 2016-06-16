#require "hbnf"

#include "inkey.ch"

PROCEDURE Main()

   LOCAL dDate := Date()
   LOCAL GetList := {}

   CLS

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   DO WHILE .T.

      @ 2, 10 SAY "Date to test:" GET dDate
      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 4, 10 SAY "Is day number " + hb_ntos( ft_DoY( dDate ) )
      @ 5, 10 SAY "Is in week number " + hb_ntos( ft_WoY( dDate ) )
      @ 6, 0

      WAIT
   ENDDO

   RETURN
