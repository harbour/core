#require "hbnf"

PROCEDURE Main()

   LOCAL dDate
   LOCAL GetList := {}

   CLS

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   DO WHILE .T.

      @ 2, 10 SAY "Date to Test"

      dDate := hb_SToD()
      @ 2, 24 GET dDate
      READ

      IF Empty( dDate )
         EXIT
      ENDIF

      @ 4, 10 SAY "Is Day Number " + hb_ntos( ft_DoY( dDate ) )
      @ 5, 10 SAY "Is in Week Number " + hb_ntos( ft_WoY( dDate ) )
      @ 6, 0

      WAIT

   ENDDO

   RETURN
