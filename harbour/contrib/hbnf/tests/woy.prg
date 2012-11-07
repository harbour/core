/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL dDate
   LOCAL GetList := {}

   SET DATE ANSI
   SET CENTURY ON

   DO WHILE .T.

      @ 2, 10 SAY "Date to Test"

      dDate := SToD( "" )
      @ 2, 24 GET dDate
      READ

      IF Empty( dDate )
         EXIT
      ENDIF

      @ 4, 10 SAY "Is Day Number " + Str( ft_DoY( dDate ), 10 )
      @ 6, 10 SAY "Is in Week Number " + Str( ft_WoY( dDate ), 10 )
      @ 7, 0

      WAIT

   ENDDO

   RETURN
