Procedure Main( sMsg )

   LOCAL sText, sPPed

   sText := "/* Sample Script for embedded PP "   + ';'
   sText += " have fun... */"                     + ';'
   sText += ""                                    + ';'
   sText += "Procedure Test( cMacroVar, xValue )" + ';'
   sText += ""                                    + ';'
   sText += "   Local cVar := 'Hi There'"         + ';'
   sText += ""                                    + ';'
   sText += "   &cMacroVar := xValue"             + ';'
   sText += ""                                    + ';'
   sText += "   ? cVar, &cMacroVar"               + ';'
   sText += ""                                    + ';'
   sText += "   WHILE Inkey() == 0 // WAIT"       + ';'
   sText += "   ENDDO"                            + ';'
   sText += ""                                    + ';'
   sText += "RETURN LastKey()"

   Alert( PP_RunText( sText, .T., { "Private_1", 1000 } ) )

   //OR ...

   sPPed := PP_PreProText( sText )
   Alert( PP_RunText( sPPed, .F., { "Private_2", 2000 } ) )

   // Now let's have some real fun...
   IF sMsg == "Recursively running self"
      Alert( "Let's STOP this madness..." )
      RETURN
   ELSE
      PP_Run( "prgscrpt.prg", { "Recursively running self" } )
   ENDIF

return
