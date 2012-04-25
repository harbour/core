/*
 * $Id$
 */

/******************************************************************
 * Test for Ini file reading/writing
 *
 * Giancarlo Niccolai
 */

PROCEDURE Main( cName )

   LOCAL hIni, aSect, cIni
   LOCAL cSection
   LOCAL cKey
   LOCAL nRow := 1

   SET COLOR TO w +/ b
   CLEAR SCREEN
   @ nRow++, 20 SAY "H A R B O U R - .ini file parser test"
   @ nRow++, 5 SAY "Call from command line using a .ini filename as the only parameter"
   nRow++

   IF Empty( cName )
      cName := "parseini.ini"
      @nRow++ , 5 SAY "Using default parseini.ini file"
   ENDIF

   hIni := hb_iniRead( cName )

   @ nRow, 0

   ? "Content of " + cName

   IF Empty( hIni )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH cSection IN hIni:Keys
         ?
         ? "Section [" + cSection + "]"
         aSect := hIni[ cSection ]

         FOR EACH cKey IN aSect:Keys
            ? cKey + " = " + aSect[ cKey ]
         NEXT
      NEXT
   ENDIF

   ?
   ? "Adding section 'Added', with key NEW = new"
   hIni[ "Added" ] := { => }
   hIni[ "Added" ][ "NEW" ] := "new"

   ? "Writing output to parseini_out.ini"
   IF hb_iniWrite( "parseini_out.ini", hIni, "#Generated file; don't touch", "#End of file" )
      ? "File written"
   ELSE
      ? "Can't write file"
   ENDIF
   ?
   ? "Press any key to next text."
   Inkey( 0 )

   nRow := 3
   @ nRow, 0 CLEAR
   ?
   ? "REPEATING TESTS WITHOUT AUTOMATIC MAIN SECTION"
   ?

   hIni := hb_iniRead( cName,; /* default case */
                       ,; /* default key indicators */
                       , .F. )

   ? "Content of " + cName

   IF Empty( hIni )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH cSection IN hIni:Keys
         /* Now (without automatic main), toplevel options may be in the root hash */
         aSect := hIni[ cSection ]

         IF hb_isHash( aSect )
            /* It's a section */
            ?
            ? "Section [" + cSection + "]"

            FOR EACH cKey IN aSect:Keys
               ? cKey + " = " + aSect[ cKey ]
            NEXT
         ELSE
            /* It's a toplevel option */
            ? "TOPLEVEL option:", cSection + " = " + aSect
         ENDIF
      NEXT
   ENDIF

   ?
   ? "Adding section 'Added', with key NEW = new"
   hIni[ "Added" ] := { => }
   hIni[ "Added" ][ "NEW" ] := "new"

   ? "Writing output to parseini_out1.ini"
   IF hb_IniWrite( "parseini_out1.ini", hIni,;
                   "#Generated file without main auto section; don't touch", "#End of file",;
                   .F. )
      ? "File written"
   ELSE
      ? "Can't write file"
   ENDIF
   ?
   ? "Press any key to next text."
   Inkey( 0 )

   nRow := 3
   @ nRow, 0 CLEAR
   ?
   ? "WRITING INI TO A STRING"
   ?

   cIni := hb_IniWriteStr( hIni )

   ? "Content of hIni : "
   ?
   ? cIni
   ?
   ? "Press any key to next text."
   Inkey( 0 )

   nRow := 3
   @ nRow, 0 CLEAR
   ?
   ? "READING INI FILE FROM A STRING"
   ?

   hIni := hb_IniReadStr( cIni,; /*default case*/
            ,; /*Default key indicators */
            , .F. )

   ? "Content: "

   IF Empty( hIni )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH cSection IN hIni:Keys
         /* Now (without automatic main), toplevel options may be in the root hash */
         aSect := hIni[ cSection ]

         IF hb_isHash( aSect )
            /* It's a section */
            ?
            ? "Section [" + cSection + "]"

            FOR EACH cKey IN aSect:Keys
               ? cKey + " = " + aSect[ cKey ]
            NEXT
         ELSE
            /* It's a toplevel option */
            ? "TOPLEVEL option:", cSection + " = " + aSect
         ENDIF
      NEXT
   ENDIF

   ?
   ? "Press any key to next text."
   Inkey( 0 )

   nRow := 3
   @ nRow, 0 CLEAR
   ?
   ? "WRITING INI FILE TO A STRING "
   ?

   cIni := hb_IniWriteStr( hb_IniRead( cName ) )

   ? "Content of " + cName
   ?
   ? cIni
   ?
   ? "Press any key to next text."
   Inkey( 0 )

   RETURN
