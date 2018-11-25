/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

#require "hbcups"

PROCEDURE Main( cFile )

   LOCAL i
   LOCAL nWidth

   LOCAL cDefault := cupsGetDefault()
   LOCAL aPrinter

   LOCAL ma, mi, pa

   ? "cups version:", hb_cups_version( @ma, @mi, @pa ), ma, mi, pa

   IF Empty( cDefault )
      ? "No default printer configured"
   ELSE
      ? "Default printer:", cDefault
   ENDIF

   ?
   ? "Cups Printer List:"

   aPrinter := cupsGetDests()

   nWidth := 0
   FOR EACH i IN aPrinter
      ? i:__enumIndex(), i
      nWidth := Max( nWidth, Len( i ) )
   NEXT

   hb_default( @cFile, __FILE__ )

   IF Empty( cDefault )
      WAIT
      CLS
      ? "Please select a printer"
      IF ( i := AChoice( 2, 5, 20, nWidth + 4, aPrinter ) ) > 0
         ? "Using printer:", aPrinter[ i ]
         ? "Printing... Job ID:", cupsPrintFile( aPrinter[ i ], cFile, "Harbour CUPS Printing" )
#if 0
         /* for duplex printing, tested on OKI B410 */
         ? "Printing... Job ID:", cupsPrintFile( aPrinter[ i ], ".." + hb_ps() + "core.c", "Harbour CUPS Printing", { "sides=two-sided-short-edge" } )
#endif
      ENDIF
   ELSE
      ? "Using default printer:", cDefault
      ? "Printing... Job ID:", cupsPrintFile( cDefault, cFile, "Harbour CUPS Printing", { "sides=one-sided" } )
#if 0
      ? "Printing... Job ID:", cupsPrintFile( cDefault, cFile, "Harbour CUPS Printing", { "sides" => "one-sided" } )
#endif
   ENDIF

   RETURN
