/*
 * $Id$
 */

#require "hbcups"

PROCEDURE Main( cFile )

   LOCAL i
   LOCAL aPrinter

   hb_default( @cFile, __FILE__ )

   IF Empty( cupsGetDefault() )
      ? "No default printer configured"
   ELSE
      ? "Default printer:", cupsGetDefault()
   ENDIF

   ?
   ? "Cups Printer List:"

   FOR EACH i IN cupsGetDests()
      ? i:__enumIndex(), i
   NEXT

   IF Empty( cupsGetDefault() )
      WAIT
      CLS
      aPrinter := cupsGetDests()
      i := AChoice( 2, 5, 30, Len( aPrinter ) + 2, aPrinter )
      ? "Printing... Job ID:", cupsPrintFile( aPrinter[ i ], cFile, "Harbour CUPS Printing" )
#if 0
      /* for duplex printing, tested on OKI B410 */
      ? "Printing... Job ID:", cupsPrintFile( aPrinter[ i ], ".." + hb_ps() + "core.c", "Harbour CUPS Printing", { "sides=two-sided-short-edge" } )
#endif
   ELSE
      ? "Printing... Job ID:", cupsPrintFile( cupsGetDefault(), cFile, "Harbour CUPS Printing", { "sides=one-sided" } )
#if 0
      ? "Printing... Job ID:", cupsPrintFile( cupsGetDefault(), cFile, "Harbour CUPS Printing", { "sides" => "one-sided" } )
#endif
   ENDIF

   RETURN
