/*
 * $Id$
 */

PROCEDURE Main()
   LOCAL i
   LOCAL aPrinter

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
      ? "Printing... Job ID:", cupsPrintFile( aPrinter[ i ], "test.prg", "Harbour CUPS Printing" )
/*    for duplex printing, tested on OKI B410 */
/*    ? "Printing... Job ID:", cupsPrintFile( aPrinter[ i ], "../../../tests/speedstr.prg", "Harbour CUPS Printing", { "sides=two-sided-short-edge" } ) */
   ELSE
      ? "Printing... Job ID:", cupsPrintFile( cupsGetDefault(), "test.prg", "Harbour CUPS Printing", { "sides=one-sided" } )
/*    ? "Printing... Job ID:", cupsPrintFile( cupsGetDefault(), "test.prg", "Harbour CUPS Printing", { "sides" => "one-sided" } ) */
   ENDIF

   RETURN
