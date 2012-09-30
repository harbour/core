/*
 * $Id$
 */

PROCEDURE Main( cParm1 )

   //-------------------------------------------------------
   // Sample routine to test function from command line
   //-------------------------------------------------------

   IF PCount() > 0
      ? FT_ESCCODE( cParm1 )
   ELSE
      ? "Usage: PRT_ESC  'escape code sequence' "
      ? "            outputs converted code to  standard output"
      ?
   ENDIF

   RETURN
