#include "hbmath.ch"

PROCEDURE Main()

   LOCAL nOldMathErrMode
   LOCAL bOldMathErr

   CLS

   ? "Testing math function: Exp(), Log() and Sqrt():"
   ?
   ? "  I) Test with correct arguments:"
   ? "     Exp(0.0) == 1.00        ? ", Exp( 0.0 )
   ? "     Exp(1.0) == 2.71(8)...  ? ", Exp( 1.0 )
   ? "     Exp(-1.0) == 0.36(7)... ? ", Exp( -1.0 )
   ?
   ? "     Log(1.0) == 0.00         ? ", Log( 1.0 )
   ? "     Log(2.7) == 0.99(3)...   ? ", Log( 2.7 )
   ? "     Log(0.36) == -1.02(1)... ? ", Log( 0.36 )
   ?
   ? "     Sqrt(1.0) == 1.00      ? ", Sqrt( 1.0 )
   ? "     Sqrt(4.0) == 2.00      ? ", Sqrt( 4.0 )
   ? "     Sqrt(2.0) == 1.41(4).. ? ", Sqrt( 2.0 )
   ?
   ? "  II) Test with numeric but incorrect arguments:"
   ?
   ? "  IIa) default error handling(by the functions themselves)"
   ? "       Exp(-1000) == 0.00   ?", Exp( -1000 )
   ? "       Exp(1000) == ****... ?", Exp( 1000 )
   ?
   ? "       Log(0) == ****...    ?", Log( 0 )
   ? "       Log(-10) == *****... ?", Log( -10 )
   ?
   ? "       Sqrt(-4) == 0.00 ?", Sqrt( -4 )
   ?

   nOldMathErrMode := hb_matherMode( HB_MATH_ERRMODE_USERDEFAULT )

   ? "  IIb) error handling by error(hb_matherMode() == HB_MATH_ERRMODE_USERDEFAULT)"
   ? "       Exp(-1000) == 0.00   ?", Exp( -1000 )
   ? "       Exp(1000) == ****... ?", Exp( 1000 )
   ?
   ? "       Log(0) == ****...    ?", Log( 0 )
   ? "       Log(-10) == *****... ?", Log( -10 )
   ?
   ? "       Sqrt(-4) == 0.00 ?", Sqrt( -4 )
   ?

   hb_matherMode( nOldMathErrMode )

   bOldMathErr := hb_matherBlock( {| nType, cFuncname, cError, nArg1, nArg2, aInfo | ;
      localmatherr( nType, cFuncname, cError, nArg1, nArg2, aInfo ) } )

   ? "  IIc) error handling by callback block(hb_matherBlock())"
   ? "       Exp(-1000) == ?", Exp( -1000 )
   ? "       Exp(1000) ==  ?", Exp( 1000 )
   ?
   ? "       Log(0) ==     ?", Log( 0 )
   ? "       Log(-10) ==   ?", Log( -10 )
   ?
   ? "       Sqrt(-4) ==   ?", Sqrt( -4 )

   hb_matherBlock( bOldMathErr )

   RETURN

STATIC FUNCTION localmatherr( nType, cFuncname, cError, nArg1, nArg2, aInfo )

   LOCAL cStr := "!! Local handling of math error MATH/"

   cStr += hb_ntos( nType ) + " in " + cFuncname + "("

   IF HB_ISNUMERIC( nArg1 )
      cStr += hb_ntos( nArg1 )
   ENDIF
   IF HB_ISNUMERIC( nArg2 )
      cStr += "," + hb_ntos( nArg2 )
   ENDIF
   cStr += "):"
   ? cStr
   ? "!!                             ", cError
   IF aInfo[ HB_MATHERRORBLOCK_HANDLED ]
      ? "!!                               --> already handled with return value:", ;
         hb_ntos( aInfo[ HB_MATHERRORBLOCK_RETVAL ] )
      RETURN 1
   ENDIF

   ? "!!       setting return value to --> 5.0"

   aInfo[ HB_MATHERRORBLOCK_RETVAL ] := 5.0
   aInfo[ HB_MATHERRORBLOCK_HANDLED ] := .T.

   RETURN 1
