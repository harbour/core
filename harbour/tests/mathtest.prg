/*
 * $Id$
 */

#include "hbmath.ch"

PROCEDURE Main()

   LOCAL nOldMathErrMode
   LOCAL bOldMathErr

   ? "Testing math function: EXP(), LOG() and SQRT():"
   ?
   ? "  I) Test with correct arguments:"
   ? "     exp(0.0) == 1.00         ? ", Exp( 0.0 )
   ? "     exp(1.0) == 2.71(8)...  ? ", Exp( 1.0 )
   ? "     exp(-1.0) == 0.36(7)... ? ", Exp( - 1.0 )
   ?
   ? "     log(1.0) == 0.00        ? ", Log( 1.0 )
   ? "     log(2.7) == 0.99(3)...  ? ", Log( 2.7 )
   ? "     log(0.36) == -1.02(1)... ? ", Log( 0.36 )
   ?
   ? "     sqrt(1.0) == 1.00      ? ", Sqrt( 1.0 )
   ? "     sqrt(4.0) == 2.00      ? ", Sqrt( 4.0 )
   ? "     sqrt(2.0) == 1.41(4).. ? ", Sqrt( 2.0 )
   ?
   ? "  II) Test with numeric but incorrect arguments:"
   ?
   ? "  IIa) default error handling(by the functions themselves)"
   ? "       exp(-1000) == 0.00   ?", Exp( -1000 )
   ? "       exp(1000) == ****... ?", Exp( 1000 )
   ?
   ? "       log(0) == ****...  ?", Log( 0 )
   ? "       log(-10) == *****... ?", Log( -10 )
   ?
   ? "       sqrt(-4) == 0.00 ?", Sqrt( -4 )
   ?

   nOldMathErrMode := hb_matherMode( HB_MATH_ERRMODE_USERDEFAULT )

   ? "  IIb) error handling by error(hb_MathErMode() == HB_MATH_ERRMODE_USERDEFAULT)"
   ? "       exp(-1000) == 0.00   ?", Exp( -1000 )
   ? "       exp(1000) == ****... ?", Exp( 1000 )
   ?
   ? "       log(0) == ****...  ?", Log( 0 )
   ? "       log(-10) == *****... ?", Log( -10 )
   ?
   ? "       sqrt(-4) == 0.00 ?", Sqrt( -4 )
   ?

   hb_matherMode( nOldMathErrMode )

   bOldMathErr := hb_matherBlock( {| nType, cFuncname, cError, nArg1, nArg2, aInfo |;
      localmatherr( nType, cFuncname, cError, nArg1, nArg2, aInfo ) } )

   ? "  IIc) error handling by callback block(hb_MathErBlock())"
   ? "       exp(-1000) == ?", Exp( -1000 )
   ? "       exp(1000) ==  ?", Exp( 1000 )
   ?
   ? "       log(0) ==     ?", Log( 0 )
   ? "       log(-10) ==   ?", Log( -10 )
   ?
   ? "       sqrt(-4) ==   ?", Sqrt( -4 )

   hb_matherBlock( bOldMathErr )

   RETURN

FUNCTION localmatherr( nType, cFuncname, cError, nArg1, nArg2, aInfo )

   LOCAL cStr := "!! Local handling of math error MATH/"

   cStr += hb_ntos( nType ) + " in " + cFuncname + "("

   IF ValType( nArg1 ) == "N"
      cStr += hb_ntos( nArg1 )
   ENDIF
   IF ValType( nArg2 ) == "N"
      cStr += "," + hb_ntos( nArg2 )
   ENDIF
   cStr += "):"
   ? cStr
   ? "!!                              " + cError
   IF aInfo[HB_MATHERRORBLOCK_HANDLED]
      ? "!!                               --> already handled with return value: " + ;
         hb_ntos( aInfo[HB_MATHERRORBLOCK_RETVAL] )
      RETURN 1
   ENDIF

   ? "!!       setting return value to --> 5.0"

   aInfo[ HB_MATHERRORBLOCK_RETVAL ] := 5.0
   aInfo[ HB_MATHERRORBLOCK_HANDLED ] := .T.

   RETURN 1
