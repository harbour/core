/*
 * $Id$
 */

#include "hbmath.ch"

PROCEDURE Main()

   LOCAL nOldMathErrMode
   LOCAL bOldMathErr

   QOut( "Testing math function: EXP(), LOG() and SQRT():" )
   QOut( "" )
   QOut( "  I) Test with correct arguments:" )
   QOut( "     exp(0.0) == 1.00         ? ", Exp( 0.0 ) )
   QOut( "     exp(1.0) == 2.71(8)...  ? ", Exp( 1.0 ) )
   QOut( "     exp(-1.0) == 0.36(7)... ? ", Exp( - 1.0 ) )
   QOut( "" )
   QOut( "     log(1.0) == 0.00        ? ", Log( 1.0 ) )
   QOut( "     log(2.7) == 0.99(3)...  ? ", Log( 2.7 ) )
   QOut( "     log(0.36) == -1.02(1)... ? ", Log( 0.36 ) )
   QOut( "" )
   QOut( "     sqrt(1.0) == 1.00      ? ", Sqrt( 1.0 ) )
   QOut( "     sqrt(4.0) == 2.00      ? ", Sqrt( 4.0 ) )
   QOut( "     sqrt(2.0) == 1.41(4).. ? ", Sqrt( 2.0 ) )
   QOut( "" )
   QOut( "  II) Test with numeric but incorrect arguments:" )
   QOut( "" )
   QOut( "  IIa) default error handling(by the functions themselves)" )
   QOut( "       exp(-1000) == 0.00   ?", Exp( - 1000 ) )
   QOut( "       exp(1000) == ****... ?", Exp( 1000 ) )
   QOut( "" )
   QOut( "       log(0) == ****...  ?", Log( 0 ) )
   QOut( "       log(-10) == *****... ?", Log( - 10 ) )
   QOut( "" )
   QOut( "       sqrt(-4) == 0.00 ?", Sqrt( - 4 ) )
   QOut( "" )

   nOldMathErrMode := hb_matherMode( HB_MATH_ERRMODE_USERDEFAULT )

   QOut( "  IIb) error handling by error(hb_MathErMode() == HB_MATH_ERRMODE_USERDEFAULT)" )
   QOut( "       exp(-1000) == 0.00   ?", Exp( - 1000 ) )
   QOut( "       exp(1000) == ****... ?", Exp( 1000 ) )
   QOut( "" )
   QOut( "       log(0) == ****...  ?", Log( 0 ) )
   QOut( "       log(-10) == *****... ?", Log( - 10 ) )
   QOut( "" )
   QOut( "       sqrt(-4) == 0.00 ?", Sqrt( - 4 ) )
   QOut( "" )

   hb_matherMode( nOldMathErrMode )

   bOldMathErr := hb_matherBlock( {| nType, cFuncname, cError, nArg1, nArg2, aInfo |;
      localmatherr( nType, cFuncname, cError, nArg1, nArg2, aInfo ) } )

   QOut( "  IIc) error handling by callback block(hb_MathErBlock())" )
   QOut( "       exp(-1000) == ?", Exp( - 1000 ) )
   QOut( "       exp(1000) ==  ?", Exp( 1000 ) )
   QOut( "" )
   QOut( "       log(0) ==     ?", Log( 0 ) )
   QOut( "       log(-10) ==   ?", Log( - 10 ) )
   QOut( "" )
   QOut( "       sqrt(-4) ==   ?", Sqrt( - 4 ) )

   hb_matherBlock( bOldMathErr )

   RETURN

FUNCTION localmatherr( nType, cFuncname, cError, nArg1, nArg2, aInfo )

   LOCAL cStr := "!! Local handling of math error MATH/"

   cStr += AllTrim( Str( nType ) ) + " in " + cFuncname + "("

   IF ValType( nArg1 ) == "N"
      cStr += AllTrim( Str( nArg1 ) )
   ENDIF
   IF ValType( nArg2 ) == "N"
      cStr += "," + AllTrim( Str( nArg2 ) )
   ENDIF
   cStr += "):"
   QOut( cStr )
   QOut( "!!                              " + cError )
   IF aInfo[HB_MATHERRORBLOCK_HANDLED]
      QOut( "!!                               --> already handled with return value: " + ;
         AllTrim( Str( aInfo[HB_MATHERRORBLOCK_RETVAL] ) ) )
      RETURN 1
   ENDIF

   QOut( "!!       setting return value to --> 5.0" )

   aInfo[ HB_MATHERRORBLOCK_RETVAL ] := 5.0
   aInfo[ HB_MATHERRORBLOCK_HANDLED ] := .T.

   RETURN 1
