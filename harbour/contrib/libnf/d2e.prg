/*
 * File......: D2E.PRG
 * Author....: Gary Baren
 * CIS ID....: 75470,1027
 *
 * This is an original work by Gary Baren and is hereby placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:02   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:24   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   09 Jun 1991 00:27:06   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *      FT_D2E()
 *  $CATEGORY$
 *      Conversion
 *  $ONELINER$
 *		Convert decimal to scientific notation
 *  $SYNTAX$
 *		FT_D2E( <nDec>, <nPrecision> )  -> <cNumE>
 *  $ARGUMENTS$
 *		<nDec>         Decimal number to convert
 *
 *		<nPrecision>   Number of decimal places in result.
 *                     Defaults to 6 decimal places.
 *  $RETURNS$
 *		<cNumE>        A string representing a number in
 *                     scientific notation
 *  $DESCRIPTION$
 *		Given a decimal number and the desired precision,
 *		a string representing the equivalent in scientific
 *		notation is returned.
 *  $EXAMPLES$
 *		? FT_D2E( 12.345, 2 )
 *		  -> 1.23E1
 *
 *		? FT_D2E( -12.345, 3 )
 *		  -> -1.235E1
 *
 *		? FT_D2E( 0.00000543, 2 )
 *		  -> 5.43E-6
 *  $SEEALSO$
 *     FT_E2D()
 *  $END$
 */

#define log10( num )	log( num ) / log( 10 )
#define DEFAULT_PRECISION	6
#command DEFAULT <p> TO <val> => <p> := iif( <p> == NIL, <val>, <p> )

#ifdef FT_TEST
  function main( cNum, cPrec )
     DEFAULT cPrec TO str( DEFAULT_PRECISION )
     return qout( ft_d2e( val(cNum), val(cPrec) ) )
#endif

function ft_d2e( nDec, nPrecision )
  local nExp, sScn
  DEFAULT nPrecision TO DEFAULT_PRECISION

  if nDec == 0
     nExp := 0
  elseif abs( nDec ) < 1
     nExp := int( log10( nDec ) ) - 1
  else
     nExp := int( log10( abs(nDec)+0.00001 ) )   && 0.00001 == kludge
  endif								                    && for imprecise logs

  nDec /= 10 ^ nExp

  if round( abs(nDec), nPrecision ) >= 10
     nDec /= 10
     nExp++
  endif another kludge for stuff like '999999999'

  sScn := ltrim( str( nDec, nPrecision + 3, nPrecision ) )
  return( sScn + 'E' + alltrim( str( nExp, 5, 0 ) ) )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
