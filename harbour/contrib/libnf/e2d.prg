/*
 * File......: E2D.PRG
 * Author....: Gary Baren
 * CIS ID....: 75470,1027
 *
 * This is an original work by Gary Baren and is hereby placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:40   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:03:32   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_E2D()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *		Convert scientific notation string to a decimal
 *  $SYNTAX$
 *		FT_E2D( <cNumE> )  -> <nDec>
 *  $ARGUMENTS$
 *		<cNumE>   Scientific notation string to convert
 *  $RETURNS$
 *		<nDec>    Decimal number
 *  $DESCRIPTION$
 *		Given a string in the format  x.yEz, the decimal
 *		equivalent is returned.
 *  $EXAMPLES$
 *		? FT_E2D( "1.23E1" )
 *		  -> 12.3
 *
 *		? FT_E2D( "-1.235E1" )
 *		  -> -12.35
 *
 *		? ft_d2e( "5.43E-6" )
 *		  -> 0.0000543
 *  $SEEALSO$
 *    FT_D2E()
 *  $END$
 */

#ifdef FT_TEST
  function main( sNumE )
     return qout( FT_E2D( sNumE ) )
#endif

function ft_e2d( sNumE )
  local nMant, nExp

  nMant := val( left( sNumE, at( 'E', sNumE ) - 1 ) )
  nExp  := val(substr( sNumE,                    ;
                 at( 'E', sNumE ) + 1,           ;
                 len( sNumE ) - at( 'E', sNumE ) ;
                     )                           ;
              )
  return( nMant * 10 ^ nExp )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
