/*
 * $Id$
 */

/*
 * File......: d2e.prg
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

#define log10( num )    log( num ) / log( 10 )
#define DEFAULT_PRECISION     6
#command DEFAULT <p> TO <val> => <p> := iif( <p> == NIL, <val>, <p> )

#ifdef FT_TEST
  PROCEDURE Main( cNum, cPrec )
     DEFAULT cPrec TO str( DEFAULT_PRECISION )
     qout( ft_d2e( val(cNum), val(cPrec) ) )
     RETURN
#endif

function ft_d2e( nDec, nPrecision )
  local nExp, sScn
  DEFAULT nPrecision TO DEFAULT_PRECISION

  if nDec == 0
     nExp := 0
  elseif abs( nDec ) < 1
     nExp := int( log10( nDec ) ) - 1
  else
     nExp := int( log10( abs(nDec)+0.00001 ) )   /* 0.00001 == kludge */
  endif           /* for imprecise logs */

  nDec /= 10 ^ nExp

  if round( abs(nDec), nPrecision ) >= 10
     nDec /= 10
     nExp++
  endif another kludge for stuff like '999999999'

  sScn := ltrim( str( nDec, nPrecision + 3, nPrecision ) )
  return sScn + 'E' + alltrim( str( nExp, 5, 0 ) )
