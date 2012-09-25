/*
 * $Id$
 */

/*
 * File......: hex2dec.prg
 * Author....: Robert A. DiFalco
 * CIS ID....: ?
 *
 * This is an original work by Robert DiFalco and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:32:56   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:03:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:58   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:28   GLENN
 * Nanforum Toolkit
 *
 */

#define HEXTABLE "0123456789ABCDEF"

#ifdef FT_TEST
  PROCEDURE Main( cHexNum )
     QOut( FT_HEX2DEC( cHexNum ) )
     RETURN
#endif

FUNCTION FT_HEX2DEC( cHexNum )
   local n, nDec := 0, nHexPower := 1

   for n := len( cHexNum ) to 1 step -1
      nDec += ( at( subs( upper(cHexNum), n, 1 ), HEXTABLE ) - 1 ) * nHexPower
      nHexPower *= 16
   next

RETURN nDec
