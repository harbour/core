/*
 * $Id$
 */

/*
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:06:14   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 20:58:56   GLENN
 * Two locals, nSECS1 and nSECS2, were not declared; this was fixed.
 *
 *    Rev 1.0   07 Jun 1991 23:39:46   GLENN
 * Initial revision.
 *
 */

FUNCTION FT_ELTIME( cTIME1, cTIME2 )

   LOCAL  nDELSECS, nHRS, nMINS, nSECS, nSECS1, nSECS2

   nSECS1   := ( Val( SubStr( cTIME1, 1, 2 ) ) * 3600 ) + ;
      ( Val( SubStr( cTIME1, 4, 2 ) ) * 60 ) + Val( SubStr( cTIME1, 7 ) )
   nSECS2   := ( Val( SubStr( cTIME2, 1, 2 ) ) * 3600 ) + ;
      ( Val( SubStr( cTIME2, 4, 2 ) ) * 60 ) + Val( SubStr( cTIME2, 7 ) )
   nDELSECS := Abs( nSECS2 - nSECS1 )
   nHRS     := Int( nDELSECS / 3600 )
   nMINS    := Int( ( nDELSECS - nHRS * 3600 ) / 60 )
   nSECS    := nDELSECS - ( nHRS * 3600 ) - ( nMINS * 60 )

   RETURN Right( "00" + hb_ntos( nHRS ), 2 ) + ;
          ":" + ;
          Right( "00" + hb_ntos( nMINS ), 2 ) + ;
          ":" + ;
          Right( "00" + hb_ntos( nSECS ), 2 )
