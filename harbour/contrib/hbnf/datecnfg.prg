/*
 * $Id$
 */

/*
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74730,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:34:08   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:10   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:26   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:00   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_DATECNFG( cFYStart, nDow )

   THREAD STATIC t_aDatePar := { "1980.01.01", 1 }

   LOCAL dCheck
   LOCAL cDateFormat

   IF HB_ISSTRING( cFYStart )
      dCheck := CToD( cFYStart )
      IF ! Empty( dCheck )

         /* No one starts a Fiscal Year on 2/29 */
         IF Month( dCheck ) == 2 .AND. Day( dcheck ) == 29
            dCheck--
         ENDIF

         cDateFormat := Set( _SET_DATEFORMAT, "yyyy.mm.dd" )
         t_aDatePar[ 1 ] := DToC( dCheck )
         Set( _SET_DATEFORMAT, cDateFormat )
      ENDIF
   ENDIF

   IF HB_ISNUMERIC( nDow ) .AND. nDow > 0 .AND. nDow < 8
      t_aDatePar[ 2 ] := nDow
   ENDIF

   RETURN AClone( t_aDatePar )
