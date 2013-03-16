/*
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:39:04   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:03:58   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:38   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_MAdd( dGivenDate, nAddMonths, lMakeEOM )

   LOCAL nAdjDay, dTemp, i

   hb_default( @dGivenDate, Date() )
   hb_default( @nAddMonths, 0 )
   hb_default( @lMakeEOM, .F. )

   nAdjDay := Day( dGivenDate ) - 1

   /* If givendate is end of month and lMakeEom, then force EOM.*/

   lMakeEom := ( lMakeEom .AND. dGivenDate == dGivenDate - nAdjDay + 31 - ;
      Day( dGivenDate - nAdjDay + 31 ) )

   dTemp := dGivenDate - nAdjDay     // first of month

   /* Work with 1st of months.*/
   FOR i := 1 TO Abs( nAddMonths )
      dTemp += iif( nAddMonths > 0, 31, -1 )
      dTemp += 1 - Day( dTemp )
   NEXT

   IF lMakeEom
      dTemp += 31 - Day( dTemp + 31 )
   ELSE
      dTemp := Min( dTemp + nAdjday, ( dTemp += 31 - Day( dTemp + 31 ) ) )
   ENDIF

   RETURN dTemp
