/*
 * $Id$
 */

/*
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   11 May 1991 00:01:00   GLENN
 * Documentation correction on ft_doy() oneliner.  Was identical to ft_woy(),
 * now it's right.
 *
 *    Rev 1.0   01 Apr 1991 01:02:32   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_WoY( dInDate )

   LOCAL nFirstDays, nDayOffset, nWkNumber, cCentury

   IF HB_ISDATE( dInDate )

      cCentury := Left( DToS( dInDate ), 4 )

      // find number of days in first week of year
      nFirstDays := 8 - DoW( hb_SToD( cCentury + "0101" ) )

      nWkNumber := 1

      // find how many days after first week till dInDate
      nDayOffset := ( dInDate - hb_SToD( cCentury + "0101" ) ) - nFirstDays + 1

      // count weeks in offset period
      DO WHILE nDayOffset > 0
         ++nWkNumber
         nDayOffset -= 7
      ENDDO

   ELSE
      nWkNumber := NIL
   ENDIF

   RETURN nWkNumber

FUNCTION ft_DoY( dInDate )

   LOCAL nDayNum

   IF HB_ISDATE( dInDate )
      nDayNum := ( dInDate - hb_SToD( Left( DToS( dInDate ), 4 ) + "0101" ) ) + 1
   ELSE
      nDayNum := NIL
   ENDIF

   RETURN nDayNum
