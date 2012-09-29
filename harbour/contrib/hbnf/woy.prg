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

FUNCTION FT_WOY( dInDate )

   LOCAL nFirstDays, nDayOffset, nWkNumber, cCentury

   IF ! HB_ISDATE( dInDate )
      nWkNumber := NIL
   ELSE

      // resolve century issue
      IF Len( DToC( dInDate ) ) > 8                  // CENTURY is on
         cCentury := SubStr( DToC( dInDate ), 7, 4 )
      ELSE
         cCentury := SubStr( DToC( dInDate ), 7, 2 )
      ENDIF

      // find number of days in first week of year

      nFirstDays := 8 - ( DOW( CToD( "01/01/" + cCentury ) ) )

      nWkNumber  := 1

      // find how many days after first week till dInDate

      nDayOffset := ( dInDate - ;
         CToD( "01/01/" + cCentury ) ) - nFirstDays + 1

      // count weeks in offset period

      DO WHILE nDayOffset > 0
         ++nWkNumber
         nDayOffset -= 7
      ENDDO

   ENDIF

   RETURN nWkNumber

FUNCTION FT_DOY( dInDate )

   LOCAL nDayNum, cCentury

   IF ! HB_ISDATE( dInDate )
      nDayNum := NIL
   ELSE

      // resolve century issue
      IF Len( DToC( dInDate ) ) > 8                  // CENTURY is on
         cCentury := SubStr( DToC( dInDate ), 7, 4 )
      ELSE
         cCentury := SubStr( DToC( dInDate ), 7, 2 )
      ENDIF

      // calculate
      nDayNum := ( dInDate - CToD( "01/01/" + cCentury ) ) + 1

   ENDIF

   RETURN nDayNum
