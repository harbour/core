/*
 * This is an original work by Forest Belt (Computer Diagnostic Services, Inc.)
 * and is placed in the public domain.
 *
 * Modification history:
 *
 *    Rev 1.2   15 Aug 1991 23:03:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   11 May 1991 00:01:00   GLENN
 * Documentation correction on ft_DoY() oneliner.  Was identical to ft_WoY(),
 * now it's right.
 *
 *    Rev 1.0   01 Apr 1991 01:02:32   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_WoY( dInDate )

   LOCAL nDayOffset, nWkNumber, dFDoY

   IF HB_ISDATE( dInDate )

      dFDoY := hb_Date( Year( dInDate ), 1, 1 )

      // find how many days after first week till dInDate
      nDayOffset := 1 + ( dInDate - dFDoY ) - ;
         ( 8 - DoW( dFDoY ) )  // number of days in first week of year

      // count weeks in offset period
      nWkNumber := 1
      DO WHILE nDayOffset > 0
         ++nWkNumber
         nDayOffset -= 7
      ENDDO

      RETURN nWkNumber
   ENDIF

   RETURN NIL

FUNCTION ft_DoY( dInDate )

   IF HB_ISDATE( dInDate )
      RETURN 1 + dInDate - hb_Date( Year( dInDate ), 1, 1 )
   ENDIF

   RETURN NIL
