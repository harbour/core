/* While I can say that I wrote the program, the algorithm is from Donald
   Knuth's The Art of Computer Programming, Section 1.3.2.  So, the source
   code is an original work by Paul Tucker <ptucker@sympatico.ca>
   and is placed in the public domain

      Rev 1.2   15 Aug 1991 23:05:28   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:51:42   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:01:16   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_Easter( nYear )

   LOCAL nGold, nCent, nCorx, nCorz, nSunday, nEpact, nMoon
   LOCAL nMonth := 0, nDay := 0

   DO CASE
   CASE HB_ISSTRING( nYear ) ; nYear := Val( nYear )
   CASE HB_ISDATE( nYear )   ; nYear := Year( nYear )
   ENDCASE

   IF HB_ISNUMERIC( nYear )

      IF nYear > 1582

         // <nGold> is Golden number of the year in the 19 year Metonic cycle
         nGold := nYear % 19 + 1

         // <nCent> is Century
         nCent := Int( nYear / 100 ) + 1

         // Corrections:
         // <nCorx> is the no. of years in which leap-year was dropped in order
         // to keep step with the sun
         nCorx := Int( ( 3 * nCent ) / 4 - 12 )

         // <nCorz> is a special correction to synchronize Easter with the moon's orbit
         nCorz := Int( ( 8 * nCent + 5 ) / 25 - 5 )

         // <nSunday> Find Sunday
         nSunday := Int( ( 5 * nYear ) / 4 - nCorx - 10 )

         // Set Epact <nEpact> (specifies occurance of a full moon)
         nEpact := Int( ( 11 * nGold + 20 + nCorz - nCorx ) % 30 )

         IF nEpact < 0
            nEpact += 30
         ENDIF

         IF ( nEpact == 25 .AND. nGold > 11 ) .OR. nEpact == 24
            ++nEpact
         ENDIF

         nMoon := 44 - nEpact  // Find full moon - the <nMoon>th of March is a "calendar" full moon

         IF nMoon < 21
            nMoon += 30
         ENDIF

         nMoon := Int( nMoon + 7 - ( nSunday + nMoon ) % 7 )  // Advance to Sunday

         // Get Month and Day
         IF nMoon > 31
            nMonth := 4
            nDay := nMoon - 31
         ELSE
            nMonth := 3
            nDay := nMoon
         ENDIF
      ENDIF
   ELSE
      nYear := 0
   ENDIF

   RETURN hb_Date( nYear, nMonth, nDay )
