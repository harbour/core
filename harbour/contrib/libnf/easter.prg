/*
 * File......: EASTER.PRG
 * Author....: Paul Tucker
 * CIS ID....: ?
 *
 * While I can say that I wrote the program, the algorithm is from Donald
 * Knuth's The Art of Computer Programming, Section 1.3.2.  So, the source
 * code is an original work by Paul Tucker and is placed in the public
 * domain
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:42   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:16   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_EASTER()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return the date of Easter
 *  $SYNTAX$
 *     FT_EASTER( <xYear> ) -> dEdate
 *  $ARGUMENTS$
 *     xYear can be a character, date or numeric describing the year
 *     for which you wish to receive the date of Easter.
 *  $RETURNS$
 *     The actual date that Easter occurs.
 *  $DESCRIPTION$
 *     Returns the date of Easter for any year after 1582 up to Clipper's
 *     limit which the manual states is 9999, but the Guide agrees with
 *     the actual imposed limit of 2999.
 *
 *     This function can be useful in calender type programs that indicate
 *     when holidays occur.
 *  $EXAMPLES$
 *     dEdate := FT_EASTER( 1990 )     && returns 04/15/1990
 *  $END$
 */


FUNCTION FT_EASTER (nYear)
  local nGold, nCent, nCorx, nCorz, nSunday, nEpact, nMoon,;
        nMonth := 0, nDay := 0, lCent := __SetCentury( .t. )

  // --------------------------------
  // NOTE: __SetCentury() is internal
  // --------------------------------

  IF VALTYPE (nYear) == "C"
     nYear = VAL(nYear)
  ENDIF

  IF VALTYPE (nYear) == "D"
     nYear = YEAR(nYear)
  ENDIF

  IF VALTYPE (nYear) == "N"
     IF nYear > 1582

        * <<nGold>> is Golden number of the year in the 19 year Metonic cycle
        nGold = nYear % 19 + 1

        * <<nCent>> is Century
        nCent = INT (nYear / 100) + 1

        * Corrections:
        * <<nCorx>> is the no. of years in which leap-year was dropped in order
        * to keep step with the sun
        nCorx = INT ((3 * nCent) / 4 - 12)

        * <<nCorz>> is a special correction to synchronize Easter with the moon's
        * orbit.
        nCorz = INT ((8 * nCent + 5) / 25 - 5)

        * <<nSunday>> Find Sunday
        nSunday = INT ((5 * nYear) / 4 - nCorx - 10)

        * Set Epact <<nEpact>> (specifies occurance of a full moon)
        nEpact = INT ((11 * nGold + 20 + nCorz - nCorx) % 30)

        IF nEpact < 0
           nEpact += 30
        ENDIF

        IF ((nEpact = 25) .AND. (nGold > 11)) .OR. (nEpact = 24)
           ++nEpact
        ENDIF

        * Find full moon - the <<nMoon>>th of MARCH is a "calendar" full moon
        nMoon = 44 - nEpact

        IF nMoon < 21
           nMoon += 30
        ENDIF

        * Advance to Sunday
        nMoon = INT (nMoon + 7 - ((nSunday + nMoon) % 7))

        * Get Month and Day
        IF nMoon > 31
           nMonth = 4
           nDay = nMoon - 31
        ELSE
           nMonth = 3
           nDay = nMoon
        ENDIF
     ENDIF
  ELSE
     nYear = 0
  ENDIF

  set century (lCent)

RETURN  CTOD (RIGHT ("00"+LTRIM (STR (nMonth)),2) + "/" +;
         RIGHT ("00"+LTRIM (STR (INT (nDay))) ,2) + "/" +STR (nYear,4))
