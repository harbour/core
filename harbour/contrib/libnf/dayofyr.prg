/*
 * File......: DAYOFYR.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS_ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:35:20   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:03:08   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   10 May 1991 23:59:38   GLENN
 * Minor adjustment to header.
 *
 *    Rev 1.0   01 Apr 1991 01:01:02   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_DAYOFYR()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return calendar, fiscal or accounting day data
 *  $SYNTAX$
 *     FT_DAYOFYR( [ <dGivenDate> ], [ <nDayNum> ], [ <lIsAcct> ] )
 *            -> aDateInfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any valid format.  Defaults
 *     to current system date if not supplied.
 *
 *     <nDayNum> is a number from 1 to 371, signifying a day of a year.
 *     Defaults to current day if not supplied.
 *
 *     <lIsAcct> is a logical which specifies the type of year to base
 *     the return value on:  .F. = calendar or fiscal year,
 *     .T. = accounting year.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        If <nDayNum> is specified:
 *
 *        aDateInfo[1] - The date of the specified day number
 *        aDateInfo[2] - The beginning date of the year
 *        aDateInfo[3] - The ending date of the year
 *
 *        If <nDayNum> is not specified:
 *
 *        aDateInfo[1] - The year and day as a character string "YYYYDDD"
 *        aDateInfo[2] - The beginning date of the year
 *        aDateInfo[3] - The ending date of the year
 *  $DESCRIPTION$
 *     FT_DAYOFYR() returns an array containing data about a day in the
 *     calendar or fiscal year containing the given date.
 *
 *     The beginning of year date defaults to January 1st but may be
 *     changed with FT_DATECNFG().
 *  $EXAMPLES$
 *     aDateInfo := FT_DAYOFYR( CTOD("03/31/91") )
 *     ? aDateInfo[1]        // 1991090    (90th day of year 1991)
 *     ? aDateInfo[2]        // 01/01/91
 *     ? aDateInfo[3]        // 12/31/91
 *
 *     aDateInfo := FT_DAYOFYR( , 90 )    // assume current date is 3/31/91
 *     ? aDateInfo[1]        // 03/31/91    (90th day of year)
 *     ? aDateInfo[2]        // 01/01/91
 *     ? aDateInfo[3]        // 12/31/91
 *
 *     aDateInfo := FT_DAYOFYR( , 90, .T. )
 *     ? aDateInfo[1]        // 03/29/91    (90th day of accounting year)
 *     ? aDateInfo[2]        // 12/30/90    (1st day of accounting year)
 *     ? aDateInfo[3]        // 12/28/91    (last day of accounting year)
 *  $SEEALSO$
 *     FT_DATECNFG()
 *  $END$
*/

FUNCTION FT_DAYOFYR( dGivenDate, nDayNum, lIsAcct)
  LOCAL lIsDay, nTemp, aRetVal

  IF !(VALTYPE(dGivenDate) $ 'NDL')
     dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'N'
     nDayNum    := dGivenDate
     dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'L'
     lIsAcct    := dGivenDate
     dGivenDate := DATE()
  ENDIF

  lIsDay  := VALTYPE(nDayNum) == 'N'
  lIsAcct := VALTYPE(lIsAcct) == 'L'

  IF lIsAcct
     aRetVal := FT_ACCTYEAR(dGivenDate)
  ELSE
     aRetVal := FT_YEAR(dGivenDate)
  ENDIF

  IF lIsDay
     nTemp := aRetVal[3] - aRetVal[2] + 1
     IF(nDayNum < 1 .OR. nDayNum > nTemp , nDayNum := nTemp, )
     aRetVal[1] := aRetVal[2] + nDayNum - 1
  ELSE
     aRetVal[1] += PADL(LTRIM(STR( dGivenDate - aRetVal[2] + 1, 3)), 3, '0')
  ENDIF

RETURN aRetVal
