/*
 * File......: ACCTWEEK.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:27:50   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:02:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:28   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_ACCTWEEK()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return accounting week data
 *  $SYNTAX$
 *     FT_ACCTWEEK( [ <dGivenDate> ], [ <nWeekNum> ] ) -> aDateInfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *
 *     <nWeekNum> is a number from 1 to 52 signifying a week.
 *     Defaults to current week if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year and week as a character string "YYYYWW"
 *        aDateInfo[2] - The beginning date of the accounting week
 *        aDateInfo[3] - The ending date of the accounting week
 *  $DESCRIPTION$
 *     FT_ACCTWEEK() returns an array containing data about the
 *     accounting week containing the given date.
 *
 *     An accounting period has the following characteristics:
 *
 *     If the first week of the period contains 4 or more 'work'
 *     days, it is included in the period; otherwise, the first
 *     week was included in the prior period.
 *
 *     If the last week of the period contains 4 or more 'work'
 *     days it is included in the period; otherwise, the last week
 *     is included in the next period.  This results in 13 week
 *     'quarters' and 4 or 5 week 'months'.  Every 5 or 6 years, a
 *     'quarter' will contain 14 weeks and the year will contain 53
 *     weeks.
 *  $EXAMPLES$
 *     // get info about accounting week containing 9/15/90
 *     aDateInfo := FT_ACCTWEEK( CTOD("09/15/90") )
 *     ? aDateInfo[1]   //  199037       (37th week)
 *     ? aDateInfo[2]   //  09/09/90     beginning of week 37
 *     ? aDateInfo[3]   //  09/15/90     end of week 37
 *
 *     // get info about accounting week 25 in year containing 9/15/90
 *     aDateInfo := FT_ACCTWEEK( CTOD("09/15/90"), 25 )
 *     ? aDateInfo[1]   //  199025
 *     ? aDateInfo[2]   //  06/17/89   beginning of week 25
 *     ? aDateInfo[3]   //  06/23/90   end of week 25
 *  $SEEALSO$
 *     FT_DATECNFG() FT_ACCTMONTH() FT_ACCTQTR() FT_ACCTYEAR()
 *  $END$
*/
 
FUNCTION FT_ACCTWEEK(dGivenDate,nWeekNum)
 
  LOCAL nTemp, lIsWeek, aRetVal
 
  IF ! VALTYPE(dGivenDate) $ 'ND'
     dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'N'
     nWeekNum := dGivenDate
     dGivenDate := DATE()
  ENDIF
 
  aRetVal := FT_ACCTYEAR(dGivenDate)
 
  lIsWeek := ( VALTYPE(nWeekNum) == 'N' )
  IF lIsWeek
     nTemp      := INT( (aRetVal[3] - aRetVal[2]) / 7 ) + 1
     IF( nWeekNum < 1 .OR. nWeekNum > nTemp, nWeekNum := nTemp, )
     dGivenDate := aRetVal[2] + (nWeekNum - 1) * 7
  ENDIF
 
  aRetVal[1] += PADL(LTRIM(STR(INT( (dGivenDate - ;
                aRetVal[2]) / 7 ) + 1, 2)), 2, '0')
  dGivenDate += ( 6 - FT_DAYTOBOW(dGivenDate) )  // end of week
  aRetVal[2] := dGivenDate - 6
  aRetVal[3] := dGivenDate
 
RETURN aRetVal

