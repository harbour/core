/*
 * File......: WEEK.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:44:52   GLENN
 * Jo French cleaned up and correct to bow().
 *
 *    Rev 1.2   15 Aug 1991 23:05:26   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:16   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:30   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_WEEK()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return calendar or fiscal week data
 *  $SYNTAX$
 *     FT_WEEK( [ <dGivenDate> ], [ <nWeekNum> ] ) -> aDateinfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *
 *     <nWeekNum> is a number from 1 to 53 signifying a week.
 *     Defaults to current week if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year and week as a character string "YYYYWW"
 *        aDateInfo[2] - The beginning date of the week
 *        aDateInfo[3] - The ending date of the week
 *  $DESCRIPTION$
 *     FT_WEEK() returns an array containing data about the week
 *     containing the given date.
 *
 *     Normally the return data will be based on a year beginning
 *     on January 1st with weeks beginning on Sunday.
 *
 *     The beginning of year date and/or beginning of week day can be
 *     changed by using FT_DATECNFG(), which will affect all subsequent
 *     calls to FT_WEEK() until another call to FT_DATECNFG().
 *
 *     The beginning of year date and beginning of week day may be reset
 *     to January 1 and Sunday by calling FT_DATECNFG() with no
 *     parameters.
 *  $EXAMPLES$
 *     // get info about week containing 9/15/90
 *     aDateInfo := FT_WEEK( CTOD("09/15/90") )
 *     ? aDateInfo[1]   //  199037       (37th week)
 *     ? aDateInfo[2]   //  09/09/90     beginning of week 37
 *     ? aDateInfo[3]   //  09/15/90     end of week 37
 *
 *     // get info about week 25 in year containing 9/15/90
 *     aDateInfo := FT_WEEK( CTOD("09/15/90"), 25 )
 *     ? aDateInfo[1]   //  199025
 *     ? aDateInfo[2]   //  06/17/90   beginning of week 25
 *     ? aDateInfo[3]   //  06/23/90   end of week 25
 *
 *     // get info about week 25 in current year( 1991 )
 *     aDateInfo := FT_WEEK( , 25 )
 *     ? aDateInfo[1]   //  199025
 *     ? aDateInfo[2]   //  06/16/91   beginning of week 25
 *     ? aDateInfo[3]   //  06/22/91   end of week 25
 *  $SEEALSO$
 *     FT_DATECNFG() FT_MONTH() FT_QTR() FT_YEAR() FT_DAYTOBOW()
 *  $END$
*/

FUNCTION FT_WEEK( dGivenDate, nWeekNum )
LOCAL lIsWeek, nTemp, aRetVal, dTemp

  IF ! (VALTYPE(dGivenDate) $ 'ND')
     dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'N'
     nWeekNum   := dGivenDate
     dGivenDate := DATE()
  ENDIF

  aRetVal    := FT_YEAR(dGivenDate)
  dTemp      := aRetVal[2]
  aRetVal[2] -= FT_DAYTOBOW( aRetVal[2] )

  lIsWeek := ( VALTYPE(nWeekNum) == 'N' )
  IF lIsWeek
     nTemp := INT( (aRetVal[3] - aRetVal[2]) / 7 ) + 1
     IF(nWeekNum < 1 .OR. nWeekNum > nTemp , nWeekNum := nTemp, )
     dGivenDate := aRetVal[2] + (nWeekNum - 1) * 7
  ENDIF

  dGivenDate += ( 6 - FT_DAYTOBOW(dGivenDate) )       // end of week

  aRetVal[1] += PADL(LTRIM(STR(INT( (dGivenDate - ;
                aRetVal[2]) / 7 ) + 1, 2)), 2, '0')
  aRetVal[2] := MAX( dGivenDate - 6, dTemp )
  aRetVal[3] := MIN( dGivenDate, aRetVal[3] )

RETURN aRetVal
