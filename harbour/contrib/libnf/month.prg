/*
 * File......: MONTH.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:40:00   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:28   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:46   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_MONTH()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return Calendar or Fiscal Month Data
 *  $SYNTAX$
 *     FT_MONTH( [ <dGivenDate> ], [nMonthNum] ) -> aDateInfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *
 *     <nMonthNum> is a number from 1 to 12 signifying a month.
 *     Defaults to current month if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year and month as a character string "YYYYMM"
 *        aDateInfo[2] - The beginning date of the month
 *        aDateInfo[3] - The ending date of the month
 *  $DESCRIPTION$
 *     FT_MONTH() returns an array containing data about the month
 *     containing the given date.
 *
 *     Normally the return data will be based on a year beginning
 *     on January 1st with weeks beginning on Sunday.
 *
 *     The beginning of year date and/or beginning of week day can be
 *     changed by using FT_DATECNFG(), which will affect all subsequent
 *     calls to FT_MONTH() until another call to FT_DATECNFG().
 *
 *     The beginning of year date and beginning of week day may be reset
 *     to January 1 and Sunday by calling FT_DATECNFG() with no
 *     parameters.
 *  $EXAMPLES$
 *     // get info about month containing 9/15/90
 *     aDateInfo := FT_MONTH( CTOD("09/15/90") )
 *     ? aDateInfo[1]   //  199009       (9th month)
 *     ? aDateInfo[2]   //  09/01/90     beginning of month 9
 *     ? aDateInfo[3]   //  09/30/90     end of week month 9
 *
 *     // get info about month 5 in year containing 9/15/90
 *     aDateInfo := FT_MONTH( CTOD("09/15/90"), 5 )
 *     ? aDateInfo[1]   //  199005
 *     ? aDateInfo[2]   //  05/01/90   beginning of month 5
 *     ? aDateInfo[3]   //  05/31/90   end of month 5
 *
 *     // get info about month 5 in current year (1991)
 *     aDateInfo := FT_MONTH( , 5 )
 *     ? aDateInfo[1]   //  199105
 *     ? aDateInfo[2]   //  05/01/91   beginning of month 5
 *     ? aDateInfo[3]   //  05/31/91   end of month 5
 *  $SEEALSO$
 *     FT_DATECNFG() FT_WEEK() FT_QTR() FT_YEAR()
 *  $END$
*/

FUNCTION FT_MONTH( dGivenDate, nMonthNum )
LOCAL lIsMonth, nTemp, aRetVal

  IF !( VALTYPE(dGivenDate) $ 'ND')
     dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'N'
     nMonthNum  := dGivenDate
     dGivenDate := DATE()
  ENDIF

  aRetVal   := FT_YEAR(dGivenDate)

  lIsMonth  := ( VALTYPE(nMonthNum) == 'N' )
  IF lISMonth
     IF( nMonthNum < 1 .OR. nMonthNum > 12, nMonthNum := 12, )
     dGivenDate := FT_MADD(aRetVal[2], nMonthNum - 1)
  ENDIF

  nTemp := MONTH( dGivenDate ) - MONTH( aRetVal[2] )
  nTemp += IF(nTemp >= 0, 1, 13)

  aRetVal[1] += PADL(LTRIM(STR(nTemp, 2)), 2, '0')
  aRetVal[2] := FT_MADD( aRetVal[2], nTemp - 1 )
  aRetVal[3] := FT_MADD( aRetVal[2], 1 ) - 1

RETURN aRetVal

