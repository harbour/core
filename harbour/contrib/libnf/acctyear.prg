/*
 * File......: ACCTYEAR.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:29:14   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:02:40   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:48   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:28   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_ACCTYEAR()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return accounting year data
 *  $SYNTAX$
 *     FT_ACCTYEAR( [ <dGivenDate> ] ) -> aDateInfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year as a character string "YYYY"
 *        aDateInfo[2] - The beginning date of the accounting year
 *        aDateInfo[3] - The ending date of the accounting year
 *  $DESCRIPTION$
 *     FT_ACCTYEAR() creates an array containing data about the
 *     accounting year containing the given date.
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
 *     // get info about accounting year containing 9/15/90
 *     aDateInfo := FT_ACCTYEAR( CTOD("09/15/90") )
 *     ? aDateInfo[1]   //  1990
 *     ? aDateInfo[2]   //  12/31/89    beginning of year
 *     ? aDateInfo[3]   //  12/29/90    end of year
 *  $SEEALSO$
 *     FT_DATECNFG() FT_ACCTWEEK() FT_ACCTMONTH() FT_ACCTQTR()
 *  $END$
*/
 
FUNCTION FT_ACCTYEAR(dGivenDate)
 
  LOCAL nYTemp, aRetVal
 
  IF( VALTYPE(dGivenDate) != 'D', dGivenDate := DATE(), )
 
  aRetVal    := FT_YEAR(dGivenDate)
  nYTemp     := VAL(aRetVal[1])
  aRetVal[2] := FT_ACCTADJ(aRetVal[2])
  aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
 
  IF dGivenDate < aRetVal[2]
    aRetVal    := FT_YEAR(FT_MADD(dGivenDate, -1))
    nYTemp --
    aRetVal[2] := FT_ACCTADJ(aRetVal[2])
    aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
  ELSEIF dGivenDate > aRetVal[3]
    aRetVal    := FT_YEAR(FT_MADD(dGivenDate, 1))
    nYTemp ++
    aRetVal[2] := FT_ACCTADJ(aRetVal[2])
    aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
  ENDIF
 
  aRetVal[1] := STR(nYTemp,4)
 
RETURN aRetVal


