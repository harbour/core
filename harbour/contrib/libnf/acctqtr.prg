/*
 * File......: ACCTQTR.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:26:30   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:02:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:44   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:26   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_ACCTQTR()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return accounting quarter data
 *  $SYNTAX$
 *     FT_ACCTQTR( [ <dGivenDate> ], [ <nQtrNum> ] ) -> aDateinfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *
 *     <nQtrNum> is a number from 1 to 4 signifying a quarter.
 *     Defaults to current quarter if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year and qtr. as a character string "YYYYQQ"
 *        aDateInfo[2] - The beginning date of the accounting quarter
 *        aDateInfo[3] - The ending date of the accounting quarter
 *  $DESCRIPTION$
 *     FT_ACCTQTR() creates an array containing data about the
 *     accounting quarter containing the given date.
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
 *     // get info about accounting month containing 9/15/90
 *     aDateInfo := FT_ACCTQTR( CTOD("09/15/90") )
 *     ? aDateInfo[1]   //  199003       (3rd quarter)
 *     ? aDateInfo[2]   //  07/01/90     beginning of quarter 3
 *     ? aDateInfo[3]   //  09/29/90     end of quarter 3
 *
 *     // get info about accounting qtr. 2 in year containing 9/15/90
 *     aDateInfo := FT_ACCTQTR( CTOD("09/15/90"), 2 )
 *     ? aDateInfo[1]   //  199002
 *     ? aDateInfo[2]   //  04/01/89   beginning of quarter 2
 *     ? aDateInfo[3]   //  06/30/90   end of quarter 2
 *  $SEEALSO$
 *     FT_DATECNFG() FT_ACCTWEEK() FT_ACCTMONTH() FT_ACCTYEAR()
 *  $END$
*/
 
FUNCTION FT_ACCTQTR(dGivenDate,nQtrNum)
  LOCAL nYTemp, nQTemp, lIsQtr, aRetVal
 
  IF ! ( VALTYPE(dGivenDate) $ 'ND' )
    dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'N'
    nQtrNum    := dGivenDate
    dGivenDate := DATE()
  ENDIF
  aRetVal    := FT_QTR(dGivenDate)
  nYTemp     := VAL(SUBSTR(aRetVal[1],1,4))
  nQTemp     := VAL(SUBSTR(aRetVal[1],5,2))
  aRetVal[2] := FT_ACCTADJ(aRetVal[2])
  aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
 
  IF dGivenDate < aRetVal[2]
    dGivenDate := FT_MADD(dGivenDate, -1)
    aRetVal    := FT_QTR(dGivenDate)
    nQTemp     -= 1
    IF nQTemp  == 0
       nYTemp  -= 1
       nQTemp  := 4
    ENDIF
    aRetVal[2] := FT_ACCTADJ(aRetVal[2])
    aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
 
  ELSEIF dGivenDate > aRetVal[3]
 
    dGivenDate := FT_MADD(dGivenDate,1)
    aRetVal    := FT_QTR(dGivenDate)
    nQTemp     += 1
    IF nQTemp  == 5
       nYTemp  += 1
       nQTemp  := 1
    ENDIF
    aRetVal[2] := FT_ACCTADJ(aRetVal[2])
    aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
 
  ENDIF
 
  lIsQtr     := ( VALTYPE(nQtrNum) == 'N' )
  IF lIsQtr
    IF( nQtrNum < 1 .OR. nQtrNum > 4 , nQtrNum := 4, )
    aRetVal    := FT_QTR(dGivenDate, nQtrNum)
    nYTemp     := VAL(SUBSTR(aRetVal[1],1,4))
    nQTemp     := VAL(SUBSTR(aRetVal[1],5,2))
    aRetVal[2] := FT_ACCTADJ(aRetVal[2])
    aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
  ENDIF
 
  aRetVal[1] := STR(nYTemp,4) + PADL(LTRIM(STR(nQTemp,2)), 2, '0')
 
RETURN aRetVal


