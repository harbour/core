/*
 * File......: QTR.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:41:40   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:04:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:44   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:04   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_QTR()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return Calendar or Fiscal Quarter Data.
 *  $SYNTAX$
 *     FT_QTR( [ <dGivenDate> ], [ <nQtrNum> ] ) -> aDateInfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *
 *     <nQtrNum> is a number from 1 to 4 signifying a quarter.
 *     Defaults to current quarter if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year and quarter as a character string "YYYYQQ"
 *        aDateInfo[2] - The beginning date of the quarter
 *        aDateInfo[3] - The ending date of the quarter
 *  $DESCRIPTION$
 *     FT_QTR() returns an array containing data about the quarter
 *     containing the given date.
 *
 *     Normally the return data will be based on a year beginning
 *     on January 1st with weeks beginning on Sunday.
 *
 *     The beginning of year date and/or beginning of week day can be
 *     changed by using FT_DATECNFG(), which will affect all subsequent
 *     calls to FT_QTR() until another call to FT_DATECNFG().
 *
 *     The beginning of year date and beginning of week day may be reset
 *     to January 1 and Sunday by calling FT_DATECNFG() with no
 *     parameters.
 *  $EXAMPLES$
 *     // get info about quarter containing 9/15/90
 *     aDateInfo := FT_QTR( CTOD("09/15/90") )
 *     ? aDateInfo[1]   //  199003       (3rd quarter)
 *     ? aDateInfo[2]   //  07/01/90     beginning of quarter 3
 *     ? aDateInfo[3]   //  09/30/90     end of week quarter 3
 *
 *     // get info about quarter 2 in year containing 9/15/90
 *     aDateInfo := FT_QTR( CTOD("09/15/90"), 2 )
 *     ? aDateInfo[1]   //  199002
 *     ? aDateInfo[2]   //  04/01/90   beginning of quarter 2
 *     ? aDateInfo[3]   //  06/30/90   end of quarter 2
 *
 *     // get info about quarter 2 in current year (1991)
 *     aDateInfo := FT_QTR( , 2 )
 *     ? aDateInfo[1]   //  199102
 *     ? aDateInfo[2]   //  04/01/91   beginning of quarter 2
 *     ? aDateInfo[3]   //  06/30/91   end of quarter 2
 *  $SEEALSO$
 *     FT_DATECNFG() FT_WEEK() FT_MONTH() FT_YEAR()
 *  $END$
*/

FUNCTION FT_QTR(dGivenDate,nQtrNum)
LOCAL lIsQtr, nTemp, aRetVal

  IF !(VALTYPE(dGivenDate) $ 'ND')
     dGivenDate := DATE()
  ELSEIF VALTYPE(dGivenDate) == 'N'
     nQtrNum    := dGivenDate
     dGivenDate := DATE()
  ENDIF

  aRetval := FT_YEAR(dGivenDate)

  lIsQtr  := ( VALTYPE(nQtrNum) == 'N' )
  IF lIsQtr
     IF( nQtrNum < 1 .OR. nQtrNum > 4, nQtrNum := 4, )
     dGivenDate := FT_MADD(aRetVal[2], 3*(nQtrNum - 1) )
  ENDIF

  nTemp := MONTH( dGivenDate ) - MONTH( aRetVal[2] )
  nTemp += IF( nTemp >= 0, 1, 13 )
  nTemp := INT( (nTemp - 1) / 3 )

  aRetVal[1] += PADL(LTRIM(STR( nTemp + 1, 2)), 2, '0')
  aRetVal[2] := FT_MADD( aRetVal[2], nTemp * 3 )
  aRetVal[3] := FT_MADD( aRetVal[2], 3 ) - 1

RETURN aRetVal
