/*
 * $Id$
 */

/*
 * File......: acctqtr.prg
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
    IF nQtrNum < 1 .OR. nQtrNum > 4
      nQtrNum := 4
    ENDIF
    aRetVal    := FT_QTR(dGivenDate, nQtrNum)
    nYTemp     := VAL(SUBSTR(aRetVal[1],1,4))
    nQTemp     := VAL(SUBSTR(aRetVal[1],5,2))
    aRetVal[2] := FT_ACCTADJ(aRetVal[2])
    aRetVal[3] := FT_ACCTADJ(aRetVal[3], .T. )
  ENDIF

  aRetVal[1] := STR(nYTemp,4) + PADL(LTRIM(STR(nQTemp,2)), 2, '0')

RETURN aRetVal
