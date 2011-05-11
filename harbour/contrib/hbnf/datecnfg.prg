/*
 * $Id$
 */

/*
 * File......: datecnfg.prg
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74730,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:34:08   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:10   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:26   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:00   GLENN
 * Nanforum Toolkit
 *
 */

#ifdef FT_TEST
  ********************************************************************
  *
  * NOTES: 1) The date functions are 'international'; i.e., the
  *           system date format is maintained, although ANSI is
  *           temporarily used within certain functions.
  *
  *        2) The date functions fall into two categories:
  *
  *           a) Calendar or fiscal periods.
  *              A calendar or fiscal year is identified by the year()
  *              of the last date in the year.
  *
  *           b) Accounting Periods. An accounting period has the
  *              following characteristics:
  *              If the first week of the period contains 4 or
  *              more 'work' days, it is included in the period;
  *              otherwise, the first week was included in the
  *              prior period.
  *
  *              If the last week of the period contains 4 or more
  *              'work' days it is included in the period; otherwise,
  *              the last week is included in the next period.
  *              This results in 13 week 'quarters' and 4 or 5 week
  *              'months'. Every 5 or 6 years, a 'quarter' will contain
  *              14 weeks and the year will contain 53 weeks.
  *
  *        3) The date functions require the presence of two variables:
  *
  *           a) cFY_Start is a character string used to define the
  *              first day of a calendar or fiscal year. It's format
  *              is ANSI; e.g., "1980.01.01" defines a calendar year,
  *              "1980.10.01" defines a fiscal year, starting October 1.
  *
  *              The year may be any valid year. It's value has no
  *              effect on the date functions. The day is assumed to be
  *              less than 29. See function: FT_DATECNFG().
  *
  *           B) nDow_Start is a number from 1 to 7 which defines the
  *              starting day, DOW(), of a work week; e.g., 1 == Sunday.
  *
  *              See function: FT_DATECNFG()
  *
  * COMPILE ALL PROGRAMS WITH /N /W /A
  *
  ********************************************************************

  FUNCTION DEMO()
     LOCAL nNum, dDate, aTestData := {}, aTemp, cFY_Start, nDOW_Start

*    SET DATE American                         // User's normal date format
     aTemp      := FT_DATECNFG()               // Get/Set cFY_Start & nDOW_Start.
*    aTemp      := FT_DATECNFG("03/01/80", 1)  // Date string in user's format.
     cFY_Start  := aTemp[1]                    // See FT_DATECNFG() in ft_date0.prg
     NDOW_START := ATEMP[2]                    // FOR PARAMETERS.
     DDATE      := DATE()
*    dDate      := CTOD("02/29/88")            // Test date, in user's normal date format

     cls
     ?    "Given       Date:  "
     ??   dDate
     ??   " cFY_Start: "+ cFY_Start
     ??   " nDOW_Start:" + STR(nDOW_Start,2)
     ?    "---- Fiscal Year Data -----------"

     aTestData := FT_YEAR(dDate)
     ? "FYYear     ", aTestData[1]+"  ", aTestData[2], aTestData[3]

     aTestData := FT_QTR(dDate)
     ? "FYQtr      ", aTestData[1], aTestData[2], aTestData[3]

     nNum      := VAL(SUBSTR(aTestData[1],5,2))
     aTestData := FT_QTR(dDate,nNum)
     ? "FYQtr    "+STR(nNum,2), aTestData[1], aTestData[2], aTestData[3]

     aTestData := FT_MONTH(dDate)
     ? "FYMonth    ", aTestData[1], aTestData[2], aTestData[3]

     nNum := VAL(SUBSTR(aTestData[1],5,2))
     aTestData := FT_MONTH(dDate,nNum)
     ? "FYMonth  "+STR(nNum,2), aTestData[1], aTestData[2], aTestData[3]

     aTestData := FT_WEEK(dDate)
     ? "FYWeek     ", aTestData[1], aTestData[2], aTestData[3]

     nNum      := VAL(SUBSTR(aTestData[1],5,2))
     aTestData := FT_WEEK(dDate,nNum)
     ? "FYWeek   "+STR(nNum,2), aTestData[1], aTestData[2], aTestData[3]

     aTestData := FT_DAYOFYR(dDate)
     ? "FYDay     ", aTestData[1], aTestData[2], aTestData[3]

     nNum      := VAL(SUBSTR(aTestData[1],5,3))
     aTestData := FT_DAYOFYR(dDate,nNum)
     ? "FYDAY   "+STR(nNum,3), aTestData[1], aTestData[2], aTestData[3]

     ?
     ? "---- Accounting Year Data -------"

     aTestData := FT_ACCTYEAR(dDate)
     ? "ACCTYear   ", aTestData[1]+"  ", aTestData[2], aTestData[3],;
           STR( (aTestData[3] - aTestData[2] + 1) /7, 3 ) + " Weeks"

     aTestData := FT_ACCTQTR(dDate)
     ? "ACCTQtr    ", aTestData[1], aTestData[2], aTestData[3],;
        STR( (aTestData[3] - aTestData[2] + 1) /7, 3 ) + " Weeks"

     nNum      := VAL(SUBSTR(aTestData[1],5,2))
     aTestData := FT_ACCTQTR(dDate,nNum)
     ? "ACCTQtr  "+STR(nNum,2), aTestData[1], aTestData[2], aTestData[3]

     aTestData := FT_ACCTMONTH(dDate)
     ? "ACCTMonth  ", aTestData[1], aTestData[2], aTestData[3],;
        STR( (aTestData[3] - aTestData[2] + 1) /7, 3 ) + " Weeks"

     nNum      := VAL(SUBSTR(aTestData[1],5,2))
     aTestData := FT_ACCTMONTH(dDate,nNum)
     ? "ACCTMonth"+STR(nNum,2), aTestData[1], aTestData[2], aTestData[3]

     aTestData := FT_ACCTWEEK(dDate)
     ? "ACCTWeek   ", aTestData[1], aTestData[2], aTestData[3]

     nNum      := VAL(SUBSTR(aTestData[1],5,2))
     aTestData := FT_ACCTWEEK(dDate,nNum)
     ? "ACCTWeek "+STR(nNum,2), aTestData[1], aTestData[2], aTestData[3]

     aTestData := FT_DAYOFYR(dDate,,.T.)
     ? "ACCTDay   ", aTestData[1], aTestData[2], aTestData[3]

     nNum      := VAL(SUBSTR(aTestData[1],5,3))
     aTestData := FT_DAYOFYR(dDate,nNum,.T.)
     ? "ACCTDay "+STR(nNum,3), aTestData[1], aTestData[2], aTestData[3]

     WAIT

     FT_CAL(dDate)
     FT_CAL(dDate,1)

  RETURN NIL

  * DEMO Monthly Calendar function.
  * nType : 0 -> FT_MONTH, 1 -> FT_ACCTMONTH
  *

  FUNCTION FT_CAL(dGivenDate,nType)
     LOCAL nTemp, dTemp, aTemp, cFY_Start, dStart, dEnd

     aTemp     := FT_DATECNFG()
     cFY_Start := aTemp[1]

     IF dGivenDate == NIL .OR. !VALTYPE(dGivenDate) $ 'ND'
        dGivenDate := DATE()
     ELSEIF VALTYPE(dGivenDate) == 'N'
        nType := dGivenDate
        dGivenDate := DATE()
     ENDIF

     nType := iif(nType == NIL .OR. VALTYPE(nType) != 'N', 0, nType)

     IF nType == 0
        IF SUBSTR(cFY_Start,6,5) == "01.01"
           ? "          Calendar Month Calendar containing " + DTOC(dGivenDate)
        ELSE
           ? "            Fiscal Month Calendar containing " + DTOC(dGivenDate)
        ENDIF

        aTemp    := FT_MONTH(dGivenDate)
        dStart   := aTemp[2]
        dEnd     := aTemp[3]
        aTemp[2] -= FT_DAYTOBOW(aTemp[2])
        aTemp[3] += 6 - FT_DAYTOBOW(aTemp[3])
     ELSE
        ? "            Accounting Month Calendar containing " + DTOC(dGivenDate)
        aTemp := FT_ACCTMONTH(dGivenDate)
     ENDIF

  ?
  dTemp := aTemp[2]

  FOR nTemp := 0 to 6
     ?? PADC( CDOW(dTemp + nTemp),10)
  NEXT

  ?
  WHILE dTemp <= aTemp[3]
     FOR nTemp := 1 TO 7
        ?? " "
        IF nType == 0 .AND. (dTemp < dStart .or. dTemp > dEnd)
           ?? SPACE(8)
        ELSE
           ?? dTemp
        ENDIF
        ?? " "
        dTemp ++
     NEXT
     ?
  END

  RETURN NIL

#endif

FUNCTION FT_DATECNFG( cFYStart ,nDow )

  STATIC aDatePar := { "1980.01.01", 1 }

  LOCAL dCheck, cDateFormat := SET(_SET_DATEFORMAT)

  IF VALTYPE( cFYStart ) == 'C'
     dCheck := CTOD( cFYStart )
     IF DTOC( dCheck ) != " "

        /* No one starts a Fiscal Year on 2/29 */
        IF MONTH(dCheck) == 2 .and. DAY(dcheck) == 29
           dCheck --
        ENDIF

        SET(_SET_DATEFORMAT, "yyyy.mm.dd")
        aDatePar[1] := DTOC(dCheck)
        SET(_SET_DATEFORMAT, cDateFormat)
     ENDIF
  ENDIF

  IF VALTYPE( nDow ) == 'N' .AND. nDow > 0 .AND. nDow < 8
     aDatePar[2] := nDow
  ENDIF

RETURN ACLONE( aDatePar )
