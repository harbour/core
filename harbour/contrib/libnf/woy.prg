/*
 * File......: WOY.PRG
 * Author....: Forest Belt, Computer Diagnostic Services, Inc.
 * CIS ID....: ?
 *
 * This is an original work by Forest Belt and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   11 May 1991 00:01:00   GLENN
 * Documentation correction on ft_doy() oneliner.  Was identical to ft_woy(),
 * now it's right.
 *
 *    Rev 1.0   01 Apr 1991 01:02:32   GLENN
 * Nanforum Toolkit
 *
 */


#ifdef FT_TEST

  // ADD PARAMETER "CENTURY" ON COMMAND LINES TO TEST 4-DIGIT YEARS

  FUNCTION MAIN( cCent )
     LOCAL  lCentOn := .F., cDate
     MEMVAR getlist

     IF VALTYPE( cCent) == "C" .AND. "CENT" $ UPPER( cCent)
     SET CENTURY ON
     lCentOn := .T.
     END

     DO WHILE .T.
     CLEAR
     @ 2,10 SAY "Date to Test"

     IF lCentOn
        cDate := SPACE(10)
        @ 2,24 GET cDate PICTURE "##/##/####"
     ELSE
        cDate := SPACE(8)
        @ 2,24 GET cDate PICTURE "##/##/##"
     END
     READ

     IF EMPTY(cDate)
        EXIT
     END

     IF DTOC( CTOD( cDate) ) = " "
        QQOUT( CHR( 7) )
        @ 4,24 SAY "INVALID DATE"
        INKEY(2)
        LOOP
     END

     @ 4,10 SAY "Is Day Number " + STR( FT_DOY( CTOD( cDate)) ,3)

     @ 6,10 SAY "Is in Week Number " + STR( FT_WOY( CTOD( cDate)) ,2)
     @ 7,0
     WAIT
     END

     CLEAR
  RETURN nil

#endif

/* $DOC$
 *  $FUNCNAME$
 *     FT_WOY()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Find number of week within year
 *  $SYNTAX$
 *     FT_WOY( <dDate> ) -> <nResult>
 *  $ARGUMENTS$
 *     <dDate> is a date in the form "mm/dd/yy" or "mm/dd/yyyy"
 *  $RETURNS$
 *     Return numeric position of week within the year or NIL if
 *     parameter does not conform.
 *  $DESCRIPTION$
 *     Considers a full week as starting on Sunday, ending on Saturday.
 *     First week of year (week 1) may start on any day, and thus
 *       contain any number of days.
 *     Final week of year (week 53) may contain any number of days.
 *     Handles dates with CENTURY ON|OFF, to allow for 21st century.
 *     Date validation must be external to this function.
 *  $EXAMPLES$
 *     These code fragments find the week number, given a date.
 *
 *     // literal character date
 *     dDate  := CTOD("01/01/91")
 *     nWkNum := FT_WOY(dDate)              // result:  1
 *
 *     // presume DOS date to be 01/06/91
 *     nWkNum := FT_WOY(DATE())             // result:  2
 *
 *     // date input
 *     cDate  := SPACE(8)
 *     @ 4,10 get cDate PICT "##/##/##"     // input 07/04/91
 *     READ
 *     nWkNum := FT_WOY(CTOD(cDate))        // result: 27
 *
 *     // last day of year
 *     nWkNum := FT_WOY(CTOD("12/31/91"))    // result: 53
 *
 *     For a demonstration of this function, compile and link the
 *     program WOY.PRG in the Nanforum Toolkit source code.
 *  $END$
 */

FUNCTION FT_WOY(dInDate)

  LOCAL nFirstDays, nDayOffset, nWkNumber, cCentury

  IF VALTYPE( dInDate) != "D"
     nWkNumber := NIL

  ELSE

     // resolve century issue
     IF LEN( DTOC( dInDate) ) > 8                  // CENTURY is on
     cCentury := SUBSTR( DTOC( dInDate) ,7 ,4)
     ELSE
     cCentury := SUBSTR( DTOC( dInDate) ,7 ,2)
     END


     // find number of days in first week of year

     nFirstDays := 8 - (DOW (CTOD ("01/01/" + cCentury) ) )

     nWkNumber  := 1


     // find how many days after first week till dInDate

     nDayOffset := (dInDate - ;
                 CTOD ("01/01/" + cCentury) ) - nFirstDays + 1


     // count weeks in offset period

     DO WHILE nDayOffset > 0
     ++nWkNumber
     nDayOffset -= 7
     END

  END

RETURN (nWkNumber)


/* $DOC$
 *  $FUNCNAME$
 *     FT_DOY()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Find number of day within year
 *  $SYNTAX$
 *     FT_DOY( <dDate> ) -> <nResult>
 *  $ARGUMENTS$
 *     <dDate> is a date in the form "mm/dd/yy" or "mm/dd/yyyy"
 *  $RETURNS$
 *     Return numeric position of day within the year.
 *     Return NIL if parameter does not conform.
 *  $DESCRIPTION$
 *     Finds the day number, considering 01/01 as day 1
 *     Handles dates with CENTURY ON|OFF, to allow for 21st century.
 *     Date validation must be external to this function.
 *  $EXAMPLES$
 *     These code fragments find the day number, given a date.
 *
 *     // literal character date
 *     dDate  := CTOD("01/01/91")
 *     nDayNum := FT_DOY(dDate)              // result:  1
 *
 *     // presume DOS date to be 01/06/91
 *     nDayNum := FT_DOY(DATE())             // result:  6
 *
 *     // date input
 *     cDate  := SPACE(8)
 *     @ 4,10 get cDate PICT "##/##/##"      // input 07/04/91
 *     READ
 *     nDayNum := FT_DOY(CTOD(cDate))        // result: 185
 *
 *     // last day of year
 *     nDayNum := FT_DOY(CTOD("12/31/91"))    // result: 365
 *
 *     For a demonstration of this function, compile and link the
 *     program WOY.PRG in the Nanforum Toolkit source code.
 *  $END$
 */

FUNCTION FT_DOY(dInDate)

  LOCAL nDayNum, cCentury

  IF VALTYPE(dInDate) != "D"
     nDayNum := NIL
  ELSE

     // resolve century issue
     IF LEN( DTOC( dInDate) ) > 8                  // CENTURY is on
     cCentury := SUBSTR( DTOC( dInDate) ,7 ,4)
     ELSE
     cCentury := SUBSTR( DTOC( dInDate) ,7 ,2)
     END

     // calculate
     nDayNum := (dInDate - CTOD ("01/01/" + cCentury)) + 1

  END

RETURN (nDayNum)
