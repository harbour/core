/*
 * $Id$
 */

/*
 * File......: woy.prg
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

     IF Left( DTOC( CTOD( cDate) ), 1 ) == " "
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
