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

PROCEDURE Main( cCent )

   LOCAL  lCentOn := .F. , cDate
   MEMVAR getlist

   IF ValType( cCent ) == "C" .AND. "CENT" $ Upper( cCent )
      SET CENTURY ON
      lCentOn := .T.
   ENDIF

   DO WHILE .T.
      CLEAR
      @ 2, 10 SAY "Date to Test"

      IF lCentOn
         cDate := Space( 10 )
         @ 2, 24 GET cDate PICTURE "##/##/####"
      ELSE
         cDate := Space( 8 )
         @ 2, 24 GET cDate PICTURE "##/##/##"
      ENDIF
      READ

      IF Empty( cDate )
         EXIT
      ENDIF

      IF Left( DToC( CToD( cDate ) ), 1 ) == " "
         QQOut( Chr( 7 ) )
         @ 4, 24 SAY "INVALID DATE"
         Inkey( 2 )
         LOOP
      ENDIF

      @ 4, 10 SAY "Is Day Number " + Str( FT_DOY( CToD( cDate ) ), 3 )

      @ 6, 10 SAY "Is in Week Number " + Str( FT_WOY( CToD( cDate ) ), 2 )
      @ 7, 0
      WAIT
   ENDDO

   CLEAR

   RETURN

#endif

FUNCTION FT_WOY( dInDate )

   LOCAL nFirstDays, nDayOffset, nWkNumber, cCentury

   IF !( ValType( dInDate ) == "D" )
      nWkNumber := NIL
   ELSE

      // resolve century issue
      IF Len( DToC( dInDate ) ) > 8                  // CENTURY is on
         cCentury := SubStr( DToC( dInDate ), 7, 4 )
      ELSE
         cCentury := SubStr( DToC( dInDate ), 7, 2 )
      ENDIF

      // find number of days in first week of year

      nFirstDays := 8 - ( DOW( CToD( "01/01/" + cCentury ) ) )

      nWkNumber  := 1

      // find how many days after first week till dInDate

      nDayOffset := ( dInDate - ;
         CToD( "01/01/" + cCentury ) ) - nFirstDays + 1

      // count weeks in offset period

      DO WHILE nDayOffset > 0
         ++nWkNumber
         nDayOffset -= 7
      ENDDO

   ENDIF

   RETURN nWkNumber

FUNCTION FT_DOY( dInDate )

   LOCAL nDayNum, cCentury

   IF !( ValType( dInDate ) == "D" )
      nDayNum := NIL
   ELSE

      // resolve century issue
      IF Len( DToC( dInDate ) ) > 8                  // CENTURY is on
         cCentury := SubStr( DToC( dInDate ), 7, 4 )
      ELSE
         cCentury := SubStr( DToC( dInDate ), 7, 2 )
      ENDIF

      // calculate
      nDayNum := ( dInDate - CToD( "01/01/" + cCentury ) ) + 1

   ENDIF

   RETURN nDayNum
