/*
 * File......: MADD.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:39:04   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:03:58   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:38   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_MADD()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Add or subtract months to/from a date
 *  $SYNTAX$
 *     FT_MADD( [ <dGivenDate> ], [ <nAddMonths> ], [ <lMakeEOM> ] )
 *         -> dDate
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format. Defaults to
 *     current system date if not supplied.
 *
 *     <nAddMonths> is the number of months to be added or subtracted.
 *     Defaults to 0 if not supplied.
 *
 *     <lMakeEOM> is a logical variable indicating whether or not to
 *     force the returned date to the last date of the month.  It only
 *     affects the returned date if <dGivenDate> is an end-of-month date.
 *  $RETURNS$
 *     A date.
 *  $DESCRIPTION$
 *     FT_MADD() adds or subtracts months to/from a given date.
 *
 *     If MakeEOM is passed and dGivenDate is the last day of a month,
 *     it will return the EOM of calculated month.  Otherwise it will
 *     return the same day as the day of the passed date.
 *  $EXAMPLES$
 *     dDate := CTOD( "09/15/90" )
 *     ? FT_MADD( dDate, 1 )        // 10/15/90
 *     ? FT_MADD( dDate, -2 )       // 07/15/90
 *
 *     // force EOM
 *     dDate := CTOD( "04/30/91" )
 *     ? FT_MADD( dDate, 1 )        // 05/30/91
 *     ? FT_MADD( dDate, 1, .T. )   // 05/31/91  <- forced EOM
 *     ? FT_MADD( dDate, 2 )        // 06/30/91
 *     ? FT_MADD( dDate, 2, .T. )   // 06/30/91  <- June only has 30 days
 *     ? FT_MADD( dDate, 3 )        // 07/30/91
 *     ? FT_MADD( dDate, 3, .T. )   // 07/31/91  <- forced EOM
 *
 *  $SEEALSO$
 *     FT_DAYOFYR() FT_DAYTOBOW()
 *  $END$
*/

FUNCTION FT_MADD( dGivenDate, nAddMonths, lMakeEOM)
  LOCAL nAdjDay, dTemp, i

  IF(VALTYPE(dGivenDate) != 'D', dGivenDate := DATE(), )
  IF(VALTYPE(nAddMonths) != 'N', nAddMonths := 0, )
  IF(VALTYPE(lMakeEOM)   != 'L', lMakeEom := .F., )

  nAdjDay := DAY( dGivenDate ) - 1

  /* If givendate is end of month and lMakeEom, then force EOM.*/

  lMakeEom := ( lMakeEom .AND. dGivenDate ==  dGivenDate - nAdjDay + 31 - ;
                DAY( dGivenDate - nAdjDay + 31 ) )

  dTemp := dGivenDate - nAdjDay     // first of month

  /* Work with 1st of months.*/
  FOR i := 1 TO ABS(nAddMonths)
      dTemp += IF( nAddMonths > 0, 31, -1 )
      dTemp += 1 - DAY( dTemp )
  NEXT

  IF lMakeEom
     dTemp += 31 - DAY( dTemp + 31 )
  ELSE
     dTemp := MIN( (dTemp + nAdjday), (dTemp += 31 - DAY( dTemp + 31 )))
  ENDIF

RETURN dTemp

