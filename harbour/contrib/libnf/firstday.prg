/*
 * File......: FIRSTDAY.PRG
 * Author....: Jeff Bryant
 * CIS ID....: ?
 *
 * This function is an original work by Jeff Bryant and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:24   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FDAY()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return first day of the month
 *  $SYNTAX$
 *     FT_FDAY( [ <dDateToChk> ] ) -> dFirstDay
 *  $ARGUMENTS$
 *     <dDateToChk> is a date within a month for which you want to find
 *     the first date of that month.  If not passed or is an incorrect type,
 *     defaults to current system date.
 *  $RETURNS$
 *     A Clipper date value representing the first date of the month.
 *  $DESCRIPTION$
 *     This function will return the first day of the month of the date
 *     passed, or the first day of the current month if no argument is
 *     supplied.
 *  $EXAMPLES$
 *     dDate := CTOD( "09/15/90" )
 *     ? FT_FDAY( dDate )             // 09/01/90
 *     ? FT_FDAY()                    // 03/01/91  (current month)
 *  $SEEALSO$
 *     FT_LDAY()
 *  $END$
 */

FUNCTION FT_FDAY(dDateToChk)

   IF Valtype(dDatetoChk) # "D"
      dDatetoChk := Date()
   ENDIF

   RETURN dDateToChk - (DAY(dDateToChk)-1)

