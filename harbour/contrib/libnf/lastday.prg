/*
 * File......: LASTDAY.PRG
 * Author....: Mike Schinkel
 * CIS ID....: ?
 *
 * This is an original work by Mike Schinkel and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:02:32   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 04:24:04   GLENN
 * Initial revision.
 *
 *
 */


 /* Librarian's note:  The toolkit's original ft_lday() function was
    submitted by Jeff Bryant.  Mike saw it and optimized it.  Thanks
    to you both for your great code!

  */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_LDAY()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return last day of the month
 *  $SYNTAX$
 *     FT_LDAY( [ <dDateToChk> ] ) -> dLastDay
 *  $ARGUMENTS$
 *     <dDateToChk> is a date within a month for which you want to find
 *     the last date of that month.  If not passed or is an incorrect
 *     type, defaults to current system date.
 *  $RETURNS$
 *     A Clipper date value representing the last date of the month.
 *  $DESCRIPTION$
 *     This function will return the last day of the month of the date
 *     passed, or the last day of the current month if no argument is
 *     supplied.
 *  $EXAMPLES$
 *     dDate := CTOD( "09/15/90" )
 *     ? FT_LDAY( dDate )             // 09/30/90
 *     ? FT_LDAY()                    // 03/31/91  (current month)
 *  $SEEALSO$
 *     FT_FDAY()
 *  $END$
 */

FUNCTION ft_lday( dDate )
   LOCAL d:= dDate
   IF dDate == NIL
      d:= Date()
   ENDIF
   RETURN ( d+= 45 - Day( d ) ) - Day( d )
