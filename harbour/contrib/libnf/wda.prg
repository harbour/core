/*
 * File......: WDA.PRG
 * Author....: Eric Splaver
 * CIS ID....: ?
 *
 * This is an original work by Eric Splaver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:34   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 04:25:46   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ADDWKDY()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return true number of days to add given number of workdays
 *  $SYNTAX$
 *     FT_ADDWKDY( <dStart>, <nWorkDays> ) -> nTrueDays
 *  $ARGUMENTS$
 *     <dStart> = date to start adding from
 *     <nWorkDays> = number of workdays to add
 *  $RETURNS$
 *     <nTrueDays> = Number of actual days to add to <dStart> in
 *                   order to add the required <nWorkDays>
 *  $DESCRIPTION$
 *      Let's say you are given the problem:
 *
 *         "All invoices are due 10 working days from the date they
 *         are printed.  Please display the due date on the invoice."
 *
 *      When is the due date?  Assuming you are printing the invoices
 *      today, your answer is:
 *
 *           dDueDate := DATE() + ft_addWkDay( DATE(), 10 )
 *
 *      A work day is defined as Monday through Friday.  Unfortunately
 *      this routine does _not_ account for holidays.
 *
 *      This documentation was written by Glenn Scott so if it's wrong,
 *      blame him.
 *
 *  $EXAMPLES$
 *      // Postdate 5 working days from the first of January
 *      dPost := CTOD("01/01/91")
 *      dPost += FT_ADDWKDY( dPost, 5 )   // returns 7 true days
 *      ? dPost                          //  01/08/91
 *
 *  $SEEALSO$
 *      FT_WORKDAYS()
 *  $END$
 */

#ifdef FT_TEST
  function main( cDate, cDays )
     local nDays := ft_addWkDy( ctod(cDate), val(cDays) )
     qout( "Num days to add: " + str( nDays ) )
     qout( "New date:        " + dtoc( ctod( cDate ) + nDays ) )
     return nil
#endif


FUNCTION ft_addWkDy( dStart, nDys )
    LOCAL nDc  := dow( dStart )
    RETURN ( iif( nDc == 7,                                                        ;
            (nDys-1)      % 5 + 7 * int( (nDys-1)      / 5 ) + 2,         ;
            (nDys+nDc-2)  % 5 + 7 * int( (nDys+nDc-2)  / 5 ) + 2  - nDc   ;
                )                                                                   ;
            )


