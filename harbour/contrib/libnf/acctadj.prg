/*
 * File......: ACCTADJ.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   28 Sep 1992 00:22:38   GLENN
 * Jo French clean up.
 *
 *    Rev 1.3   15 Aug 1991 23:04:58   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:50:40   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   11 May 1991 00:34:00   GLENN
 * Documentation rewrite.  Enter DOC header was rewritten and resubmitted
 * by the author.  No code changes.
 *
 *    Rev 1.0   01 Apr 1991 01:00:22   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ACCTADJ()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Adjust beginning or ending fiscal pd. dates to acctg. dates
 *  $SYNTAX$
 *     FT_ACCTADJ( [ <dGivenDate> ], [ <lIsEnd> ] ) -> dDate
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any valid format.
 *     Defaults to DATE() if not supplied.
 *
 *     <lIsEnd> is a logical variable. .F. = adjust for beginning of
 *     period mode, .T. = adjust for end of period mode.  Defaults to
 *     beginning of period mode.
 *  $RETURNS$
 *     An adjusted date dependent upon mode and work week start day.
 *  $DESCRIPTION$
 *     Called by other FT_ACCT.. functions. The algorithm is:
 *
 *     Beginning of period mode:
 *
 *        If dGivenDate is in last 3 days of work week
 *           Return next week's start date
 *        Else
 *           Return this week's start date
 *        Endif
 *
 *     End of period mode:
 *
 *        If dGivenDate is in last 4 days of work week
 *           Return this week's end date
 *        Else
 *           Return prior week's end date
 *        Endif
 *  $EXAMPLES$
 *     Beginning of period mode (lIsEnd == .F.)
 *
 *       dDate := Ctod( "01/31/91" )  // In last 3 days of work week
 *       ? FT_ACCTADJ( dDate )        // 02/03/91 (next week's start)
 *
 *       dDate := Ctod( "03/31/91" )  // Not in last 3 days of work week
 *       ? FT_ACCTADJ( dDate )        // 03/31/91 (this week's start)
 *
 *     End of period mode (lIsEnd == .T.)
 *
 *       dDate := Ctod( "01/31/91" )  // In last 4 days of work week
 *       ? FT_ACCTADJ( dDate, .T. )   // 02/02/91 (this week's end)
 *
 *       dDate := Ctod( "03/31/91" )  // Not in last 4 days of work week
 *       ? FT_ACCTADJ( dDate, .T. )   // 03/30/91 (prior week's end)
 *  $SEEALSO$
 *     FT_DATECNFG() FT_DAYTOBOW()
 *  $END$
*/
 
FUNCTION FT_ACCTADJ(dGivenDate, lIsEnd)
 
  LOCAL nTemp
 
  IF( VALTYPE(dGivenDate) != 'D', dGivenDate := DATE(), )
  lIsEnd     := ( VALTYPE(lIsEnd) == 'L' )
  nTemp      := FT_DAYTOBOW(dGivenDate)
 
  IF nTemp > ( 2 + IF(!lIsEnd, 1, 0) )
     dGivenDate += ( 7 - nTemp )      // Next Week Start (This Week End + 1)
  ELSE
     dGivenDate -= nTemp              // This Week Start (Prior Week End + 1)
  ENDIF
 
  IF( lIsEnd,  dGivenDate -= 1, )
 
RETURN dGivenDate


