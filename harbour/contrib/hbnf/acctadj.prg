/*
 * $Id$
 */

/*
 * File......: acctadj.prg
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

FUNCTION FT_ACCTADJ( dGivenDate, lIsEnd )

   LOCAL nTemp

   IF ! HB_ISDATE( dGivenDate )
      dGivenDate := Date()
   ENDIF

   lIsEnd := HB_ISLOGICAL( lIsEnd ) // TOFIX: Is this intended?
   nTemp  := FT_DAYTOBOW( dGivenDate )

   IF nTemp > ( 2 + iif( lIsEnd, 0, 1 ) )
      dGivenDate += ( 7 - nTemp )      // Next Week Start (This Week End + 1)
   ELSE
      dGivenDate -= nTemp              // This Week Start (Prior Week End + 1)
   ENDIF

   IF lIsEnd
      dGivenDate--
   ENDIF

   RETURN dGivenDate
