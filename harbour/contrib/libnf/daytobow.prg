/*
 * File......: DAYTOBOW.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS_ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:36:46   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:03:16   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:28   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:04   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_DAYTOBOW()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Calculate no. of days between date and beginning of week
 *  $SYNTAX$
 *     FT_DAYTOBOW( [ <dGivenDate> ] ) -> nDays
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any valid date format.
 *     Defaults to current date if not supplied.
 *  $RETURNS$
 *     A positive number of days to beginning of week, range 0 to 6.
 *  $DESCRIPTION$
 *     FT_DAYTOBOW() returns the number of days to the beginning of the
 *     week.  Normally this will be one less than the value that
 *     would be returned by the Clipper function DOW(), unless the
 *     day for the beginning of the week has been changed with
 *     FT_DATECNFG().
 *  $EXAMPLES$
 *     dDate := CTOD( "09/15/90" )
 *
 *     ? DOW( dDate )               // 7
 *     ? CDOW( dDate )              // Saturday
 *     ? FT_DAYTOBOW( dDate )       // 6
 *
 *     // change beginning of week to Friday  (yeah!)
 *     FT_DATECNFG( , 6 )
 *     ? DOW( dDate )               // 7
 *     ? CDOW( dDate )              // Saturday
 *     ? FT_DAYTOBOW( dDate )       // 1
 *  $SEEALSO$
 *     FT_DATECNFG() FT_ACCTWEEK() FT_WEEK()
 *  $END$
*/

FUNCTION FT_DAYTOBOW( dGivenDate )

  LOCAL nRetVal, nDOW_Start

  nDOW_Start := FT_DATECNFG()[2]

  IF(VALTYPE(dGivenDate) != 'D', dGivenDate := DATE(), )

  nRetVal := DOW( dGivenDate ) - nDOW_Start
  IF( nRetVal < 0, nRetVal += 7, )

RETURN nRetVal

