/*
 * File......: YEAR.PRG
 * Author....: Jo W. French dba Practical Computing
 * CIS_ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:45:50   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:04:56   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:20   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:36   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_YEAR()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Return calendar or fiscal year data
 *  $SYNTAX$
 *     FT_YEAR( [ <dGivenDate> ] ) -> aDateInfo
 *  $ARGUMENTS$
 *     <dGivenDate> is any valid date in any date format.  Defaults
 *     to current system date if not supplied.
 *  $RETURNS$
 *     A three element array containing the following data:
 *
 *        aDateInfo[1] - The year as a character string "YYYY"
 *        aDateInfo[2] - The beginning date of the year
 *        aDateInfo[3] - The ending date of the year
 *  $DESCRIPTION$
 *     FT_YEAR() returns an array containing data about the year
 *     containing the given date.
 *
 *     Normally the return data will be based on a year beginning
 *     on January 1st.
 *
 *     The beginning of year date can be changed by using FT_DATECNFG(),
 *     which will affect all subsequent calls to FT_YEAR() until another
 *     call to FT_DATECNFG().
 *
 *     The beginning of year date may be reset to January 1 by calling
 *     FT_DATECNFG() with no parameters.
 *  $EXAMPLES$
 *     // Get info about year containing 9/15/90, assuming default
 *     // beginning of year is January 1st.
 *     aDateInfo := FT_YEAR( Ctod("09/15/90") )
 *     ? aDateInfo[1]   //  1990
 *     ? aDateInfo[2]   //  01/01/90     beginning of year
 *     ? aDateInfo[3]   //  12/31/90     end of year
 *
 *     // get info about current year (1991).
 *     aDateInfo := FT_YEAR()
 *     ? aDateInfo[1]   //  1991
 *     ? aDateInfo[2]   //  01/01/91   beginning of year
 *     ? aDateInfo[3]   //  12/31/91   end of year
 *  $SEEALSO$
 *     FT_DATECNFG() FT_WEEK() FT_MONTH() FT_QTR()
 *  $END$
*/

FUNCTION FT_YEAR(dGivenDate)

  LOCAL aRetVal[3], cFY_Start, cDateFormat

  cFY_Start   := FT_DATECNFG()[1]
  cDateFormat := SET(_SET_DATEFORMAT, "yyyy.mm.dd")
  IF( VALTYPE(dGivenDate) != 'D', dGivenDate := DATE(), )

  aRetVal[2]  := CTOD(STR( YEAR(dGivenDate) - IF(MONTH(dGivenDate) < ;
                    MONTH(CTOD(cFY_Start)), 1, 0), 4) + ;
                    SUBSTR(cFY_Start, 5, 6) )
  aRetval[3]  := FT_MADD(aRetVal[2], 12) - 1
  aRetVal[1]  := STR(YEAR(aRetVal[3]),4)      // End of Year

  SET(_SET_DATEFORMAT, cDateFormat)

RETURN aRetVal

