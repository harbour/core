/*
 * File......: ELAPMIL.PRG
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:32   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:44   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:39:42   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ELAPMIN()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *  Return difference, in minutes, between two mil format times.
 *  $SYNTAX$
 *     FT_ELAPMIN( <cTIME1>, <cTIME2> ) -> nMINUTES
 *  $ARGUMENTS$
 *     <cTIME1, cTIME2>  character strings of military form "hhmm",
 *         where 0<=hh<24.
 *  $RETURNS$
 *     <nMINUTES>
 *  $DESCRIPTION$
 *     Finds the arithmetic difference between time two times
 *     (time 2 - time 1).
 *     If time 2 is smaller than time 1, a NEGATIVE value is returned.
 *  $EXAMPLES$
 *     FT_ELAPMIN( "1718", "2040" ) ->  322
 *     FT_ELAPMIN( "2040", "1718" ) -> -322
 *  $SEEALSO$
 *     FT_ELTIME() FT_MIL2MIN() FT_MIN2MIL()
 *  $END$
 */

function FT_ELAPMIN(cTIME1,cTIME2)
  return ((VAL(LEFT(cTIME2,2))*60) + (VAL(RIGHT(cTIME2,2)))) - ;
         ((VAL(LEFT(cTIME1,2))*60) + (VAL(RIGHT(cTIME1,2))))
