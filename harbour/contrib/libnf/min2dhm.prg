/*
 * File......: MIN2DHM.PRG
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:33:50   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:04:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:26   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:39:50   GLENN
 * Initial revision.
 *
 */



/*  $DOC$
 *  $FUNCNAME$
 *     FT_MIN2DHM()
 *  $CATEGORY$
 *     Date/Time
 *  $ONELINER$
 *     Convert numeric minutes to days, hours and minutes.
 *  $SYNTAX$
 *     FT_MIN2DHM( <nMinutes> ) -> aDHM_
 *  $ARGUMENTS$
 *     <nMinutes>  the number of minutes.
 *  $RETURNS$
 *     <aDHM_>
 *        where:
 *           aDHM_[1] = cDAYS, aDHM_[2] = cHours, aDHM_[3] = cMinutes
 *  $DESCRIPTION$
 *     Converts numeric minutes into a character array containing
 *     days, hours & minutes.
 *  $EXAMPLES$
 *     aDHM_ = MIN2DHM(16789) -> aDHM_[1] = 11, aDHM_[2] = 15, aDHM_[3] = 49
 *  $END$
 */

function FT_MIN2DHM(nMINS)
  local aDHM_[3]

  aDHM_[1] = ltrim((str(int(nMINS/1440))))
  aDHM_[2] = ltrim(str(int((nMINS%1440)/60)))
  aDHM_[3] = ltrim(str(int((nMINS%1440)%60)))

  return aDHM_                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
