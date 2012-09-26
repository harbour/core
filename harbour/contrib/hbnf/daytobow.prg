/*
 * $Id$
 */

/*
 * File......: daytobow.prg
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

FUNCTION FT_DAYTOBOW( dGivenDate )

   LOCAL nRetVal, nDOW_Start

   nDOW_Start := FT_DATECNFG()[ 2 ]

   IF ValType( dGivenDate ) != 'D'
      dGivenDate := Date()
   ENDIF

   nRetVal := DOW( dGivenDate ) - nDOW_Start
   IF nRetVal < 0
      nRetVal += 7
   ENDIF

   RETURN nRetVal
