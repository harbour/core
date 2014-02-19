/*
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:29:14   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:02:40   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:48   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:28   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_AcctYear( dGivenDate )

   LOCAL nYTemp, aRetVal

   IF ! HB_ISDATE( dGivenDate )
      dGivenDate := Date()
   ENDIF

   aRetVal      := ft_Year( dGivenDate )
   nYTemp       := Val( aRetVal[ 1 ] )
   aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
   aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )

   DO CASE
   CASE dGivenDate < aRetVal[ 2 ]

      aRetVal := ft_Year( ft_MAdd( dGivenDate, -1 ) )
      nYTemp--
      aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
      aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )

   CASE dGivenDate > aRetVal[ 3 ]

      aRetVal := ft_Year( ft_MAdd( dGivenDate, 1 ) )
      nYTemp++
      aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
      aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )

   ENDCASE

   aRetVal[ 1 ] := StrZero( nYTemp, 4 )

   RETURN aRetVal
