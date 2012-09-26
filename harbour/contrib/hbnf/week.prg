/*
 * $Id$
 */

/*
 * File......: week.prg
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:44:52   GLENN
 * Jo French cleaned up and correct to bow().
 *
 *    Rev 1.2   15 Aug 1991 23:05:26   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:16   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:30   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_WEEK( dGivenDate, nWeekNum )

   LOCAL lIsWeek, nTemp, aRetVal, dTemp

   IF ! ( ValType( dGivenDate ) $ 'ND' )
      dGivenDate := Date()
   ELSEIF ValType( dGivenDate ) == 'N'
      nWeekNum   := dGivenDate
      dGivenDate := Date()
   ENDIF

   aRetVal      := FT_YEAR( dGivenDate )
   dTemp        := aRetVal[ 2 ]
   aRetVal[ 2 ] -= FT_DAYTOBOW( aRetVal[ 2 ] )

   lIsWeek := ( ValType( nWeekNum ) == 'N' )
   IF lIsWeek
      nTemp := Int( ( aRetVal[ 3 ] - aRetVal[ 2 ] ) / 7 ) + 1
      IF nWeekNum < 1 .OR. nWeekNum > nTemp
         nWeekNum := nTemp
      ENDIF
      dGivenDate := aRetVal[ 2 ] + ( nWeekNum - 1 ) * 7
   ENDIF

   dGivenDate += ( 6 - FT_DAYTOBOW( dGivenDate ) )       // end of week

   aRetVal[ 1 ] += PadL( LTrim( Str( Int( ( dGivenDate - ;
      aRetVal[ 2 ] ) / 7 ) + 1, 2 ) ), 2, '0' )
   aRetVal[ 2 ] := Max( dGivenDate - 6, dTemp )
   aRetVal[ 3 ] := Min( dGivenDate, aRetVal[ 3 ] )

   RETURN aRetVal
