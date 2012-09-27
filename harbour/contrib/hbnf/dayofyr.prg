/*
 * $Id$
 */

/*
 * Author....: Jo W. French dba Practical Computing
 * CIS_ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:35:20   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:03:08   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   10 May 1991 23:59:38   GLENN
 * Minor adjustment to header.
 *
 *    Rev 1.0   01 Apr 1991 01:01:02   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_DAYOFYR( dGivenDate, nDayNum, lIsAcct )

   LOCAL lIsDay, nTemp, aRetVal

   IF !( ValType( dGivenDate ) $ "NDL" )
      dGivenDate := Date()
   ELSEIF HB_ISNUMERIC( dGivenDate )
      nDayNum    := dGivenDate
      dGivenDate := Date()
   ELSEIF HB_ISLOGICAL( dGivenDate )
      lIsAcct    := dGivenDate
      dGivenDate := Date()
   ENDIF

   lIsDay  := HB_ISNUMERIC( nDayNum )
   lIsAcct := HB_ISLOGICAL( lIsAcct )

   IF lIsAcct
      aRetVal := FT_ACCTYEAR( dGivenDate )
   ELSE
      aRetVal := FT_YEAR( dGivenDate )
   ENDIF

   IF lIsDay
      nTemp := aRetVal[ 3 ] - aRetVal[ 2 ] + 1
      IF nDayNum < 1 .OR. nDayNum > nTemp
         nDayNum := nTemp
      ENDIF
      aRetVal[ 1 ] := aRetVal[ 2 ] + nDayNum - 1
   ELSE
      aRetVal[ 1 ] += PadL( LTrim( Str( dGivenDate - aRetVal[ 2 ] + 1, 3 ) ), 3, "0" )
   ENDIF

   RETURN aRetVal
