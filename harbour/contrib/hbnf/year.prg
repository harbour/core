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

FUNCTION FT_YEAR( dGivenDate )

   LOCAL aRetVal[ 3 ], cFY_Start, cDateFormat

   cFY_Start   := FT_DATECNFG()[ 1 ]
   cDateFormat := Set( _SET_DATEFORMAT, "yyyy.mm.dd" )

   IF ! HB_ISDATE( dGivenDate )
      dGivenDate := Date()
   ENDIF

   aRetVal[ 2 ] := CToD( Str( Year( dGivenDate ) - iif( Month( dGivenDate ) < ;
      Month( CToD( cFY_Start ) ), 1, 0 ), 4 ) + ;
      SubStr( cFY_Start, 5, 6 ) )
   aRetval[ 3 ] := FT_MADD( aRetVal[ 2 ], 12 ) - 1
   aRetVal[ 1 ] := Str( Year( aRetVal[ 3 ] ), 4 )      // End of Year

   Set( _SET_DATEFORMAT, cDateFormat )

   RETURN aRetVal
