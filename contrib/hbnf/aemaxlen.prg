/*
 * $Id$
 */

/*
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:12   GLENN
 * Initial revision.
 *
 *
 */

FUNCTION ft_AEMaxLen( aArray, nDimension, nStart, nCount )

   LOCAL i, nLast, nMaxlen := 0

   __defaultNIL( @nDimension, 1 )
   __defaultNIL( @nStart, 1 )
   __defaultNIL( @nCount, Len( aArray ) - nStart + 1 )

   nLast := Min( nStart + nCount - 1, Len( aArray ) )

   FOR i := nStart TO nLast

      SWITCH ValType( aArray[ i ] )

      CASE "C"
         nMaxlen := Max( nMaxlen, Len( aArray[ i ] ) )
         EXIT

      CASE "A"
         nMaxlen := Max( nMaxlen, ;
            Len( LTrim( Transform( aArray[ i ][ nDimension ], "@X" ) ) ) )
         EXIT

      OTHERWISE
         nMaxlen := Max( nMaxlen, ;
            Len( LTrim( Transform( aArray[ i ], "@X" ) ) ) )

      ENDSWITCH
   NEXT

   RETURN nMaxlen
