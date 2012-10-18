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
 *    Rev 1.1   15 Aug 1991 23:02:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:16   GLENN
 * Initial revision.
 *
 *
 */

FUNCTION FT_AEminlen( aArray, nDimension, nStart, nCount )

   LOCAL i, nLast, nMinlen := NIL, nLen

   __defaultNIL( @nDimension, 1 )
   __defaultNIL( @nStart, 1 )
   __defaultNIL( @nCount, Len( aArray ) - nStart + 1 )

   nLast := Min( nStart + nCount - 1, Len( aArray ) )

   FOR i := nStart TO nLast

      SWITCH ValType( aArray[ i ] )
      CASE "C"
         nLen := Len( aArray[ i ] )
         EXIT
      CASE "A"
         nLen := Len( LTrim( Transform( aArray[ i ][ nDimension ], "@X" ) ) )
         EXIT
      OTHERWISE
         nLen := Len( LTrim( Transform( aArray[ i ], "@X" ) ) )
      ENDSWITCH

      nMinlen := iif( nMinLen == NIL, nLen, Min( nMinlen, nLen ) )
   NEXT

   IF nMinLen == NIL
      nMinLen := 65519 /* for compatibility */
   ENDIF

   RETURN nMinlen
