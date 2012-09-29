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

FUNCTION FT_AEmaxlen( aArray, nDimension, nStart, nCount )

   LOCAL i, nLast, cType, nMaxlen := 0

   // Set default parameters as necessary.
   IF nDimension == NIL
      nDimension := 1
   ENDIF

   IF nStart == NIL
      nStart := 1
   ENDIF

   IF nCount == NIL
      nCount := Len( aArray ) - nStart + 1
   ENDIF

   nLast := Min( nStart + nCount - 1, Len( aArray ) )

   FOR i := nStart TO nLast
      cType := ValType( aArray[ i ] )
      DO CASE
      CASE cType == "C"
         nMaxlen := Max( nMaxlen, Len( aArray[ i ] ) )

      CASE cType == "A"
         nMaxlen := Max( nMaxlen, ;
            Len( LTrim( Transform( aArray[ i ][ nDimension ], "@X" ) ) ) )

      OTHERWISE
         nMaxlen := Max( nMaxlen, ;
            Len( LTrim( Transform( aArray[ i ], "@X" ) ) ) )
      ENDCASE
   NEXT

   RETURN nMaxlen
