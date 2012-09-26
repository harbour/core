/*
 * $Id$
 */

/*
 * File......: sqzn.prg
 * Author....: Joseph D. Booth, Sr.
 * CIS ID....: 72040,2112
 *
 * This is an original work by Joseph D. Booth Sr. and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   13 Jun 1991 15:21:36   GLENN
 * Initial revision.
 *
 */

FUNCTION ft_sqzn( nValue, nSize, nDecimals )

   LOCAL tmpstr, cCompressed, k

   nSize       := iif( nSize    == NIL, 10, nSize )
   nDecimals   := iif( nDecimals == NIL, 0, nDecimals )
   nValue      := nValue * ( 10 ** nDecimals )
   nSize       := iif( nSize / 2 != Int( nSize / 2 ), nSize + 1, nSize )
   tmpstr      := Str( Abs( nValue ), nSize )
   tmpstr      := StrTran( tmpstr, " ", "0" )
   cCompressed := Chr( Val( SubStr( tmpstr, 1, 2 ) ) + iif( nValue < 0, 128, 0 ) )

   FOR k := 3 TO Len( tmpstr ) STEP 2
      cCompressed += Chr( Val( SubStr( tmpstr, k, 2 ) ) )
   NEXT

   RETURN cCompressed

FUNCTION ft_unsqzn( cCompressed, nSize, nDecimals )

   LOCAL tmp := "", k, cValue, multi := 1

   nSize       := iif( nSize     == NIL, 10, nSize )
   nDecimals   := iif( nDecimals == NIL, 0, nDecimals )
   cCompressed := iif( multi     == - 1, SubStr( cCompressed, 2 ), cCompressed )
   nSize       := iif( nSize / 2 != Int( nSize / 2 ), nSize + 1, nSize )
   IF Asc( cCompressed ) > 127
      tmp         := Str( Asc( cCompressed ) - 128, 2 )
      multi       := - 1
   ELSE
      tmp         := Str( Asc( cCompressed ), 2 )
   ENDIF

   FOR k := 2 TO Len( cCompressed )
      tmp += Str( Asc( SubStr( cCompressed, k, 1 ) ), 2 )
   NEXT

   tmp    := StrTran( tmp, " ", "0" )
   cValue := SubStr( tmp, 1, nSize - nDecimals ) + "." + SubStr( tmp, nSize - nDecimals + 1 )

   RETURN Val( cValue ) * multi
