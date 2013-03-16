/*
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

FUNCTION ft_Sqzn( nValue, nSize, nDecimals )

   LOCAL tmpstr, cCompressed, k

   __defaultNIL( @nSize, 10 )
   __defaultNIL( @nDecimals, 0 )

   nValue      := nValue * ( 10 ^ nDecimals )
   nSize       := iif( ( nSize / 2 ) != Int( nSize / 2 ), nSize + 1, nSize )
   tmpstr      := Str( Abs( nValue ), nSize )
   tmpstr      := StrTran( tmpstr, " ", "0" )
   cCompressed := hb_BChar( Val( hb_BSubStr( tmpstr, 1, 2 ) ) + iif( nValue < 0, 128, 0 ) )

   FOR k := 3 TO hb_BLen( tmpstr ) STEP 2
      cCompressed += hb_BChar( Val( hb_BSubStr( tmpstr, k, 2 ) ) )
   NEXT

   RETURN cCompressed

FUNCTION ft_Unsqzn( cCompressed, nSize, nDecimals )

   LOCAL tmp := "", k, cValue, multi

   __defaultNIL( @nSize, 10 )
   __defaultNIL( @nDecimals, 0 )

   nSize := iif( ( nSize / 2 ) != Int( nSize / 2 ), nSize + 1, nSize )
   IF hb_BCode( cCompressed ) > 127
      tmp   := Str( hb_BCode( cCompressed ) - 128, 2 )
      multi := -1
   ELSE
      tmp   := Str( hb_BCode( cCompressed ), 2 )
      multi := 1
   ENDIF

   FOR k := 2 TO hb_BLen( cCompressed )
      tmp += Str( hb_BCode( hb_BSubStr( cCompressed, k, 1 ) ), 2 )
   NEXT

   tmp    := StrTran( tmp, " ", "0" )
   cValue := hb_BSubStr( tmp, 1, nSize - nDecimals ) + "." + hb_BSubStr( tmp, nSize - nDecimals + 1 )

   RETURN Val( cValue ) * multi
