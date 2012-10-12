/*
 * $Id$
 */

/*
 * Author....: Gary Baren
 * CIS ID....: 75470,1027
 *
 * This is an original work by Gary Baren and is hereby placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:54   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   09 Jun 1991 00:26:56   GLENN
 * Initial revision.
 *
 */

STATIC sc_ones  := { "",     " One",   " Two",   " Three", " Four", " Five", ;
      " Six", " Seven", " Eight", " Nine" ;
      }

STATIC sc_teens := { " Ten",      " Eleven",    " Twelve", ;
      " Thirteen", " Fourteen",  " Fifteen", ;
      " Sixteen",  " Seventeen", " Eighteen", ;
      " Nineteen" ;
      }

STATIC sc_tens  :=  { "", "", " Twenty", " Thirty", " Forty", " Fifty", ;
      " Sixty", " Seventy", " Eighty", " Ninety"  }

STATIC sc_qualifiers := { "", " Thousand", " Million", " Billion", " Trillion" }

FUNCTION ft_ntow( nAmount )

   LOCAL nTemp, sResult := " ", nQualNo
   LOCAL nDiv := 10 ^ ( Int( sol10( nAmount ) / 3 ) * 3 )

   nTemp   := Int( nAmount % nDiv )
   nAmount := Int( nAmount / nDiv )
   nQualNo := Int( sol10( nDiv ) / 3 ) + 1
   sResult += grp_to_words( nAmount, sc_qualifiers[ nQualNo ] )

   IF nTemp > ( nDiv /= 1000 ) .AND. ( nDiv > 1 )
      sResult += ft_ntow( nTemp, nDiv )
   ELSE
      sResult += grp_to_words( nTemp, "" )
   ENDIF

   RETURN LTrim( sResult )

STATIC FUNCTION grp_to_words( nGrp, sQual )

   LOCAL sResult := "", nTemp

   nTemp   := Int( nGrp % 100 )
   nGrp    := Int( nGrp / 100 )
   sResult += sc_ones[ nGrp + 1 ] + iif( nGrp > 0, " Hundred", "" )

   DO CASE
   CASE nTemp > 19
      sResult += sc_tens[ int( nTemp / 10 ) + 1 ]
      sResult += sc_ones[ int( nTemp % 10 ) + 1 ]
   CASE nTemp < 20 .AND. nTemp > 9
      sResult += sc_teens[ int( nTemp % 10 ) + 1 ]
   CASE nTemp < 10 .AND. nTemp > 0
      sResult += sc_ones[ int( nTemp) + 1 ]
   ENDCASE

   RETURN sResult + sQual

STATIC FUNCTION sol10( nNumber )

   RETURN Len( hb_ntos( Int( nNumber ) ) ) - 1
