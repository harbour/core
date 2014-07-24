/*
 * This is an original work by Gary Baren and is placed in the public domain.
 *
 * Modification history:
 *
 *    Rev 1.1   15 Aug 1991 23:05:54   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   09 Jun 1991 00:26:56   GLENN
 * Initial revision.
 *
 */

FUNCTION ft_NToW( nAmount )

   STATIC sc_qualifiers := { "", " Thousand", " Million", " Billion", " Trillion" }

   LOCAL nTemp, cResult
   LOCAL nDiv := 10 ^ ( Int( sol10( nAmount := Max( 0, nAmount ) ) / 3 ) * 3 )

   nTemp   := Int( nAmount % nDiv )
   nAmount := Int( nAmount / nDiv )
   cResult := grp_to_words( nAmount ) + ;
      sc_qualifiers[ Int( sol10( nDiv ) / 3 ) + 1 ]

   IF nTemp > ( nDiv /= 1000 ) .AND. nDiv > 1
      cResult += " " + ft_NToW( nTemp )
   ELSE
      cResult += grp_to_words( nTemp )
   ENDIF

   RETURN LTrim( cResult )

STATIC FUNCTION grp_to_words( nGrp )

   STATIC sc_ones := { ;
      "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine" }
   STATIC sc_teens := { ;
      " Ten",     " Eleven",    " Twelve", ;
      " Thirteen", " Fourteen", " Fifteen", " Sixteen", " Seventeen", " Eighteen", " Nineteen" }
   STATIC sc_tens :=  { "", "", ;
      " Twenty", " Thirty", " Forty", " Fifty", " Sixty", " Seventy", " Eighty", " Ninety" }

   LOCAL cResult, nTemp

   nTemp   := Int( nGrp % 100 )
   nGrp    := Int( nGrp / 100 )
   cResult := iif( nGrp >= 1, " " + sc_ones[ nGrp ] + " Hundred", "" )

   DO CASE
   CASE nTemp > 19
      cResult += ;
         sc_tens[ Int( nTemp / 10 ) + 1 ] + ;
         iif( ( nTemp := Int( nTemp % 10 ) ) >= 1, "-" + sc_ones[ nTemp ], "" )
   CASE nTemp < 20 .AND. nTemp > 9
      cResult += sc_teens[ Int( nTemp % 10 ) + 1 ]
   CASE nTemp < 10 .AND. nTemp >= 1
      cResult += " " + sc_ones[ Int( nTemp ) ]
   ENDCASE

   RETURN cResult

STATIC FUNCTION sol10( nNumber )
   RETURN Len( hb_ntos( Int( nNumber ) ) ) - 1
