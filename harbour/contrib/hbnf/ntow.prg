/*
 * $Id$
 */

/*
 * File......: ntow.prg
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

static sc_ones  := { "",     " One",   " Two",   " Three", " Four", " Five",  ;
                     " Six", " Seven", " Eight", " Nine"                      ;
                   }

static sc_teens := { " Ten",      " Eleven",    " Twelve",   ;
                     " Thirteen", " Fourteen",  " Fifteen",  ;
                     " Sixteen",  " Seventeen", " Eighteen", ;
                     " Nineteen"                             ;
                   }

static sc_tens  :=  { "", "", " Twenty", " Thirty", " Forty", " Fifty", ;
                      " Sixty", " Seventy", " Eighty", " Ninety"  }

static sc_qualifiers := { "", " Thousand", " Million", " Billion", " Trillion" }

#ifdef FT_TEST
  PROCEDURE Main( cNum )
     qout( ft_ntow( val( cNum ) ) )
     RETURN
#endif

function ft_ntow(nAmount)
  local nTemp, sResult := " ", nQualNo
  local nDiv := 10 ^ ( int( sol10(nAmount) / 3 ) * 3 )

  nTemp   := int(nAmount % nDiv)
  nAmount := int(nAmount / nDiv)
  nQualNo := int( sol10( nDiv ) / 3 ) + 1
  sResult += grp_to_words(nAmount, sc_qualifiers[ nQualNo ] )

  if nTemp > (nDiv /= 1000) .and. (nDiv > 1)
     sResult += ft_ntow( nTemp, nDiv )
  else
     sResult += grp_to_words(nTemp, "")
  endif
  return ltrim(sResult)

static function grp_to_words(nGrp, sQual)
  local sResult := "", nTemp

  nTemp   := int(nGrp % 100)
  nGrp    := int(nGrp / 100)
  sResult += sc_ones[ nGrp + 1 ] + iif( nGrp > 0, " Hundred", "")

  do case
     case nTemp > 19
         sResult += sc_tens[ int( nTemp / 10 ) + 1 ]
         sResult += sc_ones[ int( nTemp % 10 ) + 1 ]
     case nTemp < 20 .and. nTemp > 9
         sResult += sc_teens[ int( nTemp % 10 ) + 1 ]
     case nTemp < 10 .and. nTemp > 0
         sResult += sc_ones[ int( nTemp) + 1 ]
  endcase
  return sResult + sQual

static function sol10( nNumber )
  local sTemp

  sTemp := ltrim( str( int(nNumber), 0) )
  return len(sTemp) - 1
