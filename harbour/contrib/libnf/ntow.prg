/*
 * File......: NTOW.PRG
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




/*  $DOC$
 *  $FUNCNAME$
 *     FT_NTOW()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Translate numeric value to words
 *  $SYNTAX$
 *     FT_NTOW( <nNumber> ) -> cWords
 *  $ARGUMENTS$
 *     <nNumber>  An integer to translate
 *  $RETURNS$
 *     A text string representing <nNumber>
 *  $DESCRIPTION$
 *      Translates numeric input to a text string.
 *
 *      FT_NTOW is intended to be used with integers only.  Since I don't
 *      know what your application will be, I can't assume the type of
 *      fraction you want returned (ninety nine cents, 99/100, .99, etc).
 *      If you want the fraction in words, just pass it as an integer.
 *
 *      Do not pass a negative number!  Handle negative numbers any way
 *      you need to in your code.  (ie: CR, DB, Negative, Minus, etc.)
 *
 *      Also, numeric 0 is returned as a null string.  You will need to
 *      make a decision how to output it (zero dollars, no dollars, etc).
 *  $EXAMPLES$
 *		? FT_NTOW( 999 )		-> Nine Hundred Ninety Nine
 *
 *		? FT_NTOW( 1000 )		-> One Thousand
 *
 *		? FT_NTOW( 23 ) + " Dollars and " + FT_NTOW( 99 ) + " Cents"
 *			-> Twenty Three Dollars and Ninety Nine Cents
 *
 *		? FT_NTOW( 23 ) + " Dollars and " + "99/100"
 *			-> Twenty Three Dollars and 99/100
 *
 *    x      := -23.99
 *    cents  := str( (x - int( x )) * 100, 2, 0 ) + "/100"
 *		x      := int( x )
 *    string := iif( x < 0, "Credit of ", "Debit of " )
 *		? string + FT_NTOW( abs(x) ) + " Dollars and " + "99/100"
 *		     -> Credit of Twenty Three Dollars and 99/100
 *  $END$
 */



static ones  := { "",     " One",   " Two",   " Three", " Four", " Five",  ;
                  " Six", " Seven", " Eight", " Nine"                      ;
                }

static teens := { " Ten",      " Eleven",    " Twelve",   ;
                  " Thirteen", " Fourteen",  " Fifteen",  ;
                  " Sixteen",  " Seventeen", " Eighteen", ;
                  " Nineteen"                             ;
                }

static tens  :=  { "", "", " Twenty", " Thirty", " Forty", " Fifty", ;
                   " Sixty", " Seventy", " Eighty", " Ninety"  }

static qualifiers := { "", " Thousand", " Million", " Billion", " Trillion" }


#ifdef FT_TEST
  function main( cNum )
     return qout( ft_ntow( val( cNum ) ) )
#endif



function ft_ntow(nAmount)
  local nTemp, sResult := " ", nQualNo
  local nDiv := 10 ^ ( int( sol10(nAmount) / 3 ) * 3 )

  nTemp   := int(nAmount % nDiv)
  nAmount := int(nAmount / nDiv)
  nQualNo := int( sol10( nDiv ) / 3 ) + 1
  sResult += grp_to_words(nAmount, qualifiers[ nQualNo ] )

  if nTemp > (nDiv /= 1000) .and. (nDiv > 1)
     sResult += ft_ntow( nTemp, nDiv )
  else
	   sResult += grp_to_words(nTemp, "")
  endif
  return( ltrim(sResult) )


static function grp_to_words(nGrp, sQual)
  local sResult := "", nTemp

  nTemp   := int(nGrp % 100)
  nGrp    := int(nGrp / 100)
  sResult += ones[ nGrp + 1 ] + iif( nGrp > 0, " Hundred", "")

  do case
	   case nTemp > 19
		   sResult += tens[ int( nTemp / 10 ) + 1 ]
  		sResult += ones[ int( nTemp % 10 ) + 1 ]
     case nTemp < 20 .and. nTemp > 9
		   sResult += teens[ int( nTemp % 10 ) + 1 ]
     case nTemp < 10 .and. nTemp > 0
		   sResult += ones[ int( nTemp) + 1 ]
  endcase
  return(sResult + sQual)


static function sol10( nNumber )
  local sTemp

  sTemp := ltrim( str( int(nNumber), 0) )
  return( len(sTemp) - 1 )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
