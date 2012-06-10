/*
 * $Id$
 */

/*
 * File......: metaph.prg
 * Author....: Dave Adams
 * CIS ID....: ?
 *
 * This is an original work by Dave Adams and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:20   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:44   GLENN
 * Nanforum Toolkit
 *
 */

/*
 * File Contents
 *
 *   FT_METAPH()      Calculates the metaphone of a name
 *   _ftMakeAlpha()   Removes non-alpha characters from a string
 *   _ftConvVowel()   Converts all vowels to the letter 'v'
 *
 *
 * Commentary
 *
 *  The concepts for this algoritm were adapted from an article in the
 *  Computer Language Magazine (Dec.90, Vol.7, No.12) written by
 *  Lawrence B.F. Phillips.
 *
 *  The STRTRAN function was selected to calculate the MetaPhone, to
 *  allow the algoritm to be fine-tuned in an easy manner, as there are
 *  always exceptions to any phonetic pronunciation in not only English,
 *  but many other languages as well.
 *
 *  What is a metaphone?
 *  Basically it takes a character string, removes the vowels, and equates
 *  letters (or groups of letters) to other consonent sounds.  The vowels
 *  are not removed until near the end, as they play an important part
 *  in determining how some consonents sound in different surroundings.
 *
 *  The consonant sounds are:  B, F, H, J, K, L, M, N, P, R, S, T, W, X, Y, 0
 *  Vowels are only included if they are at the beginning.
 *  Here are the transformations. The order of evaluation is important
 *  as characters may meet more than one transformation conditions.
 *  ( note: v == vowel )
 *
 *    B --> B  unless at end of a word after 'm' as in dumb.
 *    C --> X  (sh)  CIA, TCH, CH, ISCH, CC
 *           S  SCI, SCE, SCY, CI, CE, CY
 *           K  otherwise ( including CK )
 *    D --> J  DGE, DGY, DGI
 *           T  otherwise
 *    F --> F
 *    G --> K  GHv, vGHT
 *           W  vGH
 *           J  DGE, DGY, DGI, GI, GE, GY
 *           N  GN
 *           K  otherwise
 *    H --> H  vHv
 *              otherwise silent
 *    J --> J
 *    K --> K
 *    L --> L
 *    M --> M
 *    N --> N
 *    P --> F  PH
 *           P  otherwise
 *    Q --> K
 *    R --> R
 *    S --> X  (sh) SH, SIO, SIA, ISCH
 *           S  otherwise
 *    T --> X  (sh) TIA, TIO, TCH
 *           0  (th) TH
 *           T  otherwise
 *    V --> F
 *    W --> W
 *    X --> KS
 *    Y -->    vY
 *           Y  otherwise
 *    Z --> S
 *
 */

*------------------------------------------------
//  Demo of FT_METAPH()

//  #define FT_TEST .T.

#IFDEF FT_TEST
  FUNCTION MAIN()
  LOCAL cJunk  := SPACE( 8000 )
  LOCAL aNames := {}
  LOCAL cName, nElem

  SET( _SET_SCOREBOARD, .F.   )
  SET( _SET_COLOR,      "W/B" )
  CLS

  //  Demo will create an array of names and display in 3 columns
  //  _ftRow() and _ftCol() will calculate the screen co-ordinates
  //  by evaluating the element number

  AADD( aNames, "Adams"        )
  AADD( aNames, "Addams"       )
  AADD( aNames, "Atoms"        )
  AADD( aNames, "Adamson"      )
  AADD( aNames, "Cajun"        )
  AADD( aNames, "Cagen"        )
  AADD( aNames, "Cochy"        )
  AADD( aNames, "Cocci"        )
  AADD( aNames, "Smith"        )
  AADD( aNames, "Smythe"       )
  AADD( aNames, "Naylor"       )
  AADD( aNames, "Nailer"       )
  AADD( aNames, "Holberry"     )
  AADD( aNames, "Wholebary"    )
  AADD( aNames, "Jackson"      )
  AADD( aNames, "Jekksen"      )
  AADD( aNames, "The Source"   )
  AADD( aNames, "The Sores"    )
  AADD( aNames, "Jones"        )
  AADD( aNames, "Johns"        )
  AADD( aNames, "Lennon"       )
  AADD( aNames, "Lenin"        )
  AADD( aNames, "Fischer"      )
  AADD( aNames, "Fisher"       )
  AADD( aNames, "O'Donnell"    )
  AADD( aNames, "O Donald"     )
  AADD( aNames, "Pugh"         )
  AADD( aNames, "Pew"          )
  AADD( aNames, "Heimendinger" )
  AADD( aNames, "Hymendinker"  )
  AADD( aNames, "Knight"       )
  AADD( aNames, "Nite"         )
  AADD( aNames, "Lamb"         )
  AADD( aNames, "Lamb Chops"   )
  AADD( aNames, "Stephens"     )
  AADD( aNames, "Stevens"      )
  AADD( aNames, "Neilson"      )
  AADD( aNames, "Nelson"       )
  AADD( aNames, "Tchaikovski"  )
  AADD( aNames, "Chikofski"    )
  AADD( aNames, "Caton"        )
  AADD( aNames, "Wright"       )
  AADD( aNames, "Write"        )
  AADD( aNames, "Right"        )
  AADD( aNames, "Manual"       )
  AADD( aNames, "Now"          )
  AADD( aNames, "Wheatabix"    )
  AADD( aNames, "Science"      )
  AADD( aNames, "Cinzano"      )
  AADD( aNames, "Lucy"         )
  AADD( aNames, "Reece"        )
  AADD( aNames, "Righetti"     )
  AADD( aNames, "Oppermann"    )
  AADD( aNames, "Bookkeeper"   )
  AADD( aNames, "McGill"       )
  AADD( aNames, "Magic"        )
  AADD( aNames, "McLean"       )
  AADD( aNames, "McLane"       )
  AADD( aNames, "Maclean"      )
  AADD( aNames, "Exxon"        )

  // display names and metaphones in 3 columns on screen
  AEVAL( aNames, ;
         { | cName, nElem | ;
             SETPOS( _ftRow( nElem ), _ftCol( nElem ) ), ;
             QQOUT( PadR( cName, 18, "." ) + FT_METAPH( cName ) ) ;
         } )

  SETPOS( 21, 00 )
  QUIT

  *------------------------------------------------
  STATIC FUNCTION _ftRow( nElem )  //  Determine which row to print on
  RETURN IIF( nElem > 40, nElem - 40, IIF( nElem > 20, nElem - 20, nElem ) )
  *------------------------------------------------
  STATIC FUNCTION _ftCol( nElem )  //  Determine which column to start print
  RETURN IIF( nElem > 40,  55, IIF( nElem > 20, 28, 1 ) )
  *------------------------------------------------

#endif
// End of Test program

*------------------------------------------------
FUNCTION FT_METAPH ( cName, nSize )
//  Calculates the metaphone of a character string

LOCAL cMeta

cName := IIF( cName == NIL, "", cName )  //  catch-all
nSize := IIF( nSize == NIL, 4,  nSize )  //  default size: 4-bytes

//  Remove non-alpha characters and make upper case.
//  The string is padded with 1 space at the beginning & end.
//  Spaces, if present inside the string, are not removed until all
//  the prefix/suffix checking has been completed.
cMeta := " " + _ftMakeAlpha( UPPER( ALLTRIM( cName ) ) ) + " "

//  prefixes which need special consideration
IF " KN"   $ cMeta ;  cMeta := STRTRAN( cMeta, " KN" , " N"  ) ;  ENDIF
IF " GN"   $ cMeta ;  cMeta := STRTRAN( cMeta, " GN" , " N"  ) ;  ENDIF
IF " PN"   $ cMeta ;  cMeta := STRTRAN( cMeta, " PN" , " N"  ) ;  ENDIF
IF " AE"   $ cMeta ;  cMeta := STRTRAN( cMeta, " AE" , " E"  ) ;  ENDIF
IF " X"    $ cMeta ;  cMeta := STRTRAN( cMeta, " X"  , " S"  ) ;  ENDIF
IF " WR"   $ cMeta ;  cMeta := STRTRAN( cMeta, " WR" , " R"  ) ;  ENDIF
IF " WHO"  $ cMeta ;  cMeta := STRTRAN( cMeta, " WHO", " H"  ) ;  ENDIF
IF " WH"   $ cMeta ;  cMeta := STRTRAN( cMeta, " WH" , " W"  ) ;  ENDIF
IF " MCG"  $ cMeta ;  cMeta := STRTRAN( cMeta, " MCG", " MK" ) ;  ENDIF
IF " MC"   $ cMeta ;  cMeta := STRTRAN( cMeta, " MC" , " MK" ) ;  ENDIF
IF " MACG" $ cMeta ;  cMeta := STRTRAN( cMeta, " MACG"," MK" ) ;  ENDIF
IF " MAC"  $ cMeta ;  cMeta := STRTRAN( cMeta, " MAC", " MK" ) ;  ENDIF
IF " GI"   $ cMeta ;  cMeta := STRTRAN( cMeta, " GI",  " K"  ) ;  ENDIF

//  Suffixes which need special consideration
IF "MB " $ cMeta ;  cMeta := STRTRAN( cMeta, "MB " , "M " ) ;  ENDIF
IF "NG " $ cMeta ;  cMeta := STRTRAN( cMeta, "NG " , "N " ) ;  ENDIF

//  Remove inner spaces (1st and last byte are spaces)
IF " " $ SUBSTR( cMeta, 2, LEN( cMeta ) - 2 )
  cMeta := " " + STRTRAN( cMeta, " " , "" ) + " "
ENDIF

//  Double consonants sound much the same as singles
IF "BB"  $ cMeta ;  cMeta := STRTRAN( cMeta, "BB"  , "B"  ) ;  ENDIF
IF "CC"  $ cMeta ;  cMeta := STRTRAN( cMeta, "CC"  , "CH" ) ;  ENDIF
IF "DD"  $ cMeta ;  cMeta := STRTRAN( cMeta, "DD"  , "T"  ) ;  ENDIF
IF "FF"  $ cMeta ;  cMeta := STRTRAN( cMeta, "FF"  , "F"  ) ;  ENDIF
IF "GG"  $ cMeta ;  cMeta := STRTRAN( cMeta, "GG"  , "K"  ) ;  ENDIF
IF "KK"  $ cMeta ;  cMeta := STRTRAN( cMeta, "KK"  , "K"  ) ;  ENDIF
IF "LL"  $ cMeta ;  cMeta := STRTRAN( cMeta, "LL"  , "L"  ) ;  ENDIF
IF "MM"  $ cMeta ;  cMeta := STRTRAN( cMeta, "MM"  , "M"  ) ;  ENDIF
IF "NN"  $ cMeta ;  cMeta := STRTRAN( cMeta, "NN"  , "N"  ) ;  ENDIF
IF "PP"  $ cMeta ;  cMeta := STRTRAN( cMeta, "PP"  , "P"  ) ;  ENDIF
IF "RR"  $ cMeta ;  cMeta := STRTRAN( cMeta, "RR"  , "R"  ) ;  ENDIF
IF "SS"  $ cMeta ;  cMeta := STRTRAN( cMeta, "SS"  , "S"  ) ;  ENDIF
IF "TT"  $ cMeta ;  cMeta := STRTRAN( cMeta, "TT"  , "T"  ) ;  ENDIF
IF "XX"  $ cMeta ;  cMeta := STRTRAN( cMeta, "XX"  , "KS" ) ;  ENDIF
IF "ZZ"  $ cMeta ;  cMeta := STRTRAN( cMeta, "ZZ"  , "S"  ) ;  ENDIF

//  J sounds
IF "DGE" $ cMeta ;  cMeta := STRTRAN( cMeta, "DGE" , "J"  ) ;  ENDIF
IF "DGY" $ cMeta ;  cMeta := STRTRAN( cMeta, "DGY" , "J"  ) ;  ENDIF
IF "DGI" $ cMeta ;  cMeta := STRTRAN( cMeta, "DGI" , "J"  ) ;  ENDIF
IF "GI"  $ cMeta ;  cMeta := STRTRAN( cMeta, "GI"  , "J"  ) ;  ENDIF
IF "GE"  $ cMeta ;  cMeta := STRTRAN( cMeta, "GE"  , "J"  ) ;  ENDIF
IF "GY"  $ cMeta ;  cMeta := STRTRAN( cMeta, "GY"  , "J"  ) ;  ENDIF

//  X sounds (KS)
IF "X"   $ cMeta ;  cMeta := STRTRAN( cMeta, "X"   , "KS" ) ;  ENDIF

// special consideration for SCH
IF "ISCH" $ cMeta;  cMeta := STRTRAN( cMeta, "ISCH", "IX" ) ;  ENDIF
IF "SCH" $ cMeta ;  cMeta := STRTRAN( cMeta, "SCH" , "SK" ) ;  ENDIF

//  sh sounds (X)
IF "CIA" $ cMeta ;  cMeta := STRTRAN( cMeta, "CIA" , "X"  ) ;  ENDIF
IF "SIO" $ cMeta ;  cMeta := STRTRAN( cMeta, "SIO" , "X"  ) ;  ENDIF
IF "C"   $ cMeta ;  cMeta := STRTRAN( cMeta, "SIA" , "X"  ) ;  ENDIF
IF "SH"  $ cMeta ;  cMeta := STRTRAN( cMeta, "SH"  , "X"  ) ;  ENDIF
IF "TIA" $ cMeta ;  cMeta := STRTRAN( cMeta, "TIA" , "X"  ) ;  ENDIF
IF "TIO" $ cMeta ;  cMeta := STRTRAN( cMeta, "TIO" , "X"  ) ;  ENDIF
IF "TCH" $ cMeta ;  cMeta := STRTRAN( cMeta, "TCH" , "X"  ) ;  ENDIF
IF "CH"  $ cMeta ;  cMeta := STRTRAN( cMeta, "CH"  , "X"  ) ;  ENDIF

//  hissing sounds (S)
IF "SCI" $ cMeta ;  cMeta := STRTRAN( cMeta, "SCI" , "S"  ) ;  ENDIF
IF "SCE" $ cMeta ;  cMeta := STRTRAN( cMeta, "SCE" , "S"  ) ;  ENDIF
IF "SCY" $ cMeta ;  cMeta := STRTRAN( cMeta, "SCY" , "S"  ) ;  ENDIF
IF "CI"  $ cMeta ;  cMeta := STRTRAN( cMeta, "CI"  , "S"  ) ;  ENDIF
IF "CE"  $ cMeta ;  cMeta := STRTRAN( cMeta, "CE"  , "S"  ) ;  ENDIF
IF "CY"  $ cMeta ;  cMeta := STRTRAN( cMeta, "CY"  , "S"  ) ;  ENDIF
IF "Z"   $ cMeta ;  cMeta := STRTRAN( cMeta, "Z"   , "S"  ) ;  ENDIF

//  th sound (0)
IF "TH"  $ cMeta ;  cMeta := STRTRAN( cMeta, "TH"  , "0"  ) ;  ENDIF

//  Convert all vowels to 'v' from 3rd byte on
cMeta := LEFT( cMeta, 2 ) + _ftConvVowel( SUBSTR( cMeta, 3 ) )

// Make Y's silent if not followed by vowel
IF "Y"   $ cMeta
  cMeta := STRTRAN( cMeta, "Yv"  , "#"  )  // Y followed by vowel
  cMeta := STRTRAN( cMeta, "Y"   , ""   )  // not followed by vowel
  cMeta := STRTRAN( cMeta, "#"   , "Yv" )  // restore Y and vowel
ENDIF

//  More G sounds, looking at surrounding vowels
IF "GHv" $ cMeta ;  cMeta := STRTRAN( cMeta, "GHv" , "G"  ) ;  ENDIF
IF "vGHT" $ cMeta;  cMeta := STRTRAN( cMeta, "vGHT", "T"  ) ;  ENDIF
IF "vGH" $ cMeta ;  cMeta := STRTRAN( cMeta, "vGH" , "W"  ) ;  ENDIF
IF "GN"  $ cMeta ;  cMeta := STRTRAN( cMeta, "GN"  , "N"  ) ;  ENDIF
IF "G"   $ cMeta ;  cMeta := STRTRAN( cMeta, "G"   , "K"  ) ;  ENDIF

//  H sounds, looking at surrounding vowels
IF "vHv" $ cMeta ;  cMeta := STRTRAN( cMeta, "vHv" , "H"  ) ;  ENDIF
IF "vH"  $ cMeta ;  cMeta := STRTRAN( cMeta, "vH"  , ""   ) ;  ENDIF

//  F sounds
IF "PH"  $ cMeta ;  cMeta := STRTRAN( cMeta, "PH"  , "F"  ) ;  ENDIF
IF "V"   $ cMeta ;  cMeta := STRTRAN( cMeta, "V"   , "F"  ) ;  ENDIF

//  D sounds a bit like T
IF "D"   $ cMeta ;  cMeta := STRTRAN( cMeta, "D"   , "T"  ) ;  ENDIF

//  K sounds
IF "CK"  $ cMeta ;  cMeta := STRTRAN( cMeta, "CK"  , "K"  ) ;  ENDIF
IF "Q"   $ cMeta ;  cMeta := STRTRAN( cMeta, "Q"   , "K"  ) ;  ENDIF
IF "C"   $ cMeta ;  cMeta := STRTRAN( cMeta, "C"   , "K"  ) ;  ENDIF

//  Remove vowels
cMeta := STRTRAN( cMeta, "v", "" )

RETURN PadR( ALLTRIM( cMeta ), nSize )

*------------------------------------------------
STATIC FUNCTION _ftMakeAlpha ( cStr )
//  Strips non-alpha characters from a string, leaving spaces

LOCAL x, cAlpha := ""

FOR x := 1 to LEN( cStr )
  IF SUBSTR( cStr, x, 1 ) == " " .OR. IsAlpha( SUBSTR( cStr, x, 1 ) )
    cAlpha := cAlpha + SUBSTR( cStr, x, 1 )
  ENDIF
NEXT

RETURN cAlpha

*------------------------------------------------
STATIC FUNCTION _ftConvVowel ( cStr )
//  Converts all vowels to letter 'v'

LOCAL x, cConverted := ""

FOR x := 1 to LEN( cStr )
  IF SUBSTR( cStr, x, 1 ) $ "AEIOU"
    cConverted := cConverted + "v"
  ELSE
    cConverted := cConverted + SUBSTR( cStr, x, 1 )
  ENDIF
NEXT

RETURN cConverted

*------------------------------------------------
// eof metaph.prg
