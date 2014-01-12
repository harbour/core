/*
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
 *   ft_Metaph()      Calculates the metaphone of a name
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

FUNCTION ft_Metaph( cName, nSize ) //  Calculates the metaphone of a character string

   LOCAL cMeta

   __defaultNIL( @cName, "" )  //  catch-all
   __defaultNIL( @nSize, 4 )   //  default size: 4-bytes

   // Remove non-alpha characters and make upper case.
   // The string is padded with 1 space at the beginning and end.
   // Spaces, if present inside the string, are not removed until all
   // the prefix/suffix checking has been completed.
   cMeta := " " + _ftMakeAlpha( Upper( AllTrim( cName ) ) ) + " "

   // Prefixes which need special consideration
   IF " KN"   $ cMeta ; cMeta := StrTran( cMeta, " KN"  , " N"  ) ; ENDIF
   IF " GN"   $ cMeta ; cMeta := StrTran( cMeta, " GN"  , " N"  ) ; ENDIF
   IF " PN"   $ cMeta ; cMeta := StrTran( cMeta, " PN"  , " N"  ) ; ENDIF
   IF " AE"   $ cMeta ; cMeta := StrTran( cMeta, " AE"  , " E"  ) ; ENDIF
   IF " X"    $ cMeta ; cMeta := StrTran( cMeta, " X"   , " S"  ) ; ENDIF
   IF " WR"   $ cMeta ; cMeta := StrTran( cMeta, " WR"  , " R"  ) ; ENDIF
   IF " WHO"  $ cMeta ; cMeta := StrTran( cMeta, " WHO" , " H"  ) ; ENDIF
   IF " WH"   $ cMeta ; cMeta := StrTran( cMeta, " WH"  , " W"  ) ; ENDIF
   IF " MCG"  $ cMeta ; cMeta := StrTran( cMeta, " MCG" , " MK" ) ; ENDIF
   IF " MC"   $ cMeta ; cMeta := StrTran( cMeta, " MC"  , " MK" ) ; ENDIF
   IF " MACG" $ cMeta ; cMeta := StrTran( cMeta, " MACG", " MK" ) ; ENDIF
   IF " MAC"  $ cMeta ; cMeta := StrTran( cMeta, " MAC" , " MK" ) ; ENDIF
   IF " GI"   $ cMeta ; cMeta := StrTran( cMeta, " GI"  , " K"  ) ; ENDIF

   // Suffixes which need special consideration
   IF "MB " $ cMeta ;  cMeta := StrTran( cMeta, "MB ", "M " ) ; ENDIF
   IF "NG " $ cMeta ;  cMeta := StrTran( cMeta, "NG ", "N " ) ; ENDIF

   // Remove inner spaces (1st and last byte are spaces)
   IF " " $ SubStr( cMeta, 2, Len( cMeta ) - 2 )
      cMeta := " " + StrTran( cMeta, " " ) + " "
   ENDIF

   // Double consonants sound much the same as singles
   IF "BB"  $ cMeta ; cMeta := StrTran( cMeta, "BB", "B"  ) ; ENDIF
   IF "CC"  $ cMeta ; cMeta := StrTran( cMeta, "CC", "CH" ) ; ENDIF
   IF "DD"  $ cMeta ; cMeta := StrTran( cMeta, "DD", "T"  ) ; ENDIF
   IF "FF"  $ cMeta ; cMeta := StrTran( cMeta, "FF", "F"  ) ; ENDIF
   IF "GG"  $ cMeta ; cMeta := StrTran( cMeta, "GG", "K"  ) ; ENDIF
   IF "KK"  $ cMeta ; cMeta := StrTran( cMeta, "KK", "K"  ) ; ENDIF
   IF "LL"  $ cMeta ; cMeta := StrTran( cMeta, "LL", "L"  ) ; ENDIF
   IF "MM"  $ cMeta ; cMeta := StrTran( cMeta, "MM", "M"  ) ; ENDIF
   IF "NN"  $ cMeta ; cMeta := StrTran( cMeta, "NN", "N"  ) ; ENDIF
   IF "PP"  $ cMeta ; cMeta := StrTran( cMeta, "PP", "P"  ) ; ENDIF
   IF "RR"  $ cMeta ; cMeta := StrTran( cMeta, "RR", "R"  ) ; ENDIF
   IF "SS"  $ cMeta ; cMeta := StrTran( cMeta, "SS", "S"  ) ; ENDIF
   IF "TT"  $ cMeta ; cMeta := StrTran( cMeta, "TT", "T"  ) ; ENDIF
   IF "XX"  $ cMeta ; cMeta := StrTran( cMeta, "XX", "KS" ) ; ENDIF
   IF "ZZ"  $ cMeta ; cMeta := StrTran( cMeta, "ZZ", "S"  ) ; ENDIF

   //  J sounds
   IF "DGE" $ cMeta ; cMeta := StrTran( cMeta, "DGE", "J" ) ; ENDIF
   IF "DGY" $ cMeta ; cMeta := StrTran( cMeta, "DGY", "J" ) ; ENDIF
   IF "DGI" $ cMeta ; cMeta := StrTran( cMeta, "DGI", "J" ) ; ENDIF
   IF "GI"  $ cMeta ; cMeta := StrTran( cMeta, "GI" , "J" ) ; ENDIF
   IF "GE"  $ cMeta ; cMeta := StrTran( cMeta, "GE" , "J" ) ; ENDIF
   IF "GY"  $ cMeta ; cMeta := StrTran( cMeta, "GY" , "J" ) ; ENDIF

   //  X sounds (KS)
   IF "X"   $ cMeta ; cMeta := StrTran( cMeta, "X", "KS" ) ; ENDIF

   // special consideration for SCH
   IF "ISCH" $ cMeta; cMeta := StrTran( cMeta, "ISCH", "IX" ) ; ENDIF
   IF "SCH"  $ cMeta; cMeta := StrTran( cMeta, "SCH" , "SK" ) ; ENDIF

   //  sh sounds (X)
   IF "CIA" $ cMeta ; cMeta := StrTran( cMeta, "CIA", "X" ) ; ENDIF
   IF "SIO" $ cMeta ; cMeta := StrTran( cMeta, "SIO", "X" ) ; ENDIF
   IF "C"   $ cMeta ; cMeta := StrTran( cMeta, "SIA", "X" ) ; ENDIF
   IF "SH"  $ cMeta ; cMeta := StrTran( cMeta, "SH" , "X" ) ; ENDIF
   IF "TIA" $ cMeta ; cMeta := StrTran( cMeta, "TIA", "X" ) ; ENDIF
   IF "TIO" $ cMeta ; cMeta := StrTran( cMeta, "TIO", "X" ) ; ENDIF
   IF "TCH" $ cMeta ; cMeta := StrTran( cMeta, "TCH", "X" ) ; ENDIF
   IF "CH"  $ cMeta ; cMeta := StrTran( cMeta, "CH" , "X" ) ; ENDIF

   //  hissing sounds (S)
   IF "SCI" $ cMeta ; cMeta := StrTran( cMeta, "SCI", "S" ) ; ENDIF
   IF "SCE" $ cMeta ; cMeta := StrTran( cMeta, "SCE", "S" ) ; ENDIF
   IF "SCY" $ cMeta ; cMeta := StrTran( cMeta, "SCY", "S" ) ; ENDIF
   IF "CI"  $ cMeta ; cMeta := StrTran( cMeta, "CI" , "S" ) ; ENDIF
   IF "CE"  $ cMeta ; cMeta := StrTran( cMeta, "CE" , "S" ) ; ENDIF
   IF "CY"  $ cMeta ; cMeta := StrTran( cMeta, "CY" , "S" ) ; ENDIF
   IF "Z"   $ cMeta ; cMeta := StrTran( cMeta, "Z"  , "S" ) ; ENDIF

   //  th sound (0)
   IF "TH"  $ cMeta ; cMeta := StrTran( cMeta, "TH" , "0" ) ; ENDIF

   //  Convert all vowels to 'v' from 3rd byte on
   cMeta := Left( cMeta, 2 ) + _ftConvVowel( SubStr( cMeta, 3 ) )

   // Make Y's silent if not followed by vowel
   IF "Y" $ cMeta
      cMeta := StrTran( cMeta, "Yv", "#"  )  // Y followed by vowel
      cMeta := StrTran( cMeta, "Y" , ""   )  // not followed by vowel
      cMeta := StrTran( cMeta, "#" , "Yv" )  // restore Y and vowel
   ENDIF

   //  More G sounds, looking at surrounding vowels
   IF "GHv"  $ cMeta ; cMeta := StrTran( cMeta, "GHv" , "G"  ) ; ENDIF
   IF "vGHT" $ cMeta ; cMeta := StrTran( cMeta, "vGHT", "T"  ) ; ENDIF
   IF "vGH"  $ cMeta ; cMeta := StrTran( cMeta, "vGH" , "W"  ) ; ENDIF
   IF "GN"   $ cMeta ; cMeta := StrTran( cMeta, "GN"  , "N"  ) ; ENDIF
   IF "G"    $ cMeta ; cMeta := StrTran( cMeta, "G"   , "K"  ) ; ENDIF

   //  H sounds, looking at surrounding vowels
   IF "vHv" $ cMeta ; cMeta := StrTran( cMeta, "vHv" , "H"  ) ; ENDIF
   IF "vH"  $ cMeta ; cMeta := StrTran( cMeta, "vH"  , ""   ) ; ENDIF

   //  F sounds
   IF "PH"  $ cMeta ; cMeta := StrTran( cMeta, "PH"  , "F"  ) ; ENDIF
   IF "V"   $ cMeta ; cMeta := StrTran( cMeta, "V"   , "F"  ) ; ENDIF

   //  D sounds a bit like T
   IF "D"   $ cMeta ; cMeta := StrTran( cMeta, "D"   , "T"  ) ; ENDIF

   //  K sounds
   IF "CK"  $ cMeta ; cMeta := StrTran( cMeta, "CK"  , "K"  ) ; ENDIF
   IF "Q"   $ cMeta ; cMeta := StrTran( cMeta, "Q"   , "K"  ) ; ENDIF
   IF "C"   $ cMeta ; cMeta := StrTran( cMeta, "C"   , "K"  ) ; ENDIF

   // Remove vowels
   cMeta := StrTran( cMeta, "v" )

   RETURN PadR( AllTrim( cMeta ), nSize )

STATIC FUNCTION _ftMakeAlpha( cStr ) //  Strips non-alpha characters from a string, leaving spaces

   LOCAL x, cAlpha := ""

   FOR x := 1 TO Len( cStr )
      IF SubStr( cStr, x, 1 ) == " " .OR. IsAlpha( SubStr( cStr, x, 1 ) )
         cAlpha += SubStr( cStr, x, 1 )
      ENDIF
   NEXT

   RETURN cAlpha

STATIC FUNCTION _ftConvVowel( cStr ) //  Converts all vowels to letter 'v'

   LOCAL x, cConverted := ""

   FOR x := 1 TO Len( cStr )
      cConverted += iif( SubStr( cStr, x, 1 ) $ "AEIOU", "v", SubStr( cStr, x, 1 ) )
   NEXT

   RETURN cConverted
