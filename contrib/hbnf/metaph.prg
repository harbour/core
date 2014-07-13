/*
 * This is an original work by Dave Adams and is placed in the
 * public domain.
 *
 * Modification history:
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

/* Commentary
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

FUNCTION ft_Metaph( cName, nSize )  // Calculates the metaphone of a character string

   LOCAL cMeta

   __defaultNIL( @cName, "" )  // catch-all
   __defaultNIL( @nSize, 4 )   // default size: 4-bytes

   // Remove non-alpha characters and make upper case.
   // The string is padded with 1 space at the beginning and end.
   // Spaces, if present inside the string, are not removed until all
   // the prefix/suffix checking has been completed.
   cMeta := " " + _ftMakeAlpha( Upper( AllTrim( cName ) ) ) + " "

   #xtranslate MetaTran( <cFrom>, <cTo> ) => IF <cFrom> $ cMeta ; cMeta := StrTran( cMeta, <cFrom>, <cTo> ) ; ENDIF

   // Prefixes which need special consideration
   MetaTran( " KN"  , " N"  )
   MetaTran( " GN"  , " N"  )
   MetaTran( " PN"  , " N"  )
   MetaTran( " AE"  , " E"  )
   MetaTran( " X"   , " S"  )
   MetaTran( " WR"  , " R"  )
   MetaTran( " WHO" , " H"  )
   MetaTran( " WH"  , " W"  )
   MetaTran( " MCG" , " MK" )
   MetaTran( " MC"  , " MK" )
   MetaTran( " MACG", " MK" )
   MetaTran( " MAC" , " MK" )
   MetaTran( " GI"  , " K"  )

   // Suffixes which need special consideration
   MetaTran( "MB ", "M " )
   MetaTran( "NG ", "N " )

   // Remove inner spaces (1st and last byte are spaces)
   IF " " $ SubStr( cMeta, 2, Len( cMeta ) - 2 )
      cMeta := " " + StrTran( cMeta, " " ) + " "
   ENDIF

   // Double consonants sound much the same as singles
   MetaTran( "BB", "B"  )
   MetaTran( "CC", "CH" )
   MetaTran( "DD", "T"  )
   MetaTran( "FF", "F"  )
   MetaTran( "GG", "K"  )
   MetaTran( "KK", "K"  )
   MetaTran( "LL", "L"  )
   MetaTran( "MM", "M"  )
   MetaTran( "NN", "N"  )
   MetaTran( "PP", "P"  )
   MetaTran( "RR", "R"  )
   MetaTran( "SS", "S"  )
   MetaTran( "TT", "T"  )
   MetaTran( "XX", "KS" )
   MetaTran( "ZZ", "S"  )

   // J sounds
   MetaTran( "DGE", "J" )
   MetaTran( "DGY", "J" )
   MetaTran( "DGI", "J" )
   MetaTran( "GI" , "J" )
   MetaTran( "GE" , "J" )
   MetaTran( "GY" , "J" )

   // X sounds (KS)
   MetaTran( "X", "KS" )

   // special consideration for SCH
   MetaTran( "ISCH", "IX" )
   MetaTran( "SCH" , "SK" )

   // sh sounds (X)
   MetaTran( "CIA", "X" )
   MetaTran( "SIO", "X" )
   MetaTran( "SIA", "X" )
   MetaTran( "SH" , "X" )
   MetaTran( "TIA", "X" )
   MetaTran( "TIO", "X" )
   MetaTran( "TCH", "X" )
   MetaTran( "CH" , "X" )

   // hissing sounds (S)
   MetaTran( "SCI", "S" )
   MetaTran( "SCE", "S" )
   MetaTran( "SCY", "S" )
   MetaTran( "CI" , "S" )
   MetaTran( "CE" , "S" )
   MetaTran( "CY" , "S" )
   MetaTran( "Z"  , "S" )

   // th sound (0)
   MetaTran( "TH" , "0" )

   // Convert all vowels to 'v' from 3rd byte on
   cMeta := Left( cMeta, 2 ) + hb_StrReplace( SubStr( cMeta, 3 ), "AEIOU", "vvvvv" )

   // Make Y's silent if not followed by vowel
   IF "Y" $ cMeta
      cMeta := StrTran( cMeta, "Yv", "#"  )  // Y followed by vowel
      cMeta := StrTran( cMeta, "Y" , ""   )  // not followed by vowel
      cMeta := StrTran( cMeta, "#" , "Yv" )  // restore Y and vowel
   ENDIF

   // More G sounds, looking at surrounding vowels
   MetaTran( "GHv" , "G" )
   MetaTran( "vGHT", "T" )
   MetaTran( "vGH" , "W" )
   MetaTran( "GN"  , "N" )
   MetaTran( "G"   , "K" )

   // H sounds, looking at surrounding vowels
   MetaTran( "vHv", "H" )
   MetaTran( "vH" , ""  )

   // F sounds
   MetaTran( "PH", "F" )
   MetaTran( "V" , "F" )

   // D sounds a bit like T
   MetaTran( "D" , "T" )

   // K sounds
   MetaTran( "CK", "K" )
   MetaTran( "Q" , "K" )
   MetaTran( "C" , "K" )

   cMeta := StrTran( cMeta, "v" )  // Remove vowels

   RETURN PadR( AllTrim( cMeta ), nSize )

STATIC FUNCTION _ftMakeAlpha( cStr )  // Strips non-alpha characters from a string, leaving spaces

   LOCAL x, cAlpha := ""

   FOR x := 1 TO Len( cStr )
      IF SubStr( cStr, x, 1 ) == " " .OR. IsAlpha( SubStr( cStr, x, 1 ) )
         cAlpha += SubStr( cStr, x, 1 )
      ENDIF
   NEXT

   RETURN cAlpha
