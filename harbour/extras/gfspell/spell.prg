/*
 * $Id$
 */

// Author....: Joseph D. Booth
// Copyright.: (C)1993, Joseph D. Booth, All Rights Reserved
//
// Purpose...: Library of functions to allow a Harbour program to
//             spell check a string and to also work with individual
//             words.
//
//             Sp_Add()        - Add word to the dictionary
//             Sp_Cache()      - Add word to spelling cache
//             Sp_Check()      - Is word spelled correctly?
//             Sp_Clear()      - Clear the spelling cache
//             Sp_Init()       - Initialize the spelling dictionary
//             Sp_GetSet()     - Get/Set global parameters
//             Sp_LoadAux()    - Loads an auxiliary dictionary
//             Sp_Suggest()    - Offer list of sound alike suggestions
//             Sp_Quick()      - Offer list of suggested spellings
//             Sp_WildCard()   - List of wildcard matches
//
//             DBF2Dic()       - Convert a DBF file to a DIC file
//             Dic2DBF()       - Convert a DIC file to a DBF file
//

#include "fileio.ch"

#define EACH_WORD              6
#define NSIZE                  ( 26 * 26 * EACH_WORD )
#define CRLF                   Chr( 13 ) + Chr( 10 )
#define FOUR_BYTES             hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 )
#define MAX_STRING             40000

#define COMMON_WORDS           t_aGlobal[  1 ]
#define CACHE_WORDS            t_aGlobal[  2 ]
#define DICTIONARY_PATH        t_aGlobal[  3 ]
#define DICTIONARY_NAME        t_aGlobal[  4 ]
#define AUXILIARY_DICTIONARY   t_aGlobal[  5 ]
#define EXTRA_CODE_BLOCK       t_aGlobal[  6 ]
#define ADD_SUFFIXES           t_aGlobal[  7 ]
#define ADD_PREFIXES           t_aGlobal[  8 ]
#define ADD_PLURALS            t_aGlobal[  9 ]
#define SORT_SUGGESTIONS       t_aGlobal[ 10 ]
#define SUGGEST_PREFERENCE     t_aGlobal[ 11 ]
#define MINIMUM_WORD_LENGTH    t_aGlobal[ 12 ]
#define METAPHONE_SIZE         t_aGlobal[ 13 ]
#define MAX_DIFFERENCE         t_aGlobal[ 14 ]
#define THESAURUS_NAME         t_aGlobal[ 15 ]
#define CHECK_RUNONS           t_aGlobal[ 16 ]

THREAD STATIC t_aGlobal := { ;
   NIL, ;
   "", ;
   "", ;
   "dict.dic", ;
   "", ;
   NIL, ;
   .T., ;
   .T., ;
   .T., ;
   .T., ;
   "B", ;     // (M)etaphone, (A)lgorithmic, (B)oth
    2, ;
    5, ;
    1, ;
    "thes.dic", ;
   .T. }

STATIC sc_aContracts := { ;
   { "CAN'T"       ,"CANNOT"    }, ;
   { "WON'T"       ,"WILL NOT"  }, ;
   { "AREN'T"      ,"ARE NOT"   }, ;
   { "AIN'T"       ,"ARE NOT"   }, ;
   { "THEY'RE"     ,"THEY ARE"  }, ;
   { "THEY'VE"     ,"THEY HAVE" }, ;
   { "IT'S"        ,"IT IS"     }, ;
   { "I'LL"        ,"I WILL"    }, ;
   { "I'D"         ,"I WOULD"   }, ;
   { "DON'T"       ,"DO NOT"    } }


THREAD STATIC t_nHandle := F_ERROR
THREAD STATIC t_cOffsets


//  Function:  Sp_Add()
//   Purpose:  Adds a word to the dictionary
//    Syntax:  <logical> := Sp_Add( cWord )
// Arguments:  cWord     - Word to add to the auxiliary dictionary
//   Returns:  <logical> - TRUE if added,
//                         FALSE otherwise
//
//     Notes:  Does not check to see if the word already exists in the
//             dictionary.

FUNCTION Sp_Add( cWord )

   LOCAL was_added := .F.             // Was word written to dictionary?
   LOCAL nAuxHandle                   // Dictionary file handle
   LOCAL nWritten                     // Number of bytes written

   // Initialize the spell checker and make sure
   // an auxiliary dictionary name was specified

   IF Sp_Init() .AND. ! Empty( AUXILIARY_DICTIONARY )

      cWord := Upper( AllTrim( cWord ) )

      //
      // If the auxiliary dictionary does not exist,
      // we will create it for the user
      //

      IF hb_FileExists( AUXILIARY_DICTIONARY )
         nAuxHandle := FOpen( AUXILIARY_DICTIONARY, FO_READWRITE + FO_DENYWRITE )
      ELSE
         nAuxHandle := FCreate( AUXILIARY_DICTIONARY )
      ENDIF
      IF nAuxHandle >= 0

         FSeek( nAuxHandle, 0, FS_END )                 // Bottom of the file
         nWritten := FWrite( nAuxHandle, cWord + CRLF ) // Write word into file
         FClose( nAuxHandle )                           // Close the file
         Sp_Cache( cWord )                              // Add word to cache

         was_added := ( nWritten == hb_BLen( cWord ) + hb_BLen( CRLF ) )
      ENDIF
   ENDIF

   RETURN was_added

//  Function:  Sp_cache()
//   Purpose:  To add a word to the cache list
//    Syntax:  <logical> := Sp_Cache( cWord )
// Arguments:  cWord - upper case, all trimmed word to add
//   Returns:  <logical> - TRUE if added,
//                         FALSE otherwise
//
//    Static:  CACHE_WORDS - String of cache words
//
//     Notes:  Check to see if the word already exists in the cache

FUNCTION Sp_Cache( cWord )

   LOCAL cTemp  := "|" + Upper( AllTrim( cWord ) ) + "|"
   LOCAL lAdded := .F.

   IF ! cTemp $ CACHE_WORDS .AND. Len( CACHE_WORDS ) < MAX_STRING
      CACHE_WORDS += cTemp
      lAdded      := .T.
   ENDIF

   RETURN lAdded

//  Function:  Sp_Check()
//   Purpose:  To check the spelling of a word
//    Syntax:  <logical> := Sp_Check( cWord )
// Arguments:  cWord - upper case, all trimmed word to check
//   Returns:  <logical> - TRUE if valid spelling
//                         FALSE otherwise
//
//    Static:  CACHE_WORDS  - String of cache words
//             COMMON_WORDS - String of common words
//             t_cOffsets   -
//             t_nHandle    - Handle DIC file is opened on

FUNCTION Sp_Check( cWord )

   THREAD STATIC t_cBuf   := ""
   THREAD STATIC t_cLast

   THREAD STATIC t_nDicCount   := 0
   THREAD STATIC t_nCacheCount := 0
   THREAD STATIC t_nBuffCount  := 0

   LOCAL ok      := .T.
   LOCAL cLookup := Upper( RTrim( cWord ) )
   LOCAL nRow
   LOCAL nCol
   LOCAL x
   LOCAL y
   LOCAL z := 4
   LOCAL cTemp

   IF Sp_Init()
      IF Len( cLookup ) == 1 .AND. cLookup $ "IA"
         RETURN .T.
      ENDIF
      IF Len( cLookup ) < MINIMUM_WORD_LENGTH
         RETURN .T.
      ENDIF
      IF Right( cLookup, 2 ) == "'S"
         cLookUp := SubStr( cLookup, 1, Len( cLookup ) -2 )
      ENDIF
      cTemp := "|" + cLookup + "|"
      IF At( cTemp, COMMON_WORDS ) == 0    // Check the common words first
         IF At( cTemp, CACHE_WORDS ) == 0   // then check the cache words
            ok    := .F.
            nRow  := Asc( SubStr( cLookup, 1, 1 ) ) - 64
            nCol  := Asc( SubStr( cLookup, 2, 1 ) ) - 64
            IF ( nRow > 0 .AND. nRow <= 26 ) .AND. ( nCol > 0 .AND. nCol <= 26 )

               x  := Bin2L( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 1 ), 4 ) )
               y  := Bin2W( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 5 ), 2 ) )

               IF ! Empty( x )
                  IF !( t_cLast == SubStr( cLookup, 1, 2 ) )
                     t_cBuf := Space( y )
                     FSeek( t_nHandle, x, FS_SET )
                     FRead( t_nHandle, @t_cBuf, y )
                     t_nDicCount++
                  ELSE
                     t_nBuffCount++
                  ENDIF
                  IF Len( cLookup ) == 3
                     z  := Asc( SubStr( cLookup, 3, 1 ) ) - 64
                     ok := bit( @t_cBuf, z )
                  ELSEIF Len( cLookup ) < 3
                     ok := bit( @t_cBuf, 27 )
                  ELSEIF y > 4
                     cTemp := xForm( cLookup )
                     DO WHILE z < y
                        z := bfat( cTemp, t_cBuf, z )
                        IF z < 6
                           EXIT
                        ELSEIF hb_BSubStr( t_cBuf, z - 1, 1 ) < hb_BChar( 128 )
                           z++
                        ELSE
                           EXIT
                        ENDIF
                     ENDDO
                     ok := z > 4 .AND. z < y
                  ENDIF
                  t_cLast := SubStr( cLookup, 1, 2 )
               ENDIF
            ENDIF
         ELSE
            t_nCacheCount++
         ENDIF
      ELSE
         t_nCacheCount++
      ENDIF
   ELSE
      ok := .F.
   ENDIF
   IF ! ok .AND. EXTRA_CODE_BLOCK != NIL
      ok := Eval( EXTRA_CODE_BLOCK, cWord )
   ENDIF

   RETURN ok

//  Function:  Sp_GetBuf()
//   Purpose:  To get all words within the buffer
//    Syntax:  Sp_GetBuf( <cLetters> )
// Arguments:  <cLetters>   - First two letters
//   Returns:  cString      - Buffer string from DIC file

STATIC FUNCTION sp_GetBuf( cLetters )

   LOCAL x
   LOCAL y
   LOCAL cBuf  := ""
   LOCAL nRow  := Asc( SubStr( cLetters, 1, 1 ) ) - 64
   LOCAL nCol  := Asc( SubStr( cLetters, 2, 1 ) ) - 64

   IF ( nRow > 0 .AND. nRow <= 26 ) .AND. ( nCol > 0 .AND. nCol <= 26 )
      x  := Bin2L( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 1 ), 4 ) )
      IF ! Empty( x )
         y    := Bin2W( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 5 ), 2 ) )
         cBuf := Space( y )
         FSeek( t_nHandle, x + 4, FS_SET )
         FRead( t_nHandle, @cBuf, y - 4 )
      ENDIF
   ENDIF

   RETURN cBuf

//  Function:  Sp_Clear()
//   Purpose:  To clear out the cache list
//    Syntax:  Sp_Clear()
//   Returns:  NIL
//
//    Static:  CACHE_WORDS  - String of cache words

FUNCTION Sp_Clear()

   CACHE_WORDS := ""

   RETURN NIL

//   Function: Sp_GetSet()
//    Purpose: To get/set a parameter for the spell check function
//     Syntax: <xOldValue> := Sp_GetSet( nWhich [, xNewSetting] )
// Parameters: nWhich      - Which parameter to get/set
//             xNewSetting - Value to set parameter to
//
//    Returns: <logical>  TRUE if word succesfully added
//                        FALSE if an error occurs, usually network lock
//                        problem

FUNCTION Sp_GetSet( nWhich, xNewSetting )

   LOCAL xOld := NIL

   IF nWhich != NIL .AND. ( nWhich > 0 .AND. nWhich <= Len( t_aGlobal ) )
      xOld := t_aGlobal[ nWhich ]
      IF ValType( xNewSetting ) == ValType( xOld )
         t_aGlobal[ nWhich ] := xNewSetting
      ENDIF
   ENDIF

   RETURN xOld

// Function: Sp_LoadAux()
//  Purpose: To load an auxiliary dictionary of words

FUNCTION Sp_LoadAux( cFile )

   LOCAL is_ok := .F.
   LOCAL x     := 0
   LOCAL cStr  := ""
   LOCAL nSize

   IF ! Empty( cFile )
      IF hb_FileExists( cFile )
         x := FOpen( cFile, FO_READ + FO_DENYNONE )
      ELSE
         x := FCreate( cFile )
      ENDIF
      Sp_GetSet( 5, cFile )

      IF x > 0
         nSize := FSeek( x, 0, FS_END )
         IF nSize < MAX_STRING
            cStr  := Space( nSize )
            FSeek( x, 0, FS_SET )
            FRead( x, @cStr, nSize )
            cStr  := "|" + StrTran( cStr, CRLF, "|" )
            CACHE_WORDS += Upper( cStr )
            is_ok := .T.
         ENDIF
         FClose( x )
      ENDIF
   ENDIF

   RETURN is_ok

//  Function:  Sp_Suggest()
//   Purpose:  To return an array of possible spellings
//    Syntax:  aSuggest_ := Sp_Suggest( cWord [, lInclude] )
// Arguments:  cWord     - Word to look for suggestions for
//             lInclude  - Should word be included in list?
//   Returns:  aSuggest_ - List of suggested words

FUNCTION Sp_Suggest( cWord, lInclude )

   STATIC sc_aParts_ := { ;
      {"A"    , { "AI", "AO", "AU", "AY", "EA", "EI", "EIGH", "ET", "EY", "E", "I", "O" } }, ;
      {"AIR"  , { "ARE" } }, ;
      {"AIT"  , { "ATE" } }, ;
      {"C"    , { "CK" } }, ;
      {"CH"   , { "TCH", "TI", "TU" } }, ;
      {"CKS"  , { "X" } }, ;
      {"D"    , { "ED" } }, ;
      {"E"    , { "A", "AE", "AI", "AY", "EA", "EI", "EO", "IE", "I", "U", "O" } }, ;
      {"EM"   , { "TEM" } }, ;
      {"ER"   , { "OUR", "RE", "URE", "YR" } }, ;
      {"ERE"  , { "EIR", "EAR", "IER" } }, ;
      {"F"    , { "GH", "LF", "PH", "G" } }, ;
      {"FIZ"  , { "PHYS" } }, ;
      {"G"    , { "GH", "GU", "GUE" } }, ;
      {"GZ"   , { "X" } }, ;
      {"H"    , { "WH" } }, ;
      {"I"    , { "A", "E", "EE", "IA", "IE", "O", "U", "UI", "Y", "YE", "UY", "EI", "IGH" } }, ;
      {"IS"   , { "US", "ACE", "ICE" } }, ;
      {"ISE"  , { "IZE" } }, ;
      {"J"    , { "D", "DG", "DI", "DJ", "G", "GG" } }, ;
      {"K"    , { "C", "CC", "CH", "CK", "CQU", "CU", "LK", "Q", "QU", "QUE" } }, ;
      {"KW"   , { "QU" } }, ;
      {"L"    , { "SL" } }, ;
      {"LE"   , { "TLE", "AL" } }, ;
      {"M"    , { "CHM", "GM", "LM", "MB", "MN", "N" } }, ;
      {"N"    , { "M", "PN", "GN", "KN", "MN" } }, ;
      {"NG"   , { "N", "NGUE" } }, ;
      {"O"    , { "AU", "EAU", "EO", "EW", "OA", "OE", "OH", "OU", "OUGH", "OW", "A", "AH", "AW", "UO", "E" } }, ;
      {"OE"   , { "OWE", "OUGH" } }, ;
      {"OO"   , { "O", "EU", "EW", "OE", "OU", "OUGH", "U", "UE", "UI" } }, ;
      {"OU"   , { "OUGH", "OW" } }, ;
      {"PER"  , { "PRO", "PRI", "PRA", "PRU" } }, ;
      {"PRE"  , { "PRO", "PRI", "PRA", "PRU" } }, ;
      {"R"    , { "RH", "WR" } }, ;
      {"S"    , { "C", "CE", "PS", "SC", "SCH" } }, ;
      {"SH"   , { "CE", "CH", "CI", "S", "SCH", "SCI", "SE", "SI", "SS", "SSI", "TI" } }, ;
      {"SI"   , { "PSY", "CY" } }, ;
      {"T"    , { "ED", "GHT", "PT", "TH" } }, ;
      {"TION" , { "SION", "CION", "CEAN", "CIAN" } }, ;
      {"TIOUS", { "SEOUS" } }, ;
      {"TURE" , { "TEUR" } }, ;
      {"U"    , { "O", "OE", "OO", "OU", "EUA", "EU", "EUE", "EW", "IEU", "IEW", "UE", "UI", "YOU", "YU" } }, ;
      {"UR"   , { "EAR", "ER", "IR", "YR" } }, ;
      {"V"    , { "F", "LV", "PH" } }, ;
      {"W"    , { "O", "U", "WH" } }, ;
      {"X"    , { "CKS", "GZ", "K" } }, ;
      {"Y"    , { "I", "J", "IE", "EI" } }, ;
      {"Z"    , { "S", "SC", "SS", "X", "GE", "SI", "ZI" } }, ;
      {"ZI"   , { "XY"} } }

   STATIC sc_aEnds := { "ED", "ER", "ING", "LY", "AL", "FUL", "NESS", "MENT", "IVE" }
   STATIC sc_aBegs := { "UN", "IN", "DIS", "MIS", "EN", "WEL", "AL" }

   LOCAL jj, kk, zz, ii
   LOCAL cHold
   LOCAL aRet_   := {}                // List of suggested words
   LOCAL cTemp
   LOCAL nSugg
   LOCAL nSize    := Len( sc_aParts_ )
   LOCAL nSuffix  := Len( sc_aEnds )
   LOCAL cMeta
   LOCAL cFirst
   LOCAL cKey
   LOCAL arr_

   cWord := Upper( RTrim( cWord ) )
   zz    := Len( cWord )

   IF zz == 1                         // Don't offer suggestions for
      RETURN aRet_                    // single letter words
   ENDIF

   IF lInclude == NIL
      lInclude := .F.
   ENDIF

   //
   // Should the current word be included?
   //

   IF Sp_Check( cWord )
      AAdd( aRet_, "A0AA" + cWord )
   ENDIF

   IF "'" $ cWord
      cHold := Sp_Expand( cWord )
      IF ! Empty( cHold )
         AAdd( aRet_, "A1AA" + cHold )
      ENDIF
   ENDIF

   IF CHECK_RUNONS
      arr_ := Sp_Split( cWord )
      FOR jj := 1 TO Len( arr_ )
         AAdd( aRet_, "A1AA" + arr_[ jj ] )
      NEXT
   ENDIF

   IF SUGGEST_PREFERENCE $ "AB"

      //
      // Step One - Do letter doubling
      //

      FOR jj := 2 TO zz
         IF SubStr( cWord, jj, 1 ) $ "BCDEFGKLMNOPRSTZ"

            cHold := Left( cWord, jj ) + SubStr( cWord, jj, 1 ) + ;
               SubStr( cWord, jj + 1 )

            //
            // If the word is not already in the list, then check
            // to see if it is a valid word.
            //

            IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
               AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
            ENDIF
         ENDIF
      NEXT

      //
      // Step Two - Remove extra letters
      //

      FOR jj := 1 TO zz
         cHold := Left( cWord, jj - 1 ) + SubStr( cWord, jj + 1 )
         //
         // If the word is not already in the list, then check
         // to see if it is a valid word.
         //

         IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
            AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
         ENDIF
      NEXT

      //
      // Step Three - Transpose the letters
      //

      FOR jj := 2 TO zz
         cHold := Left( cWord, jj - 2 ) + SubStr( cWord, jj, 1 ) + ;
            SubStr( cWord, jj - 1, 1 ) + SubStr( cWord, jj + 1 )
         IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
            AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
         ENDIF
      NEXT

      //
      // Step Four - Try adding a silent E to the end
      //

      cHold := cWord + "E"
      IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
         AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
      ENDIF

      //
      // Step Five - Do sound alike substitutions
      //

      FOR jj := 1 TO nSize
         IF sc_aParts_[ jj, 1 ] $ cWord
            IF sc_aParts_[ jj, 1 ] $ "AEIOUT"
               ii := 0
               DO WHILE ( ii := fat( sc_aParts_[ jj, 1 ], cWord, ii ) ) > 0
                  FOR kk := 1 TO Len( sc_aParts_[ jj, 2 ] )
                     cHold := SubStr( cWord, 1, ii - 1 ) + sc_aParts_[ jj, 2, kk ] + SubStr( cWord, ii + 1 )
                     IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
                        AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
                     ENDIF
                  NEXT
                  ii++
               ENDDO
            ELSE
               FOR kk := 1 TO Len( sc_aParts_[ jj, 2 ] )
                  cHold := StrTran( cWord, sc_aParts_[ jj, 1 ], sc_aParts_[ jj, 2, kk ] )
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
                     AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
      nSugg := Len( aRet_ )

      //
      // At this point, we have a list of words in aRet_, which now need to
      // be checked for suffixes, prefixes, and plurals.
      //

      FOR jj := 1 TO nSugg
         cHold := RTrim( SubStr( aRet_[ jj ], 5 ) )    // Extract the word
         zz    := Len( cHold )
         //
         // Check suffixes
         //
         //
         IF ADD_SUFFIXES
            FOR kk := 1 TO nSuffix
               cTemp := cHold + sc_aEnds[ kk ]
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            NEXT
            //
            // Try doubling the last letter
            //

            FOR kk := 1 TO nSuffix
               cTemp := cHold + SubStr( cHold, zz, 1 ) + sc_aEnds[ kk ]
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            NEXT
            //
            // Accomodate words ending in C
            //

            IF SubStr( cHold, -1, 1 ) == "C"
               FOR kk := 1 TO nSuffix
                  cTemp := cHold + "K" + sc_aEnds[ kk ]
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
               NEXT
            ENDIF
            //
            // Accomodate words ending in ND
            //

            IF SubStr( cHold, -2, 2 ) == "ND"
               cTemp := SubStr( cHold, 1, zz - 1 ) + "SE"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
               FOR kk := 1 TO nSuffix
                  cTemp := SubStr( cHold, 1, zz - 1 ) + "SE" + sc_aEnds[ kk ]
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
                  cTemp := SubStr( cHold, 1, zz - 1 ) + "S" + sc_aEnds[ kk ]
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
               NEXT
            ENDIF
            //
            // Words ending in E, remove the E and try
            //

            IF SubStr( cHold, zz, 1 ) == "E"
               FOR kk := 1 TO nSuffix
                  cTemp := SubStr( cHold, 1, zz - 1 ) + sc_aEnds[ kk ]
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
               NEXT
            ENDIF
         ENDIF

         IF ADD_PREFIXES
            FOR kk := 1 TO 7
               cTemp := sc_aBegs[ kk ] + cHold
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            NEXT
         ENDIF

         IF ADD_PLURALS
            cTemp := cHold + "S"
            IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
               Sp_Check( cTemp )
               AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
            ENDIF

            IF SubStr( cHold, zz, 1 ) == "Y"
               cTemp := SubStr( cHold, 1, zz - 1 ) + "IES"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            ELSEIF SubStr( cHold, zz, 1 ) == "F"
               cTemp := SubStr( cHold, 1, zz - 1 ) + "VES"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            ELSEIF SubStr( cHold, zz, 1 ) == "O"
               cTemp := cHold + "ES"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
   ENDIF

   //
   // Check metaphone matches, only if SUGGEST_PREFERENCE is metafone or both,
   // because this search can slow things down a bit
   //

   IF SUGGEST_PREFERENCE $ "MB"
      IF METAPHONE_SIZE == 0
         zz    := Min( 5, Len( cWord ) )
         IF zz < 3
            zz := 3
         ENDIF
      ELSE
         zz  := METAPHONE_SIZE
      ENDIF
      cTemp  := Sp_GetBuf( cWord )
      ii     := Len( cTemp )
      cMeta  := C_MetaFone( cWord, zz )
      zz     := Len( cMeta )
      cFirst := SubStr( cWord, 1, 2 )

      IF ii > 0

         IF SubStr( cMeta, 2, 1 ) >= "N"

            kk := ii - 5
            jj := ii - 4

            DO WHILE kk > 0
               IF Asc( SubStr( cTemp, kk, 1 ) ) >= 128      // End of word
                  cHold := cFirst + xUnForm( SubStr( cTemp, kk + 1, jj - kk ) )
                  cKey  := C_Metafone( cHold, zz )
                  IF cMeta ==  C_Metafone( cHold, zz )
                     IF MAX_DIFFERENCE < 0 .OR. Abs( Len( cWord ) -Len( cHold ) ) <= MAX_DIFFERENCE
                        IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0
                           AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
                        ENDIF
                     ENDIF
                  ELSEIF cKey < cMeta
                     EXIT
                  ENDIF
                  jj := kk
               ENDIF
               kk--
            ENDDO

         ELSE
            kk := 1
            jj := 1

            DO WHILE kk < ii
               IF Asc( SubStr( cTemp, kk, 1 ) ) >= 128      // End of word
                  cHold := cFirst + xUnForm( SubStr( cTemp, jj, kk - jj + 1 ) )
                  cKey  := C_Metafone( cHold, zz )
                  IF cMeta ==  C_Metafone( cHold, zz )
                     IF MAX_DIFFERENCE < 0 .OR. Abs( Len( cWord ) -Len( cHold ) ) <= MAX_DIFFERENCE
                        IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0
                           AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
                        ENDIF
                     ENDIF
                  ELSEIF cKey > cMeta
                     EXIT
                  ENDIF
                  jj := kk + 1
               ENDIF
               kk++
            ENDDO
         ENDIF
      ENDIF
   ENDIF

   nSugg := Len( aRet_ )
   IF nSugg > 1 .AND. SORT_SUGGESTIONS
      ASort( aRet_ )
   ENDIF

   FOR kk := 1 TO nSugg
      aRet_[ kk ] := SubStr( aRet_[ kk ], 5 )
   NEXT

   RETURN aRet_

//  Function:  Sp_Quick()
//   Purpose:  To return an array of quick spellings
//    Syntax:  aSuggest  := Sp_Quick( cWord )
// Arguments:  cWord     - Word to look for suggestions for
//   Returns:  aSuggest  - List of suggested words

FUNCTION Sp_Quick( cWord )

   STATIC sc_aTryThese := { ;
      "AI$AO$AU$AY$EA$EI$EIGH$ET$EY$E$O$", ;
      "A$AE$AI$AY$EA$EI$EO$IE$U$O$", ;
      "A$E$EE$IA$IE$O$U$UI$Y$YE$UY$EI$IGH$", ;
      "AU$EAU$EO$EW$OA$OE$OH$OU$OUGH$OW$A$AH$AW$UO$E$", ;
      "O$OE$OO$OU$EUA$EU$EUE$EW$IEU$IEW$UE$UI$YOU$YU$", ;
      "ED$GHT$PT$TH$" }

   LOCAL ii := 0
   LOCAL nOld, jj, kk
   LOCAL zz
   LOCAL arr_ := {}
   LOCAL cHold, ll, cTemp

   cWord := Upper( RTrim( cWord ) )

   zz := Len( cWord )

   IF zz < 3                          // Don't offer suggestions for
      RETURN {}                       // one or two letter words
   ENDIF

   //
   // Step One - Do letter doubling
   //

   FOR jj := 2 TO zz
      IF SubStr( cWord, jj, 1 ) $ "BCDEFGKLMNOPRSTZ"
         cHold := Left( cWord, jj ) + SubStr( cWord, jj, 1 ) + ;
            SubStr( cWord, jj + 1 )
         //
         // If the word is not already in the list, then check
         // to see if it is a valid word.
         //

         IF AScan( arr_, cHold ) == 0 .AND. Sp_Check( cHold )
            AAdd( arr_, cHold )
         ENDIF
      ENDIF
   NEXT

   //
   // Step Two - Remove extra letters
   //

   FOR jj := 1 TO zz
      cHold := Left( cWord, jj - 1 ) + SubStr( cWord, jj + 1 )
      //
      // If the word is not already in the list, then check
      // to see if it is a valid word.
      //

      IF AScan( arr_, cHold ) == 0 .AND. Sp_Check( cHold )
         AAdd( arr_, cHold )
      ENDIF
   NEXT

   //
   // Step Three - Transpose the letters
   //

   FOR jj := 2 TO zz
      cHold := Left( cWord, jj - 2 ) + SubStr( cWord, jj, 1 ) + ;
         SubStr( cWord, jj - 1, 1 ) + SubStr( cWord, jj + 1 )
      IF AScan( arr_, cHold ) == 0 .AND. Sp_Check( cHold )
         AAdd( arr_, cHold )
      ENDIF
   NEXT

   //
   // Step Four - Try adding a silent E to the end
   //

   cHold := cWord + "E"
   IF AScan( arr_, cHold ) == 0 .AND. Sp_Check( cHold )
      AAdd( arr_, cHold )
   ENDIF

   //
   // Step Five - Do sound alike substitutions
   //

   FOR jj := 1 TO 6
      IF SubStr( "AEIOUT", jj, 1 ) $ cWord
         ii   := fat( SubStr( "AEIOUT", jj, 1 ), cWord, ii )
         nold := 1
         DO WHILE ii > 0
            FOR kk := 1 TO ChrCount( "$", sc_aTryThese[ jj ] )
               ll    := fat( "$", sc_aTryThese[ jj ], nOld )
               cTemp := SubStr( sc_aTryThese[ jj ], nOld, ll - nOld )
               nOld  := ll + 1
               cHold := SubStr( cWord, 1, ii - 1 ) + cTemp + SubStr( cWord, ii + 1 )
               IF AScan( arr_, cHold ) == 0 .AND. Sp_Check( cHold )
                  AAdd( arr_, cHold )
               ENDIF
            NEXT
            ii++
            ii := fat( SubStr( "AEIOUT", jj, 1 ), cWord, ii )
         ENDDO
      ENDIF
   NEXT

   RETURN arr_

STATIC FUNCTION ChrCount( cChar, cString )
   RETURN Len( cString ) - Len( StrTran( cString, cChar ) )

FUNCTION Sp_Split( cWord )

   LOCAL arr_ := {}
   LOCAL x
   LOCAL cWord1
   LOCAL cWord2

   FOR x := 2 TO Len( cWord )
      cWord1 := Left( cWord, x - 1 )
      cWord2 := SubStr( cWord, x )
      IF Len( cWord1 ) > 1 .AND. Len( cWord2 ) > 1
         IF Sp_Check( cWord1 ) .AND.  Sp_Check( cWord2 )
            AAdd( arr_, cWord1 + " " + cWord2 )
         ENDIF
      ENDIF
   NEXT

   RETURN arr_

FUNCTION Sp_Expand( cWord )

   LOCAL x
   LOCAL cExpand

   cWord := Upper( AllTrim( cWord ) )
   x := AScan( sc_aContracts, {| jj | Left( jj[ 1 ], Len( cWord ) ) == cWord } ) // LEFTEQUAL()
   IF x > 0
      cExpand := sc_aContracts[ x, 2 ]
   ENDIF

   RETURN cExpand

//  Function:  Sp_WildCard()
//   Purpose:  To return an array of wildcard matches
//    Syntax:  aSuggest  := Sp_WildCard( cPattern )
// Arguments:  cPattern  - Pattern to match using * or ?'s
//   Returns:  aSuggest  - List of matching words

FUNCTION Sp_WildCard( cPattern )

   LOCAL cTemp
   LOCAL ii, kk, jj, cFirst, cHold, x, z
   LOCAL arr_   := {}
   LOCAL nStart, nEnd

   cPattern := Upper( cPattern )

   IF Sp_Init()
      x := At( "*", cPattern )
      IF x == 0
         x := At( "?", cPattern )
      ENDIF
      IF x == 1                 // Can't handle wildcards in first position
         RETURN arr_
      ENDIF
      IF x == 2
         nStart := 1
         nEnd   := 26
      ELSE
         nStart := Asc( SubStr( cPattern, 2, 1 ) ) - 64
         nEnd   := nStart
      ENDIF


      FOR z := nStart TO nEnd
         cTemp  := Sp_GetBuf( SubStr( cPattern, 1, 1 ) + Chr( z + 64 ) )
         ii     := Len( cTemp )
         cFirst := SubStr( cPattern, 1, 1 ) + Chr( z + 64 )

         IF ii > 0
            kk := 1
            jj := 1
            DO WHILE kk < ii
               IF Asc( SubStr( cTemp, kk, 1 ) ) >= 128      // End of word
                  cHold := cFirst + xUnForm( SubStr( cTemp, jj, kk - jj + 1 ) )
                  IF WildCard( cPattern, cHold )
                     AAdd( arr_, cHold )
                  ENDIF
                  jj    := kk + 1
               ENDIF
               kk++
            ENDDO
         ENDIF
      NEXT
   ENDIF

   RETURN arr_

// The following functions are internal and should not be modified.

// Function:  Sp_Init()
//  Purpose:  Internal function to initialize the dictionary
//  Returns:  <logical> - Was dictionary initialized?

FUNCTION Sp_Init()

   LOCAL isok := .T.
   LOCAL cBuf
   LOCAL i
   LOCAL j
   LOCAL nOther := 0
   LOCAL cOther := ""
   LOCAL nFileSize

   IF t_cOffsets == NIL
      isok := .F.
      t_nHandle := FOpen( DICTIONARY_PATH + DICTIONARY_NAME, FO_READ + FO_DENYWRITE )
      IF t_nHandle != F_ERROR

         cBuf := Space( NSIZE + 6 )
         FRead( t_nHandle, @cBuf, NSIZE + 6 )

         IF SubStr( cBuf, 1, 2 ) == "JJ"
            nOther := Bin2L( SubStr( cBuf, 3, 4 ) )
            t_cOffsets := hb_BSubStr( cBuf, 7 )
            nFileSize := FSeek( t_nHandle, 0, FS_END )
            IF nFileSize - nOther > 0
               cOther := Space( nFileSize - nOther )
               FSeek( t_nHandle, nOther, FS_SET )
               FRead( t_nHandle, @cOther, ( nFileSize - nOther ) )
            ENDIF
            t_aGlobal[ 2 ] += cOther
            isok := .T.
         ENDIF
         IF ! Empty( AUXILIARY_DICTIONARY )
            Sp_LoadAux( AUXILIARY_DICTIONARY )
         ENDIF
      ENDIF
      IF t_aGlobal[ 1 ] == NIL
         Sp_Common()
      ENDIF

      // Thesaurus comented out as not needed
#if 0
      IF ! Empty( THESAURUS_NAME )
         IF hb_FileExists( DICTIONARY_PATH + THESAURUS_NAME )
            Sp_OpenThes( DICTIONARY_PATH + THESAURUS_NAME )
         ELSEIF hb_FileExists( THESAURUS_NAME )
            Sp_OpenThes( THESAURUS_NAME )
         ENDIF
      ENDIF
#endif

   ENDIF

   RETURN isok


//  Function:  DBF2Dic()
//   Purpose:  To create a DIC file from a DBF list of words
//    Syntax:  nStatus := DBF2Dic( <cDBF_file>, <cDIC_file> )
// Arguments:  <cDBF_File>  - DBF containing sorted list of upper
//                            case words. The field name must be
//                            WORD and of type character
//             <cDIC_File>  - Name of DIC file to create, assumes
//                            'dict.dic'
//   Returns:  nStatus      -  0 if successful
//                            -1 DBF file does not exist
//                            -2 Field WORD does not exist in DBF
//                            >0 File error occurred, see FERROR()

FUNCTION DBF2Dic( cDbf, cDictionary, lTalk )

   LOCAL nStatus := 0                 // Status code
   LOCAL i                            // Loop counters
   LOCAL j
   LOCAL x
   LOCAL nH                           // File handle
   LOCAL nWhere
   LOCAL temp
   LOCAL cBits
   LOCAL cSave
   LOCAL nSize
   LOCAL cOther := ""
   LOCAL nCurRec := 0

   //
   // DEFAULT name for dictionary file
   //

   IF cDictionary == NIL
      cDictionary := "dict.dic"
   ENDIF

   IF lTalk == NIL
      lTalk := .F.
   ENDIF

   //
   // See if the DBF file exists
   //

   IF ! "." $ cDBF
      cDBF += ".dbf"
   ENDIF

   IF ! hb_FileExists( cDBF )
      RETURN -1
   ENDIF

   USE ( cDbf ) ALIAS DICT NEW
   IF FieldPos( "WORD" ) == 0
      USE
      RETURN -2
   ENDIF

   IF lTalk

      cSave := SaveScreen( 8, 30, 11, 48 )
      hb_DispBox( 8, 30, 11, 48,, "W+/R" )
      hb_DispOutAt(  8, 33, " Creating DIC ", "W+/R" )
      hb_DispOutAt(  9, 31, " Indexing words  ", "W/R" )
      hb_DispOutAt( 10, 31, "                 ", "W/R" )

      nSize := DICT->( LastRec() )
   ENDIF

   INDEX ON SubStr( DICT->word, 1, 2 ) + PadR( C_MetaFone( AllTrim( DICT->word ), 5 ), 6 ) TO ( "$$temp" )
   dbGoTop()

   IF lTalk
      hb_DispOutAt(  9, 31, "Percent          ", "W/R" )
      hb_DispOutAt( 10, 31, "Complete:        ", "W/R" )
   ENDIF

   //
   // Create the dictionary file
   //

   // ADDED - 02/08/96 - JAMES
   IF t_nHandle != F_ERROR                // Is dictionary already open?
      FClose( t_nHandle )                 // Yes, close it
      t_nHandle := F_ERROR
   ENDIF                                  // END OF ADDITION

   nH := FCreate( cDictionary )

   IF nH != F_ERROR
      //
      // Write out enough bytes to hold the index information
      //

      FWrite( nH, "JJ" + L2Bin( NSIZE + 4 ) + Replicate( hb_BChar( 0 ), NSIZE ) + Space( 10 ) )

      FOR i := 1 TO 26
         DO WHILE Left( DICT->word, 1 ) == Chr( i + 64 ) .AND. ! Eof()
            FOR j := 1 TO 26
               temp  := ""
               cBits := FOUR_BYTES
               DO WHILE ! IsAlpha( SubStr( DICT->word, 2, 1 ) ) .AND. ! Eof()
                  IF Len( AllTrim( DICT->word ) ) > 0
                     cOther += "|" + AllTrim( DICT->word )
                  ENDIF
                  dbSkip()
               ENDDO
               DO WHILE SubStr( DICT->word, 2, 1 ) == Chr( j + 64 ) .AND. ! Eof()
                  IF Len( RTrim( DICT->word ) ) == 3
                     bit( @cBits, Asc( SubStr( DICT->word, 3, 1 ) ) - 64, .T. )
                  ELSEIF Len( RTrim( DICT->word ) ) == 2
                     bit( @cBits, 27, .T. )
                  ELSE
                     temp += xForm( RTrim( DICT->word ) )
                  ENDIF
                  dbSkip()
                  IF lTalk
                     nCurRec++
                     hb_DispOutAt( 10, 41, Transform( ( nCurRec / nSize ) * 100, "999.9%" ), "W+/R" )
                  ENDIF
               ENDDO
               IF ! Empty( temp ) .OR. !( cBits == FOUR_BYTES )

                  nWhere := FSeek( nH, 0, FS_END )

                  FSeek( nH, ( ( i - 1 ) * 26 * EACH_WORD ) + ( ( j - 1 ) * EACH_WORD ) + 6 )
                  FWrite( nH, L2Bin( nWhere ) + I2Bin( hb_BLen( temp ) + 4 ), EACH_WORD )
                  FSeek( nH, 0, FS_END )
                  FWrite( nH, cBits + temp )
               ENDIF
            NEXT
         ENDDO
      NEXT
      j := FSeek( nH, 0, FS_END )
      FSeek( nH, 2, FS_SET )
      FWrite( nH, L2Bin( j ) )
      IF ! Empty( cOther )
         FSeek( nH, j, FS_SET )
         cOther += "|"
         FWrite( nH, cOther )
      ENDIF
      FClose( nH )
   ELSE
      nStatus := nH
   ENDIF

   IF lTalk
      RestScreen( 8, 30, 11, 48, cSave )
   ENDIF

   SELECT DICT
   USE
   FErase( "$$temp" + IndexExt() )

   RETURN nStatus

//  Function:  Dic2DBF()
//   Purpose:  To create a DBF file from a DIC file
//    Syntax:  nStatus := Dic2DBF( <cDIC_file>, <cDBF_file> )
// Arguments:  <cDIC_File>  - Name of DIC file to read, assumes
//                            dict.dic
//             <cDBF_File>  - Name of DBF to create, assumes dict.dbf
//                            It will contain a single field called
//                            WORD of type character
//   Returns:  nStatus      -  0 if successful
//                            -1 DIC file does not exist
//                            -2 DIC file is not a valid dictionary

FUNCTION Dic2DBF( cDictionary, cDBF, lTalk )

   LOCAL nStatus := 0                 // Status code
   LOCAL i                            // Loop counters
   LOCAL j
   LOCAL x, y, z
   LOCAL temp
   LOCAL cBuf
   LOCAL cWord
   LOCAL cSave
   LOCAL nSize

   //
   // DEFAULT name for dictionary file
   //

   IF cDictionary == NIL
      cDictionary := "dict.dic"
   ENDIF

   IF lTalk == NIL
      lTalk := .F.
   ENDIF

   IF ! hb_FileExists( cDictionary )
      RETURN -1
   ENDIF

   DICTIONARY_NAME := cDictionary

   //
   // Read the dictionary file
   //

   IF ! Sp_Init()
      RETURN -2
   ENDIF

   //
   // See if the DBF file exists
   //

   IF ! "." $ cDBF
      cDBF += ".dbf"
   ENDIF

   dbCreate( cDbf, { { "WORD", "C", 25, 0 } } )

   USE ( cDbf ) ALIAS DICT NEW

   IF lTalk
      cSave := SaveScreen( 8, 30, 12, 48 )
      hb_DispBox( 8, 30, 12, 48,, "W+/R" )
      hb_DispOutAt(  8, 34, " Creating DBF ", "W+/R" )
      hb_DispOutAt(  9, 31, "Percent          ", "W/R" )
      hb_DispOutAt( 10, 31, "Complete:        ", "W/R" )
      hb_DispOutAt( 11, 31, "  Record:        ", "W/R" )

      temp  := Directory( cDictionary )
      nSize := temp[ 1, 2 ]

   ENDIF

   j := 2
   FOR i := 2 TO Len( CACHE_WORDS )
      IF SubStr( CACHE_WORDS, i, 1 ) == "|"
         dbAppend()
         DICT->word := SubStr( CACHE_WORDS, j, i - j )
         j := i + 1
      ENDIF
   NEXT

   FOR i := 1 TO 26
      FOR j := 1 TO 26
         temp := Chr( i + 64 ) + Chr( j + 64 )
         x  := Bin2L( hb_BSubStr( t_cOffsets, ( ( i - 1 ) * 156 ) + ( ( j - 1 ) * EACH_WORD + 1 ), 4 ) )
         IF ! Empty( x )
            y := Bin2W( hb_BSubStr( t_cOffsets, ( ( i - 1 ) * 156 ) + ( ( j - 1 ) * EACH_WORD + 5 ), 2 ) )

            IF lTalk
               hb_DispOutAt( 10, 43, Transform( ( x / nSize ) * 100, "999%"   ), "W+/R" )
               hb_DispOutAt( 11, 41, Transform( LastRec()          , "99,999" ), "W+/R" )
            ENDIF

            cBuf := Space( y )
            FSeek( t_nHandle, x, FS_SET )
            FRead( t_nHandle, @cBuf, y )
            FOR z := 1 TO 26
               IF bit( @cBuf, z )
                  dbAppend()
                  DICT->word := temp + Chr( z + 64 )
               ENDIF
            NEXT
            IF bit( @cBuf, 27 )
               dbAppend()
               DICT->word := temp
            ENDIF
            cBuf := SubStr( cBuf, 5 )
            z    := 1
            DO WHILE ! Empty( cBuf )
               IF SubStr( cBuf, z, 1 ) >= hb_BChar( 128 )
                  cWord := SubStr( cBuf, 1, z )
                  dbAppend()
                  DICT->word := temp + xUnForm( cWord )
                  cWord := ""
                  cBuf  := SubStr( cBuf, z + 1 )
                  z     := 1

                  IF lTalk
                     hb_DispOutAt( 11, 41, Transform( LastRec(), "99,999" ), "W+/R" )
                  ENDIF
               ELSE
                  z++
               ENDIF
            ENDDO
            IF ! Empty( cWord )
            ENDIF
            cWord := ""
         ENDIF
      NEXT
   NEXT

   IF lTalk
      RestScreen( 8, 30, 12, 48, cSave )
   ENDIF

   SELECT DICT
   USE

   RETURN nStatus

//  Function:  Sp_Common()
//   Purpose:  Loads the COMMON word static array element
//    Syntax:  Sp_Common()
//   Returns:  NIL
//
//     Notes:  The common word list represents some of the most commonly
//             used English words.  They are stored in RAM to prevent a
//             dictionary lookup for 70-80% of the words.

STATIC FUNCTION Sp_Common()

   t_aGlobal[ 1 ] := ;
      "|THE|OF|AND|TO|A|IN|THAT|FOR|IS|WAS|IT|HE|I|AS|WITH|ON|HIS|BE|AT|BY" + ;
      "|NOT|THIS|HAD|HAVE|YOU|BUT|FROM|ARE|OR|WHICH|AN|THEY|WILL|ONE|WERE|ALL" + ;
      "|WE|HER|SHE|WOULD|THERE|HAS|BEEN|HIM|THEIR|IF|WHEN|SO|MORE|NO|WHO|YOUR" + ;
      "|OUT|MY|WHAT|UP|CAN|THEM|ABOUT|INTO|ITS|TIME|SOME|THAN|ME|OTHER|ONLY|NEW" + ;
      "|COULD|SAID|ANY|THESE|MAY|TWO|THEN|DO|FIRST|NOW|MAN|SUCH|VERY|LIKE|OVER" + ;
      "|OUR|EVEN|MOST|AFTER|MADE|ALSO|DID|MANY|SHOULD|BEFORE|MUST|THROUGH|YEARS" + ;
      "|WHERE|MUCH|WAY|WELL|DOWN|GOOD|BECAUSE|HERE|EACH|THOSE|PEOPLE|STATE|HOW" + ;
      "|TOO|LITTLE|WORLD|LAST|WORK|STILL|BACK|OWN|SEE|MEN|LONG|GET|JUST|GO|HOUSE" + ;
      "|BETWEEN|BOTH|LIFE|BEING|UNDER|YEAR|NEVER|DAY|SAME|ANOTHER|KNOW|WHILE|MIGHT" + ;
      "|US|GREAT|OLD|OFF|COME|AGAINST|SINCE|CAME|RIGHT|TAKE|USED|THREE|SMALL|MAKE" + ;
      "|STATES|HIMSELF|FEW|DURING|WITHOUT|AGAIN|PLACE|AROUND|HOWEVER|HOME|FOUND" + ;
      "|SAY|WHOSE|THOUGHT|NIGHT|DEAR|ONCE|WENT|GENERAL|HIGH|UPON|SCHOOL|EVERY|PART" + ;
      "|DON'T|DOES|GOT|UNITED|LEFT|NUMBER|COURSE|WAR|GOING|UNTIL|ALWAYS|AWAY" + ;
      "|SOMETHING|FACT|THOUGH|WATER|LESS|PUBLIC|PUT|THINK|ALMOST|HAND|ENOUGH|FAR" + ;
      "|TOOK|YET|GOVERNMENT|SYSTEM|BETTER|EYES|SET|TOLD|NOTHING|PRESIDENT|END" + ;
      "|HEAD|WHY|CALLED|DIDN'T|FIND|LOOK|ASKED|LATER|POINT|NEXT|PROGRAM|KNEW|CITY" + ;
      "|BUSINESS|GIVE|GROUP|TOWARD|YOUNG|DAYS|LET|ROOM|WITHIN|CHILDREN|SIDE|SOCIAL" + ;
      "|GIVEN|ORDER|PRESENT|SEVERAL|NATIONAL|USE|RATHER|SECOND|FACE|PER|POSSIBLE" + ;
      "|AMONG|FORM|IMPORTANT|OFTEN|THINGS|LOOKED|EARLY|WHITE|BECOME|CASE|SEND" + ;
      "|BIG|LARGE|NEED|FOUR|FELT|ALONG|GOD|SAW|BEST|CHURCH|EVER|LEAST|POWER" + ;
      "|DEVELOPMENT|LIGHT|THING|FAMILY|INTEREST|SEEMED|WANT|TODAY|MEMBERS|MIND" + ;
      "|COUNTRY|AREA|OTHERS|TURNED|ALTHOUGH|DONE|OPEN|SERVICE|CERTAIN|KIND|PROBLEM" + ;
      "|BEGAN|DIFFERENT|DOOR|THUS|HELP|MEANS|SENSE|WHOLE|MATTER|PERHAPS|ITSELF" + ;
      "|IT'S|TIMES|HUMAN|LAW|LINE|ABOVE|NAME|EXAMPLE|COMPANY|HANDS|LOCAL|SHOW" + ;
      "|BODY|FIVE|HISTORY|WHETHER|GAVE|EITHER|ACROSS|ACT|ACTION|FEET|ANYTHING" + ;
      "|PAST|QUITE|TAKEN|HAVING|SEEN|DEATH|EXPERIENCE|HALF|REALLY|WEEK|WORDS" + ;
      "|CAR|FIELD|MONEY|WORD|ALREADY|THEMSELVES|INFORMATION|I'M|TELL|CLOSE" + ;
      "|COLLEGE|PERIOD|SHALL|TOGETHER|HELD|KEEP|SURE|PROBABLY|FREE|REAL|BEHIND" + ;
      "|SEEMS|CANNOT|POLITICAL|AIR|QUESTION|OFFICE|WOMAN|BROUGHT|MAJOR|MOTHER" + ;
      "|SPECIAL|HEARD|PROBLEMS|AGO|BECAME|AVAILABLE|FEDERAL|MOMENT|STUDY" + ;
      "|KNOWN|RESULT|STREET|BOY|ECONOMIC|CHANGE|POSITION|REASON|SOUTH|BOARD" + ;
      "|INDIVIDUAL|JOB|SOCIETY|AREAS|WEST|SPACE|TURN|LOVE|COMMUNITY|TOWN|TRUE" + ;
      "|COURT|FORCE|FULL|SEEM|AGE|AM|COST|WIFE|FUTURE|VOICE|WANTED|CAN'T|DEPARTMENT" + ;
      "|USE|CENTER|COMMON|CONTROL|NECESSARY|POLICY|FOLLOWING|FRONTSOMETIMES|FATHER" + ;
      "|GIRL|SIX|WOMEN|CLEAR|MILITARY|FURTHER|ABLE|FIGURE|LAND|FEEL|MUSIC|PARTY" + ;
      "|PROVIDE|CENTURY|EDUCATION|UNIVERSITY|CHILD|EFFECT|STUDENTS|RUN|SHORT" + ;
      "|STOOD|MORNING|TOTAL|OUTSIDE|ART|RATE|SAYS|YOU'RE|CLASS|TYPE|EVIDENCE" + ;
      "|EXCEPT|LEAVE|MILLION|MISS|NORTH|PLAN|SOUND|THEREFORE|TOP|USUALLY|BLACK" + ;
      "|HARD|SCHOOLS|STRONG|BELIEVE|PLAY|VARIOUS|SURFACE|MAKING|MEAN|SOON|VALUE" + ;
      "|LINES|MODERN|NEAR|TABLE|PRIVATE|RED|ROAD|TAX|ALONE|MINUTES|PERSONAL|PROCESS" + ;
      "|SITUATION|THAT'S|GONE|IDEA|INCREASE|NOR|PEACE|SECTION|LIVING|STARTED|BOOK" + ;
      "|LONGER|CUT|FINALLY|NATURE|SECRETARY|MONTHS|THIRD|CALL|COMPLETE|GREATER" + ;
      "|EXPECTED|FIRE|NEEDED|KEPT|VALUES|VIEW|BASIS|DARK|EVERYTHING|PRESSURE" + ;
      "|GROUND|EAST|RECENT|REQUIRED|SPIRIT|UNION|HOPE|I'LL|MOVED|NATIONS|WROTE" + ;
      "|CONDITIONS|RETURN|SUPPORT|ATTENTION|LATE|PARTICULAR|BROWN|COSTS|ELSE|NATION" + ;
      "|BEYOND|COULDN'T|FORCES|HOURS|NUCLEAR|PERSON|TAKING|COMING|DEAD|INSIDE|LOW" + ;
      "|MATERIAL|REPORT|STAGE|ADDED|AMOUNT|BASIC|DATA|FEELING|FOLLOWED|HEART|INSTEAD" + ;
      "|LOOKING|LOST|MILES|PAY|SINGLE|COLD|HUNDRED|INCLUDING|INDUSTRY|MOVE|RESEARCH" + ;
      "|SHOWN|BRIDGE|DEVELOPED|SIMPLY|TRIED|HOLD|REACHED|SHARE|SORT|COMMITTEE|DEFENSE" + ;
      "|EQUIPMENT|ISLAND|SENDING|ACTUALLY|EARTH|BEGINNING|RELIGIOUS|RIVER|TEN|CENTRAL" + ;
      "|DOING|GETTING|LETTER|RECEIVED|REST|TERMS|TRYING|CARE|FRIENDS|INDEED|MEDICAL" + ;
      "|PICTURE|DIFFICULT|FINE|SIMPLE|STATUS|SUBJECT|BUILDING|ESPECIALLY|HIGHER|RANGE" + ;
      "|WALL|BRING|MEETING|WALKED|CENT|FLOOR|FOREIGN|NEEDS|PAPER|PASSED|SIMILAR" + ;
      "|FINAL|NATURAL|PROPERTY|TRAINING|COUNTY|GROWTH|HOT|INTERNATIONAL|LIVE" + ;
      "|MARKET|POLICE|START|TALK|WASN'T|WRITTEN|BOOKS|ANSWER|CONGRESS|HEAR|PRIMARY" + ;
      "|STORY|SUDDENLY|CONSIDERED|COUNTRIES|HALL|ISSUE|LIKELY|PARTICULARLY|TRULY" + ;
      "|WORKING|EFFORT|SAT|ENTIRE|HAPPENED|LABOR|PURPOSE|RESULTS|CASES|DIFFERENCE" + ;
      "|HAIR|PRODUCTION|STAND|I'VE|I'D|WON'T|"

   RETURN NIL

FUNCTION WildCard( cPattern, cString )

   LOCAL lMatch := .F.
   LOCAL x      := At( "*", cPattern )
   LOCAL cBefore
   LOCAL cAfter
   LOCAL y
   LOCAL nStrSize, nPatSize

   cString := Upper( AllTrim( cString ) )

   IF cPattern == "*"
      RETURN .T.
   ENDIF
   //
   // Do a * match
   //
   IF x > 0
      cBefore := Upper( SubStr( cPattern, 1, x - 1 ) )
      cAfter  := Upper( SubStr( cPattern, x + 1 ) )
      DO CASE
      CASE Empty( cBefore )
         lMatch := Right( cString, Len( cAfter ) ) == cAfter
      CASE Empty( cAfter )
         lMatch := Left( cString, Len( cBefore ) ) == cBefore
      OTHERWISE
         lMatch := ( Left( cString, Len( cBefore ) ) == cBefore ) .AND. ;
            Right( cString, Len( cAfter ) ) == cAfter
      ENDCASE
   ELSE
      x := At( "?", cPattern )
      IF x > 0
         nPatSize := Len( cPattern )
         nStrSize := Len( cString )
         IF nPatSize == nStrSize
            lMatch := .T.
            FOR y := 1 TO nPatSize
               IF !( SubStr( cPattern, y, 1 ) == "?" ) .AND. ;
                  !( SubStr( cPattern, y, 1 ) == SubStr( cString, y, 1 ) )
                  lMatch := .F.
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ELSE
         lMatch := Left( Upper( cPattern ), Len( Upper( cString ) ) ) == Upper( cString )
      ENDIF
   ENDIF

   RETURN lMatch

FUNCTION aWords( cLine )

   LOCAL aWords_ := {}
   LOCAL nSize   := Len( RTrim( cLine ) )
   LOCAL x, y, z
   LOCAL cWord   := ""
   LOCAL nOffset

   z := 0
   DO WHILE z <= nSize
      z++
      y := Asc( SubStr( cLine, z, 1 ) )
      IF y >= 48 .AND. ! Chr( y ) $ ":;<=>?@[\^]_`{|}~"
         nOffset := z
         cWord   := Chr( y )
         z++
         y := Asc( SubStr( cLine, z, 1 ) )
         WHILE ( y >= 48 .AND. ! Chr( y ) $ ":;<=>?@[\^]_`{|}~" ) .OR. y == "'"
            cWord += Chr( y )
            z++
            IF z > nSize
               EXIT
            ENDIF
            y := Asc( SubStr( cLine, z, 1 ) )
         ENDDO
         AAdd( aWords_, { cWord, nOffset } )
      ENDIF
   ENDDO

   RETURN aWords_

// Find an occurrence of 'f_str' in 'l_str' starting from position 'f_rom'
FUNCTION fat( f_str, l_str, f_rom )

   IF PCount() < 3                        // Is f_rom passed?
      f_rom := 1
   ENDIF

   RETURN At( f_str, SubStr( l_str, f_rom ) )

STATIC FUNCTION bfat( f_str, l_str, f_rom )

   IF PCount() < 3                        // Is f_rom passed?
      f_rom := 1
   ENDIF

   RETURN hb_BAt( f_str, hb_BSubStr( l_str, f_rom ) )
