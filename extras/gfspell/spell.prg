// Copyright (C) 1993, Joseph D. Booth, All Rights Reserved
//
// Library of functions to allow a Harbour program to
// spell check a string and to also work with individual
// words.
//
// Sp_Add()        - Add word to the dictionary
// Sp_Cache()      - Add word to spelling cache
// Sp_Check()      - Is word spelled correctly?
// Sp_Clear()      - Clear the spelling cache
// Sp_Init()       - Initialize the spelling dictionary
// Sp_GetSet()     - Get/Set global parameters
// Sp_LoadAux()    - Loads an auxiliary dictionary
// Sp_Suggest()    - Offer list of sound alike suggestions
// Sp_Quick()      - Offer list of suggested spellings
// Sp_WildCard()   - List of wildcard matches
//
// DBF2Dic()       - Convert a DBF file to a DIC file
// Dic2DBF()       - Convert a DIC file to a DBF file
//

#include "spell.ch"

#include "fileio.ch"

#define EACH_WORD               6
#define NSIZE                   ( 26 * 26 * EACH_WORD )
#define FOUR_BYTES              ( hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) )
#define MAX_STRING              512000

#define T_COMMON_WORDS          t_aGlobal[ COMMON_WORDS         ]
#define T_CACHE_WORDS           t_aGlobal[ CACHE_WORDS          ]
#define T_DICTIONARY_PATH       t_aGlobal[ DICTIONARY_PATH      ]
#define T_DICTIONARY_NAME       t_aGlobal[ DICTIONARY_NAME      ]
#define T_AUXILIARY_DICTIONARY  t_aGlobal[ AUXILIARY_DICTIONARY ]
#define T_EXTRA_CODE_BLOCK      t_aGlobal[ EXTRA_CODE_BLOCK     ]
#define T_ADD_SUFFIXES          t_aGlobal[ ADD_SUFFIXES         ]
#define T_ADD_PREFIXES          t_aGlobal[ ADD_PREFIXES         ]
#define T_ADD_PLURALS           t_aGlobal[ ADD_PLURALS          ]
#define T_SORT_SUGGESTIONS      t_aGlobal[ SORT_SUGGESTIONS     ]
#define T_SUGGEST_PREFERENCE    t_aGlobal[ SUGGEST_PREFERENCE   ]
#define T_MINIMUM_WORD_LENGTH   t_aGlobal[ MINIMUM_WORD_LENGTH  ]
#define T_METAPHONE_SIZE        t_aGlobal[ METAPHONE_SIZE       ]
#define T_MAX_DIFFERENCE        t_aGlobal[ MAX_DIFFERENCE       ]
#define T_THESAURUS_NAME        t_aGlobal[ THESAURUS_NAME       ]
#define T_CHECK_RUNONS          t_aGlobal[ CHECK_FOR_RUNONS     ]

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


THREAD STATIC t_hFile
THREAD STATIC t_cOffsets


//   Purpose:  Adds a word to the dictionary
//    Syntax:  <logical> := Sp_Add( cWord )
// Arguments:  cWord     - Word to add to the auxiliary dictionary
//   Returns:  <logical> - .T. if added,
//                         .F. otherwise
//
//     Notes:  Does not check to see if the word already exists in the
//             dictionary.

FUNCTION Sp_Add( cWord )

   LOCAL was_added := .F.             // Was word written to dictionary?
   LOCAL hFileAux                     // Dictionary file handle
   LOCAL nWritten                     // Number of bytes written

   // Initialize the spell checker and make sure
   // an auxiliary dictionary name was specified

   IF Sp_Init() .AND. ! Empty( T_AUXILIARY_DICTIONARY )

      cWord := Upper( AllTrim( cWord ) )

      // If the auxiliary dictionary does not exist,
      // we will create it for the user

      IF ( hFileAux := hb_vfOpen( T_AUXILIARY_DICTIONARY, FO_CREAT + FO_WRITE + FO_DENYWRITE ) ) != NIL

         hb_vfSeek( hFileAux, 0, FS_END )                      // Bottom of the file
         nWritten := hb_vfWrite( hFileAux, cWord + hb_eol() )  // Write word into file
         hb_vfClose( hFileAux )                                // Close the file
         Sp_Cache( cWord )                                     // Add word to cache

         was_added := ( nWritten == hb_BLen( cWord ) + hb_BLen( hb_eol() ) )
      ENDIF
   ENDIF

   RETURN was_added

//   Purpose:  To add a word to the cache list
//    Syntax:  <logical> := Sp_Cache( cWord )
// Arguments:  cWord - upper case, all trimmed word to add
//   Returns:  <logical> - .T. if added,
//                         .F. otherwise
//
//    Static:  T_CACHE_WORDS - String of cache words
//
//     Notes:  Check to see if the word already exists in the cache

FUNCTION Sp_Cache( cWord )

   LOCAL cTemp := "|" + Upper( AllTrim( cWord ) ) + "|"

   IF ! cTemp $ T_CACHE_WORDS .AND. Len( T_CACHE_WORDS ) < MAX_STRING
      T_CACHE_WORDS += cTemp
      RETURN .T.
   ENDIF

   RETURN .F.

//   Purpose:  To check the spelling of a word
//    Syntax:  <logical> := Sp_Check( cWord )
// Arguments:  cWord - upper case, all trimmed word to check
//   Returns:  <logical> - .T. if valid spelling
//                         .F. otherwise
//
//    Static:  T_CACHE_WORDS  - String of cache words
//             T_COMMON_WORDS - String of common words
//             t_cOffsets     -
//             t_hFile        - Handle DIC file is opened on

FUNCTION Sp_Check( cWord )

   THREAD STATIC t_cBuf := ""
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
      IF Len( cLookup ) < T_MINIMUM_WORD_LENGTH
         RETURN .T.
      ENDIF
      IF Right( cLookup, 2 ) == "'S"
         cLookUp := hb_StrShrink( cLookup, 2 )
      ENDIF
      cTemp := "|" + cLookup + "|"
      IF ! cTemp $ T_COMMON_WORDS    // Check the common words first
         IF ! cTemp $ T_CACHE_WORDS  // then check the cache words
            ok   := .F.
            nRow := Asc( SubStr( cLookup, 1, 1 ) ) - 64
            nCol := Asc( SubStr( cLookup, 2, 1 ) ) - 64
            IF ( nRow > 0 .AND. nRow <= 26 ) .AND. ( nCol > 0 .AND. nCol <= 26 )

               x := Bin2L( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 1 ), 4 ) )
               y := Bin2W( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 5 ), 2 ) )

               IF x != 0
                  IF !( t_cLast == Left( cLookup, 2 ) )
                     t_cBuf := Space( y )
                     hb_vfSeek( t_hFile, x, FS_SET )
                     hb_vfRead( t_hFile, @t_cBuf, y )
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
                     cTemp := XForm( cLookup )
                     DO WHILE z < y
                        IF ( z := hb_BAt( cTemp, t_cBuf, z ) ) < 6
                           EXIT
                        ELSEIF hb_BSubStr( t_cBuf, z - 1, 1 ) < hb_BChar( 128 )
                           z++
                        ELSE
                           EXIT
                        ENDIF
                     ENDDO
                     ok := z > 4 .AND. z < y
                  ENDIF
                  t_cLast := Left( cLookup, 2 )
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
   IF ! ok .AND. HB_ISEVALITEM( T_EXTRA_CODE_BLOCK )
      ok := Eval( T_EXTRA_CODE_BLOCK, cWord )
   ENDIF

   RETURN ok

//   Purpose:  To get all words within the buffer
// Arguments:  <cLetters>   - First two letters
//   Returns:  cString      - Buffer string from DIC file

STATIC FUNCTION Sp_GetBuf( cLetters )

   LOCAL x
   LOCAL y
   LOCAL cBuf := ""
   LOCAL nRow := Asc( SubStr( cLetters, 1, 1 ) ) - 64
   LOCAL nCol := Asc( SubStr( cLetters, 2, 1 ) ) - 64

   IF nRow > 0 .AND. nRow <= 26 .AND. ;
      nCol > 0 .AND. nCol <= 26
      x := Bin2L( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 1 ), 4 ) )
      IF x != 0
         y := Bin2W( hb_BSubStr( t_cOffsets, ( ( nRow - 1 ) * 156 ) + ( ( nCol - 1 ) * EACH_WORD + 5 ), 2 ) )
         cBuf := Space( y )
         hb_vfSeek( t_hFile, x + 4, FS_SET )
         hb_vfRead( t_hFile, @cBuf, y - 4 )
      ENDIF
   ENDIF

   RETURN cBuf

//   Purpose:  To clear out the cache list
//
//    Static:  T_CACHE_WORDS - String of cache words

PROCEDURE Sp_Clear()

   T_CACHE_WORDS := ""

   RETURN

//   Purpose:  To get/set a parameter for the spell check function
//    Syntax:  <xOldValue> := Sp_GetSet( nWhich [, xNewSetting] )
// Arguments:  nWhich      - Which parameter to get/set
//             xNewSetting - Value to set parameter to
//
//   Returns:  <logical>  .T. if word succesfully added
//                        .F. if an error occurs, usually network lock
//                        problem

FUNCTION Sp_GetSet( nWhich, xNewSetting )

   LOCAL xOld

   IF HB_ISNUMERIC( nWhich ) .AND. nWhich >= 1 .AND. nWhich <= Len( t_aGlobal )
      xOld := t_aGlobal[ nWhich ]
      IF ValType( xNewSetting ) == ValType( xOld )
         t_aGlobal[ nWhich ] := xNewSetting
      ENDIF
   ENDIF

   RETURN xOld

// Purpose: To load an auxiliary dictionary of words

FUNCTION Sp_LoadAux( cFile )

   LOCAL is_ok := .F.
   LOCAL hFile
   LOCAL cStr  := ""
   LOCAL nSize

   IF ! Empty( cFile )

      Sp_GetSet( AUXILIARY_DICTIONARY, cFile )

      IF ( hFile := hb_vfOpen( cFile, FO_CREAT + FO_READ + FO_DENYNONE ) ) != NIL
         nSize := hb_vfSize( hFile )
         IF nSize < MAX_STRING
            cStr := Space( nSize )
            hb_vfSeek( hFile, 0, FS_SET )
            hb_vfRead( hFile, @cStr, nSize )
            T_CACHE_WORDS += Upper( "|" + StrTran( StrTran( cStr, Chr( 13 ) ), Chr( 10 ), "|" ) )
            is_ok := .T.
         ENDIF
         hb_vfClose( hFile )
      ENDIF
   ENDIF

   RETURN is_ok

//   Purpose:  To return an array of possible spellings
//    Syntax:  aSuggest_ := Sp_Suggest( cWord )
// Arguments:  cWord     - Word to look for suggestions for
//   Returns:  aSuggest_ - List of suggested words

FUNCTION Sp_Suggest( cWord )

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
   LOCAL aRet_ := {}                  // List of suggested words
   LOCAL cTemp
   LOCAL cMeta
   LOCAL cFirst
   LOCAL cKey

   cWord := Upper( RTrim( cWord ) )
   zz    := Len( cWord )

   IF zz == 1                         // Don't offer suggestions for
      RETURN aRet_                    // single letter words
   ENDIF

   // Should the current word be included?

   IF Sp_Check( cWord )
      AAdd( aRet_, "A0AA" + cWord )
   ENDIF

   IF "'" $ cWord
      IF ! Empty( cHold := Sp_Expand( cWord ) )
         AAdd( aRet_, "A1AA" + cHold )
      ENDIF
   ENDIF

   IF T_CHECK_RUNONS
      FOR EACH jj IN Sp_Split( cWord )
         AAdd( aRet_, "A1AA" + jj )
      NEXT
   ENDIF

   IF T_SUGGEST_PREFERENCE $ "AB"

      // Step One - Do letter doubling

      FOR jj := 2 TO zz
         IF SubStr( cWord, jj, 1 ) $ "BCDEFGKLMNOPRSTZ"

            cHold := ;
               Left( cWord, jj ) + ;
               SubStr( cWord, jj, 1 ) + ;
               SubStr( cWord, jj + 1 )

            // If the word is not already in the list, then check
            // to see if it is a valid word.

            IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
               AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
            ENDIF
         ENDIF
      NEXT

      // Step Two - Remove extra letters

      FOR jj := 1 TO zz
         cHold := Stuff( cWord, jj, 1, "" )

         // If the word is not already in the list, then check
         // to see if it is a valid word.

         IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
            AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
         ENDIF
      NEXT

      // Step Three - Transpose the letters

      FOR jj := 2 TO zz
         cHold := ;
            Left( cWord, jj - 2 ) + ;
            SubStr( cWord, jj, 1 ) + ;
            SubStr( cWord, jj - 1, 1 ) + ;
            SubStr( cWord, jj + 1 )
         IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
            AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
         ENDIF
      NEXT

      // Step Four - Try adding a silent E to the end

      cHold := cWord + "E"
      IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
         AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
      ENDIF

      // Step Five - Do sound alike substitutions

      FOR EACH jj IN sc_aParts_
         IF jj[ 1 ] $ cWord
            IF jj[ 1 ] $ "AEIOUT"
               ii := 0
               DO WHILE ( ii := hb_At( jj[ 1 ], cWord, ii ) ) > 0
                  FOR EACH kk IN jj[ 2 ]
                     cHold := Stuff( cWord, ii, 1, kk )
                     IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
                        AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
                     ENDIF
                  NEXT
                  ii++
               ENDDO
            ELSE
               FOR EACH kk IN jj[ 2 ]
                  cHold := StrTran( cWord, jj[ 1 ], kk )
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cHold } ) == 0 .AND. Sp_Check( cHold )
                     AAdd( aRet_, "B" + Sp_Rate( cHold, cWord ) + cHold )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT

      // At this point, we have a list of words in aRet_, which now need to
      // be checked for suffixes, prefixes, and plurals.

      FOR EACH jj IN aRet_
         cHold := RTrim( SubStr( jj, 5 ) )    // Extract the word
         zz    := Len( cHold )

         // Check suffixes

         IF T_ADD_SUFFIXES
            FOR EACH kk IN sc_aEnds
               cTemp := cHold + kk
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            NEXT

            // Try doubling the last letter

            FOR EACH kk IN sc_aEnds
               cTemp := cHold + SubStr( cHold, zz, 1 ) + kk
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            NEXT

            // Accomodate words ending in C

            IF Right( cHold, 1 ) == "C"
               FOR EACH kk IN sc_aEnds
                  cTemp := cHold + "K" + kk
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
               NEXT
            ENDIF

            // Accomodate words ending in ND

            IF Right( cHold, 2 ) == "ND"
               cTemp := Left( cHold, zz - 1 ) + "SE"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
               FOR EACH kk IN sc_aEnds
                  cTemp := Left( cHold, zz - 1 ) + "SE" + kk
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
                  cTemp := Left( cHold, zz - 1 ) + "S" + kk
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
               NEXT
            ENDIF

            // Words ending in E, remove the E and try

            IF SubStr( cHold, zz, 1 ) == "E"
               FOR EACH kk IN sc_aEnds
                  cTemp := Left( cHold, zz - 1 ) + kk
                  IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                     Sp_Check( cTemp )
                     AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
                  ENDIF
               NEXT
            ENDIF
         ENDIF

         IF T_ADD_PREFIXES
            FOR EACH kk IN sc_aBegs
               cTemp := kk + cHold
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
            NEXT
         ENDIF

         IF T_ADD_PLURALS
            cTemp := cHold + "S"
            IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
               Sp_Check( cTemp )
               AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
            ENDIF

            SWITCH SubStr( cHold, zz, 1 )
            CASE "Y"
               cTemp := Left( cHold, zz - 1 ) + "IES"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
               EXIT
            CASE "F"
               cTemp := Left( cHold, zz - 1 ) + "VES"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
               EXIT
            CASE "O"
               cTemp := cHold + "ES"
               IF AScan( aRet_, {| xx | SubStr( xx, 5 ) == cTemp } ) == 0 .AND. ;
                  Sp_Check( cTemp )
                  AAdd( aRet_, "C" + Sp_Rate( cTemp, cWord ) + cTemp )
               ENDIF
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
   ENDIF

   // Check metaphone matches, only if T_SUGGEST_PREFERENCE is metafone or both,
   // because this search can slow things down a bit

   IF T_SUGGEST_PREFERENCE $ "MB"
      IF T_METAPHONE_SIZE == 0
         zz := Min( 5, Len( cWord ) )
         IF zz < 3
            zz := 3
         ENDIF
      ELSE
         zz := T_METAPHONE_SIZE
      ENDIF
      cTemp  := Sp_GetBuf( cWord )
      ii     := Len( cTemp )
      cMeta  := C_Metafone( cWord, zz )
      zz     := Len( cMeta )
      cFirst := Left( cWord, 2 )

      IF ii > 0

         IF SubStr( cMeta, 2, 1 ) >= "N"

            kk := ii - 5
            jj := ii - 4

            DO WHILE kk > 0
               IF Asc( SubStr( cTemp, kk, 1 ) ) >= 128      // End of word
                  cHold := cFirst + XUnForm( SubStr( cTemp, kk + 1, jj - kk ) )
                  cKey  := C_Metafone( cHold, zz )
                  IF cMeta ==  C_Metafone( cHold, zz )
                     IF T_MAX_DIFFERENCE < 0 .OR. Abs( Len( cWord ) - Len( cHold ) ) <= T_MAX_DIFFERENCE
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
                  cHold := cFirst + XUnForm( SubStr( cTemp, jj, kk - jj + 1 ) )
                  cKey  := C_Metafone( cHold, zz )
                  IF cMeta ==  C_Metafone( cHold, zz )
                     IF T_MAX_DIFFERENCE < 0 .OR. Abs( Len( cWord ) - Len( cHold ) ) <= T_MAX_DIFFERENCE
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

   IF Len( aRet_ ) > 1 .AND. T_SORT_SUGGESTIONS
      ASort( aRet_ )
   ENDIF

   FOR EACH kk IN aRet_
      kk := SubStr( kk, 5 )
   NEXT

   RETURN aRet_

//   Purpose:  To return an array of quick spellings
//    Syntax:  aSuggest := Sp_Quick( cWord )
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

   // Step One - Do letter doubling

   FOR jj := 2 TO zz
      IF SubStr( cWord, jj, 1 ) $ "BCDEFGKLMNOPRSTZ"
         cHold := ;
            Left( cWord, jj ) + ;
            SubStr( cWord, jj, 1 ) + ;
            SubStr( cWord, jj + 1 )

         // If the word is not already in the list, then check
         // to see if it is a valid word.

         IF hb_AScan( arr_, cHold,,, .T. ) == 0 .AND. Sp_Check( cHold )
            AAdd( arr_, cHold )
         ENDIF
      ENDIF
   NEXT

   // Step Two - Remove extra letters

   FOR jj := 1 TO zz
      cHold := Stuff( cWord, jj, 1, "" )

      // If the word is not already in the list, then check
      // to see if it is a valid word.

      IF hb_AScan( arr_, cHold,,, .T. ) == 0 .AND. Sp_Check( cHold )
         AAdd( arr_, cHold )
      ENDIF
   NEXT

   // Step Three - Transpose the letters

   FOR jj := 2 TO zz
      cHold := ;
         Left( cWord, jj - 2 ) + ;
         SubStr( cWord, jj, 1 ) + ;
         SubStr( cWord, jj - 1, 1 ) + ;
         SubStr( cWord, jj + 1 )
      IF hb_AScan( arr_, cHold,,, .T. ) == 0 .AND. Sp_Check( cHold )
         AAdd( arr_, cHold )
      ENDIF
   NEXT

   // Step Four - Try adding a silent E to the end

   cHold := cWord + "E"
   IF hb_AScan( arr_, cHold,,, .T. ) == 0 .AND. Sp_Check( cHold )
      AAdd( arr_, cHold )
   ENDIF

   // Step Five - Do sound alike substitutions

   FOR jj := 1 TO 6
      IF hb_BSubStr( "AEIOUT", jj, 1 ) $ cWord
         nold := 1
         DO WHILE ( ii := hb_At( hb_BSubStr( "AEIOUT", jj, 1 ), cWord, ii ) ) > 0
            FOR kk := 1 TO ChrCount( "$", sc_aTryThese[ jj ] )
               ll    := hb_At( "$", sc_aTryThese[ jj ], nOld )
               cTemp := SubStr( sc_aTryThese[ jj ], nOld, ll - nOld )
               nOld  := ll + 1
               cHold := Stuff( cWord, ii, 1, cTemp )
               IF hb_AScan( arr_, cHold,,, .T. ) == 0 .AND. Sp_Check( cHold )
                  AAdd( arr_, cHold )
               ENDIF
            NEXT
            ii++
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

      IF Len( cWord1 ) > 1 .AND. Len( cWord2 ) > 1 .AND. ;
         Sp_Check( cWord1 ) .AND. Sp_Check( cWord2 )

         AAdd( arr_, cWord1 + " " + cWord2 )
      ENDIF
   NEXT

   RETURN arr_

FUNCTION Sp_Expand( cWord )

   LOCAL x

   cWord := Upper( AllTrim( cWord ) )

   IF ( x := AScan( sc_aContracts, {| jj | hb_LeftEq( jj[ 1 ], cWord ) } ) ) > 0
      RETURN sc_aContracts[ x, 2 ]
   ENDIF

   RETURN NIL

//   Purpose:  To return an array of wildcard matches
//    Syntax:  aSuggest := Sp_WildCard( cPattern )
// Arguments:  cPattern  - Pattern to match using * or ?'s
//   Returns:  aSuggest  - List of matching words

FUNCTION Sp_WildCard( cPattern )

   LOCAL cTemp
   LOCAL ii, kk, jj, cFirst, cHold, x, z
   LOCAL arr_ := {}
   LOCAL nStart, nEnd

   cPattern := Upper( cPattern )

   IF Sp_Init()

      IF ( x := At( "*", cPattern ) ) == 0
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
         cTemp  := Sp_GetBuf( Left( cPattern, 1 ) + Chr( z + 64 ) )
         ii     := Len( cTemp )
         cFirst := Left( cPattern, 1 ) + Chr( z + 64 )

         IF ii > 0
            kk := 1
            jj := 1
            DO WHILE kk < ii
               IF Asc( SubStr( cTemp, kk, 1 ) ) >= 128      // End of word
                  cHold := cFirst + XUnForm( SubStr( cTemp, jj, kk - jj + 1 ) )
                  IF WildCard( cPattern, cHold )
                     AAdd( arr_, cHold )
                  ENDIF
                  jj := kk + 1
               ENDIF
               kk++
            ENDDO
         ENDIF
      NEXT
   ENDIF

   RETURN arr_

// The following functions are internal and should not be modified.

//  Purpose:  Internal function to initialize the dictionary
//  Returns:  <logical> - Was dictionary initialized?

FUNCTION Sp_Init()

   LOCAL isok := .T.
   LOCAL cBuf
   LOCAL nOther
   LOCAL cOther := ""
   LOCAL nFileSize

   IF t_cOffsets == NIL
      isok := .F.
      IF ( t_hFile := hb_vfOpen( T_DICTIONARY_PATH + T_DICTIONARY_NAME, FO_READ + FO_DENYWRITE ) ) != NIL

         cBuf := Space( NSIZE + 6 )
         hb_vfRead( t_hFile, @cBuf, NSIZE + 6 )

         IF hb_LeftEq( cBuf, "JJ" )
            nOther := Bin2L( SubStr( cBuf, 3, 4 ) )
            t_cOffsets := hb_BSubStr( cBuf, 7 )
            nFileSize := hb_vfSize( t_hFile )
            IF nFileSize - nOther > 0
               cOther := Space( nFileSize - nOther )
               hb_vfSeek( t_hFile, nOther, FS_SET )
               hb_vfRead( t_hFile, @cOther, nFileSize - nOther )
            ENDIF
            T_CACHE_WORDS += cOther
            isok := .T.
         ENDIF
         IF ! Empty( T_AUXILIARY_DICTIONARY )
            Sp_LoadAux( T_AUXILIARY_DICTIONARY )
         ENDIF
      ENDIF
      IF T_COMMON_WORDS == NIL
         Sp_Common()
      ENDIF

      // Thesaurus comented out as not needed
#if 0
      IF ! Empty( T_THESAURUS_NAME )
         DO CASE
         CASE hb_vfExists( T_DICTIONARY_PATH + T_THESAURUS_NAME )
            Sp_OpenThes( T_DICTIONARY_PATH + T_THESAURUS_NAME )
         CASE hb_vfExists( T_THESAURUS_NAME )
            Sp_OpenThes( T_THESAURUS_NAME )
         ENDCASE
      ENDIF
#endif
   ENDIF

   RETURN isok


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
//                            >0 File error occurred, see FError()

FUNCTION DBF2Dic( cDbf, cDictionary, lTalk )

   LOCAL nStatus := 0                 // Status code
   LOCAL i                            // Loop counters
   LOCAL j
   LOCAL hFile                        // File handle
   LOCAL nWhere
   LOCAL temp
   LOCAL cBits
   LOCAL cSave
   LOCAL nSize
   LOCAL cOther := ""
   LOCAL nCurRec := 0
   LOCAL cTempFile
   LOCAL hFileTemp

   hb_default( @cDictionary, "dict.dic" )
   hb_default( @lTalk, .F. )

   // See if the .dbf file exists

   cDBF := hb_FNameExtSetDef( cDBF, ".dbf" )

   IF ! hb_dbExists( cDBF )
      RETURN -1
   ENDIF

   USE ( cDbf ) ALIAS DICT NEW
   IF FieldPos( "WORD" ) == 0
      dbCloseArea()
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

   IF ( hFileTemp := hb_vfTempFile( @cTempFile,,, IndexExt() ) ) != NIL
      hb_vfClose( hFileTemp )
   ENDIF

   INDEX ON SubStr( field->word, 1, 2 ) + PadR( C_Metafone( AllTrim( field->word ), 5 ), 6 ) TO ( cTempFile )
   dbGoTop()

   IF lTalk
      hb_DispOutAt(  9, 31, "Percent          ", "W/R" )
      hb_DispOutAt( 10, 31, "Complete:        ", "W/R" )
   ENDIF

   // Create the dictionary file

   // ADDED - 1996-02-08 - JAMES
   IF t_hFile != NIL                    // Is dictionary already open?
      hb_vfClose( t_hFile )             // Yes, close it
      t_hFile := NIL
   ENDIF
   // END OF ADDITION

   IF ( hFile := hb_vfOpen( cDictionary, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL

      // Write out enough bytes to hold the index information

      hb_vfWrite( hFile, "JJ" + L2Bin( NSIZE + 4 ) + Replicate( hb_BChar( 0 ), NSIZE ) + Space( 10 ) )

      FOR i := 1 TO 26
         DO WHILE hb_LeftEq( DICT->word, Chr( i + 64 ) ) .AND. ! Eof()
            FOR j := 1 TO 26
               temp  := ""
               cBits := FOUR_BYTES
               DO WHILE ! IsAlpha( SubStr( DICT->word, 2, 1 ) ) .AND. ! Eof()
                  IF ! HB_ISNULL( AllTrim( DICT->word ) )
                     cOther += "|" + AllTrim( DICT->word )
                  ENDIF
                  dbSkip()
               ENDDO
               DO WHILE SubStr( DICT->word, 2, 1 ) == Chr( j + 64 ) .AND. ! Eof()
                  SWITCH Len( RTrim( DICT->word ) )
                  CASE 3
                     bit( @cBits, Asc( SubStr( DICT->word, 3, 1 ) ) - 64, .T. )
                     EXIT
                  CASE 2
                     bit( @cBits, 27, .T. )
                     EXIT
                  OTHERWISE
                     temp += XForm( RTrim( DICT->word ) )
                  ENDSWITCH
                  dbSkip()
                  IF lTalk
                     nCurRec++
                     hb_DispOutAt( 10, 41, Transform( ( nCurRec / nSize ) * 100, "999.9%" ), "W+/R" )
                  ENDIF
               ENDDO
               IF ! Empty( temp ) .OR. !( cBits == FOUR_BYTES )

                  nWhere := hb_vfSize( hFile )

                  hb_vfSeek( hFile, ( ( i - 1 ) * 26 * EACH_WORD ) + ( ( j - 1 ) * EACH_WORD ) + 6 )
                  hb_vfWrite( hFile, L2Bin( nWhere ) + I2Bin( hb_BLen( temp ) + 4 ), EACH_WORD )
                  hb_vfSeek( hFile, 0, FS_END )
                  hb_vfWrite( hFile, cBits + temp )
               ENDIF
            NEXT
         ENDDO
      NEXT
      j := hb_vfSize( hFile )
      hb_vfSeek( hFile, 2, FS_SET )
      hb_vfWrite( hFile, L2Bin( j ) )
      IF ! Empty( cOther )
         hb_vfSeek( hFile, j, FS_SET )
         cOther += "|"
         hb_vfWrite( hFile, cOther )
      ENDIF
      hb_vfClose( hFile )
   ELSE
      nStatus := hFile
   ENDIF

   IF lTalk
      RestScreen( 8, 30, 11, 48, cSave )
   ENDIF

   DICT->( dbCloseArea() )
   hb_vfErase( cTempFile )

   RETURN nStatus

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

   hb_default( @cDictionary, "dict.dic" )
   hb_default( @lTalk, .F. )

   IF ! hb_vfExists( cDictionary )
      RETURN -1
   ENDIF

   T_DICTIONARY_NAME := cDictionary

   // Read the dictionary file

   IF ! Sp_Init()
      RETURN -2
   ENDIF

   // See if the DBF file exists

   cDBF := hb_FNameExtSetDef( cDBF, ".dbf" )

   dbCreate( cDbf, { { "WORD", "C", 25, 0 } } )

   USE ( cDbf ) ALIAS DICT NEW

   IF lTalk
      cSave := SaveScreen( 8, 30, 12, 48 )
      hb_DispBox( 8, 30, 12, 48,, "W+/R" )
      hb_DispOutAt(  8, 34, " Creating DBF ", "W+/R" )
      hb_DispOutAt(  9, 31, "Percent          ", "W/R" )
      hb_DispOutAt( 10, 31, "Complete:        ", "W/R" )
      hb_DispOutAt( 11, 31, "  Record:        ", "W/R" )

      nSize := hb_vfSize( t_hFile )
      hb_vfSeek( t_hFile, 0, FS_SET )
   ENDIF

   j := 2
   FOR i := 2 TO Len( T_CACHE_WORDS )
      IF SubStr( T_CACHE_WORDS, i, 1 ) == "|"
         dbAppend()
         DICT->word := SubStr( T_CACHE_WORDS, j, i - j )
         j := i + 1
      ENDIF
   NEXT

   FOR i := 1 TO 26
      FOR j := 1 TO 26
         temp := Chr( i + 64 ) + Chr( j + 64 )
         x := Bin2L( hb_BSubStr( t_cOffsets, ( ( i - 1 ) * 156 ) + ( ( j - 1 ) * EACH_WORD + 1 ), 4 ) )
         IF x != 0
            y := Bin2W( hb_BSubStr( t_cOffsets, ( ( i - 1 ) * 156 ) + ( ( j - 1 ) * EACH_WORD + 5 ), 2 ) )

            IF lTalk
               hb_DispOutAt( 10, 43, Transform( ( x / nSize ) * 100, "999%"   ), "W+/R" )
               hb_DispOutAt( 11, 41, Transform( LastRec()          , "99,999" ), "W+/R" )
            ENDIF

            cBuf := Space( y )
            hb_vfSeek( t_hFile, x, FS_SET )
            hb_vfRead( t_hFile, @cBuf, y )
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
                  cWord := Left( cBuf, z )
                  dbAppend()
                  DICT->word := temp + XUnForm( cWord )
                  cBuf  := SubStr( cBuf, z + 1 )
                  z     := 1

                  IF lTalk
                     hb_DispOutAt( 11, 41, Transform( LastRec(), "99,999" ), "W+/R" )
                  ENDIF
               ELSE
                  z++
               ENDIF
            ENDDO
         ENDIF
      NEXT
   NEXT

   IF lTalk
      RestScreen( 8, 30, 12, 48, cSave )
   ENDIF

   DICT->( dbCloseArea() )

   RETURN nStatus

//   Purpose:  Loads the COMMON word static array element
//
//     Notes:  The common word list represents some of the most commonly
//             used English words.  They are stored in RAM to prevent a
//             dictionary lookup for 70-80% of the words.

STATIC PROCEDURE Sp_Common()

   T_COMMON_WORDS := ;
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

   RETURN

FUNCTION WildCard( cPattern, cString )

   LOCAL lMatch
   LOCAL x
   LOCAL cBefore
   LOCAL cAfter
   LOCAL y
   LOCAL nPatSize

   IF cPattern == "*"
      RETURN .T.
   ENDIF

   lMatch := .F.

   cString := Upper( AllTrim( cString ) )

   // Do a * match

   DO CASE
   CASE ( x := At( "*", cPattern ) ) > 0

      cBefore := Upper( Left( cPattern, x - 1 ) )
      cAfter  := Upper( SubStr( cPattern, x + 1 ) )
      DO CASE
      CASE Empty( cBefore )
         lMatch := Right( cString, Len( cAfter ) ) == cAfter
      CASE Empty( cAfter )
         lMatch := hb_LeftEq( cString, cBefore )
      OTHERWISE
         lMatch := hb_LeftEq( cString, cBefore ) .AND. Right( cString, Len( cAfter ) ) == cAfter
      ENDCASE

   CASE At( "?", cPattern ) > 0

      nPatSize := Len( cPattern )
      IF nPatSize == Len( cString )
         lMatch := .T.
         FOR y := 1 TO nPatSize
            IF !( SubStr( cPattern, y, 1 ) == "?" ) .AND. ;
               !( SubStr( cPattern, y, 1 ) == SubStr( cString, y, 1 ) )
               lMatch := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF

   OTHERWISE
      lMatch := hb_LeftEqI( cPattern, cString )
   ENDCASE

   RETURN lMatch

FUNCTION AWords( cLine )

   LOCAL aWords_ := {}
   LOCAL nSize   := Len( RTrim( cLine ) )
   LOCAL y, z
   LOCAL cWord
   LOCAL nOffset

   z := 0
   DO WHILE z <= nSize
      y := Asc( SubStr( cLine, ++z, 1 ) )
      IF y >= Asc( "0" ) .AND. ! Chr( y ) $ ":;<=>?@[\^]_`{|}~" + Chr( 127 )
         nOffset := z
         cWord   := Chr( y )
         y := Asc( SubStr( cLine, ++z, 1 ) )
         DO WHILE ( y >= Asc( "0" ) .AND. ! Chr( y ) $ ":;<=>?@[\^]_`{|}~" + Chr( 127 ) ) .OR. y == "'"
            cWord += Chr( y )
            IF ++z > nSize
               EXIT
            ENDIF
            y := Asc( SubStr( cLine, z, 1 ) )
         ENDDO
         AAdd( aWords_, { cWord, nOffset } )
      ENDIF
   ENDDO

   RETURN aWords_
