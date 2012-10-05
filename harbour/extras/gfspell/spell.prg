/*
 * $Id$
 */

* Program...:  SpellLib.prg
* Author....:  Joseph D. Booth
* Copyright.:  (C)1993, Joseph D. Booth, All Rights Reserved
*
* Purpose...:  Library of functions to allow a Clipper program to
*              spell check a string and to also work with individual
*              words.
*
*              SP_Add()        - Add word to the dictionary
*              SP_Cache()      - Add word to spelling cache
*              SP_Check()      - Is word spelled correctly?
*              SP_Clear()      - Clear the spelling cache
*              SP_Init()       - Initialize the spelling dictionary
*              SP_GetSet()     - Get/Set global parameters
*              SP_LoadAux()    - Loads an auxiliary dictionary
*              SP_Suggest()    - Offer list of sound alike suggestions
*              SP_Quick()      - Offer list of suggested spellings
*              SP_WildCard()   - List of wildcard matches
*
*              DBF2Dic()       - Convert a DBF file to a DIC file
*              Dic2DBF()       - Convert a DIC file to a DBF file
*
****************************************************************************


#xtranslate    StoreWord(<cWord>)     => Xform(<cWord>)
#xtranslate    ExtractWord(<cWord>)   => Xunform(<cWord>)

#define    NSIZE                  26*26*6
#define    EACH_WORD                    6
#define    CRLF                   chr(13)+chr(10)
#define    FOUR_BYTES             chr(0)+chr(0)+chr(0)+chr(0)
#define    MAX_STRING             40000

#define    COMMON_WORDS           aGlobal[ 1]
#define    CACHE_WORDS            aGlobal[ 2]
#define    DICTIONARY_PATH        aGlobal[ 3]
#define    DICTIONARY_NAME        aGlobal[ 4]
#define    AUXILIARY_DICTIONARY   aGlobal[ 5]
#define    EXTRA_CODE_BLOCK       aGlobal[ 6]
#define    ADD_SUFFIXES           aGlobal[ 7]
#define    ADD_PREFIXES           aGlobal[ 8]
#define    ADD_PLURALS            aGlobal[ 9]
#define    SORT_SUGGESTIONS       aGlobal[10]
#define    SUGGEST_PREFERENCE     aGlobal[11]
#define    MINIMUM_WORD_LENGTH    aGlobal[12]
#define    METAPHONE_SIZE         aGlobal[13]
#define    MAX_DIFFERENCE         aGlobal[14]
#define    THESAURUS_NAME         aGlobal[15]
#define    CHECK_RUNONS           aGlobal[16]


* #define    REDUCE_MEMORY_NEEDS




THREAD STATIC aGlobal := { NIL,;
                        "",;
                        "",;
                        "DICT.DIC",;
                        "",;
                        NIL,;
                        .T.,;
                        .T.,;
                        .T.,;
                        .T.,;
                        "B",;     // (M)etaphone, (A)lgorithmic, (B)oth
                         2,;
                         5,;
                         1,;
                         "THES.DIC",;
                        .T. }

STATIC aContracts  :=  { { "CAN'T"       ,"CANNOT"    },;
                         { "WON'T"       ,"WILL NOT"  },;
                         { "AREN'T"      ,"ARE NOT"   },;
                         { "AIN'T"       ,"ARE NOT"   },;
                         { "THEY'RE"     ,"THEY ARE"  },;
                         { "THEY'VE"     ,"THEY HAVE" },;
                         { "IT'S"        ,"IT IS"     },;
                         { "I'LL"        ,"I WILL"    },;
                         { "I'D"         ,"I WOULD"   },;
                         { "DON'T"       ,"DO NOT"    } }



THREAD STATIC nHandle := 0
THREAD STATIC cOffsets



//   Function:  SP_Add()
//    Purpose:  Adds a word to the dictionary
//     Syntax:  <logical> := SP_Add(cWord)
//  Arguments:  cWord     - Word to add to the auxiliary dictionary
//    Returns:  <logical> - TRUE if added,
//                          FALSE otherwise
//
//      Notes:  Does not check to see if the word already exists in the
//              dictionary.
//
////////////////////////////
function SP_Add(cWord)
LOCAL was_added := .F.             // Was word written to dictionary?
LOCAL nAuxHandle                   // Dictionary file handle
LOCAL nWritten                     // Number of bytes written

// Initialize the spell checker and make sure
// an auxiliary dictionary name was specified

**DEBUG**
@24,30 SAY "At SP_ADD"

if SP_Init() .and. !empty( AUXILIARY_DICTIONARY )
   cWord      := upper(alltrim(cWord))

   //
   // If the auxiliary dictionary does not exist,
   // we will create it for the user
   //

   if file( AUXILIARY_DICTIONARY )
      nAuxHandle := fopen( AUXILIARY_DICTIONARY,2+32 )
   else
      nAuxHandle := fcreate( AUXILIARY_DICTIONARY )
   endif
   if nAuxHandle >= 0

      Fseek(nAuxHandle,0,2)                  // Bottom of the file
      nWritten := Fwrite(nAuxHandle,;        // Write word into file
                  cWord+CRLF,len(cWord)+2)

      Fclose(nAuxHandle)                     // Close the file
      SP_Cache(cWord)                        // Add word to cache

      was_added := (nWritten == len(cWord)+2)
   endif
endif
return was_added
*******************************************************************************

//
//   Function:  Sp_cache()
//    Purpose:  To add a word to the cache list
//     Syntax:  <logical> := Sp_cache(cWord)
//  Arguments:  cWord - upper case, all trimmed word to add
//    Returns:  <logical> - TRUE if added,
//                          FALSE otherwise
//
//     Static:  CACHE_WORDS - String of cache words
//
//      Notes:  Check to see if the word already exists in the cache
//
////////////////////////
function Sp_cache(cWord)

LOCAL cTemp  := "|"+upper(alltrim(cWord))+"|"
LOCAL lAdded := .F.

**DEBUG**
@24,30 SAY "At SP_cache"

if ! cTemp $ CACHE_WORDS .and. len(CACHE_WORDS) < MAX_STRING
   CACHE_WORDS += cTemp
   lAdded      := .T.
endif
return lAdded
*******************************************************************************

//
//   Function:  Sp_check()
//    Purpose:  To check the spelling of a word
//     Syntax:  <logical> := Sp_check(cWord)
//  Arguments:  cWord - upper case, all trimmed word to check
//    Returns:  <logical> - TRUE if valid spelling
//                          FALSE otherwise
//
//     Static:  CACHE_WORDS  - String of cache words
//              COMMON_WORDS - String of common words
//              cOffSets     -
//              nHandle      - Handle DIC file is opened on
//
//      Notes:
//
//
////////////////////////
function SP_Check(cWord)

STATIC cBuf   := ""
STATIC cLast


STATIC nDicCount   := 0
STATIC nCacheCount := 0
STATIC nBuffCount  := 0


LOCAL ok      := .T.
LOCAL cLookup := upper(rtrim(cWord))
LOCAL nRow
LOCAL nCol
LOCAL x
LOCAL y
LOCAL z := 4
LOCAL cTemp

**DEBUG**
@24,30 SAY "At SP_init"

if sp_init()
   if len(cLookup) == 1 .and. cLookup$"IA"
      return .T.
   endif
   if len(cLookup) < MINIMUM_WORD_LENGTH
      return .T.
   endif
   if right(cLookup,2)="'S"
      cLookUp := substr(cLookup,1, len(cLookup)-2 )
   endif
   cTemp := "|"+cLookup+"|"
   if fat(cTemp, COMMON_WORDS) == 0    // Check the common words first
      if fat(cTemp,CACHE_WORDS) == 0   // then check the cache words
          ok    := .F.
          nRow  := asc(substr(cLookup,1,1))-64
          nCol  := asc(substr(cLookup,2,1))-64
          if (nRow>0 .and. nRow<=26) .and. (nCol>0 .and. nCol<=26)

             #ifdef REDUCE_MEMORY_NEEDS

                cOffsets := space(6)
                fseek(nHandle,((nRow-1)*156)+((nCol-1)*6+1)+5,0)
                fread(nHandle,@cOffsets,6)
                x  := bin2l(substr(cOffsets,1,4))
                y  := bin2w(substr(cOffsets,5,2))

             #else

                x  := bin2l(substr(cOffsets,((nRow-1)*156)+((nCol-1)*EACH_WORD+1),4))
                y  := bin2w(substr(cOffsets,((nRow-1)*156)+((nCol-1)*EACH_WORD+5),2))

             #endif

             if !empty(x)
                if !(cLast==substr(cLookup,1,2))
                   cBuf := space(y)
                   fseek(nHandle,x,0)
                   fread(nHandle,@cBuf,y)
                   nDicCount++
                else
                   nBuffCount++
                endif
                if len(cLookup)=3
                   z  := asc(substr(cLookup,3,1))-64
                   ok := bit(cBuf,z)
                elseif len(cLookup) < 3
                   ok := bit(cBuf,27)
                elseif y > 4
                   cTemp := StoreWord(cLookup)
                   do while z < y
                      z   := fat( cTemp, cBuf,z )
                      if z < 6
                         exit
                      elseif substr(cBuf,z-1,1) < Chr( 128 )
                         z++
                      else
                         exit
                      endif
                   enddo
                   ok   := z > 4 .and. z<y
                endif
                cLast := substr(cLookup,1,2)
             endif
         endif
      else
         nCacheCount++
      endif
   else
      nCacheCount++
   endif
else
   ok := .F.
endif
if !ok .and. EXTRA_CODE_BLOCK != NIL
   ok := eval( EXTRA_CODE_BLOCK,cWord )
endif
return ok
*******************************************************************************

//
//   Function:  Sp_GetBuf()
//    Purpose:  To get all words within the buffer
//     Syntax:  Sp_GetBuf( <cLetters> )
//  Arguments:  <cLetters>   - First two letters
//    Returns:  cString      - Buffer string from DIC file
//
////////////////////////
STATIC function sp_GetBuf(cLetters)
LOCAL x
LOCAL y
LOCAL cBuf  := ""
LOCAL nRow  := asc(substr(cLetters,1,1))-64
LOCAL nCol  := asc(substr(cLetters,2,1))-64

**DEBUG**
@24,30 SAY "At SP_getbuf"

if (nRow>0 .and. nRow<=26) .and. (nCol>0 .and. nCol<=26)
   x  := bin2l(substr(cOffsets,((nRow-1)*156)+((nCol-1)*EACH_WORD+1),4))
   if !empty(x)
       y    := bin2w(substr(cOffsets,((nRow-1)*156)+((nCol-1)*EACH_WORD+5),2))
       cBuf := space(y)
       fseek(nHandle,x+4,0)
       fread(nHandle,@cBuf,y-4)
   endif
endif
return cBuf
******************************************************************************

//
//   Function:  Sp_clear()
//    Purpose:  To clear out the cache list
//     Syntax:  Sp_clear()
//  Arguments:  NONE
//    Returns:  NIL
//
//     Static:  CACHE_WORDS  - String of cache words
//
////////////////////////
function sp_clear

**DEBUG**
@24,30 SAY "At SP_clear"

CACHE_WORDS := ""
return NIL
******************************************************************************

//     Function: Sp_GetSet()
//      Purpose: To get/set a parameter for the spell check function
//       Syntax: <xOldValue> := Sp_GetSet( nWhich [,xNewSetting] )
//   Parameters: nWhich      - Which parameter to get/set
//               xNewSetting - Value to set parameter to
//
//      Returns: <logical>  TRUE if word succesfully added
//                          FALSE if an error occurs, usually network lock
//                          problem
//
//////////////////////////////////////
function Sp_GetSet(nWhich,xNewSetting)
LOCAL xOld := NIL

**DEBUG**
@24,30 SAY "At SP_getset"

if nWhich != NIL .and. (nWhich>0 .and. nWhich<=len(aGlobal))
    xOld := aGlobal[nWhich]
    if valtype(xNewSetting) == valtype(xOld)
       aGlobal[nWhich] := xNewSetting
    endif
endif
return xOld
******************************************************************************

//     Function: Sp_LoadAux()
//      Purpose: To load an auxiliary dictionary of words
//       Syntax:
//   Parameters:
//
//
//      Returns:
//
//
//
//////////////////////////////////////
function Sp_LoadAux(cFile)
LOCAL is_ok := .F.
LOCAL x     := 0
LOCAL cStr  := ""
LOCAL nSize

**DEBUG**
@24,30 SAY "At SP_loadaux"

if !empty(cFile)
   if file(cFile)
      x := fopen(cFile,64)
   else
      x := fcreate(cFile)
   endif
   SP_GetSet(5,cFile)

   if x > 0
      nSize := fseek(x,0,2)
      if nSize < MAX_STRING
         cStr  := space(nSize)
         fseek(x,0,0)
         fread(x,@cStr,nSize)
         cStr  := "|"+strtran(cStr,CRLF,"|")
         CACHE_WORDS += upper(cStr)
         is_ok := .T.
      endif
      fclose(x)
   endif
endif
return is_ok
******************************************************************************

//
//     Function:  Sp_Suggest()
//      Purpose:  To return an array of possible spellings
//       Syntax:  aSuggest_ := Sp_Suggest(cWord [,lInclude] )
//    Arguments:  cWord     - Word to look for suggestions for
//                lInclude  - Should word be included in list?
//      Returns:  aSuggest_ - List of suggested words
//
///////////////////////////////////
function Sp_Suggest(cWord,lInclude)

STATIC aParts_ := { {"A"    ,{"AI","AO","AU","AY","EA","EI","EIGH","ET","EY","E","I","O"}},;
                    {"AIR"  ,{"ARE"}},;
                    {"AIT"  ,{"ATE"}},;
                    {"C"    ,{"CK"}},;
                    {"CH"   ,{"TCH","TI","TU"}},;
                    {"CKS"  ,{"X"}},;
                    {"D"    ,{"ED"}},;
                    {"E"    ,{"A","AE","AI","AY","EA","EI","EO","IE","I","U","O"}},;
                    {"EM"   ,{"TEM"}},;
                    {"ER"   ,{"OUR","RE","URE","YR"}},;
                    {"ERE"  ,{"EIR","EAR","IER"}},;
                    {"F"    ,{"GH","LF","PH","G"}},;
                    {"FIZ"  ,{"PHYS"}},;
                    {"G"    ,{"GH","GU","GUE"}},;
                    {"GZ"   ,{"X"}},;
                    {"H"    ,{"WH"}},;
                    {"I"    ,{"A","E","EE","IA","IE","O","U","UI","Y","YE","UY","EI","IGH"}},;
                    {"IS"   ,{"US","ACE","ICE"}},;
                    {"ISE"  ,{"IZE"}},;
                    {"J"    ,{"D","DG","DI","DJ","G","GG"}},;
                    {"K"    ,{"C","CC","CH","CK","CQU","CU","LK","Q","QU","QUE"}},;
                    {"KW"   ,{"QU"}},;
                    {"L"    ,{"SL"}},;
                    {"LE"   ,{"TLE","AL"}},;
                    {"M"    ,{"CHM","GM","LM","MB","MN","N"}},;
                    {"N"    ,{"M","PN","GN","KN","MN"}},;
                    {"NG"   ,{"N","NGUE"}},;
                    {"O"    ,{"AU","EAU","EO","EW","OA","OE","OH","OU","OUGH","OW","A","AH","AW","UO","E"}},;
                    {"OE"   ,{"OWE","OUGH"}},;
                    {"OO"   ,{"O","EU","EW","OE","OU","OUGH","U","UE","UI"}},;
                    {"OU"   ,{"OUGH","OW"}},;
                    {"PER"  ,{"PRO","PRI","PRA","PRU"}},;
                    {"PRE"  ,{"PRO","PRI","PRA","PRU"}},;
                    {"R"    ,{"RH","WR"}},;
                    {"S"    ,{"C","CE","PS","SC","SCH"}},;
                    {"SH"   ,{"CE","CH","CI","S","SCH","SCI","SE","SI","SS","SSI","TI"}},;
                    {"SI"   ,{"PSY","CY"}},;
                    {"T"    ,{"ED","GHT","PT","TH"}},;
                    {"TION" ,{"SION","CION","CEAN","CIAN"}},;
                    {"TIOUS",{"SEOUS"}},;
                    {"TURE" ,{"TEUR"}},;
                    {"U"    ,{"O","OE","OO","OU","EUA","EU","EUE","EW","IEU","IEW","UE","UI","YOU","YU"}},;
                    {"UR"   ,{"EAR","ER","IR","YR"}},;
                    {"V"    ,{"F","LV","PH"}},;
                    {"W"    ,{"O","U","WH"}},;
                    {"X"    ,{"CKS","GZ","K"}},;
                    {"Y"    ,{"I","J","IE","EI"}},;
                    {"Z"    ,{"S","SC","SS","X","GE","SI","ZI"}},;
                    {"ZI"   ,{"XY"}} }

STATIC aEnds := { "ED","ER","ING","LY","AL","FUL","NESS","MENT","IVE" }
STATIC aBegs := { "UN","IN","DIS","MIS","EN","WEL","AL" }

LOCAL jj,kk,zz,ii
LOCAL cHold
LOCAL aRet_   := {}                // List of suggested words
LOCAL cTemp
LOCAL nSugg
LOCAL nSize    := len(aParts_)
LOCAL nSuffix  := len(aEnds)
LOCAL cMeta
LOCAL cFirst
LOCAL cKey
LOCAL arr_

**DEBUG**
@24,30 SAY "At SP_suggest"

cWord := upper(rtrim(cWord))
zz    := len(cWord)

if zz =1                           // Don't offer suggestions for
   return aRet_                    // single letter words
endif

if lInclude == NIL ; lInclude := .F.  ; endif

//
// Should the current word be included?
//
//////////////////////////////////
if Sp_Check(cWord)
   Aadd(aRet_,"A0AA"+cWord)
endif

if "'" $ cWord
  cHold := SP_Expand(cWord)
  if !empty(cHold)
     Aadd(aRet_,"A1AA"+cHold)
  endif
endif

if CHECK_RUNONS
   arr_ := SP_Split( cWord )
   for jj := 1 to len(arr_)
      Aadd(aRet_,"A1AA"+arr_[jj] )
   next
endif

if SUGGEST_PREFERENCE $ "AB"

   //
   // Step One - Do letter doubling
   //
   //////////////////////////////////
   for jj := 2 to zz
      if substr(cWord,jj,1)$"BCDEFGKLMNOPRSTZ"
         cHold := left(cWord,jj)+substr(cWord,jj, 1)+;
                  substr(cWord,jj+1)
         //
         // If the word is not already in the list, then check
         // to see if it is a valid word.
         //
         //////////////////////////////////////////
         if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0 .and. Sp_Check(cHold)
            Aadd(aRet_,"B"+SP_Rate(cHold,cWord)+cHold)
         endif
      endif
   next

   //
   // Step Two - Remove extra letters
   //
   //////////////////////////////////
   for jj := 1 to zz
      cHold := left(cWord,jj-1)+substr(cWord,jj+1)
      //
      // If the word is not already in the list, then check
      // to see if it is a valid word.
      //
      //////////////////////////////////////////
      if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0 .and. Sp_Check(cHold)
         Aadd(aRet_,"B"+SP_Rate(cHold,cWord)+cHold)
      endif
   next

   //
   // Step Three - Transpose the letters
   //
   //////////////////////////
   for jj := 2 to zz
      cHold := left(cWord,jj-2)+substr(cWord,jj, 1)+;
               substr(cWord,jj-1,1)+substr(cWord,jj+1)
     if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0 .and. Sp_Check(cHold)
         Aadd(aRet_,"B"+SP_Rate(cHold,cWord)+cHold)
      endif
   next

   //
   // Step Four - Try adding a silent E to the end
   //
   //////////////////

   cHold := cWord+"E"
   if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0 .and. Sp_Check(cHold)
      Aadd(aRet_,"B"+SP_Rate(cHold,cWord)+cHold)
   endif

   //
   // Step Five - Do sound alike substitutions
   //
   //////////////////////////////////
   for jj := 1 to nSize
      if aParts_[jj,1]$cWord
         if aParts_[jj,1]$"AEIOUT"
            ii := 0
            do while (ii := fat(aParts_[jj,1],cWord,ii))>0
               for kk := 1 to len(aParts_[jj,2])
                  cHold := substr(cWord,1,ii-1)+aParts_[jj,2,kk]+substr(cWord,ii+1)
                  if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0 .and. Sp_Check(cHold)
                     Aadd(aRet_,"B"+SP_Rate(cHold,cWord)+cHold)
                  endif
               next
               ii++
            enddo
         else
            for kk := 1 to len(aParts_[jj,2])
               cHold := strtran(cWord,aParts_[jj,1],aParts_[jj,2,kk])
               if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0 .and. Sp_Check(cHold)
                  Aadd(aRet_,"B"+SP_Rate(cHold,cWord)+cHold)
              endif
           next
        endif
      endif
   next
   nSugg := len(aRet_)

   //
   // At this point, we have a list of words in aRet_, which now need to
   // be checked for suffixes, prefixes, and plurals.
   //

   for jj := 1 to nSugg
      cHold := rtrim(substr(aRet_[jj],5))    // Extract the word
      zz    := len(cHold)
      //
      // Check suffixes
      //
      ////////////////
      if ADD_SUFFIXES
         for kk := 1 to nSuffix
            cTemp := cHold+aEnds[kk]
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               Sp_Check(cTemp)
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
         next
         //
         // Try doubling the last letter
         //
         //////////
         for kk := 1 to nSuffix
            cTemp := cHold+substr(cHold,zz,1)+aEnds[kk]
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               SP_Check( cTemp )
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
         next
         //
         // Accomodate words ending in C
         //
         //////////
         if substr(cHold,-1,1)=="C"
            for kk := 1 to nSuffix
               cTemp := cHold+"K"+aEnds[kk]
               if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
                  SP_Check( cTemp )
                  Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
               endif
            next
         endif
         //
         // Accomodate words ending in ND
         //
         //////////
         if substr(cHold,-2,2)=="ND"
            cTemp := substr(cHold,1,zz-1)+"SE"
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               SP_Check( cTemp )
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
            for kk := 1 to nSuffix
               cTemp := substr(cHold,1,zz-1)+"SE"+aEnds[kk]
               if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
                  SP_Check( cTemp )
                  Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
               endif
               cTemp := substr(cHold,1,zz-1)+"S"+aEnds[kk]
               if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
                  SP_Check( cTemp )
                  Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
               endif
            next
         endif
         //
         // Words ending in E, remove the E and try
         //
         ///////////////////////////////////////
         if substr(cHold,zz,1)=="E"
            for kk := 1 to nSuffix
               cTemp := substr(cHold,1,zz-1)+aEnds[kk]
               if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
                  SP_Check( cTemp )
                  Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
               endif
            next
         endif
      endif

      if ADD_PREFIXES
         for kk := 1 to 7
            cTemp := aBegs[kk]+cHold
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               SP_Check( cTemp )
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
         next
      endif

      if ADD_PLURALS
         cTemp := cHold+"S"
         if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
            SP_Check(cTemp)
            Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
         endif

         if substr(cHold,zz,1)=="Y"
            cTemp := substr(cHold,1,zz-1)+"IES"
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               SP_Check(cTemp)
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
         elseif substr(cHold,zz,1)=="F"
            cTemp := substr(cHold,1,zz-1)+"VES"
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               SP_Check(cTemp)
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
         elseif substr(cHold,zz,1)=="O"
            cTemp := cHold+"ES"
            if ascan(aRet_,{ |xx| substr(xx,5)==cTemp}) == 0 .and. ;
               SP_Check(cTemp)
               Aadd( aRet_,"C"+SP_Rate(cTemp,cWord)+cTemp )
            endif
         endif
      endif
   next
endif

//
// Check metaphone matches, only if SUGGEST_PREFERENCE is metafone or both,
// because this search can slow things down a bit
//
////////////////////////////
if SUGGEST_PREFERENCE $ "MB"
   if METAPHONE_SIZE == 0
      zz    := min( 5,len(cWord))
      if zz < 3
         zz := 3
      endif
   else
      zz  := METAPHONE_SIZE
   endif
   cTemp  := SP_GetBuf(cWord)
   ii     := len(cTemp)
   cMeta  := C_Metafone(cWord,zz)
   zz     := len(cMeta)
   cFirst := substr(cWord,1,2)

   if ii > 0

      if substr(cMeta,2,1) >= "N"

         kk := ii-5
         jj := ii-4

         do while kk > 0
            if asc(substr(cTemp,kk,1)) >= 128      // End of word
               cHold := cFirst+xUnForm(substr(cTemp,kk+1,jj-kk))
               cKey  := c_metafone(cHold,zz)
               if cMeta ==  c_Metafone(cHold,zz)
                  if MAX_DIFFERENCE < 0 .or. abs(len(cWord)-len(cHold)) <= MAX_DIFFERENCE
                     if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0
                        Aadd( aRet_,"B"+SP_Rate(cHold,cWord)+cHold )
                     endif
                  endif
               elseif cKey < cMeta
                 exit
               endif
               jj    := kk
            endif
            kk--
         enddo


      else
         kk := 1
         jj := 1

         do while kk < ii
            if asc(substr(cTemp,kk,1)) >= 128      // End of word
               cHold := cFirst+xUnForm(substr(cTemp,jj,kk-jj+1))
               cKey  := c_metafone(cHold,zz)
               if cMeta ==  c_Metafone(cHold,zz)
                  if MAX_DIFFERENCE < 0 .or. abs(len(cWord)-len(cHold)) <= MAX_DIFFERENCE
                     if ascan(aRet_,{ |xx| substr(xx,5)==cHold}) == 0
                        Aadd( aRet_,"B"+SP_Rate(cHold,cWord)+cHold )
                     endif
                  endif
               elseif cKey > cMeta
                 exit
               endif
               jj    := kk+1
            endif
            kk++
         enddo
      endif

   endif
endif


nSugg := len(aRet_)
if nSugg > 1 .and. SORT_SUGGESTIONS
   Asort(aRet_)
endif

for kk := 1 to nSugg
   aRet_[kk] := substr(aRet_[kk],5)
next

return aRet_
*******************************************************************************

**
**     Function:  Sp_Quick()
**      Purpose:  To return an array of quick spellings
**       Syntax:  aSuggest  := Sp_Quick( cWord )
**    Arguments:  cWord     - Word to look for suggestions for
**      Returns:  aSuggest  - List of suggested words
**
******************************************************************************
function Sp_Quick( cWord )

STATIC aTryThese := { "AI$AO$AU$AY$EA$EI$EIGH$ET$EY$E$O$",;
                      "A$AE$AI$AY$EA$EI$EO$IE$U$O$",;
                      "A$E$EE$IA$IE$O$U$UI$Y$YE$UY$EI$IGH$",;
                      "AU$EAU$EO$EW$OA$OE$OH$OU$OUGH$OW$A$AH$AW$UO$E$",;
                      "O$OE$OO$OU$EUA$EU$EUE$EW$IEU$IEW$UE$UI$YOU$YU$",;
                      "ED$GHT$PT$TH$"  }

LOCAL ii       := 0
LOCAL nOld,jj,kk
LOCAL zz       := len(cWord)
LOCAL arr_     := {}
LOCAL cHold,ll,cTemp

**DEBUG**
@24,30 SAY "At SP_quick"

cWord    := upper(rtrim(cWord))

if zz < 3                          && Don't offer suggestions for
   return {}                       && one or two letter words
endif


**
** Step One - Do letter doubling
**
**********************************
for jj := 2 to zz
   if substr(cWord,jj,1)$"BCDEFGKLMNOPRSTZ"
      cHold := left(cWord,jj)+substr(cWord,jj, 1)+;
               substr(cWord,jj+1)
      **
      ** If the word is not already in the list, then check
      ** to see if it is a valid word.
      **
      ******************************************
      if ascan(arr_,cHold) == 0 .and. Sp_Check(cHold)
         Aadd(arr_,cHold)
      endif
   endif
next

**
** Step Two - Remove extra letters
**
**********************************
for jj := 1 to zz
   cHold := left(cWord,jj-1)+substr(cWord,jj+1)
   **
   ** If the word is not already in the list, then check
   ** to see if it is a valid word.
   **
   ******************************************
   if ascan(arr_,cHold) == 0 .and. Sp_Check(cHold)
      Aadd(arr_,cHold)
   endif
next

**
** Step Three - Transpose the letters
**
**************************
for jj := 2 to zz
   cHold := left(cWord,jj-2)+substr(cWord,jj, 1)+;
            substr(cWord,jj-1,1)+substr(cWord,jj+1)
   if ascan(arr_,cHold) == 0 .and. Sp_Check(cHold)
      Aadd(arr_,cHold)
   endif
next

**
** Step Four - Try adding a silent E to the end
**
******************

cHold := cWord+"E"
if ascan(arr_,cHold) == 0 .and. Sp_Check(cHold)
   Aadd(arr_,cHold)
endif

**
** Step Five - Do sound alike substitutions
**
**********************************


for jj := 1 to 6
   if substr("AEIOUT",jj,1)$cWord
      ii   := fat(substr("AEIOUT",jj,1),cWord,ii)
      nold := 1
      do while ii > 0
         for kk := 1 to chrcount("$",aTryThese[jj])
            ll    := fat("$",aTryThese[jj],nOld)
            cTemp := substr(aTryThese[jj],nOld,ll-nOld)
            nOld  := ll +1
            cHold := substr(cWord,1,ii-1)+cTemp+substr(cWord,ii+1)
            if ascan(arr_,cHold) == 0 .and. Sp_Check(cHold)
               Aadd(arr_,cHold)
            endif
         next
         ii++
         ii := fat(substr("AEIOUT",jj,1),cWord,ii)
      enddo
   endif
next

return arr_

STATIC function ChrCount(cChar,cString)

**DEBUG**
@24,30 SAY "At ChrCount"

return len(cString)-len(strTran(cString,cChar,""))

**
**     Function:  Sp_Split()
**      Purpose:
**       Syntax:
**    Arguments:
**      Returns:
**
******************************************************************************
function Sp_Split( cWord )
LOCAL arr_ := {}
LOCAL x
LOCAL cWord1
LOCAL cWord2

**DEBUG**
@24,30 SAY "At SP_split"

for x := 2 to len(cWord)
   cWord1 := left(cWord,x-1)
   cWord2 := substr(cWord,x)
   if len(cWord1) > 1 .and. len(cWord2) > 1
      if SP_Check(cWord1) .and.  SP_Check(cWord2)
         Aadd(arr_,cWord1+' '+cWord2)
      endif
   endif
next
return arr_
******************************************************************************


**
**     Function:  Sp_Expand()
**      Purpose:
**       Syntax:
**    Arguments:
**      Returns:
**
******************************************************************************
function SP_Expand(cWord)
LOCAL x
LOCAL cExpand

**DEBUG**
@24,30 SAY "At SP_expand"

cWord := upper(alltrim(cWord))
x := ascan( aContracts, { |jj|jj[1]=cWord } )
if x > 0
   cExpand :=  aContracts[x,2]
endif
return cExpand
******************************************************************************



**
**     Function:  Sp_WildCard()
**      Purpose:  To return an array of wildcard matches
**       Syntax:  aSuggest  := Sp_WildCard( cPattern )
**    Arguments:  cPattern  - Pattern to match using * or ?'s
**      Returns:  aSuggest  - List of matching words
**
******************************************************************************
function Sp_Wildcard( cPattern )
LOCAL   cTemp
LOCAL   ii,kk,jj,cFirst,cHold,x,z
LOCAL   arr_   := {}
LOCAL   nStart,nEnd

**DEBUG**
@24,30 SAY "At SP_wildcard"

cPattern := upper(cPattern)

if SP_Init()
   x      := fat("*",cPattern)
   if x == 0
      x := fat("?",cPattern)
   endif
   if x == 1                 // Can't handle wildcards in first position
      return arr_
   endif
   if x == 2
      nStart := 1
      nEnd   := 26
   else
      nStart := asc(substr(cPattern,2,1))-64
      nEnd   := nStart
   endif


   for z := nStart to nEnd
      cTemp  := SP_GetBuf(substr(cPattern,1,1)+chr(z+64) )
      ii     := len(cTemp)
      cFirst := substr(cPattern,1,1)+chr(z+64)

      if ii > 0
         kk := 1
         jj := 1
         do while kk < ii
            if asc(substr(cTemp,kk,1)) >= 128      // End of word
               cHold := cFirst+xUnForm(substr(cTemp,jj,kk-jj+1))
               if WildCard(cPattern,cHold)
                  Aadd( arr_,cHold )
               endif
               jj    := kk+1
            endif
            kk++
         enddo
      endif
   next
endif
return arr_



******************************************************************************
* The following functions are internal and should not be modified.           *
******************************************************************************

//   Function:  SP_Init()
//    Purpose:  Internal function to initialize the dictionary
//  Arguments:  NONE
//    Returns:  <logical> - Was dictionary initialized?
//
//      Notes:
//
////////////////////////////
function Sp_init
LOCAL isok := .T.
LOCAL cBuf
LOCAL i
LOCAL j
LOCAL nOther := 0
LOCAL cOther := ""
LOCAL nFileSize

**DEBUG**
@24,30 SAY "At SP_init"

if cOffsets == NIL
   isok    := .F.
   nHandle := fopen(DICTIONARY_PATH+DICTIONARY_NAME,32)
   if nHandle > 0

      #ifdef REDUCE_MEMORY_NEEDS

         cBuf := space(6)
         fread(nHandle,@cBuf,6)

      #else

         cBuf       := space(NSIZE+6)
         fread(nHandle,@cBuf,NSIZE+6)

      #endif

      if substr(cBuf,1,2)=="JJ"
         nOther    := bin2l(substr(cBuf,3,4))

         #ifndef REDUCE_MEMORY_NEEDS

             cOffSets  := substr(cBuf,7)

         #endif

         nFileSize := fseek(nHandle,0,2)
         if nFileSize-nOther > 0
            cOther   := space(nFileSize-nOther)
            fseek(nHandle,nOther,0)
            fread(nHandle,@cOther,(nFileSize-nOther))
         endif
         aGlobal[2] += cOther
         isok     := .T.
      endif
      if !empty( AUXILIARY_DICTIONARY )
         SP_LoadAux( AUXILIARY_DICTIONARY )
      endif
   endif
   if aGlobal[1] == NIL
      SP_Common()
   endif

** Thesaurus comented out as not needed
*  if !empty( THESAURUS_NAME )
*     if file( DICTIONARY_PATH+THESAURUS_NAME )
*        SP_OpenThes( DICTIONARY_PATH+THESAURUS_NAME )
*     elseif file( THESAURUS_NAME )
*        SP_OpenThes( THESAURUS_NAME )
*     endif
*  endif

endif

return isok
******************************************************************************


//
//      Function:  DBF2Dic()
//       Purpose:  To create a DIC file from a DBF list of words
//        Syntax:  nStatus := DBF2Dic( <cDBF_file>,<cDIC_file> )
//     Arguments:  <cDBF_File>  - DBF containing sorted list of upper
//                                case words. The field name must be
//                                WORD and of type character
//                 <cDIC_File>  - Name of DIC file to create, assumes
//                                DICT.DIC
//       Returns:  nStatus      -  0 if successful
//                                -1 DBF file does not exist
//                                -2 Field WORD does not exist in DBF
//                                >0 File error occurred, see FERROR()
//
//         Calls:
//
//////////////////////////////////
function DBF2Dic(cDbf,cDictionary,lTalk)

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
//  DEFAULT name for dictionary file
//
//////////////////////////////////////
**DEBUG**
@24,30 SAY "At DBF2DIC"

if cDictionary == NIL
   cDictionary := "DICT.DIC"
endif

if lTalk == NIL
   lTalk := .F.
endif


//
//  See if the DBF file exists
//
//////////////////////////////////////
if .not. "."$cDBF
   cDBF += ".DBF"
endif

if !file( cDBF )
   return -1
endif

use (cDbf) alias DICT new
if fieldpos("WORD") == 0
   use
   return -2
endif

if lTalk

   cSave := SaveScreen(8,30,11,48)
   hb_DispBox(8,30,11,48,,"W+/R")
   @  8,33 say " Creating DIC "     color "W+/R"
   @  9,31 say " Indexing words  "  color "W/R"
   @ 10,31 say "                 "  color "W/R"

   nSize := DICT->(lastrec())

endif

index on substr(DICT->word,1,2)+padr(c_metafone(alltrim(DICT->word),5),6) to $$temp
go top


if lTalk
   @  9,31 say "Percent          "  color "W/R"
   @ 10,31 say "Complete:        "  color "W/R"
endif

//
//  Create the dictionary file
//
//////////////////////////////////////
                                       && ADDED - 02/08/96 - JAMES
IF nHandle > 0                         && Is dictionary already open?
   FCLOSE(nHandle)                     && Yes, close it
   nHandle := 0
ENDIF                                  && END OF ADDITION

nH := fcreate(cDictionary)

if nH >= 0
   //
   // Write out enough bytes to hold the index information
   //
   /////////////////////////////////////////////////////////////////////////
   fwrite(nH,"JJ"+l2bin(NSIZE+4)+replicate(chr(0),NSIZE)+space(10),NSIZE+16)

   for i := 1 to 26
      do while substr(DICT->word,1,1)==chr(i+64) .and. !eof()
         for j := 1 to 26
            temp  := ""
            cBits := FOUR_BYTES
            do while !isAlpha(substr(DICT->word,2,1)) .and. !eof()
               if len(alltrim(DICT->word)) > 0
                  cOther += "|"+alltrim(DICT->word)
               endif
               skip +1
            enddo
            do while substr(DICT->word,2,1)==chr(j+64) .and. !eof()
               if len(rtrim(DICT->word))=3
                  Bit(cBits,asc(substr(DICT->word,3,1))-64,.T.)
               elseif len(rtrim(DICT->word))==2
                  Bit(cBits,27,.T.)
               else
                  temp += StoreWord(rtrim(DICT->word))
               endif
               skip +1
               if lTalk
                   nCurRec++
                   @ 10,41 say (nCurRec/nSize) * 100   picture "999.9%"   color "W+/R"
               endif
            enddo
            if !empty(temp) .or. cBits != FOUR_BYTES

               nWhere := fseek(nH,0,2)

               Fseek(nH,((i-1)*26*EACH_WORD)+((j-1)*EACH_WORD)+6)
               Fwrite(nH,l2bin(nWhere)+i2bin(len(temp)+4),EACH_WORD)
               Fseek(nH,0,2)
               Fwrite(nH,cBits+temp,len(temp)+4)
            endif
         next
      enddo
   next
   j := fseek(nH,0,2)
   fseek(nH,2,0)
   fwrite(nH,l2bin(j),4)
   if !empty(cOther)
      fseek(nH,j,0)
      cOther += "|"
      fwrite(nH,cOther,len(cOther))
   endif
   fclose(nH)
else
   nStatus := nH
endif

if lTalk
   RestScreen(8,30,11,48,cSave)
endif

select DICT
use
//  ferase("$$TEMP"+indexext())
return nStatus
*******************************************************************************

//
//      Function:  Dic2DBF()
//       Purpose:  To create a DBF file from a DIC file
//        Syntax:  nStatus := DIC2DBF( <cDIC_file>,<cDBF_file> )
//     Arguments:  <cDIC_File>  - Name of DIC file to read, assumes
//                                DICT.DIC
//                 <cDBF_File>  - Name of DBF to create, assumes DICT.DBF
//                                It will contain a single field called
//                                WORD of type character
//       Returns:  nStatus      -  0 if successful
//                                -1 DIC file does not exist
//                                -2 DIC file is not a valid dictionary
//
//         Calls:
//
//////////////////////////////////
function DIC2DBF(cDictionary,cDBF,lTalk)

LOCAL nStatus := 0                 // Status code
LOCAL i                            // Loop counters
LOCAL j
LOCAL x,y,z
LOCAL temp
LOCAL cBuf
LOCAL cWord
LOCAL cSave
LOCAL nSize

**DEBUG**
@24,30 SAY "At DIC2DBF"

//
//  DEFAULT name for dictionary file
//
//////////////////////////////////////
if cDictionary == NIL
   cDictionary := "DICT.DIC"
endif

if lTalk == NIL
   lTalk := .F.
endif

if !file(cDictionary)
   return -1
endif

DICTIONARY_NAME := cDictionary

//
//  Read the dictionary file
//
//////////////////////////////////////
if !SP_Init()
   return -2
endif
//
//  See if the DBF file exists
//
//////////////////////////////////////
if .not. "."$cDBF
   cDBF += ".DBF"
endif

dbcreate( cDbf,{ {"WORD","C",25,0} } )

use (cDbf) alias DICT new

if lTalk
   cSave := SaveScreen(8,30,12,48)
   hb_DispBox(8,30,12,48,,"W+/R")
   @  8,34 say " Creating DBF "     color "W+/R"
   @  9,31 say "Percent          "  color "W/R"
   @ 10,31 say "Complete:        "  color "W/R"
   @ 11,31 say "  Record:        "  color "W/R"

   temp  := directory(cDictionary)
   nSize := temp[1,2]

endif


j := 2

for i := 2 to len(CACHE_WORDS)
    if substr(CACHE_WORDS,i,1)=="|"
       append blank
       replace DICT->word with substr(CACHE_WORDS,j,i-j)
       j := i +1
    endif
next


for i := 1 to 26
   for j := 1 to 26
      temp := chr(i+64)+chr(j+64)
      x  := bin2l(substr(cOffsets,((i-1)*156)+((j-1)*EACH_WORD+1),4))
      if !empty(x)
         y    := bin2w(substr(cOffsets,((i-1)*156)+((j-1)*EACH_WORD+5),2))

         if lTalk
            @ 10,43 say (x/nSize) * 100   picture "999%"   color "W+/R"
            @ 11,41 say lastrec()         picture "99,999" color "W+/R"
         endif

         cBuf := space(y)
         fseek(nHandle,x,0)
         fread(nHandle,@cBuf,y)
         for z := 1 to 26
            if bit(cBuf,z)
               append blank
               replace DICT->word with temp+chr(z+64)
            endif
         next
         if bit(cBuf,27)
            append blank
            replace DICT->word with temp
         endif
         cBuf := substr(cBuf,5)
         z    := 1
         do while !empty(cBuf)
            if substr(cBuf,z,1)>=Chr(128)
               cWord := substr(cBuf,1,z)
               append blank
               replace DICT->word with temp+ExtractWord(cWord)
               cWord := ""
               cBuf  := substr(cBuf,z+1)
               z     := 1

               if lTalk
                  @ 11,41 say lastrec()  picture "99,999" color "W+/R"
               endif

            else
               z++
            endif
         enddo
         if !empty(cWord)
         endif
         cWord := ""
      endif
   next
next

if lTalk
   RestScreen(8,30,12,48,cSave)
endif

select DICT
use
return nStatus
********************************************************************************

//     Function:  SP_Common()
//      Purpose:  Loads the COMMON word static array element
//       Syntax:  SP_Common()
//    Arguments:  <NONE>
//      Returns:  NIL
//
//        Notes:  The common word list represents some of the most commonly
//                used English words.  They are stored in RAM to prevent a
//                dictionary lookup for 70-80% of the words.
//
///////////////////////////
STATIC function SP_common

#ifDef REDUCE_MEMORY_NEEDS

aGlobal_[1]  :=  "|THE|OF|AND|TO|A|IN|THAT|FOR|IS|WAS|IT|HE|I|AS"+;
                 "|WITH|ON|HIS|BE|AT|BY|NOT|THIS|HAD|HAVE|YOU|BUT"+;
                 "|FROM|ARE|OR|WHICH|AN|THEY|WILL|ONE|WERE|ALL|WE"+;
                 "|HER|SHE|WOULD|THERE|HAS|BEEN|HIM|THEIR|IF|WHEN"+;
                 "|SO|MORE|NO|WHO|YOUR|OUT|MY|I'VE|I'D|I'LL|CAN'T|"

#else

aGlobal[1] := "|THE|OF|AND|TO|A|IN|THAT|FOR|IS|WAS|IT|HE|I|AS|WITH|ON|HIS|BE|AT|BY"+;
              "|NOT|THIS|HAD|HAVE|YOU|BUT|FROM|ARE|OR|WHICH|AN|THEY|WILL|ONE|WERE|ALL"+;
              "|WE|HER|SHE|WOULD|THERE|HAS|BEEN|HIM|THEIR|IF|WHEN|SO|MORE|NO|WHO|YOUR"+;
              "|OUT|MY|WHAT|UP|CAN|THEM|ABOUT|INTO|ITS|TIME|SOME|THAN|ME|OTHER|ONLY|NEW"+;
              "|COULD|SAID|ANY|THESE|MAY|TWO|THEN|DO|FIRST|NOW|MAN|SUCH|VERY|LIKE|OVER"+;
              "|OUR|EVEN|MOST|AFTER|MADE|ALSO|DID|MANY|SHOULD|BEFORE|MUST|THROUGH|YEARS"+;
              "|WHERE|MUCH|WAY|WELL|DOWN|GOOD|BECAUSE|HERE|EACH|THOSE|PEOPLE|STATE|HOW"+;
              "|TOO|LITTLE|WORLD|LAST|WORK|STILL|BACK|OWN|SEE|MEN|LONG|GET|JUST|GO|HOUSE"+;
              "|BETWEEN|BOTH|LIFE|BEING|UNDER|YEAR|NEVER|DAY|SAME|ANOTHER|KNOW|WHILE|MIGHT"+;
              "|US|GREAT|OLD|OFF|COME|AGAINST|SINCE|CAME|RIGHT|TAKE|USED|THREE|SMALL|MAKE"+;
              "|STATES|HIMSELF|FEW|DURING|WITHOUT|AGAIN|PLACE|AROUND|HOWEVER|HOME|FOUND"+;
              "|SAY|WHOSE|THOUGHT|NIGHT|DEAR|ONCE|WENT|GENERAL|HIGH|UPON|SCHOOL|EVERY|PART"+;
              "|DON'T|DOES|GOT|UNITED|LEFT|NUMBER|COURSE|WAR|GOING|UNTIL|ALWAYS|AWAY"+;
              "|SOMETHING|FACT|THOUGH|WATER|LESS|PUBLIC|PUT|THINK|ALMOST|HAND|ENOUGH|FAR"+;
              "|TOOK|YET|GOVERNMENT|SYSTEM|BETTER|EYES|SET|TOLD|NOTHING|PRESIDENT|END"+;
              "|HEAD|WHY|CALLED|DIDN'T|FIND|LOOK|ASKED|LATER|POINT|NEXT|PROGRAM|KNEW|CITY"+;
              "|BUSINESS|GIVE|GROUP|TOWARD|YOUNG|DAYS|LET|ROOM|WITHIN|CHILDREN|SIDE|SOCIAL"+;
              "|GIVEN|ORDER|PRESENT|SEVERAL|NATIONAL|USE|RATHER|SECOND|FACE|PER|POSSIBLE"+;
              "|AMONG|FORM|IMPORTANT|OFTEN|THINGS|LOOKED|EARLY|WHITE|BECOME|CASE|SEND"+;
              "|BIG|LARGE|NEED|FOUR|FELT|ALONG|GOD|SAW|BEST|CHURCH|EVER|LEAST|POWER"+;
              "|DEVELOPMENT|LIGHT|THING|FAMILY|INTEREST|SEEMED|WANT|TODAY|MEMBERS|MIND"+;
              "|COUNTRY|AREA|OTHERS|TURNED|ALTHOUGH|DONE|OPEN|SERVICE|CERTAIN|KIND|PROBLEM"+;
              "|BEGAN|DIFFERENT|DOOR|THUS|HELP|MEANS|SENSE|WHOLE|MATTER|PERHAPS|ITSELF"+;
              "|IT'S|TIMES|HUMAN|LAW|LINE|ABOVE|NAME|EXAMPLE|COMPANY|HANDS|LOCAL|SHOW"+;
              "|BODY|FIVE|HISTORY|WHETHER|GAVE|EITHER|ACROSS|ACT|ACTION|FEET|ANYTHING"+;
              "|PAST|QUITE|TAKEN|HAVING|SEEN|DEATH|EXPERIENCE|HALF|REALLY|WEEK|WORDS"+;
              "|CAR|FIELD|MONEY|WORD|ALREADY|THEMSELVES|INFORMATION|I'M|TELL|CLOSE"+;
              "|COLLEGE|PERIOD|SHALL|TOGETHER|HELD|KEEP|SURE|PROBABLY|FREE|REAL|BEHIND"+;
              "|SEEMS|CANNOT|POLITICAL|AIR|QUESTION|OFFICE|WOMAN|BROUGHT|MAJOR|MOTHER"+;
              "|SPECIAL|HEARD|PROBLEMS|AGO|BECAME|AVAILABLE|FEDERAL|MOMENT|STUDY"+;
              "|KNOWN|RESULT|STREET|BOY|ECONOMIC|CHANGE|POSITION|REASON|SOUTH|BOARD"+;
              "|INDIVIDUAL|JOB|SOCIETY|AREAS|WEST|SPACE|TURN|LOVE|COMMUNITY|TOWN|TRUE"+;
              "|COURT|FORCE|FULL|SEEM|AGE|AM|COST|WIFE|FUTURE|VOICE|WANTED|CAN'T|DEPARTMENT"+;
              "|USE|CENTER|COMMON|CONTROL|NECESSARY|POLICY|FOLLOWING|FRONTSOMETIMES|FATHER"+;
              "|GIRL|SIX|WOMEN|CLEAR|MILITARY|FURTHER|ABLE|FIGURE|LAND|FEEL|MUSIC|PARTY"+;
              "|PROVIDE|CENTURY|EDUCATION|UNIVERSITY|CHILD|EFFECT|STUDENTS|RUN|SHORT"+;
              "|STOOD|MORNING|TOTAL|OUTSIDE|ART|RATE|SAYS|YOU'RE|CLASS|TYPE|EVIDENCE"+;
              "|EXCEPT|LEAVE|MILLION|MISS|NORTH|PLAN|SOUND|THEREFORE|TOP|USUALLY|BLACK"+;
              "|HARD|SCHOOLS|STRONG|BELIEVE|PLAY|VARIOUS|SURFACE|MAKING|MEAN|SOON|VALUE"+;
              "|LINES|MODERN|NEAR|TABLE|PRIVATE|RED|ROAD|TAX|ALONE|MINUTES|PERSONAL|PROCESS"+;
              "|SITUATION|THAT'S|GONE|IDEA|INCREASE|NOR|PEACE|SECTION|LIVING|STARTED|BOOK"+;
              "|LONGER|CUT|FINALLY|NATURE|SECRETARY|MONTHS|THIRD|CALL|COMPLETE|GREATER"

aGlobal[1] += "|EXPECTED|FIRE|NEEDED|KEPT|VALUES|VIEW|BASIS|DARK|EVERYTHING|PRESSURE"+;
              "|GROUND|EAST|RECENT|REQUIRED|SPIRIT|UNION|HOPE|I'LL|MOVED|NATIONS|WROTE"+;
              "|CONDITIONS|RETURN|SUPPORT|ATTENTION|LATE|PARTICULAR|BROWN|COSTS|ELSE|NATION"+;
              "|BEYOND|COULDN'T|FORCES|HOURS|NUCLEAR|PERSON|TAKING|COMING|DEAD|INSIDE|LOW"+;
              "|MATERIAL|REPORT|STAGE|ADDED|AMOUNT|BASIC|DATA|FEELING|FOLLOWED|HEART|INSTEAD"+;
              "|LOOKING|LOST|MILES|PAY|SINGLE|COLD|HUNDRED|INCLUDING|INDUSTRY|MOVE|RESEARCH"+;
              "|SHOWN|BRIDGE|DEVELOPED|SIMPLY|TRIED|HOLD|REACHED|SHARE|SORT|COMMITTEE|DEFENSE"+;
              "|EQUIPMENT|ISLAND|SENDING|ACTUALLY|EARTH|BEGINNING|RELIGIOUS|RIVER|TEN|CENTRAL"+;
              "|DOING|GETTING|LETTER|RECEIVED|REST|TERMS|TRYING|CARE|FRIENDS|INDEED|MEDICAL"+;
              "|PICTURE|DIFFICULT|FINE|SIMPLE|STATUS|SUBJECT|BUILDING|ESPECIALLY|HIGHER|RANGE"+;
              "|WALL|BRING|MEETING|WALKED|CENT|FLOOR|FOREIGN|NEEDS|PAPER|PASSED|SIMILAR"+;
              "|FINAL|NATURAL|PROPERTY|TRAINING|COUNTY|GROWTH|HOT|INTERNATIONAL|LIVE"+;
              "|MARKET|POLICE|START|TALK|WASN'T|WRITTEN|BOOKS|ANSWER|CONGRESS|HEAR|PRIMARY"+;
              "|STORY|SUDDENLY|CONSIDERED|COUNTRIES|HALL|ISSUE|LIKELY|PARTICULARLY|TRULY"+;
              "|WORKING|EFFORT|SAT|ENTIRE|HAPPENED|LABOR|PURPOSE|RESULTS|CASES|DIFFERENCE"+;
              "|HAIR|PRODUCTION|STAND|I'VE|I'D|WON'T|"


#endif

**DEBUG**
@24,30 SAY "At SP_common"

return NIL
******************************************************************************

function WildCard(cPattern,cString)
LOCAL lMatch  := .F.
LOCAL x       := at("*",cPattern)
LOCAL cBefore
LOCAL cAfter
LOCAL y
LOCAL nStrSize,nPatSize

**DEBUG**
@24,30 SAY "At wildcard"

cString := upper(alltrim(cString))

if cPattern == "*"
   return .T.
endif
//
// Do a * match
//
if x > 0
   cBefore := upper(substr(cPattern,1,x-1))
   cAfter  := upper(substr(cPattern,x+1))
   do case
   case empty(cBefore)
      lMatch := right(cString,len(cAfter))==cAfter
   case empty(cAfter)
      lMatch := left(cString,len(cBefore))==cBefore
   otherwise
      lMatch := ( left(cString,len(cBefore))==cBefore ) .and. ;
                right(cString,len(cAfter))==cAfter
   endcase
else
   x      := at("?",cPattern)
   if x > 0
      nPatSize  := len(cPattern)
      nStrSize  := len(cString)
      if nPatSize == nStrSize
         lMatch := .T.
         for y := 1 to nPatSize
            if !(substr(cPattern,y,1)=="?") .and. ;
               !(substr(cPattern,y,1)== substr(cString,y,1))
                  lMatch := .F.
                  exit
            endif
         next
      endif
   else
      lMatch := upper(cPattern)=upper(cString)
   endif
endif
return lMatch
******************************************************************************
#ifndef CLIP52
function aWords(cLine)
LOCAL aWords_   := {}
LOCAL nSize     := len(rtrim(cLine))
LOCAL x,y,z
LOCAL cWord     := ""
LOCAL nOffset

**DEBUG**
@24,30 SAY "At clip52"

z := 0
do while z <= nSize
  z++
  y := asc(substr(cLine,z,1))
  if  y >=48 .and. ! chr(y)$ ":;<=>?@[\^]_`{|}~"
     nOffset := z
     cWord   := chr(y)
     z++
     y     := asc(substr(cLine,z,1))
     while ( y >=48 .and. ! chr(y)$ ":;<=>?@[\^]_`{|}~" ) .or. y == 39
        cWord += chr(y)
        z++
        if z > nSize
           exit
        endif
        y     := asc(substr(cLine,z,1))
     enddo
     Aadd(aWords_,{cWord,nOffset})
  endif
enddo
return  aWords_
#endif

* Find an occurrence of 'f_str' in 'l_str' starting from position 'f_rom'
FUNCTION FAT(f_str, l_str, f_rom)

LOCAL ret_val

**DEBUG**
@24,30 SAY "At FAT"

IF PCOUNT() < 3                        && Is f_rom passed?
   f_rom := 1
ENDIF

ret_val := AT(f_str, SUBSTR(l_str, f_rom))

RETURN ret_val
