/*
 * $Id$
 */

//*******************************************************************
// ctoken.ch: Olyan (karakter) elemek, amik speci†lis jelentÇst 
//            hordoznak.
// 199, Csisz†r Levente

// #define ID_STRING
//*******************************************************************
#ifdef ID_STRING
   #define CTKID_BOS            "BOS"
   #define CTKID_EOS            "EOS"
   #define CTKID_EOL            "EOL"
   // #define CTKID_ENDINCLUDE     "End include"
   #define CTKID_PRINTLINE      "PRINTLINE"
#else
   #define CTKID_BOS            1
   #define CTKID_EOS            2
   #define CTKID_EOL            3
   // #define CTKID_ENDINCLUDE     4
   #define CTKID_PRINTLINE      5
#endif
//*******************************************************************

#define CTK_EOF          nil

// id, filename,line,pos,deep
#define CTK_BOS          {CTKID_BOS,"",0,0,0} 

// Itt a pos az utols¢ sor hossza. Ha 1, akkor az utols¢ sor Åres 
// volt.
// id, filename,line,pos,deep
#define CTK_EOS          {CTKID_EOS,"",0,0,0} 

#define CTK_EOL          {CTKID_EOL,CTK_LF,0} // id, eolchars,lineLen
// #define CTK_ENDINCLUDE   {CTKID_ENDINCLUDE,"End include"}
#define CTK_PRINTLINE    {CTKID_PRINTLINE,"PRINTLINE"}

//*******************************************************************
#define CTK_CR         chr(13)
#define CTK_LF         chr(10)
#define CTK_IDEZ       "'"
#define CTK_FIDEZ      "`" // Ezt "'"-nak tekinti.
#define CTK_MACS       '"'
#define CTK_PER        "/"
#define CTK_ET         "&"
#define CTK_CSILLAG    "*"
#define CTK_PVESSZO    ";"

//*******************************************************************
// #define PRSERR_ENDSTRING        "TSND"
// #define PRSERR_ENDPCCOMMENT     "TPCD"

//*******************************************************************
#undef ID_STRING
#ifdef ID_STRING
   #define TKID_BOS                       "BOS"
   #define TKID_EOS                       "EOS"
   #define TKID_EOF                       "EOF"
   #define TKID_URES                      "URES"
   #define TKID_UJSOR                     "UJSOR"
   #define TKID_PVESSZO                   "PVESSZO"
   #define TKID_CHAR                      "CHAR"
   //#define TKID_IDEZ                      "IDEZ"
   //#define TKID_MACS                      "MACS"
   #define TKID_CSCOMMENT                 "CSCOMMENT"
   #define TKID_PPCOMMENT                 "PPCOMMENT"
   #define TKID_PCCOMMENT                 "PCCOMMENT"
   // #define TKID_ENDFILE                   "ENDFILE"
   #define TKID_NEV                       "NEV"
   #define TKID_SZAMTOMB                  "SZAMTOMB"
   //#define TKID_VESSZO                    "VESSZO"
   //#define TKID_GNYITO                    "GNYITO"
   //#define TKID_GZARO                     "GZARO"
   //#define TKID_KNYITO                    "KNYITO"
   //#define TKID_KZARO                     "KZARO"
   //#define TKID_SNYITO                    "SNYITO"
   //#define TKID_SZARO                     "SZARO"
   //#define TKID_ERROR                     "ERROR"
   #define TKID_PRINTLINE                 "PRINTLINE"
   #define TKID_STRING                    "STRING"
                                          
   #define TKID_MALTERSET                 "MALTERSET"
   #define TKID_REGULAR_MATCH_MARKER      "REGULAR_MATCH_MARKER"
   #define TKID_WILD_MATCH_MARKER         "WILD_MATCH_MARKER"
   #define TKID_EXT_EXPR_MATCH_MARKER     "EXT_EXPR_MATCH_MARKER"
   #define TKID_LIST_MATCH_MARKER         "LIST_MATCH_MARKER"
   #define TKID_RESTRICTED_MATCH_MARKER   "RESTRICTED_MATCH_MARKER"


   #define TKID_RALTER                    "RALTER"
   #define TKID_DUMB_STR_RESULT_MARKER    "DUMB_STR_RESULT_MARKER"
   #define TKID_REGULAR_RESULT_MARKER     "REGULAR_RESULT_MARKER"
   #define TKID_STRINGIFY_RESULT_MARKER   "STRINGIFY_RESULT_MARKER"
   #define TKID_SMART_STR_RESULT_MARKER   "SMART_STR_RESULT_MARKER"
   #define TKID_BLOCKIFY_RESULT_MARKER    "BLOCKIFY_RESULT_MARKER"
   #define TKID_LOGIFY_RESULT_MARKER      "LOGIFY_RESULT_MARKER"
#else
   #define TKID_BOS                       1
   #define TKID_EOS                       2
   #define TKID_EOF                       3
   #define TKID_URES                      4
   #define TKID_UJSOR                     5
   #define TKID_PVESSZO                   6
   #define TKID_CHAR                      7
   #define TKID_CSCOMMENT                 8
   #define TKID_PPCOMMENT                 9
   #define TKID_PCCOMMENT                10 
   #define TKID_NEV                      11 
   #define TKID_SZAMTOMB                 12 
   #define TKID_PRINTLINE                13 
   #define TKID_STRING                   14 
                                         
   #define TKID_MALTERSET                30 
   #define TKID_REGULAR_MATCH_MARKER     31
   #define TKID_WILD_MATCH_MARKER        32 
   #define TKID_EXT_EXPR_MATCH_MARKER    33 
   #define TKID_LIST_MATCH_MARKER        34 
   #define TKID_RESTRICTED_MATCH_MARKER  35


   #define TKID_RALTER                   40 
   #define TKID_DUMB_STR_RESULT_MARKER   41
   #define TKID_REGULAR_RESULT_MARKER    42 
   #define TKID_STRINGIFY_RESULT_MARKER  43 
   #define TKID_SMART_STR_RESULT_MARKER  44 
   #define TKID_BLOCKIFY_RESULT_MARKER   45 
   #define TKID_LOGIFY_RESULT_MARKER     46 
#endif
//*******************************************************************
#define TKCL_NEV      "nev"
#define TKCL_STRING   '"string"'
#define TKCL_SZAMTOMB "1"
#define TKCL_PARENT   "(a)"
   
#define TKCL_TRUE     ".T."
//*******************************************************************

#define makeHSLineStr(file,line) ("#line "+toStr(line)+" "+'"'+(file)+'"'+guessedEol())

//*******************************************************************
#define XTRTYPE_XTRANSLATE 0
#define XTRTYPE_XCOMMAND   1
#define XTRTYPE_TRANSLATE  2
#define XTRTYPE_COMMAND    3

//*******************************************************************
#define TRPRA_TREE  1
#define TRPRA_SEQ   2
//*******************************************************************

