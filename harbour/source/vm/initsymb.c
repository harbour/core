/*
 * $Id$
 */

/*
 * Initialization of runtime support symbols
 */

#include "hbsetup.h"
#include "extend.h"
#include "hbdefs.h"

extern void Arrays__InitSymbols( void );
extern void Classes__InitSymbols( void );
extern void Console__InitSymbols( void );
extern void Dates__InitSymbols( void );
extern void Descend__InitSymbols( void );
extern void Dir__InitSymbols( void );
extern void Environ__InitSymbols( void );
extern void Files__InitSymbols( void );
extern void HardCR__InitSymbols( void );
extern void Math__InitSymbols( void );
extern void Memotran__InitSymbols( void );
extern void Set__InitSymbols( void );
extern void Strings__InitSymbols( void );
extern void Transfrm__InitSymbols( void );

void ProcessSymbols( SYMBOL *, WORD );

HARBOUR HB_AADD( void );
HARBOUR HB_ABS( void );
HARBOUR HB_ASC( void );
HARBOUR HB_AT( void );
HARBOUR HB_CDOW( void );
HARBOUR HB_CHR( void );
HARBOUR HB_CMONTH( void );
HARBOUR HB_COL( void );
HARBOUR HB_CTOD( void );
HARBOUR HB_DATE( void );
HARBOUR HB_DAY( void );
HARBOUR HB_DEVPOS( void );
HARBOUR HB_DOW( void );
HARBOUR HB_DTOC( void );
HARBOUR HB_DTOS( void );
HARBOUR HB_EMPTY( void );
HARBOUR HB_EXP( void );
HARBOUR HB_INT( void );
HARBOUR HB_LEFT( void );
HARBOUR HB_LEN( void );
HARBOUR HB_LOG( void );
HARBOUR HB_LOWER( void );
HARBOUR HB_LTRIM( void );
HARBOUR HB_MAX( void );
HARBOUR HB_MIN( void );
HARBOUR HB_MONTH( void );
HARBOUR HB_PCOUNT( void );
HARBOUR HB_REPLICATE( void );
HARBOUR HB_ROW( void );
HARBOUR HB_ROUND( void );
HARBOUR HB_RTRIM( void );
HARBOUR HB_SECONDS( void );
HARBOUR HB_SETPOS( void );
HARBOUR HB_SPACE( void );
HARBOUR HB_SQRT( void );
HARBOUR HB_STOD( void );
HARBOUR HB_STR( void );
HARBOUR HB_SUBSTR( void );
HARBOUR HB_TIME( void );
HARBOUR HB_TRANSFORM( void );
HARBOUR HB_TRIM( void );
HARBOUR HB_UPPER( void );
HARBOUR HB_VAL( void );
HARBOUR HB_YEAR( void );

static SYMBOL symbols[] = {
    { "AADD"      , FS_PUBLIC, HB_AADD         , 0 },
    { "ABS"       , FS_PUBLIC, HB_ABS          , 0 },
    { "ASC"       , FS_PUBLIC, HB_ASC          , 0 },
    { "AT"        , FS_PUBLIC, HB_AT           , 0 },
    { "BOF"       , FS_PUBLIC, NULL            , 0 },
    { "BREAK"     , FS_PUBLIC, NULL            , 0 },
    { "CDOW"      , FS_PUBLIC, HB_CDOW         , 0 },
    { "CHR"       , FS_PUBLIC, HB_CHR          , 0 },
    { "CMONTH"    , FS_PUBLIC, HB_CMONTH       , 0 },
    { "COL"       , FS_PUBLIC, HB_COL          , 0 },
    { "CTOD"      , FS_PUBLIC, HB_CTOD         , 0 },
    { "DATE"      , FS_PUBLIC, HB_DATE         , 0 },
    { "DAY"       , FS_PUBLIC, HB_DAY          , 0 },
    { "DELETED"   , FS_PUBLIC, NULL            , 0 },
    { "DEVPOS"    , FS_PUBLIC, HB_DEVPOS       , 0 },
    { "DO"        , FS_PUBLIC, NULL            , 0 },
    { "DOW"       , FS_PUBLIC, HB_DOW          , 0 },
    { "DTOC"      , FS_PUBLIC, HB_DTOC         , 0 },
    { "DTOS"      , FS_PUBLIC, HB_DTOS         , 0 },
    { "EMPTY"     , FS_PUBLIC, HB_EMPTY        , 0 },
    { "EOF"       , FS_PUBLIC, NULL            , 0 },
    { "EXP"       , FS_PUBLIC, HB_EXP          , 0 },
    { "FCOUNT"    , FS_PUBLIC, NULL            , 0 },
    { "FIELDNAME" , FS_PUBLIC, NULL            , 0 },
    { "FLOCK"     , FS_PUBLIC, NULL            , 0 },
    { "FOUND"     , FS_PUBLIC, NULL            , 0 },
    { "INKEY"     , FS_PUBLIC, NULL            , 0 },
    { "INT"       , FS_PUBLIC, HB_INT          , 0 },
    { "LASTREC"   , FS_PUBLIC, NULL            , 0 },
    { "LEFT"      , FS_PUBLIC, HB_LEFT         , 0 },
    { "LEN"       , FS_PUBLIC, HB_LEN          , 0 },
    { "LOCK"      , FS_PUBLIC, NULL            , 0 },
    { "LOG"       , FS_PUBLIC, HB_LOG          , 0 },
    { "LOWER"     , FS_PUBLIC, HB_LOWER        , 0 },
    { "LTRIM"     , FS_PUBLIC, HB_LTRIM        , 0 },
    { "MAX"       , FS_PUBLIC, HB_MAX          , 0 },
    { "MIN"       , FS_PUBLIC, HB_MIN          , 0 },
    { "MONTH"     , FS_PUBLIC, HB_MONTH        , 0 },
    { "PCOL"      , FS_PUBLIC, NULL            , 0 },
    { "PCOUNT"    , FS_PUBLIC, HB_PCOUNT       , 0 },
    { "PROW"      , FS_PUBLIC, NULL            , 0 },
    { "QSELF"     , FS_PUBLIC, NULL            , 0 },
    { "RECCOUNT"  , FS_PUBLIC, NULL            , 0 },
    { "RECNO"     , FS_PUBLIC, NULL            , 0 },
    { "REPLICATE" , FS_PUBLIC, HB_REPLICATE    , 0 },
    { "RLOCK"     , FS_PUBLIC, NULL            , 0 },
    { "ROUND"     , FS_PUBLIC, HB_ROUND        , 0 },
    { "ROW"       , FS_PUBLIC, HB_ROW          , 0 },
    { "RTRIM"     , FS_PUBLIC, HB_RTRIM        , 0 },
    { "SECONDS"   , FS_PUBLIC, HB_SECONDS      , 0 },
    { "SELECT"    , FS_PUBLIC, NULL            , 0 },
    { "SETPOS"    , FS_PUBLIC, HB_SETPOS       , 0 },
    { "SPACE"     , FS_PUBLIC, HB_SPACE        , 0 },
    { "SQRT"      , FS_PUBLIC, HB_SQRT         , 0 },
    { "STOD"      , FS_PUBLIC, HB_STOD         , 0 },
    { "STR"       , FS_PUBLIC, HB_STR          , 0 },
    { "SUBSTR"    , FS_PUBLIC, HB_SUBSTR       , 0 },
    { "TIME"      , FS_PUBLIC, HB_TIME         , 0 },
    { "TRANSFORM" , FS_PUBLIC, HB_TRANSFORM    , 0 },
    { "TRIM"      , FS_PUBLIC, HB_TRIM         , 0 },
    { "TYPE"      , FS_PUBLIC, NULL            , 0 },
    { "UPPER"     , FS_PUBLIC, HB_UPPER        , 0 },
    { "VAL"       , FS_PUBLIC, HB_VAL          , 0 },
    { "WORD"      , FS_PUBLIC, NULL            , 0 },
    { "YEAR"      , FS_PUBLIC, HB_YEAR         , 0 }
  };

/*
 * Registers runtime support functions symbols
 */
void InitSymbolTable( void )
{
  /*
   * Place here your user defined <modulename>__InitSymbols functions
   */

   
  /* 
   * The symbol tables from runtime support modules start here
   */ 
#ifdef HARBOUR_STRICT_ANSI_C
  Arrays__InitSymbols();
  Classes__InitSymbols();
  Console__InitSymbols();
  Dates__InitSymbols();
  Descend__InitSymbols();
  Dir__InitSymbols();
  Environ__InitSymbols();
  Files__InitSymbols();
  HardCR__InitSymbols();
  Math__InitSymbols();
  Memotran__InitSymbols();
  Set__InitSymbols();
  Strings__InitSymbols(); 
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY  
  Strings__InitInfinity();
#endif  
  Transfrm__InitSymbols();
#endif

  /*
   * The system symbol table with runtime functions HAVE TO be called last
   */
  ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}
