/* $Id$
 *
 * Initiialization of runtime support symbols
 */
#include "hbsetup.h"
#include "extend.h"
#include "types.h"

void ProcessSymbols( SYMBOL *, WORD );

HARBOUR AADD( void );
HARBOUR ABS( void );
HARBOUR ASC( void );
HARBOUR AT( void );
HARBOUR CHR( void );
HARBOUR CTOD( void );
HARBOUR DATE( void );
HARBOUR DAY( void );
HARBOUR DTOC( void );
HARBOUR DTOS( void );
HARBOUR EMPTY( void );
HARBOUR EXP( void );
HARBOUR INT( void );
HARBOUR LEFT( void );
HARBOUR LEN( void );
HARBOUR LOG( void );
HARBOUR LOWER( void );
HARBOUR LTRIM( void );
HARBOUR MAX( void );
HARBOUR MIN( void );
HARBOUR MONTH( void );
HARBOUR PCOUNT( void );
HARBOUR REPLICATE( void );
HARBOUR RTRIM( void );
HARBOUR SPACE( void );
HARBOUR SQRT( void );
HARBOUR STR( void );
HARBOUR STR( void );
HARBOUR SUBSTR( void );
HARBOUR TIME( void );
HARBOUR TRANSFORM( void );
HARBOUR TRIM( void );
HARBOUR UPPER( void );
HARBOUR VAL( void );
HARBOUR YEAR( void );


static SYMBOL symbols[] = {
    { "AADD"      , FS_PUBLIC, AADD         , 0 },
    { "ABS"       , FS_PUBLIC, ABS          , 0 },
    { "ASC"       , FS_PUBLIC, ASC          , 0 },
    { "AT"        , FS_PUBLIC, AT           , 0 },
    { "BOF"       , FS_PUBLIC, NULL         , 0 },
    { "BREAK"     , FS_PUBLIC, NULL         , 0 },
    { "CDOW"      , FS_PUBLIC, NULL         , 0 },
    { "CHR"       , FS_PUBLIC, CHR          , 0 },
    { "CMONTH"    , FS_PUBLIC, NULL         , 0 },
    { "COL"       , FS_PUBLIC, NULL         , 0 },
    { "CTOD"      , FS_PUBLIC, CTOD         , 0 },
    { "DATE"      , FS_PUBLIC, DATE         , 0 },
    { "DAY"       , FS_PUBLIC, DAY          , 0 },
    { "DELETED"   , FS_PUBLIC, NULL         , 0 },
    { "DEVPOS"    , FS_PUBLIC, NULL         , 0 },
    { "DO"        , FS_PUBLIC, NULL         , 0 },
    { "DOW"       , FS_PUBLIC, NULL         , 0 },
    { "DTOC"      , FS_PUBLIC, DTOC         , 0 },
    { "DTOS"      , FS_PUBLIC, DTOS         , 0 },
    { "EMPTY"     , FS_PUBLIC, EMPTY        , 0 },
    { "EOF"       , FS_PUBLIC, NULL         , 0 },
    { "EXP"       , FS_PUBLIC, EXP          , 0 },
    { "FCOUNT"    , FS_PUBLIC, NULL         , 0 },
    { "FIELDNAME" , FS_PUBLIC, NULL         , 0 },
    { "FLOCK"     , FS_PUBLIC, NULL         , 0 },
    { "FOUND"     , FS_PUBLIC, NULL         , 0 },
    { "INKEY"     , FS_PUBLIC, NULL         , 0 },
    { "INT"       , FS_PUBLIC, INT          , 0 },
    { "LASTREC"   , FS_PUBLIC, NULL         , 0 },
    { "LEFT"      , FS_PUBLIC, LEFT         , 0 },
    { "LEN"       , FS_PUBLIC, LEN          , 0 },
    { "LOCK"      , FS_PUBLIC, NULL         , 0 },
    { "LOG"       , FS_PUBLIC, LOG          , 0 },
    { "LOWER"     , FS_PUBLIC, LOWER        , 0 },
    { "LTRIM"     , FS_PUBLIC, LTRIM        , 0 },
    { "MAX"       , FS_PUBLIC, MAX          , 0 },
    { "MIN"       , FS_PUBLIC, MIN          , 0 },
    { "MONTH"     , FS_PUBLIC, MONTH        , 0 },
    { "PCOL"      , FS_PUBLIC, NULL         , 0 },
    { "PCOUNT"    , FS_PUBLIC, PCOUNT       , 0 },
    { "PROW"      , FS_PUBLIC, NULL         , 0 },
    { "QSELF"     , FS_PUBLIC, NULL         , 0 },
    { "RECCOUNT"  , FS_PUBLIC, NULL         , 0 },
    { "RECNO"     , FS_PUBLIC, NULL         , 0 },
    { "REPLICATE" , FS_PUBLIC, REPLICATE    , 0 },
    { "RLOCK"     , FS_PUBLIC, NULL         , 0 },
    { "ROUND"     , FS_PUBLIC, NULL         , 0 },
    { "ROW"       , FS_PUBLIC, NULL         , 0 },
    { "RTRIM"     , FS_PUBLIC, RTRIM        , 0 },
    { "SECONDS"   , FS_PUBLIC, NULL         , 0 },
    { "SELECT"    , FS_PUBLIC, NULL         , 0 },
    { "SETPOS"    , FS_PUBLIC, NULL         , 0 },
    { "SPACE"     , FS_PUBLIC, SPACE        , 0 },
    { "SQRT"      , FS_PUBLIC, SQRT         , 0 },
    { "STR"       , FS_PUBLIC, STR          , 0 },
    { "SUBSTR"    , FS_PUBLIC, SUBSTR       , 0 },
    { "TIME"      , FS_PUBLIC, TIME         , 0 },
    { "TRANSFORM" , FS_PUBLIC, TRANSFORM    , 0 },
    { "TRIM"      , FS_PUBLIC, TRIM         , 0 },
    { "TYPE"      , FS_PUBLIC, NULL         , 0 },
    { "UPPER"     , FS_PUBLIC, UPPER        , 0 },
    { "VAL"       , FS_PUBLIC, VAL          , 0 },
    { "WORD"      , FS_PUBLIC, NULL         , 0 },
    { "YEAR"      , FS_PUBLIC, YEAR         , 0 }
  };

/*
 * Registers runtime support functions symbols
 */
void InitSymbolTable( void )
{
  /*
   * Place here your <modulename>__InitSymbols functions
   */
  Classes__InitSymbols();
  Descend__InitSymbols();

  /*
   * The system symbol table with runtime functions HAVE TO be called last
   */
  ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}
