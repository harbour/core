/*
 *
 * This file contains the exportable functions available to the Harbour program
 *
 * Currently being discussed in 'Static initializers'
 *
 * If the discussion has finished, it can be removed from here.
 *
 */
HARBOUR HB___ACCEPT();
HARBOUR HB_OUTSTD();
HARBOUR HB_OUTERR();
HARBOUR HB_DEVPOS();
HARBOUR HB_DEVOUT();
HARBOUR HB_EJECT();
HARBOUR HB_QQOUT();
HARBOUR HB_QOUT();
HARBOUR HB_ERRORSYS();
HARBOUR HB_ERRORNEW();
HARBOUR HB_EVAL();
HARBOUR HB_VALTYPE();
HARBOUR HB_ASORT();
HARBOUR HB_STOD();
HARBOUR HB_SETCENTURY();
HARBOUR HB_SET();
HARBOUR HB_OS();
HARBOUR HB_FOPEN();
HARBOUR HB_FCREATE();
HARBOUR HB_FREAD();
HARBOUR HB_FWRITE();
HARBOUR HB_FERROR();
HARBOUR HB_FCLOSE();
HARBOUR HB_FERASE();
HARBOUR HB_FRENAME();
HARBOUR HB_FSEEK();
HARBOUR HB_FILE();
HARBOUR HB_FREADSTR();
HARBOUR HB_BIN2I();
HARBOUR HB_BIN2L();
HARBOUR HB_BIN2W();
HARBOUR HB_I2BIN();
HARBOUR HB_L2BIN();
HARBOUR HB_W2BIN();
HARBOUR HB_EXP();
HARBOUR HB_LOG();
HARBOUR HB_MOD();
HARBOUR HB_DEFAULT();
HARBOUR HB_TOCHAR();
HARBOUR HB_DEBUG();
HARBOUR HB_ISALPHA();
HARBOUR HB_ISDIGIT();
HARBOUR HB_ISUPPER();
HARBOUR HB_ISLOWER();
HARBOUR HB_LTRIM();
HARBOUR HB_TRIM();
HARBOUR HB_ALLTRIM();
HARBOUR HB_PADR();
HARBOUR HB_PAD();
HARBOUR HB_PADL();
HARBOUR HB_PADC();
HARBOUR HB_RAT();
HARBOUR HB_RIGHT();
HARBOUR HB_SPACE();
HARBOUR HB_STUFF();
HARBOUR HB_STRTRAN();
HARBOUR HB_DATETIME();
HARBOUR HB_DOW();
HARBOUR HB___ASTATIC();
HARBOUR HB___STATIC();
HARBOUR HB___GLOBALSTACKLEN();
HARBOUR HB___AGLOBALSTACK();
HARBOUR HB___STACKLEN();
HARBOUR HB___ASTACK();
HARBOUR HB___APARAM();
HARBOUR HB_ACOS();
HARBOUR HB_ASIN();
HARBOUR HB_ATAN();
HARBOUR HB_COS();
HARBOUR HB_COSH();
HARBOUR HB_LOG10();
HARBOUR HB_SIN();
HARBOUR HB_SINH();
HARBOUR HB_TAN();
HARBOUR HB_TANH();
HARBOUR HB_STRDUMP();
HARBOUR HB_STRTOKEN();
HARBOUR HB_ROT13();
HARBOUR HB_PVALUE();
HARBOUR HB_HB_FUSE();
HARBOUR HB_HB_FRECNO();
HARBOUR HB_HB_FLASTREC();
HARBOUR HB_HB_FGOTOP();
HARBOUR HB_HB_FGOBOTTOM();
HARBOUR HB_HB_FGOTO();
HARBOUR HB_HB_FEOF();
HARBOUR HB_HB_FREADLN();
HARBOUR HB_HB_FSKIP();
HARBOUR HB_GETENV();
HARBOUR HB_DIRECTORY();
HARBOUR HB_GT_ASCPOS();
HARBOUR HB_GT_ATDIFF();
HARBOUR HB_GT_CHAREVEN();
HARBOUR HB_GT_CHARODD();
HARBOUR HB_GT_CHRCOUNT();
HARBOUR HB_GT_CHRTOTAL();
HARBOUR HB_GT_CHARMIX();
HARBOUR HB_GT_ASCIISUM();
HARBOUR HB_GT_CHRFIRST();
HARBOUR HB_GT_STRCOUNT();
HARBOUR HB_GT_STRCSPN();
HARBOUR HB_GT_STRDIFF();
HARBOUR HB_GT_STREXPAND();
HARBOUR HB_GT_STRLEFT();
HARBOUR HB_GT_STRPBRK();
HARBOUR HB_GT_STRRIGHT();
HARBOUR HB_MAXROW();
HARBOUR HB_MAXCOL();
HARBOUR HB_SETPRC();
HARBOUR HB_SCROLL();
HARBOUR HB_VERSION();
HARBOUR HB_SETFIXED();

/* Same story.

   All the function pointers of the internal functions
   Including Runner itself, since the first symbol gets executed by Harbour ;-)
*/

static SYMBOL symbols[] = {
{ "HB_RUN",         FS_PUBLIC, HB_HB_RUN        , 0 },
{ "__ACCEPT",       FS_PUBLIC, HB___ACCEPT      , 0 },
{ "OUTSTD",         FS_PUBLIC, HB_OUTSTD        , 0 },
{ "OUTERR",         FS_PUBLIC, HB_OUTERR        , 0 },
{ "DEVPOS",         FS_PUBLIC, HB_DEVPOS        , 0 },
{ "DEVOUT",         FS_PUBLIC, HB_DEVOUT        , 0 },
{ "EJECT",          FS_PUBLIC, HB_EJECT         , 0 },
{ "QQOUT",          FS_PUBLIC, HB_QQOUT         , 0 },
{ "QOUT",           FS_PUBLIC, HB_QOUT          , 0 },
{ "ERRORSYS",       FS_PUBLIC, HB_ERRORSYS      , 0 },
{ "ERRORNEW",       FS_PUBLIC, HB_ERRORNEW      , 0 },
{ "EVAL",           FS_PUBLIC, HB_EVAL          , 0 },
{ "VALTYPE",        FS_PUBLIC, HB_VALTYPE       , 0 },
{ "ASORT",          FS_PUBLIC, HB_ASORT         , 0 },
{ "STOD",           FS_PUBLIC, HB_STOD          , 0 },
{ "SETCENTURY",     FS_PUBLIC, HB_SETCENTURY    , 0 },
{ "SET",            FS_PUBLIC, HB_SET           , 0 },
{ "OS",             FS_PUBLIC, HB_OS            , 0 },
{ "FOPEN",          FS_PUBLIC, HB_FOPEN         , 0 },
{ "FCREATE",        FS_PUBLIC, HB_FCREATE       , 0 },
{ "FREAD",          FS_PUBLIC, HB_FREAD         , 0 },
{ "FWRITE",         FS_PUBLIC, HB_FWRITE        , 0 },
{ "FERROR",         FS_PUBLIC, HB_FERROR        , 0 },
{ "FCLOSE",         FS_PUBLIC, HB_FCLOSE        , 0 },
{ "FERASE",         FS_PUBLIC, HB_FERASE        , 0 },
{ "FRENAME",        FS_PUBLIC, HB_FRENAME       , 0 },
{ "FSEEK",          FS_PUBLIC, HB_FSEEK         , 0 },
{ "FILE",           FS_PUBLIC, HB_FILE          , 0 },
{ "FREADSTR",       FS_PUBLIC, HB_FREADSTR      , 0 },
{ "BIN2I",          FS_PUBLIC, HB_BIN2I         , 0 },
{ "BIN2L",          FS_PUBLIC, HB_BIN2L         , 0 },
{ "BIN2W",          FS_PUBLIC, HB_BIN2W         , 0 },
{ "I2BIN",          FS_PUBLIC, HB_I2BIN         , 0 },
{ "L2BIN",          FS_PUBLIC, HB_L2BIN         , 0 },
{ "W2BIN",          FS_PUBLIC, HB_W2BIN         , 0 },
{ "EXP",            FS_PUBLIC, HB_EXP           , 0 },
{ "LOG",            FS_PUBLIC, HB_LOG           , 0 },
{ "MOD",            FS_PUBLIC, HB_MOD           , 0 },
{ "DEFAULT",        FS_PUBLIC, HB_DEFAULT       , 0 },
{ "TOCHAR",         FS_PUBLIC, HB_TOCHAR        , 0 },
{ "DEBUG",          FS_PUBLIC, HB_DEBUG         , 0 },
{ "ISALPHA",        FS_PUBLIC, HB_ISALPHA       , 0 },
{ "ISDIGIT",        FS_PUBLIC, HB_ISDIGIT       , 0 },
{ "ISUPPER",        FS_PUBLIC, HB_ISUPPER       , 0 },
{ "ISLOWER",        FS_PUBLIC, HB_ISLOWER       , 0 },
{ "LTRIM",          FS_PUBLIC, HB_LTRIM         , 0 },
{ "TRIM",           FS_PUBLIC, HB_TRIM          , 0 },
{ "ALLTRIM",        FS_PUBLIC, HB_ALLTRIM       , 0 },
{ "PADR",           FS_PUBLIC, HB_PADR          , 0 },
{ "PAD",            FS_PUBLIC, HB_PAD           , 0 },
{ "PADL",           FS_PUBLIC, HB_PADL          , 0 },
{ "PADC",           FS_PUBLIC, HB_PADC          , 0 },
{ "RAT",            FS_PUBLIC, HB_RAT           , 0 },
{ "RIGHT",          FS_PUBLIC, HB_RIGHT         , 0 },
{ "SPACE",          FS_PUBLIC, HB_SPACE         , 0 },
{ "STUFF",          FS_PUBLIC, HB_STUFF         , 0 },
{ "STRTRAN",        FS_PUBLIC, HB_STRTRAN       , 0 },
{ "DATETIME",       FS_PUBLIC, HB_DATETIME      , 0 },
{ "DOW",            FS_PUBLIC, HB_DOW           , 0 },
{ "__ASTATIC",      FS_PUBLIC, HB___ASTATIC     , 0 },
{ "__STATIC",       FS_PUBLIC, HB___STATIC      , 0 },
{ "__GLOBALSTACKLEN", FS_PUBLIC, HB___GLOBALSTACKLEN, 0 },
{ "__AGLOBALSTACK", FS_PUBLIC, HB___AGLOBALSTACK, 0 },
{ "__STACKLEN",     FS_PUBLIC, HB___STACKLEN    , 0 },
{ "__ASTACK",       FS_PUBLIC, HB___ASTACK      , 0 },
{ "__APARAM",       FS_PUBLIC, HB___APARAM      , 0 },
{ "ACOS",           FS_PUBLIC, HB_ACOS          , 0 },
{ "ASIN",           FS_PUBLIC, HB_ASIN          , 0 },
{ "ATAN",           FS_PUBLIC, HB_ATAN          , 0 },
{ "COS",            FS_PUBLIC, HB_COS           , 0 },
{ "COSH",           FS_PUBLIC, HB_COSH          , 0 },
{ "LOG10",          FS_PUBLIC, HB_LOG10         , 0 },
{ "SIN",            FS_PUBLIC, HB_SIN           , 0 },
{ "SINH",           FS_PUBLIC, HB_SINH          , 0 },
{ "TAN",            FS_PUBLIC, HB_TAN           , 0 },
{ "TANH",           FS_PUBLIC, HB_TANH          , 0 },
{ "STRDUMP",        FS_PUBLIC, HB_STRDUMP       , 0 },
{ "STRTOKEN",       FS_PUBLIC, HB_STRTOKEN      , 0 },
{ "ROT13",          FS_PUBLIC, HB_ROT13         , 0 },
{ "PVALUE",         FS_PUBLIC, HB_PVALUE        , 0 },
{ "HB_FUSE",        FS_PUBLIC, HB_HB_FUSE       , 0 },
{ "HB_FRECNO",      FS_PUBLIC, HB_HB_FRECNO     , 0 },
{ "HB_FLASTREC",    FS_PUBLIC, HB_HB_FLASTREC   , 0 },
{ "HB_FGOTOP",      FS_PUBLIC, HB_HB_FGOTOP     , 0 },
{ "HB_FGOBOTTOM",   FS_PUBLIC, HB_HB_FGOBOTTOM  , 0 },
{ "HB_FGOTO",       FS_PUBLIC, HB_HB_FGOTO      , 0 },
{ "HB_FEOF",        FS_PUBLIC, HB_HB_FEOF       , 0 },
{ "HB_FREADLN",     FS_PUBLIC, HB_HB_FREADLN    , 0 },
{ "HB_FSKIP",       FS_PUBLIC, HB_HB_FSKIP      , 0 },
{ "GETENV",         FS_PUBLIC, HB_GETENV        , 0 },
{ "DIRECTORY",      FS_PUBLIC, HB_DIRECTORY     , 0 },
{ "GT_ASCPOS",      FS_PUBLIC, HB_GT_ASCPOS     , 0 },
{ "GT_ATDIFF",      FS_PUBLIC, HB_GT_ATDIFF     , 0 },
{ "GT_CHAREVEN",    FS_PUBLIC, HB_GT_CHAREVEN   , 0 },
{ "GT_CHARODD",     FS_PUBLIC, HB_GT_CHARODD    , 0 },
{ "GT_CHRCOUNT",    FS_PUBLIC, HB_GT_CHRCOUNT   , 0 },
{ "GT_CHRTOTAL",    FS_PUBLIC, HB_GT_CHRTOTAL   , 0 },
{ "GT_CHARMIX",     FS_PUBLIC, HB_GT_CHARMIX    , 0 },
{ "GT_ASCIISUM",    FS_PUBLIC, HB_GT_ASCIISUM   , 0 },
{ "GT_CHRFIRST",    FS_PUBLIC, HB_GT_CHRFIRST   , 0 },
{ "GT_STRCOUNT",    FS_PUBLIC, HB_GT_STRCOUNT   , 0 },
{ "GT_STRCSPN",     FS_PUBLIC, HB_GT_STRCSPN    , 0 },
{ "GT_STRDIFF",     FS_PUBLIC, HB_GT_STRDIFF    , 0 },
{ "GT_STREXPAND",   FS_PUBLIC, HB_GT_STREXPAND  , 0 },
{ "GT_STRLEFT",     FS_PUBLIC, HB_GT_STRLEFT    , 0 },
{ "GT_STRPBRK",     FS_PUBLIC, HB_GT_STRPBRK    , 0 },
{ "GT_STRRIGHT",    FS_PUBLIC, HB_GT_STRRIGHT   , 0 },
{ "MAXROW",         FS_PUBLIC, HB_MAXROW        , 0 },
{ "MAXCOL",         FS_PUBLIC, HB_MAXCOL        , 0 },
{ "SETPRC",         FS_PUBLIC, HB_SETPRC        , 0 },
{ "SCROLL",         FS_PUBLIC, HB_SCROLL        , 0 },
{ "VERSION",        FS_PUBLIC, HB_VERSION       , 0 },
{ "SETFIXED",       FS_PUBLIC, HB_SETFIXED      , 0 }
};



