/*
 *
 * This file contains the exportable functions available to the Harbour program
 *
 * Currently being discussed in 'Static initializers'
 *
 * If the discussion has finished, it can be removed from here.
 *
 */
HARBOUR ARRAY();
HARBOUR ASIZE();
HARBOUR ATAIL();
HARBOUR AINS();
HARBOUR ADEL();
HARBOUR AFILL();
HARBOUR ASCAN();
HARBOUR AEVAL();
HARBOUR ACOPY();
HARBOUR ACLONE();
HARBOUR __ACCEPT();
HARBOUR OUTSTD();
HARBOUR OUTERR();
HARBOUR DEVPOS();
HARBOUR DEVOUT();
HARBOUR EJECT();
HARBOUR QQOUT();
HARBOUR QOUT();
HARBOUR ERRORSYS();
HARBOUR ERRORNEW();
HARBOUR EVAL();
HARBOUR VALTYPE();
HARBOUR ASORT();
HARBOUR STOD();
HARBOUR HB_SETCENTURY();
HARBOUR SET();
HARBOUR OS();
HARBOUR FOPEN();
HARBOUR FCREATE();
HARBOUR FREAD();
HARBOUR FWRITE();
HARBOUR FERROR();
HARBOUR FCLOSE();
HARBOUR FERASE();
HARBOUR FRENAME();
HARBOUR FSEEK();
HARBOUR HB_FILE();
HARBOUR FREADSTR();
HARBOUR BIN2I();
HARBOUR BIN2L();
HARBOUR BIN2W();
HARBOUR I2BIN();
HARBOUR L2BIN();
HARBOUR W2BIN();
HARBOUR MOD();
HARBOUR DEFAULT();
HARBOUR TOCHAR();
HARBOUR HBDEBUG();
HARBOUR ISALPHA();
HARBOUR ISDIGIT();
HARBOUR ISUPPER();
HARBOUR ISLOWER();
HARBOUR LTRIM();
HARBOUR TRIM();
HARBOUR ALLTRIM();
HARBOUR PADR();
HARBOUR PAD();
HARBOUR PADL();
HARBOUR PADC();
HARBOUR RAT();
HARBOUR RIGHT();
HARBOUR SPACE();
HARBOUR STUFF();
HARBOUR STRTRAN();
HARBOUR DATETIME();
HARBOUR __ASTATIC();
HARBOUR __STATIC();
HARBOUR __GLOBALSTACKLEN();
HARBOUR __AGLOBALSTACK();
HARBOUR __STACKLEN();
HARBOUR __ASTACK();
HARBOUR __APARAM();
HARBOUR ACOS();
HARBOUR ASIN();
HARBOUR ATAN();
HARBOUR COS();
HARBOUR COSH();
HARBOUR LOG10();
HARBOUR SIN();
HARBOUR SINH();
HARBOUR TAN();
HARBOUR TANH();
HARBOUR STRDUMP();
HARBOUR STRTOKEN();
HARBOUR ROT13();
HARBOUR PVALUE();
HARBOUR HB_FUSE();
HARBOUR HB_FRECNO();
HARBOUR HB_FLASTREC();
HARBOUR HB_FGOTOP();
HARBOUR HB_FGOBOTTOM();
HARBOUR HB_FGOTO();
HARBOUR HB_FEOF();
HARBOUR HB_FREADLN();
HARBOUR HB_FSKIP();
HARBOUR GETENV();
HARBOUR DIRECTORY();
HARBOUR GT_ASCPOS();
HARBOUR GT_ATDIFF();
HARBOUR GT_CHAREVEN();
HARBOUR GT_CHARODD();
HARBOUR GT_CHRCOUNT();
HARBOUR GT_CHRTOTAL();
HARBOUR GT_CHARMIX();
HARBOUR GT_ASCIISUM();
HARBOUR GT_CHRFIRST();
HARBOUR GT_STRCOUNT();
HARBOUR GT_STRCSPN();
HARBOUR GT_STRDIFF();
HARBOUR GT_STREXPAND();
HARBOUR GT_STRLEFT();
HARBOUR GT_STRPBRK();
HARBOUR GT_STRRIGHT();
HARBOUR MAXROW();
HARBOUR MAXCOL();
HARBOUR SETPRC();
HARBOUR SCROLL();

/* Same story.

   All the function pointers of the internal functions
   Including Runner itself, since the first symbol gets executed by Harbour ;-)
*/

static SYMBOL symbols[] = {
{ "HB_RUN",         FS_PUBLIC, HB_RUN        , 0 },
{ "ARRAY",          FS_PUBLIC, ARRAY         , 0 },
{ "ASIZE",          FS_PUBLIC, ASIZE         , 0 },
{ "ATAIL",          FS_PUBLIC, ATAIL         , 0 },
{ "AINS",           FS_PUBLIC, AINS          , 0 },
{ "ADEL",           FS_PUBLIC, ADEL          , 0 },
{ "AFILL",          FS_PUBLIC, AFILL         , 0 },
{ "ASCAN",          FS_PUBLIC, ASCAN         , 0 },
{ "AEVAL",          FS_PUBLIC, AEVAL         , 0 },
{ "ACOPY",          FS_PUBLIC, ACOPY         , 0 },
{ "ACLONE",         FS_PUBLIC, ACLONE        , 0 },
{ "__ACCEPT",       FS_PUBLIC, __ACCEPT      , 0 },
{ "OUTSTD",         FS_PUBLIC, OUTSTD        , 0 },
{ "OUTERR",         FS_PUBLIC, OUTERR        , 0 },
{ "DEVPOS",         FS_PUBLIC, DEVPOS        , 0 },
{ "DEVOUT",         FS_PUBLIC, DEVOUT        , 0 },
{ "EJECT",          FS_PUBLIC, EJECT         , 0 },
{ "QQOUT",          FS_PUBLIC, QQOUT         , 0 },
{ "QOUT",           FS_PUBLIC, QOUT          , 0 },
{ "ERRORSYS",       FS_PUBLIC, ERRORSYS      , 0 },
{ "ERRORNEW",       FS_PUBLIC, ERRORNEW      , 0 },
{ "EVAL",           FS_PUBLIC, EVAL          , 0 },
{ "VALTYPE",        FS_PUBLIC, VALTYPE       , 0 },
{ "ASORT",          FS_PUBLIC, ASORT         , 0 },
{ "STOD",           FS_PUBLIC, STOD          , 0 },
{ "HB_SETCENTURY",  FS_PUBLIC, HB_SETCENTURY , 0 },
{ "SET",            FS_PUBLIC, SET           , 0 },
{ "OS",             FS_PUBLIC, OS            , 0 },
{ "FOPEN",          FS_PUBLIC, FOPEN         , 0 },
{ "FCREATE",        FS_PUBLIC, FCREATE       , 0 },
{ "FREAD",          FS_PUBLIC, FREAD         , 0 },
{ "FWRITE",         FS_PUBLIC, FWRITE        , 0 },
{ "FERROR",         FS_PUBLIC, FERROR        , 0 },
{ "FCLOSE",         FS_PUBLIC, FCLOSE        , 0 },
{ "FERASE",         FS_PUBLIC, FERASE        , 0 },
{ "FRENAME",        FS_PUBLIC, FRENAME       , 0 },
{ "FSEEK",          FS_PUBLIC, FSEEK         , 0 },
{ "HB_FILE",        FS_PUBLIC, HB_FILE       , 0 },
{ "FREADSTR",       FS_PUBLIC, FREADSTR      , 0 },
{ "BIN2I",          FS_PUBLIC, BIN2I         , 0 },
{ "BIN2L",          FS_PUBLIC, BIN2L         , 0 },
{ "BIN2W",          FS_PUBLIC, BIN2W         , 0 },
{ "I2BIN",          FS_PUBLIC, I2BIN         , 0 },
{ "L2BIN",          FS_PUBLIC, L2BIN         , 0 },
{ "W2BIN",          FS_PUBLIC, W2BIN         , 0 },
{ "MOD",            FS_PUBLIC, MOD           , 0 },
{ "DEFAULT",        FS_PUBLIC, DEFAULT       , 0 },
{ "TOCHAR",         FS_PUBLIC, TOCHAR        , 0 },
{ "HBDEBUG",        FS_PUBLIC, HBDEBUG       , 0 },
{ "ISALPHA",        FS_PUBLIC, ISALPHA       , 0 },
{ "ISDIGIT",        FS_PUBLIC, ISDIGIT       , 0 },
{ "ISUPPER",        FS_PUBLIC, ISUPPER       , 0 },
{ "ISLOWER",        FS_PUBLIC, ISLOWER       , 0 },
{ "LTRIM",          FS_PUBLIC, LTRIM         , 0 },
{ "TRIM",           FS_PUBLIC, TRIM          , 0 },
{ "ALLTRIM",        FS_PUBLIC, ALLTRIM       , 0 },
{ "PADR",           FS_PUBLIC, PADR          , 0 },
{ "PAD",            FS_PUBLIC, PAD           , 0 },
{ "PADL",           FS_PUBLIC, PADL          , 0 },
{ "PADC",           FS_PUBLIC, PADC          , 0 },
{ "RAT",            FS_PUBLIC, RAT           , 0 },
{ "RIGHT",          FS_PUBLIC, RIGHT         , 0 },
{ "SPACE",          FS_PUBLIC, SPACE         , 0 },
{ "STUFF",          FS_PUBLIC, STUFF         , 0 },
{ "STRTRAN",        FS_PUBLIC, STRTRAN       , 0 },
{ "DATETIME",       FS_PUBLIC, DATETIME      , 0 },
{ "__ASTATIC",      FS_PUBLIC, __ASTATIC     , 0 },
{ "__STATIC",       FS_PUBLIC, __STATIC      , 0 },
{ "__GLOBALSTACKLEN", FS_PUBLIC, __GLOBALSTACKLEN, 0 },
{ "__AGLOBALSTACK", FS_PUBLIC, __AGLOBALSTACK, 0 },
{ "__STACKLEN",     FS_PUBLIC, __STACKLEN    , 0 },
{ "__ASTACK",       FS_PUBLIC, __ASTACK      , 0 },
{ "__APARAM",       FS_PUBLIC, __APARAM      , 0 },
{ "ACOS",           FS_PUBLIC, ACOS          , 0 },
{ "ASIN",           FS_PUBLIC, ASIN          , 0 },
{ "ATAN",           FS_PUBLIC, ATAN          , 0 },
{ "COS",            FS_PUBLIC, COS           , 0 },
{ "COSH",           FS_PUBLIC, COSH          , 0 },
{ "LOG10",          FS_PUBLIC, LOG10         , 0 },
{ "SIN",            FS_PUBLIC, SIN           , 0 },
{ "SINH",           FS_PUBLIC, SINH          , 0 },
{ "TAN",            FS_PUBLIC, TAN           , 0 },
{ "TANH",           FS_PUBLIC, TANH          , 0 },
{ "STRDUMP",        FS_PUBLIC, STRDUMP       , 0 },
{ "STRTOKEN",       FS_PUBLIC, STRTOKEN      , 0 },
{ "ROT13",          FS_PUBLIC, ROT13         , 0 },
{ "PVALUE",         FS_PUBLIC, PVALUE        , 0 },
{ "HB_FUSE",        FS_PUBLIC, HB_FUSE       , 0 },
{ "HB_FRECNO",      FS_PUBLIC, HB_FRECNO     , 0 },
{ "HB_FLASTREC",    FS_PUBLIC, HB_FLASTREC   , 0 },
{ "HB_FGOTOP",      FS_PUBLIC, HB_FGOTOP     , 0 },
{ "HB_FGOBOTTOM",   FS_PUBLIC, HB_FGOBOTTOM  , 0 },
{ "HB_FGOTO",       FS_PUBLIC, HB_FGOTO      , 0 },
{ "HB_FEOF",        FS_PUBLIC, HB_FEOF       , 0 },
{ "HB_FREADLN",     FS_PUBLIC, HB_FREADLN    , 0 },
{ "HB_FSKIP",       FS_PUBLIC, HB_FSKIP      , 0 },
{ "GETENV",         FS_PUBLIC, GETENV        , 0 },
{ "DIRECTORY",      FS_PUBLIC, DIRECTORY     , 0 },
{ "GT_ASCPOS",      FS_PUBLIC, GT_ASCPOS     , 0 },
{ "GT_ATDIFF",      FS_PUBLIC, GT_ATDIFF     , 0 },
{ "GT_CHAREVEN",    FS_PUBLIC, GT_CHAREVEN   , 0 },
{ "GT_CHARODD",     FS_PUBLIC, GT_CHARODD    , 0 },
{ "GT_CHRCOUNT",    FS_PUBLIC, GT_CHRCOUNT   , 0 },
{ "GT_CHRTOTAL",    FS_PUBLIC, GT_CHRTOTAL   , 0 },
{ "GT_CHARMIX",     FS_PUBLIC, GT_CHARMIX    , 0 },
{ "GT_ASCIISUM",    FS_PUBLIC, GT_ASCIISUM   , 0 },
{ "GT_CHRFIRST",    FS_PUBLIC, GT_CHRFIRST   , 0 },
{ "GT_STRCOUNT",    FS_PUBLIC, GT_STRCOUNT   , 0 },
{ "GT_STRCSPN",     FS_PUBLIC, GT_STRCSPN    , 0 },
{ "GT_STRDIFF",     FS_PUBLIC, GT_STRDIFF    , 0 },
{ "GT_STREXPAND",   FS_PUBLIC, GT_STREXPAND  , 0 },
{ "GT_STRLEFT",     FS_PUBLIC, GT_STRLEFT    , 0 },
{ "GT_STRPBRK",     FS_PUBLIC, GT_STRPBRK    , 0 },
{ "GT_STRRIGHT",    FS_PUBLIC, GT_STRRIGHT   , 0 },
{ "MAXROW",         FS_PUBLIC, MAXROW        , 0 },
{ "MAXCOL",         FS_PUBLIC, MAXCOL        , 0 },
{ "SETPRC",         FS_PUBLIC, SETPRC        , 0 },
{ "SCROLL",         FS_PUBLIC, SCROLL        , 0 }
};



