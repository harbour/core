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
HARBOUR QQOUT();
HARBOUR QOUT();
HARBOUR ERRORSYS();
HARBOUR ERRORNEW();
HARBOUR EVAL();
HARBOUR VALTYPE();
HARBOUR ASORT();
HARBOUR CLASSCREATE();
HARBOUR CLASSADD();
HARBOUR CLASSNAME();
HARBOUR CLASSINSTANCE();
HARBOUR ISMESSAGE();
HARBOUR OSEND();
HARBOUR CLASSMOD();
HARBOUR CLASSDEL();
HARBOUR OCLONE();
HARBOUR STOD();
HARBOUR HB_SETCENTURY();
HARBOUR SET();
HARBOUR OS();
HARBOUR ERRORNEW();
HARBOUR FOPEN();
HARBOUR FCREATE();
HARBOUR FREAD();
HARBOUR FWRITE();
HARBOUR FERROR();
HARBOUR FCLOSE();
HARBOUR FERASE();
HARBOUR FRENAME();
HARBOUR FSEEK();
HARBOUR _FILE();
HARBOUR FREADSTR();
HARBOUR BIN2I();
HARBOUR BIN2L();
HARBOUR BIN2W();
HARBOUR I2BIN();
HARBOUR L2BIN();
HARBOUR W2BIN();
HARBOUR EXP();
HARBOUR LOG();
HARBOUR MOD();
HARBOUR ISDATA();
HARBOUR ISMETHOD();
HARBOUR AODATA();
HARBOUR AOMETHOD();
HARBOUR AOGET();
HARBOUR AOSET();
HARBOUR OADDMETHOD();
HARBOUR OADDINLINE();
HARBOUR OADDDATA();
HARBOUR OMODMETHOD();
HARBOUR OMODINLINE();
HARBOUR ODELMETHOD();
HARBOUR ODELINLINE();
HARBOUR ODELDATA();
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
HARBOUR TCLASS();
HARBOUR TRANSFORM();
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
{ "QQOUT",          FS_PUBLIC, QQOUT         , 0 },
{ "QOUT",           FS_PUBLIC, QOUT          , 0 },
{ "ERRORSYS",       FS_PUBLIC, ERRORSYS      , 0 },
{ "ERRORNEW",       FS_PUBLIC, ERRORNEW      , 0 },
{ "EVAL",           FS_PUBLIC, EVAL          , 0 },
{ "VALTYPE",        FS_PUBLIC, VALTYPE       , 0 },
{ "ASORT",          FS_PUBLIC, ASORT         , 0 },
{ "CLASSCREATE",    FS_PUBLIC, CLASSCREATE   , 0 },
{ "CLASSADD",       FS_PUBLIC, CLASSADD      , 0 },
{ "CLASSNAME",      FS_PUBLIC, CLASSNAME     , 0 },
{ "CLASSINSTANCE",  FS_PUBLIC, CLASSINSTANCE , 0 },
{ "ISMESSAGE",      FS_PUBLIC, ISMESSAGE     , 0 },
{ "OSEND",          FS_PUBLIC, OSEND         , 0 },
{ "CLASSMOD",       FS_PUBLIC, CLASSMOD      , 0 },
{ "CLASSDEL",       FS_PUBLIC, CLASSDEL      , 0 },
{ "OCLONE",         FS_PUBLIC, OCLONE        , 0 },
{ "STOD",           FS_PUBLIC, STOD          , 0 },
{ "HB_SETCENTURY",  FS_PUBLIC, HB_SETCENTURY , 0 },
{ "SET",            FS_PUBLIC, SET           , 0 },
{ "OS",             FS_PUBLIC, OS            , 0 },
{ "ERRORNEW",       FS_PUBLIC, ERRORNEW      , 0 },
{ "FOPEN",          FS_PUBLIC, FOPEN         , 0 },
{ "FCREATE",        FS_PUBLIC, FCREATE       , 0 },
{ "FREAD",          FS_PUBLIC, FREAD         , 0 },
{ "FWRITE",         FS_PUBLIC, FWRITE        , 0 },
{ "FERROR",         FS_PUBLIC, FERROR        , 0 },
{ "FCLOSE",         FS_PUBLIC, FCLOSE        , 0 },
{ "FERASE",         FS_PUBLIC, FERASE        , 0 },
{ "FRENAME",        FS_PUBLIC, FRENAME       , 0 },
{ "FSEEK",          FS_PUBLIC, FSEEK         , 0 },
{ "_FILE",          FS_PUBLIC, _FILE         , 0 },
{ "FREADSTR",       FS_PUBLIC, FREADSTR      , 0 },
{ "BIN2I",          FS_PUBLIC, BIN2I         , 0 },
{ "BIN2L",          FS_PUBLIC, BIN2L         , 0 },
{ "BIN2W",          FS_PUBLIC, BIN2W         , 0 },
{ "I2BIN",          FS_PUBLIC, I2BIN         , 0 },
{ "L2BIN",          FS_PUBLIC, L2BIN         , 0 },
{ "W2BIN",          FS_PUBLIC, W2BIN         , 0 },
{ "EXP",            FS_PUBLIC, EXP           , 0 },
{ "LOG",            FS_PUBLIC, LOG           , 0 },
{ "MOD",            FS_PUBLIC, MOD           , 0 },
{ "ISDATA",         FS_PUBLIC, ISDATA        , 0 },
{ "ISMETHOD",       FS_PUBLIC, ISMETHOD      , 0 },
{ "AODATA",         FS_PUBLIC, AODATA        , 0 },
{ "AOMETHOD",       FS_PUBLIC, AOMETHOD      , 0 },
{ "AOGET",          FS_PUBLIC, AOGET         , 0 },
{ "AOSET",          FS_PUBLIC, AOSET         , 0 },
{ "OADDMETHOD",     FS_PUBLIC, OADDMETHOD    , 0 },
{ "OADDINLINE",     FS_PUBLIC, OADDINLINE    , 0 },
{ "OADDDATA",       FS_PUBLIC, OADDDATA      , 0 },
{ "OMODMETHOD",     FS_PUBLIC, OMODMETHOD    , 0 },
{ "OMODINLINE",     FS_PUBLIC, OMODINLINE    , 0 },
{ "ODELMETHOD",     FS_PUBLIC, ODELMETHOD    , 0 },
{ "ODELINLINE",     FS_PUBLIC, ODELINLINE    , 0 },
{ "ODELDATA",       FS_PUBLIC, ODELDATA      , 0 },
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
{ "TCLASS",         FS_PUBLIC, TCLASS        , 0 },
{ "TRANSFORM",      FS_PUBLIC, TRANSFORM     , 0 },
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
{ "ROT13",          FS_PUBLIC, ROT13         , 0 }
};
