/*
 *
 * This file contains the exportable functions available to the Harbour program
 *
 * Currently being discussed in 'Static initializers'
 *
 * If the discussion has finished, it can be removed from here.
 *
 */
HARBOUR HB___AGLOBALSTACK();
HARBOUR HB___APARAM();
HARBOUR HB___ASTACK();
HARBOUR HB___ASTATIC();
HARBOUR HB___GLOBALSTACKLEN();
HARBOUR HB___STACKLEN();
HARBOUR HB___STATIC();
HARBOUR HB_ACOS();
HARBOUR HB_ASIN();
HARBOUR HB_ATAN();
HARBOUR HB_COS();
HARBOUR HB_COSH();
HARBOUR HB_DATETIME();
HARBOUR HB_DEBUG();
HARBOUR HB_DEFAULT();
HARBOUR HB_ERRORNEW();
HARBOUR HB_ERRORSYS();
HARBOUR HB_EVAL();
HARBOUR HB_GT_ASCIISUM();
HARBOUR HB_GT_ASCPOS();
HARBOUR HB_GT_ATDIFF();
HARBOUR HB_GT_CHAREVEN();
HARBOUR HB_GT_CHARMIX();
HARBOUR HB_GT_CHARODD();
HARBOUR HB_GT_CHRCOUNT();
HARBOUR HB_GT_CHRFIRST();
HARBOUR HB_GT_CHRTOTAL();
HARBOUR HB_GT_STRCOUNT();
HARBOUR HB_GT_STRCSPN();
HARBOUR HB_GT_STRDIFF();
HARBOUR HB_GT_STREXPAND();
HARBOUR HB_GT_STRLEFT();
HARBOUR HB_GT_STRPBRK();
HARBOUR HB_GT_STRRIGHT();
HARBOUR HB_HB_FEOF();
HARBOUR HB_HB_FGOBOTTOM();
HARBOUR HB_HB_FGOTO();
HARBOUR HB_HB_FGOTOP();
HARBOUR HB_HB_FLASTREC();
HARBOUR HB_HB_FREADLN();
HARBOUR HB_HB_FRECNO();
HARBOUR HB_HB_FSKIP();
HARBOUR HB_HB_FUSE();
HARBOUR HB_LOG10();
HARBOUR HB_PVALUE();
HARBOUR HB_ROT13();
HARBOUR HB_SIN();
HARBOUR HB_SINH();
HARBOUR HB_STRDUMP();
HARBOUR HB_STRTOKEN();
HARBOUR HB_TAN();
HARBOUR HB_TANH();
HARBOUR HB_TOCHAR();
HARBOUR HB_VALTYPE();

/* Same story.

   All the function pointers of the internal functions
   Including Runner itself, since the first symbol gets executed by Harbour ;-)
*/

static SYMBOL symbols[] = {
{ "HB_RUN",         FS_PUBLIC, HB_HB_RUN        , 0 },
{ "__AGLOBALSTACK", FS_PUBLIC, HB___AGLOBALSTACK, 0 },
{ "__APARAM",       FS_PUBLIC, HB___APARAM      , 0 },
{ "__ASTACK",       FS_PUBLIC, HB___ASTACK      , 0 },
{ "__ASTATIC",      FS_PUBLIC, HB___ASTATIC     , 0 },
{ "__GLOBALSTACKLEN", FS_PUBLIC, HB___GLOBALSTACKLEN, 0 },
{ "__STACKLEN",     FS_PUBLIC, HB___STACKLEN    , 0 },
{ "__STATIC",       FS_PUBLIC, HB___STATIC      , 0 },
{ "ACOS",           FS_PUBLIC, HB_ACOS          , 0 },
{ "ASIN",           FS_PUBLIC, HB_ASIN          , 0 },
{ "ATAN",           FS_PUBLIC, HB_ATAN          , 0 },
{ "COS",            FS_PUBLIC, HB_COS           , 0 },
{ "COSH",           FS_PUBLIC, HB_COSH          , 0 },
{ "DATETIME",       FS_PUBLIC, HB_DATETIME      , 0 },
{ "DEBUG",          FS_PUBLIC, HB_DEBUG         , 0 },
{ "DEFAULT",        FS_PUBLIC, HB_DEFAULT       , 0 },
{ "ERRORSYS",       FS_PUBLIC, HB_ERRORSYS      , 0 },
{ "ERRORNEW",       FS_PUBLIC, HB_ERRORNEW      , 0 },
{ "EVAL",           FS_PUBLIC, HB_EVAL          , 0 },
{ "GT_ASCIISUM",    FS_PUBLIC, HB_GT_ASCIISUM   , 0 },
{ "GT_ASCPOS",      FS_PUBLIC, HB_GT_ASCPOS     , 0 },
{ "GT_ATDIFF",      FS_PUBLIC, HB_GT_ATDIFF     , 0 },
{ "GT_CHAREVEN",    FS_PUBLIC, HB_GT_CHAREVEN   , 0 },
{ "GT_CHARODD",     FS_PUBLIC, HB_GT_CHARODD    , 0 },
{ "GT_CHARMIX",     FS_PUBLIC, HB_GT_CHARMIX    , 0 },
{ "GT_CHRCOUNT",    FS_PUBLIC, HB_GT_CHRCOUNT   , 0 },
{ "GT_CHRFIRST",    FS_PUBLIC, HB_GT_CHRFIRST   , 0 },
{ "GT_CHRTOTAL",    FS_PUBLIC, HB_GT_CHRTOTAL   , 0 },
{ "GT_STRCOUNT",    FS_PUBLIC, HB_GT_STRCOUNT   , 0 },
{ "GT_STRCSPN",     FS_PUBLIC, HB_GT_STRCSPN    , 0 },
{ "GT_STRDIFF",     FS_PUBLIC, HB_GT_STRDIFF    , 0 },
{ "GT_STREXPAND",   FS_PUBLIC, HB_GT_STREXPAND  , 0 },
{ "GT_STRLEFT",     FS_PUBLIC, HB_GT_STRLEFT    , 0 },
{ "GT_STRPBRK",     FS_PUBLIC, HB_GT_STRPBRK    , 0 },
{ "GT_STRRIGHT",    FS_PUBLIC, HB_GT_STRRIGHT   , 0 },
{ "HB_FEOF",        FS_PUBLIC, HB_HB_FEOF       , 0 },
{ "HB_FGOBOTTOM",   FS_PUBLIC, HB_HB_FGOBOTTOM  , 0 },
{ "HB_FGOTO",       FS_PUBLIC, HB_HB_FGOTO      , 0 },
{ "HB_FGOTOP",      FS_PUBLIC, HB_HB_FGOTOP     , 0 },
{ "HB_FLASTREC",    FS_PUBLIC, HB_HB_FLASTREC   , 0 },
{ "HB_FRECNO",      FS_PUBLIC, HB_HB_FRECNO     , 0 },
{ "HB_FREADLN",     FS_PUBLIC, HB_HB_FREADLN    , 0 },
{ "HB_FSKIP",       FS_PUBLIC, HB_HB_FSKIP      , 0 },
{ "HB_FUSE",        FS_PUBLIC, HB_HB_FUSE       , 0 },
{ "LOG10",          FS_PUBLIC, HB_LOG10         , 0 },
{ "PVALUE",         FS_PUBLIC, HB_PVALUE        , 0 },
{ "ROT13",          FS_PUBLIC, HB_ROT13         , 0 },
{ "SIN",            FS_PUBLIC, HB_SIN           , 0 },
{ "SINH",           FS_PUBLIC, HB_SINH          , 0 },
{ "STRDUMP",        FS_PUBLIC, HB_STRDUMP       , 0 },
{ "STRTOKEN",       FS_PUBLIC, HB_STRTOKEN      , 0 },
{ "TAN",            FS_PUBLIC, HB_TAN           , 0 },
{ "TANH",           FS_PUBLIC, HB_TANH          , 0 },
{ "TOCHAR",         FS_PUBLIC, HB_TOCHAR        , 0 },
{ "VALTYPE",        FS_PUBLIC, HB_VALTYPE       , 0 }
};



