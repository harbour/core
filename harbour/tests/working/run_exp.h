/*
 *
 * This file contains the exportable functions available to the Harbour program
 *
 * Currently being discussed in 'Static initializers'
 *
 * If the discussion has finished, it can be removed from here.
 *
 * Currently containing :
 *
 * Arrays
 * ASort
 * Classes
 * Console
 * HVM
 *
 */
HARBOUR ARRAY();
HARBOUR AADD();
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
HARBOUR UPPER();
HARBOUR VAL();
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
HARBOUR CTOD();
HARBOUR DTOC();
HARBOUR DTOS();
HARBOUR STOD();
HARBOUR DAY();
HARBOUR MONTH();
HARBOUR YEAR();
HARBOUR TIME();
HARBOUR HB_SETCENTURY();
HARBOUR SET();
HARBOUR LTRIM();
HARBOUR RTRIM();
HARBOUR ALLTRIM();
HARBOUR TRIM();

/* Same story.

   All the function pointers of the internal functions
   Including Runner itself, since the first symbol gets executed by Harbour ;-)
*/

static SYMBOL symbols[] = {
{ "HB_RUN",         FS_PUBLIC, HB_RUN        , 0 },
{ "ARRAY",          FS_PUBLIC, ARRAY         , 0 },
{ "AADD",           FS_PUBLIC, AADD          , 0 },
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
{ "UPPER",          FS_PUBLIC, UPPER         , 0 },
{ "VAL",            FS_PUBLIC, VAL           , 0 },
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
{ "CTOD",           FS_PUBLIC, CTOD          , 0 },
{ "DTOC",           FS_PUBLIC, DTOC          , 0 },
{ "DTOS",           FS_PUBLIC, DTOS          , 0 },
{ "STOD",           FS_PUBLIC, STOD          , 0 },
{ "DAY",            FS_PUBLIC, DAY           , 0 },
{ "MONTH",          FS_PUBLIC, MONTH         , 0 },
{ "YEAR",           FS_PUBLIC, YEAR          , 0 },
{ "TIME",           FS_PUBLIC, TIME          , 0 },
{ "HB_SETCENTURY",  FS_PUBLIC, HB_SETCENTURY , 0 },
{ "SET",            FS_PUBLIC, SET           , 0 },
{ "TRIM",           FS_PUBLIC, TIME          , 0 },
{ "LTRIM",          FS_PUBLIC, LTRIM         , 0 },
{ "RTRIM",          FS_PUBLIC, RTRIM         , 0 },
{ "ALLTRIM",        FS_PUBLIC, ALLTRIM       , 0 }
};

