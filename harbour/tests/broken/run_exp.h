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
HARBOUR STRDUMP();
HARBOUR TRIM();
HARBOUR LTRIM();
HARBOUR RTRIM();
HARBOUR CHR();

/* Same story.

   All the function pointers of the internal functions
   Including Runner itself, since the first symbol gets executed by Harbour ;-)
*/
static SYMBOL symbols[] = {
{ "Runner",   FS_PUBLIC, Runner  , 0 },
{ "ARRAY",    FS_PUBLIC, ARRAY   , 0 },
{ "AADD",     FS_PUBLIC, AADD    , 0 },
{ "ASIZE",    FS_PUBLIC, ASIZE   , 0 },
{ "ATAIL",    FS_PUBLIC, ATAIL   , 0 },
{ "AINS",     FS_PUBLIC, AINS    , 0 },
{ "ADEL",     FS_PUBLIC, ADEL    , 0 },
{ "AFILL",    FS_PUBLIC, AFILL   , 0 },
{ "ASCAN",    FS_PUBLIC, ASCAN   , 0 },
{ "AEVAL",    FS_PUBLIC, AEVAL   , 0 },
{ "ACOPY",    FS_PUBLIC, ACOPY   , 0 },
{ "ACLONE",   FS_PUBLIC, ACLONE  , 0 },
{ "__ACCEPT", FS_PUBLIC, __ACCEPT, 0 },
{ "OUTSTD",   FS_PUBLIC, OUTSTD  , 0 },
{ "QQOUT",    FS_PUBLIC, QQOUT   , 0 },
{ "QOUT",     FS_PUBLIC, QOUT    , 0 },
{ "ERRORSYS", FS_PUBLIC, ERRORSYS, 0 },
{ "ERRORNEW", FS_PUBLIC, ERRORNEW, 0 },
{ "EVAL",     FS_PUBLIC, EVAL    , 0 },
{ "VALTYPE",  FS_PUBLIC, VALTYPE , 0 },
{ "UPPER",    FS_PUBLIC, UPPER   , 0 },
{ "STRDUMP",  FS_PUBLIC, STRDUMP , 0 },
{ "TRIM",     FS_PUBLIC, TRIM    , 0 },
{ "LTRIM",    FS_PUBLIC, LTRIM   , 0 },
{ "RTRIM",    FS_PUBLIC, RTRIM   , 0 },
{ "CHR",      FS_PUBLIC, CHR     , 0 },
{ "VAL",      FS_PUBLIC, VAL     , 0 }
};


