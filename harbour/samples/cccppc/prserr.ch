/*
 * $Id$
 */

//*******************************************************************
// Az el“fordul¢ hib k hibak¢djai.
#ifdef OLD
#define FRERR_REOPEN 1
#define FRERR_OPEN   2
#define FRERR_READ   3

//*******************************************************************
#define PRSERR_INVALIDMN       'PIMN'
#define PRSERR_UNSUPPORTED     'PUNS'
#define PRSERR_SDEFINE         'PSDF'
#define PRSERR_LDEFINE         'PLDF'
#define PRSERR_PDEFINE         'PPDF'
#define PRSERR_MDUPLICATE      'PMDF'
#define PRSERR_SUNDEF          'PUDF'
#define PRSERR_SIFDEF          'PIFD'
// #define PRSERR_IFDEF           'PIFD'
// #define PRSERR_IFNDEF          'PIFN'
#define PRSERR_ELSE            'PELS' // Nincs ifdef
#define PRSERR_ELSE2           'PEL2' // Duplik lt else
#define PRSERR_ENDIF           'PEND' // Nincs hozz  ifdef
// #define PRSERR_NMENDIF         'PENM'  // #endif not match #if
#define PRSERR_INCLUDE         'INC '  // Bad filename in #include
// #define PRSERR_MAXINCLUDE      'MINC'  
#define PRSERR_INCLUDEFIND      'FINC'  
#define PRSERR_INCLUDEOPEN      'OINC'  
#define PRSERR_SXTRANSLATE      'SXTR'
#endif

//*******************************************************************

