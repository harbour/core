/*
 * $Id$
 */

/* CA-Cl*pper Compatibility Header File */
/* DON'T USE THIS FILE FOR NEW HARBOUR C CODE */

/* This file is provided to support some level of */
/* Harbour compatibility for old Clipper C extension code */

#ifndef HB_CLIPDEFS_H_
#define HB_CLIPDEFS_H_

#include "hbdefs.h"

/* Types moved here from *.api files */

typedef PHB_ITEM  ERRORP;
typedef double    XDOUBLE;
typedef PHB_ITEM  ITEM;
typedef PEVALINFO EVALINFOP;

/* Old types */

typedef BYTE    byte;
typedef USHORT  quant;
typedef BOOL    Boolean;

/* New types */

typedef BYTE*   BYTEP;
typedef BYTEP   PBYTE;
typedef BYTEP   BYTEPP;

typedef SHORT*  SHORTP;
typedef SHORTP  PSHORT;

typedef USHORT* USHORTP;
typedef USHORTP PUSHORT;

typedef WORD*   WORDP;
typedef WORDP   PWORD;

typedef LONG*   LONGP;
typedef LONGP   PLONG;

typedef ULONG*  ULONGP;
typedef ULONGP  PULONG;

typedef DWORD*  DWORDP;
typedef DWORDP  PDWORD;

typedef BOOL*   BOOLP;
typedef BOOLP   PBOOL;

typedef void*   NEARP;
typedef NEARP*  NEARPP;

typedef void*   FARP;
typedef FARP*   FARPP;

typedef FARP    VOIDP;
typedef FARP    PVOID;

typedef void*   HANDLE;
typedef USHORT  ERRCODE;
typedef ERRCODE IHELP;
typedef ERRCODE ICODE;

/* default func ptr -- USHORT return, USHORT param */
typedef USHORT  (far * FUNCP)(USHORT param, ...);
typedef FUNCP*  FUNCPP;

#define HIDE    static
#define CLIPPER HARBOUR

#ifndef NIL
   #define NIL     '\0'
#endif
#ifndef NULL
   #define NULL    0
#endif

#endif /* HB_CLIPDEFS_H_ */
