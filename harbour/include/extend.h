/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_EXTEND_H_
#define HB_EXTEND_H_

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hbdefs.h"
#include "hbsetup.h"

struct _DYNSYM;         /* forward declaration */

typedef struct          /* symbol support structure */
{
   char*       szName;  /* the name of the symbol */
   SYMBOLSCOPE cScope;  /* the scope of the symbol */
   HARBOURFUNC pFunPtr; /* function address for function symbol table entries */
   struct _DYNSYM * pDynSym;   /* pointer to its dynamic symbol if defined */
} SYMBOL, * PSYMBOL;

/* Harbour Functions scope */
#define FS_PUBLIC       0
#define FS_STATIC       2
#define FS_INIT         8
#define FS_EXIT        16
#define FS_INITEXIT    ( FS_INIT | FS_EXIT )
#define FS_MESSAGE     32
#define FS_MEMVAR     128

void VirtualMachine( PBYTE pCode, PSYMBOL pSymbols );  /* invokes the virtual machine */
void ProcessSymbols( SYMBOL *, WORD );

/* items types */
#define IT_NIL       0x0000
#define IT_INTEGER   0x0002
#define IT_LONG      0x0008
#define IT_DOUBLE    0x0010
#define IT_DATE      0x0020
#define IT_LOGICAL   0x0080
#define IT_SYMBOL    0x0100
#define IT_ALIAS     0x0200
#define IT_STRING    0x0400
#define IT_MEMOFLAG  0x0800
#define IT_MEMO      ( IT_MEMOFLAG & IT_STRING )
#define IT_BLOCK     0x1000
#define IT_BYREF     0x2000
#define IT_MEMVAR    0x4000
#define IT_ARRAY     0x8000
#define IT_OBJECT    IT_ARRAY
#define IT_NUMERIC   ( IT_INTEGER | IT_LONG | IT_DOUBLE )
#define IT_ANY       0xFFFF

/* forward declarations
*/
struct _HB_CODEBLOCK;
struct _HB_BASEARRAY;
struct _HB_ITEM;
struct _HB_VALUE;

/* Internal structures that holds data */
struct hb_struArray
{
  struct _HB_BASEARRAY * value;
};

struct hb_struBlock
{
  LONG statics;
  WORD lineno;
  WORD paramcnt;
  struct _HB_CODEBLOCK * value;
};

struct hb_struDate
{
  WORD length;
  LONG value;
};

struct hb_struDouble
{
  WORD length;
  WORD decimal;
  double value;
};

struct hb_struInteger
{
  WORD length;
  WORD decimal;
  int value;
};

struct hb_struLogical
{
  WORD length;
  WORD value;
};

struct hb_struLong
{
  WORD length;
  WORD decimal;
  long value;
};

struct hb_struMemvar
{
  struct _HB_VALUE * *itemsbase;
  LONG offset;
  LONG value;
};

struct hb_struPointer
{
  PVOID value;
};

struct hb_struRefer
{
  struct _HB_ITEM * *itemsbase;
  LONG offset;
  LONG value;
};

struct hb_struString
{
  ULONG length;
  char *value;
};

struct hb_struSymbol
{
  LONG stackbase;
  WORD lineno;
  WORD paramcnt;
  PSYMBOL value;
};

typedef struct _HB_ITEM       /* items hold at the virtual machine stack */
{
  WORD type;
  union
  {
        struct hb_struArray   asArray;
        struct hb_struBlock   asBlock;
        struct hb_struDate    asDate;
        struct hb_struDouble  asDouble;
        struct hb_struInteger asInteger;
        struct hb_struLogical asLogical;
        struct hb_struLong    asLong;
        struct hb_struMemvar  asMemvar;
        struct hb_struPointer asPointer;
        struct hb_struRefer   asRefer;
        struct hb_struString  asString;
        struct hb_struSymbol  asSymbol;
  } item;
}
HB_ITEM, *PHB_ITEM;
typedef PHB_ITEM HB_ITEM_PTR;

typedef struct _HB_BASEARRAY
{
   PHB_ITEM pItems;       /* pointer to the array items */
   ULONG    ulLen;        /* number of items in the array */
   WORD     wHolders;     /* number of holders of this array */
   WORD     wClass;       /* offset to the classes base if it is an object */
   WORD     wSuperCast;   /* is it a super cast ? */
} BASEARRAY, * PBASEARRAY;

typedef struct     /* stack managed by the virtual machine */
{
   PHB_ITEM pItems;       /* pointer to the stack items */
   PHB_ITEM pPos;         /* pointer to the latest used item */
   LONG     wItems;       /* total items that may be holded on the stack */
   HB_ITEM  Return;       /* latest returned value */
   PHB_ITEM pBase;        /* stack frame position for the current function call */
   PHB_ITEM pEvalBase;    /* stack frame position for the evaluated codeblock */
   int      iStatics;     /* statics base for the current function call */
   char     szDate[ 9 ];  /* last returned date from _pards() yyyymmdd format */
} STACK;

typedef struct _DYNSYM
{
   HB_HANDLE hArea;       /* Workarea number */
   HB_HANDLE hMemvar;     /* Index number into memvars ( publics & privates ) array */
   PSYMBOL   pSymbol;     /* pointer to its relative local symbol */
   HARBOURFUNC pFunPtr;   /* Pointer to the function address */
} DYNSYM, * PDYNSYM;      /* dynamic symbol structure */

/* internal structure for codeblocks */
typedef struct _HB_CODEBLOCK
{
   BYTE*    pCode;        /* codeblock pcode */
   PHB_ITEM pLocals;      /* table with referenced local variables */
   WORD     wLocals;      /* number of referenced local variables */
   PSYMBOL  pSymbols;     /* codeblocks symbols */
   ULONG    lCounter;     /* numer of references to this codeblock */
} HB_CODEBLOCK, * HB_CODEBLOCK_PTR;

typedef struct _HB_VALUE
{
   HB_ITEM   item;
   ULONG     counter;
   HB_HANDLE hPrevMemvar;
} HB_VALUE, * HB_VALUE_PTR;

extern STACK stack;
extern SYMBOL symEval;
extern HB_ITEM errorBlock;
extern HB_ITEM aStatics;

PHB_ITEM hb_param( int iParam, WORD wType ); /* retrieve a generic parameter */
char *   hb_parc( int iParam, ... );  /* retrieve a string parameter */
ULONG    hb_parclen( int iParam, ... ); /* retrieve a string parameter length */
char *   hb_pards( int iParam, ... ); /* retrieve a date as a string yyyymmdd */
int      hb_parl( int iParam, ... ); /* retrieve a logical parameter as an int */
double   hb_parnd( int iParam, ... ); /* retrieve a numeric parameter as a double */
int      hb_parni( int iParam, ... ); /* retrieve a numeric parameter as a integer */
long     hb_parnl( int iParam, ... ); /* retrieve a numeric parameter as a long */
WORD     hb_parinfo( int iParam ); /* Determine the param count or data type */
ULONG    hb_parinfa( int iParamNum, ULONG uiArrayIndex );
void     hb_storc( char * szText, int iParam, ... ); /* stores a szString on a variable by reference */
void     hb_storclen( char * fixText, WORD wLength, int iParam, ... ); /* stores a fixed length string on a variable by reference */
void     hb_stords( char * szDate, int iParam, ... );   /* szDate must have yyyymmdd format */
void     hb_storl( int iLogical, int iParam, ... ); /* stores a logical integer on a variable by reference */
void     hb_storni( int iValue, int iParam, ... ); /* stores an integer on a variable by reference */
void     hb_stornl( long lValue, int iParam, ... ); /* stores a long on a variable by reference */
void     hb_stornd( double dValue, int iParam, ... ); /* stores a double on a variable by reference */
WORD     hb_pcount( void );          /* returns the number of suplied parameters */
void     hb_ret( void );             /* post a NIL return value */
void     hb_retc( char * szText );   /* returns a string */
void     hb_retclen( char * szText, ULONG ulLen ); /* returns a string with a specific length */
void     hb_retds( char * szDate );  /* returns a date, must use yyyymmdd format */
void     hb_retl( int iTrueFalse );  /* returns a logical integer */
void     hb_retni( int iNumber );    /* returns a integer number */
void     hb_retnl( long lNumber );   /* returns a long number */
void     hb_retnd( double dNumber ); /* returns a double */
void     hb_reta( ULONG ulLen );  /* returns an array with a specific length */

void *   hb_xalloc( ULONG lSize );   /* allocates memory, returns NULL on failure */
void *   hb_xgrab( ULONG lSize );   /* allocates memory, exists on failure */
void *   hb_xrealloc( void * pMem, ULONG lSize );   /* reallocates memory */
void     hb_xfree( void * pMem );    /* frees memory */
ULONG    hb_xsize( void * pMem ); /* returns the size of an allocated memory block */

void     hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ); /* creates a new array */
void     hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* retrieves an item */
ULONG    hb_arrayLen( PHB_ITEM pArray ); /* retrives the array len */
void     hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* sets an array element */
void     hb_arraySize( PHB_ITEM pArray, ULONG ulLen ); /* sets the array total length */
void     hb_arrayRelease( PHB_ITEM pArray ); /* releases an array - don't call it - use ItemRelease() !!! */
char *   hb_arrayGetString( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string contained on an array element */
ULONG    hb_arrayGetStringLen( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string length contained on an array element */
BOOL     hb_arrayGetBool( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the logical value contained on an array element */
double   hb_arrayGetDouble( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the double value contained on an array element */
int      hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex );
void     hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex );
PHB_ITEM hb_arrayClone( PHB_ITEM pArray );
void     hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue );
char *   hb_arrayGetDate( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the date value contained on an array element */

int      hb_itemStrCmp( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact ); /* our string compare */
void     hb_itemCopy( PHB_ITEM pDest, PHB_ITEM pSource ); /* copies an item to one place to another respecting its containts */
void     hb_itemClear( PHB_ITEM pItem );
PHB_ITEM hb_itemUnRef( PHB_ITEM pItem ); /* de-references passed variable */

char *   hb_str( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec ); /* convert number to string */
int      hb_stricmp( const char *s1, const char *s2 );
BOOL     hb_strempty( char * szText, ULONG ulLen );
ULONG    hb_strAt( char *, long, char *, long );
char *   hb_strUpper( char * szText, long lLen );
char *   hb_strLower( char * szText, long lLen );

HARBOURFUNC hb_GetMethod( PHB_ITEM pObject, PSYMBOL pSymMsg ); /* returns the method pointer of a object class */
char *   hb_GetClassName( PHB_ITEM pObject ); /* retrieves an object class name */
ULONG    hb_isMessage( PHB_ITEM, char * );

/* dynamic symbol table management */
PDYNSYM  hb_GetDynSym( char * szName );   /* finds and creates a dynamic symbol if not found */
PDYNSYM  hb_NewDynSym( PSYMBOL pSymbol ); /* creates a new dynamic symbol based on a local one */
PDYNSYM  hb_FindDynSym( char * szName );  /* finds a dynamic symbol */

HB_CODEBLOCK_PTR hb_CodeblockNew( BYTE *, WORD, WORD *, PSYMBOL );
void     hb_CodeblockDelete( PHB_ITEM );
PHB_ITEM hb_CodeblockGetVar( PHB_ITEM, LONG );
PHB_ITEM hb_CodeblockGetRef( PHB_ITEM, PHB_ITEM );
void     hb_CodeblockEvaluate( PHB_ITEM );
void     hb_CodeblockCopy( PHB_ITEM, PHB_ITEM );

/* Initialisation and closing memvars subsystem */
void     hb_MemvarsInit( void );
void     hb_MemvarsRelease( void );
void     hb_MemvarValueIncRef( HB_HANDLE );
void     hb_MemvarValueDecRef( HB_HANDLE );
void     hb_MemvarSetValue( PSYMBOL, HB_ITEM_PTR );
void     hb_MemvarGetValue( HB_ITEM_PTR, PSYMBOL );
void     hb_MemvarGetRefer( HB_ITEM_PTR, PSYMBOL );
void     hb_MemvarNewSymbol( PSYMBOL );
ULONG    hb_MemvarGetPrivatesBase( void );
void     hb_MemvarSetPrivatesBase( ULONG );

#endif /* HB_EXTEND_H_ */
