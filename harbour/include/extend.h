/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Extend API, Array API, misc API and base declarations
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_EXTEND_H_
#define HB_EXTEND_H_

#include "hbdefs.h"
#include "hb_vmpub.h"

/* items types and type checking macros */
#define IT_NIL          ( ( USHORT ) 0x0000 )
#define IT_INTEGER      ( ( USHORT ) 0x0002 )
#define IT_LONG         ( ( USHORT ) 0x0008 )
#define IT_DOUBLE       ( ( USHORT ) 0x0010 )
#define IT_DATE         ( ( USHORT ) 0x0020 )
#define IT_LOGICAL      ( ( USHORT ) 0x0080 )
#define IT_SYMBOL       ( ( USHORT ) 0x0100 )
#define IT_ALIAS        ( ( USHORT ) 0x0200 )
#define IT_STRING       ( ( USHORT ) 0x0400 )
#define IT_MEMOFLAG     ( ( USHORT ) 0x0800 )
#define IT_MEMO         ( IT_MEMOFLAG & IT_STRING )
#define IT_BLOCK        ( ( USHORT ) 0x1000 )
#define IT_BYREF        ( ( USHORT ) 0x2000 )
#define IT_MEMVAR       ( ( USHORT ) 0x4000 )
#define IT_ARRAY        ( ( USHORT ) 0x8000 )
#define IT_OBJECT       IT_ARRAY
#define IT_NUMERIC      ( ( USHORT ) ( IT_INTEGER | IT_LONG | IT_DOUBLE ) )
#define IT_ANY          ( ( USHORT ) 0xFFFF )

#define IS_BYREF( p )      ( ( p )->type & IT_BYREF )
#define IS_OF_TYPE( p, t ) ( ( ( p )->type & ~IT_BYREF ) == t )
#define IS_ARRAY( p )      IS_OF_TYPE( p, IT_ARRAY )
#define IS_NIL( p )        IS_OF_TYPE( p, IT_NIL )
#define IS_BLOCK( p )      IS_OF_TYPE( p, IT_BLOCK )
#define IS_DATE( p )       IS_OF_TYPE( p, IT_DATE )
#define IS_DOUBLE( p )     IS_OF_TYPE( p, IT_DOUBLE )
#define IS_INTEGER( p )    IS_OF_TYPE( p, IT_INTEGER )
#define IS_LOGICAL( p )    IS_OF_TYPE( p, IT_LOGICAL )
#define IS_LONG( p )       IS_OF_TYPE( p, IT_LONG )
#define IS_NUMERIC( p )    ( ( p )->type & IT_NUMERIC )
#define IS_OBJECT( p )     IS_OF_TYPE( p, IT_OBJECT )
#define IS_STRING( p )     IS_OF_TYPE( p, IT_STRING )
#define IS_SYMBOL( p )     IS_OF_TYPE( p, IT_SYMBOL )
#define IS_MEMVAR( p )     IS_OF_TYPE( p, IT_MEMVAR )

#define ISNIL( n )         ( IS_NIL( hb_param( n, IT_ANY ) ) )
#define ISCHAR( n )        ( hb_param( n, IT_STRING ) != NULL )
#define ISNUM( n )         ( hb_param( n, IT_NUMERIC ) != NULL )
#define ISLOG( n )         ( hb_param( n, IT_LOGICAL ) != NULL )
#define ISDATE( n )        ( hb_param( n, IT_DATE ) != NULL )
#define ISMEMO( n )        ( hb_param( n, IT_MEMO ) != NULL )
#define ISBYREF( n )       ( hb_parinfo( n ) & IT_BYREF ) /* NOTE: Intentionally using a different method */
#define ISARRAY( n )       ( hb_param( n, IT_ARRAY ) != NULL )
#define ISBLOCK( n )       ( hb_param( n, IT_BLOCK ) != NULL ) /* Not available in CA-Cl*pper. */

#define PCOUNT             hb_pcount()
#define ALENGTH( n )       hb_parinfa( n, 0 )

/* forward declarations */
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
   USHORT lineno;
   USHORT paramcnt;
   struct _HB_CODEBLOCK * value;
};

struct hb_struDate
{
   LONG value;
};

/* this definition signals that number of decimal places for double value
 * was not specified at compile time (the value is a result of optimization
 * performed by the compiler)
 */
#define HB_DEFAULT_DECIMALS  255
struct hb_struDouble
{
   USHORT length;
   USHORT decimal;
   double value;
};

struct hb_struInteger
{
   USHORT length;
   int value;
};

struct hb_struLogical
{
   BOOL value;
};

struct hb_struLong
{
   USHORT length;
   long value;
};

struct hb_struMemvar
{
   struct _HB_VALUE ** itemsbase;
   LONG offset;
   LONG value;
};

struct hb_struPointer
{
   void * value;
};

struct hb_struRefer
{
   struct _HB_ITEM ** itemsbase;
   LONG offset;
   LONG value;
};

struct hb_struString
{
   ULONG length;
   char * value;
};

struct hb_struSymbol
{
   LONG stackbase;
   USHORT lineno;
   USHORT paramcnt;
   PHB_SYMB value;
};

/* items hold at the virtual machine stack */
typedef struct _HB_ITEM
{
   USHORT type;
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
} HB_ITEM, * PHB_ITEM, * HB_ITEM_PTR;

typedef struct _HB_BASEARRAY
{
   PHB_ITEM pItems;       /* pointer to the array items */
   ULONG    ulLen;        /* number of items in the array */
   USHORT   uiHolders;    /* number of holders of this array */
   USHORT   uiClass;      /* offset to the classes base if it is an object */
   USHORT   uiPrevCls;    /* for fixing after access super */
} HB_BASEARRAY, * PHB_BASEARRAY, * HB_BASEARRAY_PTR;

/* stack managed by the virtual machine */
typedef struct
{
   PHB_ITEM pItems;       /* pointer to the stack items */
   PHB_ITEM pPos;         /* pointer to the latest used item */
   LONG     wItems;       /* total items that may be holded on the stack */
   HB_ITEM  Return;       /* latest returned value */
   PHB_ITEM pBase;        /* stack frame position for the current function call */
   PHB_ITEM pEvalBase;    /* stack frame position for the evaluated codeblock */
   int      iStatics;     /* statics base for the current function call */
   char     szDate[ 9 ];  /* last returned date from _pards() yyyymmdd format */
} HB_STACK;

/* internal structure for codeblocks */
typedef struct _HB_CODEBLOCK
{
   BYTE *   pCode;        /* codeblock pcode */
   PHB_ITEM pLocals;      /* table with referenced local variables */
   USHORT   uiLocals;     /* number of referenced local variables */
   PHB_SYMB pSymbols;     /* codeblocks symbols */
   ULONG    ulCounter;    /* numer of references to this codeblock */
} HB_CODEBLOCK, * PHB_CODEBLOCK, * HB_CODEBLOCK_PTR;

typedef struct _HB_VALUE
{
   HB_ITEM   item;
   ULONG     counter;
   HB_HANDLE hPrevMemvar;
} HB_VALUE, * PHB_VALUE, * HB_VALUE_PTR;

/* RDD method return codes */
typedef USHORT ERRCODE;
#define SUCCESS            0
#define FAILURE            1

extern HB_STACK hb_stack;
extern HB_SYMB  hb_symEval;

/* Extend API */
extern char *   hb_parc( int iParam, ... );  /* retrieve a string parameter */
extern ULONG    hb_parclen( int iParam, ... ); /* retrieve a string parameter length */
extern ULONG    hb_parcsiz( int iParam, ... );
extern char *   hb_pards( int iParam, ... ); /* retrieve a date as a string yyyymmdd */
extern char *   hb_pardsbuff( char * szDate, int iParam, ... ); /* retrieve a date as a string yyyymmdd */
extern ULONG    hb_parinfa( int iParamNum, ULONG uiArrayIndex );
extern int      hb_parinfo( int iParam ); /* Determine the param count or data type */
extern int      hb_parl( int iParam, ... ); /* retrieve a logical parameter as an int */
extern double   hb_parnd( int iParam, ... ); /* retrieve a numeric parameter as a double */
extern int      hb_parni( int iParam, ... ); /* retrieve a numeric parameter as a integer */
extern long     hb_parnl( int iParam, ... ); /* retrieve a numeric parameter as a long */
extern PHB_ITEM hb_param( int iParam, int iMask ); /* retrieve a generic parameter */
extern int      hb_pcount( void );          /* returns the number of suplied parameters */

extern void     hb_ret( void );             /* post a NIL return value */
extern void     hb_retc( char * szText );   /* returns a string */
extern void     hb_retclen( char * szText, ULONG ulLen ); /* returns a string with a specific length */
extern void     hb_retds( char * szDate );  /* returns a date, must use yyyymmdd format */
extern void     hb_retl( int iTrueFalse );  /* returns a logical integer */
extern void     hb_retnd( double dNumber ); /* returns a double */
extern void     hb_retni( int iNumber );    /* returns a integer number */
extern void     hb_retnl( long lNumber );   /* returns a long number */
extern void     hb_retnlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
extern void     hb_retndlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
extern void     hb_retnilen( int iNumber, int iWidth ); /* returns a integer number, with specific width */
extern void     hb_retnllen( long lNumber, int iWidth ); /* returns a long number, with specific width */
extern void     hb_reta( ULONG ulLen );  /* returns an array with a specific length */

extern void     hb_storc( char * szText, int iParam, ... ); /* stores a szString on a variable by reference */
extern void     hb_storclen( char * szText, ULONG ulLength, int iParam, ... ); /* stores a fixed length string on a variable by reference */
extern void     hb_stords( char * szDate, int iParam, ... );   /* szDate must have yyyymmdd format */
extern void     hb_storl( int iLogical, int iParam, ... ); /* stores a logical integer on a variable by reference */
extern void     hb_storni( int iValue, int iParam, ... ); /* stores an integer on a variable by reference */
extern void     hb_stornl( long lValue, int iParam, ... ); /* stores a long on a variable by reference */
extern void     hb_stornd( double dValue, int iParam, ... ); /* stores a double on a variable by reference */

extern void     hb_xinit( void );                         /* Initialize fixed memory subsystem */
extern void     hb_xexit( void );                         /* Deinitialize fixed memory subsystem */
extern void *   hb_xalloc( ULONG ulSize );                /* allocates memory, returns NULL on failure */
extern void *   hb_xgrab( ULONG ulSize );                 /* allocates memory, exists on failure */
extern void     hb_xfree( void * pMem );                  /* frees memory */
extern void *   hb_xrealloc( void * pMem, ULONG ulSize ); /* reallocates memory */
extern ULONG    hb_xsize( void * pMem );                  /* returns the size of an allocated memory block */
extern ULONG    hb_xquery( USHORT uiMode );               /* Query different types of memory information */

#if UINT_MAX == ULONG_MAX
   /* NOTE: memcpy/memset can work with ULONG data blocks */
   #define  hb_xmemcpy  memcpy
   #define  hb_xmemset  memset
#else
   /* NOTE: otherwise, the hb_xmemcpy and hb_xmemset functions
            will be used to copy and/or set ULONG data blocks */
extern void *   hb_xmemcpy( void * pDestArg, void * pSourceArg, ULONG ulLen ); /* copy more than memcpy() can */
extern void *   hb_xmemset( void * pDestArg, int iFill, ULONG ulLen ); /* set more than memset() can */
#endif

/* array management */
extern BOOL     hb_arrayError( PHB_ITEM pArray, ULONG ulIndex, BOOL bAssign ); /* Checks if the passed parameters are valid, launches runtim error if needed */
extern BOOL     hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ); /* creates a new array */
extern ULONG    hb_arrayLen( PHB_ITEM pArray ); /* retrives the array len */
extern BOOL     hb_arrayIsObject( PHB_ITEM pArray ); /* retrives if the array is an object */
extern BOOL     hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue );
extern BOOL     hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex );
extern BOOL     hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex );
extern BOOL     hb_arraySize( PHB_ITEM pArray, ULONG ulLen ); /* sets the array total length */
extern BOOL     hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult );
extern BOOL     hb_arrayRelease( PHB_ITEM pArray ); /* releases an array - don't call it - use ItemRelease() !!! */
extern BOOL     hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* sets an array element */
extern BOOL     hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* retrieves an item */
extern PHB_ITEM hb_arrayGetItemPtr( PHB_ITEM pArray, ULONG ulIndex ); /* returns pointer to specified element of the array */
extern ULONG    hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen );
extern char *   hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string contained on an array element */
extern char *   hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string pointer on an array element */
extern ULONG    hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string length contained on an array element */
extern BOOL     hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the logical value contained on an array element */
extern int      hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the int value contained on an array element */
extern long     hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the long numeric value contained on an array element */
extern double   hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the double value contained on an array element */
extern char *   hb_arrayGetDS( PHB_ITEM pArray, ULONG ulIndex, char * szDate ); /* retrieves the date value contained on an array element */
extern USHORT   hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex );
extern BOOL     hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount );
extern ULONG    hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount );
extern BOOL     hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG * pulStart, ULONG * pulCount );
extern BOOL     hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG * pulStart, ULONG * pulCount, ULONG * pulTarget );
extern PHB_ITEM hb_arrayClone( PHB_ITEM pArray );

/* string management */

extern int      hb_stricmp( const char * s1, const char * s2 );
extern int      hb_strnicmp( const char * s1, const char * s2, ULONG ulLen );
extern void     hb_strupr( char * szText );
extern BOOL     hb_strMatchRegExp( const char * szString, const char * szMask );
extern BOOL     hb_strEmpty( const char * szText, ULONG ulLen );
extern void     hb_strDescend( char * szStringTo, const char * szStringFrom, ULONG ulLen );
extern ULONG    hb_strAt( const char * szSub, ULONG ulSubLen, const char * szText, ULONG ulLen );
extern char *   hb_strUpper( char * szText, ULONG ulLen );
extern char *   hb_strLower( char * szText, ULONG ulLen );
extern char *   hb_strncpyUpper( char * pDest, const char * pSource, ULONG ulLen );
extern double   hb_strVal( const char * szText );
extern char *   hb_strLTrim( const char * szText, ULONG * ulLen );
extern ULONG    hb_strRTrimLen( const char * szText, ULONG ulLen, BOOL bAnySpace );

extern double   hb_numRound( double dResult, int iDec );

/* class management */
extern void     hb_clsReleaseAll( void );    /* releases all defined classes */

/* object management */
extern char *   hb_objGetClsName( PHB_ITEM pObject ); /* retrieves an object class name */
extern PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pSymMsg ); /* returns the method pointer of a object class */
extern ULONG    hb_objHasMsg( PHB_ITEM pObject, char * szString );

/* dynamic symbol table management */
extern PHB_DYNS hb_dynsymGet( char * szName );    /* finds and creates a dynamic symbol if not found */
extern PHB_DYNS hb_dynsymNew( PHB_SYMB pSymbol ); /* creates a new dynamic symbol based on a local one */
extern PHB_DYNS hb_dynsymFind( char * szName );   /* finds a dynamic symbol */
extern PHB_DYNS hb_dynsymFindName( char * szName );   /* converts to uppercase and finds a dynamic symbol */
extern void     hb_dynsymLog( void );             /* displays all dynamic symbols */
extern void     hb_dynsymRelease( void );         /* releases the memory of the dynamic symbol table */
extern void     hb_dynsymEval( PHB_DYNS_FUNC, void * );   /* enumerates all dynamic symbols */

/* Command line and environment argument management */
extern void     hb_cmdargInit( int argc, char * argv[] );
extern int      hb_cmdargARGC( void );
extern char **  hb_cmdargARGV( void );
extern BOOL     hb_cmdargIsInternal( const char * szArg );
extern BOOL     hb_cmdargCheck( const char * pszName ); /* Check if a given internal switch (like //INFO) was set */
extern char *   hb_cmdargString( const char * pszName ); /* Returns the string value of an internal switch (like //TEMPPATH:"C:\") */
extern int      hb_cmdargNum( const char * pszName ); /* Returns the numeric value of an internal switch (like //F:90) */

/* Symbol management */
extern PHB_SYMB hb_symbolNew( char * szName );

/* Codeblock management */
extern HB_CODEBLOCK_PTR hb_codeblockNew( BYTE *, USHORT, USHORT *, PHB_SYMB );
extern void     hb_codeblockDelete( PHB_ITEM );
extern PHB_ITEM hb_codeblockGetVar( PHB_ITEM, LONG );
extern PHB_ITEM hb_codeblockGetRef( PHB_ITEM, PHB_ITEM );
extern void     hb_codeblockEvaluate( PHB_ITEM );
extern void     hb_codeblockCopy( PHB_ITEM, PHB_ITEM );

/* memvars subsystem */
extern HB_HANDLE hb_memvarValueNew( PHB_ITEM, BOOL );
extern HB_VALUE_PTR * hb_memvarValueBaseAddress( void );
extern void     hb_memvarsInit( void );
extern void     hb_memvarsRelease( void );
extern void     hb_memvarValueIncRef( HB_HANDLE );
extern void     hb_memvarValueDecRef( HB_HANDLE );
extern void     hb_memvarSetValue( PHB_SYMB, HB_ITEM_PTR );
extern ERRCODE  hb_memvarGet( HB_ITEM_PTR, PHB_SYMB );
extern void     hb_memvarGetValue( HB_ITEM_PTR, PHB_SYMB );
extern void     hb_memvarGetRefer( HB_ITEM_PTR, PHB_SYMB );
extern ULONG    hb_memvarGetPrivatesBase( void );
extern void     hb_memvarSetPrivatesBase( ULONG );
extern void     hb_memvarNewParameter( PHB_SYMB, HB_ITEM_PTR );

/* console I/O subsystem */
extern void     hb_consoleInitialize( void );
extern void     hb_consoleRelease( void );
extern char *   hb_consoleGetNewLine( void );
extern void     hb_outstd( char * pStr, ULONG ulLen );
extern void     hb_outerr( char * pStr, ULONG ulLen );
extern USHORT   hb_setCursor( BOOL bSetCursor, USHORT usNewCursor );
extern void     hb_tone( double dFrequency, double dDuration );
extern char *   hb_setColor( char * );

/* misc */
extern char *   hb_version( USHORT uiMode );

/* Please leave these at the bottom of this file */

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
   /* Clipper includes these from extend.h */
   #include "extend.api"
   #include "fm.api"
#endif

#endif /* HB_EXTEND_H_ */
