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
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* TOFIX: There are several things in this file which are not part of the
          standard Harbour API, in other words these things are not
          guaranteed to remain unchanged. To avoid confusion these should be
          moved to somewhere else (like HBRTL.H). [vszakats] */

#ifndef HB_APIEXT_H_
#define HB_APIEXT_H_

#include "hbvmpub.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* items types and type checking macros */
#define HB_IT_NIL       ( ( USHORT ) 0x0000 )
#define HB_IT_POINTER   ( ( USHORT ) 0x0001 )
#define HB_IT_INTEGER   ( ( USHORT ) 0x0002 )
#define HB_IT_LONG      ( ( USHORT ) 0x0008 )
#define HB_IT_DOUBLE    ( ( USHORT ) 0x0010 )
#define HB_IT_DATE      ( ( USHORT ) 0x0020 )
#define HB_IT_LOGICAL   ( ( USHORT ) 0x0080 )
#define HB_IT_SYMBOL    ( ( USHORT ) 0x0100 )
#define HB_IT_ALIAS     ( ( USHORT ) 0x0200 )
#define HB_IT_STRING    ( ( USHORT ) 0x0400 )
#define HB_IT_MEMOFLAG  ( ( USHORT ) 0x0800 )
#define HB_IT_MEMO      ( HB_IT_MEMOFLAG | HB_IT_STRING )
#define HB_IT_BLOCK     ( ( USHORT ) 0x1000 )
#define HB_IT_BYREF     ( ( USHORT ) 0x2000 )
#define HB_IT_MEMVAR    ( ( USHORT ) 0x4000 )
#define HB_IT_ARRAY     ( ( USHORT ) 0x8000 )
#define HB_IT_OBJECT    HB_IT_ARRAY
#define HB_IT_NUMERIC   ( ( USHORT ) ( HB_IT_INTEGER | HB_IT_LONG | HB_IT_DOUBLE ) )
#define HB_IT_ANY       ( ( USHORT ) 0xFFFF )

#define HB_IS_OF_TYPE( p, t ) ( ( ( p )->type & ~HB_IT_BYREF ) == t )
#define HB_IS_BYREF( p )   ( ( p )->type & HB_IT_BYREF )
#define HB_IS_ARRAY( p )   HB_IS_OF_TYPE( p, HB_IT_ARRAY )
#define HB_IS_NIL( p )     HB_IS_OF_TYPE( p, HB_IT_NIL )
#define HB_IS_BLOCK( p )   HB_IS_OF_TYPE( p, HB_IT_BLOCK )
#define HB_IS_DATE( p )    HB_IS_OF_TYPE( p, HB_IT_DATE )
#define HB_IS_DOUBLE( p )  HB_IS_OF_TYPE( p, HB_IT_DOUBLE )
#define HB_IS_INTEGER( p ) HB_IS_OF_TYPE( p, HB_IT_INTEGER )
#define HB_IS_LOGICAL( p ) HB_IS_OF_TYPE( p, HB_IT_LOGICAL )
#define HB_IS_LONG( p )    HB_IS_OF_TYPE( p, HB_IT_LONG )
#define HB_IS_NUMERIC( p ) ( ( p )->type & HB_IT_NUMERIC )
#define HB_IS_OBJECT( p )  HB_IS_OF_TYPE( p, HB_IT_OBJECT )
#define HB_IS_STRING( p )  ( ( ( p )->type & ~( HB_IT_BYREF | HB_IT_MEMOFLAG ) ) == HB_IT_STRING )
#define HB_IS_MEMO( p )    HB_IS_OF_TYPE( p, HB_IT_MEMO )
#define HB_IS_SYMBOL( p )  HB_IS_OF_TYPE( p, HB_IT_SYMBOL )
#define HB_IS_MEMVAR( p )  HB_IS_OF_TYPE( p, HB_IT_MEMVAR )
#define HB_IS_POINTER( p ) HB_IS_OF_TYPE( p, HB_IT_POINTER )

#define ISNIL( n )         ( hb_param( n, HB_IT_ANY ) == NULL || HB_IS_NIL( hb_param( n, HB_IT_ANY ) ) ) /* NOTE: Intentionally using a different method */
#define ISCHAR( n )        ( hb_param( n, HB_IT_STRING ) != NULL )
#define ISNUM( n )         ( hb_param( n, HB_IT_NUMERIC ) != NULL )
#define ISLOG( n )         ( hb_param( n, HB_IT_LOGICAL ) != NULL )
#define ISDATE( n )        ( hb_param( n, HB_IT_DATE ) != NULL )
#define ISMEMO( n )        ( hb_param( n, HB_IT_MEMO ) != NULL )
#define ISBYREF( n )       ( hb_parinfo( n ) & HB_IT_BYREF ) /* NOTE: Intentionally using a different method */
#define ISARRAY( n )       ( hb_param( n, HB_IT_ARRAY ) != NULL )
#define ISOBJECT( n )      ( ISARRAY( n ) && hb_param( n, HB_IT_ARRAY )->asArray.value->uiClass != 0 )
#define ISBLOCK( n )       ( hb_param( n, HB_IT_BLOCK ) != NULL ) /* Not available in CA-Cl*pper. */
#define ISPOINTER( n )     ( hb_param( n, HB_IT_POINTER ) != NULL ) /* Not available in CA-Cl*pper. */

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
   long value;
};

/* this definition signals that number of decimal places for double value
 * was not specified at compile time (the value is a result of optimization
 * performed by the compiler)
 */
#define HB_DEFAULT_WIDTH     255
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
   union {
      struct _HB_CODEBLOCK * block;    /* codeblock */
      struct _HB_ITEM ** itemsbase;    /* static variables */
      struct _HB_ITEM ** *itemsbasePtr; /* local variables */
   } BasePtr;
   LONG offset;    /* 0 for static variables */
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

/* internal structure for codeblocks */
typedef struct _HB_CODEBLOCK
{
   BYTE *   pCode;        /* codeblock pcode */
   PHB_ITEM pLocals;      /* table with referenced local variables */
   USHORT   uiLocals;     /* number of referenced local variables */
   PHB_SYMB pSymbols;     /* codeblocks symbols */
   ULONG    ulCounter;    /* numer of references to this codeblock */
   BOOL     dynBuffer;    /* is pcode buffer allocated dynamically */
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

extern HB_SYMB  hb_symEval;

/* Extend API */
extern char *   hb_parc( int iParam, ... );  /* retrieve a string parameter */
extern ULONG    hb_parclen( int iParam, ... ); /* retrieve a string parameter length */
extern ULONG    hb_parcsiz( int iParam, ... ); /* retrieve a by-reference string parameter length, including terminator */
extern char *   hb_pards( int iParam, ... ); /* retrieve a date as a string yyyymmdd */
extern char *   hb_pardsbuff( char * szDate, int iParam, ... ); /* retrieve a date as a string yyyymmdd */
extern ULONG    hb_parinfa( int iParamNum, ULONG uiArrayIndex ); /* retrieve length or element type of an array parameter */
extern int      hb_parinfo( int iParam ); /* Determine the param count or data type */
extern int      hb_parl( int iParam, ... ); /* retrieve a logical parameter as an int */
extern double   hb_parnd( int iParam, ... ); /* retrieve a numeric parameter as a double */
extern int      hb_parni( int iParam, ... ); /* retrieve a numeric parameter as a integer */
extern long     hb_parnl( int iParam, ... ); /* retrieve a numeric parameter as a long */
extern PHB_ITEM hb_param( int iParam, int iMask ); /* retrieve a generic parameter */
extern PHB_ITEM hb_paramError( int iParam ); /* Returns either the generic parameter or a NIL item if param not provided */

#ifdef HB_API_MACROS

#include "hbapiitm.h"

#define hb_pcount()                          ( ( int ) hb_stack.pBase->item.asSymbol.paramcnt )

#define hb_ret()                             hb_itemClear( &hb_stack.Return )
#define hb_reta( ulLen )                     hb_arrayNew( &hb_stack.Return, ulLen )
#define hb_retc( szText )                    hb_itemPutC( &hb_stack.Return, szText )
#define hb_retclen( szText, ulLen )          hb_itemPutCL( &hb_stack.Return, szText, ulLen )
#define hb_retds( szDate )                   hb_itemPutDS( &hb_stack.Return, szDate )
#define hb_retd( lYear, lMonth, lDay )       hb_itemPutD( &hb_stack.Return, lYear, lMonth, lDay )
#define hb_retdl( lJulian )                  hb_itemPutDL( &hb_stack.Return, lJulian )
#define hb_retl( iLogical )                  hb_itemPutL( &hb_stack.Return, iLogical ? TRUE : FALSE )
#define hb_retnd( dNumber )                  hb_itemPutND( &hb_stack.Return, dNumber )
#define hb_retni( iNumber )                  hb_itemPutNI( &hb_stack.Return, iNumber )
#define hb_retnl( lNumber )                  hb_itemPutNL( &hb_stack.Return, lNumber )
#define hb_retnlen( dNumber, iWidth, iDec )  hb_itemPutNLen( &hb_stack.Return, dNumber, iWidth, iDec )
#define hb_retndlen( dNumber, iWidth, iDec ) hb_itemPutNDLen( &hb_stack.Return, dNumber, iWidth, iDec )
#define hb_retnilen( iNumber, iWidth )       hb_itemPutNILen( &hb_stack.Return, iNumber, iWidth )
#define hb_retnllen( lNumber, iWidth )       hb_itemPutNLLen( &hb_stack.Return, lNumber, iWidth )

#else

extern int      hb_pcount( void );          /* returns the number of suplied parameters */

extern void     hb_ret( void );             /* post a NIL return value */
extern void     hb_retc( char * szText );   /* returns a string */
extern void     hb_retclen( char * szText, ULONG ulLen ); /* returns a string with a specific length */
extern void     hb_retds( char * szDate );  /* returns a date, must use yyyymmdd format */
extern void     hb_retd( long lYear, long lMonth, long lDay ); /* returns a date */
extern void     hb_retdl( long lJulian );   /* returns a long value as a julian date */
extern void     hb_retl( int iTrueFalse );  /* returns a logical integer */
extern void     hb_retnd( double dNumber ); /* returns a double */
extern void     hb_retni( int iNumber );    /* returns a integer number */
extern void     hb_retnl( long lNumber );   /* returns a long number */
extern void     hb_retnlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
extern void     hb_retndlen( double dNumber, int iWidth, int iDec ); /* returns a double, with specific width and decimals */
extern void     hb_retnilen( int iNumber, int iWidth ); /* returns a integer number, with specific width */
extern void     hb_retnllen( long lNumber, int iWidth ); /* returns a long number, with specific width */
extern void     hb_reta( ULONG ulLen );  /* returns an array with a specific length */

#endif

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
extern void *   hb_xgrab( ULONG ulSize );                 /* allocates memory, exits on failure */
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
extern BOOL     hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ); /* creates a new array */
extern ULONG    hb_arrayLen( PHB_ITEM pArray ); /* retrives the array len */
extern BOOL     hb_arrayIsObject( PHB_ITEM pArray ); /* retrives if the array is an object */
extern BOOL     hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue ); /* add a new item to the end of an array item */
extern BOOL     hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex ); /* insert a nil item into an array, without changing the length */
extern BOOL     hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex ); /* delete an array item, without changing length */
extern BOOL     hb_arraySize( PHB_ITEM pArray, ULONG ulLen ); /* sets the array total length */
extern BOOL     hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult ); /* retrieve last item in an array */
extern BOOL     hb_arrayRelease( PHB_ITEM pArray ); /* releases an array - don't call it - use ItemRelease() !!! */
extern BOOL     hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* sets an array element */
extern BOOL     hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); /* retrieves an item */
extern PHB_ITEM hb_arrayGetItemPtr( PHB_ITEM pArray, ULONG ulIndex ); /* returns pointer to specified element of the array */
extern ULONG    hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen ); /* copy a string into an array item */
extern char *   hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string contained on an array element */
extern char *   hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string pointer on an array element */
extern ULONG    hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the string length contained on an array element */
extern BOOL     hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the logical value contained on an array element */
extern int      hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the int value contained on an array element */
extern long     hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the long numeric value contained on an array element */
extern double   hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the double value contained on an array element */
extern char *   hb_arrayGetDS( PHB_ITEM pArray, ULONG ulIndex, char * szDate ); /* retrieves the date value contained in an array element */
extern long     hb_arrayGetDL( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the date value contained in an array element, as a long integer */
extern USHORT   hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex ); /* retrieves the type of an array item */
extern BOOL     hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount ); /* fill an array with a given item */
extern ULONG    hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount ); /* scan an array for a given item, or until code-block item returns TRUE */
extern BOOL     hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG * pulStart, ULONG * pulCount ); /* execute a code-block for every element of an array item */
extern BOOL     hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG * pulStart, ULONG * pulCount, ULONG * pulTarget ); /* copy items from one array to another */
extern PHB_ITEM hb_arrayClone( PHB_ITEM pArray ); /* returns a duplicate of an existing array, including all nested items */
extern BOOL     hb_arraySort( PHB_ITEM pArray, ULONG * pulStart, ULONG * pulCount, PHB_ITEM pBlock ); /* sorts an array item */
extern PHB_ITEM hb_arrayFromStack( USHORT uiLen ); /* Creates and returns an Array of n Elements from the Eval Stack - Does NOT pop the items. */
extern PHB_ITEM hb_arrayFromParams( void ); /* Creates and returns an Array of current Generic Parameters. */

/* string management */

#define HB_ISSPACE( c ) ( ( c ) == ' ' || \
                          ( c ) == HB_CHAR_HT || \
                          ( c ) == HB_CHAR_LF || \
                          ( c ) == HB_CHAR_CR )

extern int      hb_stricmp( const char * s1, const char * s2 ); /* compare two strings without regards to case */
extern int      hb_strnicmp( const char * s1, const char * s2, ULONG ulLen ); /* compare two string without regards to case, limited by length */
extern char *   hb_strupr( char * pszText ); /* convert a string in-place to upper-case */
extern char *   hb_strdup( const char * pszText ); /* returns a pointer to a newly allocated copy of the source string */
extern BOOL     hb_strMatchRegExp( const char * szString, const char * szMask ); /* compare two strings using a regular expression pattern */
extern BOOL     hb_strEmpty( const char * szText, ULONG ulLen ); /* returns whether a string contains only white space */
extern void     hb_strDescend( char * szStringTo, const char * szStringFrom, ULONG ulLen ); /* copy a string to a buffer, inverting each character */
extern ULONG    hb_strAt( const char * szSub, ULONG ulSubLen, const char * szText, ULONG ulLen ); /* returns an index to a sub-string within another string */
extern char *   hb_strUpper( char * szText, ULONG ulLen ); /* convert an existing string buffer to upper case */
extern char *   hb_strLower( char * szText, ULONG ulLen ); /* convert an existing string buffer to lower case */
extern char *   hb_strncpyUpper( char * pDest, const char * pSource, ULONG ulLen ); /* copy an existing string buffer to another buffer, as upper case */
extern double   hb_strVal( const char * szText, ULONG ulLen ); /* return the numeric value of a character string representation of a number */
extern char *   hb_strLTrim( const char * szText, ULONG * ulLen ); /* return a pointer to the first non-white space character */
extern ULONG    hb_strRTrimLen( const char * szText, ULONG ulLen, BOOL bAnySpace ); /* return length of a string, ignoring trailing white space (or true spaces) */

extern double   hb_numRound( double dResult, int iDec ); /* round a number to a specific number of digits */

/* class management */
extern void     hb_clsReleaseAll( void );    /* releases all defined classes */

/* object management */
extern char *   hb_objGetClsName( PHB_ITEM pObject ); /* retrieves an object class name */
extern PHB_FUNC hb_objGetMethod( PHB_ITEM pObject, PHB_SYMB pSymMsg ); /* returns the method pointer of a object class */
extern ULONG    hb_objHasMsg( PHB_ITEM pObject, char * szString ); /* returns TRUE/FALSE whether szString is an existing message for object */

/* dynamic symbol table management */
extern PHB_DYNS hb_dynsymGet( char * szName );    /* finds and creates a dynamic symbol if not found */
extern PHB_DYNS hb_dynsymNew( PHB_SYMB pSymbol ); /* creates a new dynamic symbol based on a local one */
extern PHB_DYNS hb_dynsymFind( char * szName );   /* finds a dynamic symbol */
extern PHB_DYNS hb_dynsymFindName( char * szName ); /* converts to uppercase and finds a dynamic symbol */
extern void     hb_dynsymLog( void );             /* displays all dynamic symbols */
extern void     hb_dynsymRelease( void );         /* releases the memory of the dynamic symbol table */
extern void     hb_dynsymEval( PHB_DYNS_FUNC pFunction, void * Cargo ); /* enumerates all dynamic symbols */

/* Command line and environment argument management */
extern void     hb_cmdargInit( int argc, char * argv[] ); /* initialize command line argument API's */
extern int      hb_cmdargARGC( void ); /* retrieve command line argument count */
extern char **  hb_cmdargARGV( void ); /* retrieve command line argument buffer pointer */
extern BOOL     hb_cmdargIsInternal( const char * szArg ); /* determine if a string is an internal setting */
extern BOOL     hb_cmdargCheck( const char * pszName ); /* Check if a given internal switch (like //INFO) was set */
extern char *   hb_cmdargString( const char * pszName ); /* Returns the string value of an internal switch (like //TEMPPATH:"C:\") */
extern int      hb_cmdargNum( const char * pszName ); /* Returns the numeric value of an internal switch (like //F:90) */
extern void     hb_cmdargProcessVM( void ); /* Check for command line internal arguments */

/* Symbol management */
extern PHB_SYMB hb_symbolNew( char * szName ); /* create a new symbol */

/* Codeblock management */
extern HB_CODEBLOCK_PTR hb_codeblockNew( BYTE * pBuffer, USHORT uiLocals, USHORT * pLocalPosTable, PHB_SYMB pSymbols ); /* create a code-block */
extern HB_CODEBLOCK_PTR hb_codeblockMacroNew( BYTE * pBuffer, USHORT usLen );
extern void     hb_codeblockDelete( HB_ITEM_PTR pItem ); /* delete a codeblock */
extern PHB_ITEM hb_codeblockGetVar( PHB_ITEM pItem, LONG iItemPos ); /* get local variable referenced in a codeblock */
extern PHB_ITEM hb_codeblockGetRef( HB_CODEBLOCK_PTR pCBlock, PHB_ITEM pRefer ); /* get local variable passed by reference */
extern void     hb_codeblockEvaluate( HB_ITEM_PTR pItem ); /* evaluate a codeblock */

/* memvars subsystem */
extern HB_HANDLE hb_memvarValueNew( HB_ITEM_PTR pSource, BOOL bTrueMemvar ); /* create a new global value */
extern HB_VALUE_PTR * hb_memvarValueBaseAddress( void ); /* retrieve the base address of the values table */
extern void     hb_memvarsInit( void ); /* initialize the memvar API system */
extern void     hb_memvarsRelease( void ); /* clear all PUBLIC and PRIVATE variables */
extern void     hb_memvarsFree( void ); /* release the memvar API system */
extern void     hb_memvarValueIncRef( HB_HANDLE hValue ); /* increase the reference count of a global value */
extern void     hb_memvarValueDecRef( HB_HANDLE hValue ); /* decrease the reference count of a global value */
extern void     hb_memvarValueDecGarbageRef( HB_HANDLE hValue ); /* decrease the reference count of a detached local variable */
extern void     hb_memvarSetValue( PHB_SYMB pMemvarSymb, HB_ITEM_PTR pItem ); /* copy an item into a symbol */
extern ERRCODE  hb_memvarGet( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb ); /* copy an symbol value into an item */
extern void     hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb ); /* copy an symbol value into an item, with error trapping */
extern void     hb_memvarGetRefer( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb ); /* copy a reference to a symbol value into an item, with error trapping */
extern ULONG    hb_memvarGetPrivatesBase( void ); /* retrieve current PRIVATE variables stack base */
extern void     hb_memvarSetPrivatesBase( ULONG ulBase ); /* release PRIVATE variables created after specified base */
extern void     hb_memvarNewParameter( PHB_SYMB pSymbol, PHB_ITEM pValue );
extern char   * hb_memvarGetStrValuePtr( char * szVarName, ULONG *pulLen );
extern void     hb_memvarCreateFromItem( PHB_ITEM pMemvar, BYTE bScope, PHB_ITEM pValue );
extern int      hb_memvarScope( char * szVarName, ULONG ulLength ); /* retrieve scope of a dynamic variable symbol */

/* console I/O subsystem */
extern void     hb_conInit( void ); /* initialize the console API system */
extern void     hb_conRelease( void ); /* release the console API system */
extern char *   hb_conNewLine( void ); /* retrieve a pointer to a static buffer containing new-line characters */
extern void     hb_conOutStd( char * pStr, ULONG ulLen ); /* output an string to STDOUT */
extern void     hb_conOutErr( char * pStr, ULONG ulLen ); /* output an string to STDERR */
extern USHORT   hb_conSetCursor( BOOL bSetCursor, USHORT usNewCursor ); /* retrieve and optionally set cursor shape */
extern char *   hb_conSetColor( char * szColor ); /* retrieve and optionally set console color */
extern void     hb_conXSaveRestRelease( void ); /* release the save/restore API */

/* compiler and macro compiler */
extern char *   hb_compReservedName( char * szName ); /* determines if a string contains a reserve word */

/* misc */
extern char *   hb_procname( int iLevel, char * szName ); /* retrieve a procedure name into a buffer */

/* macro compiler */

typedef struct HB_CBVAR_  /* This structure holds local variables declared in a codeblock */
{
   char * szName;
   BYTE bType;
   struct HB_CBVAR_ * pNext;
} HB_CBVAR, * HB_CBVAR_PTR;

typedef struct HB_PCODE_INFO_   /* compiled pcode container */
{
   BYTE * pCode;           /* pointer to a memory block where pcode is stored */
   ULONG  lPCodeSize;      /* total memory size for pcode */
   ULONG  lPCodePos;       /* actual pcode offset */
   struct HB_PCODE_INFO_ * pPrev;
   HB_CBVAR_PTR pLocals;
} HB_PCODE_INFO, * HB_PCODE_INFO_PTR;

typedef struct HB_MACRO_    /* a macro compiled pcode container */
{
   char * string;          /* compiled string */
   ULONG  length;          /* length of the string */
   ULONG  pos;             /* current position inside of compiled string */
   int    Flags;           /* some flags we may need */
   int    status;          /* status of compilation */
   int    FlexState;       /* internal flex state during parsing */
   HB_PCODE_INFO_PTR pCodeInfo;  /* pointer to pcode buffer and info */
   void * pParseInfo;      /* data needed by the parser - it should be 'void *' to allow different implementation of macr compiler */
   USHORT uiNameLen;       /* the maximum symbol name length */
   BOOL   bShortCuts;      /* are we using logical shorcuts (in OR/AND)  */
   int exprType;        /* type of successfully compiled expression */
} HB_MACRO, * HB_MACRO_PTR;

extern void   hb_macroGetValue( HB_ITEM_PTR pItem ); /* retrieve results of a macro expansion */
extern void   hb_macroSetValue( HB_ITEM_PTR pItem ); /* assign a value to a macro-expression item */
extern void   hb_macroTextValue( HB_ITEM_PTR pItem ); /* macro text substitution */
extern void   hb_macroPushSymbol( HB_ITEM_PTR pItem ); /* handle a macro function calls, e.g. var := &macro() */
extern void   hb_macroRun( HB_MACRO_PTR pMacro ); /* executes pcode compiled by macro compiler */
extern HB_MACRO_PTR hb_macroCompile( char * szString ); /* compile a string and return a pcode buffer */
extern void   hb_macroDelete( HB_MACRO_PTR pMacro ); /* release all memory allocated for macro evaluation */
extern char * hb_macroTextSubst( char * szString, ULONG *pulStringLen ); /* substitute macro variables occurences within a given string */
extern BOOL   hb_macroIsIdent( char * szString ); /* determine if a string is a valid function or variable name */
extern void   hb_macroPopAliasedValue( HB_ITEM_PTR pAlias, HB_ITEM_PTR pVar ); /* compiles and evaluates an aliased macro expression */
extern void   hb_macroPushAliasedValue( HB_ITEM_PTR pAlias, HB_ITEM_PTR pVar ); /* compiles and evaluates an aliased macro expression */
extern char * hb_macroGetType( HB_ITEM_PTR pItem ); /* determine the type of an expression */

/* garbage collector */
#define HB_GARBAGE_FUNC( hbfunc )   void hbfunc( void * Cargo )	/* callback function for cleaning garbage memory pointer */
typedef HB_GARBAGE_FUNC( HB_GARBAGE_FUNC_ );
typedef HB_GARBAGE_FUNC_ *HB_GARBAGE_FUNC_PTR;

extern HB_ITEM_PTR hb_gcGripGet( HB_ITEM_PTR pItem );
extern void hb_gcGripDrop( HB_ITEM_PTR pItem );
#define _getGrip hb_gcGripGet
#define _getDrop hb_gcGripDrop

extern void * hb_gcAlloc( ULONG ulSize, HB_GARBAGE_FUNC_PTR pFunc ); /* allocates a memory controlled by the garbage collector */
extern void   hb_gcFree( void *pAlloc ); /* deallocates a memory allocated by the garbage collector */
extern void * hb_gcLock( void *pAlloc ); /* do not release passed memory block */
extern void * hb_gcUnlock( void *pAlloc ); /* passed block is allowed to be released */
extern void   hb_gcCollect( void ); /* checks if a single memory block can be released */
extern void   hb_gcCollectAll( void ); /* checks if all memory blocks can be released */
extern void   hb_gcItemRef( HB_ITEM_PTR pItem ); /* checks if passed item refers passed memory block pointer */
extern void   hb_vmIsLocalRef( void ); /* hvm.c - mark all local variables as used */
extern void   hb_vmIsStaticRef( void ); /* hvm.c - mark all static variables as used */
extern void   hb_memvarsIsMemvarRef( void ); /* memvars.c - mark all memvar variables as used */
extern void   hb_clsIsClassRef( void ); /* classes.c - mark all class internals as used */
extern HB_GARBAGE_FUNC( hb_codeblockDeleteGarbage ); /* clear a codeblock before releasing by the GC */
extern HB_GARBAGE_FUNC( hb_arrayReleaseGarbage ); /* clear an array before releasing by the GC */

/* idle states */
extern void   hb_idleState( void ); /* services a single idle state */
extern void   hb_idleReset( void ); /* services a single idle state */
extern void   hb_idleShutDown( void ); /* closes all background tasks */

/* misc */
extern char * hb_verPlatform( void ); /* retrieves a newly allocated buffer containing platform version */
extern char * hb_verCompiler( void ); /* retrieves a newly allocated buffer containing compiler version */
extern char * hb_verHarbour( void ); /* retrieves a newly allocated buffer containing harbour version */
extern void   hb_verBuildInfo( void ); /* display harbour, compiler, and platform versions to standard console */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_APIEXT_H_ */

