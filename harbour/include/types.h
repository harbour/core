/*
 * $Id$
 */

#ifndef _TYPES_H
#define _TYPES_H

#undef BYTE
typedef unsigned char BYTE, * PBYTE;   /* 1 byte */

#undef WORD                            /* 2 bytes unsigned */
typedef unsigned short int WORD;

#undef SHORT                           /* 2 bytes signed */
typedef short int SHORT;

#undef LONG                            /* 4 bytes signed */
typedef long LONG;
typedef unsigned long ULONG;

#undef DWORD                           /* 4 bytes unsigned */
typedef unsigned long DWORD;

#undef FALSE
#undef TRUE
#define FALSE  0
#define TRUE   1
typedef int BOOL;

#undef PVOID
typedef void * PVOID;

#define LOBYTE(w)           ((BYTE)(w))
#define HIBYTE(w)           ((BYTE)(((WORD)(w) >> 8) & 0xFF))
#define LOWORD(l)           ((WORD)(l))

#ifdef __GNUC__
   #define pascal __attribute__ ((stdcall))
#endif

#ifdef _MSC_VER
   #define HARBOUR void
   #define EXTERNAL_LINKAGE
#else
#ifdef __IBMCPP__
   #define HARBOUR void
   #define EXTERNAL_LINKAGE _LNK_CONV
#else
   #define HARBOUR void pascal
   #define EXTERNAL_LINKAGE
#endif
#endif
typedef HARBOUR ( * HARBOURFUNC )( void );

#ifndef _POSIX_PATH_MAX
   #define _POSIX_PATH_MAX    255
#endif

#define IS_OF_TYPE( p, t ) ( ( p )->wType == t )
#define IS_ARRAY( p )      IS_OF_TYPE( p, IT_ARRAY )
#define IS_BYREF( p )      IS_OF_TYPE( p, IT_BYREF )
#define IS_NIL( p )        IS_OF_TYPE( p, IT_NIL )
#define IS_BLOCK( p )      IS_OF_TYPE( p, IT_BLOCK )
#define IS_DATE( p )       IS_OF_TYPE( p, IT_DATE )
#define IS_DOUBLE( p )     IS_OF_TYPE( p, IT_DOUBLE )
#define IS_INTEGER( p )    IS_OF_TYPE( p, IT_INTEGER )
#define IS_LOGICAL( p )    IS_OF_TYPE( p, IT_LOGICAL )
#define IS_LONG( p )       IS_OF_TYPE( p, IT_LONG )
#define IS_NUMERIC( p )    ( ( p )->wType & IT_NUMERIC )
#define IS_OBJECT( p )     IS_OF_TYPE( p, IT_ARRAY )
#define IS_STRING( p )     IS_OF_TYPE( p, IT_STRING )
#define IS_SYMBOL( p )     IS_OF_TYPE( p, IT_SYMBOL )

#define _PCOUNT            _parinfo( 0 )
#define ISCHAR( n )        _param( n, IT_STRING )
#define ISNUM( n )         _param( n, IT_NUMERIC )
#define ISLOG( n )         _param( n, IT_LOGICAL )
#define ISDATE( n )        _param( n, IT_DATE )
/* #define ISMEMO( n )     _param( n, IT_MEMO ) TODO */
#define ISBYREF( n )       _param( n, IT_BYREF )
#define ISARRAY( n )       _param( n, IT_ARRAY )
#define ALENGTH( n )       _parinfa( n, 0 ) /* TODO! */

#endif
