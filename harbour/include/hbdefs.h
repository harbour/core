/*
 * $Id$
 */

#ifndef HB_DEFS_H_
#define HB_DEFS_H_

#include <limits.h>

#ifdef __IBMCPP__
  #define INCL_TYPES
  #include <os2.h>
  typedef unsigned short int WORD;
  #undef INT
  #undef UINT
#else

#undef BYTE
typedef unsigned char BYTE, * PBYTE;   /* 1 byte unsigned */

#undef WORD                            /* 2 bytes unsigned */
typedef unsigned short int WORD;

#undef SHORT                           /* 2 bytes signed */
typedef short int SHORT;

#undef USHORT                          /* 2 bytes unsigned */
typedef unsigned short int USHORT;

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

#endif /* __IBMCPP__ */

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

#define HB_SYMBOL_UNUSED(symbol) (void)symbol

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
#define IS_OBJECT( p )     IS_OF_TYPE( p, IT_ARRAY )
#define IS_STRING( p )     IS_OF_TYPE( p, IT_STRING )
#define IS_SYMBOL( p )     IS_OF_TYPE( p, IT_SYMBOL )

#define PCOUNT             hb_parinfo( 0 )

#define ISCHAR( n )        hb_param( n, IT_STRING )
#define ISNUM( n )         hb_param( n, IT_NUMERIC )
#define ISLOG( n )         hb_param( n, IT_LOGICAL )
#define ISDATE( n )        hb_param( n, IT_DATE )
#define ISMEMO( n )        hb_param( n, IT_MEMO )
#define ISBYREF( n )       hb_param( n, IT_BYREF )
#define ISARRAY( n )       hb_param( n, IT_ARRAY )
#define ALENGTH( n )       hb_parinfa( n, 0 ) /* TODO! */

#endif /* HB_DEFS_H_ */
