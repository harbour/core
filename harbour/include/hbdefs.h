/*
 * $Id$
 */

#ifndef HB_DEFS_H_
#define HB_DEFS_H_

#include <limits.h>

#if defined(__IBMCPP__)
  /* With the exception of WORD, the IBM Visual Age C++ compiler has
     its own definitions of the Harbour types defined in the #else
     section of this #ifdef block, most of which conflict with the
     Harbour #undefs, due to typedef being the prevalent method of
     defining the types in IBMCPP, whereas Harbour assumes that the
     definitions that it is replacing have been defined using
     #define. Therefore, it is necessary to skip the Harbour
     definition section when using the IBMCPP compiiler, include
     the IBMCPP type definitions, and then add the definition for WORD

     NOTE: This only applies to the common types that most C compilers
           define. Any new types, particulary those that start with
           HB_, must be placed AFTER the #endif __IBMCPP__ line!
  */
  #define INCL_TYPES
  #include <os2.h>
  typedef unsigned short int WORD;
  #undef INT
  #undef UINT

#else

#if ! defined(HB_DONT_DEFINE_BASIC_TYPES)

#undef BOOL                            /* boolean */
typedef int BOOL;

#undef BYTE
typedef unsigned char BYTE;            /* 1 byte unsigned */

#undef SHORT                           /* 2 bytes signed */
typedef short int SHORT;

#undef WORD                            /* 2 bytes unsigned */
typedef unsigned short int WORD;

#undef USHORT                          /* 2 bytes unsigned */
typedef unsigned short int USHORT;

#undef LONG                            /* 4 bytes signed */
typedef long LONG;

#undef ULONG                           /* 4 bytes unsigned */
typedef unsigned long ULONG;

#undef DWORD                           /* 4 bytes unsigned */
typedef unsigned long DWORD;

#undef FALSE
#undef TRUE
#define FALSE  0
#define TRUE   1

#define LOBYTE(w)		((BYTE)(w))
#define HIBYTE(w)		((BYTE)(((WORD)(w) >> 8) & 0xFF))
#define LOWORD(l)		((WORD)(l))

#endif /* HB_DONT_DEFINE_BASIC_TYPES */
#endif /* __IBMCPP__ */

#ifndef MAX
#define MAX(a,b)                (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a,b)                (((a) < (b)) ? (a) : (b))
#endif

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

typedef HARBOUR ( * PHB_FUNC )( void );
typedef PHB_FUNC HB_FUNC_PTR;

#ifndef _POSIX_PATH_MAX
   #define _POSIX_PATH_MAX    255
#endif

typedef LONG HB_HANDLE;     /* handle to memvar value */
typedef char SYMBOLSCOPE;   /* stores symbol's scope */

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
#define IS_MEMVAR( p )     IS_OF_TYPE( p, IT_MEMVAR )

#define ISNIL( n )         ( hb_param( n, IT_NIL ) != NULL )
#define ISCHAR( n )        ( hb_param( n, IT_STRING ) != NULL )
#define ISNUM( n )         ( hb_param( n, IT_NUMERIC ) != NULL )
#define ISLOG( n )         ( hb_param( n, IT_LOGICAL ) != NULL )
#define ISDATE( n )        ( hb_param( n, IT_DATE ) != NULL )
#define ISMEMO( n )        ( hb_param( n, IT_MEMO ) != NULL )
#define ISBYREF( n )       ( hb_param( n, IT_BYREF ) != NULL )
#define ISARRAY( n )       ( hb_param( n, IT_ARRAY ) != NULL )

#define PCOUNT             hb_parinfo( 0 )
#define ALENGTH( n )       hb_parinfa( n, 0 )

#endif /* HB_DEFS_H_ */
