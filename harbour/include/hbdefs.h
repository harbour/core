/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for compiler and runtime basic type declarations
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#ifndef HB_DEFS_H_
#define HB_DEFS_H_

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbsetup.h"
#include "hbtrace.h"

/* Include windows.h if applicable and requested */

#if defined(HB_OS_WIN_32_USED) && defined(HB_OS_WIN_32)
   #define WIN32_LEAN_AND_MEAN
   #include <windows.h>
   #if defined(__GNUC__)
      #define HB_DONT_DEFINE_BASIC_TYPES
   #endif
#endif

#if defined(__IBMCPP__) || defined(HARBOUR_GCC_OS2)
   /* With the exception of WORD, the IBM Visual Age C++ compiler has
      its own definitions of the Harbour types most of which conflict with the
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
   /* 28/03/2000 - maurilio.longo@libero.it
      The same holds true when using GCC under OS/2
   */
   #define INCL_TYPES
   #include <os2.h>
   #undef INT
   #undef UINT
   #define HB_DONT_DEFINE_BASIC_TYPES
#endif /* __IBMCPP__ */

#if ! defined(HB_DONT_DEFINE_BASIC_TYPES)

   #undef BOOL                            /* boolean */
   typedef int BOOL;

   #undef BYTE
   typedef unsigned char BYTE;            /* 1 byte unsigned */

   #undef SHORT                           /* 2 bytes signed */
   typedef short int SHORT;

   #undef USHORT                          /* 2 bytes unsigned */
   typedef unsigned short int USHORT;

   #undef LONG                            /* 4 bytes signed */
   typedef long LONG;

   #undef ULONG                           /* 4 bytes unsigned */
   typedef unsigned long ULONG;

   #undef FALSE
   #define FALSE  0
   #undef TRUE
   #define TRUE   1

#endif /* HB_DONT_DEFINE_BASIC_TYPES */

#define HB_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define HB_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define HB_LOBYTE( w )          ( ( BYTE ) ( w ) )
#define HB_HIBYTE( w )          ( ( BYTE ) ( ( ( USHORT ) ( w ) >> 8 ) & 0xFF ) )
#define HB_MKSHORT( lo, hi )    ( ( SHORT ) ( ( ( SHORT ) hi ) << 8 ) | lo )
#define HB_MKUSHORT( lo, hi )   ( ( USHORT ) ( ( ( USHORT ) hi ) << 8 ) | lo )
#define HB_MKLONG( b1, b2, b3, b4 )  ( ( ( LONG ) b4 ) << 24 ) | \
                                     ( ( ( LONG ) b3 ) << 16 ) | \
                                     ( ( ( LONG ) b2 ) <<  8 ) | \
                                     ( ( ( LONG ) b1 ) )
#define HB_MKULONG( b1, b2, b3, b4 ) ( ( ( ULONG ) b4 ) << 24 ) | \
                                     ( ( ( ULONG ) b3 ) << 16 ) | \
                                     ( ( ( ULONG ) b2 ) <<  8 ) | \
                                     ( ( ( ULONG ) b1 ) )

#define HB_SYMBOL_UNUSED( symbol ) ( void ) symbol

/* ***********************************************************************
 * The name of starting procedure
 * Note: You have to define it in case when Harbour cannot find the proper
 * starting procedure (due to incorrect order of static data initialization)
 *
 * The list of compilers that require it:
 * - Watcom C/C++ 10.0
 * - GCC on Linux
 *
 * By default we are using automatic lookup (symbol not defined)
*/
#if defined(__WATCOMC__) || ( defined(__GNUC__) && !defined(__DJGPP__) && !defined(HARBOUR_GCC_OS2) )
   #define HARBOUR_START_PROCEDURE "MAIN"
#endif

#if defined(HB_FUNC_CALLCONV)
   #define HARBOUR void HB_FUNC_CALLCONV
#else
   #define HARBOUR void
#endif

#define __HARBOUR__

typedef HARBOUR ( * PHB_FUNC )( void );
typedef PHB_FUNC HB_FUNC_PTR;

/* Function declaration macros */

/* NOTE: The prefix is "HB_FUN_" currently, this is needed to
         avoid collision with any other declared symbol.
         Note that "HB_" is not enough, since the Harbour internals
         are also prefixed with HB_. [vszakats] */

#define HB_FUNCNAME( funcname ) HB_FUN_##funcname
#define HB_FUNC( funcname )     HARBOUR HB_FUN_##funcname ( void )

/* */

typedef ULONG HB_HANDLE;        /* handle to memvar value */
typedef char  HB_SYMBOLSCOPE;   /* stores symbol's scope */

typedef BYTE HB_CHAR;
typedef BYTE HB_ATTR;

/* Some common character constants */

#define HB_CHAR_NUL             '\0'    /*   0 - NUL */
#define HB_CHAR_EOS             HB_CHAR_NUL
#define HB_CHAR_BEL             '\a'    /*   7 - Bell */
#define HB_CHAR_BS              '\b'    /*   8 - Backspace */
#define HB_CHAR_HT              '\t'    /*   9 - Tab horizontal */
#define HB_CHAR_LF              '\n'    /*  10 - Linefeed */
#define HB_CHAR_VT              '\v'    /*  11 - Tab vertical */
#define HB_CHAR_FF              '\f'    /*  12 - Formfeed */
#define HB_CHAR_CR              '\r'    /*  13 - Carriage return */
#define HB_CHAR_EOF             '\x1A'  /*  26 - End of file marker */

#endif /* HB_DEFS_H_ */
