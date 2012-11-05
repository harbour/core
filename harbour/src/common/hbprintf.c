/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_sprintf() function.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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

/*
   patterm format:
   '%' [<flags>*] [<field width>] [.<precision>] [<length modifier>]
       <conversion specifier>
 */

/*
   The folowwing conversions are not explicitly supported:
      A, a
      E, e
      G, g
   These are (long) double conversions. If necessary they can be easy added.
   Now they are simply redirected to F, f conversions.

      C (or Lc)
      S (or Ls)
   These are wide character conversions and needs locale settings.

   double conversion if not necessary can be disabled to not create unnencessary
   overhead and/or references to math library by
      #define __NO_DOUBLE__
   It can be also greatly optimized anyhow it will increase dependences list and
   reduce portability.
   Internally 'long double' is used for all calculations. If some platforms do
   not support it then it can be eliminated by
      #define __NO_LONGDOUBLE__

   If positional parameters are not necessary then support for them can be
   disabled by
      #define __NO_ARGPOS__
   In such case this code neither allocates memory nor extensively use stack
   as memory buffer. All conversions are done "on the fly". If memory
   allocations or stack size is not a problem then some parts can be easy
   optimized.
 */


/* #define __NO_DOUBLE__ */
/* #define __NO_LONGDOUBLE__ */
/* #define __NO_LONGLONG__ */
/* #define __NO_ARGPOS__ */


/* hbfloat.h have to be included first */
#include "hbfloat.h"
#include "hbapicdp.h"
#include <stddef.h>

#if defined( __FreeBSD__ )
#include <sys/param.h>
#endif

#if defined( HB_LONG_DOUBLE_OFF ) && ! defined( __NO_LONGDOUBLE__ )
#  define __NO_LONGDOUBLE__
#endif

#if defined( HB_LONG_LONG_OFF ) && ! defined( __NO_LONGLONG__ )
#  define __NO_LONGLONG__
#endif


#if ! defined( HB_USE_CRTL_SNPRINTF )

/* few macros for some platform dependent floating point functions/macros */

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) || \
    ( defined( __WATCOMC__ ) && __WATCOMC__ < 1270 ) || \
    defined( HB_OS_QNX ) || defined( HB_OS_SYMBIAN ) || \
    defined( __DCC__ ) || defined( __ACK__ ) || \
    ( defined( __DJGPP__ ) && \
      ( __DJGPP__ < 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ <= 3 ) ) ) || \
    ( defined( _MSC_VER ) && \
      !( defined( __LCC__ ) || defined( __POCC__ ) || defined( __XCC__ ) ) )
   /* TODO: add other C compilers which does not support [u]intmax_t
    *       definitions (check C compiler version number).
    *       If compiler supports stdint.h then it should be added
    *       to hbdefs.h.
    */
#  define intmax_t      _x_longlong
#  define uintmax_t     _x_ulonglong
#endif


#ifndef va_copy
#  ifdef __va_copy
#     define va_copy( dst, src )    __va_copy( dst, src )
#  else
#     define va_copy( dst, src )    ( (dst) = (src) )
#  endif
#endif


#define _ARGBUF_SIZE    16
#define _ARGBUF_ALLOC   16

#define _F_ALTERNATE    0x01  /* only for: o xX aA eE fF gG  */
#define _F_ZEROPADED    0x02  /* only for: d i o u xX aA eE fF gG */
#define _F_LEFTADJUSTED 0x04  /* clears _F_ZEROPADED */
#define _F_SPACE        0x08  /* only for signed num conversions: d i fF aA eE gG */
#define _F_SIGN         0x10  /* only for signed num conversions: d i fF aA eE gG, clears _F_SPACE */

#define _L_UNDEF_       0
#define _L_CHAR_        1
#define _L_SHORT_       2
#define _L_LONG_        3
#define _L_LONGLONG_    4
#define _L_INTMAX_      5
#define _L_SIZE_        6
#define _L_PTRDIFF_     7
#define _L_DOUBLE_      8
#define _L_LONGDOUBLE_  9

#ifndef __NO_DOUBLE__
#  ifdef __NO_LONGDOUBLE__
#     define _x_long_dbl      double
#     define _MODFD( x, p )   modf( x, p )
#  else
#     define _x_long_dbl      long double
#     if defined( HB_NO_MODFL ) || \
         defined( __WATCOMC__ ) || defined( __MINGW32CE__ ) || defined( HB_OS_CYGWIN ) || \
         defined( HB_OS_BEOS ) || defined( HB_OS_IPHONE ) || defined( HB_OS_SYMBIAN ) || \
         defined( __OpenBSD__ ) || defined( __NetBSD__ ) || defined( __DragonFly__ ) || \
         ( defined( __FreeBSD_version ) && __FreeBSD_version < 603000 ) || \
         defined( HB_OS_ANDROID ) || \
         ( defined( HB_OS_WIN_CE ) && defined( __POCC__ ) ) || defined( HB_OS_MINIX )
#        define _HB_WRAP_MODFL_
#        define _MODFD( x, p )   _hb_modfl( x, p )
#     else
#        define _MODFD( x, p )   modfl( x, p )
#     endif
#  endif
#  define _x_double           double
#else
#  define _x_long_dbl         int
#  define _x_double           int
#endif
#ifndef __NO_LONGLONG__
#  define _x_longlong         HB_LONGLONG
#  define _x_ulonglong        HB_ULONGLONG
#else
#  define _x_longlong         long
#  define _x_ulonglong        unsigned long
#endif
#define _x_int                int
#define _x_uint               unsigned int
#define _x_ulong              unsigned long
#define _x_long               long
#define _x_intmax_t           intmax_t
#define _x_uintmax_t          uintmax_t
#define _x_size_t             size_t
#define _x_ptrdiff_t          ptrdiff_t
#define _x_ptr                void *
#define _x_str                char *
#define _x_wchar              wchar_t
#define _x_wstr               _x_wchar *
#define _x_intptr             int *

#define v_x_int         1
#define v_x_uint        2
#define v_x_long        3
#define v_x_ulong       4
#define v_x_longlong    5
#define v_x_ulonglong   6
#define v_x_intmax_t    7
#define v_x_uintmax_t   8
#define v_x_size_t      9
#define v_x_ptrdiff_t   10
#define v_x_ptr         11
#define v_x_str         12
#define v_x_wstr        13
#define v_x_intptr      14
#define v_x_double      15
#define v_x_long_dbl    16

typedef union {
   _x_int         as_x_int;
   _x_uint        as_x_uint;
   _x_long        as_x_long;
   _x_ulong       as_x_ulong;
   _x_longlong    as_x_longlong;
   _x_ulonglong   as_x_ulonglong;
   _x_intmax_t    as_x_intmax_t;
   _x_uintmax_t   as_x_uintmax_t;
   _x_size_t      as_x_size_t;
   _x_ptrdiff_t   as_x_ptrdiff_t;
   _x_ptr         as_x_ptr;
   _x_str         as_x_str;
   _x_wstr        as_x_wstr;
   _x_intptr      as_x_intptr;
   _x_double      as_x_double;
   _x_long_dbl    as_x_long_dbl;
} x_type;

typedef struct {
   int      id;
   x_type   value;
} v_param;

typedef struct {
   int         maxarg;
   int         size;
   HB_BOOL     repeat;
   v_param *   arglst;
} v_paramlst;

#ifdef __NO_ARGPOS__

/* this version does not support positional parameters (f.e. '%1$d')
 * they can be added but it will force allocating additional
 * memory block in heap or stack and making second pass for
 * format decoding/coping.
 * so we will use only this simple macro which ignores parameter
 * positions.
 */
#define va_arg_n( va, type, n )     va_arg( va, type )

/* on some systems where each parameter allocates memory with
 * the same size in function stack frame this simple macro can
 * be used.
 */

/*
#define va_arg_n( va, type, n )     \
   ( { \
      type result; \
      if( n == 0 ) \
         result = va_arg( va, type ); \
      else \
      { \
         int count = (n); \
         va_list ap; \
         va_start( ap, format ); \
         do \
            result = va_arg( ap, type ); \
         while( --count > 0 ); \
         va_end( ap ); \
      } \
      result; \
   } )
*/

#else

#define va_arg_n( va, type, n )     \
               ( n == 0 ? va_arg( va, type ) : \
                          va_arg_get( n, &params, v##type )->value.as##type )

static v_param * va_arg_get( int iArg, v_paramlst * plst, int iType )
{
   if( plst->maxarg == 0 )
   {
      plst->repeat = HB_TRUE;
      memset( plst->arglst, 0, plst->size * sizeof( v_param ) );
   }
   if( plst->repeat )
   {
      if( iArg > plst->maxarg )
         plst->maxarg = iArg;
      if( iArg > plst->size )
      {
         int prev_size = plst->size;

         plst->size = iArg + _ARGBUF_ALLOC;
         if( prev_size == _ARGBUF_SIZE )
            plst->arglst = ( v_param * ) memcpy( hb_xgrab( plst->size * sizeof( v_param ) ),
                                                 plst->arglst, _ARGBUF_SIZE * sizeof( v_param ) );
         else
            plst->arglst = ( v_param * ) hb_xrealloc( plst->arglst, plst->size * sizeof( v_param ) );
         memset( &plst->arglst[ prev_size ], 0, ( plst->size - prev_size ) *
                                                  sizeof( v_param ) );
      }
      plst->arglst[ iArg - 1 ].id = iType;
   }
   return &plst->arglst[ iArg - 1 ];
}

static void va_arg_fill( v_paramlst * plst, va_list va )
{
   int iArg;
   for( iArg = 0; iArg < plst->maxarg; ++iArg )
   {
      switch( plst->arglst[ iArg ].id )
      {
         case v_x_uint:
             plst->arglst[ iArg ].value.as_x_uint = va_arg( va, _x_uint );
             break;
         case v_x_long:
             plst->arglst[ iArg ].value.as_x_long = va_arg( va, _x_long );
             break;
         case v_x_ulong:
             plst->arglst[ iArg ].value.as_x_ulong = va_arg( va, _x_ulong );
             break;
         case v_x_longlong:
             plst->arglst[ iArg ].value.as_x_longlong = va_arg( va, _x_longlong );
             break;
         case v_x_ulonglong:
             plst->arglst[ iArg ].value.as_x_ulonglong = va_arg( va, _x_ulonglong );
             break;
         case v_x_intmax_t:
             plst->arglst[ iArg ].value.as_x_intmax_t = va_arg( va, _x_intmax_t );
             break;
         case v_x_uintmax_t:
             plst->arglst[ iArg ].value.as_x_uintmax_t = va_arg( va, _x_uintmax_t );
             break;
         case v_x_size_t:
             plst->arglst[ iArg ].value.as_x_size_t = va_arg( va, _x_size_t );
             break;
         case v_x_ptrdiff_t:
             plst->arglst[ iArg ].value.as_x_ptrdiff_t = va_arg( va, _x_ptrdiff_t );
             break;
         case v_x_ptr:
             plst->arglst[ iArg ].value.as_x_ptr = va_arg( va, _x_ptr );
             break;
         case v_x_str:
             plst->arglst[ iArg ].value.as_x_str = va_arg( va, _x_str );
             break;
         case v_x_wstr:
             plst->arglst[ iArg ].value.as_x_wstr = va_arg( va, _x_wstr );
             break;
         case v_x_intptr:
             plst->arglst[ iArg ].value.as_x_intptr = va_arg( va, _x_intptr );
             break;
         case v_x_double:
             plst->arglst[ iArg ].value.as_x_double = va_arg( va, _x_double );
             break;
         case v_x_long_dbl:
             plst->arglst[ iArg ].value.as_x_long_dbl = va_arg( va, _x_long_dbl );
             break;
         default:
             plst->arglst[ iArg ].value.as_x_int = va_arg( va, _x_int );
             break;
      }
   }
}
#endif

#ifdef _HB_WRAP_MODFL_
_x_long_dbl _hb_modfl( _x_long_dbl x, _x_long_dbl * p )
{
   _x_double pd;
   _x_long_dbl r = modf( x, &pd );
   *p = pd;
   return r;
}
#endif

static char get_decimal( char c, const char **format, int *result )
{
   *result = c - '0';
   while( ( c = *(*format)++ ) >= '0' && c <= '9' )
      *result = *result * 10 + ( c - '0' );

   return c;
}

static size_t put_octal( char *buffer, size_t bufsize, size_t size,
                         uintmax_t value, int flags, int width, int precision )
{
   uintmax_t v = value;
   int nums = 0, n;
   char c;

   while( v )
   {
      ++nums;
      v >>= 3;
   }
   if( precision > nums )
      nums = precision;
   else if( flags & _F_ALTERNATE )
      ++nums;
   else if( nums == 0 && precision != 0 )
      ++nums;
   width -= nums;

   if( ( flags & _F_LEFTADJUSTED ) == 0 )
   {
      while( width > 0 )
      {
         if( size < bufsize )
            buffer[ size ] = ( flags & _F_ZEROPADED ) ? '0' : ' ';
         ++size;
         --width;
      }
   }
   if( nums )
   {
      n = nums;
      do
      {
         c = ( char ) ( value & 0x7 ) + '0';
         value >>= 3;
         --n;
         if( size + n < bufsize )
            buffer[ size + n ] = c;
      }
      while( n );
      size += nums;
   }
   while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }

   return size;
}

static size_t put_dec( char *buffer, size_t bufsize, size_t size,
                       uintmax_t value, int flags, int width, int precision,
                       int sign )
{
   uintmax_t v = value;
   int nums = 0, n;
   char c;

   while( v )
   {
      ++nums;
      v /= 10;
   }
   if( precision > nums )
      nums = precision;
   else if( nums == 0 && precision != 0 )
      ++nums;
   if( ( flags & ( _F_SPACE | _F_SIGN ) ) || sign )
      width--;
   if( ( flags & ( _F_LEFTADJUSTED | _F_ZEROPADED ) ) == _F_ZEROPADED &&
       width > nums )
      nums = width;
   width -= nums;

   if( ( flags & _F_LEFTADJUSTED ) == 0 )
   {
      while( width > 0 )
      {
         if( size < bufsize )
            buffer[ size ] = ' ';
         ++size;
         --width;
      }
   }
   if( ( flags & ( _F_SPACE | _F_SIGN ) ) || sign )
   {
      if( size < bufsize )
         buffer[ size ] = sign ? '-' : ( flags & _F_SIGN ? '+' : ' ' );
      ++size;
   }
   if( nums )
   {
      n = nums;
      do
      {
         c = ( char ) ( value % 10 ) + '0';
         value /= 10;
         --n;
         if( size + n < bufsize )
            buffer[ size + n ] = c;
      }
      while( n );
      size += nums;
   }
   while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }

   return size;
}

#ifndef __NO_DOUBLE__
static size_t put_dbl( char *buffer, size_t bufsize, size_t size,
                       _x_long_dbl value, int flags, int width, int precision )
{
   _x_long_dbl dInt, dFract;
   int sign, nums = 0, n;
   char c;

   if( precision < 0 )
      precision = 6;
   sign = hb_signbit( value );
   if( sign )
      value = - value;

   /* Round the number to given precision.
    * powl() is of course faster when precision is big but it is libm
    * (-lm link switch) function so we will make small trick here and
    * calculate it in a loop
    */
#if 0
   value += _POWD( 10, -precision ) / 2;
#else
   n = precision;
   dFract = 1;
   while( --n >= 0 )
      dFract /= 10;
   value += dFract / 2;
#endif

   dFract = _MODFD( value, &dInt );
   width -= precision;
   if( ( flags & ( _F_SPACE | _F_SIGN ) ) || sign )
      width--;
   if( precision > 0 || ( flags & _F_ALTERNATE ) )
      width--;
   value = dInt;
   do
   {
      ++nums;
      _MODFD( value / 10 + 0.01, &value );
   }
   while( value >= 1 );
   width -= nums;
   c = ( ( flags & ( _F_SPACE | _F_SIGN ) ) || sign ) ?
       ( buffer[ size ] = sign ? '-' : ( flags & _F_SIGN ? '+' : ' ' ) ) : 0;
   if( ( flags & _F_LEFTADJUSTED ) == 0 && width > 0 )
   {
      if( c && ( flags & _F_ZEROPADED ) )
      {
         if( size < bufsize )
            buffer[ size ] = c;
         ++size;
         c = 0;
      }
      do
      {
         if( size < bufsize )
            buffer[ size ] = ( flags & _F_ZEROPADED ) ? '0' : ' ';
         ++size;
      }
      while( --width > 0 );
   }
   if( c )
   {
      if( size < bufsize )
         buffer[ size ] = sign ? '-' : ( flags & _F_SIGN ? '+' : ' ' );
      ++size;
   }

   n = nums;
   do
   {
      value = _MODFD( dInt / 10 + 0.01, &dInt ) * 10;
      c = '0' + ( char ) ( value + 0.01 );
      --n;
      if( size + n < bufsize )
         buffer[ size + n ] = c;
   }
   while( n );
   size += nums;

   if( precision > 0 || ( flags & _F_ALTERNATE ) )
   {
      if( size < bufsize )
         buffer[ size ] = '.';
      ++size;
      while( precision > 0 )
      {
         dFract = _MODFD( dFract * 10, &dInt );
         c = '0' + ( char ) ( dInt + 0.01 );
         if( size < bufsize )
            buffer[ size ] = c;
         ++size;
         --precision;
      }
   }
   while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }

   return size;
}
#endif

static size_t put_hex( char *buffer, size_t bufsize, size_t size,
                       uintmax_t value, int flags, int width, int precision,
                       int upper )
{
   uintmax_t v = value;
   int nums = 0, n;
   char c;

   while( v )
   {
      ++nums;
      v >>= 4;
   }
   if( precision > nums )
      nums = precision;
   else if( nums == 0 && precision != 0 )
      ++nums;
   if( ( flags & _F_ALTERNATE ) && value )
      width -= 2;
   width -= nums;

   if( ( flags & ( _F_LEFTADJUSTED | _F_ZEROPADED ) ) == 0 )
   {
      while( width > 0 )
      {
         if( size < bufsize )
            buffer[ size ] = ' ';
         ++size;
         --width;
      }
   }
   if( ( flags & _F_ALTERNATE ) && value )
   {
      if( size < bufsize )
         buffer[ size ] = '0';
      ++size;
      if( size < bufsize )
         buffer[ size ] = upper ? 'X' : 'x';
      ++size;
   }
   if( ( flags & _F_LEFTADJUSTED ) == 0 )
   {
      while( width > 0 )
      {
         if( size < bufsize )
            buffer[ size ] = ( flags & _F_ZEROPADED ) ? '0' : ' ';
         ++size;
         --width;
      }
   }
   if( nums )
   {
      n = nums;
      do
      {
         c = ( char ) ( value & 0x0f ) + '0';
         if( c > '9' )
            c += upper ? 'A' - '9' - 1 : 'a' - '9' - 1;
         value >>= 4;
         --n;
         if( size + n < bufsize )
            buffer[ size + n ] = c;
      }
      while( n );
      size += nums;
   }
   while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }

   return size;
}

static int _hb_strnlen( const char * str, int len )
{
   int i = 0;

   while( len-- && *str++ )
      ++i;

   return i;
}

static size_t put_str( char *buffer, size_t bufsize, size_t size,
                       const _x_str str, int flags, int width, int precision )
{
   if( !str )
      str = "(null)";
   if( precision < 0 )
      precision = ( int ) strlen( str );
   else if( precision > 0 )
      precision = ( int ) _hb_strnlen( str, precision );

   width -= precision;
   if( ( flags & _F_LEFTADJUSTED ) == 0 ) while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }
   while( precision > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = *str++;
      ++size;
      --precision;
   }
   while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }

   return size;
}

static size_t put_wstr( char *buffer, size_t bufsize, size_t size,
                        const _x_wstr wstr, int flags, int width,
                        int precision )
{
   if( !wstr )
   {
      const _x_wchar wstr_null[] = { '(', 'n', 'u', 'l', 'l', ')', 0 };
      wstr = wstr_null;
   }

   if( precision < 0 )
   {
      precision = 0;
      while( wstr[ precision ] )
         ++precision;
   }
   else if( precision > 0 )
   {
      int size = precision;
      precision = 0;
      while( precision < size && wstr[ precision ] )
         ++precision;
   }

   width -= precision;
   if( ( flags & _F_LEFTADJUSTED ) == 0 ) while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }
   while( precision > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ( char ) *wstr++;
      ++size;
      --precision;
   }
   while( width > 0 )
   {
      if( size < bufsize )
         buffer[ size ] = ' ';
      ++size;
      --width;
   }

   return size;
}

int hb_printf_params( const char * format )
{
   int iParam = 0, iMax = 0;
   char c;

   do
   {
      c = *format++;
      if( c == '%' )
      {
         const char * pattern = format;
         int value, param = 0;

         c = *format++;
         if( c != 0 && c != '%' )
         {
            /* parameter position */
            if( c >= '0' && c <= '9' )
            {
               c = get_decimal( c, &format, &param );
               if( c != '$' )
                  format = pattern;
               c = *format++;
            }

            /* flags */
            value = 0;
            while( !value ) switch( c )
            {
               case '#':
               case '0':
               case '-':
               case ' ':
               case '+':
#ifdef _SUSV2_COMPAT_
               case '\'':  /* group with locale thousands' grouping characters */
#endif
                  c = *format++;
                  break;
               default:
                  value = 1;
                  break;
            }

            /* field width */
            if( c == '*' )
            {
               c = *format++;
               if( c >= '0' && c <= '9' )
               {
                  c = get_decimal( c, &format, &value );
                  if( c == '$' )
                  {
                     if( value > iMax )
                        iMax = value;
                     c = *format++;
                  }
                  /* else error, wrong format */
               }
               else
                  ++iParam;
            }
            else if( c >= '0' && c <= '9' )
               c = get_decimal( c, &format, &value );

            /* precision */
            if( c == '.' )
            {
               c = *format++;
               if( c == '*' )
               {
                  c = *format++;
                  if( c >= '0' && c <= '9' )
                  {
                     c = get_decimal( c, &format, &value );
                     if( c == '$' )
                     {
                        if( value > iMax )
                           iMax = value;
                        c = *format++;
                     }
                     /* else error, wrong format */
                  }
                  else
                     ++iParam;
               }
               else if( c >= '0' && c <= '9' )
                  c = get_decimal( c, &format, &value );
            }

            /* length modifier */
            switch( c )
            {
               case 'h':
                  c = *format++;
                  if( c == 'h' )
                     c = *format++;
                  break;
               case 'l':
                  c = *format++;
                  if( c == 'l' )
                     c = *format++;
                  break;
               case 'L':
                  c = *format++;
                  break;
               case 'j':
                  c = *format++;
                  break;
               case 'z':
                  c = *format++;
                  break;
               case 't':
                  c = *format++;
                  break;
               case 'I':   /* MS-Windows extension */
                  if( format[ 0 ] == '6' && format[ 1 ] == '4' )
                  {
                     format += 2;
                     c = *format++;
                     break;
                  }
                  else if( format[ 0 ] == '1' && format[ 1 ] == '6' )
                  {
                     format += 2;
                     c = *format++;
                     break;
                  }
                  else if( format[ 0 ] == '3' && format[ 1 ] == '2' )
                  {
                     format += 2;
                     c = *format++;
                  }
                  /* no break; */
               default:
                  break;
            }

            /* conversion specifier */
            switch( c )
            {
#ifndef __NO_DOUBLE__
               case 'a':
               case 'A':
               case 'e':
               case 'E':
               case 'g':
               case 'G':
               case 'f':   /* double decimal notation */
               case 'F':   /* double decimal notation */
#endif
               case 'd':
               case 'i':   /* signed int decimal conversion */
               case 'o':   /* unsigned int octal conversion */
               case 'u':   /* unsigned int decimal conversion */
               case 'x':   /* unsigned int hexadecimal conversion */
               case 'X':   /* unsigned int hexadecimal conversion */
               case 'p':   /* void * pointer */
               case 'c':   /* signed int casted to unsigned char */
               case 's':   /* const char * */
               case 'n':   /* store current result size in int * arg */
                  if( param == 0 )
                     ++iParam;
                  else if( param > iMax )
                     iMax = param;
                  break;
               case '%':   /* store % consuming arguments % */
                  break;
               default:    /* error, wrong format, store pattern */
                  format = pattern;
                  c = '%';
                  break;
            }
         }
      }
   }
   while( c );

   return iParam > iMax ? iParam : iMax;
}

int hb_vsnprintf( char * buffer, size_t bufsize, const char * format, va_list ap )
{
   va_list args;
   size_t size;
   char c;
#ifndef __NO_ARGPOS__
   const char * fmt_start = format;
   v_param argbuf[ _ARGBUF_SIZE ];
   v_paramlst params;

   params.size = _ARGBUF_SIZE;
   params.maxarg = 0;
   params.arglst = argbuf;
#endif


#ifndef __NO_ARGPOS__
   do
   {
      params.repeat = HB_FALSE;
      if( params.maxarg > 0 )
      {
         va_copy( args, ap );
         va_arg_fill( &params, args );
         va_end( args );
      }
      format = fmt_start;
#endif
      va_copy( args, ap );
      size = 0;

      do
      {
         c = *format++;
         if( c == '%' )
         {
            const char * pattern = format;

            c = *format++;
            if( c != 0 && c != '%' )
            {
               /* decode pattern */
               v_param argval;
               int param = 0, flags = 0, width = -1, precision = -1, length,
                   value, stop = 0;

               /* parameter position */
               if( c >= '0' && c <= '9' )
               {
                  c = get_decimal( c, &format, &value );
                  if( c == '$' )
                     param = value;
                  else
                     format = pattern;
                  c = *format++;
               }

               /* flags */
               while( !stop ) switch( c )
               {
                  case '#':
                     flags |= _F_ALTERNATE;
                     c = *format++;
                     break;
                  case '0':
                     flags |= _F_ZEROPADED;
                     c = *format++;
                     break;
                  case '-':
                     flags |= _F_LEFTADJUSTED;
                     c = *format++;
                     break;
                  case ' ':
                     flags |= _F_SPACE;
                     c = *format++;
                     break;
                  case '+':
                     flags |= _F_SIGN;
                     c = *format++;
                     break;
#ifdef _SUSV2_COMPAT_
                  case '\'':  /* group with locale thousands' grouping characters */
                     c = *format++;
                     break;
#endif
                  default:
                     stop = 1;
                     break;
               }

               /* field width */
               if( c == '*' )
               {
                  c = *format++;
                  if( c >= '0' && c <= '9' )
                  {
                     c = get_decimal( c, &format, &value );
                     if( c == '$' )
                     {
                        width = va_arg_n( args, _x_int, value );
                        c = *format++;
                     }
                     /* else error, wrong format */
                  }
                  else
                     width = va_arg_n( args, _x_int, 0 );
               }
               else if( c >= '0' && c <= '9' )
                  c = get_decimal( c, &format, &width );

               /* precision */
               if( c == '.' )
               {
                  precision = 0;
                  c = *format++;
                  if( c == '*' )
                  {
                     c = *format++;
                     if( c >= '0' && c <= '9' )
                     {
                        c = get_decimal( c, &format, &value );
                        if( c == '$' )
                        {
                           precision = va_arg_n( args, _x_int, value );
                           c = *format++;
                        }
                        /* else error, wrong format */
                     }
                     else
                        precision = va_arg_n( args, _x_int, 0 );
                  }
                  else if( c >= '0' && c <= '9' )
                     c = get_decimal( c, &format, &precision );
               }

               /* length modifier */
               switch( c )
               {
                  case 'h':
                     c = *format++;
                     if( c == 'h' )
                     {
                        length = _L_CHAR_;
                        c = *format++;
                     }
                     else
                        length = _L_SHORT_;
                     break;
                  case 'l':
                     c = *format++;
                     if( c == 'l' )
                     {
                        length = _L_LONGLONG_;
                        c = *format++;
                     }
                     else
                        length = _L_LONG_;
                     break;
                  case 'L':
                     length = _L_LONGDOUBLE_;
                     c = *format++;
                     break;
                  case 'j':
                     length = _L_INTMAX_;
                     c = *format++;
                     break;
                  case 'z':
                     length = _L_SIZE_;
                     c = *format++;
                     break;
                  case 't':
                     length = _L_PTRDIFF_;
                     c = *format++;
                     break;
                  case 'I':   /* MS-Windows extension */
                     if( format[ 0 ] == '6' && format[ 1 ] == '4' )
                     {
                        length = _L_LONGLONG_;
                        format += 2;
                        c = *format++;
                        break;
                     }
                     else if( format[ 0 ] == '1' && format[ 1 ] == '6' )
                     {
                        length = _L_SHORT_;
                        format += 2;
                        c = *format++;
                        break;
                     }
                     else if( format[ 0 ] == '3' && format[ 1 ] == '2' )
                     {
                        format += 2;
                        c = *format++;
                     }
                     /* no break; */
                  default:
                     length = _L_UNDEF_;
                     break;
               }

               /* conversion specifier */
               switch( c )
               {
#ifndef __NO_DOUBLE__
                  case 'a':
                  case 'A':
                  case 'e':
                  case 'E':
                  case 'g':
                  case 'G':
                     /* redirect above conversion to 'f' or 'F' type to keep
                      * valid parameters order
                      */
                     c = ( c == 'a' || c == 'e' || c == 'g' ) ? 'f' : 'F';
                     /* no break; */
                  case 'f':   /* double decimal notation */
                  case 'F':   /* double decimal notation */
                     if( length == _L_LONGDOUBLE_ )
                     {
                        argval.value.as_x_long_dbl = va_arg_n( args, _x_long_dbl, param );
                        HB_NUMTYPEL( value, argval.value.as_x_long_dbl );
                     }
                     else
                     {
                        double d = va_arg_n( args, _x_double, param );
                        HB_NUMTYPE( value, d );
                        argval.value.as_x_long_dbl =
                           ( value & ( _HB_NUM_NAN | _HB_NUM_PINF | _HB_NUM_NINF ) ) == 0 ? d : 0;
                     }
                     if( value & _HB_NUM_NAN )
                        size = put_str( buffer, bufsize, size,
                                        c == 'f' ?
                                        ( flags & _F_SIGN ? "+nan": "nan" ) :
                                        ( flags & _F_SIGN ? "+NAN": "NAN" ) ,
                                        flags, width, -1 );
                     else if( value & _HB_NUM_PINF )
                        size = put_str( buffer, bufsize, size,
                                        c == 'f' ?
                                        ( flags & _F_SIGN ? "+inf": "inf" ) :
                                        ( flags & _F_SIGN ? "+INF": "INF" ),
                                        flags, width, -1 );
                     else if( value & _HB_NUM_NINF )
                        size = put_str( buffer, bufsize, size,
                                        c == 'f' ? "-inf" : "-INF",
                                        flags, width, -1 );
                     else
                        size = put_dbl( buffer, bufsize, size, argval.value.as_x_long_dbl,
                                        flags, width, precision );
                     continue;
#endif
                  case 'd':
                  case 'i':   /* signed int decimal conversion */
                     if( length == _L_CHAR_ )
                        argval.value.as_x_intmax_t = ( unsigned char ) va_arg_n( args, _x_int, param );
                     else if( length == _L_SHORT_ )
                        argval.value.as_x_intmax_t = ( unsigned short ) va_arg_n( args, _x_int, param );
                     else if( length == _L_LONG_ )
                        argval.value.as_x_intmax_t = va_arg_n( args, _x_long, param );
                     else if( length == _L_LONGLONG_ )
                        argval.value.as_x_intmax_t = va_arg_n( args, _x_longlong, param );
                     else if( length == _L_INTMAX_ )
                        argval.value.as_x_intmax_t = va_arg_n( args, _x_intmax_t, param );
                     else if( length == _L_SIZE_ )
                        argval.value.as_x_intmax_t = va_arg_n( args, _x_size_t, param );
                     else if( length == _L_PTRDIFF_ )
                        argval.value.as_x_intmax_t = va_arg_n( args, _x_ptrdiff_t, param );
                     else
                        argval.value.as_x_intmax_t = va_arg_n( args, _x_int, param );
                     value = argval.value.as_x_intmax_t < 0;
                     argval.value.as_x_uintmax_t = value ? -argval.value.as_x_intmax_t :
                                                            argval.value.as_x_intmax_t;
                     size = put_dec( buffer, bufsize, size, argval.value.as_x_uintmax_t,
                                     flags, width, precision, value );
                     continue;
                  case 'o':   /* unsigned int octal conversion */
                  case 'u':   /* unsigned int decimal conversion */
                  case 'x':   /* unsigned int hexadecimal conversion */
                  case 'X':   /* unsigned int hexadecimal conversion */
                     if( length == _L_CHAR_ )
                        argval.value.as_x_uintmax_t = ( unsigned char ) va_arg_n( args, _x_int, param );
                     else if( length == _L_SHORT_ )
                        argval.value.as_x_uintmax_t = ( unsigned short ) va_arg_n( args, _x_int, param );
                     else if( length == _L_LONG_ )
                        argval.value.as_x_uintmax_t = va_arg_n( args, _x_ulong, param );
                     else if( length == _L_LONGLONG_ )
                        argval.value.as_x_uintmax_t = va_arg_n( args, _x_ulonglong, param );
                     else if( length == _L_INTMAX_ )
                        argval.value.as_x_uintmax_t = va_arg_n( args, _x_uintmax_t, param );
                     else if( length == _L_SIZE_ )
                        argval.value.as_x_uintmax_t = va_arg_n( args, _x_size_t, param );
                     else if( length == _L_PTRDIFF_ )
                        argval.value.as_x_uintmax_t = va_arg_n( args, _x_ptrdiff_t, param );
                     else
                        argval.value.as_x_uintmax_t = va_arg_n( args, _x_uint, param );

                     if( c == 'o' )
                        size = put_octal( buffer, bufsize, size, argval.value.as_x_uintmax_t,
                                          flags, width, precision );
                     else if( c == 'u' )
                        size = put_dec( buffer, bufsize, size, argval.value.as_x_uintmax_t,
                                        flags & ~( _F_SPACE | _F_SIGN ),
                                        width, precision, 0 );
                     else
                        size = put_hex( buffer, bufsize, size, argval.value.as_x_uintmax_t,
                                        flags, width, precision, c == 'X' );
                     continue;
                  case 'p':   /* void * pointer */
                     argval.value.as_x_ptr = va_arg_n( args, _x_ptr, param );
                     if( argval.value.as_x_ptr )
                        size = put_hex( buffer, bufsize, size, ( HB_PTRUINT ) argval.value.as_x_ptr,
                                        flags | _F_ALTERNATE, width, precision, 0 );
                     else
                        size = put_str( buffer, bufsize, size, "(nil)",
                                        flags, width, -1 );
                     continue;
                  case 'c':   /* signed int casted to unsigned char */
                     if( ( flags & _F_LEFTADJUSTED ) == 0 ) while( --width > 0 )
                     {
                        if( size < bufsize )
                           buffer[ size ] = ' ';
                        ++size;
                     }
                     c = ( unsigned char ) va_arg_n( args, _x_int, param );
                     if( size < bufsize )
                        buffer[ size ] = c;
                     ++size;
                     while( --width > 0 )
                     {
                        if( size < bufsize )
                           buffer[ size ] = ' ';
                        ++size;
                     }
                     continue;
                  case 's':   /* const char * */
                     if( length == _L_LONG_ )
                     {
                        argval.value.as_x_wstr = va_arg_n( args, _x_wstr, param );
                        size = put_wstr( buffer, bufsize, size, argval.value.as_x_wstr,
                                         flags, width, precision );
                     }
                     else
                     {
                        argval.value.as_x_str = va_arg_n( args, _x_str, param );
                        size = put_str( buffer, bufsize, size, argval.value.as_x_str,
                                        flags, width, precision );
                     }
                     continue;
                  case 'n':   /* store current result size in int * arg */
                     /* This is very danger feature in *printf() functions
                      * family very often used by hackers to create buffer
                      * overflows. It can also cause unintentional memory
                      * corruption by programmers typo in pattern so if it's
                      * not strictly necessary it's good to disable it.
                      */
                     *( va_arg_n( args, _x_intptr, param ) ) = ( int ) size;
                     continue;
                  case '%':   /* store % consuming arguments % */
                     break;
                  default:    /* error, wrong format, store pattern */
                     format = pattern;
                     c = '%';
                     break;
               }
            }
         }

         /* ISO C99 defines that when size is 0 and buffer is NULL we should
          * return number of characters that would have been written in case
          * the output string has been large enough without trailing 0.
          * Many implementations always returns number of characters necessary
          * to hold the string even if the above condition is not true so the
          * returned value can be used to check if buffer was large enough
          * and if not to allocate bigger buffer. Let's do the same.
          */
         if( size < bufsize )
            buffer[ size ] = c;
         ++size;
      }
      while( c != 0 );

      va_end( args );

#ifndef __NO_ARGPOS__
   }
   while( params.repeat );

   if( params.arglst != argbuf )
      hb_xfree( params.arglst );
#endif

   /* always set trailing \0 !!! */
   if( bufsize )
      buffer[ bufsize - 1 ] = 0;

   return ( int ) ( size - 1 );
}

#else /* defined( HB_USE_CRTL_SNPRINTF ) */

#undef _HB_SNPRINTF_ADD_EOS

/* NOTE: The full size of the buffer is expected as nSize. [vszakats] */
int hb_vsnprintf( char * buffer, size_t nSize, const char * format, va_list arglist )
{
   int result;

#if defined( __DJGPP__ ) && ( __DJGPP__ < 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ <= 3 ) )
   /* Use vsprintf() for DJGPP <= 2.03.
      This is a temporary hack, should implement a C99 snprintf() ourselves. */
   result = vsprintf( buffer, format, arglist );
   #define _HB_SNPRINTF_ADD_EOS
#elif defined( _MSC_VER ) && _MSC_VER >= 1400
   result = _vsnprintf_s( buffer, nSize, _TRUNCATE, format, arglist );
#elif ( defined( _MSC_VER ) || defined( __DMC__ ) ) && !defined( __XCC__ )
   result = _vsnprintf( buffer, nSize, format, arglist );
   #define _HB_SNPRINTF_ADD_EOS
#elif defined( __WATCOMC__ ) && __WATCOMC__ < 1200
   result = _vbprintf( buffer, nSize, format, arglist );
#else
   result = vsnprintf( buffer, nSize, format, arglist );
#endif

#ifdef _HB_SNPRINTF_ADD_EOS
   if( buffer && nSize )
      buffer[ nSize - 1 ] = '\0';
#endif

   return result;
}

#endif /* HB_USE_CRTL_SNPRINTF */

int hb_snprintf( char * buffer, size_t bufsize, const char * format, ... )
{
   va_list ap;
   int iResult;

   va_start( ap, format );
   iResult = hb_vsnprintf( buffer, bufsize, format, ap );
   va_end( ap );

   return iResult;
}
