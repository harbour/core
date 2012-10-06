/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * IS*() string functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include <ctype.h>

#include "hbapi.h"

#ifdef __dj_include_inline_ctype_ha_

#undef isalnum
#undef isalpha
#undef iscntrl
#undef isdigit
#undef isgraph
#undef islower
#undef isprint
#undef ispunct
#undef isspace
#undef isupper
#undef isxdigit

#undef tolower
#undef toupper

#define isalnum(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISALNUM)
#define isalpha(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISALPHA)
#define iscntrl(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISCNTRL)
#define isdigit(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISDIGIT)
#define isgraph(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISGRAPH)
#define islower(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISLOWER)
#define isprint(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISPRINT)
#define ispunct(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISPUNCT)
#define isspace(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISSPACE)
#define isupper(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISUPPER)
#define isxdigit(c) (__dj_ctype_flags[(unsigned char)(c)+1] & __dj_ISXDIGIT)

#define tolower(c) (__dj_ctype_tolower[(unsigned char)(c)+1])
#define toupper(c) (__dj_ctype_toupper[(unsigned char)(c)+1])

#endif /* __dj_include_inline_ctype_ha_ */

#if !defined( isascii )
   #define isascii( c )   ( ( unsigned ) ( c ) <= 0x7F )
#endif

/* determines if first char of a string is an alphanumeric character */

HB_FUNC( ISALNUM )
{
   const char * szString = hb_parc( 1 );

   if( szString != NULL )
      hb_retl( HB_ISALNUM( ( HB_BYTE ) * szString ) );
   else
      hb_retl( HB_FALSE );
}

/* determines if first char of a string is a white-space character;
   that is, a horizontal tab, a new-line, a vertical tab, a form-feed,
   a carriage-return or a space.
*/

HB_FUNC( ISSPACE )
{
   const char * szString = hb_parc( 1 );

   if( szString != NULL )
      hb_retl( HB_ISSPACE( ( HB_BYTE ) * szString ) );
   else
      hb_retl( HB_FALSE );
}

/* determines if first char of a string is a hexadecimal digit
  ('A' - 'F', 'a' - 'f', or '0' -'9').
*/

HB_FUNC( ISXDIGIT )
{
   const char * szString = hb_parc( 1 );

   hb_retl( szString && HB_ISXDIGIT( ( HB_BYTE ) * szString ) );
}

/* determines if first char of a string is a control character;
   that is, if it is in the range 0 - 31 or 127 (0x00 - 0x1f or 0x7f).
*/

HB_FUNC( ISCNTRL )
{
   const char * szString = hb_parc( 1 );

   hb_retl( szString && iscntrl( ( HB_BYTE ) * szString ) );
}

/* determines if first char of a string is a printable character.
   The space character (' ') is not considered a printable character.
*/

HB_FUNC( ISGRAPH )
{
   const char * szString = hb_parc( 1 );

   hb_retl( szString && isgraph( ( HB_BYTE ) * szString ) );
}

/* determines if first char of a string is a printable character.
   Printable characters have an ASCII value between 32 - 126, (0x20 - 0x7e),
   a space and the tilde, inclusive.
*/

HB_FUNC( ISPRINT )
{
   const char * szString = hb_parc( 1 );

   hb_retl( szString && isprint( ( HB_BYTE ) * szString ) );
}

/* determines if first char of a string is a punctuation character.
   A punctuation character is one that is not alphabetic, not numeric,
   not a control character, and not a white space.
*/

HB_FUNC( ISPUNCT )
{
   const char * szString = hb_parc( 1 );

   hb_retl( szString && ispunct( ( HB_BYTE ) * szString ) );
}

/* determines if first char of a string is a member of the 7-bit ASCII
   character set; that is, if: 0  <=  c  <= 127
*/

HB_FUNC( ISASCII )
{
   const char * szString = hb_parc( 1 );

   hb_retl( szString && isascii( ( HB_BYTE ) * szString ) );
}
