/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */
#include "sxapi.h"
#include <time.h>

/*
   Synopsis:
   Concatenates multiple strings into a single result.
   Eg. xstrcat (buffer, "A", "B", NULL) stores "AB" in buffer.
   Returns dest. Append the string to any existing contents of dest.
   From DDJ Nov 1992 p. 155, with adaptions.
 */
char * _sx_randomname( const char * szPrefix )
{
   HB_ISIZ     iLen    = szPrefix ? strlen( szPrefix ) : 0;
   char *      szRet   = ( char * ) hb_xgrab( iLen + 8 ); /* _011200 */
   SYSTEMTIME  t;

   GetLocalTime( &t );

   if( iLen == 0 )
      szPrefix = "_";

   hb_snprintf( szRet, iLen + 8, "%s%02d%02d%02d", szPrefix, t.wHour, t.wMinute,
                t.wSecond );

   return szRet;
}

char * _sx_strcat( char * dest, const char * src, ... )
{
   char *   feedback = dest;
   va_list  va;

   while( *dest ) /*  Find end of dest string          */
      dest++;
   va_start( va, src );
   while( src )
   {
      while( *src )
         *dest++ = *src++;
      src = va_arg( va, char * );
   }

   *dest = '\0';  /*  Append a null character          */
   va_end( va );
   return feedback;
}

char * _sx_insertchar( char * strbuf, char chrtoins, HB_ISIZ pos )
{
   memmove( ( strbuf + pos ) + 1, ( strbuf + pos ), strlen( ( strbuf + pos ) ) + 1 );
   *( strbuf + pos ) = chrtoins;
   return strbuf;
}

char * _sx_alltrim( char * string )
{
   return _sx_ltrim( _sx_rtrim( string ) );
}

char * _sx_padl( char * strbuf, char chrtofill, HB_SIZE len )
{
   while( strlen( strbuf ) < len )
   {
      _sx_insertchar( strbuf, chrtofill, 0 );
   }

   return strbuf;
}

char * _sx_padr( char * strbuf, char chrtofill, HB_SIZE len )
{
   while( strlen( strbuf ) < len )
   {
      _sx_insertchar( strbuf, chrtofill, strlen( strbuf ) );
   }

   return strbuf;
}

char * _sx_rtrim( char * string )
{
   char * last;

   if( string )
   {
      last = string + strlen( string );
      while( last > string )
      {
         if( ! isspace( *( last - 1 ) ) )
            break;
         last--;
      }

      *last = 0;
   }

   return string;
}

char * _sx_ltrim( char * string )
{
   while( isspace( *string ) )
      hb_xstrcpy( string, string + 1 );

   return string;
}

char * _sx_upper( char * pszText )
{
   char * pszPos;

   for( pszPos = pszText; *pszPos; pszPos++ )
      *pszPos = ( char ) toupper( ( UCHAR ) *pszPos );

   return pszText;

#if 0
   char * scan;
   if( string )
   {
      scan = string;
      while( *scan )
      {
         *scan = ( char ) toupper( *scan );
         scan++;
      }
   }

   return string;
#endif
}
