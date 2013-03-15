/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on ncurses screen library.
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
 * www - http://harbour-project.org
 * Special thanks to Marek Paliwoda <paliwoda@inetia.pl>
 * author of gtsln from which I borrowed a lot of code and ideas.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.   If not, write to
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

#include "hbgtcore.h"
#include "hbapifs.h"

#if defined( HB_OS_UNIX ) || defined( HB_OS_DOS )

#define MAX_CHAR_VAL  0xff
#define HB_CHRMAP( a, c )  ( ( ( a ) << 16 ) | ( c ) )

const char * hb_gt_szCharMapFileDefault = "/etc/harbour/hb-charmap.def";

static void chrmap_init( int * piTransTbl )
{
   int i;

   for( i = 0; i < 256; ++i )
      piTransTbl[ i ] = HB_CHRMAP( i < 128 ? 1 : 0, i );

   piTransTbl[ 155 ] = HB_CHRMAP( 1, '.' );
}

static void chrmap_dotctrl( int * piTransTbl )
{
   int i;

   for( i = 0; i < 32; ++i )
      piTransTbl[ i ] = piTransTbl[ i + 128 ] = HB_CHRMAP( 1, '.' );
}

static void chrmap_ascictrl( int * piTransTbl )
{
   piTransTbl[ 4 ]  = HB_CHRMAP( 1, '#' );
   piTransTbl[ 16 ] = HB_CHRMAP( 1, '>' );
   piTransTbl[ 17 ] = HB_CHRMAP( 1, '<' );
   piTransTbl[ 30 ] = HB_CHRMAP( 1, '^' );
   piTransTbl[ 31 ] = HB_CHRMAP( 1, 'v' );
   piTransTbl[ 24 ] = HB_CHRMAP( 1, '^' );
   piTransTbl[ 25 ] = HB_CHRMAP( 1, 'v' );
   piTransTbl[ 26 ] = HB_CHRMAP( 1, '>' );
   piTransTbl[ 27 ] = HB_CHRMAP( 1, '<' );
}

static void chrmap_acscbox( int * piTransTbl )
{
   piTransTbl[   4 ] = HB_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[  16 ] = HB_CHRMAP( 5, '+' ); /* ACS_RARROW */
   piTransTbl[  17 ] = HB_CHRMAP( 5, ',' ); /* ACS_LARROW */
   piTransTbl[  24 ] = HB_CHRMAP( 5, '-' ); /* ACS_UARROW */
   piTransTbl[  25 ] = HB_CHRMAP( 5, '.' ); /* ACS_DARROW */
   piTransTbl[  26 ] = HB_CHRMAP( 5, '+' ); /* ACS_RARROW */
   piTransTbl[  27 ] = HB_CHRMAP( 5, ',' ); /* ACS_LARROW */
   piTransTbl[  30 ] = HB_CHRMAP( 5, '-' ); /* ACS_UARROW */
   piTransTbl[  31 ] = HB_CHRMAP( 5, '.' ); /* ACS_DARROW */

   piTransTbl[ 176 ] = HB_CHRMAP( 5, 'h' ); /* ACS_BOARD */
   piTransTbl[ 177 ] = HB_CHRMAP( 5, 'a' ); /* ACS_CKBOARD */
   piTransTbl[ 178 ] = HB_CHRMAP( 5, '0' ); /* ACS_BLOCK */
   piTransTbl[ 179 ] = HB_CHRMAP( 5, 'x' ); /* ACS_VLINE */
   piTransTbl[ 180 ] = HB_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 181 ] = HB_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 182 ] = HB_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 183 ] = HB_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 184 ] = HB_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 185 ] = HB_CHRMAP( 5, 'u' ); /* ACS_RTEE */
   piTransTbl[ 186 ] = HB_CHRMAP( 5, 'x' ); /* ACS_VLINE */
   piTransTbl[ 187 ] = HB_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 188 ] = HB_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 189 ] = HB_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 190 ] = HB_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 191 ] = HB_CHRMAP( 5, 'k' ); /* ACS_URCORNER */
   piTransTbl[ 192 ] = HB_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 193 ] = HB_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 194 ] = HB_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 195 ] = HB_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 196 ] = HB_CHRMAP( 5, 'q' ); /* ACS_HLINE */
   piTransTbl[ 197 ] = HB_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 198 ] = HB_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 199 ] = HB_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 200 ] = HB_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 201 ] = HB_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */
   piTransTbl[ 202 ] = HB_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 203 ] = HB_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 204 ] = HB_CHRMAP( 5, 't' ); /* ACS_LTEE */
   piTransTbl[ 205 ] = HB_CHRMAP( 5, 'q' ); /* ACS_HLINE */
   piTransTbl[ 206 ] = HB_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 207 ] = HB_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 208 ] = HB_CHRMAP( 5, 'v' ); /* ACS_BTEE */
   piTransTbl[ 209 ] = HB_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 210 ] = HB_CHRMAP( 5, 'w' ); /* ACS_TTEE */
   piTransTbl[ 211 ] = HB_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 212 ] = HB_CHRMAP( 5, 'm' ); /* ACS_LLCORNER */
   piTransTbl[ 213 ] = HB_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */
   piTransTbl[ 214 ] = HB_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */
   piTransTbl[ 215 ] = HB_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 216 ] = HB_CHRMAP( 5, 'n' ); /* ACS_PLUS */
   piTransTbl[ 217 ] = HB_CHRMAP( 5, 'j' ); /* ACS_LRCORNER */
   piTransTbl[ 218 ] = HB_CHRMAP( 5, 'l' ); /* ACS_ULCORNER */

#if 0
   piTransTbl[ 219 ] = HB_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 220 ] = HB_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 221 ] = HB_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 222 ] = HB_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
   piTransTbl[ 223 ] = HB_CHRMAP( 5, '`' ); /* ACS_DIAMOND */
#endif
}

static void skip_blank( char ** buf )
{
   while( **buf != '\0' && **buf == ' ' )
      ++( *buf );
}

static int get_val( char ** buf )
{
   int n = -1;
   char c;

   if( ( *buf )[ 0 ] == '\'' && ( *buf )[ 1 ] != '\0' && ( *buf )[ 2 ] == '\'' )
   {
      n = ( *buf )[ 1 ] & 0xff;
      *buf += 3;
   }
   else if( ( *buf )[ 0 ] == '0' && ( ( *buf )[ 1 ] == 'x' || ( *buf )[ 1 ] == 'X' ) )
   {
      n = 0;
      *buf += 2;
      for(; ( **buf >= '0' && **buf <= '9' ) ||
            ( **buf >= 'A' && **buf <= 'F' ) ||
            ( **buf >= 'a' && **buf <= 'f' ); ( *buf )++ )
      {
         c = **buf | 0x20;
         n = ( n << 4 ) + c - ( c > '9' ? ( 'a' - 10 ) : '0' );
      }
   }
   else if( **buf >= '0' && **buf <= '9' )
   {
      n = 0;
      for(; ( **buf >= '0' && **buf <= '9' ); ( *buf )++ )
         n = n * 10 + ( **buf - '0' );
   }
   return n > 0xff ? -1 : n;
}

static int parse_line( char * buf, int * from, int * to, char * op, int * val, int * mod )
{
   char *s, *s2;
   int ret = 0, ina = 0;

   s = buf;
   while( *s != '\0' )
   {
      switch( *s )
      {
         case '\t':
            *s = ' ';
            break;
         case '\'':
            ina ^= 1;
            if( ina )
               ++s;
            break;
         case '\n':
         case '\r':
         case '#':
            *s = '\0';
            break;
      }
      if( *s != '\0' )
         ++s;
   }

   s = buf;
   skip_blank( &s );

   if( *s == '@' )
   {
      ++s;
      s2 = buf;
      while( *s != '\0' && *s != ' ' )
         *s2++ = *s++;
      *s2 = '\0';
      ret = strlen( buf ) > 0 ? 2 : -1;
   }
   else if( *s != '\0' )
   {
      ret = *from = *to = *val = *mod = -1;
      *op = '=';

      *from = get_val( &s );
      if( *from >= 0 )
      {
         if( *s == '-' )
         {
            ++s;
            *to = get_val( &s );
         }
         else
            *to = *from;
      }

      if( *to >= 0 && *s == ':' && s[ 1 ] == ' ' )
      {
         ++s;
         skip_blank( &s );
         if( *s == '*' && ( s[ 1 ] == '+' || s[ 1 ] == '-' || s[ 1 ] == '&' ||
                            s[ 1 ] == '|' || s[ 1 ] == '^' || s[ 1 ] == '=' ||
                            s[ 1 ] == ' ' ) )
         {
            *op = s[1];
            s+=2;
         }
         *val = *op == ' ' ? 0 : get_val( &s );
         if( *val >= 0 )
         {
            skip_blank( &s );
            *mod = get_val( &s );
            skip_blank( &s );
            if( *mod >= 0 && *mod <= 5 && *s == '\0' )
               ret = 1;
         }
      }
   }
   return ret;
}

static int chrmap_parse( FILE * fp, const char * pszTerm, int * nTransTbl, const char * pszFile )
{
   int line = 0, from = 0, to = 0, val = 0, mod = 0, i, n;
   char buf[ 256 ], * s, op = 0;
   int isTerm = 0;
   fpos_t pos;

   fgetpos( fp, &pos );
   fseek( fp, 0, SEEK_SET );

   while( ! feof( fp ) && isTerm < 2 )
   {
      ++line;
      if( fgets( buf, sizeof( buf ), fp ) != NULL )
      {
         n = 0;
         if( *buf == ':' )
         {
            if( isTerm == 1 )
               isTerm = 2;
            else
            {
               *buf = '|';
               s = buf;
               while( *s != '\0' && *s != ' ' && *s != '\t' &&
                      *s != '\n' && *s != '\r' )
                  ++s;
               *s = '\0';
               s = buf;
               i = strlen( pszTerm );
               while( isTerm == 0 && ( s = strstr( s + 1, pszTerm ) ) != NULL )
               {
                  if( *( s - 1 ) == '|' &&
                      ( s[ i ] == '|' || s[ i ] == '\0' ) )
                     isTerm = 1;
               }
            }
         }
         else if( isTerm == 1 )
         {
            n = parse_line( buf, &from, &to, &op, &val, &mod );
         }

         if( n == 2 )
         {
            chrmap_parse( fp, buf, nTransTbl, pszFile );
         }
         else if( n == 1 )
         {
            /* printf("line: %3d\tfrom=%d, to=%d, op='%c', val=%d, mod=%d\n", line, from, to, op, val, mod); */
            for( i = from; i <= to; ++i )
            {
               switch( op )
               {
                  case '|':
                     nTransTbl[ i ] = ( i | val );
                     break;
                  case '&':
                     nTransTbl[ i ] = ( i & val );
                     break;
                  case '^':
                     nTransTbl[ i ] = ( i ^ val );
                     break;
                  case '+':
                     nTransTbl[ i ] = ( i + val ) & 0xff;
                     break;
                  case '-':
                     nTransTbl[ i ] = ( i - val ) & 0xff;
                     break;
                  case '=':
                     nTransTbl[ i ] = val;
                     break;
                  case '*':
                  case ' ':
                  default:
                     nTransTbl[ i ] = i;
                     break;
               }
               nTransTbl[ i ] |= mod << 16;
            }
         }
         else if( n == -1 )
         {
            fprintf( stderr, "file: %s, parse error at line: %d\n", pszFile, line );
         }
      }
   }

   fsetpos( fp, &pos );

   return isTerm;
}

static int hb_gt_chrmapread( const char * pszFile, const char * pszTerm, int * nTransTbl )
{
   FILE * fp;
   char buf[ 256 ], * ptr, * pTerm;
   int isTerm = -1;

   fp = hb_fopen( pszFile, "r" );

   if( fp != NULL )
   {
      hb_strncpy( buf, pszTerm, sizeof( buf ) - 1 );
      isTerm = 0;
      pTerm = buf;
      while( pTerm )
      {
         if( ( ptr = strchr( pTerm, '/' ) ) != NULL )
            *ptr++ = '\0';

         if( *pTerm )
            if( chrmap_parse( fp, pTerm, nTransTbl, pszFile ) > 0 )
               isTerm = 1;

         pTerm = ptr;
      }
      fclose( fp );
   }
   return isTerm;
}

int hb_gt_chrmapinit( int * piTransTbl, const char * pszTerm, HB_BOOL fSetACSC )
{
   char * pszFree = NULL, * pszFile, szFile[ HB_PATH_MAX ];
   int nRet = -1;

   chrmap_init( piTransTbl );

   if( pszTerm == NULL || *pszTerm == '\0' )
      pszTerm = pszFree = hb_getenv( "HB_TERM" );
   if( pszTerm == NULL || *pszTerm == '\0' )
   {
      if( pszFree )
         hb_xfree( pszFree );
      pszTerm = pszFree = hb_getenv( "TERM" );
   }

   if( pszTerm != NULL && *pszTerm != '\0' )
   {
      pszFile = hb_getenv( "HB_CHARMAP" );
      if( pszFile != NULL && *pszFile != '\0' )
         nRet = hb_gt_chrmapread( pszFile, pszTerm, piTransTbl );
      if( nRet == -1 )
      {
         if( pszFile )
            hb_xfree( pszFile );
         pszFile = hb_getenv( "HB_ROOT" );
         if( pszFile != NULL && sizeof( szFile ) >
                        strlen( pszFile ) + strlen( hb_gt_szCharMapFileDefault ) )
         {
            hb_strncpy( szFile, pszFile, sizeof( szFile ) - 1 );
            hb_strncat( szFile, hb_gt_szCharMapFileDefault, sizeof( szFile ) - 1 );
            nRet = hb_gt_chrmapread( szFile, pszTerm, piTransTbl );
         }
      }
      if( pszFile )
         hb_xfree( pszFile );
      if( nRet == -1 )
         nRet = hb_gt_chrmapread( hb_gt_szCharMapFileDefault, pszTerm, piTransTbl );
   }

   if( pszFree )
      hb_xfree( pszFree );

   if( nRet == -1 )
   {
      chrmap_dotctrl( piTransTbl );
      if( fSetACSC )
         chrmap_acscbox( piTransTbl );
      else
         chrmap_ascictrl( piTransTbl );
   }

   return nRet;
}

#if 0
int main(int argc, char **argv)
{
   int piTransTbl[ 256 ], i;

   if( hb_gt_chrmapinit( piTransTbl, NULL ) == -1 )
   {
      printf( "cannot init charmap.\n" );
      exit( 1 );
   }

   for( i = 0; i < 256; i++ )
      printf( "%3d -> %3d : %d\n", i, piTransTbl[ i ] & 0xff, piTransTbl[ i ] >> 16 );

   return 0;
}
#endif

#endif /* HB_OS_UNIX || HB_OS_DOS */
