/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Console API
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    HB___ACCEPT()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    adjust_pos(), hb_altout(), hb_devout(), HB_DEVOUT(), hb_devpos(),
 *    HB_DEVPOS(), hb_dispout(), HB___EJECT(), hb_max_col(), HB_MAXCOL(),
 *    hb_max_row(), HB_MAXROW(), hb_out(), hb_outerr(), HB_OUTERR(),
 *    hb_outstd(), HB_OUTSTD(), HB_PCOL(), HB_PROW(), hb_setpos(),
 *    HB_SETPOS(), HB_SETPRC(), HB_SCROLL(), and hb_consoleInitialize()
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB_SETPOSBS()
 *    HB_DISPBOX() (GT version)
 *    HB_DISPBEGIN()
 *    HB_DISPEND()
 *    HB_DISPCOUNT()
 *    HB_ISCOLOR()
 *    HB_NOSNOW()
 *    HB___ACCEPTSTR()
 *    HB___COLORINDEX()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "filesys.h"
#include "dates.h"
#include "set.h"
#include "inkey.h"
#include "gtapi.h"            /* HARBOUR_USE_GTAPI is checked inside gtapi.h, so that
                                 we can always get the border styles */

#if defined(__GNUC__) && ! defined(__MINGW32__)
   #include <unistd.h>
   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(HARBOUR_GCC_OS2)
      #include <io.h>
   #endif
#else
   #include <io.h>
#endif

#if defined(OS_UNIX_COMPATIBLE) || defined(__CYGWIN__)
   #include <unistd.h>
   #include <termios.h>
#endif

#define ACCEPT_BUFFER_LEN 256 /* length of input buffer for ACCEPT command */

#if defined(OS_UNIX_COMPATIBLE)
   #define CRLF_BUFFER_LEN 2     /*length of buffer for CR/LF characters */
#else
   #define CRLF_BUFFER_LEN 3     /*length of buffer for CR/LF characters */
#endif

static USHORT s_uiDevRow;
static USHORT s_uiDevCol;
static USHORT s_uiPRow;
static USHORT s_uiPCol;
static char   s_szCrLf[ CRLF_BUFFER_LEN ];
static char   s_szAcceptResult[ ACCEPT_BUFFER_LEN ];

void hb_consoleInitialize( void )
{
#if defined(OS_DOS_COMPATIBLE)
   s_szCrLf[ 0 ] = '\r';
   s_szCrLf[ 1 ] = '\n';
   s_szCrLf[ 2 ] = '\0';
#else
   s_szCrLf[ 0 ] = '\n';
   s_szCrLf[ 1 ] = '\0';
#endif

   s_uiPRow = s_uiPCol = 0;

   /* Some compilers open stdout and stderr in text mode, but
      Harbour needs them to be open in binary mode. */

   hb_fsSetDevMode( fileno( stdout ), FM_BINARY );
   hb_fsSetDevMode( fileno( stderr ), FM_BINARY );

#ifdef HARBOUR_USE_GTAPI
   hb_gtInit();
   hb_gtGetPos( &s_uiDevRow, &s_uiDevCol );
#else
   s_uiDevRow = 0;
   s_uiDevCol = 0;
#endif
}

void hb_consoleRelease( void )
{
   hb_fsSetDevMode( fileno( stdout ), FM_TEXT );
   hb_fsSetDevMode( fileno( stderr ), FM_TEXT );

#ifdef HARBOUR_USE_GTAPI
   hb_gtExit();
#endif
}

char * hb_consoleGetNewLine( void )
{
   return s_szCrLf;
}

USHORT hb_max_row( void )
{
#ifdef HARBOUR_USE_GTAPI
   return hb_gtMaxRow();
#else
   return 23; /* QUESTION: Shouldn't this be 24 ? info@szelvesz.hu */
#endif        /* ANSWER  : No. ANSI terminals commonly only have 24 lines */
}

USHORT hb_max_col( void )
{
#ifdef HARBOUR_USE_GTAPI
   return hb_gtMaxCol();
#else
   return 79;
#endif
}

#ifndef HARBOUR_USE_GTAPI
static void adjust_pos( BYTE * pStr, ULONG len, USHORT * row, USHORT * col, USHORT max_row, USHORT max_col )
{
   ULONG count;
   BYTE * pPtr = pStr;

   for( count = 0; count < len; count++ )
   {
      switch( *pPtr++  )
      {
         case 7:
            break;
         case 8:
            if( *col ) ( *col )--;
            else
            {
               *col = max_col;
               if( *row ) ( *row )--;
            }
            break;
         case 10:
            if( *row < max_row ) ( *row )++;
            break;
         case 13:
            *col = 0;
            break;
         default:
            if( *col < max_col ) ( *col )++;
            else
            {
               *col = 0;
               if( *row < max_row ) ( *row )++;
            }
      }
   }
}
#endif

typedef void hb_out_func_typedef( BYTE *, ULONG );

/* Format items for output, then call specified output function */
static void hb_out( USHORT uiParam, hb_out_func_typedef * hb_out_func )
{
   PHB_ITEM pItem = hb_param( uiParam, IT_ANY );

   switch( pItem->type )
   {
      case IT_STRING:
         hb_out_func( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ) );
         break;

      case IT_DATE:
      {
         char szBuffer[ 11 ];
         hb_dtoc( hb_pards( uiParam ), szBuffer, hb_set.HB_SET_DATEFORMAT );
         hb_out_func( ( BYTE * ) szBuffer, strlen( szBuffer ) );
         break;
      }

      case IT_DOUBLE:
      case IT_INTEGER:
      case IT_LONG:
      {
         char * szText = hb_itemStr( pItem, NULL, NULL ); /* Let hb_itemStr() do the hard work */
         if( szText )
         {
            hb_out_func( ( BYTE * ) szText, strlen( szText ) );
            hb_xfree( szText );
         }
         break;
      }
      case IT_NIL:
         hb_out_func( "NIL", 3 );
         break;

      case IT_LOGICAL:
         if( hb_itemGetL( pItem ) )
            hb_out_func( ".T.", 3 );
         else
            hb_out_func( ".F.", 3 );
         break;

      default:
         break;
   }
}

/* Output an item to STDOUT */
static void hb_outstd( BYTE * pStr, ULONG len )
{
   ULONG count = len;
   BYTE * pPtr = pStr;

#ifdef HARBOUR_USE_GTAPI
   hb_gtPreExt();
#endif

   if( strlen( pStr ) != count )
      while( count-- ) printf( "%c", *pPtr++ );
   else
      printf( "%s", pStr );
   fflush( stdout );
#ifdef HARBOUR_USE_GTAPI
   #ifndef __CYGWIN__
   if( isatty( fileno( stdout ) ) )
   #endif
   {
      s_uiDevRow = hb_gt_Row();
      s_uiDevCol = hb_gt_Col();
      hb_gtSetPos( s_uiDevRow, s_uiDevCol );
   }
   hb_gtPostExt();
#else
   adjust_pos( pStr, len, &s_uiDevRow, &s_uiDevCol, hb_max_row(), hb_max_col() );
#endif
}

/* Output an item to STDERR */
static void hb_outerr( BYTE * pStr, ULONG len )
{
   ULONG count = len;
   BYTE * pPtr = pStr;

#ifdef HARBOUR_USE_GTAPI
   hb_gtPreExt();
#endif

   if( strlen( pStr ) != count )
      while( count-- ) fprintf( stderr, "%c", *pPtr++ );
   else
      fprintf( stderr, "%s", pStr );
   fflush( stderr );
#ifdef HARBOUR_USE_GTAPI
   #ifndef __CYGWIN__
   if( isatty( fileno( stderr ) ) )
   #endif
   {
      s_uiDevRow = hb_gt_Row();
      s_uiDevCol = hb_gt_Col();
      hb_gtSetPos( s_uiDevRow, s_uiDevCol );
   }
   hb_gtPostExt();
#else
   adjust_pos( pStr, len, &s_uiDevRow, &s_uiDevCol, hb_max_row(), hb_max_col() );
#endif
}

/* Output an item to the screen and/or printer and/or alternate */
static void hb_altout( BYTE * pStr, ULONG len )
{
   BYTE * pPtr = pStr;

   if( hb_set.HB_SET_CONSOLE )
   {
#ifdef HARBOUR_USE_GTAPI
      hb_gtWriteCon( pStr, len );
      hb_gtGetPos( &s_uiDevRow, &s_uiDevCol );
#else
      ULONG count = len;
      if( strlen( pStr ) != count )
         while( count-- ) printf( "%c", *pPtr++ );
      else
         printf( "%s", pStr );
      adjust_pos( pStr, len, &s_uiDevRow, &s_uiDevCol, hb_max_row(), hb_max_col() );
#endif
   }
   if( hb_set.HB_SET_ALTERNATE && hb_set_althan >= 0 )
   {
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      int user_ferror = hb_fsError(); /* Save current user file error code */
      unsigned write_len;
      ULONG count = len;
      pPtr = pStr;
      while( count )
      {
         if( count > UINT_MAX )
         {
            write_len = UINT_MAX;
            count -= UINT_MAX;
         }
         else
         {
            write_len = count;
            count = 0;
         }
         hb_fsWrite( hb_set_althan, ( BYTE * ) pPtr, write_len );
         pPtr += write_len;
      }
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
   if( hb_set_extrahan >= 0 )
   {
      /* Print to extra file if valid alternate file */
      int user_ferror = hb_fsError(); /* Save current user file error code */
      unsigned write_len;
      ULONG count = len;
      pPtr = pStr;
      while( count )
      {
         if( count > UINT_MAX )
         {
            write_len = UINT_MAX;
            count -= UINT_MAX;
         }
         else
         {
            write_len = count;
            count = 0;
         }
         hb_fsWrite( hb_set_extrahan, ( BYTE * ) pPtr, write_len );
         pPtr += write_len;
      }
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
   if( hb_set.HB_SET_PRINTER && hb_set_printhan >= 0 )
   {
      /* Print to printer if SET PRINTER ON and valid printer file */
      int user_ferror = hb_fsError(); /* Save current user file error code */
      unsigned write_len;
      ULONG count = len;
      pPtr = pStr;
      while( count )
      {
         if( count > UINT_MAX )
         {
            write_len = UINT_MAX;
            count -= UINT_MAX;
         }
         else
         {
            write_len = count;
            count = 0;
         }
         hb_fsWrite( hb_set_printhan, ( BYTE * ) pPtr, write_len );
         pPtr += write_len;
      }
      if( len + s_uiPCol > USHRT_MAX ) s_uiPCol = USHRT_MAX;
      else s_uiPCol += len;
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
}

/* Output an item to the screen and/or printer */
static void hb_devout( BYTE * pStr, ULONG len )
{
   if( hb_set_printhan >= 0 && hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      int user_ferror = hb_fsError(); /* Save current user file error code */
      unsigned write_len;
      ULONG count = len;
      BYTE * pPtr = pStr;
      while( count )
      {
         if( count > UINT_MAX )
         {
            write_len = UINT_MAX;
            count -= UINT_MAX;
         }
         else
         {
            write_len = count;
            count = 0;
         }
         hb_fsWrite( hb_set_printhan, ( BYTE * ) pPtr, write_len );
         pPtr += write_len;
      }
      if( len + s_uiPCol > USHRT_MAX ) s_uiPCol = USHRT_MAX;
      else s_uiPCol += len;
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
   else
   {
#ifdef HARBOUR_USE_GTAPI
      /* Otherwise, display to console */
      hb_gtWrite( pStr, len );
      hb_gtGetPos( &s_uiDevRow, &s_uiDevCol );
#else
      ULONG count = len;
      BYTE * pPtr = pStr;
      if( strlen( pStr ) != count )
         while( count-- ) printf( "%c", *pPtr++ );
      else
         printf( "%s", pStr );
      adjust_pos( pStr, len, &s_uiDevRow, &s_uiDevCol, hb_max_row(), hb_max_col() );
#endif
   }
}

/* Output an item to the screen */
static void hb_dispout( BYTE * pStr, ULONG len )
{
#ifdef HARBOUR_USE_GTAPI
   /* Display to console */
   hb_gtWrite( pStr, len );
   hb_gtGetPos( &s_uiDevRow, &s_uiDevCol );
#else
   ULONG count = len;
   BYTE * pPtr = pStr;
   if( strlen( pStr ) != count )
      while( count-- ) printf( "%c", *pPtr++ );
   else
      printf( "%s", pStr );
   adjust_pos( pStr, len, &s_uiDevRow, &s_uiDevCol, hb_max_row(), hb_max_col() );
#endif
}

void hb_setpos( USHORT row, USHORT col )
{
#ifdef HARBOUR_USE_GTAPI
      hb_gtSetPos( row, col );
#else
      USHORT count;

      if( row < s_uiDevRow || col < s_uiDevCol )
      {
         printf( s_szCrLf );
         s_uiDevCol = 0;
         s_uiDevRow++;
      }
      else if( row > s_uiDevRow ) s_uiDevCol = 0;
      for( count = s_uiDevRow; count < row; count++ )
         printf( s_szCrLf );
      for( count = s_uiDevCol; count < col; count++ )
         printf( " " );
#endif

   s_uiDevRow = row;
   s_uiDevCol = col;
}

void hb_devpos( USHORT row, USHORT col )
{
   USHORT count;
   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */
   if( hb_set_printhan >= 0 && hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      int user_ferror = hb_fsError(); /* Save current user file error code */
      if( row < s_uiPRow )
      {
         hb_fsWrite( hb_set_printhan, ( BYTE * ) "\x0C", 1 );
         s_uiPRow = s_uiPCol = 0;
      }

      for( count = s_uiPRow; count < row; count++ )
         hb_fsWrite( hb_set_printhan, ( BYTE * ) s_szCrLf, CRLF_BUFFER_LEN-1 );

      if( row > s_uiPRow ) s_uiPCol = 0;
      col += hb_set.HB_SET_MARGIN;

      for( count = s_uiPCol; count < col; count++ )
         hb_fsWrite( hb_set_printhan, ( BYTE * ) " ", 1 );

      s_uiPRow = row;
      s_uiPCol = col;
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
   else
   {
      hb_setpos( row, col );
   }
}

HARBOUR HB_OUTSTD( void ) /* writes a list of values to the standard output device */
{
   USHORT uiParam, uiPCount = hb_pcount();

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_out( uiParam, hb_outstd );
      if( uiParam < uiPCount )
         hb_outstd( " ", 1 );
   }
}

HARBOUR HB_OUTERR( void ) /* writes a list of values to the standard error device */
{
   USHORT uiParam, uiPCount = hb_pcount();

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_out( uiParam, hb_outerr );
      if( uiParam < uiPCount )
         hb_outerr( " ", 1 );
   }
}

HARBOUR HB_QQOUT( void ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   USHORT uiParam, uiPCount = hb_pcount();

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_out( uiParam, hb_altout );
      if( uiParam < uiPCount )
         hb_altout( " ", 1 );
   }
}

HARBOUR HB_QOUT( void )
{
   USHORT count;

   hb_altout( s_szCrLf, CRLF_BUFFER_LEN - 1 );

   if( hb_set.HB_SET_PRINTER && hb_set_printhan >= 0 )
   {
      int user_ferror = hb_fsError(); /* Save current user file error code */
      s_uiPRow++;
      s_uiPCol = hb_set.HB_SET_MARGIN;
      count = s_uiPCol;
      while( count-- > 0 )
         hb_fsWrite( hb_set_printhan, ( BYTE * ) " ", 1 );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }

   HB_QQOUT();
}

HARBOUR HB_SETPOS( void ) /* Sets the screen position */
{
   if( hb_pcount() == 2 )
   {
      if( ISNUM( 1 ) && ISNUM( 2 ) )
      {
         int i_row = hb_parni( 1 );
         int i_col = hb_parni( 2 );
         USHORT row, col;

         /* Limit the new position to the range (0,0) to (MAXROW(),MAXCOL()) */
         if( i_row < 0 ) row = 0;
         else if( i_row > hb_max_row() ) row = hb_max_row();
         else row = i_row;

         if( i_col < 0 ) col = 0;
         else if( i_col > hb_max_col() ) col = hb_max_col();
         else col = i_col;

         /* Set the new screen position */
         hb_setpos( row, col );
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "SETPOS" ); /* NOTE: Clipper catches this at compile time! */
}

/* Move the screen position to the right by one column */
HARBOUR HB_SETPOSBS( void )
{
   if( hb_pcount() == 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      USHORT uiRow;
      USHORT uiCol;

      /* NOTE: Clipper does no checks about reaching the border or anything */
      hb_gtGetPos( &uiRow, &uiCol );
      hb_gtSetPos( uiRow, uiCol + 1 );
#endif
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "SETPOSBS" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_DEVPOS( void ) /* Sets the screen and/or printer position */
{
   if( hb_pcount() == 2 )
   {
      if( ISNUM( 1 ) && ISNUM( 2 ) )
      {
         long l_row = hb_parnl( 1 );
         long l_col = hb_parnl( 2 );
         USHORT row, col;

         /* Limit the new position to the range (0,0) to (65535,65535) */
         if( l_row < 0 ) row = 0;
         else if( l_row > USHRT_MAX ) row = USHRT_MAX;
         else row = l_row;

         if( l_col < 0 ) col = 0;
         else if( l_col > USHRT_MAX ) col = USHRT_MAX;
         else col = l_col;

         /* Set the new screen position */
         hb_devpos( row, col );
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "DEVPOS" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_DEVOUT( void ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( hb_pcount() > 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      char pOldColor[ CLR_STRLEN ];

      if( ISCHAR( 2 ) )
      {
         hb_gtGetColorStr( pOldColor );
         hb_gtSetColorStr( hb_parc( 2 ) );
      }
#endif

      hb_out( 1, hb_devout );

#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR( 2 ) )
         hb_gtSetColorStr( pOldColor );
#endif
   }
}

HARBOUR HB_DISPOUT( void ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( hb_pcount() > 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      char pOldColor[ CLR_STRLEN ];

      if( ISCHAR( 2 ) )
      {
         hb_gtGetColorStr( pOldColor );
         hb_gtSetColorStr( hb_parc( 2 ) );
      }
#endif

      hb_out( 1, hb_dispout );

#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR( 2 ) )
         hb_gtSetColorStr( pOldColor );
#endif
   }
}

HARBOUR HB___EJECT( void ) /* Ejects the current page from the printer */
{
   if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set_printhan >= 0 )
   {
      int user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWrite( hb_set_printhan, ( BYTE * ) "\x0C\x0D", 2 );
      s_uiPRow = s_uiPCol = 0;
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
}

HARBOUR HB_PROW( void ) /* Returns the current printer row position */
{
   if( hb_pcount() == 0 )
      hb_retni( s_uiPRow );
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "PROW" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_PCOL( void ) /* Returns the current printer row position */
{
   if( hb_pcount() == 0 )
      hb_retni( s_uiPCol );
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "PCOL" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_SETPRC( void ) /* Sets the current printer row and column positions */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      long l_row = hb_parnl( 1 );
      long l_col = hb_parnl( 2 );

      /* Limit the new position to the range (0,0) to (65535,65535) */

      if( l_row < 0 ) s_uiPRow = 0;
      else if( l_row > USHRT_MAX ) s_uiPRow = USHRT_MAX;
      else s_uiPRow = l_row;

      if( l_col < 0 ) s_uiPCol = 0;
      else if( l_col > USHRT_MAX ) s_uiPCol = USHRT_MAX;
      else s_uiPCol = l_col;
   }
}

HARBOUR HB_SCROLL( void ) /* Scrolls a screen region (requires the GT API) */
{
   USHORT top, left, bottom, right;

   int iMR      = hb_max_row();
   int iMC      = hb_max_col();

   int i_top    = ISNUM( 1 ) ? hb_parni( 1 ) : 0;
   int i_left   = ISNUM( 2 ) ? hb_parni( 2 ) : 0;
   int i_bottom = ISNUM( 3 ) ? hb_parni( 3 ) : iMR;
   int i_right  = ISNUM( 4 ) ? hb_parni( 4 ) : iMC;
   int v_scroll = ISNUM( 5 ) ? hb_parni( 5 ) : 0;
   int h_scroll = ISNUM( 6 ) ? hb_parni( 6 ) : 0;

   /* Enforce limits of (0,0) to (MAXROW(),MAXCOL()) */
   if( i_top < 0 ) top = 0;
   else if( i_top > iMR ) top = iMR;
   else top = i_top;
   if( i_left < 0 ) left = 0;
   else if( i_left > iMC ) left = iMC;
   else left = i_left;
   if( i_bottom < 0 ) bottom = 0;
   else if( i_bottom > iMR ) bottom = iMR;
   else bottom = i_bottom;
   if( i_right < 0 ) right = 0;
   else if( i_right > iMC ) right = iMC;
   else right = i_right;

#ifdef HARBOUR_USE_GTAPI
   hb_gtScroll( top, left, bottom, right, v_scroll, h_scroll );
#else
   if( top == 0 && bottom == iMR
   && left == 0 && right == iMC
   && v_scroll == 0 && h_scroll == 0 )
   {
      USHORT count;
      s_uiDevRow = iMR;
      for( count = 0; count < s_uiDevRow ; count++ ) printf( s_szCrLf );
      s_uiDevRow = s_uiDevCol = 0;
   }
#endif
}

HARBOUR HB_MAXROW( void ) /* Return the maximum screen row number (zero origin) */
{
   hb_retni( hb_max_row() );
}

HARBOUR HB_MAXCOL( void ) /* Return the maximum screen column number (zero origin) */
{
   hb_retni( hb_max_col() );
}

HARBOUR HB_ROW( void ) /* Return the current screen row position (zero origin) */
{
   if( hb_pcount() == 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      hb_gtGetPos( &s_uiDevRow, &s_uiDevCol );
#endif
      hb_retni( s_uiDevRow );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "ROW" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_COL( void ) /* Return the current screen column position (zero origin) */
{
   if( hb_pcount() == 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      hb_gtGetPos( &s_uiDevRow, &s_uiDevCol );
#endif
      hb_retni( s_uiDevCol );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "COL" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_DISPBOX( void )
{
#ifdef HARBOUR_USE_GTAPI
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) && ISNUM( 4 ) )
   {
      char szOldColor[ CLR_STRLEN ];

      if( ISCHAR( 6 ) )
      {
         hb_gtGetColorStr( szOldColor );
         hb_gtSetColorStr( hb_parc( 6 ) );
      }

      if( ISCHAR( 5 ) )
         hb_gtBox( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( BYTE * ) hb_parc( 5 ));
      else if( ISNUM( 5 ) && hb_parni( 5 ) == 2 )
         hb_gtBoxD( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
      else
         hb_gtBoxS( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );

      if( ISCHAR( 6 ) )
         hb_gtSetColorStr( szOldColor );
   }
#else
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) && ISNUM( 4 ) )
   {
      char * szBorderStyle = B_SINGLE;
      int i_top = hb_parni( 1 );
      int i_left = hb_parni( 2 );
      int i_bottom = hb_parni( 3 );
      int i_right = hb_parni( 4 );
      USHORT top, left, bottom, right, size = strlen( B_SINGLE );
      USHORT row, col, width, height;
      char Borders[ 9 ];

      /* Set limits on the box coordinates to (0,0) and (max_row(),max_col()) */
      if( i_top < 0 ) top = 0; else top = ( USHORT ) i_top;
      if( i_left < 0 ) left = 0; else left = ( USHORT ) i_left;
      if( i_bottom < 0 ) bottom  = 0; else bottom = ( USHORT ) i_bottom;
      if( i_right < 0 ) right = 0; else right = ( USHORT ) i_right;
      if( top > hb_max_row() ) top = hb_max_row();
      if( left > hb_max_col() ) left = hb_max_col();
      if( bottom > hb_max_row() ) bottom  = hb_max_row();
      if( right > hb_max_col() ) right = hb_max_col();

      /* Force the box to be drawn from top left to bottom right */
      if( top > bottom )
      {
         int temp;
         temp = top;
         top = bottom;
         bottom = temp;
      }
      if( left > right )
      {
         int temp;
         temp = right;
         right = left;
         left = temp;
      }
      width = right - left + 1;
      height = bottom - top + 1;

      /* Determine the box style */
      if( ISCHAR( 5 ) )
      {
         szBorderStyle = hb_parc( 5 );
         size = hb_parclen( 5 );
      }
      else if( ISNUM( 5 ) )
      {
         switch( hb_parni( 5 ) )
         {
            case 2:
               szBorderStyle = B_DOUBLE;
               break;
            case 3:
               szBorderStyle = B_SINGLE_DOUBLE;
               break;
            case 4:
               szBorderStyle = B_DOUBLE_SINGLE;
               break;
            default:
               szBorderStyle = B_SINGLE;
         }
          size = strlen( szBorderStyle );
      }
      /* We only need 9 characters from the source string */
      if( size > 9 ) size = 9;
      /* If we have at least one character... */
      if( size )
         /* ...copy the source string */
         memcpy( Borders, szBorderStyle, size );
      else
         /* If not, set the first character to a space */
         Borders[ size++ ] = ' ';
      /* If there were less than 8 characters in the source... */
      for( ; size < 8; size++ )
      {
         /* ...copy the last character into the remaining 8 border positions */
         Borders[ size ] = Borders[ size - 1 ];
      }
      /* If there were less than 9 characters in the source... */
      if( size < 9 )
         /* ...set the fill character to space */
         Borders[ 8 ] = ' ';

      /* Draw the box */
      hb_setpos( top, left );
      if( height > 1 && width > 1 )
         printf( "%c", Borders[ 0 ] );    /* Upper left corner */
      for( col = ( height > 1 ? left + 1 : left ); col < ( height > 1 ? right : right + 1 ); col++ )
         printf( "%c", Borders[ 1 ] );    /* Top line */
      if( height > 1 && width > 1 )
         printf( "%c", Borders[ 2 ] );    /* Upper right corner */
      for( row = ( height > 1 ? top + 1 : top ); row < ( width > 1 ? bottom : bottom + 1 ); row++ )
      {
         hb_setpos( row, left );
         if( height > 1 )
            printf( "%c", Borders[ 3 ] ); /* Left side */
         if( height > 1 && width > 1 ) for( col = left + 1; col < right; col++ )
            printf( "%c", Borders[ 8 ] ); /* Fill */
         if( height > 1 && width > 1 )
            printf( "%c", Borders[ 7 ] ); /* Right side */
      }
      if( height > 1 && width > 1 )
      {
         hb_setpos( bottom, left );
         col = left;
         printf( "%c", Borders[ 6 ] );    /* Bottom left corner */
         for( col = left + 1; col < right; col++ )
            printf( "%c", Borders[ 5 ] ); /* Bottom line */
         printf( "%c", Borders[ 4 ] );    /* Bottom right corner */
      }
      hb_setpos( bottom + 1, right + 1 );
   }
#endif
}

HARBOUR HB_DISPBEGIN( void )
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtDispBegin();
#endif
}

HARBOUR HB_DISPEND( void )
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtDispEnd();
#endif
}

HARBOUR HB_DISPCOUNT( void )
{
#ifdef HARBOUR_USE_GTAPI
   hb_retni( hb_gtDispCount() );
#else
   hb_retni( 0 );
#endif
}

HARBOUR HB_ISCOLOR( void )
{
#ifdef HARBOUR_USE_GTAPI
   hb_retl( hb_gtIsColor() );
#else
   hb_retl( FALSE );
#endif
}

HARBOUR HB_NOSNOW( void )
{
#ifdef HARBOUR_USE_GTAPI
   if( ISLOG( 1 ) )
      hb_gtSetSnowFlag( hb_parl( 1 ) );
#endif
}

HARBOUR HB___SHADOW( void )
{
#ifdef HARBOUR_USE_GTAPI
   USHORT uiAttr;

   if( hb_pcount() == 4 )
      uiAttr = 7;
   else if( hb_pcount() == 5 )
      uiAttr = hb_parni( 5 );

   if( hb_pcount() > 3 )
      hb_gt_DrawShadow( hb_parni( 1 ) + 1,
                        hb_parni( 2 ) + 1,
                        hb_parni( 3 ) + 1,
                        hb_parni( 4 ) + 1, uiAttr );
#endif
}

HARBOUR HB_DBGSHADOW( void )
{
   HB___SHADOW();
}

HARBOUR HB_SAVESCREEN( void )
{
#ifdef HARBOUR_USE_GTAPI
   USHORT uiX;
   USHORT uiCoords[ 4 ];
   void * pBuffer;

   uiCoords[ 0 ] = 0;
   uiCoords[ 1 ] = 0;
   uiCoords[ 2 ] = hb_gtMaxRow();
   uiCoords[ 3 ] = hb_gtMaxCol();

   for( uiX = 1; uiX <= 4; uiX++ )
      if( ISNUM( uiX ) )
         uiCoords[ uiX - 1 ] = hb_parni( uiX );

   hb_gtRectSize( uiCoords[ 0 ], uiCoords[ 1 ], uiCoords[ 2 ], uiCoords[ 3 ], &uiX );
   pBuffer = hb_xgrab( uiX );
   hb_gtSave( uiCoords[ 0 ], uiCoords[ 1 ], uiCoords[ 2 ], uiCoords[ 3 ], pBuffer );
   hb_retclen( pBuffer, uiX );
   hb_xfree( pBuffer );
#else
   hb_retc( "" );
#endif
}

HARBOUR HB_RESTSCREEN( void )
{
#ifdef HARBOUR_USE_GTAPI
   if( hb_pcount() == 5 )
   {
      USHORT uiX;
      USHORT uiCoords[ 4 ];

      uiCoords[ 0 ] = 0;
      uiCoords[ 1 ] = 0;
      uiCoords[ 2 ] = hb_gtMaxRow();
      uiCoords[ 3 ] = hb_gtMaxCol();

      for( uiX = 1; uiX < 5; uiX++ )
         if( ISNUM( uiX ) )
            uiCoords[ uiX - 1 ] = hb_parni( uiX );

      hb_gtRest( uiCoords[ 0 ], uiCoords[ 1 ], uiCoords[ 2 ], uiCoords[ 3 ],
                 ( void * ) hb_parc( 5 ) );
   }
#endif
}

HARBOUR HB_SETCURSOR( void )
{
#ifdef HARBOUR_USE_GTAPI
   USHORT usPreviousCursor;

   hb_gtGetCursor( &usPreviousCursor );
   if( hb_pcount() == 1 )
      hb_gtSetCursor( hb_parni( 1 ) );

   hb_retni( usPreviousCursor );
#else
   hb_retni( SC_NORMAL );
#endif
}

HARBOUR HB_SETBLINK( void )
{
#ifdef HARBOUR_USE_GTAPI
   BOOL bPreviousBlink;

   hb_gtGetBlink( &bPreviousBlink );
   if( ISLOG( 1 ) )
      hb_gtSetBlink( hb_parl( 1 ) );

   hb_retl( bPreviousBlink );
#else
   hb_retl( FALSE );
#endif
}

HARBOUR HB_SETMODE( void )
{
#ifdef HARBOUR_USE_GTAPI
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_gtSetMode( hb_parni( 1 ), hb_parni( 2 ) );
#endif
}

HARBOUR HB___ACCEPT( void ) /* Internal Clipper function used in ACCEPT command  */
                            /* Basically the simplest Clipper function to        */
                            /* receive data. Parameter : cPrompt. Returns : cRet */
{
   int input;
   ULONG len;

   if( hb_pcount() >= 1 )          /* cPrompt passed                         */
      HB_QOUT();

#if defined (OS_UNIX_COMPATIBLE) || defined(__CYGWIN__)
   /* Read the data using fgets(), because hb_inkeyPoll() doesn't support
       Unix compatible operating systems yet. */
   {
      /* Set up for echoed canonical input */
      struct termios ta;
      tcgetattr( STDIN_FILENO, &ta );
      ta.c_lflag |= ( ICANON | ECHO );
      tcsetattr( STDIN_FILENO, TCSAFLUSH, &ta );

      /* Avoid an undefined variable warning */
      input = 0;

      /* Get a line of user input */
      s_szAcceptResult[ 0 ] = '\0';          /* start with something defined */
      if( fgets( s_szAcceptResult, ACCEPT_BUFFER_LEN, stdin ) )
         strtok( s_szAcceptResult, "\n" ); /* strip off the trailing newline if it exists */

      /* Restore unechoed non-canonical input */
      ta.c_lflag &= ~( ICANON | ECHO );
      tcsetattr( STDIN_FILENO, TCSAFLUSH, &ta );

      len = strlen( s_szAcceptResult );
   }
#else
   len = 0;
   input = 0;
   while( input != 13 )
   {
      /* Wait forever, for keyboard events only */
      input = hb_inkey( 0.0, INKEY_KEYBOARD, 1, 1 );
      switch( input )
      {
         case 8:  /* Backspace */
         case 19: /* Left arrow */
            if( len > 0 )
            {
               len--; /* Adjust input count to get rid of last character,
                         then erase it from the screen. */
#ifdef HARBOUR_USE_GTAPI
               hb_gtWriteCon( "\x8 \x8", 3L );
#else
               printf( "\x8 \x8" );
#endif
            }
            break;
         case 13:  /* Nothing to do. While loop will now exit. */
            break;
         default:
            if( len < ( ACCEPT_BUFFER_LEN - 1 ) && input >= 32 )
            {
               s_szAcceptResult[ len ] = input; /* Accept the input */
               hb_dispout( &s_szAcceptResult[ len ], sizeof( char ) ); /* Then display it */
               len++;  /* Then adjust the input count */
            }
      }
   }
   s_szAcceptResult[ len ] = '\0';
#endif
   hb_retc( s_szAcceptResult );
}

HARBOUR HB__ACCEPTSTR( void )
{
   hb_retc( s_szAcceptResult );
}

/* ------------------------------------------------- */
/* Copyright (C) 1999 Victor Szel <info@szelvesz.hu> */
/* ------------------------------------------------- */

/*  $DOC$
 *  $FUNCNAME$
 *      __ColorIndex
 *  $CATEGORY$
 *      GT
 *  $ONELINER$
 *      Extract one color from a full Clipper colorspec string.
 *  $SYNTAX$
 *      __ColorIndex( <cColorSpec>, <nIndex> )
 *  $ARGUMENTS$
 *      <cColorSpec> is a Clipper color list
 *      <nIndex> is the position of the color item to be extracted, the first
 *               position is the zero.
 *  $RETURNS$
 *      The selected color string, or if anything goes wrong, and empty
 *      string
 *  $DESCRIPTION$
 *      Clipper has color spec string, which have more than one single
 *      colors in it, separated with commas. This function is able to extract
 *      a given item from this list. You may use the manifest constants
 *      defined in color.ch to extract common Clipper colors.
 *  $EXAMPLES$
 *      ? __ColorIndex( "W/N, N/W", CLR_ENHANCED ) // "N/W"
 *  $TESTS$
 *      see in rtl_test.prg for a comprehensive regression test suit.
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      Was not part of CA-Clipper.
 *  $SEEALSO$
 *      ColorSelect()
 *  $END$
 */

HARBOUR HB___COLORINDEX( void )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      char * szColor = hb_parc( 1 );
      ULONG  ulColorPos;
      ULONG  ulColorLen;
      USHORT uiColorIndex = ( USHORT ) hb_parni( 2 );

      /* Skip the given number of commas */

      for( ulColorPos = 0 ; szColor[ ulColorPos ] != '\0' && uiColorIndex > 0 ; ulColorPos++ )
      {
         if( szColor[ ulColorPos ] == ',' )
            uiColorIndex--;
      }

      /* if found, continue */

      if( uiColorIndex == 0 )
      {
         /* Skip the spaces after the comma */

         while( szColor[ ulColorPos ] == ' ' ) ulColorPos++;

         /* Search for next comma or end of string */

         ulColorLen = 0;

         while( szColor[ ulColorPos + ulColorLen ] != '\0' &&
                szColor[ ulColorPos + ulColorLen ] != ',' ) ulColorLen++;

         /* Skip the trailing spaces */

         while( ulColorLen > 0 &&
                szColor[ ulColorPos + ulColorLen - 1 ] == ' ' ) ulColorLen--;

         /* Return the string */

         hb_retclen( szColor + ulColorPos, ulColorLen );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

