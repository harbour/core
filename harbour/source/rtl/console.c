/*
 * $Id$
 */

/* Harbour Project source code
   http://www.Harbour-Project.org/

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com

   Partial Copyright (C) 1999 Eddie Runia <eddie@runia.com>
     partial copyright regarding the following function :
        __ACCEPT()

   The following functions are Copyright 1999 David G. Holm <dholm@jsd-llc.com>:
      adjust_pos(), hb_altout(), hb_devout(), HB_DEVOUT(), hb_devpos(),
      HB_DEVPOS(), hb_dispout(), HB___EJECT(), hb_max_col(), HB_MAXCOL(),
      hb_max_row(), HB_MAXROW(), hb_out(), hb_outerr(), HB_OUTERR(),
      hb_outstd(), HB_OUTSTD(), HB_PCOL(), HB_PROW(), hb_setpos(),
      HB_SETPOS(), HB_SETPRC(), HB_SCROLL(), and InitializeConsole().
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following function is Copyright 1999 Victor Szel <info@szelvesz.hu>:
      HB___COLORINDEX().
      HB___COLORINVERT().
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "hbsetup.h"
#include "extend.h"
#include "itemapi.h"
#include "init.h"
#include "dates.h"
#include "set.h"
#include "inkey.h"

#if defined(__GNUC__)
  #include <unistd.h>
  #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(HARBOUR_GCC_OS2)
    #include <io.h>
  #endif
#else
  #include <io.h>
#endif
#include <fcntl.h>
#include "gtapi.h"            /* HARBOUR_USE_GTAPI is checked inside gtapi.h, so that
                                 we can always get the border styles */

#define ACCEPT_BUFFER_LEN 256 /*length of input buffer for ACCEPT command */

#if defined(OS_UNIX_COMPATIBLE)
#define CRLF_BUFFER_LEN 2     /*length of buffer for CR/LF characters */
#else
#define CRLF_BUFFER_LEN 3     /*length of buffer for CR/LF characters */
#endif

HARBOUR HB___ACCEPT(void);
HARBOUR HB___EJECT( void );
HARBOUR HB_COL( void );
HARBOUR HB_ROW( void );
HARBOUR HB_DBGSHADOW( void );
HARBOUR HB_DEVOUT( void );
HARBOUR HB_DEVOUTPICT( void );
HARBOUR HB_DEVPOS( void );
HARBOUR HB_DISPBEGIN( void );
HARBOUR HB_DISPBOX( void );
HARBOUR HB_DISPCOUNT( void );
HARBOUR HB_DISPEND( void );
HARBOUR HB_DISPOUT( void );
HARBOUR HB_ISCOLOR( void );
HARBOUR HB_MAXCOL( void );
HARBOUR HB_MAXROW( void );
HARBOUR HB_NOSNOW( void );
HARBOUR HB_OUTSTD( void );
HARBOUR HB_OUTERR( void );
HARBOUR HB_PCOL( void );
HARBOUR HB_PROW( void );
HARBOUR HB_RESTSCREEN( void );
HARBOUR HB_SCROLL( void );
HARBOUR HB_SAVESCREEN( void );
HARBOUR HB_SETCURSOR( void );
HARBOUR HB_SETBLINK( void );
HARBOUR HB_SETPOS( void );
HARBOUR HB_SETPRC( void );
HARBOUR HB_SHADOW( void );
HARBOUR HB_QOUT( void );
HARBOUR HB_QQOUT( void );
HARBOUR HB___COLORINDEX( void );

HB_INIT_SYMBOLS_BEGIN( Console__InitSymbols )
{ "__ACCEPT"    , FS_PUBLIC, HB___ACCEPT    , 0 },
{ "__EJECT"     , FS_PUBLIC, HB___EJECT     , 0 },
{ "COL"         , FS_PUBLIC, HB_COL         , 0 },
{ "ROW"         , FS_PUBLIC, HB_ROW         , 0 },
{ "DBGSHADOW"   , FS_PUBLIC, HB_DBGSHADOW   , 0 },
{ "DEVOUT"      , FS_PUBLIC, HB_DEVOUT      , 0 },
{ "DEVOUTPICT"  , FS_PUBLIC, HB_DEVOUTPICT  , 0 },
{ "DEVPOS"      , FS_PUBLIC, HB_DEVPOS      , 0 },
{ "DISPBEGIN"   , FS_PUBLIC, HB_DISPBEGIN   , 0 },
{ "DISPBOX"     , FS_PUBLIC, HB_DISPBOX     , 0 },
{ "DISPCOUNT"   , FS_PUBLIC, HB_DISPCOUNT   , 0 },
{ "DISPEND"     , FS_PUBLIC, HB_DISPEND     , 0 },
{ "DISPOUT"     , FS_PUBLIC, HB_DISPOUT     , 0 },
{ "ISCOLOR"     , FS_PUBLIC, HB_ISCOLOR     , 0 },
{ "MAXCOL"      , FS_PUBLIC, HB_MAXCOL      , 0 },
{ "MAXROW"      , FS_PUBLIC, HB_MAXROW      , 0 },
{ "NOSNOW"      , FS_PUBLIC, HB_NOSNOW      , 0 },
{ "OUTERR"      , FS_PUBLIC, HB_OUTERR      , 0 },
{ "OUTSTD"      , FS_PUBLIC, HB_OUTSTD      , 0 },
{ "PCOL"        , FS_PUBLIC, HB_PCOL        , 0 },
{ "PROW"        , FS_PUBLIC, HB_PROW        , 0 },
{ "RESTSCREEN"  , FS_PUBLIC, HB_RESTSCREEN  , 0 },
{ "SCROLL"      , FS_PUBLIC, HB_SCROLL      , 0 },
{ "SAVESCREEN"  , FS_PUBLIC, HB_SAVESCREEN  , 0 },
{ "SETCURSOR"   , FS_PUBLIC, HB_SETCURSOR   , 0 },
{ "SETBLINK"    , FS_PUBLIC, HB_SETBLINK    , 0 },
{ "SETPOS"      , FS_PUBLIC, HB_SETPOS      , 0 },
{ "SETPRC"      , FS_PUBLIC, HB_SETPRC      , 0 },
{ "SHADOW"      , FS_PUBLIC, HB_SHADOW      , 0 },
{ "QOUT"        , FS_PUBLIC, HB_QOUT        , 0 },
{ "QQOUT"       , FS_PUBLIC, HB_QQOUT       , 0 },
{ "__COLORINDEX", FS_PUBLIC, HB___COLORINDEX, 0 }
HB_INIT_SYMBOLS_END( Console__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup Console__InitSymbols
#endif

static USHORT dev_row, dev_col, p_row, p_col;
static char CrLf [ CRLF_BUFFER_LEN ];

void hb_consoleRelease( void )
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtExit ();
#endif
}

void hb_consoleInitialize( void )
{
#if defined(OS_DOS_COMPATIBLE)
   CrLf [0] = 13;
   CrLf [1] = 10;
   CrLf [2] = 0;
#else
   CrLf [0] = 10;
   CrLf [1] = 0;
#endif

   p_row = p_col = 0;

   /* Some compilers open stdout and stderr in text mode, but
      Harbour needs them to be open in binary mode. */
#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(__DJGPP__) || defined(__CYGWIN__)
   setmode( fileno( stdout ), O_BINARY );
   setmode( fileno( stderr ), O_BINARY );
#elif defined(_MSC_VER)
   _setmode( _fileno( stdout ), _O_BINARY );
   _setmode( _fileno( stderr ), _O_BINARY );
#endif

#ifdef HARBOUR_USE_GTAPI
   hb_gtInit();
   hb_gtGetPos( &dev_row, & dev_col);
#else
   dev_row = 0;
   dev_col = 0;
#endif
}

WORD hb_max_row( void )
{
#ifdef HARBOUR_USE_GTAPI
   return hb_gtMaxRow ();
#else
   return 23; /* QUESTION: Shouldn't this be 24 ? info@szelvesz.hu */
#endif        /* ANSWER  : No. ANSI terminals commonly only have 24 lines */
}

WORD hb_max_col( void )
{
#ifdef HARBOUR_USE_GTAPI
   return hb_gtMaxCol ();
#else
   return 79;
#endif
}

#ifndef HARBOUR_USE_GTAPI
static void adjust_pos( char * fpStr, ULONG len, WORD * row, WORD * col, WORD max_row, WORD max_col )
{
   ULONG count;
   char * fpPtr = fpStr;

   for( count = 0; count < len; count++ )
   {
      switch( *fpPtr++  )
      {
         case 7:
            break;
         case 8:
            if( *col ) (*col)--;
            else
            {
               *col = max_col;
               if( *row ) (*row)--;
            }
            break;
         case 10:
            if( *row < max_row ) (*row)++;
            break;
         case 13:
            *col = 0;
            break;
         default:
            if( *col < max_col ) (*col)++;
            else
            {
               *col = 0;
               if( *row < max_row ) (*row)++;
            }
      }
   }
}
#endif

typedef void hb_out_func_typedef (char *, ULONG);

/* Format items for output, then call specified output function */
static void hb_out( WORD wParam, hb_out_func_typedef * hb_out_func )
{
   char * szText;
   PHB_ITEM pItem = hb_param( wParam, IT_ANY );
   char szBuffer [11];

   switch( hb_parinfo( wParam ) )
   {
      case IT_STRING:
           hb_out_func( hb_parc( wParam ), hb_parclen( wParam ) );
           break;

      case IT_DATE:
           szText = hb_dtoc( hb_pards( wParam ), szBuffer, hb_set.HB_SET_DATEFORMAT );
           if( szText )
                 hb_out_func( szText, strlen( szText ) );
           break;

      case IT_DOUBLE:
      case IT_INTEGER:
      case IT_LONG:
           szText = hb_itemStr( pItem, 0, 0 ); /* Let hb_itemStr() do the hard work */
           if( szText )
           {
              hb_out_func( szText, strlen( szText ) );
              hb_xfree( szText );
           }
           break;

      case IT_NIL:
           hb_out_func( "NIL", 3 );
           break;

      case IT_LOGICAL:
           if( hb_parl( wParam ) )
              hb_out_func( ".T.", 3 );
           else
              hb_out_func( ".F.", 3 );
           break;

      default:
           break;
   }
}

/* Output an item to STDOUT */
static void hb_outstd( char * fpStr, ULONG len )
{
   ULONG count = len;
   char * fpPtr = fpStr;

   if( strlen( fpStr ) != count )
      while( count-- ) printf( "%c", *fpPtr++ );
   else
      printf( "%s", fpStr );
   fflush( stdout );
#ifdef HARBOUR_USE_GTAPI
 #ifndef __CYGWIN__
   if( isatty( fileno( stdout ) ) )
 #endif
   {
      dev_row = hb_gt_Row();
      dev_col = hb_gt_Col();
      hb_gtSetPos( dev_row, dev_col );
   }
#else
   adjust_pos( fpStr, len, &dev_row, &dev_col, hb_max_row(), hb_max_col() );
#endif
}

/* Output an item to STDERR */
static void hb_outerr( char * fpStr, ULONG len )
{
   ULONG count = len;
   char * fpPtr = fpStr;

   if( strlen( fpStr ) != count )
      while( count-- ) fprintf( stderr, "%c", *fpPtr++ );
   else
      fprintf( stderr, "%s", fpStr );
   fflush( stderr );
#ifdef HARBOUR_USE_GTAPI
 #ifndef __CYGWIN__
   if( isatty( fileno( stderr ) ) )
 #endif
   {
      dev_row = hb_gt_Row();
      dev_col = hb_gt_Col();
      hb_gtSetPos( dev_row, dev_col );
   }
#else
   adjust_pos( fpStr, len, &dev_row, &dev_col, hb_max_row(), hb_max_col() );
#endif
}

/* Output an item to the screen and/or printer and/or alternate */
static void hb_altout( char * fpStr, ULONG len )
{
   char * fpPtr = fpStr;
   if( hb_set.HB_SET_CONSOLE )
   {
   #ifdef HARBOUR_USE_GTAPI
      hb_gtWriteCon( fpStr, len );
      hb_gtGetPos( &dev_row, &dev_col );
   #else
      ULONG count = len;
      if( strlen( fpStr ) != count )
         while( count-- ) printf( "%c", *fpPtr++ );
      else
         printf( "%s", fpStr );
      adjust_pos( fpStr, len, &dev_row, &dev_col, hb_max_row(), hb_max_col() );
   #endif
   }
   if( hb_set.HB_SET_ALTERNATE && hb_set_althan >= 0 )
   {
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      unsigned write_len;
      ULONG count = len;
      fpPtr = fpStr;
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
         write( hb_set_althan, fpPtr, write_len );
         fpPtr += write_len;
      }
   }
   if( hb_set_extrahan >= 0 )
   {
      /* Print to extra file if valid alternate file */
      unsigned write_len;
      ULONG count = len;
      fpPtr = fpStr;
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
         write( hb_set_extrahan, fpPtr, write_len );
         fpPtr += write_len;
      }
   }
   if( hb_set.HB_SET_PRINTER && hb_set_printhan >= 0 )
   {
      /* Print to printer if SET PRINTER ON and valid printer file */
      unsigned write_len;
      ULONG count = len;
      fpPtr = fpStr;
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
         write( hb_set_printhan, fpPtr, write_len );
         fpPtr += write_len;
      }
      if( len + p_col > USHRT_MAX ) p_col = USHRT_MAX;
      else p_col += len;
   }
}

/* Output an item to the screen and/or printer */
static void hb_devout( char * fpStr, ULONG len )
{
   if( hb_set_printhan >= 0 && hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      unsigned write_len;
      ULONG count = len;
      char * fpPtr = fpStr;
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
         write( hb_set_printhan, fpPtr, write_len );
         fpPtr += write_len;
      }
      if( len + p_col > USHRT_MAX ) p_col = USHRT_MAX;
      else p_col += len;
   }
   else
   {
   #ifdef HARBOUR_USE_GTAPI
      /* Otherwise, display to console */
      hb_gtWrite( fpStr, len );
      hb_gtGetPos( &dev_row, &dev_col );
   #else
      ULONG count = len;
      char * fpPtr = fpStr;
      if( strlen( fpStr ) != count )
         while( count-- ) printf( "%c", *fpPtr++ );
      else
         printf( "%s", fpStr );
      adjust_pos( fpStr, len, &dev_row, &dev_col, hb_max_row(), hb_max_col() );
   #endif
   }
}

/* Output an item to the screen */
static void hb_dispout( char * fpStr, ULONG len )
{
   #ifdef HARBOUR_USE_GTAPI
      /* Display to console */
      hb_gtWrite( fpStr, len );
      hb_gtGetPos( &dev_row, &dev_col );
   #else
      ULONG count = len;
      char * fpPtr = fpStr;
      if( strlen( fpStr ) != count )
         while( count-- ) printf( "%c", *fpPtr++ );
      else
         printf( "%s", fpStr );
      adjust_pos( fpStr, len, &dev_row, &dev_col, hb_max_row(), hb_max_col() );
   #endif
}

void hb_setpos( WORD row, WORD col )
{
   #ifdef HARBOUR_USE_GTAPI
      hb_gtSetPos( row, col );
   #else
      WORD count;

      if( row < dev_row || col < dev_col )
      {
         printf("\n");
         dev_col = 0;
         dev_row++;
      }
      else if( row > dev_row ) dev_col = 0;
      for( count = dev_row; count < row; count++ ) printf("\n");
      for( count = dev_col; count < col; count++ ) printf(" ");
   #endif
      dev_row = row;
      dev_col = col;
}

void hb_devpos( WORD row, WORD col )
{
   WORD count;
   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */
   if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set_printhan >= 0 )
   {
      if( row < p_row )
      {
         write( hb_set_printhan, "\x0C", 1 );
         p_row = p_col = 0;
      }
      for( count = p_row; count < row; count++ ) write( hb_set_printhan, CrLf, CRLF_BUFFER_LEN-1 );
      if( row > p_row ) p_col = 0;
      col += hb_set.HB_SET_MARGIN;
      for( count = p_col; count < col; count++ ) write( hb_set_printhan, " ", 1 );
      p_row = row;
      p_col = col;
   }
   else
   {
      hb_setpos( row, col );
   }
}

HARBOUR HB_OUTSTD( void ) /* writes a list of values to the standard output device */
{
   WORD w;

   for( w = 0; w < hb_pcount(); w++ )
   {
      hb_out( w + 1, hb_outstd );
      if( w < hb_pcount() - 1) hb_outstd( " ", 1 );
   }
}

HARBOUR HB_OUTERR( void ) /* writes a list of values to the standard error device */
{
   WORD w;

   for( w = 0; w < hb_pcount(); w++ )
   {
      hb_out( w + 1, hb_outerr );
      if( w < hb_pcount() - 1) hb_outerr( " ", 1 );
   }
}

HARBOUR HB_QQOUT( void ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   WORD w;

   for( w = 0; w < hb_pcount(); w++ )
   {
      hb_out( w + 1, hb_altout );
      if( w < hb_pcount() - 1) hb_altout( " ", 1 );
   }
}

HARBOUR HB_QOUT( void )
{
   WORD count;
   hb_altout( CrLf, CRLF_BUFFER_LEN-1 );
   if( hb_set.HB_SET_PRINTER && hb_set_printhan >= 0 )
   {
      p_row++;
      p_col = hb_set.HB_SET_MARGIN;
      count = p_col;
      while( count-- > 0 ) write( hb_set_printhan, " ", 1 );
   }
   HB_QQOUT();
}

HARBOUR HB_SETPOS( void ) /* Sets the screen position */
{
   PHB_ITEM pRow, pCol;
   if( hb_pcount() > 1 )
   {
      pRow = hb_param( 1, IT_NUMERIC );
      pCol = hb_param( 2, IT_NUMERIC );
      if( pRow && pCol )
      {
         int i_row = hb_parni( 1 );
         int i_col = hb_parni( 2 );
         WORD row, col;

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
}

HARBOUR HB_DEVPOS( void ) /* Sets the screen and/or printer position */
{
   PHB_ITEM pRow, pCol;
   if( hb_pcount() > 1 )
   {
      pRow = hb_param( 1, IT_NUMERIC );
      pCol = hb_param( 2, IT_NUMERIC );
      if( pRow && pCol )
      {
         long l_row = hb_parnl( 1 );
         long l_col = hb_parnl( 2 );
         WORD row, col;

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
}

HARBOUR HB_DEVOUT( void ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( hb_pcount() > 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      char fpOldColor[ CLR_STRLEN ];
      if( ISCHAR(2) )
      {
         hb_gtGetColorStr( fpOldColor );
         hb_gtSetColorStr( hb_parc(2) );
      }
#endif

      hb_out( 1, hb_devout );

#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR(2) )
      {
         hb_gtSetColorStr( fpOldColor );
      }
#endif
   }
}

HARBOUR HB_DISPOUT( void ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( hb_pcount() > 0 )
   {
#ifdef HARBOUR_USE_GTAPI
      char fpOldColor[ CLR_STRLEN ];

      if( ISCHAR(2) )
      {
         hb_gtGetColorStr( fpOldColor );
         hb_gtSetColorStr( hb_parc(2) );
      }
#endif

      hb_out( 1, hb_dispout );

#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR(2) )
      {
         hb_gtSetColorStr( fpOldColor );
      }
#endif
   }
}

HARBOUR HB___EJECT( void ) /* Ejects the current page from the printer */
{
   if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set_printhan >= 0 )
   {
      write( hb_set_printhan, "\x0C\x0D", 2 );
      p_row = p_col = 0;
   }
}

HARBOUR HB_PROW( void ) /* Returns the current printer row position */
{
   hb_retni( p_row );
}

HARBOUR HB_PCOL( void ) /* Returns the current printer row position */
{
   hb_retni( p_col );
}

HARBOUR HB_SETPRC( void ) /* Sets the current printer row and column positions */
{
   if( hb_pcount() > 1 )
   {
      PHB_ITEM pRow = hb_param( 1, IT_NUMERIC );
      PHB_ITEM pCol = hb_param( 1, IT_NUMERIC );
      if( pRow && pCol )
      {
         long l_row = hb_parnl( 1 );
         long l_col = hb_parnl( 2 );

         /* Limit the new position to the range (0,0) to (65535,65535) */
         if( l_row < 0 ) p_row = 0;
         else if( l_row > USHRT_MAX ) p_row = USHRT_MAX;
         else p_row = l_row;
         if( l_col < 0 ) p_col = 0;
         else if( l_col > USHRT_MAX ) p_col = USHRT_MAX;
         else p_col = l_col;
      }
   }
}

HARBOUR HB_SCROLL( void ) /* Scrolls a screen region (requires the GT API) */
{
   int i_top = 0, i_left = 0,iMR, iMC, i_bottom = iMR=hb_max_row(), i_right = iMC = hb_max_col(),
   v_scroll = 0, h_scroll = 0;
   WORD top, left, bottom, right;

   if( hb_pcount() > 0 && hb_param( 1, IT_NUMERIC ) )
      i_top = hb_parni( 1 );
   if( hb_pcount() > 1 && hb_param( 2, IT_NUMERIC ) )
      i_left = hb_parni( 2 );
   if( hb_pcount() > 2 && hb_param( 3, IT_NUMERIC ) )
      i_bottom = hb_parni( 3 );
   if( hb_pcount() > 3 && hb_param( 4, IT_NUMERIC ) )
      i_right = hb_parni( 4 );
   if( hb_pcount() > 4 && hb_param( 5, IT_NUMERIC ) )
      v_scroll = hb_parni( 5 );
   if( hb_pcount() > 5 && hb_param( 6, IT_NUMERIC ) )
      h_scroll = hb_parni( 6 );

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
      WORD count;
      dev_row = iMR;
      for( count = 0; count < dev_row ; count++ ) printf( "\n" );
      dev_row = dev_col = 0;
   }
#endif
}

HARBOUR HB_MAXROW( void ) /* Return the maximum screen row number (zero origin) */
{
   hb_retni( hb_max_row () );
}

HARBOUR HB_MAXCOL( void ) /* Return the maximum screen column number (zero origin) */
{
   hb_retni( hb_max_col () );
}

HARBOUR HB_ROW( void ) /* Return the current screen row position (zero origin) */
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtGetPos( &dev_row, & dev_col);
#endif
   hb_retni( dev_row );
}

HARBOUR HB_COL( void ) /* Return the current screen column position (zero origin) */
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtGetPos( &dev_row, & dev_col);
#endif
   hb_retni( dev_col );
}

HARBOUR HB_DISPBOX (void)
{
#ifdef HARBOUR_USE_GTAPI
   if (ISNUM(1) && ISNUM(2) && ISNUM(3) && ISNUM(4))
   {
      char szOldColor [CLR_STRLEN];

      if (ISCHAR(6))
      {
         hb_gtGetColorStr(szOldColor);
         hb_gtSetColorStr(hb_parc(6));
      }

      if (ISCHAR(5))
      {
         hb_gtBox(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parc(5));
      }
      else if (ISNUM(5) && hb_parni(5) == 2)
      {
         hb_gtBoxD(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4));
      }
      else
      {
         hb_gtBoxS(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4));
      }

      if (ISCHAR(6))
      {
         hb_gtSetColorStr(szOldColor);
      }
   }
#else
   if (ISNUM(1) && ISNUM(2) && ISNUM(3) && ISNUM(4))
   {
      char * szBorderStyle = B_SINGLE;
      int i_top = hb_parni( 1 ), i_left = hb_parni( 2 );
      int i_bottom = hb_parni( 3 ), i_right = hb_parni( 4 );
      WORD top, left, bottom, right, size = strlen( B_SINGLE );
      WORD row, col, width, height;
      char Borders[ 9 ];

      /* Set limits on the box coordinates to (0,0) and (max_row(),max_col()) */
      if( i_top < 0 ) top = 0; else top = (WORD)i_top;
      if( i_left < 0 ) left = 0; else left = (WORD)i_left;
      if( i_bottom < 0 ) bottom  = 0; else bottom = (WORD)i_bottom;
      if( i_right < 0 ) right = 0; else right = (WORD)i_right;
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
      for( col = (height > 1 ? left + 1 : left ); col < (height > 1 ? right : right + 1); col++ )
         printf( "%c", Borders[ 1 ] );    /* Top line */
      if( height > 1 && width > 1 )
         printf( "%c", Borders[ 2 ] );    /* Upper right corner */
      for( row = (height > 1 ? top + 1 : top); row < (width > 1 ? bottom : bottom + 1); row++ )
      {
         hb_setpos( row, left );
         if( height > 1 )
            printf( "%c", Borders[ 3 ] ); /* Left side */
         if( height > 1 && width > 1) for( col = left + 1; col < right; col++ )
            printf( "%c", Borders[ 8 ] ); /* Fill */
         if( height > 1 && width > 1 )
            printf( "%c", Borders[ 7 ] ); /* Right side */
      }
      if( height > 1 && width > 1)
      {
         hb_setpos( bottom, left );
         col = left;
         printf( "%c", Borders[ 6 ] );    /* Bottom left corner */
         for( col = left + 1; col < right; col++ )
            printf( "%c", Borders[ 5 ] ); /* Bottom line */
         printf( "%c", Borders[ 4 ] );    /* Bottom right corner */
      }
      hb_setpos( bottom + 1, right + 1);
   }
#endif
}

HARBOUR HB_DISPBEGIN (void)
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtDispBegin();
#endif
}

HARBOUR HB_DISPEND (void)
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtDispEnd();
#endif
}

HARBOUR HB_DISPCOUNT (void)
{
#ifdef HARBOUR_USE_GTAPI
   hb_retni(hb_gtDispCount());
#else
   hb_retni(0);
#endif
}

HARBOUR HB_ISCOLOR (void)
{
#ifdef HARBOUR_USE_GTAPI
   hb_retl(hb_gtIsColor());
#else
   hb_retl(FALSE);
#endif
}

HARBOUR HB_NOSNOW (void)
{
#ifdef HARBOUR_USE_GTAPI
   if ( ISLOG( 1 ) )
   {
      hb_gtSetSnowFlag(hb_parl(1));
   }
#endif
}

HARBOUR HB_SHADOW (void)
{
#ifdef HARBOUR_USE_GTAPI
  USHORT uiAttr;

   if( hb_pcount() == 4 )
      uiAttr = 7;
   else if( hb_pcount() == 5 )
      uiAttr = hb_parni(5);

   if( hb_pcount() > 3 )
      hb_gt_DrawShadow(hb_parni(1)+1,hb_parni(2)+1,hb_parni(3)+1,hb_parni(4)+1,uiAttr);
#endif
}

HARBOUR HB_DBGSHADOW (void)
{
   HB_SHADOW();
}

HARBOUR HB_SAVESCREEN (void)
{
#ifdef HARBOUR_USE_GTAPI
   USHORT uiX;
   USHORT uiCoords[ 4 ];
   char * pBuffer;

   uiCoords[ 0 ] = 0;
   uiCoords[ 1 ] = 0;
   uiCoords[ 2 ] = hb_gtMaxRow();
   uiCoords[ 3 ] = hb_gtMaxCol();

   for( uiX = 1; uiX < 5; uiX++ )
      if( ISNUM( uiX ) )
         uiCoords[uiX - 1 ] = hb_parni( uiX );

   hb_gtRectSize( uiCoords[0], uiCoords[1], uiCoords[2], uiCoords[3], &uiX );
   pBuffer = ( char * ) hb_xgrab( uiX );
   hb_gtSave( uiCoords[0], uiCoords[1], uiCoords[2], uiCoords[3], pBuffer );
   hb_retclen( pBuffer, uiX );
   hb_xfree( ( void * ) pBuffer );
#endif
}

HARBOUR HB_RESTSCREEN (void)
{
#ifdef HARBOUR_USE_GTAPI
   USHORT uiX;
   USHORT uiCoords[ 4 ];

   uiCoords[ 0 ] = 0;
   uiCoords[ 1 ] = 0;
   uiCoords[ 2 ] = hb_gtMaxRow();
   uiCoords[ 3 ] = hb_gtMaxCol();

   if( hb_pcount() == 5 )
   {
      for( uiX = 1; uiX < 5; uiX++ )
         if( ISNUM( uiX ) )
            uiCoords[uiX - 1 ] = hb_parni( uiX );

      hb_gtRest( uiCoords[0], uiCoords[1], uiCoords[2], uiCoords[3],
                 hb_parc( 5 ) );
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
#endif
}

HARBOUR HB_SETBLINK( void )
{
#ifdef HARBOUR_USE_GTAPI
   BOOL bPreviousBlink;

   hb_gtGetBlink( &bPreviousBlink );
   if ( ISLOG( 1 ) )
      hb_gtSetBlink( hb_parl( 1 ) );

   hb_retl( bPreviousBlink );
#else
   hb_retl( FALSE );
#endif
}

HARBOUR HB___ACCEPT( void ) /* Internal Clipper function used in ACCEPT command  */
                         /* Basically the simplest Clipper function to        */
                         /* receive data. Parameter : cPrompt. Returns : cRet */
{
   char *szResult = ( char * ) hb_xgrab(ACCEPT_BUFFER_LEN); /* Return parameter. */
   char *szPrompt = hb_parc(1);    /* Pass prompt                            */
   ULONG len      = hb_parclen(1);
   int input;

   if( hb_pcount() == 1 )          /* cPrompt passed                         */
   {
      hb_altout( CrLf, CRLF_BUFFER_LEN-1 );
      hb_altout( szPrompt, len );
   }
#ifdef OS_UNIX_COMPATIBLE
   /* Read the data using fgets(), because hb_inkeyPoll() doesn't support
       Unix compatible operating systems yet. */
   szResult[0] = '\x0';             /* start with something defined */
   if (fgets( szResult, ACCEPT_BUFFER_LEN, stdin ))
   {
      strtok(szResult, "\n");     /* strip off the trailing newline
                                     if it exists */
   }
#else
   len = 0;
   input = 0;
   while( input != 13 )
   {
      /* Wait forever, for keyboard events only */
      input = hb_inkey ( 0.0, INKEY_KEYBOARD, 1, 1 );
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
            if( len < 255 && input >= 32 && input <= 127 )
            {
               szResult[ len ] = input; /* Accept the input */
               hb_dispout( &szResult[ len ], 1 ); /* Then display it */
               len++;  /* Then adjust the input count */
            }
      }
   }
#endif
   hb_retclen( szResult, len );
   hb_xfree( szResult );
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
 *      see in coloring.prg for a comprehensive regression test suit.
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
   if ( ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      char * szColor = hb_parc( 1 );
      ULONG  ulColorPos;
      ULONG  ulColorLen;
      USHORT uiColorIndex = (USHORT) hb_parni( 2 );

      /* Skip the given number of commas */

      for ( ulColorPos = 0 ; szColor[ ulColorPos ] != '\0' && uiColorIndex > 0 ; ulColorPos++ )
      {
         if ( szColor[ ulColorPos ] == ',' )
            uiColorIndex--;
      }

      /* if found, continue */

      if ( uiColorIndex == 0 )
      {
         /* Skip the spaces after the comma */

         while ( szColor[ ulColorPos ] == ' ' ) ulColorPos++;

         /* Search for next comma or end of string */

         ulColorLen = 0;

         while ( szColor[ ulColorPos + ulColorLen ] != '\0' &&
                 szColor[ ulColorPos + ulColorLen ] != ',' ) ulColorLen++;

         /* Skip the trailing spaces */

         while ( ulColorLen > 0 &&
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

