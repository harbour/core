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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_consoleGetNewLine()
 *    HB_DISPOUTAT()
 *    HB_SETPOSBS()
 *    HB_DISPBOX() (GT version)
 *    HB_DISPBEGIN()
 *    HB_DISPEND()
 *    HB_DISPCOUNT()
 *    HB_ISCOLOR()
 *    HB_NOSNOW()
 *    HB___ACCEPTSTR()
 *    HB_HB_COLORINDEX()
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
#include "inkey.ch"
#include "gtapi.h"            /* HARBOUR_USE_GTAPI is checked inside gtapi.h, so that
                                 we can always get the border styles */
#include "mouseapi.h"

#if defined(__GNUC__) && ! defined(__MINGW32__)
   #include <unistd.h>
   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(HARBOUR_GCC_OS2)
      #include <io.h>
   #endif
#else
   #include <io.h>
#endif

#if defined(__CYGWIN__)
   #include <unistd.h>
   #include <termios.h>
#endif

#define ACCEPT_BUFFER_LEN 256 /* length of input buffer for ACCEPT command */

#if defined(OS_UNIX_COMPATIBLE)
   #define CRLF_BUFFER_LEN 2     /*length of buffer for CR/LF characters */
#else
   #define CRLF_BUFFER_LEN 3     /*length of buffer for CR/LF characters */
#endif

static BOOL   s_bInit = FALSE;
static SHORT  s_iDevRow;
static SHORT  s_iDevCol;
static USHORT s_uiPRow;
static USHORT s_uiPCol;
static char   s_szCrLf[ CRLF_BUFFER_LEN ];
static char   s_szAcceptResult[ ACCEPT_BUFFER_LEN ];
static int    s_iFilenoStdout;
static int    s_iFilenoStderr;

void hb_consoleInitialize( void )
{
   int iStderr;

   HB_TRACE(HB_TR_DEBUG, ("hb_consoleInitialize()"));

#if defined(OS_DOS_COMPATIBLE)
   s_szCrLf[ 0 ] = HB_CHAR_CR;
   s_szCrLf[ 1 ] = HB_CHAR_LF;
   s_szCrLf[ 2 ] = '\0';
#else
   s_szCrLf[ 0 ] = HB_CHAR_LF;
   s_szCrLf[ 1 ] = '\0';
#endif

   s_szAcceptResult[ 0 ] = '\0';

   s_uiPRow = s_uiPCol = 0;

   /* Some compilers open stdout and stderr in text mode, but
      Harbour needs them to be open in binary mode. */

   s_iFilenoStdout = fileno( stdout );
   hb_fsSetDevMode( s_iFilenoStdout, FD_BINARY );

   iStderr = hb_cmdargNum( "STDERR" ); /* Undocumented CA-Clipper switch //STDERR:x */

   if( iStderr < 0 )        /* //STDERR not used or invalid */
      s_iFilenoStderr = fileno( stderr );
   else if( iStderr == 0 )  /* //STDERR with no parameter or 0 */
      s_iFilenoStderr = s_iFilenoStdout;
   else                     /* //STDERR:x */
      s_iFilenoStderr = iStderr;

   hb_fsSetDevMode( s_iFilenoStderr, FD_BINARY );

#ifdef HARBOUR_USE_GTAPI
   hb_mouseInit();
   hb_gtInit();
   hb_gtGetPos( &s_iDevRow, &s_iDevCol );
   hb_gtSetCursor( SC_NORMAL );
#else
   s_iDevRow = 0;
   s_iDevCol = 0;
#endif

   s_bInit = TRUE;
}

void hb_consoleRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_consoleRelease()"));

   hb_fsSetDevMode( s_iFilenoStdout, FD_TEXT );
   hb_fsSetDevMode( s_iFilenoStderr, FD_TEXT );

   /* The is done by the OS from now on */
   s_szCrLf[ 0 ] = HB_CHAR_LF;
   s_szCrLf[ 1 ] = '\0';

#ifdef HARBOUR_USE_GTAPI
   hb_gtExit();
#endif

   s_bInit = FALSE;
}

char * hb_consoleGetNewLine( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_consoleGetNewLine()"));

   return s_szCrLf;
}

/*  $DOC$
 *  $FUNCNAME$
 *      HB_OSNEWLINE()
 *  $CATEGORY$
 *      Operating System Specific
 *  $ONELINER$
 *      Returns the newline character(s) to use with the current OS
 *  $SYNTAX$
 *      HB_OSNewLine() --> cString
 *  $ARGUMENTS$
 *  $RETURNS$
 *      A character string containing the character or characters required
 *      to move the screen cursor or print head to the start of a new line.
 *      The string will hold either CHR( 10 ) or CHR( 13 ) + CHR( 10 ).
 *  $DESCRIPTION$
 *      Returns a character string containing the character or characters
 *      required to move the screen cursor or print head to the start of a
 *      new line for the operating system that the program is running on
 *      (or thinks it is running on, if an OS emulator is being used).
 *  $EXAMPLES$
 *      // Get the newline character(s) for the current OS using defaults.
 *      STATIC s_cNewLine
 *      ...
 *      s_cNewLine := HB_OSNewLine()
 *      ...
 *      OutStd( "Hello World!" + s_cNewLine )
 *      ...
 *  $TESTS$
 *      valtype( HB_OSNewLine() ) == "C"
 *      LEN( HB_OSNewLine() ) == 1
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This is an add-on Operating System Tool function.
 *  $SEEALSO$
 *      dos.ngo:OS(),OUTSTD(),OUTERR()
 *  $END$
 */

HARBOUR HB_HB_OSNEWLINE( void )
{
   hb_retc( s_szCrLf );
}

USHORT hb_max_row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_max_row()"));

#ifdef HARBOUR_USE_GTAPI
   return hb_gtMaxRow();
#else
   #if defined(HB_OS_UNIX_COMPATIBLE)
      return 23;
   #else
      return 24;
   #endif
#endif
}

USHORT hb_max_col( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_max_col()"));

#ifdef HARBOUR_USE_GTAPI
   return hb_gtMaxCol();
#else
   return 79;
#endif
}

#ifndef HARBOUR_USE_GTAPI
static void adjust_pos( char * pStr, ULONG ulLen, SHORT * row, SHORT * col, USHORT max_row, USHORT max_col )
{
   ULONG ulCount;
   char * pPtr = pStr;

   HB_TRACE(HB_TR_DEBUG, ("adjust_pos(%s, %lu, %p, %p, %hu, %hu)", pStr, ulLen, row, col, max_row, max_col));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pPtr++  )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( *col ) ( *col )--;
            else
            {
               *col = max_col;
               if( *row ) ( *row )--;
            }
            break;

         case HB_CHAR_LF:
            if( *row < max_row ) ( *row )++;
            break;

         case HB_CHAR_CR:
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

typedef void hb_out_func_typedef( char *, ULONG );

/* Format items for output, then call specified output function */
static void hb_out( USHORT uiParam, hb_out_func_typedef * hb_out_func )
{
   ULONG ulLen;
   BOOL bFreeReq;
   PHB_ITEM pItem;
   char * pString;

   HB_TRACE(HB_TR_DEBUG, ("hb_out(%hu, %p)", uiParam, hb_out_func));

   pItem = hb_param( uiParam, IT_ANY );
   pString = hb_itemString( pItem, &ulLen, &bFreeReq );

   hb_out_func( pString, ulLen );

   if( bFreeReq )
      hb_xfree( pString );
}

/* Output an item to STDOUT */
void hb_outstd( char * pStr, ULONG ulLen )
{
   USHORT user_ferror;

   HB_TRACE(HB_TR_DEBUG, ("hb_outstd(%s, %lu)", pStr, ulLen));

   if( ulLen == 0 )
      ulLen = strlen( pStr );

#ifdef HARBOUR_USE_GTAPI
   if( s_bInit )
      hb_gtPreExt();
#endif

   user_ferror = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( user_ferror ); /* Restore last user file error code */

#ifdef HARBOUR_USE_GTAPI
   if( s_bInit )
   {
      #ifndef __CYGWIN__
      if( isatty( s_iFilenoStdout ) )
      #endif
      {
         s_iDevRow = hb_gt_Row();
         s_iDevCol = hb_gt_Col();
         hb_gtSetPos( s_iDevRow, s_iDevCol );
      }
      hb_gtPostExt();
   }
#else
   adjust_pos( pStr, ulLen, &s_iDevRow, &s_iDevCol, hb_max_row(), hb_max_col() );
#endif
}

/* Output an item to STDERR */
void hb_outerr( char * pStr, ULONG ulLen )
{
   USHORT user_ferror;

   HB_TRACE(HB_TR_DEBUG, ("hb_outerr(%s, %lu)", pStr, ulLen));

   if( ulLen == 0 )
      ulLen = strlen( pStr );

#ifdef HARBOUR_USE_GTAPI
   if( s_bInit )
      hb_gtPreExt();
#endif

   user_ferror = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStderr, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( user_ferror ); /* Restore last user file error code */

#ifdef HARBOUR_USE_GTAPI
   if( s_bInit )
   {
      #ifndef __CYGWIN__
      if( isatty( s_iFilenoStdout ) )
      #endif
      {
         s_iDevRow = hb_gt_Row();
         s_iDevCol = hb_gt_Col();
         hb_gtSetPos( s_iDevRow, s_iDevCol );
      }
      hb_gtPostExt();
   }
#else
   adjust_pos( pStr, ulLen, &s_iDevRow, &s_iDevCol, hb_max_row(), hb_max_col() );
#endif
}

/* Output an item to the screen and/or printer and/or alternate */
static void hb_altout( char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_altout(%s, %lu)", pStr, ulLen));

   if( hb_set.HB_SET_CONSOLE )
   {
#ifdef HARBOUR_USE_GTAPI
      hb_gtWriteCon( ( BYTE * ) pStr, ulLen );
      hb_gtGetPos( &s_iDevRow, &s_iDevCol );
#else
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pStr, ulLen );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
      adjust_pos( pStr, ulLen, &s_iDevRow, &s_iDevCol, hb_max_row(), hb_max_col() );
#endif
   }

   if( hb_set.HB_SET_ALTERNATE && hb_set.hb_set_althan != FS_ERROR )
   {
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWriteLarge( hb_set.hb_set_althan, ( BYTE * ) pStr, ulLen );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }

   if( hb_set.hb_set_extrahan != FS_ERROR )
   {
      /* Print to extra file if valid alternate file */
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWriteLarge( hb_set.hb_set_extrahan, ( BYTE * ) pStr, ulLen );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }

   if( hb_set.HB_SET_PRINTER && hb_set.hb_set_printhan != FS_ERROR )
   {
      /* Print to printer if SET PRINTER ON and valid printer file */
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWriteLarge( hb_set.hb_set_printhan, ( BYTE * ) pStr, ulLen );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
      s_uiPCol += ulLen;
   }
}

/* Output an item to the screen and/or printer */
static void hb_devout( char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_devout(%s, %lu)", pStr, ulLen));

   if( hb_set.hb_set_printhan != FS_ERROR && hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWriteLarge( hb_set.hb_set_printhan, ( BYTE * ) pStr, ulLen );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
      s_uiPCol += ulLen;
   }
   else
   {
#ifdef HARBOUR_USE_GTAPI
      /* Otherwise, display to console */
      hb_gtWriteAt( s_iDevRow, s_iDevCol, ( BYTE * ) pStr, ulLen );
      hb_gtGetPos( &s_iDevRow, &s_iDevCol );
#else
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pStr, ulLen );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
      adjust_pos( pStr, ulLen, &s_iDevRow, &s_iDevCol, hb_max_row(), hb_max_col() );
#endif
   }
}

/* Output an item to the screen */
static void hb_dispout( char * pStr, ULONG ulLen )
{
   USHORT user_ferror;

   HB_TRACE(HB_TR_DEBUG, ("hb_dispout(%s, %lu)", pStr, ulLen));

#ifdef HARBOUR_USE_GTAPI
   /* Display to console */
   hb_gtWriteAt( s_iDevRow, s_iDevCol, ( BYTE * ) pStr, ulLen );
   hb_gtGetPos( &s_iDevRow, &s_iDevCol );
   HB_SYMBOL_UNUSED( user_ferror );
#else
   user_ferror = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( user_ferror ); /* Restore last user file error code */
   adjust_pos( pStr, ulLen, &s_iDevRow, &s_iDevCol, hb_max_row(), hb_max_col() );
#endif
}

void hb_setpos( SHORT row, SHORT col )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setpos(%hd, %hd)", row, col));

#ifdef HARBOUR_USE_GTAPI
   hb_gtSetPos( row, col );
#else
   {
      SHORT iCount;

      if( row < s_iDevRow || col < s_iDevCol )
      {
         fputs( s_szCrLf, stdout );
         s_iDevCol = 0;
         s_iDevRow++;
      }
      else if( row > s_iDevRow ) s_iDevCol = 0;
      for( iCount = s_iDevRow; iCount < row; iCount++ )
         fputs( s_szCrLf, stdout );
      for( iCount = s_iDevCol; iCount < col; iCount++ )
         fputc( ' ', stdout );
      fflush( stdout );
   }
#endif

   s_iDevRow = row;
   s_iDevCol = col;
}

void hb_devpos( SHORT row, SHORT col )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_devpos(%hd, %hd)", row, col));

   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */
   if( hb_set.hb_set_printhan != FS_ERROR && hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      USHORT uiCount, uiProw = ( USHORT ) row, uiPcol = ( USHORT ) col;
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      if( uiProw < s_uiPRow )
      {
         hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) "\x0C\x0D", 2 );
         s_uiPRow = s_uiPCol = 0;
      }

      for( uiCount = s_uiPRow; uiCount < uiProw; uiCount++ )
         hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) s_szCrLf, CRLF_BUFFER_LEN-1 );

      if( uiProw > s_uiPRow ) s_uiPCol = 0;
      uiPcol += hb_set.HB_SET_MARGIN;

      for( uiCount = s_uiPCol; uiCount < uiPcol; uiCount++ )
         hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) " ", 1 );

      s_uiPRow = uiProw;
      s_uiPCol = uiPcol;
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
   else
      hb_setpos( row, col );
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
   USHORT uiCount;

   hb_altout( s_szCrLf, CRLF_BUFFER_LEN - 1 );

   if( hb_set.HB_SET_PRINTER && hb_set.hb_set_printhan != FS_ERROR )
   {
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      s_uiPRow++;
      s_uiPCol = hb_set.HB_SET_MARGIN;
      uiCount = s_uiPCol;
      while( uiCount-- > 0 )
         hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) " ", 1 );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }

   HB_QQOUT();
}

HARBOUR HB_SETPOS( void ) /* Sets the screen position */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_setpos( hb_parni( 1 ), hb_parni( 2 ) );
}

/* Move the screen position to the right by one column */
HARBOUR HB_SETPOSBS( void )
{
#ifdef HARBOUR_USE_GTAPI
   SHORT iRow, iCol;

   /* NOTE: Clipper does no checks about reaching the border or anything.
            [vszakats] */
   hb_gtGetPos( &iRow, &iCol );
   hb_gtSetPos( iRow, iCol + 1 );
#endif
}

HARBOUR HB_DEVPOS( void ) /* Sets the screen and/or printer position */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_devpos( hb_parni( 1 ), hb_parni( 2 ) );
}

HARBOUR HB_DEVOUT( void ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( hb_pcount() >= 1 )
   {
#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR( 2 ) )
      {
         char szOldColor[ CLR_STRLEN ];

         hb_gtGetColorStr( szOldColor );
         hb_gtSetColorStr( hb_parc( 2 ) );

         hb_out( 1, hb_devout );

         hb_gtSetColorStr( szOldColor );
      }
      else
         hb_out( 1, hb_devout );
#else
      hb_out( 1, hb_devout );
#endif
   }
}

HARBOUR HB_DISPOUT( void ) /* writes a single value to the screen, but is not affected by SET ALTERNATE */
{
   if( hb_pcount() >= 1 )
   {
#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR( 2 ) )
      {
         char szOldColor[ CLR_STRLEN ];

         hb_gtGetColorStr( szOldColor );
         hb_gtSetColorStr( hb_parc( 2 ) );

         hb_out( 1, hb_dispout );

         hb_gtSetColorStr( szOldColor );
      }
      else
         hb_out( 1, hb_dispout );
#else
      hb_out( 1, hb_devout );
#endif
   }
}

/* Undocumented Clipper function */

HARBOUR HB_DISPOUTAT( void ) /* writes a single value to the screen at speficic position, but is not affected by SET ALTERNATE */
{
   if( hb_pcount() >= 3 )
   {
      /* NOTE: Clipper does no checks here. [vszakats] */
      hb_setpos( hb_parni( 1 ), hb_parni( 2 ) );

#ifdef HARBOUR_USE_GTAPI
      if( ISCHAR( 4 ) )
      {
         char szOldColor[ CLR_STRLEN ];

         hb_gtGetColorStr( szOldColor );
         hb_gtSetColorStr( hb_parc( 4 ) );

         hb_out( 3, hb_dispout );

         hb_gtSetColorStr( szOldColor );
      }
      else
         hb_out( 3, hb_dispout );
#else
      hb_out( 3, hb_devout );
#endif
   }
}

HARBOUR HB___EJECT( void ) /* Ejects the current page from the printer */
{
   if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set.hb_set_printhan != FS_ERROR )
   {
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) "\x0C\x0D", 2 );
      s_uiPRow = s_uiPCol = 0;
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }
}

HARBOUR HB_PROW( void ) /* Returns the current printer row position */
{
   hb_retni( s_uiPRow );
}

HARBOUR HB_PCOL( void ) /* Returns the current printer row position */
{
   hb_retni( s_uiPCol );
}

HARBOUR HB_SETPRC( void ) /* Sets the current printer row and column positions */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      s_uiPRow = ( USHORT ) hb_parni( 1 );
      s_uiPCol = ( USHORT ) hb_parni( 2 );
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
      USHORT uiCount;
      s_iDevRow = iMR;
      for( uiCount = 0; uiCount < s_iDevRow ; uiCount++ )
         fputs( s_szCrLf, stdout );
      fflush( stdout );
      s_iDevRow = s_iDevCol = 0;
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
#ifdef HARBOUR_USE_GTAPI
   hb_gtGetPos( &s_iDevRow, &s_iDevCol );
#endif
   hb_retni( s_iDevRow );
}

HARBOUR HB_COL( void ) /* Return the current screen column position (zero origin) */
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtGetPos( &s_iDevRow, &s_iDevCol );
#endif
   hb_retni( s_iDevCol );
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
      if( i_bottom < 0 ) bottom = 0; else bottom = ( USHORT ) i_bottom;
      if( i_right < 0 ) right = 0; else right = ( USHORT ) i_right;
      if( top > hb_max_row() ) top = hb_max_row();
      if( left > hb_max_col() ) left = hb_max_col();
      if( bottom > hb_max_row() ) bottom = hb_max_row();
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
         fputc( Borders[ 0 ], stdout );   /* Upper left corner */
      for( col = ( height > 1 ? left + 1 : left ); col < ( height > 1 ? right : right + 1 ); col++ )
         fputc( Borders[ 1 ], stdout );   /* Top line */
      if( height > 1 && width > 1 )
         fputc( Borders[ 2 ], stdout );   /* Upper right corner */
      for( row = ( height > 1 ? top + 1 : top ); row < ( width > 1 ? bottom : bottom + 1 ); row++ )
      {
         hb_setpos( row, left );
         if( height > 1 )
            fputc( Borders[ 3 ], stdout ); /* Left side */
         if( height > 1 && width > 1 ) for( col = left + 1; col < right; col++ )
            fputc( Borders[ 8 ], stdout ); /* Fill */
         if( height > 1 && width > 1 )
            fputc( Borders[ 7 ], stdout ); /* Right side */
      }
      if( height > 1 && width > 1 )
      {
         hb_setpos( bottom, left );
         col = left;
         fputc( Borders[ 6 ], stdout );    /* Bottom left corner */
         for( col = left + 1; col < right; col++ )
            fputc( Borders[ 5 ], stdout ); /* Bottom line */
         fputc( Borders[ 4 ], stdout );    /* Bottom right corner */
      }
      fflush( stdout );
      hb_setpos( bottom + 1, right + 1 );
   }
#endif
}

HARBOUR HB_DISPBEGIN( void )
{
   hb_gtDispBegin();
}

HARBOUR HB_DISPEND( void )
{
   hb_gtDispEnd();
}

HARBOUR HB_DISPCOUNT( void )
{
   hb_retni( hb_gtDispCount() );
}

HARBOUR HB_ISCOLOR( void )
{
   hb_retl( hb_gtIsColor() );
}

HARBOUR HB_NOSNOW( void )
{
   if( ISLOG( 1 ) )
      hb_gtSetSnowFlag( hb_parl( 1 ) );
}

HARBOUR HB___SHADOW( void )
{
   if( hb_pcount() >= 4 )
   {
      hb_gtDrawShadow( hb_parni( 1 ),
                       hb_parni( 2 ),
                       hb_parni( 3 ),
                       hb_parni( 4 ),
                       ISNUM( 5 ) ? hb_parni( 5 ) : 7 );
   }
}

HARBOUR HB_DBGSHADOW( void )
{
   HB___SHADOW();
}

HARBOUR HB_SAVESCREEN( void )
{
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
   hb_retclen( ( char * ) pBuffer, uiX );
   hb_xfree( ( char * ) pBuffer );
}

HARBOUR HB_RESTSCREEN( void )
{
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
}

USHORT hb_setCursor( BOOL bSetCursor, USHORT usNewCursor )
{
   USHORT usPreviousCursor;

   HB_TRACE(HB_TR_DEBUG, ("hb_setCursor(%d, %hu)", (int) bSetCursor, usNewCursor));

   hb_gtGetCursor( &usPreviousCursor );
   if( bSetCursor )
      hb_gtSetCursor( usNewCursor );

   return usPreviousCursor;
}

HARBOUR HB_SETCURSOR( void )
{
   if( hb_pcount() == 1 && ISNUM( 1 ) )
      hb_retni( hb_setCursor( TRUE, hb_parni( 1 ) ) );
   else
      hb_retni( hb_setCursor( FALSE, 0 ) );
}

HARBOUR HB_SETBLINK( void )
{
   BOOL bPreviousBlink;

   hb_gtGetBlink( &bPreviousBlink );
   if( ISLOG( 1 ) )
      hb_gtSetBlink( hb_parl( 1 ) );

   hb_retl( bPreviousBlink );
}

HARBOUR HB_SETMODE( void )
{
   hb_retl( hb_gtSetMode( ISNUM( 1 ) ? hb_parni( 1 ) : ( hb_gtMaxRow() + 1 ),
                          ISNUM( 2 ) ? hb_parni( 2 ) : ( hb_gtMaxCol() + 1 ) ) == 0 );
}

HARBOUR HB___ACCEPT( void ) /* Internal Clipper function used in ACCEPT command  */
                            /* Basically the simplest Clipper function to        */
                            /* receive data. Parameter : cPrompt. Returns : cRet */
{
   int input;
   ULONG ulLen;

   if( hb_pcount() >= 1 )          /* cPrompt passed                         */
      HB_QOUT();

   ulLen = 0;
   input = 0;
   while( input != K_ENTER )
   {
      /* Wait forever, for keyboard events only */
      input = hb_inkey( 0.0, ( HB_inkey_enum ) INKEY_KEYBOARD, 1, 1 );
      switch( input )
      {
         case K_BS:
         case K_LEFT:
            if( ulLen > 0 )
            {
               ulLen--; /* Adjust input count to get rid of last character,
                         then erase it from the screen. */
#ifdef HARBOUR_USE_GTAPI
               hb_gtWriteCon( ( BYTE * ) "\x8 \x8", 3L );
#else
               fputs( "\x8 \x8", stdout );
               fflush( stdout );
#endif
            }
            break;

         default:
            if( ulLen < ( ACCEPT_BUFFER_LEN - 1 ) && input >= 32 )
            {
               s_szAcceptResult[ ulLen ] = input; /* Accept the input */
               hb_dispout( &s_szAcceptResult[ ulLen ], sizeof( char ) ); /* Then display it */
               ulLen++;  /* Then adjust the input count */
            }
      }
   }
   s_szAcceptResult[ ulLen ] = '\0';
   hb_retc( s_szAcceptResult );
}

HARBOUR HB___ACCEPTSTR( void )
{
   hb_retc( s_szAcceptResult );
}

/* ------------------------------------------------- */
/* Copyright (C) 1999-2000 Victor Szakats <info@szelvesz.hu> */
/* --------------------------------------------------------- */

/*  $DOC$
 *  $FUNCNAME$
 *      hb_ColorIndex()
 *  $CATEGORY$
 *      GT
 *  $ONELINER$
 *      Extract one color from a full Clipper colorspec string.
 *  $SYNTAX$
 *      hb_ColorIndex( <cColorSpec>, <nIndex> )
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
 *      ? hb_ColorIndex( "W/N, N/W", CLR_ENHANCED ) // "N/W"
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

HARBOUR HB_HB_COLORINDEX( void )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) )
   {
      char * szColor = hb_parc( 1 );
      ULONG ulColorPos;
      ULONG ulColorLen;
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

