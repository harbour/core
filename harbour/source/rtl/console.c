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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_altout(), hb_devout(), HB_DEVOUT(), hb_devpos(),
 *    HB_DEVPOS(), hb_dispout(), HB___EJECT(), 
 *    hb_out(), hb_outerr(), HB_OUTERR(),
 *    hb_outstd(), HB_OUTSTD(), HB_PCOL(), HB_PROW(),
 *    HB_SETPRC(), and hb_consoleInitialize()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_consoleGetNewLine()
 *    HB_DISPOUTAT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbset.h"

#if defined(__GNUC__) && ! defined(__MINGW32__)
   #include <unistd.h>
#endif
#if !defined(OS_UNIX_COMPATIBLE)
   #include <io.h>
#endif

/* length of buffer for CR/LF characters */
#if defined(OS_UNIX_COMPATIBLE)
   #define CRLF_BUFFER_LEN 2
#else
   #define CRLF_BUFFER_LEN 3
#endif

static BOOL   s_bInit = FALSE;
static USHORT s_uiPRow;
static USHORT s_uiPCol;
static char   s_szCrLf[ CRLF_BUFFER_LEN ];
static int    s_iFilenoStdin;
static int    s_iFilenoStdout;
static int    s_iFilenoStderr;

void hb_consoleInitialize( void )
{
   int iStderr;

   HB_TRACE(HB_TR_DEBUG, ("hb_consoleInitialize()"));

#if defined(OS_UNIX_COMPATIBLE)
   s_szCrLf[ 0 ] = HB_CHAR_LF;
   s_szCrLf[ 1 ] = '\0';
#else
   s_szCrLf[ 0 ] = HB_CHAR_CR;
   s_szCrLf[ 1 ] = HB_CHAR_LF;
   s_szCrLf[ 2 ] = '\0';
#endif

   s_uiPRow = s_uiPCol = 0;

   /* Some compilers open stdout and stderr in text mode, but
      Harbour needs them to be open in binary mode. */

   s_iFilenoStdin = fileno( stdin );
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

   s_bInit = TRUE;

   hb_mouseInit();
   hb_gtInit( s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr );
}

void hb_consoleRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_consoleRelease()"));

   hb_fsSetDevMode( s_iFilenoStdout, FD_TEXT );
   hb_fsSetDevMode( s_iFilenoStderr, FD_TEXT );

   /* The is done by the OS from now on */
   s_szCrLf[ 0 ] = HB_CHAR_LF;
   s_szCrLf[ 1 ] = '\0';

   hb_gtExit();

   s_bInit = FALSE;
}

char * hb_consoleGetNewLine( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_consoleGetNewLine()"));

   return s_szCrLf;
}

HARBOUR HB_HB_OSNEWLINE( void )
{
   hb_retc( s_szCrLf );
}

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

   if( s_bInit )
      hb_gtPreExt();

   user_ferror = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( user_ferror ); /* Restore last user file error code */

   if( s_bInit )
   {
      hb_gtAdjustPos( s_iFilenoStdout, pStr, ulLen );
      hb_gtPostExt();
   }
}

/* Output an item to STDERR */
void hb_outerr( char * pStr, ULONG ulLen )
{
   USHORT user_ferror;

   HB_TRACE(HB_TR_DEBUG, ("hb_outerr(%s, %lu)", pStr, ulLen));

   if( ulLen == 0 )
      ulLen = strlen( pStr );

   if( s_bInit )
      hb_gtPreExt();

   user_ferror = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStderr, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( user_ferror ); /* Restore last user file error code */

   if( s_bInit )
   {
      hb_gtAdjustPos( s_iFilenoStderr, pStr, ulLen );
      hb_gtPostExt();
   }
}

/* Output an item to the screen and/or printer and/or alternate */
static void hb_altout( char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_altout(%s, %lu)", pStr, ulLen));

   if( hb_set.HB_SET_CONSOLE )
      hb_gtWriteCon( ( BYTE * ) pStr, ulLen );

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
      s_uiPCol += ( USHORT ) ulLen;
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
      s_uiPCol += ( USHORT ) ulLen;
   }
   else
   {
      /* Otherwise, display to console */
      hb_gtWrite( ( BYTE * ) pStr, ulLen );
   }
}

/* Output an item to the screen */
static void hb_dispout( char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dispout(%s, %lu)", pStr, ulLen));
   hb_gtWrite( ( BYTE * ) pStr, ulLen );
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
      hb_gtSetPos( row, col );
}

/* NOTE: This should be placed after the hb_devpos() definition. */

HARBOUR HB_DEVPOS( void ) /* Sets the screen and/or printer position */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_devpos( hb_parni( 1 ), hb_parni( 2 ) );
}

HARBOUR HB_OUTSTD( void ) /* writes a list of values to the standard output device */
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_out( uiParam, hb_outstd );
      if( uiParam < uiPCount )
         hb_outstd( " ", 1 );
   }
}

HARBOUR HB_OUTERR( void ) /* writes a list of values to the standard error device */
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_out( uiParam, hb_outerr );
      if( uiParam < uiPCount )
         hb_outerr( " ", 1 );
   }
}

HARBOUR HB_QQOUT( void ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_out( uiParam, hb_altout );
      if( uiParam < uiPCount )
         hb_altout( " ", 1 );
   }
}

HARBOUR HB_QOUT( void )
{
   hb_altout( s_szCrLf, CRLF_BUFFER_LEN - 1 );

   if( hb_set.HB_SET_PRINTER && hb_set.hb_set_printhan != FS_ERROR )
   {
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      USHORT uiCount;

      s_uiPRow++;
      uiCount = s_uiPCol = hb_set.HB_SET_MARGIN;
      while( uiCount-- > 0 )
         hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) " ", 1 );

      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }

   HB_QQOUT();
}

HARBOUR HB_DEVOUT( void ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( hb_pcount() >= 1 )
   {
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
   }
}

/* TOFIX: CA-Cl*pper will print an eject even if SET DEVICE=SCREEN */

HARBOUR HB___EJECT( void ) /* Ejects the current page from the printer */
{
   if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set.hb_set_printhan != FS_ERROR )
   {
      USHORT user_ferror = hb_fsError(); /* Save current user file error code */
      hb_fsWrite( hb_set.hb_set_printhan, ( BYTE * ) "\x0C\x0D", 2 );
      hb_fsSetError( user_ferror ); /* Restore last user file error code */
   }

   s_uiPRow = s_uiPCol = 0;
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

HARBOUR HB_DISPOUT( void ) /* writes a single value to the screen, but is not affected by SET ALTERNATE */
{
   if( hb_pcount() >= 1 )
   {
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
   }
}

/* Undocumented Clipper function */

HARBOUR HB_DISPOUTAT( void ) /* writes a single value to the screen at speficic position, but is not affected by SET ALTERNATE */
{
   if( hb_pcount() >= 3 )
   {
      /* NOTE: Clipper does no checks here. [vszakats] */
      hb_gtSetPos( hb_parni( 1 ), hb_parni( 2 ) );

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
   }
}

