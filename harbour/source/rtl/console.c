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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_conOutAlt(), hb_conOutDev(), DEVOUT(), hb_conDevPos(),
 *    DEVPOS(), __EJECT(),
 *    hb_conOut(), hb_conOutErr(), OUTERR(),
 *    hb_conOutStd(), OUTSTD(), PCOL(), PROW(),
 *    SETPRC(), and hb_conInit()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_conNewLine()
 *    DISPOUTAT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbset.h"
#include "hb_io.h"

/* NOTE: Some C compilers like BCC32 optimize the call of small static buffers
 *       into an integer to read it faster. Later, programs like CodeGuard
 *       complain if the given buffer was smaller than an int. [ckedem]
 */

/* length of buffer for CR/LF characters */
#if !defined(HB_OS_EOL_LEN) || HB_OS_EOL_LEN < 4
#  define CRLF_BUFFER_LEN   4
#else
#  define CRLF_BUFFER_LEN   HB_OS_EOL_LEN + 1
#endif

#if defined(HB_OS_UNIX_COMPATIBLE) && !defined(HB_EOL_CRLF)
   static const char s_szCrLf[ CRLF_BUFFER_LEN ] = { HB_CHAR_LF, 0 };
   static const int  s_iCrLfLen = 1;
#else
   static const char s_szCrLf[ CRLF_BUFFER_LEN ] = { HB_CHAR_CR, HB_CHAR_LF, 0 };
   static const int  s_iCrLfLen = 2;
#endif

static HB_FHANDLE s_hFilenoStdin  = ( HB_FHANDLE ) 0;
static HB_FHANDLE s_hFilenoStdout = ( HB_FHANDLE ) 1;
static HB_FHANDLE s_hFilenoStderr = ( HB_FHANDLE ) 2;

static USHORT  s_uiPRow;
static USHORT  s_uiPCol;

void hb_conInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conInit()"));

#if !defined( HB_WIN32_IO )
   /* when HB_WIN32_IO is set file handles with numbers 0, 1, 2 are
      transalted inside filesys to:
      GetStdHandle( STD_INPUT_HANDLE ), GetStdHandle( STD_OUTPUT_HANDLE ),
      GetStdHandle( STD_ERROR_HANDLE ) */

   s_hFilenoStdin  = fileno( stdin );
   s_hFilenoStdout = fileno( stdout );
   s_hFilenoStderr = fileno( stderr );

#endif

#ifdef HB_C52_UNDOC
   {
      /* Undocumented CA-Cl*pper switch //STDERR:x */
      int iStderr = hb_cmdargNum( "STDERR" );

      if( iStderr == 0 || iStderr == 1 )  /* //STDERR with no parameter or 0 */
         s_hFilenoStderr = s_hFilenoStdout;
      /* disabled in default builds. It's not multiplatform and very
       * dangerous because it can redirect error messages to data files
       * [druzus]
       */
#ifdef HB_C52_STRICT
      else if( iStderr > 0 ) /* //STDERR:x */
         s_hFilenoStderr = ( HB_FHANDLE ) iStderr;
#endif
   }
#endif

   /*
    * Some compilers open stdout and stderr in text mode, but
    * Harbour needs them to be open in binary mode.
    */
   hb_fsSetDevMode( s_hFilenoStdout, FD_BINARY );
   hb_fsSetDevMode( s_hFilenoStderr, FD_BINARY );

   s_uiPRow = s_uiPCol = 0;

   hb_gtInit( s_hFilenoStdin, s_hFilenoStdout, s_hFilenoStderr );
   hb_setkeyInit();  /* April White, May 6, 2000 */
}

void hb_conRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conRelease()"));

   /*
    * Clipper does not restore screen size on exit so I removed the code with:
    *    hb_gtSetMode( s_originalMaxRow + 1, s_originalMaxCol + 1 );
    * If the low level GT drive change some video adapter parameters which
    * have to be restored on exit then it should does it in its Exit()
    * method. Here we cannot force any actions because it may cause bad
    * results in some GTs, f.e. when the screen size is controlled by remote
    * user and not Harbour application (some terminal modes), [Druzus]
    */

   hb_setkeyExit();  /* April White, May 6, 2000 */
   hb_conXSaveRestRelease();

   hb_gtExit();

   hb_fsSetDevMode( s_hFilenoStdout, FD_TEXT );
   hb_fsSetDevMode( s_hFilenoStderr, FD_TEXT );
}

char * hb_conNewLine( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conNewLine()"));

   return ( char * ) s_szCrLf;
}

HB_FUNC( HB_OSNEWLINE )
{
   hb_retc( s_szCrLf );
}

/* Output an item to STDOUT */
void hb_conOutStd( const char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conOutStd(%s, %lu)", pStr, ulLen));

   if( ulLen == 0 )
      ulLen = strlen( pStr );

   if( ulLen > 0 )
      hb_gtOutStd( ( BYTE * ) pStr, ulLen );
}

/* Output an item to STDERR */
void hb_conOutErr( const char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conOutErr(%s, %lu)", pStr, ulLen));

   if( ulLen == 0 )
      ulLen = strlen( pStr );

   if( ulLen > 0 )
      hb_gtOutErr( ( BYTE * ) pStr, ulLen );
}

/* Output an item to the screen and/or printer and/or alternate */
void hb_conOutAlt( const char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conOutAlt(%s, %lu)", pStr, ulLen));

   if( hb_set.HB_SET_CONSOLE )
      hb_gtWriteCon( ( BYTE * ) pStr, ulLen );

   if( hb_set.HB_SET_ALTERNATE && hb_set.hb_set_althan != FS_ERROR )
   {
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      hb_fsWriteLarge( hb_set.hb_set_althan, ( BYTE * ) pStr, ulLen );
   }

   if( hb_set.hb_set_extrahan != FS_ERROR )
   {
      /* Print to extra file if valid alternate file */
      hb_fsWriteLarge( hb_set.hb_set_extrahan, ( BYTE * ) pStr, ulLen );
   }

   if( hb_set.HB_SET_PRINTER && hb_set.hb_set_printhan != FS_ERROR )
   {
      /* Print to printer if SET PRINTER ON and valid printer file */
      hb_fsWriteLarge( hb_set.hb_set_printhan, ( BYTE * ) pStr, ulLen );
      s_uiPCol += ( USHORT ) ulLen;
   }
}

/* Output an item to the screen and/or printer */
static void hb_conOutDev( const char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conOutDev(%s, %lu)", pStr, ulLen));

   if( hb_set.hb_set_printhan != FS_ERROR &&
       hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      hb_fsWriteLarge( hb_set.hb_set_printhan, ( BYTE * ) pStr, ulLen );
      s_uiPCol += ( USHORT ) ulLen;
   }
   else
      /* Otherwise, display to console */
      hb_gtWrite( ( BYTE * ) pStr, ulLen );
}

typedef void hb_out_func_typedef( const char *, ULONG );

static char * hb_itemStringCon( PHB_ITEM pItem, ULONG * pulLen, BOOL * pfFreeReq )
{
   /* logical values in device output (not console, stdout or stderr) are
      shown as single letter */
   if( HB_IS_LOGICAL( pItem ) )
   {
      *pulLen = 1;
      *pfFreeReq = FALSE;
      return ( char * ) ( hb_itemGetL( pItem ) ? "T" : "F" );
   }
   return hb_itemString( pItem, pulLen, pfFreeReq );
}

/* Format items for output, then call specified output function */
static void hb_conOut( USHORT uiParam, hb_out_func_typedef * pOutFunc )
{
   char * pszString;
   ULONG ulLen;
   BOOL bFreeReq;
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_conOut(%hu, %p)", uiParam, pOutFunc));

   pItem = hb_param( uiParam, HB_IT_ANY );

   if( pOutFunc == hb_conOutDev )
      pszString = hb_itemStringCon( pItem, &ulLen, &bFreeReq );
   else
      pszString = hb_itemString( pItem, &ulLen, &bFreeReq );

   if( ulLen )
      pOutFunc( pszString, ulLen );

   if( bFreeReq )
      hb_xfree( pszString );
}

HB_FUNC( OUTSTD ) /* writes a list of values to the standard output device */
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_conOut( uiParam, hb_conOutStd );
      if( uiParam < uiPCount )
         hb_conOutStd( " ", 1 );
   }
}

HB_FUNC( OUTERR ) /* writes a list of values to the standard error device */
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_conOut( uiParam, hb_conOutErr );
      if( uiParam < uiPCount )
         hb_conOutErr( " ", 1 );
   }
}

HB_FUNC( QQOUT ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   USHORT uiPCount = hb_pcount();
   USHORT uiParam;

   for( uiParam = 1; uiParam <= uiPCount; uiParam++ )
   {
      hb_conOut( uiParam, hb_conOutAlt );
      if( uiParam < uiPCount )
         hb_conOutAlt( " ", 1 );
   }
}

HB_FUNC( QOUT )
{
   hb_conOutAlt( s_szCrLf, s_iCrLfLen );

   if( hb_set.HB_SET_PRINTER && hb_set.hb_set_printhan != FS_ERROR )
   {
      BYTE buf[ 80 ];

      s_uiPRow++;
      s_uiPCol = hb_set.HB_SET_MARGIN;
      if( s_uiPCol )
      {
         if( s_uiPCol > sizeof( buf ) )
         {
            BYTE * pBuf = ( BYTE * ) hb_xgrab( s_uiPCol );
            memset( pBuf, ' ', s_uiPCol );
            hb_fsWrite( hb_set.hb_set_printhan, pBuf, s_uiPCol );
            hb_xfree( pBuf );
         }
         else
         {
            memset( buf, ' ', s_uiPCol );
            hb_fsWrite( hb_set.hb_set_printhan, buf, s_uiPCol );
         }
      }
   }

   HB_FUNC_EXEC( QQOUT );
}

HB_FUNC( __EJECT ) /* Ejects the current page from the printer */
{
   if( hb_set.hb_set_printhan != FS_ERROR && hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      static const BYTE byEop[ 4 ] = { 0x0C, 0x0D, 0x00, 0x00 }; /* Buffer is 4 bytes to make CodeGuard happy */
      hb_fsWrite( hb_set.hb_set_printhan, byEop, 2 );
   }

   s_uiPRow = s_uiPCol = 0;
}

HB_FUNC( PROW ) /* Returns the current printer row position */
{
   hb_retni( ( int ) s_uiPRow );
}

HB_FUNC( PCOL ) /* Returns the current printer row position */
{
   hb_retni( ( int ) s_uiPCol );
}

static void hb_conDevPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_conDevPos(%hd, %hd)", iRow, iCol));

   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */

   if( hb_set.hb_set_printhan != FS_ERROR &&
       hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
   {
      USHORT uiPRow = ( USHORT ) iRow;
      USHORT uiPCol = ( USHORT ) iCol + hb_set.HB_SET_MARGIN;

      if( s_uiPRow != uiPRow || s_uiPCol != uiPCol )
      {
         BYTE buf[ 256 ];
         int iPtr = 0;

         if( s_uiPRow != uiPRow )
         {
            if( ++s_uiPRow > uiPRow )
            {
               memcpy( &buf[ iPtr ], "\x0C\x0D\x00\x00", 2 );  /* Source buffer is 4 bytes to make CodeGuard happy */
               iPtr += 2;
               s_uiPRow = 0;
            }
            else
            {
               memcpy( &buf[ iPtr ], s_szCrLf, s_iCrLfLen );
               iPtr += s_iCrLfLen;
            }

            while( s_uiPRow < uiPRow )
            {
               if( iPtr + s_iCrLfLen > ( int ) sizeof( buf ) )
               {
                  hb_fsWrite( hb_set.hb_set_printhan, buf, ( USHORT ) iPtr );
                  iPtr = 0;
               }
               memcpy( &buf[ iPtr ], s_szCrLf, s_iCrLfLen );
               iPtr += s_iCrLfLen;
               ++s_uiPRow;
            }
            s_uiPCol = 0;
         }
         else if( s_uiPCol > uiPCol )
         {
            buf[ iPtr++ ] = '\x0D';
            s_uiPCol = 0;
         }

         while( s_uiPCol < uiPCol )
         {
            if( iPtr == ( int ) sizeof( buf ) )
            {
               hb_fsWrite( hb_set.hb_set_printhan, buf, ( USHORT ) iPtr );
               iPtr = 0;
            }
            buf[ iPtr++ ] = ' ';
            ++s_uiPCol;
         }

         if( iPtr )
            hb_fsWrite( hb_set.hb_set_printhan, buf, ( SHORT ) iPtr );
      }
   }
   else
      hb_gtSetPos( iRow, iCol );
}

/* NOTE: This should be placed after the hb_conDevPos() definition. */

HB_FUNC( DEVPOS ) /* Sets the screen and/or printer position */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_conDevPos( ( SHORT ) hb_parni( 1 ), ( SHORT ) hb_parni( 2 ) );
}

HB_FUNC( SETPRC ) /* Sets the current printer row and column positions */
{
   if( hb_pcount() == 2 && ISNUM( 1 ) && ISNUM( 2 ) )
   {
      s_uiPRow = ( USHORT ) hb_parni( 1 );
      s_uiPCol = ( USHORT ) hb_parni( 2 );
   }
}

HB_FUNC( DEVOUT ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( ISCHAR( 2 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 2 ) );

      hb_conOut( 1, hb_conOutDev );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 1 )
      hb_conOut( 1, hb_conOutDev );
}

HB_FUNC( DISPOUT ) /* writes a single value to the screen, but is not affected by SET ALTERNATE */
{
   char * pszString;
   ULONG ulLen;
   BOOL bFreeReq;

   if( ISCHAR( 2 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 2 ) );

      pszString = hb_itemStringCon( hb_param( 1, HB_IT_ANY ), &ulLen, &bFreeReq );

      hb_gtWrite( ( BYTE * ) pszString, ulLen );

      if( bFreeReq )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 1 )
   {
      pszString = hb_itemStringCon( hb_param( 1, HB_IT_ANY ), &ulLen, &bFreeReq );

      hb_gtWrite( ( BYTE * ) pszString, ulLen );

      if( bFreeReq )
         hb_xfree( pszString );
   }
}

/* Undocumented Clipper function */

/* NOTE: Clipper does no checks about the screen positions. [vszakats] */

HB_FUNC( DISPOUTAT ) /* writes a single value to the screen at speficic position, but is not affected by SET ALTERNATE */
{
   char * pszString;
   ULONG ulLen;
   BOOL bFreeReq;

   if( ISCHAR( 4 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 4 ) );

      pszString = hb_itemStringCon( hb_param( 3, HB_IT_ANY ), &ulLen, &bFreeReq );

      hb_gtWriteAt( ( USHORT ) hb_parni( 1 ), ( USHORT ) hb_parni( 2 ), ( BYTE * ) pszString, ulLen );

      if( bFreeReq )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 3 )
   {
      pszString = hb_itemStringCon( hb_param( 3, HB_IT_ANY ), &ulLen, &bFreeReq );

      hb_gtWriteAt( ( USHORT ) hb_parni( 1 ), ( USHORT ) hb_parni( 2 ), ( BYTE * ) pszString, ulLen );

      if( bFreeReq )
         hb_xfree( pszString );
   }
}


HB_FUNC( HB_GETSTDIN ) /* Return Handel for STDIN */
{
   hb_retnint( ( HB_NHANDLE ) s_hFilenoStdin );
}

HB_FUNC( HB_GETSTDOUT ) /* Return Handel for STDOUT */
{
   hb_retnint( ( HB_NHANDLE ) s_hFilenoStdout );
}

HB_FUNC( HB_GETSTDERR ) /* Return Handel for STDERR */
{
   hb_retnint( ( HB_NHANDLE ) s_hFilenoStderr );
}
