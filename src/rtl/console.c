/*
 * Harbour Project source code:
 * The Console API
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
 * www - http://harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_conOutAlt(), hb_conOutDev(), DevOut(), hb_conDevPos(),
 *    DevPos(), __Eject(),
 *    hb_conOut(), hb_conOutErr(), OutErr(),
 *    hb_conOutStd(), OutStd(), PCol(), PRow(),
 *    SetPRC(), and hb_conInit()
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *    hb_conNewLine()
 *    DispOutAt()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapigt.h"
#include "hbstack.h"
#include "hbset.h"
#include "hb_io.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
#endif

/* NOTE: Some C compilers like Borland C optimize the call of small static buffers
 *       into an integer to read it faster. Later, programs like CodeGuard
 *       complain if the given buffer was smaller than an int. [ckedem]
 */

/* length of buffer for CR/LF characters */
#if ! defined( HB_OS_EOL_LEN ) || HB_OS_EOL_LEN < 4
#  define CRLF_BUFFER_LEN  4
#else
#  define CRLF_BUFFER_LEN  HB_OS_EOL_LEN + 1
#endif

#if defined( HB_OS_UNIX ) && ! defined( HB_EOL_CRLF )
   static const char s_szCrLf[ CRLF_BUFFER_LEN ] = { HB_CHAR_LF, 0 };
   static const int  s_iCrLfLen = 1;
#else
   static const char s_szCrLf[ CRLF_BUFFER_LEN ] = { HB_CHAR_CR, HB_CHAR_LF, 0 };
   static const int  s_iCrLfLen = 2;
#endif

static HB_FHANDLE s_hFilenoStdin  = ( HB_FHANDLE ) HB_STDIN_HANDLE;
static HB_FHANDLE s_hFilenoStdout = ( HB_FHANDLE ) HB_STDOUT_HANDLE;
static HB_FHANDLE s_hFilenoStderr = ( HB_FHANDLE ) HB_STDERR_HANDLE;

typedef struct
{
   int row;
   int col;
} HB_PRNPOS, * PHB_PRNPOS;

static HB_TSD_NEW( s_prnPos, sizeof( HB_PRNPOS ), NULL, NULL );

static PHB_PRNPOS hb_prnPos( void )
{
   return ( PHB_PRNPOS ) hb_stackGetTSD( &s_prnPos );
}

void hb_conInit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_conInit()" ) );

#if ! defined( HB_OS_WIN )
   /* On Windows file handles with numbers 0, 1, 2 are
      transalted inside filesys to:
      GetStdHandle( STD_INPUT_HANDLE ), GetStdHandle( STD_OUTPUT_HANDLE ),
      GetStdHandle( STD_ERROR_HANDLE ) */

   s_hFilenoStdin  = fileno( stdin );
   s_hFilenoStdout = fileno( stdout );
   s_hFilenoStderr = fileno( stderr );

#endif

#ifdef HB_CLP_UNDOC
   {
      /* Undocumented CA-Cl*pper switch //STDERR:x */
      int iStderr = hb_cmdargNum( "STDERR" );

      if( iStderr == 0 || iStderr == 1 )  /* //STDERR with no parameter or 0 */
         s_hFilenoStderr = s_hFilenoStdout;
      /* disabled in default builds. It's not multiplatform and very
       * dangerous because it can redirect error messages to data files
       * [druzus]
       */
#ifdef HB_CLP_STRICT
      else if( iStderr > 0 ) /* //STDERR:x */
         s_hFilenoStderr = ( HB_FHANDLE ) iStderr;
#endif
   }
#endif

   /*
    * Some compilers open stdout and stderr in text mode, but
    * Harbour needs them to be open in binary mode.
    */
   hb_fsSetDevMode( s_hFilenoStdin,  FD_BINARY );
   hb_fsSetDevMode( s_hFilenoStdout, FD_BINARY );
   hb_fsSetDevMode( s_hFilenoStderr, FD_BINARY );

   if( hb_gtInit( s_hFilenoStdin, s_hFilenoStdout, s_hFilenoStderr ) != HB_SUCCESS )
      hb_errInternal( 9995, "Harbour terminal (GT) initialization failure", NULL, NULL );

   if( hb_cmdargCheck( "INFO" ) )
   {
      hb_conOutErr( hb_gtVersion( 1 ), 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }
}

void hb_conRelease( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_conRelease()" ) );

   /*
    * Clipper does not restore screen size on exit so I removed the code with:
    *    hb_gtSetMode( s_originalMaxRow + 1, s_originalMaxCol + 1 );
    * If the low level GT drive change some video adapter parameters which
    * have to be restored on exit then it should does it in its Exit()
    * method. Here we cannot force any actions because it may cause bad
    * results in some GTs, f.e. when the screen size is controlled by remote
    * user and not Harbour application (some terminal modes), [Druzus]
    */

   hb_gtExit();

   hb_fsSetDevMode( s_hFilenoStdin,  FD_TEXT );
   hb_fsSetDevMode( s_hFilenoStdout, FD_TEXT );
   hb_fsSetDevMode( s_hFilenoStderr, FD_TEXT );
}

const char * hb_conNewLine( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_conNewLine()" ) );

   return s_szCrLf;
}

HB_FUNC( HB_EOL )
{
   hb_retc_const( s_szCrLf );
}

#if defined( HB_LEGACY_LEVEL4 )

/* Deprecated */
HB_FUNC( HB_OSNEWLINE )
{
   hb_retc_const( s_szCrLf );
}

#endif

/* Output an item to STDOUT */
void hb_conOutStd( const char * szStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_conOutStd(%s, %" HB_PFS "u)", szStr, nLen ) );

   if( nLen == 0 )
      nLen = strlen( szStr );

   if( nLen > 0 )
      hb_gtOutStd( szStr, nLen );
}

/* Output an item to STDERR */
void hb_conOutErr( const char * szStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_conOutErr(%s, %" HB_PFS "u)", szStr, nLen ) );

   if( nLen == 0 )
      nLen = strlen( szStr );

   if( nLen > 0 )
      hb_gtOutErr( szStr, nLen );
}

/* Output an item to the screen and/or printer and/or alternate */
void hb_conOutAlt( const char * szStr, HB_SIZE nLen )
{
   HB_FHANDLE hFile;

   HB_TRACE( HB_TR_DEBUG, ( "hb_conOutAlt(%s, %" HB_PFS "u)", szStr, nLen ) );

   if( hb_setGetConsole() )
      hb_gtWriteCon( szStr, nLen );

   if( hb_setGetAlternate() && ( hFile = hb_setGetAltHan() ) != FS_ERROR )
   {
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      hb_fsWriteLarge( hFile, szStr, nLen );
   }

   if( ( hFile = hb_setGetExtraHan() ) != FS_ERROR )
   {
      /* Print to extra file if valid alternate file */
      hb_fsWriteLarge( hFile, szStr, nLen );
   }

   if( ( hFile = hb_setGetPrinterHandle( HB_SET_PRN_CON ) ) != FS_ERROR )
   {
      /* Print to printer if SET PRINTER ON and valid printer file */
      hb_fsWriteLarge( hFile, szStr, nLen );
      hb_prnPos()->col += ( int ) nLen;
   }
}

/* Output an item to the screen and/or printer */
static void hb_conOutDev( const char * szStr, HB_SIZE nLen )
{
   HB_FHANDLE hFile;

   HB_TRACE( HB_TR_DEBUG, ( "hb_conOutDev(%s, %" HB_PFS "u)", szStr, nLen ) );

   if( ( hFile = hb_setGetPrinterHandle( HB_SET_PRN_DEV ) ) != FS_ERROR )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      hb_fsWriteLarge( hFile, szStr, nLen );
      hb_prnPos()->col += ( int ) nLen;
   }
   else
      /* Otherwise, display to console */
      hb_gtWrite( szStr, nLen );
}

static char * hb_itemStringCon( PHB_ITEM pItem, HB_SIZE * pnLen, HB_BOOL * pfFreeReq )
{
   /* logical values in device output (not console, stdout or stderr) are
      shown as single letter */
   if( HB_IS_LOGICAL( pItem ) )
   {
      *pnLen = 1;
      *pfFreeReq = HB_FALSE;
      return ( char * ) ( hb_itemGetL( pItem ) ? "T" : "F" );
   }
   return hb_itemString( pItem, pnLen, pfFreeReq );
}

HB_FUNC( OUTSTD ) /* writes a list of values to the standard output device */
{
   int iPCount = hb_pcount(), iParam;

   for( iParam = 1; iParam <= iPCount; iParam++ )
   {
      char * pszString;
      HB_SIZE nLen;
      HB_BOOL fFree;

      if( iParam > 1 )
         hb_conOutStd( " ", 1 );
      pszString = hb_itemString( hb_param( iParam, HB_IT_ANY ), &nLen, &fFree );
      if( nLen )
         hb_conOutStd( pszString, nLen );
      if( fFree )
         hb_xfree( pszString );
   }
}

HB_FUNC( OUTERR ) /* writes a list of values to the standard error device */
{
   int iPCount = hb_pcount(), iParam;

   for( iParam = 1; iParam <= iPCount; iParam++ )
   {
      char * pszString;
      HB_SIZE nLen;
      HB_BOOL fFree;

      if( iParam > 1 )
         hb_conOutErr( " ", 1 );
      pszString = hb_itemString( hb_param( iParam, HB_IT_ANY ), &nLen, &fFree );
      if( nLen )
         hb_conOutErr( pszString, nLen );
      if( fFree )
         hb_xfree( pszString );
   }
}

HB_FUNC( QQOUT ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   int iPCount = hb_pcount(), iParam;

   for( iParam = 1; iParam <= iPCount; iParam++ )
   {
      char * pszString;
      HB_SIZE nLen;
      HB_BOOL fFree;

      if( iParam > 1 )
         hb_conOutAlt( " ", 1 );
      pszString = hb_itemString( hb_param( iParam, HB_IT_ANY ), &nLen, &fFree );
      if( nLen )
         hb_conOutAlt( pszString, nLen );
      if( fFree )
         hb_xfree( pszString );
   }
}

HB_FUNC( QOUT )
{
   HB_FHANDLE hFile;

   hb_conOutAlt( s_szCrLf, s_iCrLfLen );

   if( ( hFile = hb_setGetPrinterHandle( HB_SET_PRN_CON ) ) != FS_ERROR )
   {
      char buf[ 256 ];
      PHB_PRNPOS pPrnPos = hb_prnPos();

      pPrnPos->row++;
      pPrnPos->col = hb_setGetMargin();

      if( pPrnPos->col )
      {
         if( pPrnPos->col > ( int ) sizeof( buf ) )
         {
            char * pBuf = ( char * ) hb_xgrab( pPrnPos->col );
            memset( pBuf, ' ', pPrnPos->col );
            hb_fsWrite( hFile, pBuf, ( HB_USHORT ) pPrnPos->col );
            hb_xfree( pBuf );
         }
         else
         {
            memset( buf, ' ', pPrnPos->col );
            hb_fsWrite( hFile, buf, ( HB_USHORT ) pPrnPos->col );
         }
      }
   }

   HB_FUNC_EXEC( QQOUT );
}

HB_FUNC( __EJECT ) /* Ejects the current page from the printer */
{
   PHB_PRNPOS pPrnPos;
   HB_FHANDLE hFile;

   if( ( hFile = hb_setGetPrinterHandle( HB_SET_PRN_ANY ) ) != FS_ERROR )
   {
      static const char s_szEop[ 4 ] = { 0x0C, 0x0D, 0x00, 0x00 }; /* Buffer is 4 bytes to make CodeGuard happy */
      hb_fsWrite( hFile, s_szEop, 2 );
   }

   pPrnPos = hb_prnPos();
   pPrnPos->row = pPrnPos->col = 0;
}

HB_FUNC( PROW ) /* Returns the current printer row position */
{
   hb_retni( ( int ) hb_prnPos()->row );
}

HB_FUNC( PCOL ) /* Returns the current printer row position */
{
   hb_retni( ( int ) hb_prnPos()->col );
}

static void hb_conDevPos( int iRow, int iCol )
{
   HB_FHANDLE hFile;

   HB_TRACE( HB_TR_DEBUG, ( "hb_conDevPos(%d, %d)", iRow, iCol ) );

   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */

   if( ( hFile = hb_setGetPrinterHandle( HB_SET_PRN_DEV ) ) != FS_ERROR )
   {
      int iPRow = iRow;
      int iPCol = iCol + hb_setGetMargin();
      PHB_PRNPOS pPrnPos = hb_prnPos();

      if( pPrnPos->row != iPRow || pPrnPos->col != iPCol )
      {
         char buf[ 256 ];
         int iPtr = 0;

         if( pPrnPos->row != iPRow )
         {
            if( ++pPrnPos->row > iPRow )
            {
               memcpy( &buf[ iPtr ], "\x0C\x0D\x00\x00", 2 );  /* Source buffer is 4 bytes to make CodeGuard happy */
               iPtr += 2;
               pPrnPos->row = 0;
            }
            else
            {
               memcpy( &buf[ iPtr ], s_szCrLf, s_iCrLfLen );
               iPtr += s_iCrLfLen;
            }

            while( pPrnPos->row < iPRow )
            {
               if( iPtr + s_iCrLfLen > ( int ) sizeof( buf ) )
               {
                  hb_fsWrite( hFile, buf, ( HB_USHORT ) iPtr );
                  iPtr = 0;
               }
               memcpy( &buf[ iPtr ], s_szCrLf, s_iCrLfLen );
               iPtr += s_iCrLfLen;
               ++pPrnPos->row;
            }
            pPrnPos->col = 0;
         }
         else if( pPrnPos->col > iPCol )
         {
            buf[ iPtr++ ] = '\x0D';
            pPrnPos->col = 0;
         }

         while( pPrnPos->col < iPCol )
         {
            if( iPtr == ( int ) sizeof( buf ) )
            {
               hb_fsWrite( hFile, buf, ( HB_USHORT ) iPtr );
               iPtr = 0;
            }
            buf[ iPtr++ ] = ' ';
            ++pPrnPos->col;
         }

         if( iPtr )
            hb_fsWrite( hFile, buf, ( HB_USHORT ) iPtr );
      }
   }
   else
      hb_gtSetPos( iRow, iCol );
}

/* NOTE: This should be placed after the hb_conDevPos() definition. */

HB_FUNC( DEVPOS ) /* Sets the screen and/or printer position */
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
      hb_conDevPos( hb_parni( 1 ), hb_parni( 2 ) );

#if defined( HB_CLP_UNDOC )
   /* NOTE: Both 5.2e and 5.3 does that, while the documentation
            says it will return NIL. [vszakats] */
   hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
#endif
}

HB_FUNC( SETPRC ) /* Sets the current printer row and column positions */
{
   if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_PRNPOS pPrnPos = hb_prnPos();
      pPrnPos->row = hb_parni( 1 );
      pPrnPos->col = hb_parni( 2 );
   }
}

HB_FUNC( DEVOUT ) /* writes a single value to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   char * pszString;
   HB_SIZE nLen;
   HB_BOOL fFree;

   if( HB_ISCHAR( 2 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 2 ) );

      pszString = hb_itemStringCon( hb_param( 1, HB_IT_ANY ), &nLen, &fFree );
      if( nLen )
         hb_conOutDev( pszString, nLen );
      if( fFree )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 1 )
   {
      pszString = hb_itemStringCon( hb_param( 1, HB_IT_ANY ), &nLen, &fFree );
      if( nLen )
         hb_conOutDev( pszString, nLen );
      if( fFree )
         hb_xfree( pszString );
   }
}

HB_FUNC( DISPOUT ) /* writes a single value to the screen, but is not affected by SET ALTERNATE */
{
   char * pszString;
   HB_SIZE nLen;
   HB_BOOL bFreeReq;

   if( HB_ISCHAR( 2 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 2 ) );

      pszString = hb_itemStringCon( hb_param( 1, HB_IT_ANY ), &nLen, &bFreeReq );

      hb_gtWrite( pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 1 )
   {
      pszString = hb_itemStringCon( hb_param( 1, HB_IT_ANY ), &nLen, &bFreeReq );

      hb_gtWrite( pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );
   }
}

/* Undocumented Clipper function */

/* NOTE: Clipper does no checks about the screen positions. [vszakats] */

HB_FUNC( DISPOUTAT ) /* writes a single value to the screen at speficic position, but is not affected by SET ALTERNATE */
{
   char * pszString;
   HB_SIZE nLen;
   HB_BOOL bFreeReq;

   if( HB_ISCHAR( 4 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 4 ) );

      pszString = hb_itemStringCon( hb_param( 3, HB_IT_ANY ), &nLen, &bFreeReq );

      hb_gtWriteAt( hb_parni( 1 ), hb_parni( 2 ), pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 3 )
   {
      pszString = hb_itemStringCon( hb_param( 3, HB_IT_ANY ), &nLen, &bFreeReq );

      hb_gtWriteAt( hb_parni( 1 ), hb_parni( 2 ), pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );
   }
}

/* Harbour extension, works like DISPOUTAT but does not change cursor position */

HB_FUNC( HB_DISPOUTAT )
{
   if( hb_pcount() >= 3 )
   {
      char * pszString;
      HB_SIZE nLen;
      HB_BOOL bFreeReq;
      int iColor;

      pszString = hb_itemStringCon( hb_param( 3, HB_IT_ANY ), &nLen, &bFreeReq );

      if( HB_ISCHAR( 4 ) )
         iColor = hb_gtColorToN( hb_parc( 4 ) );
      else if( HB_ISNUM( 4 ) )
         iColor = hb_parni( 4 );
      else
         iColor = -1;

      hb_gtPutText( hb_parni( 1 ), hb_parni( 2 ), pszString, nLen, iColor );

      if( bFreeReq )
         hb_xfree( pszString );
   }
}

/* Same as hb_DispOutAt(), but draws with the attribute HB_GT_ATTR_BOX,
   so we can use it to draw graphical elements. */
HB_FUNC( HB_DISPOUTATBOX )
{
   HB_SIZE nLen = hb_parclen( 3 );

   if( nLen > 0 )
   {
      int iRow = hb_parni( 1 );
      int iCol = hb_parni( 2 );
      const char * pszString = hb_parc( 3 );
      int iColor;
      PHB_CODEPAGE cdp;
      HB_SIZE nIndex = 0;
      HB_WCHAR wc;

      if( HB_ISCHAR( 4 ) )
         iColor = hb_gtColorToN( hb_parc( 4 ) );
      else if( HB_ISNUM( 4 ) )
         iColor = hb_parni( 4 );
      else
         iColor = hb_gtGetCurrColor();

      cdp = hb_gtBoxCP();

      while( HB_CDPCHAR_GET( cdp, pszString, nLen, &nIndex, &wc ) )
         hb_gtPutChar( iRow, iCol++, iColor, HB_GT_ATTR_BOX, wc );

      hb_gtFlush();
   }
}

HB_FUNC( HB_GETSTDIN ) /* Return handle for STDIN */
{
   hb_retnint( ( HB_NHANDLE ) s_hFilenoStdin );
}

HB_FUNC( HB_GETSTDOUT ) /* Return handle for STDOUT */
{
   hb_retnint( ( HB_NHANDLE ) s_hFilenoStdout );
}

HB_FUNC( HB_GETSTDERR ) /* Return handle for STDERR */
{
   hb_retnint( ( HB_NHANDLE ) s_hFilenoStderr );
}
