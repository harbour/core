/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Low level ClipBoard code common to some GT drivers
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


/* NOTE: User programs should never call this layer directly! */

#define HB_OS_WIN_USED
#include "hbgtcore.h"

#if defined( __CYGWIN__ )
   #include <wchar.h>
#elif defined( __POCC__ ) && defined( HB_OS_WIN_CE )
   #ifndef GMEM_MOVEABLE
      #define GMEM_MOVEABLE       2
   #endif
#endif

#include "hbthread.h"

static HB_CRITICAL_NEW( s_clipMtx );

static char *     s_szClipboardData;
static HB_SIZE      s_ulClipboardLen;

BOOL hb_gt_setClipboard( const char * szClipData, HB_SIZE ulLen )
{
   hb_threadEnterCriticalSection( &s_clipMtx );

   if( s_ulClipboardLen )
      hb_xfree( s_szClipboardData );
   s_ulClipboardLen = ulLen;
   if( ulLen )
   {
      s_szClipboardData = ( char * ) hb_xgrab( s_ulClipboardLen + 1 );
      memcpy( s_szClipboardData, szClipData, s_ulClipboardLen );
      s_szClipboardData[ s_ulClipboardLen ] = '\0';
   }

   hb_threadLeaveCriticalSection( &s_clipMtx );

   return TRUE;
}

BOOL hb_gt_getClipboard( char ** pszClipData, HB_SIZE *pulLen )
{
   hb_threadEnterCriticalSection( &s_clipMtx );

   *pszClipData = NULL;
   *pulLen = s_ulClipboardLen;
   if( s_ulClipboardLen )
   {
      *pszClipData = ( char * ) hb_xgrab( s_ulClipboardLen + 1 );
      memcpy( *pszClipData, s_szClipboardData, s_ulClipboardLen );
      ( *pszClipData )[ s_ulClipboardLen ] = '\0';
   }

   hb_threadLeaveCriticalSection( &s_clipMtx );

   return s_ulClipboardLen != 0;
}

#if defined( HB_OS_WIN )

BOOL hb_gt_winapi_setClipboard( UINT uFormat, const char * szClipData, HB_SIZE ulLen )
{
   BOOL fResult = FALSE;

   if( OpenClipboard( NULL ) )
   {
      HGLOBAL hglbCopy;

      EmptyClipboard();

      /* Allocate a global memory object for the text. */
      hglbCopy = GlobalAlloc( GMEM_MOVEABLE, uFormat == CF_UNICODETEXT ? ( ulLen + 1 ) * sizeof( wchar_t ) : ulLen + 1 );
      if( hglbCopy )
      {
         /* Lock the handle and copy the text to the buffer. */
         LPTSTR lptstrCopy = ( LPTSTR ) GlobalLock( hglbCopy );
         if( lptstrCopy )
         {
            if( uFormat == CF_UNICODETEXT )
            {
               hb_mbtowcset( ( LPWSTR ) lptstrCopy, szClipData, ulLen );
               * ( ( ( LPWSTR ) lptstrCopy ) + ulLen ) = L'\0';
            }
            else
            {
               memcpy( lptstrCopy, szClipData, ulLen );
               lptstrCopy[ ulLen ] = '\0';
            }
            fResult = TRUE;
         }
         ( void ) GlobalUnlock( hglbCopy );
         /* Place the handle on the clipboard. */
         SetClipboardData( uFormat, hglbCopy );
      }
      CloseClipboard();
   }
   return fResult;
}

BOOL hb_gt_winapi_getClipboard( UINT uFormat, char ** pszClipData, HB_SIZE * pulLen )
{
   *pulLen = 0;
   *pszClipData = NULL;
   if( IsClipboardFormatAvailable( uFormat ) && OpenClipboard( NULL ) )
   {
      HGLOBAL hglb = GetClipboardData( uFormat );
      if( hglb )
      {
         LPTSTR lptstr = ( LPTSTR ) GlobalLock( hglb );
         if( lptstr )
         {
            switch( uFormat )
            {
               case CF_UNICODETEXT:
                  *pulLen = wcslen( ( LPWSTR ) lptstr );
                  if( *pulLen )
                     *pszClipData = hb_wctomb( ( LPWSTR ) lptstr );
                  break;
               case CF_OEMTEXT:
               case CF_TEXT:
                  *pulLen = strlen( ( char * ) lptstr );
                  if( *pulLen )
                  {
                     *pszClipData = ( char * ) hb_xgrab( *pulLen + 1 );
                     memcpy( *pszClipData, lptstr, *pulLen );
                     ( *pszClipData )[ *pulLen ] = '\0';
                  }
                  break;
               default:
                  *pulLen = GlobalSize( hglb );
                  if( *pulLen )
                  {
                     *pszClipData = ( char * ) hb_xgrab( *pulLen + 1 );
                     memcpy( *pszClipData, lptstr, *pulLen );
                     ( *pszClipData )[ *pulLen ] = '\0';
                  }
                  break;
            }
            ( void ) GlobalUnlock( hglb );
         }
      }
      CloseClipboard();
   }

   return *pulLen != 0;
}

#endif /* HB_OS_WIN */
