/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Low level ClipBoard code common to some GT drivers
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* NOTE: User programs should never call this layer directly! */

#include "hbgtcore.h"
#include "hbapistr.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   /* For Global*() */
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif

   #if defined( __CYGWIN__ )
      #include <wchar.h>
   #elif defined( __POCC__ ) && defined( HB_OS_WIN_CE )
      #ifndef GMEM_MOVEABLE
         #define GMEM_MOVEABLE       2
      #endif
   #endif
#endif

#include "hbthread.h"

static HB_CRITICAL_NEW( s_clipMtx );

static char *     s_szClipboardData;
static HB_SIZE    s_nClipboardLen;

HB_BOOL hb_gt_setClipboard( const char * szClipData, HB_SIZE nLen )
{
   hb_threadEnterCriticalSection( &s_clipMtx );

   if( s_nClipboardLen )
      hb_xfree( s_szClipboardData );
   s_nClipboardLen = nLen;
   if( nLen )
   {
      s_szClipboardData = ( char * ) hb_xgrab( s_nClipboardLen + 1 );
      memcpy( s_szClipboardData, szClipData, s_nClipboardLen );
      s_szClipboardData[ s_nClipboardLen ] = '\0';
   }

   hb_threadLeaveCriticalSection( &s_clipMtx );

   return HB_TRUE;
}

HB_BOOL hb_gt_getClipboard( char ** pszClipData, HB_SIZE * pnLen )
{
   hb_threadEnterCriticalSection( &s_clipMtx );

   *pszClipData = NULL;
   *pnLen = s_nClipboardLen;
   if( s_nClipboardLen )
   {
      *pszClipData = ( char * ) hb_xgrab( s_nClipboardLen + 1 );
      memcpy( *pszClipData, s_szClipboardData, s_nClipboardLen );
      ( *pszClipData )[ s_nClipboardLen ] = '\0';
   }

   hb_threadLeaveCriticalSection( &s_clipMtx );

   return s_nClipboardLen != 0;
}

#if defined( HB_OS_WIN )

HB_BOOL hb_gt_winapi_setClipboardRaw( HB_UINT uFormat, void * pData, HB_SIZE nSize )
{
   HB_BOOL fResult = HB_FALSE;

   if( OpenClipboard( NULL ) )
   {
      EmptyClipboard();

      if( nSize )
      {
         /* Allocate a global memory object for the text. */
         HGLOBAL hglb = GlobalAlloc( GMEM_MOVEABLE, nSize );
         if( hglb )
         {
            /* Lock the handle and copy the text to the buffer. */
            LPVOID lpMem = GlobalLock( hglb );

            if( lpMem )
            {
               memcpy( lpMem, pData, nSize );
               ( void ) GlobalUnlock( hglb );
               /* Place the handle on the clipboard. */
               fResult = SetClipboardData( ( UINT ) uFormat, hglb ) != 0;
            }
            if( !fResult )
               GlobalFree( hglb );
         }
      }
      else
         fResult = HB_TRUE;

      CloseClipboard();
   }
   return fResult;
}

HB_BOOL hb_gt_winapi_setClipboard( HB_UINT uFormat, PHB_ITEM pItem )
{
   HB_BOOL fResult = HB_FALSE;

   if( OpenClipboard( NULL ) )
   {
      HB_SIZE nSize;

      EmptyClipboard();

      if( uFormat == CF_UNICODETEXT )
         nSize = hb_itemCopyStrU16( pItem, HB_CDP_ENDIAN_NATIVE, NULL, 0 );
      else
         nSize = hb_itemCopyStr( pItem, hb_setGetOSCP(), NULL, 0 );

      if( nSize )
      {
         /* Allocate a global memory object for the text. */
         HGLOBAL hglb = GlobalAlloc( GMEM_MOVEABLE, ( nSize + 1 ) *
                                       ( uFormat == CF_UNICODETEXT ?
                                       sizeof( wchar_t ) : sizeof( char ) ) );
         if( hglb )
         {
            /* Lock the handle and copy the text to the buffer. */
            LPVOID lpMem = GlobalLock( hglb );

            if( lpMem )
            {
               if( uFormat == CF_UNICODETEXT )
                  hb_itemCopyStrU16( pItem, HB_CDP_ENDIAN_NATIVE,
                                     ( wchar_t * ) lpMem, nSize + 1 );
               else
                  hb_itemCopyStr( pItem, hb_setGetOSCP(),
                                  ( char * ) lpMem, nSize + 1 );
               ( void ) GlobalUnlock( hglb );
               /* Place the handle on the clipboard. */
               fResult = SetClipboardData( ( UINT ) uFormat, hglb ) != 0;
            }
            if( !fResult )
               GlobalFree( hglb );
         }
      }
      else
         fResult = HB_TRUE;

      CloseClipboard();
   }
   return fResult;
}

HB_BOOL hb_gt_winapi_getClipboard( HB_UINT uFormat, PHB_ITEM pItem )
{
   HB_SIZE nSize = 0;

   if( IsClipboardFormatAvailable( uFormat ) && OpenClipboard( NULL ) )
   {
      HGLOBAL hglb = GetClipboardData( ( UINT ) uFormat );
      if( hglb )
      {
         LPVOID lpMem = GlobalLock( hglb );
         if( lpMem )
         {
            nSize = ( HB_SIZE ) GlobalSize( hglb );

            switch( uFormat )
            {
               case CF_UNICODETEXT:
                  nSize = hb_wstrnlen( ( const wchar_t * ) lpMem, nSize >> 1 );
                  if( nSize )
                     hb_itemPutStrLenU16( pItem, HB_CDP_ENDIAN_NATIVE,
                                          ( const wchar_t * ) lpMem, nSize );
                  break;
               case CF_OEMTEXT:
               case CF_TEXT:
                  nSize = hb_strnlen( ( const char * ) lpMem, nSize );
               default:
                  if( nSize )
                     hb_itemPutStrLen( pItem, uFormat == CF_TEXT ?
                                              hb_setGetOSCP() : NULL,
                                       ( const char * ) lpMem, nSize );
                  break;
            }
            ( void ) GlobalUnlock( hglb );
         }
      }
      CloseClipboard();
   }

   if( nSize == 0 )
      hb_itemPutC( pItem, NULL );

   return nSize != 0;
}

#endif /* HB_OS_WIN */
