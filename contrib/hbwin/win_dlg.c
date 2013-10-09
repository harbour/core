/*
 * Harbour Project source code:
 * Windows dialogs
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 *    win_PrintDlgDC()
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    win_GetOpenFileName(), win_GetSaveFileName()
 *
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

#include "hbwin.h"
#include "hbwapi.h"

#if defined( __XCC__ )
#  include <ole2.h>
#endif
#include <commdlg.h>

/* win_PrintDlgDC( [@<cDevice>], [<nFromPage>], [<nToPage>], [<nCopies>] )
 *                -> <hDC>
 */
HB_FUNC( WIN_PRINTDLGDC )
{
   PRINTDLG pd;

   memset( &pd, 0, sizeof( pd ) );

#if ! defined( HB_OS_WIN_CE )
   pd.lStructSize = sizeof( pd );
   pd.hwndOwner = GetActiveWindow();
   pd.Flags = PD_RETURNDC | PD_USEDEVMODECOPIESANDCOLLATE;
   pd.nFromPage = ( WORD ) hb_parnidef( 2, 1 );
   pd.nToPage = ( WORD ) hb_parnidef( 3, 1 );
   pd.nCopies = ( WORD ) hb_parnidef( 4, 1 );
#endif

   if( PrintDlg( &pd ) )
   {
#if ! defined( HB_OS_WIN_CE )
      if( pd.hDevNames )
      {
         LPDEVNAMES lpdn = ( LPDEVNAMES ) GlobalLock( pd.hDevNames );
         HB_STORSTR( ( LPCTSTR ) lpdn + lpdn->wDeviceOffset, 1 );
         GlobalUnlock( pd.hDevNames );
         GlobalFree( pd.hDevNames );
      }

      if( pd.hDevMode )
         GlobalFree( pd.hDevMode );

      hbwapi_ret_HDC( pd.hDC );
#else
      hb_storc( NULL, 1 );

#if defined( __MINGW32__ ) /* NOTE: mingwarm has the struct names/members wrong. */
      hbwapi_ret_HDC( pd.hDC );
#else
      hbwapi_ret_HDC( pd.hdc );
#endif
#endif
   }
   else
      hb_retptr( NULL );
}

static LPTSTR s_dialogPairs( int iParam, DWORD * pdwIndex )
{
   PHB_ITEM pItem = hb_param( iParam, HB_IT_ARRAY | HB_IT_STRING ), pArrItem;
   LPTSTR lpStr = NULL;
   DWORD dwMaxIndex = 0;

   if( pItem )
   {
      HB_SIZE nLen, nSize, nTotal, n, n1, n2;

      if( HB_IS_ARRAY( pItem ) )
      {
         nSize = hb_arrayLen( pItem );
         for( n = nLen = 0; n < nSize; ++n )
         {
            pArrItem = hb_arrayGetItemPtr( pItem, n + 1 );
            if( HB_IS_STRING( pArrItem ) )
            {
               n1 = HB_ITEMCOPYSTR( pArrItem, NULL, 0 );
               if( n1 )
                  nLen += n1 * 2 + 2;
            }
            else if( hb_arrayLen( pArrItem ) >= 2 )
            {
               n1 = HB_ITEMCOPYSTR( hb_arrayGetItemPtr( pArrItem, 1 ), NULL, 0 );
               n2 = HB_ITEMCOPYSTR( hb_arrayGetItemPtr( pArrItem, 2 ), NULL, 0 );
               if( n1 && n2 )
                  nLen += n1 + n2 + 2;
            }
         }
         if( nLen )
         {
            nTotal = nLen + 1;
            lpStr = ( LPTSTR ) hb_xgrab( nTotal * sizeof( TCHAR ) );
            for( n = nLen = 0; n < nSize; ++n )
            {
               pArrItem = hb_arrayGetItemPtr( pItem, n + 1 );
               if( HB_IS_STRING( pArrItem ) )
               {
                  n1 = HB_ITEMCOPYSTR( pArrItem,
                                       lpStr + nLen, nTotal - nLen );
                  if( n1 )
                  {
                     nLen += n1 + 1;
                     n1 = HB_ITEMCOPYSTR( pArrItem,
                                          lpStr + nLen, nTotal - nLen );
                     nLen += n1 + 1;
                     dwMaxIndex++;
                  }
               }
               else if( hb_arrayLen( pArrItem ) >= 2 )
               {
                  n1 = HB_ITEMCOPYSTR( hb_arrayGetItemPtr( pArrItem, 1 ),
                                       lpStr + nLen, nTotal - nLen );
                  if( n1 )
                  {
                     n2 = HB_ITEMCOPYSTR( hb_arrayGetItemPtr( pArrItem, 2 ),
                                          lpStr + nLen + n1 + 1,
                                          nTotal - nLen - n1 - 1 );
                     if( n2 )
                     {
                        nLen += n1 + n2 + 2;
                        dwMaxIndex++;
                     }
                  }
               }
            }
            lpStr[ nLen ] = 0;
         }
      }
      else
      {
         nLen = HB_ITEMCOPYSTR( pItem, NULL, 0 );
         if( nLen )
         {
            lpStr = ( LPTSTR ) hb_xgrab( ( nLen * 2 + 3 ) * sizeof( TCHAR ) );
            HB_ITEMCOPYSTR( pItem, lpStr, nLen + 1 );
            for( n = n1 = 0; n < nLen; ++n )
            {
               if( lpStr[ n ] == 0 )
               {
                  ++n1;
                  if( lpStr[ n + 1 ] == 0 )
                     break;
               }
            }
            if( n1 == 0 )
            {
               HB_ITEMCOPYSTR( pItem, lpStr + nLen + 1, nLen + 1 );
               lpStr[ nLen * 2 + 2 ] = 0;
               dwMaxIndex = 1;
            }
            else
            {
               if( n == nLen && lpStr[ n - 1 ] != 0 )
               {
                  lpStr[ n + 1 ] = 0;
                  ++n1;
               }
               if( ( n1 & 1 ) == 0 )
                  dwMaxIndex = ( DWORD ) n1;
               else
               {
                  hb_xfree( lpStr );
                  lpStr = NULL;
               }
            }
         }
      }
   }

   if( pdwIndex )
   {
      if( dwMaxIndex < *pdwIndex )
         *pdwIndex = dwMaxIndex;
      else if( dwMaxIndex && *pdwIndex == 0 )
         *pdwIndex = 1;
   }

   return lpStr;
}

static void s_GetFileName( HB_BOOL fSave )
{
   void * hInitDir, * hTitle, * hDefExt;
   LPTSTR lpstrFilter;
   OPENFILENAME ofn;

   memset( &ofn, 0, sizeof( ofn ) );
#if defined( OPENFILENAME_SIZE_VERSION_400 )
   ofn.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
   ofn.lStructSize = sizeof( ofn );
#endif
   ofn.hwndOwner = GetActiveWindow();
   ofn.hInstance = GetModuleHandle( NULL );

   ofn.nFilterIndex     = hbwapi_par_DWORD( 6 );
   ofn.lpstrFilter      = lpstrFilter = s_dialogPairs( 5, &ofn.nFilterIndex );

   ofn.nMaxFile         = hbwapi_par_DWORD( 7 );
   if( ofn.nMaxFile < 0x400 )
      ofn.nMaxFile = ofn.nMaxFile == 0 ? 0x10000 : 0x400;
   ofn.lpstrFile        = ( LPTSTR )
                          memset( hb_xgrab( ofn.nMaxFile * sizeof( TCHAR ) ),
                                  0, ofn.nMaxFile * sizeof( TCHAR ) );

   ofn.lpstrInitialDir  = HB_PARSTR( 3, &hInitDir, NULL );
   ofn.lpstrTitle       = HB_PARSTR( 2, &hTitle, NULL );
   ofn.Flags            = HB_ISNUM( 1 ) ? hbwapi_par_DWORD( 1 ) :
                          ( OFN_EXPLORER | OFN_ALLOWMULTISELECT |
                            OFN_HIDEREADONLY | OFN_NOCHANGEDIR );
   ofn.lpstrDefExt      = HB_PARSTR( 4, &hDefExt, NULL );
   if( ofn.lpstrDefExt && ofn.lpstrDefExt[ 0 ] == '.' )
      ++ofn.lpstrDefExt;

   HB_ITEMCOPYSTR( hb_param( 8, HB_IT_ANY ), ofn.lpstrFile, ofn.nMaxFile );

   if( fSave ? GetSaveFileName( &ofn ) : GetOpenFileName( &ofn ) )
   {
      HB_SIZE nLen;
      for( nLen = 0; nLen < ofn.nMaxFile; ++nLen )
      {
         if( ofn.lpstrFile[ nLen ] == 0 &&
             ( nLen + 1 == ofn.nMaxFile || ofn.lpstrFile[ nLen + 1 ] == 0 ) )
            break;
      }
      hb_stornint( ofn.Flags, 1 );
      hb_stornint( ofn.nFilterIndex, 6 );
      HB_RETSTRLEN( ofn.lpstrFile, nLen );
   }
   else
      hb_retc_null();

   hb_xfree( ofn.lpstrFile );
   if( lpstrFilter )
      hb_xfree( lpstrFilter );

   hb_strfree( hInitDir );
   hb_strfree( hTitle );
   hb_strfree( hDefExt );
}

/* win_GetOpenFileName( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>],;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 *
 */
HB_FUNC( WIN_GETOPENFILENAME )
{
   s_GetFileName( HB_FALSE );
}

/* win_GetSaveFileName( [[@]<nFlags>], [<cTitle>], [<cInitDir>], [<cDefExt>],;
 *                      [<acFilter>], [[@]<nFilterIndex>], [<nBufferSize>], [<cDefName>] )
 *    -> <cFilePath> | <cPath> + e"\0" + <cFile1> [ + e"\0" + <cFileN> ] | ""
 *
 */
HB_FUNC( WIN_GETSAVEFILENAME )
{
   s_GetFileName( HB_TRUE );
}
