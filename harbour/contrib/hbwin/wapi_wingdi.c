/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (wingdi.h) (alpha)
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.hu)
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

/* WinCE MSDN documentation:
      http://msdn.microsoft.com/en-us/library/aa923590.aspx
 */

#include "hbwapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* For Arc() */
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#else
   #include <winspool.h>
#endif

static void s_hb_hashSetCItemNL( PHB_ITEM pHash, const char * pszKey, long v )
{
   PHB_ITEM pKey = hb_itemPutC( NULL, pszKey );
   PHB_ITEM pValue = hb_itemPutNL( NULL, v );

   hb_hashAdd( pHash, pKey, pValue );

   hb_itemRelease( pValue );
   hb_itemRelease( pKey );
}

POINT * hbwapi_par_POINT( POINT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   memset( p, 0, sizeof( POINT ) );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      p->x = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "x" ) );
      p->y = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "y" ) );

      return p;
   }
   else if( pStru && HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 2 )
   {
      p->x = ( LONG ) hb_arrayGetNL( pStru, 1 );
      p->y = ( LONG ) hb_arrayGetNL( pStru, 2 );

      return p;
   }
   else if( bMandatory )
      return p;

   return NULL;
}

void hbwapi_stor_POINT( POINT * p, int iParam )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      s_hb_hashSetCItemNL( pStru, "x", p->x );
      s_hb_hashSetCItemNL( pStru, "y", p->y );
   }
   else if( pStru && HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 2 )
   {
      hb_arraySetNL( pStru, 1, p->x );
      hb_arraySetNL( pStru, 2, p->y );
   }
}

RECT * hbwapi_par_RECT( RECT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   memset( p, 0, sizeof( RECT ) );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      p->left   = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "left"   ) );
      p->top    = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "top"    ) );
      p->right  = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "right"  ) );
      p->bottom = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "bottom" ) );

      return p;
   }
   else if( pStru && HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 4 )
   {
      p->left   = ( LONG ) hb_arrayGetNL( pStru, 1 );
      p->top    = ( LONG ) hb_arrayGetNL( pStru, 2 );
      p->right  = ( LONG ) hb_arrayGetNL( pStru, 3 );
      p->bottom = ( LONG ) hb_arrayGetNL( pStru, 4 );

      return p;
   }
   else if( bMandatory )
      return p;

   return NULL;
}

void hbwapi_stor_RECT( RECT * p, int iParam )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      s_hb_hashSetCItemNL( pStru, "left"  , p->left   );
      s_hb_hashSetCItemNL( pStru, "top"   , p->top    );
      s_hb_hashSetCItemNL( pStru, "right" , p->right  );
      s_hb_hashSetCItemNL( pStru, "bottom", p->bottom );
   }
   else if( pStru && HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 4 )
   {
      hb_arraySetNL( pStru, 1, p->left   );
      hb_arraySetNL( pStru, 2, p->top    );
      hb_arraySetNL( pStru, 3, p->right  );
      hb_arraySetNL( pStru, 4, p->bottom );
   }
}

LOGFONT * hbwapi_par_LOGFONT( LOGFONT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   void * hfFaceName;
   LPCTSTR pfFaceName;
   HB_SIZE nLen;

   memset( p, 0, sizeof( LOGFONT ) );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      p->lfHeight         = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "lfHeight"         ) );
      p->lfWidth          = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "lfWidth"          ) );
      p->lfEscapement     = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "lfEscapement"     ) );
      p->lfOrientation    = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "lfOrientation"    ) );
      p->lfWeight         = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "lfWeight"         ) );
      p->lfItalic         = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfItalic"         ) );
      p->lfUnderline      = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfUnderline"      ) );
      p->lfStrikeOut      = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfStrikeOut"      ) );
      p->lfCharSet        = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfCharSet"        ) );
      p->lfOutPrecision   = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfOutPrecision"   ) );
      p->lfClipPrecision  = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfClipPrecision"  ) );
      p->lfQuality        = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfQuality"        ) );
      p->lfPitchAndFamily = ( BYTE ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "lfPitchAndFamily" ) );

      pfFaceName = HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lfFaceName" ), &hfFaceName, &nLen );

      if( nLen > ( LF_FACESIZE - 1 ) )
         nLen = LF_FACESIZE - 1;

      memcpy( p->lfFaceName, pfFaceName, nLen * sizeof( TCHAR ) );
      p->lfFaceName[ nLen ] = TEXT( '\0' );

      hb_strfree( hfFaceName );

      return p;
   }
   else if( pStru && HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 14 )
   {
      p->lfHeight         = ( LONG ) hb_arrayGetNL( pStru,  1 );
      p->lfWidth          = ( LONG ) hb_arrayGetNL( pStru,  2 );
      p->lfEscapement     = ( LONG ) hb_arrayGetNL( pStru,  3 );
      p->lfOrientation    = ( LONG ) hb_arrayGetNL( pStru,  4 );
      p->lfWeight         = ( LONG ) hb_arrayGetNL( pStru,  5 );
      p->lfItalic         = ( BYTE ) hb_arrayGetNI( pStru,  6 );
      p->lfUnderline      = ( BYTE ) hb_arrayGetNI( pStru,  7 );
      p->lfStrikeOut      = ( BYTE ) hb_arrayGetNI( pStru,  8 );
      p->lfCharSet        = ( BYTE ) hb_arrayGetNI( pStru,  9 );
      p->lfOutPrecision   = ( BYTE ) hb_arrayGetNI( pStru, 10 );
      p->lfClipPrecision  = ( BYTE ) hb_arrayGetNI( pStru, 11 );
      p->lfQuality        = ( BYTE ) hb_arrayGetNI( pStru, 12 );
      p->lfPitchAndFamily = ( BYTE ) hb_arrayGetNI( pStru, 13 );

      pfFaceName = HB_ARRAYGETSTR( pStru, 14, &hfFaceName, &nLen );

      if( nLen > ( LF_FACESIZE - 1 ) )
         nLen = LF_FACESIZE - 1;

      memcpy( p->lfFaceName, pfFaceName, nLen * sizeof( TCHAR ) );
      p->lfFaceName[ nLen ] = TEXT( '\0' );

      hb_strfree( hfFaceName );

      return p;
   }
   else if( bMandatory )
      return p;

   return NULL;
}

DOCINFO * hbwapi_par_DOCINFO( DOCINFO * p, int iParam, HB_BOOL bMandatory, void *** ph )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );
   void ** h = ( void ** ) hb_xgrabz( 3 * sizeof( void * ) );

   *ph = h;

   memset( p, 0, sizeof( DOCINFO ) );

   p->cbSize = sizeof( DOCINFO );

   if( pStru && HB_IS_HASH( pStru ) )
   {
      p->lpszDocName  = HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpszDocName"  ), &h[ 0 ], NULL );
      p->lpszOutput   = HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpszOutput"   ), &h[ 1 ], NULL );
      p->lpszDatatype = HB_ITEMGETSTR( hb_hashGetCItemPtr( pStru, "lpszDatatype" ), &h[ 2 ], NULL );
      p->fwType       = ( DWORD ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "fwType" ) );

      return p;
   }
   else if( bMandatory )
      return p;

   hb_xfree( h );
   *ph = NULL;

   return NULL;
}

void hbwapi_strfree_DOCINFO( void ** h )
{
   if( h )
   {
      int i;
      for( i = 0; i < 3; ++i )
         hb_strfree( h[ i ] );
   }
}

HB_FUNC( __WAPI_DEVMODE_NEW )
{
#if ! defined( HB_OS_WIN_CE )
   HANDLE hPrinter;
   void * hDeviceName;
   LPCTSTR lpDeviceName = HB_PARSTR( 1, &hDeviceName, NULL );

   hb_retptr( NULL );

   if( OpenPrinter( ( LPTSTR ) lpDeviceName, &hPrinter, NULL ) )
   {
      LONG lSize = DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, NULL, NULL, 0 );

      if( lSize > 0 )
      {
         PDEVMODE pDevMode = ( PDEVMODE ) hb_xgrab( lSize );

         if( DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, pDevMode, pDevMode, DM_OUT_BUFFER ) == IDOK )
            hbwapi_ret_PDEVMODE( pDevMode );
         else
            hb_xfree( pDevMode );
      }

      ClosePrinter( hPrinter );
   }

   hb_strfree( hDeviceName );
#else
   hb_retptr( NULL );
#endif
}

HB_FUNC( __WAPI_DEVMODE_SET )
{
#if ! defined( HB_OS_WIN_CE )
   PDEVMODE pDevMode = hbwapi_par_PDEVMODE( 1 );
   PHB_ITEM pStru = hb_param( 2, HB_IT_ANY );

   if( pDevMode && pStru && HB_IS_HASH( pStru ) )
   {
      pDevMode->dmOrientation   = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmOrientation"   ) );
      pDevMode->dmPaperSize     = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPaperSize"     ) );
      pDevMode->dmPaperLength   = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPaperLength"   ) );
      pDevMode->dmPaperWidth    = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPaperWidth"    ) );
      pDevMode->dmScale         = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmScale"         ) );
      pDevMode->dmCopies        = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmCopies"        ) );
      pDevMode->dmDefaultSource = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmDefaultSource" ) );
      pDevMode->dmPrintQuality  = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPrintQuality"  ) );
      pDevMode->dmDuplex        = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmDuplex"        ) );

      pDevMode->dmFields = 0;
      if( hb_hashGetCItemPtr( pStru, "dmOrientation"   ) ) pDevMode->dmFields |= DM_ORIENTATION;
      if( hb_hashGetCItemPtr( pStru, "dmPaperSize"     ) ) pDevMode->dmFields |= DM_PAPERSIZE;
      if( hb_hashGetCItemPtr( pStru, "dmPaperLength"   ) ) pDevMode->dmFields |= DM_PAPERLENGTH;
      if( hb_hashGetCItemPtr( pStru, "dmPaperWidth"    ) ) pDevMode->dmFields |= DM_PAPERWIDTH;
      if( hb_hashGetCItemPtr( pStru, "dmScale"         ) ) pDevMode->dmFields |= DM_SCALE;
      if( hb_hashGetCItemPtr( pStru, "dmCopies"        ) ) pDevMode->dmFields |= DM_COPIES;
      if( hb_hashGetCItemPtr( pStru, "dmDefaultSource" ) ) pDevMode->dmFields |= DM_DEFAULTSOURCE;
      if( hb_hashGetCItemPtr( pStru, "dmPrintQuality"  ) ) pDevMode->dmFields |= DM_PRINTQUALITY;
      if( hb_hashGetCItemPtr( pStru, "dmDuplex"        ) ) pDevMode->dmFields |= DM_DUPLEX;
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( __WAPI_DEVMODE_GET )
{
#if ! defined( HB_OS_WIN_CE )
   PDEVMODE pDevMode = hbwapi_par_PDEVMODE( 1 );
   PHB_ITEM pStru = hb_param( 2, HB_IT_ANY );

   if( pDevMode && pStru && HB_IS_HASH( pStru ) )
   {
      s_hb_hashSetCItemNL( pStru, "dmOrientation"  , pDevMode->dmOrientation   );
      s_hb_hashSetCItemNL( pStru, "dmPaperSize"    , pDevMode->dmPaperSize     );
      s_hb_hashSetCItemNL( pStru, "dmPaperLength"  , pDevMode->dmPaperLength   );
      s_hb_hashSetCItemNL( pStru, "dmPaperWidth"   , pDevMode->dmPaperWidth    );
      s_hb_hashSetCItemNL( pStru, "dmScale"        , pDevMode->dmScale         );
      s_hb_hashSetCItemNL( pStru, "dmCopies"       , pDevMode->dmCopies        );
      s_hb_hashSetCItemNL( pStru, "dmDefaultSource", pDevMode->dmDefaultSource );
      s_hb_hashSetCItemNL( pStru, "dmPrintQuality" , pDevMode->dmPrintQuality  );
      s_hb_hashSetCItemNL( pStru, "dmDuplex"       , pDevMode->dmDuplex        );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( WAPI_CREATEDC )
{
   void * hDriver;
   void * hDevice;
   void * hOutput;

   hbwapi_ret_HDC( CreateDC( HB_PARSTRDEF( 1, &hDriver, NULL ),
                             HB_PARSTRDEF( 2, &hDevice, NULL ),
                             HB_PARSTR( 3, &hOutput, NULL ),
                             hbwapi_par_PDEVMODE( 4 ) ) );

   hb_strfree( hDriver );
   hb_strfree( hDevice );
   hb_strfree( hOutput );
}

HB_FUNC( WAPI_RESETDC )
{
#if ! defined( HB_OS_WIN_CE )
   HDC hDC = hbwapi_par_HDC( 1 );
   PDEVMODE pDEVMODE = hbwapi_par_PDEVMODE( 2 );

   if( hDC )
      hb_retl( ResetDC( hDC, pDEVMODE ) == hDC );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WAPI_STARTDOC )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   void ** hDOCINFO = NULL;
   DOCINFO di;

   if( hDC && hbwapi_par_DOCINFO( &di, 2, HB_FALSE, &hDOCINFO ) )
      hb_retni( StartDoc( hDC, &di ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   hbwapi_strfree_DOCINFO( hDOCINFO );
}

HB_FUNC( WAPI_ENDDOC )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( EndDoc( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_ABORTDOC )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( AbortDoc( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_STARTPAGE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( StartPage( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_ENDPAGE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( EndPage( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_SETBKMODE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( SetBkMode( hDC, hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETBKMODE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( GetBkMode( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETDEVICECAPS )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( GetDeviceCaps( hDC, hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_SETMAPMODE )
{
#if ! defined( HB_OS_WIN_CE )
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( SetMapMode( hDC, hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( WAPI_GETMAPMODE )
{
#if ! defined( HB_OS_WIN_CE )
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( GetMapMode( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( WAPI_SETTEXTALIGN )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( ( int ) SetTextAlign( hDC, ( UINT ) hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETTEXTALIGN )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( ( int ) GetTextAlign( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_TEXTOUT )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      void * hData;
      HB_SIZE nDataLen;
      LPCTSTR lpData = HB_PARSTR( 4, &hData, &nDataLen );

#if ! defined( HB_OS_WIN_CE )
      hb_retl( TextOut( hDC, hb_parni( 2 ) /* iRow */,
                             hb_parni( 3 ) /* iCol */,
                             lpData,
                             ( int ) nDataLen ) );
#else
      /* Emulating TextOut() using ExtTextOut(). [vszakats] */
      hb_retl( ExtTextOut( hDC, hb_parni( 2 ) /* iRow */,
                                hb_parni( 3 ) /* iCol */,
                                0,
                                NULL,
                                lpData,
                                ( UINT ) nDataLen,
                                NULL ) );
#endif

      hb_strfree( hData );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   hb_retnl( HB_FALSE );
}

HB_FUNC( WAPI_EXTTEXTOUT )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      void * hData;
      HB_SIZE nDataLen;
      LPCTSTR lpData = HB_PARSTR( 6, &hData, &nDataLen );
      RECT rect;
      PHB_ITEM pFontWidths = hb_param( 7, HB_IT_ARRAY );
      INT * lpFontWidths;

      if( pFontWidths )
      {
         HB_SIZE nFontWidthsLen = hb_arrayLen( pFontWidths );
         HB_SIZE tmp;
         INT iWidth = 0;

         lpFontWidths = ( INT * ) hb_xgrab( nDataLen * sizeof( INT ) );

         for( tmp = 0; tmp < nDataLen; ++tmp )
         {
            /* Pad width array with last known value if passed array was smaller than length of the string. */
            if( tmp < nFontWidthsLen )
               iWidth = ( INT ) hb_arrayGetNI( pFontWidths, tmp + 1 );

            lpFontWidths[ tmp ] = iWidth;
         }
      }
      else
         lpFontWidths = NULL;


      hb_retl( ExtTextOut( hDC, hb_parni( 2 ) /* iRow */,
                                hb_parni( 3 ) /* iCol */,
                                ( UINT ) hb_parni( 4 ) /* fuOptions */,
                                hbwapi_par_RECT( &rect, 5, HB_FALSE ),
                                lpData,
                                ( UINT ) nDataLen,
                                lpFontWidths ) );

      if( lpFontWidths )
         hb_xfree( lpFontWidths );

      hb_strfree( hData );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_SETTEXTCOLOR )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retnl( ( long ) SetTextColor( hDC, ( COLORREF ) hb_parnl( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETTEXTCOLOR )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retnl( ( long ) GetTextColor( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETTEXTFACE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      TCHAR tszFontName[ 128 ];

      GetTextFace( hDC, HB_SIZEOFARRAY( tszFontName ) - 1, tszFontName );

      HB_RETSTR( tszFontName );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_SETBKCOLOR )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retnl( ( long ) SetBkColor( hDC, ( COLORREF ) hb_parnl( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETBKCOLOR )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retnl( ( long ) GetBkColor( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_CREATEPEN )
{
   hbwapi_ret_HPEN( CreatePen( hb_parni( 1 ) /* fnPenStyle */,
                               hb_parni( 2 ) /* nWidth */,
                               ( COLORREF ) hb_parnl( 3 ) /* crColor */ ) );
}

HB_FUNC( WAPI_CREATESOLIDBRUSH )
{
   HBRUSH h = CreateSolidBrush( ( COLORREF ) hb_parnl( 1 ) /* crColor */ );
#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( GetLastError() );
#endif
   hbwapi_ret_HBRUSH( h );
}

HB_FUNC( WAPI_CREATEHATCHBRUSH )
{
#if ! defined( HB_OS_WIN_CE )
   hbwapi_ret_HBRUSH( CreateHatchBrush( hb_parni( 1 ) /* fnStyle */,
                                        ( COLORREF ) hb_parnl( 2 ) /* crColor */ ) );
#else
   hb_retptr( NULL );
#endif
}

HB_FUNC( WAPI_CREATEFONT )
{
#if ! defined( HB_OS_WIN_CE )
   void * hFontFace;

   hbwapi_ret_HFONT( CreateFont( hb_parni( 1 ) /* nHeight */,
                                 hb_parni( 2 ) /* nWidth */,
                                 hb_parni( 3 ) /* nEscapement */,
                                 hb_parni( 4 ) /* nOrientation */,
                                 hb_parni( 5 ) /* fnWeight */,
                                 ( DWORD ) hb_parl( 6 ) /* fdwItalic */,
                                 ( DWORD ) hb_parl( 7 ) /* fdwUnderline */,
                                 ( DWORD ) hb_parl( 8 ) /* fdwStrikeOut */,
                                 ( DWORD ) hb_parnl( 9 ) /* fdwCharSet */,
                                 ( DWORD ) hb_parnldef( 10, OUT_DEFAULT_PRECIS ) /* fdwOutputPrecision */,
                                 ( DWORD ) hb_parnldef( 11, CLIP_DEFAULT_PRECIS ) /* fdwClipPrecision */,
                                 ( DWORD ) hb_parnldef( 12, DEFAULT_QUALITY ) /* fdwQuality */,
                                 ( DWORD ) hb_parnldef( 13, DEFAULT_PITCH | FF_DONTCARE ) /* fdwPitchAndFamily */,
                                 HB_PARSTR( 14, &hFontFace, NULL ) /* lpszFace */ ) );

   hb_strfree( hFontFace );
#else
   hb_retptr( NULL );
#endif
}

HB_FUNC( WAPI_CREATEFONTINDIRECT )
{
   LOGFONT p;

   if( hbwapi_par_LOGFONT( &p, 1, HB_TRUE ) )
      hbwapi_ret_HFONT( CreateFontIndirect( &p ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_SELECTOBJECT )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   HB_BOOL bRegion = HB_FALSE;
   HGDIOBJ h;

   if(      ( h = hbwapi_par_HPEN( 2 ) ) != NULL ) {}
   else if( ( h = hbwapi_par_HBRUSH( 2 ) ) != NULL ) {}
   else if( ( h = hbwapi_par_HFONT( 2 ) ) != NULL ) {}
   /* TODO: Add BITMAP, REGION */
   else
      h = NULL;

   if( hDC && h )
   {
      /* TODO: Solve reference counting to 'h' handle. Also for returned one. */
      if( bRegion )
         hb_retnint( ( HB_PTRDIFF ) SelectObject( hDC, h ) );
      else
         hb_retl( SelectObject( hDC, h ) != NULL ); /* NOTE: We don't return a raw pointer. */
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_MOVETOEX )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      POINT p;

      if( hbwapi_par_POINT( &p, 4, HB_FALSE ) )
      {
         hb_retl( MoveToEx( hDC, hb_parni( 2 ) /* X */, hb_parni( 3 ) /* Y */, &p ) );

         hbwapi_stor_POINT( &p, 4 );
      }
      else
         hb_retl( MoveToEx( hDC, hb_parni( 2 ) /* X */, hb_parni( 3 ) /* Y */, NULL ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_LINETO )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retl( LineTo( hDC, hb_parni( 2 ) /* XEnd */, hb_parni( 3 ) /* YEnd */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_FILLRECT )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   RECT rect;
   HBRUSH hBrush = hbwapi_par_HBRUSH( 3 );

   if( hDC && hbwapi_par_RECT( &rect, 2, HB_TRUE ) && hBrush )
      hb_retni( FillRect( hDC, &rect, hBrush ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_ROUNDRECT )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retl( RoundRect( hDC,
                          hb_parni( 2 ) /* x1 */,
                          hb_parni( 3 ) /* y1 */,
                          hb_parni( 4 ) /* x2 */,
                          hb_parni( 5 ) /* y2 */,
                          hb_parni( 6 ) /* iWidth */,
                          hb_parni( 7 ) /* iHeight */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_RECTANGLE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retl( Rectangle( hDC,
                          hb_parni( 2 ) /* x1 */,
                          hb_parni( 3 ) /* y1 */,
                          hb_parni( 4 ) /* x2 */,
                          hb_parni( 5 ) /* y2 */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_ARC )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retl( Arc( hDC,
                    hb_parni( 2 ) /* nLeftRect */,
                    hb_parni( 3 ) /* nTopRect */,
                    hb_parni( 4 ) /* nRightRect */,
                    hb_parni( 5 ) /* nBottomRect */,
                    hb_parni( 6 ) /* nXStartArc */,
                    hb_parni( 7 ) /* nYStartArc */,
                    hb_parni( 8 ) /* nXEndArc */,
                    hb_parni( 9 ) /* nYEndArc */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_ELLIPSE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retl( Ellipse( hDC,
                        hb_parni( 2 ) /* nLeftRect */,
                        hb_parni( 3 ) /* nTopRect */,
                        hb_parni( 4 ) /* nRightRect */,
                        hb_parni( 5 ) /* nBottomRect */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
