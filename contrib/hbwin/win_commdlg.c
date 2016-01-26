/*
 * Common dialogs
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
 * based on: Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbwapi.h"
#include "hbapiitm.h"
#include "hbvm.h"

#include <commdlg.h>

#if defined( __MINGW32CE__ )
/* ChooseColorW() problem is fixed in current devel MINGW32CE version but
   people who use recent official release (0.50) needs it */
#undef ChooseColor
BOOL WINAPI ChooseColor( LPCHOOSECOLORW );
#endif

#define _HB_CHOOSECOLOR_CB_PROP_  TEXT( "__hbwin_win_ChooseColor_CB" )

#if ! defined( HB_OS_WIN_CE )
static UINT_PTR CALLBACK CCHookProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   UINT_PTR res;
   HB_BOOL  fInit = HB_FALSE;
   PHB_ITEM pBlock;

   if( msg == WM_INITDIALOG )
   {
      CHOOSECOLOR * cc = ( CHOOSECOLOR * ) lParam;
      SetProp( hWnd, _HB_CHOOSECOLOR_CB_PROP_, hb_itemNew( ( PHB_ITEM ) cc->lCustData ) );
      fInit = HB_TRUE;
   }

   if( ( pBlock = ( PHB_ITEM ) GetProp( hWnd, _HB_CHOOSECOLOR_CB_PROP_ ) ) != NULL &&
       hb_vmRequestReenter() )
   {
      PHB_ITEM pWnd = hbwapi_itemPut_HANDLE( NULL, hWnd );
      PHB_ITEM pMsg = hb_itemPutNInt( NULL, msg );
      PHB_ITEM pLPa = hb_itemPutNInt( NULL, wParam );
      PHB_ITEM pWPa = hb_itemPutNInt( NULL, lParam );

      hb_evalBlock( pBlock, pWnd, pMsg, pLPa, pWPa );

      res = ( UINT_PTR ) hbwapi_par_RESULT( -1 );

      hb_itemRelease( pWnd );
      hb_itemRelease( pMsg );
      hb_itemRelease( pLPa );
      hb_itemRelease( pWPa );

      if( msg == WM_NCDESTROY )
      {
         RemoveProp( hWnd, _HB_CHOOSECOLOR_CB_PROP_ );
         hb_itemRelease( pBlock );
      }

      hb_vmRequestRestore();
   }
   else
      res = 0;

   return fInit ? 1 : res;
}
#endif

HB_FUNC( WIN_CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF    crCustClr[ 16 ];
   int         i;

   void * hTpl;

   for( i = 0; i < ( int ) HB_SIZEOFARRAY( crCustClr ); ++i )
      crCustClr[ i ] = HB_ISARRAY( 4 ) ? hbwapi_parv_COLORREF( 4, i + 1 ) : RGB( 0, 0, 0 );

   memset( &cc, 0, sizeof( cc ) );

   cc.lStructSize    = sizeof( cc );
   cc.hwndOwner      = hbwapi_par_raw_HWND( 1 );
#if ! defined( HB_OS_WIN_CE )
   cc.hInstance      = hbwapi_par_raw_HWND( 2 );
#endif
   cc.rgbResult      = hbwapi_par_COLORREF( 3 );
   cc.lpCustColors   = crCustClr;
   cc.Flags          = hbwapi_par_WORD( 5 );
#if ! defined( HB_OS_WIN_CE )
   cc.lCustData      = ( LPARAM ) ( HB_PTRUINT ) hb_param( 6, HB_IT_EVALITEM );
   cc.lpfnHook       = cc.lCustData ? CCHookProc : NULL;
#endif
   cc.lpTemplateName = HB_PARSTR( 7, &hTpl, NULL );

   if( ChooseColor( &cc ) )
      hbwapi_ret_COLORREF( cc.rgbResult );
   else
      hbwapi_ret_COLORREF( -1 );

   hb_strfree( hTpl );
}
