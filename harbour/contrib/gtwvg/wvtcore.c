/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2007 Pritpal Bedi <pritpal@vouchcac.com>
 * Based on:
 *
 * Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                      GUI Drawing Functions
//               Pritpal Bedi <pritpal@vouchcac.com>
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#define HB_OS_WIN_32_USED

//-------------------------------------------------------------------//

#include "gtwvt.h"

static GLOBAL_DATA *_s = NULL;

static void hb_wvt_DrawBoxRaised( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawBoxRecessed( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawOutline( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawBoxGet( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawBoxGroup( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawBoxGroupRaised( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawToolButtonFlat( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawToolButtonUp( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );
static void hb_wvt_DrawToolButtonDown( HDC hdc, int iTop, int iLeft, int iBottom, int iRight );

//-------------------------------------------------------------------//

void HB_EXPORT hb_wvt_wvtCore( void )
{
   _s = hb_wvt_gtGetGlobalData();
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_CORE )
{
   hb_wvt_wvtCore();
}

//-------------------------------------------------------------------//
//
//                 Modeless Dialogs Implementation
//
//-------------------------------------------------------------------//

HB_EXPORT BOOL CALLBACK hb_wvt_gtDlgProcMLess( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   long int lReturn = 0;
   PHB_ITEM pFunc = NULL;

   iType = (int) NULL;

   for ( iIndex = 0; iIndex < WVT_DLGML_MAX; iIndex++ )
   {
      if ( ( _s->hDlgModeless[ iIndex ] != NULL ) && ( _s->hDlgModeless[ iIndex ] == hDlg ) )
      {
         if ( _s->pFunc[ iIndex ] != NULL )
         {
            pFunc = _s->pFunc[ iIndex ];
            iType = _s->iType[ iIndex ];
         }
         break;
      }
   }

   if ( pFunc )
   {
      switch ( iType )
      {
         case 1:  /* Function Name */
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( ( PHB_DYNS ) pFunc );
               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_LONG ) ( HB_PTRDIFF ) hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmDo( 4 );
               lReturn = hb_parnl( -1 );
               hb_vmRequestRestore();
            }
            break;

         case 2:  /* Block */
            /* eval the codeblock */
            if ( HB_IS_BLOCK( pFunc ) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( _s->pFunc[ iIndex ] );
                  hb_vmPushNumInt( ( HB_LONG ) ( HB_PTRDIFF ) hDlg );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam );
                  hb_vmPushNumInt( lParam );
                  hb_vmSend( 4 );
                  lReturn = hb_parnl( -1 );
                  hb_vmRequestRestore();
               }
            }
            else
            {
               /* TODO: internal error: missing codeblock */
            }
            break;
      }
   }

   switch( message )
   {
      case WM_COMMAND:
         switch( LOWORD( wParam ) )
         {
            case IDOK:
               DestroyWindow( hDlg );
               lReturn = 1;
               break;

            case IDCANCEL:
               DestroyWindow( hDlg );
               lReturn = 0;
               break;
         }
         break;

      case WM_CLOSE:
         DestroyWindow( hDlg );
         lReturn = 0;
         break;

      case WM_NCDESTROY:
         if ( _s->pFunc[ iIndex ] != NULL && _s->iType[ iIndex ] == 2 )
         {
            hb_itemRelease( ( PHB_ITEM ) _s->pFunc[ iIndex ] );
         }
         _s->hDlgModeless[ iIndex ] = NULL;
         _s->pFunc[ iIndex ] = NULL;
         _s->iType[ iIndex ] = (int) NULL;
         lReturn = 0;
         break;
   }

   return lReturn;
}

//-------------------------------------------------------------------//

HB_EXPORT BOOL CALLBACK hb_wvt_gtDlgProcModal( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   int      iIndex, iType;
   long int lReturn = 0;
   PHB_ITEM pFunc   = NULL;
   int      iFirst  = ( int ) lParam;

   if ( iFirst > 0 && iFirst <= WVT_DLGMD_MAX )
   {
      _s->hDlgModal[ iFirst-1 ] = hDlg ;
      SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      return lReturn;
   }

   iType = ( int ) NULL;

   for ( iIndex = 0; iIndex < WVT_DLGMD_MAX; iIndex++ )
   {
      if ( ( _s->hDlgModal[ iIndex ] != NULL ) && ( _s->hDlgModal[ iIndex ] == hDlg ) )
      {
         if ( _s->pFuncModal[ iIndex ] != NULL )
         {
            pFunc = _s->pFuncModal[ iIndex ];
            iType = _s->iTypeModal[ iIndex ];
         }
         break;
      }
   }

   if ( pFunc )
   {
      switch ( iType )
      {
         case 1:  /* Function Name */
            if( hb_vmRequestReenter() )
            {
               hb_vmPushDynSym( ( PHB_DYNS ) pFunc );
               hb_vmPushNil();
               hb_vmPushNumInt( ( HB_LONG ) ( HB_PTRDIFF ) hDlg );
               hb_vmPushNumInt( message );
               hb_vmPushNumInt( wParam );
               hb_vmPushNumInt( lParam );
               hb_vmDo( 4 );
               lReturn = hb_parnl( -1 );
               hb_vmRequestRestore();
            }
            break;

         case 2:  /* Block */
            /* eval the codeblock */
            if ( HB_IS_BLOCK( pFunc ) )
            {
               if( hb_vmRequestReenter() )
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( _s->pFunc[ iIndex ] );
                  hb_vmPushNumInt( ( HB_LONG ) ( HB_PTRDIFF ) hDlg );
                  hb_vmPushNumInt( message );
                  hb_vmPushNumInt( wParam );
                  hb_vmPushNumInt( lParam );
                  hb_vmSend( 4 );
                  lReturn = hb_parnl( -1 );
                  hb_vmRequestRestore();
               }
            }
            else
            {
               /* TODO: internal error: missing codeblock */
            }
            break;
      }
   }

   switch( message )
   {
      case WM_COMMAND:
         switch( LOWORD( wParam ) )
         {
            case IDOK:
               EndDialog( hDlg, IDOK );
               lReturn = 1;
               break;

            case IDCANCEL:
               EndDialog( hDlg, IDCANCEL );
               lReturn = 0;
               break;
         }
         break;

      case WM_CLOSE:
         EndDialog( hDlg, IDCANCEL );
         lReturn = 0;
         break;

      case WM_NCDESTROY:
         if ( _s->pFuncModal[ iIndex ] != NULL && _s->iTypeModal[ iIndex ] == 2 )
         {
            hb_itemRelease( ( PHB_ITEM ) _s->pFuncModal[ iIndex ] );
         }
         _s->hDlgModal[ iIndex ]  = NULL;
         _s->pFuncModal[ iIndex ] = NULL;
         _s->iTypeModal[ iIndex ] = ( int ) NULL;
         lReturn = 0;
         break;
   }

   return lReturn;
}

//-------------------------------------------------------------------//

HB_EXPORT BOOL hb_wvt_DrawImage( HDC hdc, int x1, int y1, int wd, int ht, char * image )
{
  HGLOBAL  hGlobal;
  HANDLE   hFile;
  DWORD    nFileSize;
  DWORD    nReadByte;
  LONG     lWidth,lHeight;
  int      x,y,xe,ye;
  int      c   = x1 ;
  int      r   = y1 ;
  int      dc  = wd ;
  int      dr  = ht ;
  int      tor =  0 ;
  int      toc =  0 ;
  HRGN     hrgn1;
  POINT    lpp = { 0,0 };
  BOOL     bResult = FALSE;
  LPTSTR   lpImage = HB_TCHAR_CONVTO( image );

  hFile = CreateFile( lpImage, GENERIC_READ, 0, NULL, OPEN_EXISTING,
                                      FILE_ATTRIBUTE_NORMAL, NULL );
  HB_TCHAR_FREE( lpImage );

  if ( hFile != INVALID_HANDLE_VALUE )
  {
    nFileSize = GetFileSize( hFile, NULL );

    if ( nFileSize != INVALID_FILE_SIZE )
    {
      hGlobal = GlobalAlloc( GPTR, nFileSize );

      if ( hGlobal )
      {
        if ( ReadFile( hFile, hGlobal, nFileSize, &nReadByte, NULL ) )
        {
          IStream  *iStream;
          IPicture *iPicture;
          IPicture ** iPictureRef = &iPicture;

          CreateStreamOnHGlobal( hGlobal, TRUE, &iStream );
          OleLoadPicture( iStream, nFileSize, TRUE, (REFIID) &IID_IPicture, ( LPVOID * ) iPictureRef );

          if ( iPicture )
          {
            iPicture->lpVtbl->get_Width( iPicture,&lWidth );
            iPicture->lpVtbl->get_Height( iPicture,&lHeight );

            if ( dc  == 0 )
            {
              dc = ( int ) ( ( float ) dr * lWidth  / lHeight );
            }
            if ( dr  == 0 )
            {
              dr = ( int ) ( ( float ) dc * lHeight / lWidth  );
            }
            if ( tor == 0 )
            {
              tor = dr;
            }
            if ( toc == 0 )
            {
              toc = dc;
            }
            x  = c;
            y  = r;
            xe = c + toc - 1;
            ye = r + tor - 1;

            GetViewportOrgEx( hdc, &lpp );

            hrgn1 = CreateRectRgn( c+lpp.x, r+lpp.y, xe+lpp.x, ye+lpp.y );
            SelectClipRgn( hdc, hrgn1 );

            while ( x < xe )
            {
              while ( y < ye )
              {
                iPicture->lpVtbl->Render( iPicture, hdc, x, y, dc, dr, 0,
                                          lHeight, lWidth, -lHeight, NULL );
                y += dr;
              }
              y =  r;
              x += dc;
            }

            SelectClipRgn( hdc, NULL );
            DeleteObject( hrgn1 );

            iPicture->lpVtbl->Release( iPicture );
            bResult = TRUE ;
          }
        }
        GlobalFree( hGlobal );
      }
    }
    CloseHandle( hFile );
  }
  return( bResult );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawBoxRaised( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penWhiteDim );
   MoveToEx( hdc, iLeft, iTop, NULL );        //  Top Inner
   LineTo( hdc, iRight, iTop );
   MoveToEx( hdc, iLeft, iTop, NULL );        //  Left Inner
   LineTo( hdc, iLeft, iBottom );

   SelectObject( hdc, _s->penWhite );
   MoveToEx( hdc, iLeft-1, iTop-1, NULL );    //  Top Outer
   LineTo( hdc, iRight+1, iTop-1 );
   MoveToEx( hdc, iLeft-1, iTop-1, NULL );    //  Left Outer
   LineTo( hdc, iLeft-1, iBottom+1 );

   SelectObject( hdc, _s->penDarkGray );
   MoveToEx( hdc, iLeft, iBottom, NULL );     //  Bottom Inner
   LineTo( hdc, iRight, iBottom );
   MoveToEx( hdc, iRight, iBottom, NULL );    //  Right Inner
   LineTo( hdc, iRight, iTop );

   SelectObject( hdc, _s->penBlack );
   MoveToEx( hdc, iLeft-1, iBottom+1, NULL ); //  Bottom Outer
   LineTo( hdc, iRight+1+1, iBottom+1 );
   MoveToEx( hdc, iRight+1, iTop-1, NULL );   //  Right Outer
   LineTo( hdc, iRight+1, iBottom+1 );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawBoxRecessed( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penWhiteDim );
   MoveToEx( hdc, iRight, iTop, NULL );            // Right Inner
   LineTo( hdc, iRight, iBottom );
   MoveToEx( hdc, iLeft, iBottom, NULL );          // Bottom Inner
   LineTo( hdc, iRight, iBottom );

   SelectObject( hdc, _s->penWhite );
   MoveToEx( hdc, iRight+1, iTop-1, NULL );        // Right Outer
   LineTo( hdc, iRight + 1, iBottom + 1 );
   MoveToEx( hdc, iLeft - 1, iBottom + 1, NULL );  // Bottom Outer
   LineTo( hdc, iRight + 2, iBottom + 1 );

   SelectObject( hdc, _s->penBlack );
   MoveToEx( hdc, iLeft, iTop, NULL );             // Left Inner
   LineTo( hdc, iLeft, iBottom );
   MoveToEx( hdc, iLeft, iTop, NULL );             // Top Inner
   LineTo( hdc, iRight, iTop );

   SelectObject( hdc, _s->penDarkGray );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Left Outer
   LineTo( hdc, iLeft - 1 , iBottom + 1 );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Top Outer
   LineTo( hdc, iRight + 1, iTop - 1 );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawOutline( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   MoveToEx( hdc, iLeft, iTop, NULL );             //  Top
   LineTo( hdc, iRight, iTop );

   MoveToEx( hdc, iLeft, iTop, NULL );             //  Left
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );          //  Bottom
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iRight, iTop, NULL );            //  Right
   LineTo( hdc, iRight, iBottom + 1);
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawBoxGet( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penBlack );
   MoveToEx( hdc, iLeft-1 , iTop-1, NULL );        // Top Inner
   LineTo(   hdc, iRight-1, iTop-1       );
   MoveToEx( hdc, iLeft-1 , iTop-1, NULL );        // Left Inner
   LineTo(   hdc, iLeft-1 , iBottom-1    );

   SelectObject( hdc, _s->penDarkGray );
   MoveToEx( hdc, iLeft-2, iTop-2, NULL );         // Top Outer
   LineTo(   hdc, iRight , iTop-2       );
   MoveToEx( hdc, iLeft-2, iTop-2, NULL );         // Left Outer
   LineTo(   hdc, iLeft-2, iBottom      );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawBoxGroup( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penDarkGray );

   MoveToEx( hdc, iRight, iTop, NULL );            // Right Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );          // Bottom Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Left Outer
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Top Outer
   LineTo( hdc, iRight + 1, iTop - 1 );


   SelectObject( hdc, _s->penWhite );

   MoveToEx( hdc, iRight + 1, iTop, NULL );        // Right Outer
   LineTo( hdc, iRight + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft -1, iBottom + 1, NULL );   // Bottom Outer
   LineTo( hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( hdc, iLeft, iTop, NULL );             // Left Inner
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );             // Top Inner
   LineTo( hdc, iRight, iTop );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawBoxGroupRaised( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penWhite );

   MoveToEx( hdc, iRight, iTop, NULL );           // Right Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );         // Bottom Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );    // Left Outer
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );    // Top Outer
   LineTo( hdc, iRight + 1, iTop - 1 );

   SelectObject( hdc, _s->penDarkGray );

   MoveToEx( hdc, iRight + 1, iTop, NULL );       // Right Outer
   LineTo( hdc, iRight + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft -1, iBottom + 1, NULL );  // Bottom Outer
   LineTo( hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( hdc, iLeft, iTop, NULL );            // Left Inner
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Top Inner
   LineTo( hdc, iRight, iTop );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawToolButtonFlat( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penGray );

   MoveToEx( hdc, iRight, iTop, NULL );           // Right
   LineTo( hdc, iRight, iBottom + 1);

   MoveToEx( hdc, iLeft, iBottom, NULL );         // Bottom
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Left
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Top
   LineTo( hdc, iRight, iTop );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawToolButtonUp( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penBlack );

   MoveToEx( hdc, iRight, iTop, NULL );           // Right
   LineTo( hdc, iRight, iBottom+1 );

   MoveToEx( hdc, iLeft, iBottom, NULL );         // Bottom
   LineTo( hdc, iRight, iBottom );

   SelectObject( hdc, _s->penWhite );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Left
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Top
   LineTo( hdc, iRight, iTop );
}

//-------------------------------------------------------------------//

static void hb_wvt_DrawToolButtonDown( HDC hdc, int iTop, int iLeft, int iBottom, int iRight )
{
   SelectObject( hdc, _s->penWhite );

   MoveToEx( hdc, iRight, iTop, NULL );           // Right
   LineTo( hdc, iRight, iBottom+1 );

   MoveToEx( hdc, iLeft, iBottom, NULL );         // Bottom
   LineTo( hdc, iRight, iBottom );

   SelectObject( hdc, _s->penBlack );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Left
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Top
   LineTo( hdc, iRight, iTop );
}

//-------------------------------------------------------------------//
//
//   Wvt_SetPen( nPenStyle, nWidth, nColor )
//
HB_FUNC( WVT_SETPEN )
{
   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;

   if ( ISNIL( 1 ) )
   {
      hb_retl( FALSE );
   }

   iPenStyle = hb_parni( 1 ) ;
   iPenWidth = ISNIL( 2 ) ? 0 : hb_parni( 2 );
   crColor   = ISNIL( 3 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 3 );

   hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      if ( _s->currentPen )
      {
         DeleteObject( _s->currentPen );
      }
      _s->currentPen = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_SetBrush( nStyle, nColor, [ nHatch ] )
//
HB_FUNC( WVT_SETBRUSH )
{
   HBRUSH   hBrush;
   LOGBRUSH lb = { 0,0,0 };

   if ( ISNIL( 1 ) )
   {
      hb_retl( FALSE );
   }

   lb.lbStyle = hb_parnl( 1 );
   lb.lbColor = ISNIL( 2 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 2 ) ;
   lb.lbHatch = ISNIL( 3 ) ? 0 : hb_parnl( 3 );

   hBrush     = CreateBrushIndirect( &lb );

   if ( hBrush )
   {
      if ( _s->currentBrush )
      {
         DeleteObject( _s->currentBrush );
      }
      _s->currentBrush = hBrush;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawBoxGet( nRow, nCol, nWidth )
//
HB_FUNC( WVT_DRAWBOXGET )
{
   POINT xy = { 0,0 };
   POINT yz = { 0,0 };

   xy = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   yz = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ) + hb_parni( 3 ), hb_parni( 1 ) + 1 );

   hb_wvt_DrawBoxGet( _s->hdc, xy.y, xy.x, yz.y, yz.x );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxGet( _s->hGuiDC, xy.y, xy.x, yz.y, yz.x );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWBOXRAISED )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   hb_wvt_DrawBoxRaised( _s->hdc, iTop-1, iLeft-1, iBottom+1, iRight+1 );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxRaised( _s->hGuiDC, iTop-1, iLeft-1, iBottom+1, iRight+1 );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawBoxRecessed( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWBOXRECESSED )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   hb_wvt_DrawBoxRecessed( _s->hdc, iTop-1, iLeft-1, iBottom+1, iRight+1 );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxRecessed( _s->hGuiDC, iTop-1, iLeft-1, iBottom+1, iRight+1 );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawBoxGroup( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWBOXGROUP )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   hb_wvt_DrawBoxGroup( _s->hdc, iTop, iLeft, iBottom, iRight );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxGroup( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawBoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWBOXGROUPRAISED )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   hb_wvt_DrawBoxGroupRaised( _s->hdc, iTop, iLeft, iBottom, iRight );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxGroupRaised( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawImage( nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, aOffSet )
//
HB_FUNC( WVT_DRAWIMAGE )
{
   POINT xy = { 0,0 };
   int   iLeft, iTop, iRight, iBottom;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y + hb_parni( 6,1 );
   iLeft   = xy.x + hb_parni( 6,2 ) ;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y - 1 + hb_parni( 6,3 );
   iRight  = xy.x - 1 + hb_parni( 6,4 );

   if ( ISNUM( 5 ) )
   {
      hb_wvt_gtRenderPicture( iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, _s->iPicture[ hb_parni( 5 )-1 ] );
   }
   else
   {
      hb_wvt_DrawImage( _s->hdc, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 5 ) ) ;
      if ( _s->bGui )
      {
         hb_wvt_DrawImage( _s->hGuiDC, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 5 ) ) ;
      }
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    WVT_DRAWLABEL( nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, nBkColor,
//                     cFontFace, , , , ,  )
//
HB_FUNC( WVT_DRAWLABEL )
{
   POINT    xy = { 0,0 };
   HFONT    hFont, hOldFont, hOldFontGui;
   LOGFONT  logfont;// = { 0 };

   logfont.lfEscapement     = ( ISNIL(  5 ) ? 0 : ( hb_parni( 5 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL( 11 ) ? 0 : hb_parni( 11 ) );
   logfont.lfItalic         = ( ISNIL( 14 ) ? 0 : hb_parl( 14 ) );
   logfont.lfUnderline      = ( ISNIL( 15 ) ? 0 : hb_parl( 15 ) );
   logfont.lfStrikeOut      = ( ISNIL( 16 ) ? 0 : hb_parl( 16 ) );
   logfont.lfCharSet        = ( ISNIL( 13 ) ? _s->CodePage : hb_parni( 13 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 12 ) ? DEFAULT_QUALITY : hb_parni( 12 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  9 ) ? _s->fontHeight : hb_parni(  9 ) );
   logfont.lfWidth          = ( ISNIL( 10 ) ? (_s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth ) : hb_parni( 10 ) );

   //strcpy( logfont.lfFaceName, ( ISNIL( 8 ) ? _s->fontFace : hb_parcx( 8 ) ) );
   HB_TCHAR_CPTO( logfont.lfFaceName, ( ISNIL( 8 ) ? _s->fontFace : hb_parcx( 8 ) ), sizeof( logfont.lfFaceName )-1 );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      LPTSTR text = HB_TCHAR_CONVTO( hb_parc( 3 ) );

      xy          = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

      SetBkColor( _s->hdc, ISNIL( 7 ) ? _s->background : ( COLORREF ) hb_parnl( 7 ) );
      SetTextColor( _s->hdc, ISNIL( 6 ) ? _s->foreground : ( COLORREF ) hb_parnl( 6 ) );
      SetTextAlign( _s->hdc, ( ISNIL( 4 ) ? TA_LEFT : hb_parni( 4 ) ) );
      hOldFont = ( HFONT ) SelectObject( _s->hdc, hFont );

      //ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 3 ), strlen( hb_parcx( 3 ) ), NULL );
      ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, text, lstrlen( text ), NULL );

      SelectObject( _s->hdc, hOldFont );

      if ( _s->bGui )
      {
         SetBkColor( _s->hGuiDC, ISNIL( 7 ) ? _s->background : ( COLORREF ) hb_parnl( 7 ) );
         SetTextColor( _s->hGuiDC, ISNIL( 6 ) ? _s->foreground : ( COLORREF ) hb_parnl( 6 ) );
         SetTextAlign( _s->hGuiDC, ( ISNIL( 4 ) ? TA_LEFT : hb_parni( 4 ) ) );
         hOldFontGui = ( HFONT ) SelectObject( _s->hGuiDC, hFont );

         //ExtTextOut( _s->hGuiDC, xy.x, xy.y, 0, NULL, hb_parcx( 3 ), strlen( hb_parcx( 3 ) ), NULL );
         ExtTextOut( _s->hGuiDC, xy.x, xy.y, 0, NULL, text, lstrlen( text ), NULL );
         SelectObject( _s->hGuiDC, hOldFontGui );
      }
      HB_TCHAR_FREE( text );
      DeleteObject( hFont );
      hb_retl( TRUE );
   }

   hb_retl( FALSE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawOutline( nTop, nLeft, nBottom, nRight, nThick, nShape, nRGBColor )
//
HB_FUNC( WVT_DRAWOUTLINE )
{
   HPEN  hPen, hOldPen, hOldPenGUI;
   POINT xy = { 0,0 };
   int   iTop, iLeft, iBottom, iRight;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;
   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
   iBottom = xy.y;
   iRight  = xy.x;

   hOldPenGUI = hOldPen = 0;

   if ( ISNUM( 5 ) )
   {
      hPen = CreatePen( hb_parni( 5 ), 0, ( ISNIL( 7 ) ? 0 : ( COLORREF ) hb_parnl( 7 ) ) );
      if ( hPen )
      {
         hOldPen = (HPEN) SelectObject( _s->hdc, hPen );
      }
   }
   else
   {
      hPen = 0;
      SelectObject( _s->hdc, _s->penBlack );
   }

   hb_wvt_DrawOutline( _s->hdc, iTop, iLeft, iBottom, iRight );
   if ( _s->bGui )
   {
      if ( hPen )
      {
         hOldPenGUI = (HPEN) SelectObject( _s->hGuiDC, hPen );
      }
      else
      {
         hOldPenGUI = (HPEN) SelectObject( _s->hGuiDC, _s->penBlack );
         hb_wvt_DrawOutline( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
      }
   }

   if ( hPen )
   {
      SelectObject( _s->hdc, hOldPen );
      if ( hOldPenGUI )
      {
         SelectObject( _s->hGuiDC, hOldPenGUI );
      }
      DeleteObject( hPen );
   }
}

//-------------------------------------------------------------------//
//                  1      2       3       4        5        6       7       8       9      10        11
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor, aPxlOff )
//
HB_FUNC( WVT_DRAWLINE )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 11,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 11,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 11,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 11,4 );

   int      iOrient, iFormat, iAlign, iStyle, iThick;
   int      x, y, iOffset;
   COLORREF cr;
   HPEN     hPen, hOldPen, hOldPenGUI;

   //   Resolve Parameters
   iOrient = ISNIL( 5 ) ? 0 : hb_parni( 5 );
   iFormat = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iAlign  = ISNIL( 7 ) ? 0 : hb_parni( 7 );
   iStyle  = ISNIL( 8 ) ? 0 : hb_parni( 8 );
   iThick  = ISNIL( 9 ) ? 0 : hb_parni( 9 );
   cr      = ISNIL(10 ) ? 0 : ( COLORREF ) hb_parnl( 10 );

   x       = iLeft ;
   y       = iTop ;

   switch ( iAlign )
   {
      case 0:                  // Center
         if ( iOrient == 0 )   // Horizontal
         {
            iOffset = ( ( iBottom - iTop ) / 2 ) ;
            y       = iTop + iOffset ;
         }
         else
         {
            iOffset = ( ( iRight - iLeft ) / 2 ) ;
            x       = iLeft + iOffset ;
         }
         break;

      case 1:                  // Top
         break;

      case 2:                  // bottom
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
         {
            y = iBottom - 1;
         }
         else
         {
            y = iBottom;
         }
         break;

      case 3:                  // Left
         break;

      case 4:                  // Right
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
         {
            x = iRight - 1;
         }
         else
         {
            x = iRight;
         }
         break;
   }

   hPen = CreatePen( iStyle, iThick, cr );
   hOldPen = (HPEN) SelectObject( _s->hdc, hPen );
   hOldPenGUI = _s->bGui ? (HPEN) SelectObject( _s->hGuiDC, hPen ) : 0;

   switch ( iFormat )
   {
      case 0:                                       // Raised
      {
         if ( iOrient == 0 )                        //  Horizontal
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );

            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, iRight, y );
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y+1, NULL );
               LineTo( _s->hGuiDC, iRight, y+1 );
            }
         }
         else                                       //  Vertical
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, x, iBottom );
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x+1, y, NULL );
               LineTo( _s->hGuiDC, x+1, iBottom );
            }
         }
      }
      break;

      case 1:                                      // Recessed
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, iRight, y );
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x, y+1, NULL );
               LineTo( _s->hGuiDC, iRight, y+1 );
            }
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, x, iBottom );
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x+1, y, NULL );
               LineTo( _s->hGuiDC, x+1, iBottom );
            }
         }
      }
      break;

      case 2:                                      // Plain
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, iRight, y );
            }
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, x, iBottom );
            }
         }
      }
      break;
   }

   SelectObject( _s->hdc, hOldPen );
   if ( hOldPenGUI )
   {
      SelectObject( _s->hGuiDC, hOldPenGUI );
   }
   DeleteObject( hPen );
   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Inside the area requested!
//    Wvt_DrawEllipse( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWELLIPSE )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   SelectObject( _s->hdc, _s->currentBrush );
   SelectObject( _s->hdc, _s->currentPen   );

   hb_retl( Ellipse( _s->hdc, iLeft, iTop, iRight, iBottom ) );
   if ( _s->bGui )
   {
      hb_retl( Ellipse( _s->hGuiDC, iLeft, iTop, iRight, iBottom ) );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawRectangle( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWRECTANGLE )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   SelectObject( _s->hdc, _s->currentBrush );
   SelectObject( _s->hdc, _s->currentPen );

   hb_retl( Rectangle( _s->hdc, iLeft, iTop, iRight, iBottom ) );
   if ( _s->bGui )
   {
      hb_retl( Rectangle( _s->hGuiDC, iLeft, iTop, iRight, iBottom ) );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawRoundRect( nTop, nLeft, nBottom, nRight, aPxlOff, nRoundHeight, nRoundWidth )
//
HB_FUNC( WVT_DRAWROUNDRECT )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );
   int iHt, iWd;

   iHt     = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iWd     = ISNIL( 7 ) ? 0 : hb_parni( 7 );

   SelectObject( _s->hdc, _s->currentBrush );
   SelectObject( _s->hdc, _s->currentPen   );

   hb_retl( RoundRect( _s->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt ) );
   if ( _s->bGui )
   {
      hb_retl( RoundRect( _s->hGuiDC, iLeft, iTop, iRight, iBottom, iWd, iHt ) );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawFocusRect( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVT_DRAWFOCUSRECT )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );
   RECT rc = { 0,0,0,0 };

   rc.left   = iLeft;
   rc.top    = iTop;
   rc.right  = iRight;
   rc.bottom = iBottom;

   hb_retl( DrawFocusRect( _s->hdc, &rc ) );
   if ( _s->bGui )
   {
      hb_retl( DrawFocusRect( _s->hGuiDC, &rc ) );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawColorRect( nTop, nLeft, nBottom, nRight, aPxlOff, nRGB )
//
HB_FUNC( WVT_DRAWCOLORRECT )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );
   RECT rc = { 0,0,0,0 };
   HBRUSH hBrush;

   hBrush = CreateSolidBrush( ( COLORREF ) hb_parnl( 6 ) );

   if ( hBrush )
   {
      rc.left   = iLeft;
      rc.top    = iTop;
      rc.right  = iRight;
      rc.bottom = iBottom;

      hb_retl( FillRect( _s->hdc, &rc, hBrush ) );
      if ( _s->bGui )
      {
         hb_retl( FillRect( _s->hGuiDC, &rc, hBrush ) );
      }

      DeleteObject( hBrush );
   }
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawGridHorz( nTop, nLeft, nRight, nRows )
//
HB_FUNC( WVT_DRAWGRIDHORZ )
{
   int   iAtRow = hb_parni( 1 );
   int   iRows  = hb_parni( 4 );
   int   i, y;
   int   iLeft, iRight;

   iLeft  = ( hb_parni( 2 ) * _s->PTEXTSIZE.x );
   iRight = ( ( ( hb_parni( 3 ) + 1 ) * _s->PTEXTSIZE.x ) - 1 );

   SelectObject( _s->hdc, _s->currentPen );

   for ( i = 0; i < iRows; i++ )
   {
      y = ( ( iAtRow ) * _s->PTEXTSIZE.y );

      MoveToEx( _s->hdc, iLeft, y, NULL );
      LineTo( _s->hdc, iRight, y );

      iAtRow++;
   }

   if ( _s->bGui )
   {
      iAtRow = hb_parni( 1 );

      SelectObject( _s->hGuiDC, _s->currentPen );

      for ( i = 0; i < iRows; i++ )
      {
         y = ( ( iAtRow ) * _s->PTEXTSIZE.y );

         MoveToEx( _s->hGuiDC, iLeft, y, NULL );
         LineTo( _s->hGuiDC, iRight, y );

         iAtRow++;
      }
   }
   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//     Wvt_DrawGridVert( nTop, nBottom, aCols, nCols )
//
HB_FUNC( WVT_DRAWGRIDVERT )
{
   int iTop, iBottom, x, i, iCharHeight, iCharWidth;
   int iTabs = hb_parni( 4 );

   if ( ! iTabs )
   {
      hb_retl( FALSE );
   }

   iCharWidth  = _s->PTEXTSIZE.x;
   iCharHeight = _s->PTEXTSIZE.y;

   iTop    = ( hb_parni( 1 ) * iCharHeight );
   iBottom = ( ( hb_parni( 2 ) + 1 ) * iCharHeight ) - 1;

   SelectObject( _s->hdc, _s->currentPen );

   for ( i = 1; i <= iTabs; i++ )
   {
      x = ( hb_parni( 3,i ) * iCharWidth );

      MoveToEx( _s->hdc, x, iTop, NULL );
      LineTo( _s->hdc, x, iBottom );
   }

   if ( _s->bGui )
   {
      SelectObject( _s->hGuiDC, _s->currentPen );
      for ( i = 1; i <= iTabs; i++ )
      {
         x = ( hb_parni( 3,i ) * iCharWidth );

         MoveToEx( _s->hGuiDC, x, iTop, NULL );
         LineTo( _s->hGuiDC, x, iBottom );
      }
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawButton( nTop, nLeft, nBottom, nRight, cText, cnImage, ;
//                    nFormat, nTextColor, nBkColor, nImageAt ) ;
//
HB_FUNC( WVT_DRAWBUTTON )
{
   SIZE     sz = { 0,0 };
   POINT    xy = { 0,0 };
   RECT     rc = { 0,0,0,0 };
   int      iTop, iLeft, iBottom, iRight;
   int      iAlign;
   int      iTextHeight /*, iTextWidth */ ;
   int      iImageWidth, iImageHeight;
   LOGBRUSH lb = { 0,0,0 };
   HBRUSH   hBrush;
   IPicture *iPicture;

   BOOL     bText     = ISCHAR( 5 );
   BOOL     bImage    = !( ISNIL( 6 ) );
   int      iFormat   = ISNIL(  7 ) ? 0 : hb_parni( 7 );
   COLORREF textColor = ISNIL(  8 ) ? hb_wvt_gtGetColorData( 0 ) : ( COLORREF ) hb_parnl( 8 ) ;
   COLORREF bkColor   = ISNIL(  9 ) ? hb_wvt_gtGetColorData( 7 ) : ( COLORREF ) hb_parnl( 9 ) ;
   // int      iImageAt  = ISNIL( 10 ) ? 0 : hb_parni( 10 );

   xy         = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop       = xy.y;
   iLeft      = xy.x;
   xy         = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom    = xy.y-1;
   iRight     = xy.x-1;

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;

   hBrush     = CreateBrushIndirect( &lb );

   rc.left    = iLeft ;
   rc.top     = iTop ;
   rc.right   = iRight  + 1;
   rc.bottom  = iBottom + 1;

   FillRect( _s->hdc, &rc, hBrush );
   if ( _s->bGui )
   {
      FillRect( _s->hGuiDC, &rc, hBrush );
   }
   DeleteObject( hBrush );

   switch ( iFormat )
   {
      case 1:
         hb_wvt_DrawBoxRecessed( _s->hdc, iTop+1, iLeft+1, iBottom-1, iRight-1 );
         if ( _s->bGui )
         {
            hb_wvt_DrawBoxRecessed( _s->hGuiDC, iTop+1, iLeft+1, iBottom-1, iRight-1 );
         }
         break;
      case 2:
         break;
      case 3:
         hb_wvt_DrawOutline( _s->hdc, iTop, iLeft, iBottom, iRight );
         if ( _s->bGui )
         {
            hb_wvt_DrawOutline( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
         }
         break;
      case 4:
         break;
      default:
         hb_wvt_DrawBoxRaised( _s->hdc, iTop+1, iLeft+1, iBottom-1, iRight-1 );
         if ( _s->bGui )
         {
            hb_wvt_DrawBoxRaised( _s->hGuiDC, iTop+1, iLeft+1, iBottom-1, iRight-1 );
         }
         break;
   }

   if ( bText )
   {
      LPTSTR text = HB_TCHAR_CONVTO( hb_parc( 5 ) );

      SelectObject( _s->hdc, GetStockObject( DEFAULT_GUI_FONT ) );

      //GetTextExtentPoint32( _s->hdc, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), &sz );
      GetTextExtentPoint32( _s->hdc, text, lstrlen( text ), &sz );

      // iTextWidth   = sz.cx;
      iTextHeight  = sz.cy;

      xy.x = iLeft + ( ( iRight - iLeft + 1 ) / 2 ) ;

      if ( bImage )
      {
         xy.y = ( iBottom - 2 - iTextHeight );
      }
      else
      {
         xy.y = iTop + ( ( iBottom - iTop + 1 - iTextHeight ) / 2 ) ;
      }

      if ( iFormat == 1 )
      {
         xy.x = xy.x + 2;
         xy.y = xy.y + 2;
      }

      iAlign = TA_CENTER + TA_TOP ;

      SetTextAlign( _s->hdc, iAlign );
      SetBkMode( _s->hdc, TRANSPARENT );
      SetTextColor( _s->hdc, textColor );

      //ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), NULL );
      ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, text, lstrlen( text ), NULL );
      if ( _s->bGui )
      {
         SelectObject( _s->hGuiDC, GetStockObject( DEFAULT_GUI_FONT ) );
         SetTextAlign( _s->hGuiDC, iAlign );
         SetBkMode( _s->hGuiDC, TRANSPARENT );
         SetTextColor( _s->hGuiDC, textColor );

         //ExtTextOut( _s->hGuiDC, xy.x, xy.y, 0, NULL, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), NULL );
         ExtTextOut( _s->hGuiDC, xy.x, xy.y, 0, NULL, text, lstrlen( text ), NULL );
      }
      HB_TCHAR_FREE( text );
   }
   else
   {
      iTextHeight = -1;
   }

   if ( bImage )
   {
      iImageWidth  = ( iRight - iLeft + 1 - 8 );
      iImageHeight = ( iBottom - iTop + 1 - 8 - iTextHeight );

      if ( ISNUM( 6 ) )
      {
         iPicture = _s->iPicture[ hb_parni( 6 ) - 1 ];
         hb_wvt_gtRenderPicture( iLeft+4, iTop+4, iImageWidth, iImageHeight, iPicture );
      }
      else
      {
         hb_wvt_DrawImage( _s->hdc, iLeft+4, iTop+4, iImageWidth, iImageHeight, hb_parcx( 6 ) );
         if ( _s->bGui )
         {
            hb_wvt_DrawImage( _s->hGuiDC, iLeft+4, iTop+4, iImageWidth, iImageHeight, hb_parcx( 6 ) );
         }
      }
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_DRAWSTATUSBAR )
{
   int   iPanels   = hb_parni( 1 );
   int   i, iNext;
   int   iTop, iLeft, iBottom, iRight;
   POINT xy = { 0,0 };

   iNext = 0;

   for ( i = 0; i < iPanels; i++ )
   {
      xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2, iNext+2 ), hb_parni( 2, iNext+1 ) );
      iTop    = xy.y;
      iLeft   = xy.x + 1;

      xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2, iNext+4 ), hb_parni( 2, iNext+3 )+1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 2;

      SelectObject( _s->hdc, _s->penWhite );

      MoveToEx( _s->hdc, iRight, iTop, NULL );            // Right
      LineTo( _s->hdc, iRight, iBottom );

      MoveToEx( _s->hdc, iLeft, iBottom, NULL );          // Bottom
      LineTo( _s->hdc, iRight, iBottom );

      SelectObject( _s->hdc, _s->penDarkGray );

      MoveToEx( _s->hdc, iLeft, iTop, NULL );             // Left
      LineTo( _s->hdc, iLeft, iBottom );

      MoveToEx( _s->hdc, iLeft, iTop, NULL );             // Top
      LineTo( _s->hdc, iRight, iTop );

      iNext = iNext + 4;
   }

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2, 4 * iPanels ), hb_parni( 2, ( 4 * iPanels ) - 1 )+1 );
   iTop    = xy.y - 2;
   iLeft   = xy.x - 2;
   iBottom = iTop;
   iRight  = iLeft;

   SelectObject( _s->hdc, _s->penBlack );

   MoveToEx( _s->hdc, iLeft-4, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-4 );
   MoveToEx( _s->hdc, iLeft-7, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-7 );
   MoveToEx( _s->hdc, iLeft-10, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-10 );

   SelectObject( _s->hdc, _s->penWhite );

   MoveToEx( _s->hdc, iLeft-5, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-5 );
   MoveToEx( _s->hdc, iLeft-8, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-8 );
   MoveToEx( _s->hdc, iLeft-11, iBottom, NULL );
   LineTo( _s->hdc, iRight, iTop-11 );
}

//-------------------------------------------------------------------//
//
//  Wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aPxlOff ) -> lOk
//  nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right }
//
HB_FUNC( WVT_DRAWPICTURE )
{
   POINT    xy = { 0,0 };
   int      iTop, iLeft, iBottom, iRight;
   int      iSlot   = hb_parni( 5 ) - 1;

   if ( iSlot < WVT_PICTURES_MAX )
   {
      if ( _s->iPicture[ iSlot ] )
      {
         xy       = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
         iTop     = xy.y + hb_parni( 6,1 );
         iLeft    = xy.x + hb_parni( 6,2 );

         xy       = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
         iBottom  = xy.y-1 + hb_parni( 6,3 );
         iRight   = xy.x-1 + hb_parni( 6,4 );

         hb_retl( hb_wvt_gtRenderPicture( iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, _s->iPicture[ iSlot ] ) );
      }
   }
}

//-------------------------------------------------------------------//
//
//    WVT_DRAWLABELEX( nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont )
//
HB_FUNC( WVT_DRAWLABELEX )
{
   POINT    xy = { 0,0 };
   int      iSlot = hb_parni( 7 ) - 1;

   if ( _s->hUserFonts[ iSlot ] )
   {
      LPTSTR text = HB_TCHAR_CONVTO( hb_parc( 3 ) );
      xy          = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );

      SetBkColor( _s->hdc, ISNIL( 6 ) ? _s->background : ( COLORREF ) hb_parnl( 6 ) );
      SetTextColor( _s->hdc, ISNIL( 5 ) ? _s->foreground : ( COLORREF ) hb_parnl( 5 ) );
      SetTextAlign( _s->hdc, ( ISNIL( 4 ) ? TA_LEFT : hb_parni( 4 ) ) );
      SelectObject( _s->hdc, _s->hUserFonts[ iSlot ] );

      //ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 3 ), strlen( hb_parcx( 3 ) ), NULL );
      ExtTextOut( _s->hdc, xy.x, xy.y, 0, NULL, text, lstrlen( text ), NULL );
      if ( _s->bGui )
      {
         SetBkColor( _s->hGuiDC, ISNIL( 6 ) ? _s->background : ( COLORREF ) hb_parnl( 6 ) );
         SetTextColor( _s->hGuiDC, ISNIL( 5 ) ? _s->foreground : ( COLORREF ) hb_parnl( 5 ) );
         SetTextAlign( _s->hGuiDC, ( ISNIL( 4 ) ? TA_LEFT : hb_parni( 4 ) ) );
         SelectObject( _s->hGuiDC, _s->hUserFonts[ iSlot ] );

         //ExtTextOut( _s->hGuiDC, xy.x, xy.y, 0, NULL, hb_parcx( 3 ), strlen( hb_parcx( 3 ) ), NULL );
         ExtTextOut( _s->hGuiDC, xy.x, xy.y, 0, NULL, text, lstrlen( text ), NULL );
      }
      HB_TCHAR_FREE( text );
      hb_retl( TRUE );
   }

   hb_retl( FALSE );
}

//-------------------------------------------------------------------//
//                  1      2       3       4        5        6       7       8
//   Wvt_DrawLine( nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen )
//
HB_FUNC( WVT_DRAWLINEEX )
{
   POINT    xy = { 0,0 };
   int      iTop, iLeft, iBottom, iRight, iOffset ;
   int      iOrient, iFormat, iAlign ;
   int      x, y;
   HPEN     hPen;
   int      iSlot = hb_parni( 8 ) - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   //   Resolve Parameters
   iOrient = ISNIL( 5 ) ? 0 : hb_parni( 5 );
   iFormat = ISNIL( 6 ) ? 0 : hb_parni( 6 );
   iAlign  = ISNIL( 7 ) ? 0 : hb_parni( 7 );

   x       = iLeft ;
   y       = iTop ;

   switch ( iAlign )
   {
      case 0:                  // Center
      {
         if ( iOrient == 0 )   // Horizontal
         {
            iOffset = ( ( iBottom - iTop ) / 2 ) ;
            y       = iTop + iOffset ;
         }
         else
         {
            iOffset = ( ( iRight - iLeft ) / 2 ) ;
            x       = iLeft + iOffset ;
         }
      }
      break;

      case 1:                  // Top
      break;

      case 2:                  // bottom
      {
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
         {
            y = iBottom - 1;
         }
         else
         {
            y = iBottom;
         }
      }
      break;

      case 3:                  // Left
      break;

      case 4:                  // Right
      {
         if ( iFormat == 0 || iFormat == 1 )  // Raised/Recessd
         {
            x = iRight - 1;
         }
         else
         {
            x = iRight;
         }
      }
      break;
   }

   hPen = _s->hUserPens[ iSlot ];

   switch ( iFormat )
   {
      case 0:                                       // Raised
      {
         if ( iOrient == 0 )                        //  Horizontal
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, iRight, y );
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y+1, NULL );
               LineTo( _s->hGuiDC, iRight, y+1 );
            }
         }
         else                                       //  Vertical
         {
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, x, iBottom );
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x+1, y, NULL );
               LineTo( _s->hGuiDC, x+1, iBottom );
            }
         }
      }
      break;

      case 1:                                      // Recessed
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x, y+1, NULL );
            LineTo( _s->hdc, iRight, y+1 );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, iRight, y );
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x, y+1, NULL );
               LineTo( _s->hGuiDC, iRight, y+1 );
            }
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            SelectObject( _s->hdc, _s->penWhite );
            MoveToEx( _s->hdc, x+1, y, NULL );
            LineTo( _s->hdc, x+1, iBottom );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, x, iBottom );
               SelectObject( _s->hGuiDC, _s->penWhite );
               MoveToEx( _s->hGuiDC, x+1, y, NULL );
               LineTo( _s->hGuiDC, x+1, iBottom );
            }
         }
      }
      break;

      case 2:                                      // Plain
      {
         if ( iOrient == 0 )                       // Horizontal
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, iRight, y );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, iRight, y );
            }
         }
         else                                      //  Vertical
         {
            SelectObject( _s->hdc, hPen );
            MoveToEx( _s->hdc, x, y, NULL );
            LineTo( _s->hdc, x, iBottom );
            if ( _s->bGui )
            {
               SelectObject( _s->hGuiDC, hPen );
               MoveToEx( _s->hGuiDC, x, y, NULL );
               LineTo( _s->hGuiDC, x, iBottom );
            }
         }
       }
      break;
   }

   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//    Wvt_DrawOutlineEx( nTop, nLeft, nBottom, nRight, nSlotPen )
//
HB_FUNC( WVT_DRAWOUTLINEEX )
{
   POINT xy = { 0,0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iSlot = hb_parni( 5 ) - 1;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y - 1;
   iLeft   = xy.x - 1;
   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
   iBottom = xy.y;
   iRight  = xy.x;

   if ( _s->hUserPens[ iSlot ] )
   {
      SelectObject( _s->hdc, _s->hUserPens[ iSlot ] );
   }
   else
   {
      SelectObject( _s->hdc, _s->penBlack );
   }

   hb_wvt_DrawOutline( _s->hdc, iTop, iLeft, iBottom, iRight );
   if ( _s->bGui )
   {
      hb_wvt_DrawOutline( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
   }
}

//-------------------------------------------------------------------//
//
//   Wvt_LoadPicture( nSlot, cFilePic )
//
HB_FUNC( WVT_LOADPICTURE )
{
   IPicture * iPicture = hb_wvt_gtLoadPicture( hb_parcx( 2 ) );
   BOOL       bResult  = FALSE;
   int        iSlot    = hb_parni( 1 ) - 1 ;

   if ( iPicture )
   {
      if ( _s->iPicture[ iSlot ] )
      {
         hb_wvt_gtDestroyPicture( _s->iPicture[ iSlot ] );
      }

      _s->iPicture[ iSlot ] = iPicture;
      bResult = TRUE;
   }
   hb_retl( bResult );
}

//-------------------------------------------------------------------//

HB_FUNC( WVT_LOADPICTUREFROMRESOURCE )
{
   IPicture * iPicture = hb_wvt_gtLoadPictureFromResource( hb_parcx( 2 ),hb_parcx( 3 ) );
   BOOL       bResult  = FALSE;
   int        iSlot    = hb_parni( 1 ) - 1 ;

   if ( iPicture )
   {
      if ( _s->iPicture[ iSlot ] )
      {
         hb_wvt_gtDestroyPicture( _s->iPicture[ iSlot ] );
      }

      _s->iPicture[ iSlot ] = iPicture;
      bResult = TRUE;
   }
   hb_retl( bResult );
}

//-------------------------------------------------------------------//
//
// Wvt_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout,
//               nCharSet, nQuality, nEscapement )
//
HB_FUNC( WVT_LOADFONT )
{
   LOGFONT  logfont;// = { 0 };
   int      iSlot = hb_parni( 1 ) - 1;
   HFONT    hFont;

   logfont.lfEscapement     = ( ISNIL( 11 ) ? 0 : ( hb_parni( 11 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL(  5 ) ? 0 : hb_parni( 5 ) );
   logfont.lfItalic         = ( ISNIL(  6 ) ? 0 : hb_parl(  6 ) );
   logfont.lfUnderline      = ( ISNIL(  7 ) ? 0 : hb_parl(  7 ) );
   logfont.lfStrikeOut      = ( ISNIL(  8 ) ? 0 : hb_parl(  8 ) );
   logfont.lfCharSet        = ( ISNIL(  9 ) ? _s->CodePage : hb_parni( 9 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 10 ) ? DEFAULT_QUALITY : hb_parni( 10 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  3 ) ? _s->fontHeight : hb_parni( 3 ) );
   logfont.lfWidth          = ( ISNIL(  4 ) ? ( _s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth ) : hb_parni( 4 ) );

   //strcpy( logfont.lfFaceName, ( ISNIL( 2 ) ? _s->fontFace : hb_parcx( 2 ) ) );
   HB_TCHAR_CPTO( logfont.lfFaceName, ( ISNIL( 2 ) ? _s->fontFace : hb_parcx( 2 ) ), sizeof( logfont.lfFaceName )-1 );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      if ( _s->hUserFonts[ iSlot ] )
      {
         DeleteObject( _s->hUserFonts[ iSlot ] );
      }
      _s->hUserFonts[ iSlot ] = hFont;
   }
}

//-------------------------------------------------------------------//
//
//  Wvt_LoadPen( nSlot, nStyle, nWidth, nRGBColor )
//
HB_FUNC( WVT_LOADPEN )
{
   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;
   int      iSlot = hb_parni( 1 ) - 1;

   iPenStyle = ISNIL( 2 ) ? 0 : hb_parni( 2 ) ;
   iPenWidth = ISNIL( 3 ) ? 0 : hb_parni( 3 );
   crColor   = ISNIL( 4 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 4 );

   hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      if ( _s->hUserPens[ iSlot ] )
      {
         DeleteObject( _s->hUserPens[ iSlot ] );
      }
      _s->hUserPens[ iSlot ] = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//-------------------------------------------------------------------//
//
//   aScr := Wvt_SaveScreen( nTop, nLeft, nBottom, nRight )
//
HB_FUNC( WVT_SAVESCREEN )
{
   HBITMAP  hBmp, oldBmp;
   POINT    xy = { 0,0 };
   int      iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   PHB_ITEM info = hb_itemArrayNew( 3 );
   PHB_ITEM temp = hb_itemNew( NULL );

   xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop      = xy.y;
   iLeft     = xy.x;

   xy        = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom   = xy.y-1;
   iRight    = xy.x-1;

   iWidth    = iRight - iLeft + 1;
   iHeight   = iBottom - iTop + 1;

   hBmp      = CreateCompatibleBitmap( _s->hdc, iWidth, iHeight ) ;

   oldBmp = (HBITMAP) SelectObject( _s->hCompDC, hBmp );
   BitBlt( _s->hCompDC, 0, 0, iWidth, iHeight, _s->hdc, iLeft, iTop, SRCCOPY );
   SelectObject( _s->hCompDC, oldBmp );

   hb_arraySet( info, 1, hb_itemPutNI( temp, iWidth ) );
   hb_arraySet( info, 2, hb_itemPutNI( temp, iHeight ) );
   hb_arraySet( info, 3, hb_itemPutNL( temp, ( ULONG ) hBmp ) );
   hb_itemRelease( temp );

   hb_itemReturn( info );
   hb_itemRelease( info );
}

//-------------------------------------------------------------------//
//
//   Wvt_RestScreen( nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP )
//
HB_FUNC( WVT_RESTSCREEN )
{
   POINT   xy = { 0,0 };
   int     iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   HBITMAP hBmp;

   BOOL    bResult = FALSE;
   BOOL    bDoNotDestroyBMP = ISNIL( 6 ) ? FALSE : hb_parl( 6 );

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_wvt_gtGetXYFromColRow( hb_parni( 4 ) + 1, hb_parni( 3 ) + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWidth  = iRight - iLeft + 1 ;
   iHeight = iBottom - iTop + 1 ;

   hBmp    = (HBITMAP) SelectObject( _s->hCompDC, ( HBITMAP ) hb_parnl( 5,3 ) );
   if ( hBmp )
   {
      if ( ( iWidth == hb_parni( 5,1 ) )  && ( iHeight == hb_parni( 5,2 ) ) )
      {
         if ( BitBlt( _s->hdc,
                      iLeft,
                      iTop,
                      iWidth,
                      iHeight,
                      _s->hCompDC,
                      0,
                      0,
                      SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
      else
      {
         if ( StretchBlt( _s->hdc,
                          iLeft,
                          iTop,
                          iWidth,
                          iHeight,
                          _s->hCompDC,
                          0,
                          0,
                          hb_parni( 5,1 ),
                          hb_parni( 5,2 ),
                          SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
   }

   SelectObject( _s->hCompDC, hBmp );

   if ( ! bDoNotDestroyBMP )
   {
      DeleteObject( ( HBITMAP ) hb_parnl( 5,3 ) );
   }

   hb_retl( bResult );
}

//-------------------------------------------------------------------//
//
// Wvt_CreateFont( cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,
//                 lStrikeout, nCharSet, nQuality, nEscapement )
//
HB_FUNC( WVT_CREATEFONT )
{
   LOGFONT  logfont;// = { 0,0,0 };
   HFONT    hFont;

   logfont.lfEscapement     = ( ISNIL( 10 ) ? 0 : ( hb_parni( 10 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( ISNIL(  4 ) ? 0 : hb_parni( 4 ) );
   logfont.lfItalic         = ( ISNIL(  5 ) ? 0 : hb_parl(  5 ) );
   logfont.lfUnderline      = ( ISNIL(  6 ) ? 0 : hb_parl(  6 ) );
   logfont.lfStrikeOut      = ( ISNIL(  7 ) ? 0 : hb_parl(  7 ) );
   logfont.lfCharSet        = ( ISNIL(  8 ) ? _s->CodePage : hb_parni( 8 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( ISNIL( 9 ) ? DEFAULT_QUALITY : hb_parni( 9 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( ISNIL(  2 ) ? _s->fontHeight : hb_parni( 2 ) );
   logfont.lfWidth          = ( ISNIL(  3 ) ? ( _s->fontWidth < 0 ? -_s->fontWidth : _s->fontWidth ) : hb_parni( 3 ) );

   //strcpy( logfont.lfFaceName, ( ISNIL( 1 ) ? _s->fontFace : hb_parcx( 1 ) ) );
   HB_TCHAR_CPTO( logfont.lfFaceName, ( ISNIL( 1 ) ? _s->fontFace : hb_parcx( 1 ) ), sizeof( logfont.lfFaceName )-1 );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      hb_retnl( ( ULONG ) hFont );
   }
   else
   {
      hb_retnl( 0 );
   }
}

//-------------------------------------------------------------------//
//
//    WVT_DRAWLABELOBJ( nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor, hFont )
//
HB_FUNC( WVT_DRAWLABELOBJ )
{
   POINT    xy = { 0,0 };
   RECT     rect = { 0,0,0,0 };
   int      iTop, iLeft, iBottom, iRight, x, y;
   int      iAlignHorz, iAlignVert, iAlignH, iAlignV;
   UINT     uiOptions;
   SIZE     sz = { 0,0 };
   LPTSTR   text = HB_TCHAR_CONVTO( hb_parc( 5 ) );

   xy           = hb_wvt_gtGetXYFromColRow( hb_parni( 2 ), hb_parni( 1 ) );
   iTop         = xy.y;
   iLeft        = xy.x;
   xy           = hb_wvt_gtGetXYFromColRow( hb_parni( 4 )+1, hb_parni( 3 )+1 );
   iBottom      = xy.y - 1;
   iRight       = xy.x - 1;

   iAlignHorz   = hb_parni( 6 ); /* default is 0 */
   iAlignVert   = hb_parni( 7 ); /* default is 0 */

   SetTextColor( _s->hdc, ISNIL( 8 ) ? _s->foreground : ( COLORREF ) hb_parnl( 8 ) );
   SetBkColor( _s->hdc, ISNIL( 9 ) ? _s->background : ( COLORREF ) hb_parnl( 9 ) );
   SelectObject( _s->hdc, ( HFONT ) hb_parnl( 10 ) );

   //GetTextExtentPoint32( _s->hdc, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), &sz );
   GetTextExtentPoint32( _s->hdc, text, lstrlen( text ), &sz );

   x = iLeft;
   y = iTop;

   switch ( iAlignHorz )
   {
      case 0:
         iAlignH = TA_LEFT;
         break;

      case 1:
         iAlignH = TA_RIGHT;
         x = iRight;
         break;

      case 2:
         iAlignH = TA_CENTER;
         x = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         break;

      default:
         iAlignH = 0;
   }

   iAlignV = TA_TOP;

   switch ( iAlignVert )
   {
      case 1:
         y = iBottom - sz.cy;
         break;

      case 2:
         y = iTop + ( ( iBottom - iTop + 1 - sz.cy ) / 2 );
         break;
   }

   SetTextAlign( _s->hdc, iAlignH | iAlignV );

   rect.top     = iTop;
   rect.left    = iLeft;
   rect.bottom  = iBottom;
   rect.right   = iRight;

   uiOptions    = ETO_CLIPPED | ETO_OPAQUE ;

   //  Ground is Ready, Draw Text
   //
   //ExtTextOut( _s->hdc, x, y, uiOptions, &rect, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), NULL );
   ExtTextOut( _s->hdc, x, y, uiOptions, &rect, text, lstrlen( text ), NULL );
   if ( _s->bGui )
   {
      SetTextColor( _s->hGuiDC, ISNIL( 8 ) ? _s->foreground : ( COLORREF ) hb_parnl( 8 ) );
      SetBkColor( _s->hGuiDC, ISNIL( 9 ) ? _s->background : ( COLORREF ) hb_parnl( 9 ) );
      SelectObject( _s->hGuiDC, ( HFONT ) hb_parnl( 10 ) );
      SetTextAlign( _s->hGuiDC, iAlignH | iAlignV );

      //ExtTextOut( _s->hGuiDC, x, y, uiOptions, &rect, hb_parcx( 5 ), strlen( hb_parcx( 5 ) ), NULL );
      ExtTextOut( _s->hGuiDC, x, y, uiOptions, &rect, text, lstrlen( text ), NULL );
   }
   HB_TCHAR_FREE( text );
   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//    nState 0 Flat, 1 Raised, 2 Recessed
//    Wvt_DrawToolButtonState( nTop, nLeft, nBottom, nRight, aPxlOff, nState )
//
HB_FUNC( WVT_DRAWTOOLBUTTONSTATE )
{
   int iTop     = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft    = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom  = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight   = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   switch ( hb_parni( 6 ) )
   {
      case 0:     // Flat
      {
         hb_wvt_DrawToolButtonFlat( _s->hdc, iTop, iLeft, iBottom, iRight );
         if ( _s->bGui )
         {
            hb_wvt_DrawToolButtonFlat( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
         }
      }
      break;

      case 1:     // Raised
      {
         hb_wvt_DrawToolButtonUp( _s->hdc, iTop, iLeft, iBottom, iRight );
         if ( _s->bGui )
         {
            hb_wvt_DrawToolButtonUp( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
         }
      }
      break;

      case 2:     // Recessed
      {
         hb_wvt_DrawToolButtonDown( _s->hdc, iTop, iLeft, iBottom, iRight );
         if ( _s->bGui )
         {
            hb_wvt_DrawToolButtonDown( _s->hGuiDC, iTop, iLeft, iBottom, iRight );
         }
      }
      break;
   }
   hb_retl( TRUE );
}

//-------------------------------------------------------------------//
//
//   Wvt_DrawScrollButton( nTop, nLeft, nBottom, nRight, aOffPixels, nTLBR, lDepressed )
//
HB_FUNC( WVT_DRAWSCROLLBUTTON )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   POINT    * Point;
   POINT    xy = { 0,0 };
   int      iHeight, iOff;
   BOOL     bDepressed = ISNIL( 7 ) ? FALSE : hb_parl( 7 ) ;

   Point      = ( POINT * ) hb_xgrab( 3 * sizeof( POINT ) );
   iOff       = 6;

   iHeight    = iBottom - iTop + 1;

   if ( bDepressed )
   {
      hb_wvt_DrawBoxRecessed( _s->hdc, iTop+1, iLeft+1, iBottom-2, iRight-2 );
      if ( _s->bGui )
      {
         hb_wvt_DrawBoxRecessed( _s->hGuiDC, iTop+1, iLeft+1, iBottom-2, iRight-2 );
      }
   }
   else
   {
      hb_wvt_DrawBoxRaised( _s->hdc, iTop+1, iLeft+1, iBottom-2, iRight-2 );
      if ( _s->bGui )
      {
         hb_wvt_DrawBoxRaised( _s->hGuiDC, iTop+1, iLeft+1, iBottom-2, iRight-2 );
      }
   }

   switch ( hb_parni( 6 ) )
   {
      case 1:   // Top
      {
         xy.y       = iTop + iOff - 1;
         xy.x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         Point[ 0 ] = xy ;
         xy.y       = iBottom - iOff - 1;
         xy.x       = iLeft + iOff - 1;
         Point[ 1 ] = xy;
         xy.x       = iRight - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;

      case 2:  // Left
      {
         xy.y       = iTop + ( ( iBottom - iTop + 1 ) / 2 );
         xy.x       = iLeft + iOff;
         Point[ 0 ] = xy ;
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + iOff - 1;
         Point[ 1 ] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;

      case 3:  //  Bottom
      {
         xy.x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         xy.y       = iBottom - iOff;
         Point[ 0 ] = xy ;
         xy.x       = iLeft + iOff - 1;
         xy.y       = iBottom - iHeight + iOff + 1;
         Point[ 1 ] = xy;
         xy.x       = iRight - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;

      case 4:  // Right
      {
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + ( ( iBottom - iTop + 1 ) / 2 );
         Point[ 0 ] = xy ;
         xy.x       = iLeft + iOff + 1;
         xy.y       = iTop + iOff - 1;
         Point[ 1 ] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[ 2 ] = xy;
      }
      break;
   }

   SelectObject( _s->hdc, _s->solidBrush );
   Polygon( _s->hdc, Point, 3 );
   if ( _s->bGui )
   {
      SelectObject( _s->hGuiDC, _s->solidBrush );
      Polygon( _s->hGuiDC, Point, 3 );
   }

   hb_xfree( Point );
}

//-------------------------------------------------------------------//
//
//  Wvt_DrawScrollbarThumbVert( nTop, nLeft, nBottom, nRight, aPxlOff, nThumbPos )
//
HB_FUNC( WVT_DRAWSCROLLTHUMBVERT )
{
   int iTop        = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft       = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom     = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight      = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );
   int iTabTop, iTabLft, iTabBtm, iTabRgt;

   //  Background
   //
   SetBkMode( _s->hdc, OPAQUE );
   SetBkColor( _s->hdc, RGB( 230,230,230 ) );
   SelectObject( _s->hdc, _s->diagonalBrush );
   SelectObject( _s->hdc, _s->penNull );
   Rectangle( _s->hdc, iLeft, iTop, iRight+1, iBottom+1 );
   if ( _s->bGui )
   {
      SetBkMode( _s->hGuiDC, OPAQUE );
      SetBkColor( _s->hGuiDC, RGB( 230,230,230 ) );
      SelectObject( _s->hGuiDC, _s->diagonalBrush );
      SelectObject( _s->hGuiDC, _s->penNull );
      Rectangle( _s->hGuiDC, iLeft, iTop, iRight+1, iBottom+1 );
   }

   //  Thumb
   //
   iTabTop  = _s->PTEXTSIZE.y * hb_parni( 6 );
   iTabLft  = iLeft;
   iTabBtm  = iTabTop + _s->PTEXTSIZE.y - 1;
   iTabRgt  = iRight;

   SelectObject( _s->hdc, _s->wvtWhiteBrush );
   SelectObject( _s->hdc, _s->penGray );
   Rectangle( _s->hdc, iTabLft, iTabTop, iTabRgt+1, iTabBtm );
   if ( _s->bGui )
   {
      SelectObject( _s->hGuiDC, _s->wvtWhiteBrush );
      SelectObject( _s->hGuiDC, _s->penGray );
      Rectangle( _s->hGuiDC, iTabLft, iTabTop, iTabRgt+1, iTabBtm );
   }

   hb_wvt_DrawBoxRaised( _s->hdc, iTabTop+1, iTabLft+1, iTabBtm-2, iTabRgt-2 );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxRaised( _s->hGuiDC, iTabTop+1, iTabLft+1, iTabBtm-2, iTabRgt-2 );
   }
}

//-------------------------------------------------------------------//
//
//  Wvt_DrawScrollbarThumbHorz( nTop, nLeft, nBottom, nRight, aPxlOff, nThumbPos )
//
HB_FUNC( WVT_DRAWSCROLLTHUMBHORZ )
{
   int iTop        = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft       = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom     = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight      = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );
   int iThumbLeft, iThumbRight;;

   iThumbLeft  = _s->PTEXTSIZE.x * hb_parni( 6 );
   iThumbRight = iThumbLeft + ( _s->PTEXTSIZE.x * 2 ) - 1;

   //  Background
   //
   SetBkMode( _s->hdc, OPAQUE );
   SetBkColor( _s->hdc, RGB( 230,230,230 ) );
   SelectObject( _s->hdc, _s->diagonalBrush );
   SelectObject( _s->hdc, _s->penNull );
   Rectangle( _s->hdc, iLeft, iTop, iRight+1, iBottom+1 );
   if ( _s->bGui )
   {
      SetBkMode( _s->hGuiDC, OPAQUE );
      SetBkColor( _s->hGuiDC, RGB( 230,230,230 ) );
      SelectObject( _s->hGuiDC, _s->diagonalBrush );
      SelectObject( _s->hGuiDC, _s->penNull );
      Rectangle( _s->hGuiDC, iLeft, iTop, iRight+1, iBottom+1 );
   }

   //  Thumb
   //
   SelectObject( _s->hdc, _s->wvtWhiteBrush );
   SelectObject( _s->hdc, _s->penGray );
   Rectangle( _s->hdc, iThumbLeft, iTop, iThumbRight, iBottom );
   if ( _s->bGui )
   {
      SelectObject( _s->hGuiDC, _s->wvtWhiteBrush );
      SelectObject( _s->hGuiDC, _s->penGray );
      Rectangle( _s->hGuiDC, iThumbLeft, iTop, iThumbRight, iBottom );
   }

   hb_wvt_DrawBoxRaised( _s->hdc, iTop+1, iThumbLeft+1, iBottom-2, iThumbRight-2 );
   if ( _s->bGui )
   {
      hb_wvt_DrawBoxRaised( _s->hGuiDC, iTop+1, iThumbLeft+1, iBottom-2, iThumbRight-2 );
   }
}

//#if WINVER > 0x500
//-------------------------------------------------------------------//
//
//    Wvt_DrawShadedRect( nTop, nLeft, nBottom, nRight, aPxlOff, nHorVert, aRGBb, aRGBe  )
//
HB_FUNC( WVT_DRAWSHADEDRECT )
{
   BOOL bGF = FALSE;

   if ( _s->hMSImg32 )
   {
      TRIVERTEX     vert[ 2 ] ;
      GRADIENT_RECT gRect = { 0,0 };

      int iTop        = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
      int iLeft       = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
      int iBottom     = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
      int iRight      = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

      int iMode       = ISNIL( 6 ) ? GRADIENT_FILL_RECT_H : hb_parni( 6 ) ;

      vert[ 0 ].x     = iLeft;
      vert[ 0 ].y     = iTop;
      vert[ 0 ].Red   = hb_parni( 7,1 );
      vert[ 0 ].Green = hb_parni( 7,2 );
      vert[ 0 ].Blue  = hb_parni( 7,3 );
      vert[ 0 ].Alpha = hb_parni( 7,4 );

      vert[ 1 ].x     = iRight;
      vert[ 1 ].y     = iBottom;
      vert[ 1 ].Red   = hb_parni( 8,1 );
      vert[ 1 ].Green = hb_parni( 8,2 );
      vert[ 1 ].Blue  = hb_parni( 8,3 );
      vert[ 1 ].Alpha = hb_parni( 8,4 );

      gRect.UpperLeft = 0;
      gRect.LowerRight= 1;

      bGF = ( BOOL ) _s->pfnGF( _s->hdc, vert, 2, &gRect, 1, iMode );
      if ( _s->bGui )
      {
         bGF = ( BOOL ) _s->pfnGF( _s->hGuiDC, vert, 2, &gRect, 1, iMode );
      }
   }
   hb_retl( bGF );
}
//#endif

//-------------------------------------------------------------------//
//
//   Wvt_DrawTextBox( nTop, nLeft, nBottom, nRight, aPxlOff, cText, ;
//                    nAlignHorz, nAlignVert, nTextColor, nBackColor, ;
//                    nBackMode, hFont )
//
HB_FUNC( WVT_DRAWTEXTBOX )
{
   int iTop    = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int iLeft   = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int iBottom = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int iRight  = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );

   int iAlignHorz = hb_parni( 7 ); /* default to 0 */
   int iAlignH = 0;

   RECT     rc = { 0,0,0,0 };
   LPTSTR   text = HB_TCHAR_CONVTO( hb_parc( 6 ) );

   switch ( iAlignHorz )
   {
      case 0:
         iAlignH = DT_LEFT;
         break;

      case 1:
         iAlignH = DT_RIGHT;
         break;

      case 2:
         iAlignH = DT_CENTER;
         break;
   }

   rc.top       = iTop;
   rc.left      = iLeft;
   rc.bottom    = iBottom;
   rc.right     = iRight;

   SetTextAlign( _s->hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP );
   SetTextColor( _s->hdc, ISNIL( 9 ) ? _s->foreground : ( COLORREF ) hb_parnl( 9 ) );
   SetBkColor( _s->hdc, ISNIL( 10 ) ? _s->background : ( COLORREF ) hb_parnl( 10 ) );
   SetBkMode( _s->hdc, ISNIL( 11 ) ? OPAQUE : hb_parni( 11 ) );
   SelectObject( _s->hdc, ( HFONT ) hb_parnl( 12 ) );

   //DrawText( _s->hdc, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), &rc, iAlignH | DT_WORDBREAK | DT_TOP );
   DrawText( _s->hdc, text, lstrlen( text ), &rc, iAlignH | DT_WORDBREAK | DT_TOP );
   if ( _s->bGui )
   {
      SetTextAlign( _s->hGuiDC, TA_TOP | TA_LEFT | TA_NOUPDATECP );
      SetTextColor( _s->hGuiDC, ISNIL( 9 ) ? _s->foreground : ( COLORREF ) hb_parnl( 9 ) );
      SetBkColor( _s->hGuiDC, ISNIL( 10 ) ? _s->background : ( COLORREF ) hb_parnl( 10 ) );
      SetBkMode( _s->hGuiDC, ISNIL( 11 ) ? OPAQUE : hb_parni( 11 ) );
      SelectObject( _s->hGuiDC, ( HFONT ) hb_parnl( 12 ) );

      //DrawText( _s->hGuiDC, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), &rc, iAlignH | DT_WORDBREAK | DT_TOP );
      DrawText( _s->hGuiDC, text, lstrlen( text ), &rc, iAlignH | DT_WORDBREAK | DT_TOP );
   }
   HB_TCHAR_FREE( text );
}

//-------------------------------------------------------------------//
//
// Wvt_DrawProgressBar( nTop, nLeft, nBottom, nRight, aPxlOff, nPercent,;
//                      nBackColor, nBarColor, cImage, lVertical, nDirection )
//
HB_FUNC( WVT_DRAWPROGRESSBAR )
{
   int      iTop     = ( _s->PTEXTSIZE.y * hb_parni( 1 ) ) + hb_parni( 5,1 );
   int      iLeft    = ( _s->PTEXTSIZE.x * hb_parni( 2 ) ) + hb_parni( 5,2 );
   int      iBottom  = ( _s->PTEXTSIZE.y * ( hb_parni( 3 ) + 1 ) ) - 1 + hb_parni( 5,3 );
   int      iRight   = ( _s->PTEXTSIZE.x * ( hb_parni( 4 ) + 1 ) ) - 1 + hb_parni( 5,4 );
   int      iPercent,  iBarUpto, iDirection;
   BOOL     bVertical, bImage;
   COLORREF crBarColor;
   HBRUSH   hBrush;
   LOGBRUSH lb = { 0,0,0 };
   RECT     rc = { 0,0,0,0 };

   iPercent   = hb_parni( 6 );
   bImage     = ISNIL(  9 ) ? FALSE : TRUE ;
   bVertical  = ISNIL( 10 ) ? FALSE : hb_parl( 10 ) ;
   iDirection = ISNIL( 11 ) ? 0 : hb_parni( 11 );

   if ( bVertical )
   {
      if ( iDirection == 0 )
      {
         iBarUpto  = iTop + ( ( iBottom - iTop ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBarUpto;
         rc.right  = iRight;
      }
      else
      {
         iBarUpto  = iBottom - ( ( iBottom - iTop ) * iPercent / 100 );
         rc.top    = iBarUpto;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }
   else
   {
      if ( iDirection == 0 )
      {
         iBarUpto  = iLeft + ( ( iRight - iLeft ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iBarUpto;
      }
      else
      {
         iBarUpto  = iRight - ( ( iRight - iLeft ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iBarUpto;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }

   if ( bImage )
   {
      hb_wvt_DrawImage( _s->hdc, rc.left, rc.top, rc.right-rc.left+1, rc.bottom-rc.top+1, hb_parc( 9 ) );
      if ( _s->bGui )
      {
         hb_wvt_DrawImage( _s->hGuiDC, rc.left, rc.top, rc.right-rc.left+1, rc.bottom-rc.top+1, hb_parc( 9 ) );
      }
   }
   else
   {
      crBarColor  = ISNIL( 8 ) ? hb_wvt_gtGetColorData(  0 ) : ( COLORREF ) hb_parnl( 8 );

      lb.lbStyle  = BS_SOLID;
      lb.lbColor  = crBarColor;
      lb.lbHatch  = 0;

      hBrush      = CreateBrushIndirect( &lb );

      rc.bottom++;
      rc.right++;

      FillRect( _s->hdc, &rc, hBrush );
      if ( _s->bGui )
      {
         FillRect( _s->hGuiDC, &rc, hBrush );
      }
      DeleteObject( hBrush );
   }
}

//-------------------------------------------------------------------//


