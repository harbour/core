/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (wingdi.h) (alpha)
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

/* WinCE MSDN documentation:
      http://msdn.microsoft.com/en-us/library/aa923590.aspx
 */

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbwinuni.h"
#include "hbwapi.h"

POINT * hbwapi_par_POINT( POINT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   memset( p, 0, sizeof( POINT ) );

   if( HB_IS_HASH( pStru ) )
   {
      p->x = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "x" ) );
      p->y = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "y" ) );

      return p;
   }
   else if( HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 2 )
   {
      p->x = ( LONG ) hb_arrayGetNL( pStru, 1 );
      p->y = ( LONG ) hb_arrayGetNL( pStru, 2 );

      return p;
   }
   else
      return bMandatory ? p : NULL;
}

RECT * hbwapi_par_RECT( RECT * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   memset( p, 0, sizeof( RECT ) );

   if( HB_IS_HASH( pStru ) )
   {
      p->left   = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "left"   ) );
      p->top    = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "top"    ) );
      p->right  = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "right"  ) );
      p->bottom = ( LONG ) hb_itemGetNL( hb_hashGetCItemPtr( pStru, "bottom" ) );

      return p;
   }
   else if( HB_IS_ARRAY( pStru ) && hb_arrayLen( pStru ) >= 4 )
   {
      p->left   = ( LONG ) hb_arrayGetNL( pStru, 1 );
      p->top    = ( LONG ) hb_arrayGetNL( pStru, 2 );
      p->right  = ( LONG ) hb_arrayGetNL( pStru, 3 );
      p->bottom = ( LONG ) hb_arrayGetNL( pStru, 4 );

      return p;
   }
   else
      return bMandatory ? p : NULL;
}

DEVMODE * hbwapi_par_DEVMODE( DEVMODE * p, int iParam, HB_BOOL bMandatory )
{
   PHB_ITEM pStru = hb_param( iParam, HB_IT_ANY );

   memset( p, 0, sizeof( DEVMODE ) );

   p->dmSize = sizeof( DEVMODE );

   if( HB_IS_HASH( pStru ) )
   {
      p->dmOrientation   = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmOrientation"   ) );
      p->dmPaperSize     = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPaperSize"     ) );
      p->dmPaperLength   = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPaperLength"   ) );
      p->dmPaperWidth    = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPaperWidth"    ) );
      p->dmScale         = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmScale"         ) );
      p->dmCopies        = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmCopies"        ) );
      p->dmDefaultSource = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmDefaultSource" ) );
      p->dmPrintQuality  = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmPrintQuality"  ) );
      p->dmDuplex        = ( short ) hb_itemGetNI( hb_hashGetCItemPtr( pStru, "dmDuplex"        ) );

      if( hb_hashGetCItemPtr( pStru, "dmOrientation"   ) ) p->dmFields |= DM_ORIENTATION;
      if( hb_hashGetCItemPtr( pStru, "dmPaperSize"     ) ) p->dmFields |= DM_PAPERSIZE;
      if( hb_hashGetCItemPtr( pStru, "dmPaperLength"   ) ) p->dmFields |= DM_PAPERLENGTH;
      if( hb_hashGetCItemPtr( pStru, "dmPaperWidth"    ) ) p->dmFields |= DM_PAPERWIDTH;
      if( hb_hashGetCItemPtr( pStru, "dmScale"         ) ) p->dmFields |= DM_SCALE;
      if( hb_hashGetCItemPtr( pStru, "dmCopies"        ) ) p->dmFields |= DM_COPIES;
      if( hb_hashGetCItemPtr( pStru, "dmDefaultSource" ) ) p->dmFields |= DM_DEFAULTSOURCE;
      if( hb_hashGetCItemPtr( pStru, "dmPrintQuality"  ) ) p->dmFields |= DM_PRINTQUALITY;
      if( hb_hashGetCItemPtr( pStru, "dmDuplex"        ) ) p->dmFields |= DM_DUPLEX;

      return p;
   }
   else
      return bMandatory ? p : NULL;
}

#if ! defined( HB_OS_WIN_CE )

HB_FUNC( WAPI_CREATEDC )
{
   void * hDriver;
   void * hDevice;
   void * hOutput;
   DEVMODE dm;

   hbwapi_ret_HDC( CreateDC( HB_PARSTRDEF( 1, &hDriver, NULL ),
                             HB_PARSTRDEF( 2, &hDevice, NULL ),
                             HB_PARSTR( 3, &hOutput, NULL ),
                             hbwapi_par_DEVMODE( &dm, 4, HB_FALSE ) ) );

   hb_strfree( hDriver );
   hb_strfree( hDevice );
   hb_strfree( hOutput );
}

HB_FUNC( WAPI_RESETDC )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   DEVMODE dm;

   if( hDC && hbwapi_par_DEVMODE( &dm, 2, HB_TRUE ) )
      hb_retl( ResetDC( hDC, &dm ) == hDC );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_STARTDOC )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   /* TODO: Use hash */
   PHB_ITEM pDOCINFO = hb_param( 2, HB_IT_ARRAY );

   if( hDC && pDOCINFO && hb_arrayLen( pDOCINFO ) >= 4 )
   {
      DOCINFO di;

      void * hDocName;
      void * hOutput;
      void * hDatatype;

      di.cbSize       = sizeof( DOCINFO );
      di.lpszDocName  = HB_ARRAYGETSTR( pDOCINFO, 1, &hDocName , NULL ); /* DEF? */
      di.lpszOutput   = HB_ARRAYGETSTR( pDOCINFO, 2, &hOutput  , NULL );
      di.lpszDatatype = HB_ARRAYGETSTR( pDOCINFO, 3, &hDatatype, NULL );
      di.fwType       = ( DWORD ) hb_arrayGetNL( pDOCINFO, 4 );

      hb_retni( StartDoc( hDC, &di ) );

      hb_strfree( hDocName  );
      hb_strfree( hOutput   );
      hb_strfree( hDatatype );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
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
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( SetMapMode( hDC, hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( WAPI_GETMAPMODE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
      hb_retni( GetMapMode( hDC ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
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

      hb_retl( TextOut( hDC, hb_parni( 2 ) /* iRow */,
                             hb_parni( 3 ) /* iCol */,
                             lpData,
                             nDataLen ) );

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
   hbwapi_ret_HBRUSH( CreateHatchBrush( hb_parni( 1 ) /* fnStyle */,
                                        ( COLORREF ) hb_parnl( 2 ) /* crColor */ ) );
}

HB_FUNC( WAPI_CREATEFONT )
{
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
}

HB_FUNC( WAPI_SELECTOBJECT )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   HB_BOOL bRegion = HB_FALSE;
   HGDIOBJ h;

   if(      ( h = hbwapi_par_HPEN( 2 ) ) != NULL );
   else if( ( h = hbwapi_par_HFONT( 2 ) ) != NULL );
   /* TODO: Add BRUSH, BITMAP, REGION */
   else
      h = NULL;

   if( hDC && h )
   {
      if( bRegion )
         hb_retni( ( int ) SelectObject( hDC, h ) );
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
      PHB_ITEM pPOINT = hb_param( 4, HB_IT_ARRAY );

      if( pPOINT && hb_arrayLen( pPOINT ) >= 2 )
      {
         POINT p;

         hb_retl( MoveToEx( hDC, hb_parni( 2 ) /* X */, hb_parni( 3 ) /* Y */, &p ) );

         /* TODO: Support both has and array */
         hb_arraySetNL( pPOINT, 1, p.x );
         hb_arraySetNL( pPOINT, 2, p.y );
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

#endif
