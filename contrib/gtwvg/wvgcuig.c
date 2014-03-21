/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * Based on:
 *
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * See COPYING.txt for licensing terms.
 *
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/ ).
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

/*
 *                              EkOnkar
 *                        ( The LORD is ONE )
 *
 *                 Simplified GUI Drawing Functions
 *                     CUI Enhancement Oriented
 *                         No Callbacks etc.
 *                            Pure Statics
 *                                 .
 *                Pritpal Bedi <bedipritpal@hotmail.com>
 *                            28Feb2009
 */

#include "hbwinole.h"
#include "gtwvg.h"

static PHB_GOBJS hb_wvg_ObjectNew( PHB_GTWVT pWVT )
{
   int iHandle     = ( pWVT->gObjs ? pWVT->gObjs->iHandle + 1 : 1 );
   HB_GOBJS * gObj = ( HB_GOBJS * ) hb_xgrab( sizeof( HB_GOBJS ) );

   memset( gObj, 0, sizeof( HB_GOBJS ) );

   gObj->iHandle = iHandle;
   gObj->iState  = GOBJ_OBJSTATE_ENABLED;
   gObj->lpText  = NULL;
   gObj->bBlock  = NULL;
   gObj->hText   = NULL;

   hb_retni( iHandle );

   return gObj;
}

static void hb_wvg_RefreshRect( PHB_GTWVT pWVT, PHB_GOBJS gObj )
{
   RECT rc = { 0, 0, 0, 0 };

   /* Calculate the region occupied +- 3 pixels as most controls are outside of designated area */
   rc.top    = ( pWVT->PTEXTSIZE.y * gObj->iTop ) + gObj->aOffset.iTop - 3;
   rc.left   = ( pWVT->PTEXTSIZE.x * gObj->iLeft ) + gObj->aOffset.iLeft - 3;
   rc.bottom = ( pWVT->PTEXTSIZE.y * ( gObj->iBottom + 1 ) ) - 1 + gObj->aOffset.iBottom + 3;
   rc.right  = ( pWVT->PTEXTSIZE.x * ( gObj->iRight + 1 ) ) - 1 + gObj->aOffset.iRight + 3;

   InvalidateRect( pWVT->hWnd, &rc, TRUE );
}

static void hb_wvg_RestFromBuffer( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   BitBlt( pWVT->hdc, iLeft, iTop, iRight - iLeft, iBottom - iTop,
           pWVT->hGuiDC, iLeft, iTop, SRCCOPY );
}

/*
   Wvg_ClearGUIObjects() -> NIL
 */
HB_FUNC( WVG_CLEARGUIOBJECTS )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();

   if( pWVT->gObjs )
   {
      PHB_GOBJS gObj;

      while( pWVT->gObjs )
      {
         gObj = pWVT->gObjs->gObjNext;

         if( pWVT->gObjs->hText )
            hb_strfree( pWVT->gObjs->hText );
         if( pWVT->gObjs->hFont != NULL )
            if( pWVT->gObjs->bDestroyFont )
               DeleteObject( pWVT->gObjs->hFont );
         if( pWVT->gObjs->hPen )
            if( pWVT->gObjs->bDestroyPen )
               DeleteObject( pWVT->gObjs->hPen );
         if( pWVT->gObjs->hBrush )
            if( pWVT->gObjs->bDestroyBrush )
               DeleteObject( pWVT->gObjs->hBrush );
         if( pWVT->gObjs->bBlock )
            hb_itemRelease( pWVT->gObjs->bBlock );
#if ! defined( HB_OS_WIN_CE )
         if( pWVT->gObjs->iPicture )
            if( pWVT->gObjs->bDestroyPicture )
               HB_VTBL( pWVT->gObjs->iPicture )->Release( HB_THIS( pWVT->gObjs->iPicture ) );
#endif
         hb_xfree( pWVT->gObjs );
         pWVT->gObjs = gObj;
      }
      pWVT->gObjs = NULL;
   }
}

HB_FUNC( WVG_SETGOBJSTATE )
{
   PHB_GTWVT pWVT    = hb_wvt_gtGetWVT();
   int       iHandle = hb_parni( 1 );
   int       iOState = 0;

   if( iHandle && pWVT->gObjs )
   {
      PHB_GOBJS gObj = pWVT->gObjs;

      while( gObj )
      {
         if( iHandle == gObj->iHandle )
         {
            iOState = gObj->iState;

            if( HB_ISNUM( 2 ) )
            {
               int iState = hb_parni( 2 );
               if( iOState != iState && iState > 0 && iState <= 4 )
               {
                  gObj->iState = iState;
                  hb_wvg_RefreshRect( pWVT, gObj );
               }
            }
            break;
         }
         gObj = gObj->gObjNext;
      }
   }
   hb_retni( iOState );
}

/*
   Wvg_SetGuiObjectData( hObj, nGobjDataType, xData, xData1 ) -> lSuccess
 */
HB_FUNC( WVG_SETGOBJDATA )
{
   PHB_GTWVT pWVT     = hb_wvt_gtGetWVT();
   int       iHandle  = hb_parni( 1 );
   HB_BOOL   bSuccess = HB_FALSE;

   if( iHandle )
   {
      PHB_GOBJS gObj = pWVT->gObjs;

      while( gObj )
      {
         if( iHandle == gObj->iHandle )
         {
            int iDataType = hb_parni( 2 );

            bSuccess = HB_TRUE;

            switch( iDataType )
            {
               case GOBJ_OBJDATA_TEXT:
                  if( gObj->hText )
                     hb_strfree( gObj->hText );
                  gObj->lpText = HB_PARSTR( 3, &gObj->hText, NULL );
                  break;
#if ! defined( HB_OS_WIN_CE )
               case GOBJ_OBJDATA_PICTUREEX:
                  if( HB_ISNUM( 3 )  )
                     gObj->iPicture = ( IPicture * ) ( HB_PTRDIFF ) hb_parni( 3 );
                  break;
               case GOBJ_OBJDATA_PICTURE:
                  if( HB_ISNUM( 3 ) && hb_parni( 3 ) <= WVT_PICTURES_MAX )
                     gObj->iPicture = pWVT->pGUI->iPicture[ hb_parni( 3 ) - 1 ];
                  break;
               case GOBJ_OBJDATA_IMAGE:
               {
                  IPicture * iPicture = NULL;

                  if( HB_ISNUM( 3 ) )
                  {
                     if( hb_parni( 3 ) <= WVT_PICTURES_MAX )
                        iPicture = pWVT->pGUI->iPicture[ hb_parni( 3 ) - 1 ];
                  }
                  else
                  {
                     void * hPic;
                     iPicture = hb_wvt_gtLoadPicture( HB_PARSTR( 3, &hPic, NULL ) );
                     hb_strfree( hPic );
                     if( ! iPicture )
                     {
                        void * hRes;
                        void * hSec;
                        iPicture = hb_wvt_gtLoadPictureFromResource( HB_PARSTR( 3, &hRes, NULL ), HB_PARSTR( 4, &hSec, NULL ) );
                        hb_strfree( hRes );
                        hb_strfree( hSec );
                     }
                  }
                  if( iPicture )
                  {
                     if( gObj->bDestroyPicture && gObj->iPicture )
                        HB_VTBL( gObj->iPicture )->Release( HB_THIS( gObj->iPicture ) );
                     gObj->iPicture        = iPicture;
                     gObj->bDestroyPicture = HB_TRUE;
                  }
                  break;
               }
#endif
               case GOBJ_OBJDATA_HFONT:
                  if( gObj->hFont && gObj->bDestroyFont )
                     DeleteObject( gObj->hFont );
                  gObj->hFont        = ( HFONT ) ( HB_PTRDIFF ) hb_parnint( 3 );
                  gObj->bDestroyFont = HB_FALSE;
                  break;
               case GOBJ_OBJDATA_HPEN:
                  if( gObj->hPen && gObj->bDestroyPen )
                     DeleteObject( gObj->hPen );
                  gObj->hPen        = ( HPEN ) ( HB_PTRDIFF ) hb_parnint( 3 );
                  gObj->bDestroyPen = HB_FALSE;
                  break;
               case GOBJ_OBJDATA_HBRUSH:
                  if( gObj->hBrush && gObj->bDestroyBrush )
                     DeleteObject( gObj->hBrush );
                  gObj->hBrush        = ( HBRUSH ) ( HB_PTRDIFF ) hb_parnint( 3 );
                  gObj->bDestroyBrush = HB_TRUE;
                  break;
               case GOBJ_OBJDATA_COLORTEXT:
                  if( HB_ISNUM( 3 ) )
                     gObj->crRGBText = ( COLORREF ) ( HB_PTRDIFF ) hb_parnint( 3 );
                  else
                     bSuccess = HB_FALSE;
                  break;
               case GOBJ_OBJDATA_COLORBK:
                  gObj->crRGBBk = ( COLORREF ) ( HB_PTRDIFF ) hb_parnint( 3 );
                  break;
               case GOBJ_OBJDATA_BLOCK:
                  if( gObj->bBlock )
                     hb_itemRelease( gObj->bBlock );
                  gObj->bBlock = hb_itemNew( hb_param( 3, HB_IT_EVALITEM ) );
                  break;
               default:
                  bSuccess = HB_FALSE;
                  break;
            }
            if( bSuccess )
               hb_wvg_RefreshRect( pWVT, gObj );

            hb_retl( bSuccess );
            return;
         }
         gObj = gObj->gObjNext;
      }
   }
   hb_retl( bSuccess );
}

/*
   Wvg_BoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff )
 */
HB_FUNC( WVG_BOXRAISED )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_BOXRAISED;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_BoxRaised( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC hdc = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penWhiteDim );
   MoveToEx( hdc, iLeft, iTop, NULL );          /*  Top Inner    */
   LineTo( hdc, iRight, iTop );
   MoveToEx( hdc, iLeft, iTop, NULL );          /*  Left Inner   */
   LineTo( hdc, iLeft, iBottom );

   SelectObject( hdc, pGUI->penWhite );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     /*  Top Outer    */
   LineTo( hdc, iRight + 1, iTop - 1 );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     /*  Left Outer   */
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   SelectObject( hdc, pGUI->penDarkGray );
   MoveToEx( hdc, iLeft, iBottom, NULL );       /*  Bottom Inner */
   LineTo( hdc, iRight, iBottom );
   MoveToEx( hdc, iRight, iBottom, NULL );      /*  Right Inner  */
   LineTo( hdc, iRight, iTop );

   SelectObject( hdc, pGUI->penBlack );
   MoveToEx( hdc, iLeft - 1, iBottom + 1, NULL );  /*  Bottom Outer */
   LineTo( hdc, iRight + 1 + 1, iBottom + 1 );
   MoveToEx( hdc, iRight + 1, iTop - 1, NULL );    /*  Right Outer  */
   LineTo( hdc, iRight + 1, iBottom + 1 );
}

/*
   Wvg_BoxRecessed( nTop, nLeft, nBottom, nRight, aPxlOff ) -> NIL
 */
HB_FUNC( WVG_BOXRECESSED )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_BOXRECESSED;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_BoxRecessed( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC hdc = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penWhiteDim );
   MoveToEx( hdc, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( hdc, iRight, iBottom );
   MoveToEx( hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( hdc, iRight, iBottom );

   SelectObject( hdc, pGUI->penWhite );
   MoveToEx( hdc, iRight + 1, iTop - 1, NULL );    /* Right Outer  */
   LineTo( hdc, iRight + 1, iBottom + 1 );
   MoveToEx( hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
   LineTo( hdc, iRight + 2, iBottom + 1 );

   SelectObject( hdc, pGUI->penBlack );
   MoveToEx( hdc, iLeft, iTop, NULL );             /* Left  Inner  */
   LineTo( hdc, iLeft, iBottom );
   MoveToEx( hdc, iLeft, iTop, NULL );             /* Top Inner    */
   LineTo( hdc, iRight, iTop );

   SelectObject( hdc, pGUI->penDarkGray );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     /* Left Outer   */
   LineTo( hdc, iLeft - 1, iBottom + 1 );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     /* Top Outer    */
   LineTo( hdc, iRight + 1, iTop - 1 );
}

/*
   Wvt_DrawBoxGet( nRow, nCol, nWidth ) -> NIL
 */
HB_FUNC( WVG_BOXGET )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_BOXGET;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 1 );
   gObj->iRight  = hb_parni( 2 ) + hb_parni( 3 ) - 1;

   gObj->aOffset.iTop    = 0;
   gObj->aOffset.iLeft   = 0;
   gObj->aOffset.iBottom = 0;
   gObj->aOffset.iRight  = 0;

   gObj->gObjNext = pWVT->gObjs;

   pWVT->gObjs = gObj;
}

static void hb_wvg_BoxGet( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC hdc = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penBlack );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );        /* Top Inner   */
   LineTo(   hdc, iRight - 1, iTop - 1       );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );        /* Left  Inner */
   LineTo(   hdc, iLeft - 1, iBottom - 1    );

   SelectObject( hdc, pGUI->penDarkGray );
   MoveToEx( hdc, iLeft - 2, iTop - 2, NULL );           /* Top Outer   */
   LineTo(   hdc, iRight, iTop - 2       );
   MoveToEx( hdc, iLeft - 2, iTop - 2, NULL );           /* Left Outer  */
   LineTo(   hdc, iLeft - 2, iBottom      );
}

/*
   Wvg_BoxGroup( nTop, nLeft, nBottom, nRight, aPxlOff ) -> NIL
 */
HB_FUNC( WVG_BOXGROUP )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_BOXGROUP;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_BoxGroup( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC hdc = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penDarkGray );

   MoveToEx( hdc, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     /* Left Outer   */
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     /* Top Outer    */
   LineTo( hdc, iRight + 1, iTop - 1 );


   SelectObject( hdc, pGUI->penWhite );

   MoveToEx( hdc, iRight + 1, iTop, NULL );        /* Right Outer  */
   LineTo( hdc, iRight + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iBottom + 1, NULL );   /* Bottom Outer */
   LineTo( hdc, iRight + 1 + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft, iTop, NULL );             /* Left  Inner  */
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );             /* Top Inner    */
   LineTo( hdc, iRight, iTop );
}

/*
   Wvg_BoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff ) -> NIL
 */
HB_FUNC( WVG_BOXGROUPRAISED )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_BOXGROUPRAISED;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_BoxGroupRaised( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC hdc = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penWhite );

   MoveToEx( hdc, iRight, iTop, NULL );           /* Right Inner  */
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );         /* Bottom Inner */
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );    /* Left Outer   */
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );    /* Top Outer    */
   LineTo( hdc, iRight + 1, iTop - 1 );

   SelectObject( hdc, pGUI->penDarkGray );

   MoveToEx( hdc, iRight + 1, iTop, NULL );       /* Right Outer  */
   LineTo( hdc, iRight + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
   LineTo( hdc, iRight + 1 + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft, iTop, NULL );            /* Left  Inner  */
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            /* Top Inner    */
   LineTo( hdc, iRight, iTop );
}

/*
 *    Wvg_Label( nRow, nCol, aPxlOff, cLabel, nAlign,
 *               nEscapement, nTextColor, nBkColor, cFontFace, nFontHeight,
 *               nFontWidth, nFontWeight, nQuality,  nCharSet, lItalics,
 *               lUnderline, lStrikeOut )
 */
HB_FUNC( WVG_LABEL )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();
   LOGFONT   lf;
   HFONT     hFont;
   void *    hText = NULL;

   lf.lfEscapement     = hb_parni( 6 ) * 10;
   lf.lfOrientation    = 0;
   lf.lfWeight         = hb_parni( 12 );
   lf.lfItalic         = ( BYTE ) hb_parl( 15 );
   lf.lfUnderline      = ( BYTE ) hb_parl( 16 );
   lf.lfStrikeOut      = ( BYTE ) hb_parl( 17 );
   lf.lfCharSet        = ( BYTE ) hb_parnidef( 14, pWVT->CodePage );
   lf.lfOutPrecision   = 0;
   lf.lfClipPrecision  = 0;
   lf.lfQuality        = ( BYTE ) hb_parnidef( 13, DEFAULT_QUALITY );
   lf.lfPitchAndFamily = FF_DONTCARE;
   lf.lfHeight         = hb_parnidef( 10, pWVT->fontHeight );
   lf.lfWidth          = hb_parnidef( 11, pWVT->fontWidth < 0 ? -pWVT->fontWidth : pWVT->fontWidth );

   HB_STRNCPY( lf.lfFaceName, ( ! HB_ISCHAR( 9 ) ? pWVT->fontFace : HB_PARSTR( 9, &hText, NULL ) ), HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );
   hb_strfree( hText );

   hFont = CreateFontIndirect( &lf );
   if( hFont )
   {
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_LABEL;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 1 );
      gObj->iRight  = hb_parni( 2 );

      gObj->aOffset.iTop    = hb_parvni( 3, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 3, 2 );
      gObj->aOffset.iBottom = hb_parvni( 3, 3 );
      gObj->aOffset.iRight  = hb_parvni( 3, 4 );

      gObj->lpText = HB_PARSTR( 4, &gObj->hText, NULL );

      gObj->iAlign       = hb_parnidef( 5, TA_LEFT );
      gObj->crRGBText    = ( COLORREF ) hb_parnint( 7 );
      gObj->crRGBBk      = ( COLORREF ) hb_parnint( 8 );
      gObj->hFont        = hFont;
      gObj->bDestroyFont = HB_TRUE;

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
}

/*
   Wvg_LabelEx( nRow, nCol, aPxlOff, cLabel, nAlign, nTextColor, nBkColor, nSlotFont )
 */
HB_FUNC( WVG_LABELEX )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_LABEL;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 1 );
   gObj->iRight  = hb_parni( 2 );

   gObj->aOffset.iTop    = hb_parvni( 3, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 3, 2 );
   gObj->aOffset.iBottom = hb_parvni( 3, 3 );
   gObj->aOffset.iRight  = hb_parvni( 3, 4 );

   gObj->lpText = HB_PARSTR( 4, &gObj->hText, NULL );

   gObj->iAlign    = hb_parnidef( 5, TA_LEFT );
   gObj->crRGBText = ( COLORREF ) hb_parnint( 6 );
   gObj->crRGBBk   = ( COLORREF ) hb_parnint( 7 );

   gObj->hFont        = pWVT->pGUI->hUserFonts[ hb_parni( 8 ) - 1 ];
   gObj->bDestroyFont = HB_FALSE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_Label( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop )
{
   HFONT hOldFont;

   if( gObj->crRGBBk != ( COLORREF ) 0 )
   {
      SetBkColor( pWVT->hdc, gObj->crRGBBk );
      SetBkMode( pWVT->hdc, OPAQUE );
   }
   else
      SetBkMode( pWVT->hdc, TRANSPARENT );

   SetTextColor( pWVT->hdc, gObj->crRGBText );
   SetTextAlign( pWVT->hdc, gObj->iAlign );

   hOldFont = ( HFONT ) SelectObject( pWVT->hdc, gObj->hFont );

   ExtTextOut( pWVT->hdc, iLeft, iTop, 0, NULL, gObj->lpText, lstrlen( gObj->lpText ), NULL );

   SelectObject( pWVT->hdc, hOldFont );
}


/*
 *    Wvg_LabelEx2( nTop, nLeft, nBottom, nRight, aPxlOff, cLabel, nAlign,
 *                  nEscapement, nTextColor, nBkColor, cFontFace, nFontHeight,
 *                  nFontWidth, nFontWeight, nQuality,  nCharSet, lItalics,
 *                  lUnderline, lStrikeOut )
 */
HB_FUNC( WVG_LABELEX2 )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();
   LOGFONT   lf;
   HFONT     hFont;
   void *    hText = NULL;

   lf.lfEscapement     = hb_parni( 8 ) * 10;
   lf.lfOrientation    = 0;
   lf.lfWeight         = hb_parni( 14 );
   lf.lfItalic         = ( BYTE ) hb_parl( 17 );
   lf.lfUnderline      = ( BYTE ) hb_parl( 18 );
   lf.lfStrikeOut      = ( BYTE ) hb_parl( 19 );
   lf.lfCharSet        = ( BYTE ) hb_parnidef( 16, pWVT->CodePage );
   lf.lfOutPrecision   = 0;
   lf.lfClipPrecision  = 0;
   lf.lfQuality        = ( BYTE ) hb_parnidef( 15, DEFAULT_QUALITY );
   lf.lfPitchAndFamily = FF_DONTCARE;
   lf.lfHeight         = hb_parnidef( 12, pWVT->fontHeight );
   lf.lfWidth          = hb_parnidef( 13, pWVT->fontWidth < 0 ? -pWVT->fontWidth : pWVT->fontWidth );

   HB_STRNCPY( lf.lfFaceName, ( ! HB_ISCHAR( 11 ) ? pWVT->fontFace : HB_PARSTR( 11, &hText, NULL ) ), HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );
   hb_strfree( hText );

   hFont = CreateFontIndirect( &lf );
   if( hFont )
   {
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_LABEL_EX2;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 3 );
      gObj->iRight  = hb_parni( 4 );

      gObj->aOffset.iTop    = hb_parvni( 5, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
      gObj->aOffset.iBottom = hb_parvni( 5, 3 );
      gObj->aOffset.iRight  = hb_parvni( 5, 4 );

      gObj->lpText = HB_PARSTR( 6, &gObj->hText, NULL );

      gObj->iAlign       = hb_parnidef( 7, TA_LEFT );
      gObj->crRGBText    = ( COLORREF ) hb_parnint( 9 );
      gObj->crRGBBk      = ( COLORREF ) hb_parnint( 10 );
      gObj->hFont        = hFont;
      gObj->bDestroyFont = HB_TRUE;

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
}

static void hb_wvg_LabelEx2( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HFONT hOldFont;
   int   x, y, iAlignV, iAlignH;
   SIZE  sz = { 0, 0 };
   RECT  rect = { 0, 0, 0, 0 };

   SetBkColor( pWVT->hdc, gObj->crRGBBk );
   SetTextColor( pWVT->hdc, gObj->crRGBText );

   hOldFont = ( HFONT ) SelectObject( pWVT->hdc, gObj->hFont );

   x = iLeft;
   y = iTop;

   switch( gObj->iAlign )
   {
      case 0:
         iAlignH = TA_LEFT;
         break;

      case 2:
         iAlignH = TA_RIGHT;
         x = iRight;
         break;

      case 1:
         iAlignH = TA_CENTER;
         x = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         break;

      default:
         iAlignH = 0;
   }

   iAlignV = TA_TOP;

   GetTextExtentPoint32( pWVT->hdc, gObj->lpText, lstrlen( gObj->lpText ), &sz );
   y = iTop + ( ( iBottom - iTop + 1 - sz.cy ) / 2 );

   SetTextAlign( pWVT->hdc, iAlignH | iAlignV );

   rect.top    = iTop;
   rect.left   = iLeft;
   rect.bottom = iBottom;
   rect.right  = iRight;

   ExtTextOut( pWVT->hdc, x, y, ETO_CLIPPED | ETO_OPAQUE, &rect, gObj->lpText, lstrlen( gObj->lpText ), NULL );
   SelectObject( pWVT->hdc, hOldFont );
}

/*
 *                  1      2       3        4       5       6        7         8
 *    Wvg_Outline( nTop, nLeft, nBottom, nRight, aPxlOff, nThick, nShape, nRGBColor )
 */
HB_FUNC( WVG_OUTLINE )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_OUTLINE;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->iWidth = hb_parni( 6 );             /* iThick */
   gObj->iStyle = hb_parni( 7 );             /* iShape */
   gObj->crRGB  = ( COLORREF ) hb_parnl( 8 );

   if( gObj->iWidth > 0 )
   {
      gObj->hPen        = CreatePen( gObj->iWidth, gObj->iStyle, gObj->crRGB );
      gObj->bDestroyPen = HB_TRUE;
   }
   else
   {
      gObj->hPen        = pWVT->pGUI->penBlack;
      gObj->bDestroyPen = HB_FALSE;
   }

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

/*
   Wvg_OutlineEx( nTop, nLeft, nBottom, nRight, aPxlOff, nSlotPen )
 */
HB_FUNC( WVG_OUTLINEEX )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_OUTLINEEX;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   if( pWVT->pGUI->hUserPens[ hb_parni( 6 ) - 1 ] )
   {
      gObj->hPen        = pWVT->pGUI->hUserPens[ hb_parni( 6 ) - 1 ];
      gObj->bDestroyPen = HB_FALSE;
   }
   else
   {
      gObj->hPen        = pWVT->pGUI->penBlack;
      gObj->bDestroyPen = HB_FALSE;
   }

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_Outline( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC hdc = pWVT->hdc;

   SelectObject( pWVT->hdc, gObj->hPen );

   MoveToEx( hdc, iLeft, iTop, NULL );             /*  Top    */
   LineTo( hdc, iRight, iTop );

   MoveToEx( hdc, iLeft, iTop, NULL );             /*  Left   */
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );          /*  Bottom */
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iRight, iTop, NULL );            /*  Right  */
   LineTo( hdc, iRight, iBottom + 1 );
}

/*            1      2       3       4        5        6       7       8       9      10        11
 * Wvg_Line( nTop, nLeft, nBottom, nRight, aPxlOff, nOrient, nFormat, nAlign, nStyle, nThick, nColor )
 */
HB_FUNC( WVG_LINE )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_LINE;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->iOrient = hb_parni( 6 );
   gObj->iFormat = hb_parni( 7 );
   gObj->iAlign  = hb_parni( 8 );

   gObj->iStyle = hb_parni( 9 );
   gObj->iWidth = hb_parni( 10 );           /* iThick */
   gObj->crRGB  = ( COLORREF ) hb_parnl( 11 );

   gObj->hPen        = CreatePen( gObj->iStyle, gObj->iWidth, gObj->crRGB );
   gObj->bDestroyPen = HB_TRUE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

/*                1      2       3       4        5        6       7       8          9
 *   Wvg_LineEx( nTop, nLeft, nBottom, nRight, aPxlOff, nOrient, nFormat, nAlign, nSlotPen )
 */
HB_FUNC( WVG_LINEEX )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_LINE;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->iOrient = hb_parni( 6 );
   gObj->iFormat = hb_parni( 7 );
   gObj->iAlign  = hb_parni( 8 );

   gObj->hPen        = pWVT->pGUI->hUserPens[ hb_parni( 9 ) - 1 ];
   gObj->bDestroyPen = HB_FALSE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_Line( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC  hdc = pWVT->hdc;
   HPEN hPen, hOldPen;

   int iOffset;
   int x = iLeft;
   int y = iTop;

   switch( gObj->iAlign )
   {
      case 0:                       /* Center */
         if( gObj->iOrient == 0 )   /* Horizontal  */
         {
            iOffset = ( ( iBottom - iTop ) / 2 );
            y       = iTop + iOffset;
         }
         else
         {
            iOffset = ( ( iRight - iLeft ) / 2 );
            x       = iLeft + iOffset;
         }
         break;

      case 1:                  /* Top */
         break;

      case 2:                                            /* bottom */
         if( gObj->iFormat == 0 || gObj->iFormat == 1 )  /* Raised/Recessd */
            y = iBottom - 1;
         else
            y = iBottom;
         break;

      case 3:                  /* Left */
         break;

      case 4:                                            /* Right */
         if( gObj->iFormat == 0 || gObj->iFormat == 1 )  /* Raised/Recessd */
            x = iRight - 1;
         else
            x = iRight;
         break;
   }

   hPen    = gObj->hPen;
   hOldPen = ( HPEN ) SelectObject( hdc, gObj->hPen );

   switch( gObj->iFormat )
   {
      case 0:                                         /* Raised */
         if( gObj->iOrient == 0 )                     /* Horizontal */
         {
            SelectObject( hdc, pWVT->pGUI->penWhite );
            MoveToEx( hdc, x, y, NULL );
            LineTo( hdc, iRight, y );
            SelectObject( hdc, hPen );
            MoveToEx( hdc, x, y + 1, NULL );
            LineTo( hdc, iRight, y + 1 );
         }
         else                                       /*  Vertical */
         {
            SelectObject( hdc, pWVT->pGUI->penWhite );
            MoveToEx( hdc, x, y, NULL );
            LineTo( hdc, x, iBottom );
            SelectObject( hdc, hPen );
            MoveToEx( hdc, x + 1, y, NULL );
            LineTo( hdc, x + 1, iBottom );
         }
         break;

      case 1:                                      /* Recessed */
         if( gObj->iOrient == 0 )                  /* Horizontal  */
         {
            SelectObject( hdc, hPen );
            MoveToEx( hdc, x, y, NULL );
            LineTo( hdc, iRight, y );
            SelectObject( hdc, pWVT->pGUI->penWhite );
            MoveToEx( hdc, x, y + 1, NULL );
            LineTo( hdc, iRight, y + 1 );
         }
         else                                      /*  Vertical */
         {
            SelectObject( hdc, hPen );
            MoveToEx( hdc, x, y, NULL );
            LineTo( hdc, x, iBottom );
            SelectObject( hdc, pWVT->pGUI->penWhite );
            MoveToEx( hdc, x + 1, y, NULL );
            LineTo( hdc, x + 1, iBottom );
         }
         break;

      case 2:                                      /* Plain */
         if( gObj->iOrient == 0 )                  /* Horizontal  */
         {
            SelectObject( hdc, hPen );
            MoveToEx( hdc, x, y, NULL );
            LineTo( hdc, iRight, y );
         }
         else                                      /*  Vertical */
         {
            SelectObject( hdc, hPen );
            MoveToEx( hdc, x, y, NULL );
            LineTo( hdc, x, iBottom );
         }
         break;
   }
   SelectObject( hdc, hOldPen );
}

/*
 *    Inside the area requested!
 *    Wvg_Ellipse( nTop, nLeft, nBottom, nRight, aPxlOff )
 */
HB_FUNC( WVG_ELLIPSE )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_ELLIPSE;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->hPen          = pWVT->currentPen;
   gObj->bDestroyPen   = HB_FALSE;
   gObj->hBrush        = pWVT->currentBrush;
   gObj->bDestroyBrush = HB_FALSE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_Ellipse( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HBRUSH hBrush = ( HBRUSH ) SelectObject( pWVT->hdc, gObj->hBrush );
   HPEN   hPen   = ( HPEN ) SelectObject( pWVT->hdc, gObj->hPen );

   Ellipse( pWVT->hdc, iLeft, iTop, iRight, iBottom );

   SelectObject( pWVT->hdc, hPen );
   SelectObject( pWVT->hdc, hBrush );
}

/*
   Wvg_Rectangle( nTop, nLeft, nBottom, nRight, aPxlOff )
 */
HB_FUNC( WVG_RECTANGLE )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_RECTANGLE;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->hPen          = pWVT->currentPen;
   gObj->bDestroyPen   = HB_FALSE;
   gObj->hBrush        = pWVT->currentBrush;
   gObj->bDestroyBrush = HB_FALSE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_Rectangle( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HBRUSH hBrush = ( HBRUSH ) SelectObject( pWVT->hdc, gObj->hBrush );
   HPEN   hPen   = ( HPEN ) SelectObject( pWVT->hdc, gObj->hPen );

   Rectangle( pWVT->hdc, iLeft, iTop, iRight, iBottom );

   SelectObject( pWVT->hdc, hPen );
   SelectObject( pWVT->hdc, hBrush );
}

/*
   Wvg_RoundRect( nTop, nLeft, nBottom, nRight, aPxlOff, nRoundHeight, nRoundWidth )
 */
HB_FUNC( WVG_ROUNDRECT )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_ROUNDRECT;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->iHeight = hb_parni( 6 );
   gObj->iWidth  = hb_parni( 7 );

   gObj->hPen          = pWVT->currentPen;
   gObj->bDestroyPen   = HB_FALSE;
   gObj->hBrush        = pWVT->currentBrush;
   gObj->bDestroyBrush = HB_FALSE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_RoundRect( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HBRUSH hBrush = ( HBRUSH ) SelectObject( pWVT->hdc, gObj->hBrush );
   HPEN   hPen   = ( HPEN ) SelectObject( pWVT->hdc, gObj->hPen );

   RoundRect( pWVT->hdc, iLeft, iTop, iRight, iBottom, gObj->iWidth, gObj->iHeight );

   SelectObject( pWVT->hdc, hPen );
   SelectObject( pWVT->hdc, hBrush );
}

/*
   Wvg_ColorRect( nTop, nLeft, nBottom, nRight, aPxlOff, nRGB )
 */
HB_FUNC( WVG_COLORRECT )
{
   HBRUSH hBrush = CreateSolidBrush( ( COLORREF ) hb_parnint( 6 ) );

   if( hBrush )
   {
      PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_COLORRECT;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 3 );
      gObj->iRight  = hb_parni( 4 );

      gObj->aOffset.iTop    = hb_parvni( 5, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
      gObj->aOffset.iBottom = hb_parvni( 5, 3 );
      gObj->aOffset.iRight  = hb_parvni( 5, 4 );

      gObj->hBrush        = hBrush;
      gObj->bDestroyBrush = HB_TRUE;

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
}

static void hb_wvg_ColorRect( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   RECT rc = { 0, 0, 0, 0 };

   SetRect( &rc, iLeft, iTop, iRight, iBottom );
   FillRect( pWVT->hGuiDC, &rc, gObj->hBrush );
}

/*
   Wvg_ShadedRect( nTop, nLeft, nBottom, nRight, aPxlOff, nHorVert, aRGBb, aRGBe  )
 */
HB_FUNC( WVG_SHADEDRECT )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();

   if( pWVT->pGUI->hMSImg32 )
   {
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_SHADEDRECT;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 3 );
      gObj->iRight  = hb_parni( 4 );

      gObj->aOffset.iTop    = hb_parvni( 5, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
      gObj->aOffset.iBottom = hb_parvni( 5, 3 );
      gObj->aOffset.iRight  = hb_parvni( 5, 4 );

      gObj->iData = hb_parnidef( 6, GRADIENT_FILL_RECT_H );

      gObj->vert[ 0 ].x     = 0;
      gObj->vert[ 0 ].y     = 0;
      gObj->vert[ 0 ].Red   = ( COLOR16 ) hb_parvni( 7, 1 );
      gObj->vert[ 0 ].Green = ( COLOR16 ) hb_parvni( 7, 2 );
      gObj->vert[ 0 ].Blue  = ( COLOR16 ) hb_parvni( 7, 3 );
      gObj->vert[ 0 ].Alpha = ( COLOR16 ) hb_parvni( 7, 4 );

      gObj->vert[ 1 ].x     = 0;
      gObj->vert[ 1 ].y     = 0;
      gObj->vert[ 1 ].Red   = ( COLOR16 ) hb_parvni( 8, 1 );
      gObj->vert[ 1 ].Green = ( COLOR16 ) hb_parvni( 8, 2 );
      gObj->vert[ 1 ].Blue  = ( COLOR16 ) hb_parvni( 8, 3 );
      gObj->vert[ 1 ].Alpha = ( COLOR16 ) hb_parvni( 8, 4 );

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
}

static void hb_wvg_ShadedRect( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   HB_BOOL       bGF;
   GRADIENT_RECT gRect = { 0, 0 };

   gRect.UpperLeft  = 0;
   gRect.LowerRight = 1;

   gObj->vert[ 0 ].x = iLeft;
   gObj->vert[ 0 ].y = iTop;

   gObj->vert[ 1 ].x = iRight;
   gObj->vert[ 1 ].y = iBottom;

   bGF = ( HB_BOOL ) pWVT->pGUI->pfnGF( pWVT->hGuiDC, gObj->vert, 2, &gRect, 1, gObj->iData );

   HB_SYMBOL_UNUSED( bGF );
}

/*
 *   Wvg_TextBox( nTop, nLeft, nBottom, nRight, aPxlOff, cText, ;
 *                nAlignHorz, nAlignVert, nTextColor, nBackColor, hFont )
 */
HB_FUNC( WVG_TEXTBOX )
{
   PHB_GTWVT  pWVT    = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj    = hb_wvg_ObjectNew( pWVT );
   int        iAlignH = 0;

   gObj->iObjType = GOBJ_OBJTYPE_TEXTBOX;

   gObj->iTop    = hb_parni( 1 );
   gObj->iLeft   = hb_parni( 2 );
   gObj->iBottom = hb_parni( 3 );
   gObj->iRight  = hb_parni( 4 );

   gObj->aOffset.iTop    = hb_parvni( 5, 1 );
   gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
   gObj->aOffset.iBottom = hb_parvni( 5, 3 );
   gObj->aOffset.iRight  = hb_parvni( 5, 4 );

   gObj->lpText = HB_PARSTR( 6, &gObj->hText, NULL );

   switch( hb_parni( 7 ) )
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
   gObj->iAlign = iAlignH;

   gObj->crRGBText = ( COLORREF ) hb_parnint( 9 );
   gObj->crRGBBk   = ( COLORREF ) hb_parnint( 10 );

   gObj->hFont        = ( HFONT ) ( HB_PTRDIFF ) hb_parnint( 11 );
   gObj->bDestroyFont = HB_FALSE;

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

static void hb_wvg_TextBox( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
   RECT rc  = { 0, 0, 0, 0 };
   HDC  hdc = pWVT->hGuiDC;

   SetRect( &rc, iLeft, iTop, iRight, iBottom );

   SetTextAlign( hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP );
   SetTextColor( hdc, gObj->crRGBText );
   if( gObj->crRGBBk == ( COLORREF ) 0 )
      SetBkMode( hdc, TRANSPARENT );
   else
   {
      SetBkMode( hdc, OPAQUE );
      SetBkColor( hdc, gObj->crRGBBk );
   }
   SelectObject( hdc, gObj->hFont );

   DrawText( hdc, gObj->lpText, lstrlen( gObj->lpText ), &rc, gObj->iAlign | DT_WORDBREAK | DT_TOP );
}

/*
 *  Wvt_DrawPicture( nTop, nLeft, nBottom, nRight, nSlot, aPxlOff ) -> lOk
 *  Wvg_Picture(  nTop, nLeft, nBottom, nRight, aPxlOff, nSlot, lDoNotScale ) -> NIL
 */
HB_FUNC( WVG_PICTURE )
{
#if ! defined( HB_OS_WIN_CE )
   if( HB_ISNUM( 6 ) && hb_parni( 6 ) <= WVT_PICTURES_MAX )
   {
      PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_PICTURE;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 3 );
      gObj->iRight  = hb_parni( 4 );

      gObj->aOffset.iTop    = hb_parvni( 5, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
      gObj->aOffset.iBottom = hb_parvni( 5, 3 );
      gObj->aOffset.iRight  = hb_parvni( 5, 4 );

      gObj->iPicture        = pWVT->pGUI->iPicture[ hb_parni( 6 ) - 1 ];
      gObj->iData           = hb_parl( 7 ) ? 1 : 0;
      gObj->bDestroyPicture = HB_FALSE;

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
   else
      hb_retni( 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( WVG_PICTUREEX )
{
#if ! defined( HB_OS_WIN_CE )
   if( HB_ISNUM( 6 ) )
   {
      PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_PICTURE;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 3 );
      gObj->iRight  = hb_parni( 4 );

      gObj->aOffset.iTop    = hb_parvni( 5, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
      gObj->aOffset.iBottom = hb_parvni( 5, 3 );
      gObj->aOffset.iRight  = hb_parvni( 5, 4 );

      gObj->iPicture        = ( IPicture * ) ( HB_PTRDIFF ) hb_parnint( 6 );
      gObj->iData           = hb_parl( 7 ) ? 1 : 0;
      gObj->bDestroyPicture = HB_FALSE;

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
   else
      hb_retni( 0 );
#else
   hb_retni( 0 );
#endif
}

/*
    Wvg_Image( nTop, nLeft, nBottom, nRight, aPxlOff, nImageSource, cImage/nPictureSlot, cSection, lDoNotScale )
 */
HB_FUNC( WVG_IMAGE )
{
#if ! defined( HB_OS_WIN_CE )
   PHB_GTWVT  pWVT     = hb_wvt_gtGetWVT();
   int        iSource  = hb_parni( 6 );
   IPicture * iPicture = NULL;

   if( iSource == 0 )
   {
      hb_retni( 0 );
      return;
   }

   switch( iSource )
   {
      case GOBJ_IMAGESOURCE_SLOT:
         if( HB_ISNUM( 7 ) && hb_parni( 7 ) <= WVT_PICTURES_MAX )
            iPicture = pWVT->pGUI->iPicture[ hb_parni( 7 ) - 1 ];
         break;
      case GOBJ_IMAGESOURCE_RESOURCE:
      {
         void * hPic;
         void * hRes;
         iPicture = hb_wvt_gtLoadPictureFromResource( HB_PARSTR( 7, &hPic, NULL ), HB_PARSTR( 8, &hRes, NULL ) );
         hb_strfree( hPic );
         hb_strfree( hRes );
         break;
      }
      case GOBJ_IMAGESOURCE_FILE:
      {
         void * hPic;
         iPicture = hb_wvt_gtLoadPicture( HB_PARSTR( 7, &hPic, NULL ) );
         hb_strfree( hPic );
         break;
      }
   }

   if( iPicture )
   {
      HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

      gObj->iObjType = GOBJ_OBJTYPE_PICTURE;

      gObj->iTop    = hb_parni( 1 );
      gObj->iLeft   = hb_parni( 2 );
      gObj->iBottom = hb_parni( 3 );
      gObj->iRight  = hb_parni( 4 );

      gObj->aOffset.iTop    = hb_parvni( 5, 1 );
      gObj->aOffset.iLeft   = hb_parvni( 5, 2 );
      gObj->aOffset.iBottom = hb_parvni( 5, 3 );
      gObj->aOffset.iRight  = hb_parvni( 5, 4 );

      gObj->iPicture        = iPicture;
      gObj->iData           = hb_parl( 9 ) ? 1 : 0;

      if( iSource == GOBJ_IMAGESOURCE_SLOT )
         gObj->bDestroyPicture = HB_FALSE;
      else
         gObj->bDestroyPicture = HB_TRUE;

      gObj->gObjNext = pWVT->gObjs;
      pWVT->gObjs    = gObj;
   }
   else
      hb_retni( 0 );
#else
   hb_retni( 0 );
#endif
}

static void hb_wvg_RenderPicture( PHB_GTWVT pWVT, PHB_GOBJS gObj, int iLeft, int iTop, int iRight, int iBottom )
{
#if ! defined( HB_OS_WIN_CE )
   LONG       lWidth, lHeight;
   int        xe, ye, iWd = 0, iHt = 0, x, y, wd, ht;
   HRGN       hrgn1;
   POINT      lpp     = { 0, 0 };

   HDC        hdc      = pWVT->hGuiDC;
   IPicture * iPicture = gObj->iPicture;

   if( iPicture )
   {
      HB_VTBL( iPicture )->get_Width( HB_THIS_( iPicture ) & lWidth );
      HB_VTBL( iPicture )->get_Height( HB_THIS_( iPicture ) & lHeight );

      x = iLeft;
      y = iTop;
      wd = iRight - iLeft + 1;
      ht = iBottom - iTop + 1;

      if( gObj->iData == 1 )
      {
         iHt = ( int ) ( ( float )  wd * lHeight / lWidth );
         iWd = ( int ) ( ( float ) iHt * lWidth / lHeight );
         x  += abs( ( iWd - wd ) / 2 );
         y  += abs( ( iHt - ht ) / 2 );
         wd  = iWd;
         ht  = iHt;
      }
      xe = x + wd ;
      ye = y + ht ;

      GetViewportOrgEx( hdc, &lpp );

      hrgn1 = CreateRectRgn( lpp.x + x, lpp.y + y, lpp.x + xe, lpp.y + ye );
      SelectClipRgn( hdc, hrgn1 );

      HB_VTBL( iPicture )->Render( HB_THIS_( iPicture ) hdc, x, y, wd, ht, 0, lHeight, lWidth, -lHeight, NULL );

      SelectClipRgn( hdc, NULL );
      DeleteObject( hrgn1 );
   }
#else
   HB_SYMBOL_UNUSED( pWVT );
   HB_SYMBOL_UNUSED( gObj );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iRight );
   HB_SYMBOL_UNUSED( iBottom );
#endif
}

/*
   Wvg_Object( nObj, bBlock )
   nObj == one of the objects == GOBJ_OBJTYPE_ROUNDRECT | GOBJ_OBJTYPE_IMAGE | etc
   bBlock == Block returning the array conataining as many elements as necessary for the given object
             Also it will return those array elements in the same order expected by the object to draw

   This protocol is necessary for dyanamic coordinates which might have been changed by the
   applications, like TBrowse dimensions.

   Wvg_Object( GOBJ_OBJTYPE_BOXRAISED, {|| { oBrw:nTop, oBrw:nLeft, oBrw:nBottom, oBrw:nRight, {-2,-2,2,2} } } )
 */
HB_FUNC( WVG_OBJECT )
{
   PHB_GTWVT  pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = hb_wvg_ObjectNew( pWVT );

   gObj->iObjType = GOBJ_OBJTYPE_OBJECT;

   gObj->iData  = hb_parni( 1 );        /* Object to be executed */
   gObj->bBlock = hb_itemNew( hb_param( 2, HB_IT_EVALITEM ) );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs    = gObj;
}

/*
 *    Wvg_Object( GOBJ_OBJTYPE_GRIDVERT, {|| { nTop, nBottom, aCols, nCols, aPxlOff } } )
 *                                                  aPxlOff[ 1 ] and aPxlOff[ 3 ] used
 */
static void hb_wvg_GridVert( PHB_GTWVT pWVT, PHB_ITEM pArray, RECT * uRect )
{
   PHB_ITEM pCols = hb_arrayGetItemPtr( pArray, 3 );
   HB_ISIZ  iTabs = hb_arrayLen( pCols );

   if( iTabs > 0 )
   {
      int     x, iTop, iBottom;
      HB_ISIZ i;

      iTop    = ( hb_arrayGetNI( pArray, 1 ) * ( int ) pWVT->PTEXTSIZE.y );
      iBottom = ( ( hb_arrayGetNI( pArray, 2 ) + 1 ) * ( int ) pWVT->PTEXTSIZE.y ) - 1;

      if( ( iTop >= uRect->top && iTop <= uRect->bottom ) ||
          ( iBottom >= uRect->top && iBottom <= uRect->bottom ) )
      {
         HDC hdc = pWVT->hGuiDC;
         SelectObject( hdc, pWVT->currentPen );
         for( i = 1; i <= iTabs; i++ )
         {
            x = hb_arrayGetNI( pCols, i ) * pWVT->PTEXTSIZE.x;
            MoveToEx( hdc, x, iTop, NULL );
            LineTo( hdc, x, iBottom );
         }

         /* Play it on screen too directly as this area is not going to be revalidated
          * we are not to invalidate this region as it is redrawn many many times.
          * perhaps we can avoid drawing on the image ???
          */
         hdc = pWVT->hdc;
         SelectObject( hdc, pWVT->currentPen );
         for( i = 1; i <= iTabs; i++ )
         {
            x = hb_arrayGetNI( pCols, i ) * pWVT->PTEXTSIZE.x;
            MoveToEx( hdc, x, iTop, NULL );
            LineTo( hdc, x, iBottom );
         }
      }
   }
}


static void hb_wvg_GridHorz( PHB_GTWVT pWVT, PHB_ITEM pArray, RECT * uRect )
{
   int iAtRow = hb_arrayGetNI( pArray, 1 );
   int iRows  = hb_arrayGetNI( pArray, 4 );
   int i, y, iLeft, iRight, iTop, iBottom;
   HDC hdc;

   iLeft   = ( hb_arrayGetNI( pArray, 2 ) * pWVT->PTEXTSIZE.x );
   iRight  = ( ( ( hb_arrayGetNI( pArray, 3 ) + 1 ) * pWVT->PTEXTSIZE.x ) - 1 );
   iTop    = iAtRow * pWVT->PTEXTSIZE.y;
   iBottom = ( iAtRow + iRows ) * pWVT->PTEXTSIZE.y;  /* do not add 1 */

   if( ( uRect->left > iRight ) || ( uRect->top > iBottom ) ||
       ( uRect->bottom < iTop ) || ( uRect->right < iLeft ) )
      return;

   hdc = pWVT->hdc;
   SelectObject( hdc, pWVT->currentPen );
   for( i = 0; i < iRows; i++ )
   {
      y = ( ( iAtRow ) * pWVT->PTEXTSIZE.y );
      MoveToEx( hdc, iLeft, y, NULL );
      LineTo( hdc, iRight, y );
      iAtRow++;
   }

   hdc = pWVT->hGuiDC;
   SelectObject( hdc, pWVT->currentPen );
   for( i = 0; i < iRows; i++ )
   {
      y = ( ( iAtRow ) * pWVT->PTEXTSIZE.y );
      MoveToEx( hdc, iLeft, y, NULL );
      LineTo( hdc, iRight, y );
      iAtRow++;
   }
}

/*                       Owner Draw Implementation                      */

void hb_gt_wvt_PaintGObjects( PHB_GTWVT pWVT, RECT * uRect )
{
   PHB_GOBJS gObj = pWVT->gObjs;
   int       iTop = 0, iLeft = 0, iBottom = 0, iRight = 0;
   int       iObjType;

   while( gObj )
   {
      iObjType = 0;

      if( gObj->iState == GOBJ_OBJSTATE_ENABLED )
      {
         if( gObj->iObjType == GOBJ_OBJTYPE_OBJECT )
         {
            if( hb_vmRequestReenter() )
            {
               PHB_ITEM pArray;

               hb_vmPushEvalSym();
               hb_vmPush( gObj->bBlock );
               hb_vmSend( 0 );

               pArray = hb_param( -1, HB_IT_ARRAY );

               if( pArray && hb_arrayLen( pArray ) >= 3 )
               {
                  iObjType = gObj->iData;

                  if( iObjType == GOBJ_OBJTYPE_GRIDVERT )
                  {
                     if( hb_arrayGetNI( pArray, 4 ) > 0 )
                        hb_wvg_GridVert( pWVT, pArray, uRect );
                  }
                  else if( iObjType == GOBJ_OBJTYPE_GRIDHORZ )
                     hb_wvg_GridHorz( pWVT, pArray, uRect );
                  else
                  {
                     /* Take care of offsets 5th element */
                     iTop    = ( pWVT->PTEXTSIZE.y * hb_arrayGetNI( pArray, 1 ) );
                     iLeft   = ( pWVT->PTEXTSIZE.x * hb_arrayGetNI( pArray, 2 ) );
                     iBottom = ( pWVT->PTEXTSIZE.y * ( hb_arrayGetNI( pArray, 3 ) + 1 ) ) - 1;
                     iRight  = ( pWVT->PTEXTSIZE.x * ( hb_arrayGetNI( pArray, 4 ) + 1 ) ) - 1;
                  }
               }
               else
                  iObjType = 0;

               /*  C A R E F U L */
               #if 0
               if( pArray )
               {
                  /*hb_itemRelease( pArray );*/
                  /*pArray = NULL;*/
               }
               #endif
               hb_vmRequestRestore();
            }
         }
         else
         {
            iObjType = gObj->iObjType;

            iTop    = ( pWVT->PTEXTSIZE.y * gObj->iTop ) + gObj->aOffset.iTop;
            iLeft   = ( pWVT->PTEXTSIZE.x * gObj->iLeft ) + gObj->aOffset.iLeft;
            iBottom = ( pWVT->PTEXTSIZE.y * ( gObj->iBottom + 1 ) ) - 1 + gObj->aOffset.iBottom;
            iRight  = ( pWVT->PTEXTSIZE.x * ( gObj->iRight + 1 ) ) - 1 + gObj->aOffset.iRight;
         }

         switch( iObjType )
         {
            case GOBJ_OBJTYPE_BOXRAISED:
            case GOBJ_OBJTYPE_BOXRECESSED:
            case GOBJ_OBJTYPE_BOXGET:
            case GOBJ_OBJTYPE_BOXGROUP:
            case GOBJ_OBJTYPE_BOXGROUPRAISED:
               if( ( uRect->left > iRight + 2 ) || ( uRect->top > iBottom + 2 ) ||
                   ( uRect->bottom < iTop - 2 ) || ( uRect->right < iLeft - 2 ) )
               {
                  /* It is outside of the boundaries */
               }
               else if( uRect->left >= iLeft &&
                        uRect->right <= iRight &&
                        uRect->top >= iTop &&
                        uRect->bottom <= iBottom )
               {
                  /* It is inside of the boundaries */
               }
               else
               {
                  switch( iObjType )
                  {
                     case GOBJ_OBJTYPE_BOXRAISED:
                        hb_wvg_BoxRaised( pWVT, iLeft - 1, iTop - 1, iRight + 1, iBottom + 1 );
                        break;
                     case GOBJ_OBJTYPE_BOXRECESSED:
                        hb_wvg_BoxRecessed( pWVT, iLeft - 1, iTop - 1, iRight + 1, iBottom + 1 );
                        break;
                     case GOBJ_OBJTYPE_BOXGET:
                        hb_wvg_BoxGet( pWVT, iLeft, iTop, iRight + 1, iBottom + 1 );
                        break;
                     case GOBJ_OBJTYPE_BOXGROUP:
                        hb_wvg_BoxGroup( pWVT, iLeft - 1, iTop - 1, iRight + 1, iBottom + 1 );
                        break;
                     case GOBJ_OBJTYPE_BOXGROUPRAISED:
                        hb_wvg_BoxGroupRaised( pWVT, iLeft - 1, iTop - 1, iRight + 1, iBottom + 1 );
                        break;
                  }
               }
               break;

            case GOBJ_OBJTYPE_PICTURE:
            case GOBJ_OBJTYPE_LINE:
            case GOBJ_OBJTYPE_LINEEX:
            case GOBJ_OBJTYPE_ELLIPSE:
            case GOBJ_OBJTYPE_RECTANGLE:
            case GOBJ_OBJTYPE_ROUNDRECT:
            case GOBJ_OBJTYPE_COLORRECT:
            case GOBJ_OBJTYPE_SHADEDRECT:
            case GOBJ_OBJTYPE_TEXTBOX:
               if( ( uRect->left > iRight ) || ( uRect->top > iBottom ) ||
                   ( uRect->bottom < iTop ) || ( uRect->right < iLeft ) )
               {
                  /* Nothing to do */
               }
               else
               {
                  switch( iObjType )
                  {

                     case GOBJ_OBJTYPE_LINE:
                     case GOBJ_OBJTYPE_LINEEX:
                        hb_wvg_Line( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_ELLIPSE:
                        hb_wvg_Ellipse( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_RECTANGLE:
                        hb_wvg_Rectangle( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_ROUNDRECT:
                        hb_wvg_RoundRect( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_COLORRECT:
                        hb_wvg_ColorRect( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        hb_wvg_RestFromBuffer( pWVT, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_SHADEDRECT:
                        hb_wvg_ShadedRect( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        hb_wvg_RestFromBuffer( pWVT, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_PICTURE:
                        hb_wvg_RenderPicture( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        hb_wvg_RestFromBuffer( pWVT, iLeft, iTop, iRight, iBottom );
                        break;
                     case GOBJ_OBJTYPE_TEXTBOX:
                        hb_wvg_TextBox( pWVT, gObj, iLeft, iTop, iRight, iBottom );
                        hb_wvg_RestFromBuffer( pWVT, iLeft, iTop, iRight, iBottom );
                        break;
                  }
               }
               break;

            case GOBJ_OBJTYPE_LABEL:
            case GOBJ_OBJTYPE_LABELEX:
               #if 0
               if( ( uRect->top > iTop + gObj->lf->lfHeight ) ||
                   ( uRect->bottom < iTop ) || ( uRect->right < iLeft ) )
               {
                  /* Nothing to do */
               }
               else
               #endif
               {
                  hb_wvg_Label( pWVT, gObj, iLeft, iTop );
               }
               break;

            case GOBJ_OBJTYPE_LABEL_EX2:
               {
                  hb_wvg_LabelEx2( pWVT, gObj, iLeft, iTop, iRight, iBottom );
               }
               break;

            case GOBJ_OBJTYPE_OUTLINE:
            case GOBJ_OBJTYPE_OUTLINEEX:
               if( ( uRect->left > iRight ) || ( uRect->top > iBottom ) ||
                   ( uRect->bottom < iTop ) || ( uRect->right < iLeft ) )
               {
                  /* It is outside of the boundaries */
               }
               else if( uRect->left >= iLeft &&
                        uRect->right <= iRight &&
                        uRect->top >= iTop &&
                        uRect->bottom <= iBottom )
               {
                  /* It is inside of the boundaries */
               }
               else
                  hb_wvg_Outline( pWVT, gObj, iLeft - 1, iTop - 1, iRight + 1, iBottom + 1 );
               break;
         }
      }
      gObj = gObj->gObjNext;
   }
}
