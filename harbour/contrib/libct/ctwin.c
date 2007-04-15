/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Clipper Tools like window system
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

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	CTW

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

#include "ctwin.h"

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

#define HB_CTWIN_ALLOC        16
#define HB_CTWIN_MINROWS      1
#define HB_CTWIN_MINCOLS      1
#define HB_CTWIN_MAXROWS      255
#define HB_CTWIN_MAXCOLS      255

typedef struct
{
   int iHandle;

   BOOL fHidden;

   int iShadowAttr;
   int iCursorStyle;

   int iRow;
   int iCol;

   int iTopMargin;
   int iLeftMargin;
   int iBottomMargin;
   int iRightMargin;

   int iHeight;
   int iWidth;

   int iFirstRow;
   int iFirstCol;

   int iColorIndex;
   int iColorCount;
   int * piColors;

   PHB_SCREENCELL screenBuffer;

} HB_CT_WND, * PHB_CT_WND;

static BOOL s_fInit           = FALSE;

static int s_iShadowWidth     = 2;
static int s_iShadowAttr      = -1;

static int s_iOpenWindows     = 0;
static int s_iCurrWindow      = 0;
static int s_iMaxWindow       = 0;

static int s_fBoardSet        = FALSE;
static int s_iBoardTop        = 0;
static int s_iBoardLeft       = 0;
static int s_iBoardBottom     = 0;
static int s_iBoardRight      = 0;

static int s_fBoardTop        = FALSE;
static int s_fBoardLeft       = FALSE;
static int s_fBoardBottom     = FALSE;
static int s_fBoardRight      = FALSE;

static int s_iMoveMode        = 1;
static int s_iVerticalStep    = 2;
static int s_iHorizontalStep  = 5;

static PHB_CT_WND * s_windows = NULL;
static int * s_windowStack    = NULL;
static int * s_pWindowMap     = NULL;
static int * s_pShadowMap     = NULL;
static int s_iMapWidth        = 0;
static int s_iMapHeight       = 0;


static int hb_ctw_CalcShadowWidth( int iRows, int iCols )
{
   if( iRows + iRows >= iCols )
      return 1;
   else
      return 2;
}

static void hb_ctw_SetMap( int * piMap, int iWindow, int iTop, int iLeft, int iBottom, int iRight )
{
   ULONG lIndex;
   int i;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SetMap(%p,%d,%d,%d,%d,%d)", piMap, iWindow, iTop, iLeft, iBottom, iRight));

   if( iTop < 0 )
      iTop = 0;
   if( iBottom >= s_iMapHeight )
      iBottom = s_iMapHeight - 1;
   if( iLeft < 0 )
      iLeft = 0;
   if( iRight >= s_iMapWidth )
      iRight = s_iMapWidth - 1;

   while( iTop <= iBottom )
   {
      lIndex = iTop * s_iMapWidth + iLeft;
      for( i = iLeft; i <= iRight; ++i, ++lIndex )
         piMap[ lIndex ] = iWindow;
      ++iTop;
   }
}

static void hb_ctw_ClearMap( void )
{
   ULONG ulSize;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_ClearMap()"));

   ulSize = ( ULONG ) s_iMapHeight * s_iMapWidth * sizeof( int );
   memset( s_pWindowMap, 0, ulSize );
   memset( s_pShadowMap, 0, ulSize );
}

static void hb_ctw_WindowMap( int iWindow, BOOL fExpose )
{
   PHB_CT_WND pWnd;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_WindowMap(%d,%d)", iWindow, (int) fExpose));

   pWnd = s_windows[ iWindow ];

   if( ! pWnd->fHidden )
   {
      int iLastRow = pWnd->iFirstRow + pWnd->iHeight - 1,
          iLastCol = pWnd->iFirstCol + pWnd->iWidth - 1;

      hb_ctw_SetMap( s_pWindowMap, iWindow, 
                     pWnd->iFirstRow, pWnd->iFirstCol,
                     iLastRow, iLastCol );
      hb_ctw_SetMap( s_pShadowMap, 0,
                     pWnd->iFirstRow, pWnd->iFirstCol,
                     iLastRow, iLastCol );
      if( pWnd->iShadowAttr >= 0 &&
          iLastRow >= s_iBoardTop && iLastCol >= s_iBoardLeft &&
          pWnd->iFirstRow <= s_iBoardBottom && pWnd->iFirstCol <= s_iBoardRight )
      {
         iLastRow += 1;
         iLastCol += s_iShadowWidth;
         hb_ctw_SetMap( s_pShadowMap, iWindow,
                        iLastRow, pWnd->iFirstCol + s_iShadowWidth,
                        iLastRow, iLastCol );
         hb_ctw_SetMap( s_pShadowMap, iWindow,
                        pWnd->iFirstRow + 1, pWnd->iFirstCol + pWnd->iWidth,
                        iLastRow - 1, iLastCol );
      }
      if( fExpose )
      {
         HB_GTSUPER_EXPOSEAREA( pWnd->iFirstRow, pWnd->iFirstCol,
                             iLastRow, iLastCol );
      }
   }
}

static void hb_ctw_RemapAllWindows( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_RemapAllWindows()"));

   if( s_iMaxWindow )
   {
      int i;

      hb_ctw_ClearMap();
      for( i = 0; i < s_iOpenWindows; ++i )
         hb_ctw_WindowMap( s_windowStack[ i ], FALSE );
      HB_GTSUPER_EXPOSEAREA( 0, 0, s_iMapHeight, s_iMapWidth );
   }
}

BOOL hb_ctw_Init( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_Init()"));

   if( ! s_fInit )
   {
      s_fInit = hb_gtLoad( HB_GT_DRVNAME( HB_GT_NAME ), NULL );
      if( s_fInit )
      {
         int iRow, iCol;

         HB_GTSUPER_GETSIZE( &s_iMapHeight, &s_iMapWidth );

         /* update cursor position to the rules used by CTWIN */
         hb_gt_GetPos( &iRow, &iCol );
         hb_gt_SetPos( iRow, iCol );
      }
   }

   return s_fInit;
}

int hb_ctw_SetShadowAttr( int iAttr )
{
   int iOldAttr = s_iShadowAttr;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SetShadowAttr(%d)", iAttr));

   if( iAttr >= -1 )
      s_iShadowAttr = iAttr;

   return iOldAttr;
}

int hb_ctw_SetMoveMode( int iMode )
{
   int iOldMode = s_iMoveMode;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SetMoveMode(%d)", iMode));

   if( iMode >= 0 )
      s_iMoveMode = iMode;

   return iOldMode;
}

int hb_ctw_SetMoveStep( int iVertical, int iHorizontal )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SetMoveStep(%d,%d)", iVertical, iHorizontal));

   if( s_fInit || hb_ctw_Init() )
   {
      if( iVertical < s_iMapHeight && iHorizontal < s_iMapWidth )
      {
         s_iVerticalStep   = iVertical;
         s_iHorizontalStep = iHorizontal;

         return 0;
      }
   }

   return -1;
}

int hb_ctw_SetWindowBoard( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SetWindowBoard(%d,%d,%d,%d)", iTop, iLeft, iBottom, iRight));

   if( s_fInit || hb_ctw_Init() )
   {
      /*
       * This limitation is only for strict CT3 compatibility, the CTW GTs
       * can work in practice with any virtual board size and position and
       * is limited only by available physical memory, [druzus]
       */
      if( iBottom >= s_iMapHeight )
         iBottom = s_iMapHeight - 1;
      if( iRight >= s_iMapWidth )
         iRight = s_iMapWidth - 1;
      if( iTop >= 0 && iLeft >= 0 && iTop < iBottom && iLeft < iRight )
      {
         s_iBoardTop     = iTop;
         s_iBoardLeft    = iLeft;
         s_iBoardBottom  = iBottom;
         s_iBoardRight   = iRight;
         s_fBoardSet     = TRUE;
         hb_ctw_RemapAllWindows();

         return 0;
      }
   }

   return -1;
}

int  hb_ctw_SetBorderMode( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SetBorderMode(%d,%d,%d,%d)", iTop, iLeft, iBottom, iRight));

   if( s_fInit || hb_ctw_Init() )
   {
      if( iTop >= 0 )
         s_fBoardTop     = iTop != 0;
      if( iLeft >= 0 )
         s_fBoardLeft    = iLeft != 0;
      if( iBottom >= 0 )
         s_fBoardBottom  = iBottom != 0;
      if( iRight >= 0 )
         s_fBoardRight   = iRight != 0;

      return 0;
   }

   return -1;
}

int hb_ctw_CurrentWindow( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_CurrentWindow()"));

   return s_iCurrWindow;
}

int hb_ctw_SelectWindow( int iWindow )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_SelectWindow(%d)", iWindow));

   if( iWindow == 0 )
      s_iCurrWindow = iWindow;
   else if( iWindow != s_iCurrWindow &&
            iWindow > 0 && iWindow <= s_iMaxWindow &&
            s_windows[ iWindow ] != NULL )
   {
      s_iCurrWindow = iWindow;
      if( iWindow != s_windowStack[ s_iOpenWindows - 1 ] )
      {
         int i;

         for( i = 0; i < s_iOpenWindows; ++i )
         {
            if( s_windowStack[ i ] == iWindow )
               break;
         }
         while( i < s_iOpenWindows - 1 )
         {
            s_windowStack[ i ] = s_windowStack[ i + 1 ];
            ++i;
         }
         s_windowStack[ s_iOpenWindows - 1 ] = iWindow;
         /* INFO: CT effectively calls hb_ctw_RemapAllWindows() here */
         hb_ctw_WindowMap( iWindow, TRUE );
      }
   }

   return s_iCurrWindow;
}

int hb_ctw_MaxWindow( void )
{
   int i, iMaxHandle = 0;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_MaxWindow()"));

   for( i = 0; i < s_iOpenWindows; ++i )
   {
      if( iMaxHandle < s_windowStack[ i ] )
         iMaxHandle = s_windowStack[ i ];
   }

   return iMaxHandle;
}

int hb_ctw_CreateWindow( int iTop, int iLeft, int iBottom, int iRight, BOOL fClear, int iColor )
{
   PHB_CT_WND pWnd;
   BYTE bAttr, bColor;
   USHORT usChar;
   int iRow, iCol, iHeight, iWidth, iTmp;
   long lIndex;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_CreateWindow(%d,%d,%d,%d,%d,%d)", iTop, iLeft, iBottom, iRight, (int) fClear, iColor));

   if( s_iOpenWindows == s_iMaxWindow )
   {
      int i = s_iMaxWindow;

      if( s_iMaxWindow == 0 )
      {
         ULONG ulSize;

         if( !s_fInit )
         {
            if( ! hb_ctw_Init() )
               return -1;
         }

         HB_GTSUPER_GETSIZE( &s_iMapHeight, &s_iMapWidth );
         s_iShadowWidth = hb_ctw_CalcShadowWidth( s_iMapHeight, s_iMapWidth );
         if( !s_fBoardSet )
            hb_ctw_SetWindowBoard( 0, 0, s_iMapHeight - 1, s_iMapWidth - 1 );
         ulSize = ( ULONG ) s_iMapHeight * s_iMapWidth * sizeof( int );
         s_pWindowMap = ( int * ) hb_xgrab( ulSize );
         s_pShadowMap = ( int * ) hb_xgrab( ulSize );
         hb_ctw_ClearMap();

         s_iMaxWindow = HB_CTWIN_ALLOC;
         s_windows = ( PHB_CT_WND * ) hb_xgrab( ( HB_CTWIN_ALLOC + 1 ) * sizeof( PHB_CT_WND ) );
         s_windowStack = ( int * ) hb_xgrab( HB_CTWIN_ALLOC * sizeof( int ) );
         s_windows[ 0 ] = NULL;
      }
      else
      {
         s_iMaxWindow += HB_CTWIN_ALLOC;
         s_windows = ( PHB_CT_WND * ) hb_xrealloc( s_windows, ( s_iMaxWindow + 1 ) * sizeof( PHB_CT_WND ) );
         s_windowStack = ( int * ) hb_xrealloc( s_windowStack, s_iMaxWindow * sizeof( int ) );
      }
      do
      {
         s_windows[ i + 1 ] = NULL;
         s_windowStack[ i ] = 0;
      }
      while( ++i < s_iMaxWindow );
   }

   iHeight = iBottom - iTop + 1;
   iWidth  = iRight - iLeft + 1;
   iRow = iTop;
   iCol = iLeft;

   if( iHeight > s_iBoardBottom - s_iBoardTop + 1 )
      iHeight = s_iBoardBottom - s_iBoardTop + 1;
   if( iWidth > s_iBoardRight - s_iBoardLeft + 1 )
      iWidth = s_iBoardRight - s_iBoardLeft + 1;

   if( iHeight < HB_CTWIN_MINROWS || iWidth < HB_CTWIN_MINCOLS ||
       iHeight > HB_CTWIN_MAXROWS || iWidth > HB_CTWIN_MAXCOLS )
      return -1;

   iTop    = s_iBoardTop - ( s_fBoardTop ? iHeight : 0 );
   iBottom = s_iBoardBottom + 1 - ( s_fBoardBottom ? 0 : iHeight );
   iLeft   = s_iBoardLeft - ( s_fBoardLeft ? iWidth : 0 );
   iRight  = s_iBoardRight + 1 - ( s_fBoardRight ? 0 : iWidth );

   if( iRow < iTop )
      iRow = iTop;
   else if( iRow > iBottom )
      iRow = iBottom;
   if( iCol < iLeft )
      iCol = iLeft;
   else if( iCol > iRight )
      iCol = iRight;

   pWnd = ( PHB_CT_WND ) hb_xgrab( sizeof( HB_CT_WND ) );
   memset( pWnd, 0,  sizeof( HB_CT_WND ) );

   pWnd->fHidden = FALSE;
   pWnd->iShadowAttr = s_iShadowAttr;
   pWnd->iCursorStyle = hb_gt_GetCursorStyle();

   pWnd->iHeight = iHeight;
   pWnd->iWidth  = iWidth;
   pWnd->iFirstRow = iRow;
   pWnd->iFirstCol = iCol;

   hb_gt_GetColorData( &pWnd->piColors, &pWnd->iColorCount, &pWnd->iColorIndex );

   pWnd->screenBuffer = ( PHB_SCREENCELL ) hb_xgrab( ( ULONG ) pWnd->iHeight *
                                    pWnd->iWidth * sizeof( HB_SCREENCELL ) );

   if( pWnd->iShadowAttr >= 0 )
      fClear = TRUE;
   bAttr  = 0;
   bColor = iColor ? iColor : hb_gt_GetColor();
   usChar = ( USHORT ) hb_gt_GetClearChar();

   lIndex = 0;
   for( iRow = pWnd->iFirstRow; iRow < pWnd->iFirstRow + pWnd->iHeight; ++iRow )
   {
      for( iCol = pWnd->iFirstCol; iCol < pWnd->iFirstCol + pWnd->iWidth; ++iCol )
      {
         if( !fClear && !hb_gt_GetScrChar( iRow, iCol, &bColor, &bAttr, &usChar ) )
         {
            usChar = ( USHORT ) hb_gt_GetClearChar();
            bColor = ( BYTE ) hb_gt_GetColor();
            bAttr  = 0;
         }
         pWnd->screenBuffer[ lIndex ].c.usChar = usChar;
         pWnd->screenBuffer[ lIndex ].c.bColor = bColor;
         pWnd->screenBuffer[ lIndex ].c.bAttr  = 0;
         ++lIndex;
      }
   }

   for( iTmp = 1; iTmp < s_iMaxWindow; ++iTmp )
   {
      if( s_windows[ iTmp ] == NULL )
         break;
   }
   pWnd->iHandle = iTmp;

   s_windows[ pWnd->iHandle ] = pWnd;
   s_windowStack[ s_iOpenWindows++ ] = pWnd->iHandle;
   s_iCurrWindow = pWnd->iHandle;

   hb_ctw_WindowMap( pWnd->iHandle, TRUE );

   return pWnd->iHandle;
}

int hb_ctw_CloseWindow( int iWindow )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_CloseWindow(%d)", iWindow));

   if( iWindow > 0 && iWindow <= s_iMaxWindow && s_windows[ iWindow ] )
   {
      PHB_CT_WND pWnd = s_windows[ iWindow ];
      int i, iWnd, iLast;
      BOOL fHidden = pWnd->fHidden;

      hb_xfree( pWnd->screenBuffer );
      if( pWnd->iColorCount )
         hb_xfree( pWnd->piColors );
      hb_xfree( pWnd );
      s_windows[ iWindow ] = NULL;

      iWnd = 0;
      i = --s_iOpenWindows;
      do
      {
         iLast = s_windowStack[ i ];
         s_windowStack[ i ] = iWnd;
         if( iLast == iWindow )
            break;
         iWnd = iLast;
      }
      while( --i >= 0 );

      if( iWindow == s_iCurrWindow )
         s_iCurrWindow = s_iOpenWindows > 0 ? s_windowStack[ s_iOpenWindows - 1 ] : 0;

      if( !fHidden )
         hb_ctw_RemapAllWindows();
      return s_iCurrWindow;
   }

   return -1;
}

int hb_ctw_CloseAllWindows( void )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_CloseAllWindows()"));

   if( s_iOpenWindows > 0 )
   {
      PHB_CT_WND pWnd;
      int i, iWindow;

      for( i = 0; i < s_iOpenWindows; ++i )
      {
         iWindow = s_windowStack[ i ];
         pWnd = s_windows[ iWindow ];
         s_windowStack[ i ] = 0;
         s_windows[ iWindow ] = NULL;
         hb_xfree( pWnd->screenBuffer );
         if( pWnd->iColorCount )
            hb_xfree( pWnd->piColors );
         hb_xfree( pWnd );
      }
      s_iOpenWindows = s_iCurrWindow = 0;
      hb_ctw_RemapAllWindows();
      return 0;
   }

   return -1;
}

int hb_ctw_CenterWindow( int iWindow, BOOL fCenter )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_CenterWindow(%d,%d)", iWindow, (int) fCenter));

   if( iWindow > 0 && iWindow <= s_iOpenWindows )
   {
      PHB_CT_WND pWnd = s_windows[ iWindow ];

      if( pWnd )
      {
         if( fCenter )
         {
            int iHeight = s_iBoardBottom - s_iBoardTop + 1,
                iWidth = s_iBoardRight - s_iBoardLeft + 1;

            pWnd->iFirstRow = s_iBoardTop;
            pWnd->iFirstCol = s_iBoardLeft;

            if( iHeight > pWnd->iHeight )
               pWnd->iFirstRow += ( iHeight - pWnd->iHeight ) >> 1;
            if( iWidth > pWnd->iWidth )
               pWnd->iFirstCol += ( iWidth - pWnd->iWidth ) >> 1;
         }
         else
         {
            if( pWnd->iFirstRow > s_iBoardBottom - pWnd->iHeight + 1 )
               pWnd->iFirstRow = s_iBoardBottom - pWnd->iHeight + 1;
            if( pWnd->iFirstRow < s_iBoardTop )
               pWnd->iFirstRow = s_iBoardTop;
            if( pWnd->iFirstCol > s_iBoardRight - pWnd->iWidth + 1 )
               pWnd->iFirstCol = s_iBoardRight - pWnd->iWidth + 1;
            if( pWnd->iFirstCol < s_iBoardLeft )
               pWnd->iFirstCol = s_iBoardLeft;
         }
         return iWindow;
      }
   }

   return -1;
}

int hb_ctw_MoveWindow( int iWindow, int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_MoveWindow(%d,%d,%d)", iWindow, iRow, iCol));

   if( iWindow > 0 && iWindow <= s_iOpenWindows )
   {
      PHB_CT_WND pWnd = s_windows[ iWindow ];

      if( pWnd )
      {
         if( ( iRow + ( s_fBoardTop ? pWnd->iHeight : 0 ) >= s_iBoardTop ) &&
             ( iRow + ( s_fBoardBottom ? 0 : pWnd->iHeight ) <= s_iBoardBottom + 1 ) &&
             ( iCol + ( s_fBoardLeft ? pWnd->iWidth : 0 ) >= s_iBoardLeft ) &&
             ( iCol + ( s_fBoardRight ? 0 : pWnd->iWidth ) <= s_iBoardRight + 1 ) )
         {
            pWnd->iFirstRow = iRow;
            pWnd->iFirstCol = iCol;
            if( ! pWnd->fHidden )
               hb_ctw_RemapAllWindows();
            return iWindow;
         }
      }
   }

   return -1;
}

int hb_ctw_ChangeMargins( int iWindow, int iTop, int iLeft, int iBottom, int iRight )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_ChangeMargins(%d,%d,%d,%d,%d)", iWindow, iTop, iLeft, iBottom, iRight));

   if( iWindow > 0 && iWindow <= s_iOpenWindows )
   {
      PHB_CT_WND pWnd = s_windows[ iWindow ];

      if( pWnd )
      {
         if( ( iTop += pWnd->iTopMargin ) < 0 )
            iTop = 0;
         if( ( iLeft += pWnd->iLeftMargin ) < 0 )
            iLeft = 0;
         if( ( iBottom += pWnd->iBottomMargin ) < 0 )
            iBottom = 0;
         if( ( iRight += pWnd->iRightMargin ) < 0 )
            iRight = 0;

         if( iTop + iBottom < pWnd->iHeight && iLeft + iRight < pWnd->iWidth )
         {
            pWnd->iTopMargin    = iTop;
            pWnd->iLeftMargin   = iLeft;
            pWnd->iBottomMargin = iBottom;
            pWnd->iRightMargin  = iRight;

            return iWindow;
         }
      }
   }

   return -1;
}

int hb_ctw_GetWindowCords( int iWindow, BOOL fCenter, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_GetWindowCords(%d,%d,%p,%p,%p,%p)", iWindow, (int) fCenter, piTop, piLeft, piBottom, piRight));

   if( iWindow > 0 && iWindow <= s_iOpenWindows )
   {
      PHB_CT_WND pWnd = s_windows[ iWindow ];

      if( pWnd )
      {
         if( fCenter )
         {
            int iHeight = s_iBoardBottom - s_iBoardTop + 1,
                iWidth = s_iBoardRight - s_iBoardLeft + 1;

            *piTop  = s_iBoardTop;
            *piLeft = s_iBoardLeft;

            if( iHeight > pWnd->iHeight )
               *piTop += ( iHeight - pWnd->iHeight ) >> 1;
            if( iWidth > pWnd->iWidth )
               *piLeft += ( iWidth - pWnd->iWidth ) >> 1;
         }
         else
         {
            *piTop  = pWnd->iFirstRow;
            *piLeft = pWnd->iFirstCol;
         }
         *piBottom = *piTop + pWnd->iHeight - 1;
         *piRight  = *piLeft + pWnd->iWidth - 1;

         return iWindow;
      }
   }

   *piTop = *piLeft = 0;
   *piBottom = hb_gt_MaxRow();
   *piRight  = hb_gt_MaxCol();

   return -1;
}

int hb_ctw_GetFormatCords( int iWindow, BOOL fRelative, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_GetFormatCords(%d,%d,%p,%p,%p,%p)", iWindow, (int) fRelative, piTop, piLeft, piBottom, piRight));

   if( iWindow > 0 && iWindow <= s_iOpenWindows )
   {
      PHB_CT_WND pWnd = s_windows[ iWindow ];

      if( pWnd )
      {
         if( fRelative )
         {
            *piTop    = pWnd->iTopMargin;
            *piLeft   = pWnd->iLeftMargin;
            *piBottom = pWnd->iBottomMargin;
            *piRight  = pWnd->iRightMargin;
         }
         else
         {
            *piTop    = pWnd->iFirstRow + pWnd->iTopMargin;
            *piLeft   = pWnd->iFirstCol + pWnd->iLeftMargin;
            *piBottom = pWnd->iFirstRow + pWnd->iHeight - pWnd->iBottomMargin - 1;
            *piRight  = pWnd->iFirstCol + pWnd->iWidth - pWnd->iRightMargin - 1;
         }
         return iWindow;
      }
   }

   if( fRelative )
   {
      *piTop = *piLeft = *piBottom = *piRight = 0;
   }
   else
   {
      *piTop = *piLeft = 0;
      *piBottom = hb_gt_MaxRow();
      *piRight  = hb_gt_MaxCol();
   }

   return -1;
}

/* ********************************************************************** */

int hb_ctw_AddWindowBox( int iWindow, BYTE * szBox, int iColor )
{
   int iMaxRow, iMaxCol;

   HB_TRACE(HB_TR_DEBUG, ( "hb_ctw_AddWindowBox(%d,%p,%d)", iWindow, szBox, iColor));

   iMaxRow = hb_gt_MaxRow();
   iMaxCol = hb_gtMaxCol();

   if( iMaxRow > 1 && iMaxCol > 1 )
   {
      if( iColor == 0 )
         iColor = hb_gt_GetColor();
      hb_gt_Box( 0, 0, iMaxRow, iMaxCol, szBox, iColor );
      if( iWindow > 0 && iWindow <= s_iOpenWindows &&
          s_windows[ iWindow ] != NULL )
      {
         hb_gt_SetPos( 0, 0 );
         hb_ctw_ChangeMargins( iWindow, 1, 1, 1, 1 );
      }
      else
         hb_gt_SetPos( 1, 1 );

      return 0;
   }

   return -1;
}

/* ********************************************************************** */

static void hb_ctw_gt_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );

   s_fInit = TRUE;
}

static void hb_ctw_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Exit()"));

   if( s_iMaxWindow > 0 )
   {
      hb_ctw_CloseAllWindows();

      hb_xfree( s_windows );
      hb_xfree( s_windowStack );
      hb_xfree( s_pWindowMap );
      hb_xfree( s_pShadowMap );
      s_windows = NULL;
      s_windowStack = NULL;
      s_pWindowMap = s_pShadowMap = NULL;
      s_iMaxWindow = s_iOpenWindows = s_iCurrWindow =
      s_iMapWidth = s_iMapHeight = 0;
      s_fBoardSet = s_fInit = FALSE;
   }

   HB_GTSUPER_EXIT();
}

static int hb_ctw_MouseRow( void )
{
   int iRow;

   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_MouseRow()"));

   iRow = HB_GTSUPER_MOUSEROW();

   if( s_iCurrWindow > 0 )
      iRow -= s_windows[ s_iCurrWindow ]->iFirstRow +
              s_windows[ s_iCurrWindow ]->iTopMargin;

   return iRow;
}

static int hb_ctw_MouseCol( void )
{
   int iCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_MouseCol()"));

   iCol = HB_GTSUPER_MOUSECOL();

   if( s_iCurrWindow > 0 )
      iCol -= s_windows[ s_iCurrWindow ]->iFirstCol +
              s_windows[ s_iCurrWindow ]->iLeftMargin;

   return iCol;
}

static void hb_ctw_gt_GetPos( int * piRow, int * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetPos(%p,%p)", piRow, piCol));

   if( s_iCurrWindow > 0 )
   {
      *piRow = s_windows[ s_iCurrWindow ]->iRow;
      *piCol = s_windows[ s_iCurrWindow ]->iCol;
   }
   else
      HB_GTSUPER_GETPOS( piRow, piCol );
}

/*
 * CTWIN uses differ rules when set cursor position out of screen visible
 * area then standard Clipper's GT drivers so we have to replicate it in
 * SETPOS() method, [druzus]
 */
static void hb_ctw_gt_SetPos( int iRow, int iCol )
{
   int iHeight, iWidth;

   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetPos(%d,%d)", iRow, iCol));

   iHeight = hb_gt_MaxRow() + 1;
   iWidth = hb_gt_MaxCol() + 1;

   if( iCol > iWidth )
      iCol = iWidth;
   else if( iCol < 0 )
   {
      iRow += iCol / iWidth - 1;
      iCol = iWidth + iCol % iWidth;
   }
   if( iRow > iHeight )
      iRow = iHeight;

   if( s_iCurrWindow > 0 )
   {
      if( iRow < - s_windows[ s_iCurrWindow ]->iTopMargin )
         iRow = - s_windows[ s_iCurrWindow ]->iTopMargin;
      s_windows[ s_iCurrWindow ]->iRow = iRow;
      s_windows[ s_iCurrWindow ]->iCol = iCol;
   }
   else
   {
      if( iRow < 0 )
         iRow = 0;
      HB_GTSUPER_SETPOS( iRow, iCol );
   }
}

static int hb_ctw_gt_MaxCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_MaxCol()"));

   if( s_iCurrWindow > 0 )
      return s_windows[ s_iCurrWindow ]->iWidth -
             s_windows[ s_iCurrWindow ]->iLeftMargin -
             s_windows[ s_iCurrWindow ]->iRightMargin - 1;
   else
      return HB_GTSUPER_MAXCOL();
}

static int hb_ctw_gt_MaxRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_MaxRow()"));

   if( s_iCurrWindow > 0 )
      return s_windows[ s_iCurrWindow ]->iHeight -
             s_windows[ s_iCurrWindow ]->iTopMargin -
             s_windows[ s_iCurrWindow ]->iBottomMargin - 1;
   else
      return HB_GTSUPER_MAXROW();
}

/*
 * CTWIN uses differ rules in console output then standard Clipper's
 * GT drivers so we have to overload WRITECON() method, [druzus]
 */
#define WRITECON_BUFFER_SIZE 512

static void hb_ctw_gt_WriteCon( BYTE * pText, ULONG ulLength )
{
   int iLen = 0;
   BOOL bDisp = FALSE;
   BOOL bBell = FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   BYTE szString[ WRITECON_BUFFER_SIZE ];

   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_WriteCon(%p,%lu)", pText, ulLength));

   iMaxRow = hb_gt_MaxRow();
   iMaxCol = hb_gt_MaxCol();

   hb_gt_GetPos( &iRow, &iCol );

   if( iRow > iMaxRow || iCol > iMaxCol )
   {
      if( iRow > iMaxRow )
         iRow = iMaxRow;
      if( iCol > iMaxCol )
         iCol = iMaxCol;
      hb_gt_SetPos( iRow, iCol );
   }

   while( ulLength-- )
   {
      BYTE ch = *pText++;

      switch( ch )
      {
         case HB_CHAR_BEL:
            bDisp = bBell = TRUE;
            break;

         case HB_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               bDisp = TRUE;
            }
            else if( iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               bDisp = TRUE;
            }
            break;

         case HB_CHAR_LF:
            iCol = 0;
            ++iRow;
            bDisp = TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *pText == HB_CHAR_LF )
            {
               ++iRow;
               ++pText;
               --ulLength;
            }
            bDisp = TRUE;
            break;

         default:
            szString[ iLen++ ] = ch;
            if( ++iCol > iMaxCol )
            {
               iCol = 0;
               ++iRow;
               bDisp = TRUE;
            }
            else if( iLen >= WRITECON_BUFFER_SIZE )
               bDisp = TRUE;
      }

      if( bDisp || ulLength == 0 )
      {
         if( iLen )
            hb_gt_Write( szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            hb_gt_Scroll( 0, 0, iMaxRow, iMaxCol, ( BYTE ) hb_gt_GetColor(), hb_gt_GetClearChar(), iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         hb_gt_SetPos( iRow, iCol );
         bDisp = FALSE;

         /* To emulate scrolling */
         hb_gt_Flush();

         if( bBell )
         {
            hb_gt_Bell();
            bBell = FALSE;
         }
      }
   }
}


static int hb_ctw_gt_GetCursorStyle( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetCursorStyle()"));

   if( s_iCurrWindow > 0 )
      return s_windows[ s_iCurrWindow ]->iCursorStyle;
   else
      return HB_GTSUPER_GETCURSORSTYLE();
}

static void hb_ctw_gt_SetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_SetCursorStyle(%d)", iStyle));

   if( s_iCurrWindow > 0 )
   {
      switch( iStyle )
      {
         case SC_NONE:
         case SC_NORMAL:
         case SC_INSERT:
         case SC_SPECIAL1:
         case SC_SPECIAL2:
            s_windows[ s_iCurrWindow ]->iCursorStyle = iStyle;
            break;
         default:
            s_windows[ s_iCurrWindow ]->iCursorStyle = SC_NORMAL;
            break;
      }
   }
   else
      HB_GTSUPER_SETCURSORSTYLE( iStyle );
}

static void hb_ctw_gt_GetColorStr( char * pszColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetColorStr(%p)", pszColorString));

   if( s_iCurrWindow > 0 )
   {
      PHB_CT_WND pWnd = s_windows[ s_iCurrWindow ];
      HB_GTSUPER_COLORSTOSTRING( pWnd->piColors, pWnd->iColorCount, pszColorString, CLR_STRLEN );
   }
   else
      HB_GTSUPER_GETCOLORSTR( pszColorString );
}

static void hb_ctw_gt_SetColorStr( const char * szColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_SetColorStr(%s)", szColorString));

   if( s_iCurrWindow > 0 )
   {
      PHB_CT_WND pWnd = s_windows[ s_iCurrWindow ];
      HB_GTSUPER_STRINGTOCOLORS( szColorString, &pWnd->piColors, &pWnd->iColorCount );
      pWnd->iColorIndex = HB_CLR_STANDARD;
   }
   else
      HB_GTSUPER_SETCOLORSTR( szColorString );
}

static void hb_ctw_gt_ColorSelect( int iColorIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_ColorSelect(%d)", iColorIndex));

   if( s_iCurrWindow > 0 )
   {
      PHB_CT_WND pWnd = s_windows[ s_iCurrWindow ];
      if( iColorIndex >= 0 && iColorIndex < pWnd->iColorCount )
         pWnd->iColorIndex = iColorIndex;
   }
   else
      HB_GTSUPER_COLORSELECT( iColorIndex );
}

static int hb_ctw_gt_GetColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetColor()"));

   if( s_iCurrWindow > 0 )
   {
      PHB_CT_WND pWnd = s_windows[ s_iCurrWindow ];
      return pWnd->piColors[ pWnd->iColorIndex ];
   }
   else
      return HB_GTSUPER_GETCOLOR();
}

static void hb_ctw_gt_GetColorData( int ** pColorsPtr, int * piColorCount, int * piColorIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetColor(%p,%p,%p)", pColorsPtr, piColorCount, piColorIndex));

   if( s_iCurrWindow > 0 )
   {
      PHB_CT_WND pWnd = s_windows[ s_iCurrWindow ];

      *pColorsPtr = ( int * ) hb_xgrab( pWnd->iColorCount * sizeof( int ) );
      memcpy( *pColorsPtr, pWnd->piColors, pWnd->iColorCount * sizeof( int ) );
      *piColorCount = pWnd->iColorCount;
      *piColorIndex = pWnd->iColorIndex;
   }
   else
      HB_GTSUPER_GETCOLORDATA( pColorsPtr, piColorCount, piColorIndex );
}

static void hb_ctw_gt_GetScrCursor( int * piRow, int * piCol, int * piStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetScrCursor(%p,%p,%p)", piRow, piCol, piStyle));

   HB_GTSUPER_GETSCRCURSOR( piRow, piCol, piStyle );
   if( s_iCurrWindow > 0 )
   {
      *piRow += s_windows[ s_iCurrWindow ]->iFirstRow +
                s_windows[ s_iCurrWindow ]->iTopMargin;
      *piCol += s_windows[ s_iCurrWindow ]->iFirstCol +
                s_windows[ s_iCurrWindow ]->iLeftMargin;
      if( *piStyle != SC_NONE &&
          ( *piRow < s_iBoardTop  || *piRow > s_iBoardBottom ||
            *piCol < s_iBoardLeft || *piCol > s_iBoardRight ) )
         *piStyle = SC_NONE;
   }
}

static BOOL hb_ctw_gt_GetScrChar( int iRow, int iCol,
                                  BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   int iWindow = s_iCurrWindow, iShadow = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetScrChar(%d,%d,%p,%p,%p)", iRow, iCol, pbColor, pbAttr, pusChar));

   if( s_iOpenWindows > 0 )
   {
      if( iRow < s_iBoardTop  || iRow > s_iBoardBottom ||
          iCol < s_iBoardLeft || iCol > s_iBoardRight )
         iWindow = 0;
      else
      {
         long lIndex = ( long ) iRow * s_iMapWidth + iCol;
         iWindow = s_pWindowMap[ lIndex ];
         iShadow = s_pShadowMap[ lIndex ];
      }
   }

   if( iWindow > 0 )
   {
      iRow -= s_windows[ iWindow ]->iFirstRow;
      iCol -= s_windows[ iWindow ]->iFirstCol;
      if( iCol >= 0 && iRow >= 0 && iRow < s_windows[ iWindow ]->iHeight &&
          iCol < s_windows[ iWindow ]->iWidth )
      {
         long lIndex = ( long ) iRow * s_windows[ iWindow ]->iWidth + iCol;
         *pusChar = s_windows[ iWindow ]->screenBuffer[ lIndex ].c.usChar;
         *pbColor = s_windows[ iWindow ]->screenBuffer[ lIndex ].c.bColor;
         *pbAttr  = s_windows[ iWindow ]->screenBuffer[ lIndex ].c.bAttr;
      }
      else
         return FALSE;
   }
   else if( ! HB_GTSUPER_GETSCRCHAR( iRow, iCol, pbColor, pbAttr, pusChar ) )
      return FALSE;

   if( iShadow > 0 )
   {
      if( s_windows[ iShadow ]->iShadowAttr >= 0 )
         *pbColor = ( BYTE ) s_windows[ iShadow ]->iShadowAttr;
      *pbAttr |= HB_GT_ATTR_SHADOW;
   }

   return TRUE;
}

static BOOL hb_ctw_gt_GetChar( int iRow, int iCol,
                               BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_GetChar(%d,%d,%p,%p,%p)", iRow, iCol, pbColor, pbAttr, pusChar));

   if( s_iCurrWindow == 0 )
      /* TODO: it may badly interacts with character translations */
      return hb_gt_GetScrChar( iRow, iCol, pbColor, pbAttr, pusChar );

   iRow += s_windows[ s_iCurrWindow ]->iTopMargin;
   iCol += s_windows[ s_iCurrWindow ]->iLeftMargin;

   if( iCol >= 0 && iRow >= 0 &&
       iRow < s_windows[ s_iCurrWindow ]->iHeight &&
       iCol < s_windows[ s_iCurrWindow ]->iWidth )
   {
      long lIndex = ( long ) iRow * s_windows[ s_iCurrWindow ]->iWidth + iCol;
      *pusChar = s_windows[ s_iCurrWindow ]->screenBuffer[ lIndex ].c.usChar;
      *pbColor = s_windows[ s_iCurrWindow ]->screenBuffer[ lIndex ].c.bColor;
      *pbAttr  = s_windows[ s_iCurrWindow ]->screenBuffer[ lIndex ].c.bAttr;
      return TRUE;
   }

   return FALSE;
}

static BOOL hb_ctw_gt_PutChar( int iRow, int iCol,
                               BYTE bColor, BYTE bAttr, USHORT usChar )
{
   int iWindow = s_iCurrWindow;

   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_PutChar(%d,%d,%d,%d,%hu)", iRow, iCol, (int)bColor, (int)bAttr, (int)usChar));

   if( s_iOpenWindows > 0 && iWindow == 0 )
   {
      if( iRow >= s_iBoardTop  && iRow <= s_iBoardBottom &&
          iCol >= s_iBoardLeft && iCol <= s_iBoardRight )
      {
         long lIndex = ( long ) iRow * s_iMapWidth + iCol;
         iWindow = s_pWindowMap[ lIndex ];
         s_pShadowMap[ lIndex ] = 0;
      }
   }

   if( iWindow > 0 )
   {
      int iWndRow, iWndCol;

      if( s_iCurrWindow == 0 )
      {
         iWndRow = iRow - s_windows[ iWindow ]->iFirstRow;
         iWndCol = iCol - s_windows[ iWindow ]->iFirstCol;
      }
      else
      {
         iWndRow = iRow + s_windows[ iWindow ]->iTopMargin;
         iWndCol = iCol + s_windows[ iWindow ]->iLeftMargin;
         iRow = iWndRow + s_windows[ iWindow ]->iFirstRow;
         iCol = iWndCol + s_windows[ iWindow ]->iFirstCol;
      }
      if( iWndCol >= 0 && iWndRow >= 0 &&
          iWndRow < s_windows[ iWindow ]->iHeight &&
          iWndCol < s_windows[ iWindow ]->iWidth )
      {
         long lIndex = ( long ) iWndRow * s_windows[ iWindow ]->iWidth + iWndCol;

         s_windows[ iWindow ]->screenBuffer[ lIndex ].c.usChar = usChar;
         s_windows[ iWindow ]->screenBuffer[ lIndex ].c.bColor = bColor;
         s_windows[ iWindow ]->screenBuffer[ lIndex ].c.bAttr  = bAttr;
         if( ! s_windows[ iWindow ]->fHidden )
         {
            if( s_iCurrWindow == 0 )
               HB_GTSUPER_TOUCHCELL( iRow, iCol );
            else if( iRow >= s_iBoardTop  && iRow <= s_iBoardBottom &&
                     iCol >= s_iBoardLeft && iCol <= s_iBoardRight )
               HB_GTSUPER_TOUCHCELL( iRow, iCol );
         }
         return TRUE;
      }
      return FALSE;
   }

   return HB_GTSUPER_PUTCHAR( iRow, iCol, bColor, bAttr, usChar );
}

static BOOL hb_ctw_gt_Resize( int iRows, int iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctw_gt_Resize(%d,%d)", iRows, iCols));

   if( HB_GTSUPER_RESIZE( iRows, iCols ) )
   {
      if( s_iMaxWindow > 0 )
      {
         ULONG ulSize;

         s_iMapHeight = iRows;
         s_iMapWidth  = iCols;
         s_iShadowWidth = hb_ctw_CalcShadowWidth( s_iMapHeight, s_iMapWidth );
         ulSize = ( ULONG ) s_iMapHeight * s_iMapWidth * sizeof( int );
         s_pWindowMap = ( int * ) hb_xrealloc( s_pWindowMap, ulSize );
         s_pShadowMap = ( int * ) hb_xrealloc( s_pShadowMap, ulSize );
      }
      if( s_fBoardSet )
         hb_ctw_SetWindowBoard( 0, 0, iRows - 1, iCols - 1 );
      return TRUE;
   }
   return FALSE;
}

static BOOL hb_ctw_gt_Info( int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_ctw_gt_Info(%d,%p)", iType, pInfo ) );

   switch ( iType )
   {
      case GTI_ISCTWIN:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, TRUE );
         break;

      case GTI_NEWWIN:
      {
         BOOL fResult;

         hb_ctw_SelectWindow( 0 );
         fResult = HB_GTSUPER_INFO( iType, pInfo );

         if( fResult && hb_arrayLen( pInfo->pResult ) >= 8 )
            hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 8 ),
                          s_iCurrWindow );
         return fResult;
      }
      case GTI_GETWIN:
      {
         BOOL fResult;
         int iWindow = s_iCurrWindow;

         hb_ctw_SelectWindow( 0 );
         fResult = HB_GTSUPER_INFO( iType, pInfo );
         if( fResult && hb_arrayLen( pInfo->pResult ) >= 8 )
            hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 8 ), iWindow );
         return fResult;
      }
      case GTI_SETWIN:
      {
         BOOL fResult;

         hb_ctw_SelectWindow( 0 );
         fResult = HB_GTSUPER_INFO( iType, pInfo );
         if( hb_arrayLen( pInfo->pNewVal ) == 8 )
            hb_ctw_SelectWindow( hb_arrayGetNI( pInfo->pNewVal, 8 ) );
         return fResult;
      }
      default:
         return HB_GTSUPER_INFO( iType, pInfo );
   }

   return TRUE;
}

static int hb_ctw_gt_Alert( PHB_ITEM pMessage, PHB_ITEM pOptions,
                            int iClrNorm, int iClrHigh, double dDelay )
{
   int iOptions, iRet = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_ctw_gt_Alert(%p,%p,%d,%d,%f)", pMessage, pOptions, iClrNorm, iClrHigh, dDelay ) );

   iOptions = ( int ) hb_arrayLen( pOptions );

   if( HB_IS_STRING( pMessage ) && iOptions > 0 )
   {
      int iRows, iCols;
      BOOL fScreen;

      hb_gt_GetSize( &iRows, &iCols );
      if( iCols <= 4 || iRows <= 4 )
         fScreen = FALSE;
      else
      {
         HB_GT_INFO gtInfo;
         gtInfo.pNewVal = gtInfo.pResult = NULL;
         hb_gt_Info( GTI_FULLSCREEN, &gtInfo );
         fScreen = gtInfo.pResult && hb_itemGetL( gtInfo.pResult );
         hb_gt_Info( GTI_KBDSUPPORT, &gtInfo );
         if( gtInfo.pResult )
         {
            if( !hb_itemGetL( gtInfo.pResult ) )
               fScreen = FALSE;
            hb_itemRelease( gtInfo.pResult );
         }
      }
      if( fScreen )
      {
         ULONG ulWidth = 0, ulCurrWidth = 0, ul = 0, ul2, ulMaxWidth, ulLast;
         int iKey, iDspCount, iLines = 0, iTop, iLeft, iBottom, iRight,
             iMnuCol, iPos, iClr, iWnd, i;
         char * szMessage = hb_itemGetCPtr( pMessage );
         ULONG ulLen = hb_itemGetCLen( pMessage );

         ulMaxWidth = iCols - 4;
         while( ul < ulLen )
         {
            if( szMessage[ ul ] == '\n' )
            {
               ++iLines;
               if( ulCurrWidth > ulWidth )
                  ulWidth = ulCurrWidth;
               ulCurrWidth = 0;
            }
            else
               ++ulCurrWidth;
            ++ul;
         }
         if( ulCurrWidth )
            ++iLines;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         ulCurrWidth = 0;
         for( i = 1; i <= iOptions; ++i )
         {
            ulCurrWidth += hb_arrayGetCLen( pOptions, i ) + ( i > 1 ? 3 : 0 );
         }
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ulWidth > ulMaxWidth )
            ulWidth = ulMaxWidth;
         if( iRows < iLines + 4 )
            iLines = iRows - 4;
         iTop = ( iRows - iLines - 4 ) >> 1;
         iLeft = ( iCols - ulWidth - 4 ) >> 1;
         iBottom = iTop + iLines + 3;
         iRight = iLeft + ulWidth + 3;
         if( iClrNorm == 0 )
            iClrNorm = 79;
         if( iClrHigh == 0 )
            iClrHigh = 31;

         iDspCount = hb_gt_DispCount();
         if( iDspCount == 0 )
            hb_gt_DispBegin();

         iWnd = hb_ctw_CreateWindow( iTop, iLeft, iBottom, iRight, TRUE, iClrNorm );
         hb_ctw_AddWindowBox( iWnd, ( BYTE * ) _B_SINGLE, iClrNorm );
         hb_gt_SetCursorStyle( SC_NONE );
         ulLast = 0;
         i = 0;
         for( ul = 0; ul < ulLen; ++ul )
         {
            if( szMessage[ ul ] == '\n' )
            {
               if( ul > ulLast )
               {
                  ul2 = ul - ulLast;
                  if( ul2 > ulWidth )
                     ul2 = ulWidth;
                  hb_gt_PutText( i, ( ( ulWidth - ul2 + 1 ) >> 1 ) + 1, iClrNorm,
                                 ( BYTE * ) szMessage + ulLast, ul2 );
               }
               ulLast = ul + 1;
               if( ++i >= iLines )
                  break;
            }
         }
         if( ul > ulLast && i < iLines )
         {
            ul2 = ul - ulLast;
            if( ul2 > ulWidth )
               ul2 = ulWidth;
            hb_gt_PutText( i, ( ( ulWidth - ul2 + 1 ) >> 1 ) + 1, iClrNorm,
                           ( BYTE * ) szMessage + ulLast, ul2 );
         }

         iPos = 1;
         while( iRet == 0 )
         {
            hb_gt_DispBegin();
            iMnuCol = ( ( ulWidth - ulCurrWidth ) >> 1 ) + 1;
            for( i = 1; i <= iOptions; ++i )
            {
               iClr = i == iPos ? iClrHigh : iClrNorm;
               ulLen = hb_arrayGetCLen( pOptions, i );
               hb_gt_PutText( iLines + 1, iMnuCol, iClr,
                              ( BYTE * ) hb_arrayGetCPtr( pOptions, i ), ulLen );
               iMnuCol += ulLen + 3;
            }
            while( hb_gt_DispCount() )
               hb_gt_DispEnd();
            hb_gt_Refresh();

            iKey = hb_inkey( TRUE, dDelay, INKEY_ALL );
            /* TODO: add support for SET KEY blocks */

            if( iKey == K_ESC )
               break;
            else if( iKey == K_ENTER || iKey == K_SPACE || iKey == 0 )
            {
               iRet = iPos;
            }
            else if( iKey == K_LEFT || iKey == K_SH_TAB )
            {
               if( --iPos == 0 )
                  iPos = iOptions;
               dDelay = 0.0;
            }
            else if( iKey == K_RIGHT || iKey == K_TAB )
            {
               if( ++iPos > iOptions )
                  iPos = 1;
               dDelay = 0.0;
            }
#ifdef HB_COMPAT_C53
            else if( iKey == K_LBUTTONDOWN )
            {
               int iMRow = hb_mouse_Row(), iMCol = hb_mouse_Col();
               if( iMRow == iLines + 1 )
               {
                  iMnuCol = ( ( ulWidth - ulCurrWidth ) >> 1 ) + 1;
                  for( i = 1; i <= iOptions; ++i )
                  {
                     ulLen = hb_arrayGetCLen( pOptions, i );
                     if( iMCol >= iMnuCol && iMCol < iMnuCol + ( int ) ulLen )
                     {
                        iRet = i;
                        break;
                     }
                     iMnuCol += ulLen + 3;
                  }
               }
            }
#endif
            else if( iKey >= 32 && iKey <= 255 )
            {
               int iUp = hb_charUpper( iKey );
               for( i = 1; i <= iOptions; ++i )
               {
                  char *szValue = hb_arrayGetCPtr( pOptions, i );
                  if( szValue && iUp == hb_charUpper( *szValue ) )
                  {
                     iRet = i;
                     break;
                  }
               }
            }
         }

         hb_ctw_CloseWindow( iWnd );
         hb_gt_Refresh();

         while( hb_gt_DispCount() < iDspCount )
            hb_gt_DispBegin();

         return iRet;
      }
   }

   return HB_GTSUPER_ALERT( pMessage, pOptions, iClrNorm, iClrHigh, dDelay );
}


static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_ctw_gt_Init;
   pFuncTable->Exit                       = hb_ctw_gt_Exit;
   pFuncTable->MouseRow                   = hb_ctw_MouseRow;
   pFuncTable->MouseCol                   = hb_ctw_MouseCol;
   pFuncTable->MaxCol                     = hb_ctw_gt_MaxCol;
   pFuncTable->MaxRow                     = hb_ctw_gt_MaxRow;
   pFuncTable->GetPos                     = hb_ctw_gt_GetPos;
   pFuncTable->SetPos                     = hb_ctw_gt_SetPos;
   pFuncTable->WriteCon                   = hb_ctw_gt_WriteCon;
   pFuncTable->GetCursorStyle             = hb_ctw_gt_GetCursorStyle;
   pFuncTable->SetCursorStyle             = hb_ctw_gt_SetCursorStyle;
   pFuncTable->GetColorStr                = hb_ctw_gt_GetColorStr;
   pFuncTable->SetColorStr                = hb_ctw_gt_SetColorStr;
   pFuncTable->ColorSelect                = hb_ctw_gt_ColorSelect;
   pFuncTable->GetColor                   = hb_ctw_gt_GetColor;
   pFuncTable->GetColorData               = hb_ctw_gt_GetColorData;
   pFuncTable->GetScrCursor               = hb_ctw_gt_GetScrCursor;
   pFuncTable->GetScrChar                 = hb_ctw_gt_GetScrChar;
   pFuncTable->GetChar                    = hb_ctw_gt_GetChar;
   pFuncTable->PutChar                    = hb_ctw_gt_PutChar;
   pFuncTable->Resize                     = hb_ctw_gt_Resize;
   pFuncTable->Info                       = hb_ctw_gt_Info;
   pFuncTable->Alert                      = hb_ctw_gt_Alert;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif

/* *********************************************************************** */
