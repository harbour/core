/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2007 Pritpal Bedi <pritpal@vouchcac.com>
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
/*-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                Simplified GUI Drawing Functions
//                     CUI Enhancement Oriented
//                         No Callbacks etc.
//                            Pure Statics
//                                 .
//               Pritpal Bedi <pritpal@vouchcac.com>
//                            28Feb2009
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------*/

#define HB_OS_WIN_USED

#include "gtwvg.h"

/*----------------------------------------------------------------------*/
//   Wvg_BoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVG_BOXRAISED )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = ( HB_GOBJS *) hb_xgrab( sizeof( HB_GOBJS ) );

   gObj->iObjType = GOBJ_OBJTYPE_BOXRAISED;

   gObj->iTop     = hb_parni( 1 );
   gObj->iLeft    = hb_parni( 2 );
   gObj->iBottom  = hb_parni( 3 );
   gObj->iRight   = hb_parni( 4 );

   gObj->aOffset.iTop     = hb_parni( 5,1 );
   gObj->aOffset.iLeft    = hb_parni( 5,2 );
   gObj->aOffset.iBottom  = hb_parni( 5,3 );
   gObj->aOffset.iRight   = hb_parni( 5,4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs = gObj;
}
/*----------------------------------------------------------------------*/
//   Wvg_BoxRecessed( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVG_BOXRECESSED )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = ( HB_GOBJS *) hb_xgrab( sizeof( HB_GOBJS ) );

   gObj->iObjType = GOBJ_OBJTYPE_BOXRECESSED;

   gObj->iTop     = hb_parni( 1 );
   gObj->iLeft    = hb_parni( 2 );
   gObj->iBottom  = hb_parni( 3 );
   gObj->iRight   = hb_parni( 4 );

   gObj->aOffset.iTop     = hb_parni( 5,1 );
   gObj->aOffset.iLeft    = hb_parni( 5,2 );
   gObj->aOffset.iBottom  = hb_parni( 5,3 );
   gObj->aOffset.iRight   = hb_parni( 5,4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs = gObj;
}
/*----------------------------------------------------------------------*/
//   Wvt_DrawBoxGet( nRow, nCol, nWidth )
//
HB_FUNC( WVG_BOXGET )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();

   HB_GOBJS * gObj = ( HB_GOBJS *) hb_xgrab( sizeof( HB_GOBJS ) );

   gObj->iObjType = GOBJ_OBJTYPE_BOXGET;

   gObj->iTop     = hb_parni( 1 );
   gObj->iLeft    = hb_parni( 2 );
   gObj->iBottom  = hb_parni( 1 );
   gObj->iRight   = hb_parni( 2 ) + hb_parni( 3 ) - 1;

   gObj->aOffset.iTop     = 0;
   gObj->aOffset.iLeft    = 0;
   gObj->aOffset.iBottom  = 0;
   gObj->aOffset.iRight   = 0;

   gObj->gObjNext = pWVT->gObjs;

   pWVT->gObjs = gObj;
}
/*----------------------------------------------------------------------*/
//    Wvg_BoxGroup( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVG_BOXGROUP )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = ( HB_GOBJS *) hb_xgrab( sizeof( HB_GOBJS ) );

   gObj->iObjType = GOBJ_OBJTYPE_BOXGROUP;

   gObj->iTop     = hb_parni( 1 );
   gObj->iLeft    = hb_parni( 2 );
   gObj->iBottom  = hb_parni( 3 );
   gObj->iRight   = hb_parni( 4 );

   gObj->aOffset.iTop     = hb_parni( 5,1 );
   gObj->aOffset.iLeft    = hb_parni( 5,2 );
   gObj->aOffset.iBottom  = hb_parni( 5,3 );
   gObj->aOffset.iRight   = hb_parni( 5,4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs = gObj;
}
/*----------------------------------------------------------------------*/
//    Wvg_BoxRaised( nTop, nLeft, nBottom, nRight, aPxlOff )
//
HB_FUNC( WVG_BOXGROUPRAISED )
{
   PHB_GTWVT pWVT = hb_wvt_gtGetWVT();
   HB_GOBJS * gObj = ( HB_GOBJS *) hb_xgrab( sizeof( HB_GOBJS ) );

   gObj->iObjType = GOBJ_OBJTYPE_BOXGROUPRAISED;

   gObj->iTop     = hb_parni( 1 );
   gObj->iLeft    = hb_parni( 2 );
   gObj->iBottom  = hb_parni( 3 );
   gObj->iRight   = hb_parni( 4 );

   gObj->aOffset.iTop     = hb_parni( 5,1 );
   gObj->aOffset.iLeft    = hb_parni( 5,2 );
   gObj->aOffset.iBottom  = hb_parni( 5,3 );
   gObj->aOffset.iRight   = hb_parni( 5,4 );

   gObj->gObjNext = pWVT->gObjs;
   pWVT->gObjs = gObj;
}
/*-------------------------------------------------------------------//


//-------------------------------------------------------------------*/
static void hb_wvg_BoxRaised( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC         hdc  = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penWhiteDim );
   MoveToEx( hdc, iLeft, iTop, NULL );        //  Top Inner
   LineTo( hdc, iRight, iTop );
   MoveToEx( hdc, iLeft, iTop, NULL );        //  Left Inner
   LineTo( hdc, iLeft, iBottom );

   SelectObject( hdc, pGUI->penWhite );
   MoveToEx( hdc, iLeft-1, iTop-1, NULL );    //  Top Outer
   LineTo( hdc, iRight+1, iTop-1 );
   MoveToEx( hdc, iLeft-1, iTop-1, NULL );    //  Left Outer
   LineTo( hdc, iLeft-1, iBottom+1 );

   SelectObject( hdc, pGUI->penDarkGray );
   MoveToEx( hdc, iLeft, iBottom, NULL );     //  Bottom Inner
   LineTo( hdc, iRight, iBottom );
   MoveToEx( hdc, iRight, iBottom, NULL );    //  Right Inner
   LineTo( hdc, iRight, iTop );

   SelectObject( hdc, pGUI->penBlack );
   MoveToEx( hdc, iLeft-1, iBottom+1, NULL ); //  Bottom Outer
   LineTo( hdc, iRight+1+1, iBottom+1 );
   MoveToEx( hdc, iRight+1, iTop-1, NULL );   //  Right Outer
   LineTo( hdc, iRight+1, iBottom+1 );
}
/*----------------------------------------------------------------------*/
static void hb_wvg_BoxRecessed( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC         hdc  = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penWhiteDim );
   MoveToEx( hdc, iRight, iTop, NULL );            // Right Inner
   LineTo( hdc, iRight, iBottom );
   MoveToEx( hdc, iLeft, iBottom, NULL );          // Bottom Inner
   LineTo( hdc, iRight, iBottom );

   SelectObject( hdc, pGUI->penWhite );
   MoveToEx( hdc, iRight+1, iTop-1, NULL );        // Right Outer
   LineTo( hdc, iRight + 1, iBottom + 1 );
   MoveToEx( hdc, iLeft - 1, iBottom + 1, NULL );  // Bottom Outer
   LineTo( hdc, iRight + 2, iBottom + 1 );

   SelectObject( hdc, pGUI->penBlack );
   MoveToEx( hdc, iLeft, iTop, NULL );             // Left Inner
   LineTo( hdc, iLeft, iBottom );
   MoveToEx( hdc, iLeft, iTop, NULL );             // Top Inner
   LineTo( hdc, iRight, iTop );

   SelectObject( hdc, pGUI->penDarkGray );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Left Outer
   LineTo( hdc, iLeft - 1 , iBottom + 1 );
   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Top Outer
   LineTo( hdc, iRight + 1, iTop - 1 );
}
/*----------------------------------------------------------------------*/
static void hb_wvg_BoxGet( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC         hdc  = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penBlack );
   MoveToEx( hdc, iLeft-1 , iTop-1, NULL );        // Top Inner
   LineTo(   hdc, iRight-1, iTop-1       );
   MoveToEx( hdc, iLeft-1 , iTop-1, NULL );        // Left Inner
   LineTo(   hdc, iLeft-1 , iBottom-1    );

   SelectObject( hdc, pGUI->penDarkGray );
   MoveToEx( hdc, iLeft-2, iTop-2, NULL );         // Top Outer
   LineTo(   hdc, iRight , iTop-2       );
   MoveToEx( hdc, iLeft-2, iTop-2, NULL );         // Left Outer
   LineTo(   hdc, iLeft-2, iBottom      );
}
/*----------------------------------------------------------------------*/
static void hb_wvg_BoxGroup( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC         hdc  = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penDarkGray );

   MoveToEx( hdc, iRight, iTop, NULL );            // Right Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );          // Bottom Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Left Outer
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );     // Top Outer
   LineTo( hdc, iRight + 1, iTop - 1 );


   SelectObject( hdc, pGUI->penWhite );

   MoveToEx( hdc, iRight + 1, iTop, NULL );        // Right Outer
   LineTo( hdc, iRight + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft -1, iBottom + 1, NULL );   // Bottom Outer
   LineTo( hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( hdc, iLeft, iTop, NULL );             // Left Inner
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );             // Top Inner
   LineTo( hdc, iRight, iTop );
}
/*----------------------------------------------------------------------*/
static void hb_wvg_BoxGroupRaised( PHB_GTWVT pWVT, int iLeft, int iTop, int iRight, int iBottom )
{
   HDC         hdc  = pWVT->hdc;
   PHB_GUIDATA pGUI = pWVT->pGUI;

   SelectObject( hdc, pGUI->penWhite );

   MoveToEx( hdc, iRight, iTop, NULL );           // Right Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft, iBottom, NULL );         // Bottom Inner
   LineTo( hdc, iRight, iBottom );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );    // Left Outer
   LineTo( hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( hdc, iLeft - 1, iTop - 1, NULL );    // Top Outer
   LineTo( hdc, iRight + 1, iTop - 1 );

   SelectObject( hdc, pGUI->penDarkGray );

   MoveToEx( hdc, iRight + 1, iTop, NULL );       // Right Outer
   LineTo( hdc, iRight + 1, iBottom + 1 );

   MoveToEx( hdc, iLeft -1, iBottom + 1, NULL );  // Bottom Outer
   LineTo( hdc, iRight + 1 + 1, iBottom + 1);

   MoveToEx( hdc, iLeft, iTop, NULL );            // Left Inner
   LineTo( hdc, iLeft, iBottom );

   MoveToEx( hdc, iLeft, iTop, NULL );            // Top Inner
   LineTo( hdc, iRight, iTop );
}
/*----------------------------------------------------------------------*/
void hb_gt_wvt_PaintGObjects( PHB_GTWVT pWVT, RECT *uRect )
{
   PHB_GOBJS gObj = pWVT->gObjs;
   int iTop, iLeft, iBottom, iRight;

   while( gObj )
   {
      iTop    = ( pWVT->PTEXTSIZE.y * gObj->iTop  ) + gObj->aOffset.iTop;
      iLeft   = ( pWVT->PTEXTSIZE.x * gObj->iLeft ) + gObj->aOffset.iLeft;
      iBottom = ( pWVT->PTEXTSIZE.y * ( gObj->iBottom + 1 ) ) - 1 + gObj->aOffset.iBottom;
      iRight  = ( pWVT->PTEXTSIZE.x * ( gObj->iRight  + 1 ) ) - 1 + gObj->aOffset.iRight;

      switch( gObj->iObjType )
      {
         case GOBJ_OBJTYPE_BOXRAISED:
         case GOBJ_OBJTYPE_BOXRECESSED:
         case GOBJ_OBJTYPE_BOXGET:
         case GOBJ_OBJTYPE_BOXGROUP:
         case GOBJ_OBJTYPE_BOXGROUPRAISED:
         {
#if 0
hb_ToOutDebug( "[uRect %i %i %i %i ][gObj %i %i %i %i ]", uRect->left, uRect->top,
                     uRect->right, uRect->bottom, iLeft, iTop, iRight, iBottom );
#endif
            if( ( uRect->left > iRight + 2 ) || ( uRect->top > iBottom + 2 ) ||
                ( uRect->bottom < iTop - 2 ) || ( uRect->right < iLeft - 2 )
               )
            {
               // It is outside of the boundaries
            }
            else if( uRect->left   >= iLeft   &&
                     uRect->right  <= iRight  &&
                     uRect->top    >= iTop    &&
                     uRect->bottom <= iBottom  )
            {
               // It is inside of the boundaries
            }
            else
            {
#if 0
hb_ToOutDebug( "Box Painted" );
#endif
               switch( gObj->iObjType )
               {
                  case GOBJ_OBJTYPE_BOXRAISED:
                     hb_wvg_BoxRaised( pWVT, iLeft-1, iTop-1, iRight+1, iBottom+1 );
                     break;
                  case GOBJ_OBJTYPE_BOXRECESSED:
                     hb_wvg_BoxRecessed( pWVT, iLeft-1, iTop-1, iRight+1, iBottom+1 );
                     break;
                  case GOBJ_OBJTYPE_BOXGET:
                     hb_wvg_BoxGet( pWVT, iLeft, iTop, iRight+1, iBottom+1 );
                     break;
                  case GOBJ_OBJTYPE_BOXGROUP:
                     hb_wvg_BoxGroup( pWVT, iLeft-1, iTop-1, iRight+1, iBottom+1 );
                     break;
                  case GOBJ_OBJTYPE_BOXGROUPRAISED:
                     hb_wvg_BoxGroupRaised( pWVT, iLeft-1, iTop-1, iRight+1, iBottom+1 );
                     break;
               }
            }
         }
         break;
      }
      gObj = gObj->gObjNext;
   }
}
/*----------------------------------------------------------------------*/
