/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* NOTE: User programs should never call this layer directly! */

/* TODO: include any standard headers here */

#include "gtapi.h"

static SHORT  s_iRow;
static SHORT  s_iCol;
static USHORT s_uiMaxRow;
static USHORT s_uiMaxCol;
static USHORT s_uiCursorStyle;
static BOOL   s_bBlink;

void hb_gt_Init( void )
{
   s_iRow = 0;
   s_iCol = 0;
#if defined(HB_OS_UNIX_COMPATIBLE)
   s_uiMaxRow = 24;
#else
   s_uiMaxRow = 25;
#endif
   s_uiMaxCol = 80;
   s_uiCursorStyle = SC_NONE;
   s_bBlink = FALSE;
}

void hb_gt_Done( void )
{
   ;
}

BOOL hb_gt_IsColor( void )
{
   return FALSE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   return s_uiMaxCol;
}

USHORT hb_gt_GetScreenHeight( void )
{
   return s_uiMaxRow;
}

void hb_gt_SetPos( USHORT uiRow, USHORT uiCol )
{
   s_iCol = ( SHORT ) uiCol;
   s_iRow = ( SHORT ) uiRow;
}

USHORT hb_gt_Col( void )
{
   return s_iCol;
}

USHORT hb_gt_Row( void )
{
   return s_iRow;
}

USHORT hb_gt_GetCursorStyle( void )
{
   return s_uiCursorStyle;
}

void hb_gt_SetCursorStyle( USHORT uiCursorStyle )
{
   s_uiCursorStyle = uiCursorStyle;
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( pbyStr );
   HB_SYMBOL_UNUSED( ulLen );
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst )
{
   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbyDst );
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbySrc );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );
}

void hb_gt_DispBegin( void )
{
   ; /* Do nothing */
}

void hb_gt_DispEnd()
{
   ; /* Do nothing */
}

BOOL hb_gt_SetMode( USHORT uiMaxRow, USHORT uiMaxCol )
{
   s_uiMaxRow = uiMaxRow;
   s_uiMaxCol = uiMaxCol;
}

void hb_gt_Replicate( BYTE byChar, ULONG ulLen )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( byChar );
   HB_SYMBOL_UNUSED( ulLen );
}

BOOL hb_gt_GetBlink()
{
   return s_bBlink;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   s_bBlink = bBlink;
}
