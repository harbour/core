/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem template
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "hbapigt.h"

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   /* TODO: Is anything required to initialize the video subsystem? */
}

void hb_gt_Done( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Done()"));

   /* TODO: */
}

int hb_gt_ReadKey( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey()")); 

   /* TODO: */

   return 0;
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   USHORT row = s_usRow;
   USHORT col = s_usCol;
   ULONG ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++  )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            else
            {
               col = s_usMaxCol;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            if( row < s_usMaxRow )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < s_usMaxCol )
               col++;
            else
            {
               col = 0;
               if( row < s_usMaxRow )
                  row++;
            }
      }
   }
   hb_gt_SetPos( row, col );
   return TRUE;
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   /* TODO: How to detect this? */
   return TRUE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   /* TODO: How many columns on screen? */
   return 0;
}

USHORT hb_gt_GetScreenHeight( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   /* TODO: How many rows on screen? */
   return 0;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   /* TODO: How to reposition the cursor? */

   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

SHORT hb_gt_Col( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   /* TODO: What Column is the cursor on? */
   return 0;
}

SHORT hb_gt_Row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   /* TODO: What Row is the cursor on? */
   return 0;
}

USHORT hb_gt_GetCursorStyle( void )
{
   /* TODO: What shape is the cursor? */
   USHORT uiStyle = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

      /* example from the dos driver */
/*
      hb_gt_GetCursorSize( &start, &end )

      if( start == 32 && end == 32 )
         uiStyle = SC_NONE;

      else if( start == 6 && end == 7 )
         uiStyle = SC_NORMAL;

      else if( start == 4 && end == 7 )
         uiStyle = SC_INSERT;

      else if( start == 0 && end == 7 )
         uiStyle = SC_SPECIAL1;

      else if( start == 0 && end == 3 )
         uiStyle = SC_SPECIAL2;
   }
*/
   return uiStyle;
}

void hb_gt_SetCursorStyle( USHORT uiStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

   /* TODO: How to set the shape of the cursor? */
   /* see ..\..\..\tests\working\cursrtst.prg for an explanation */
   switch( uiStyle )
   {
   case SC_NONE:
      /* TODO: turn it off */
      break;

   case SC_NORMAL:
      break;

   case SC_INSERT:
      break;

   case SC_SPECIAL1:
      break;

   case SC_SPECIAL2:
      break;

   default:
      break;
   }
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( pbyStr );
   HB_SYMBOL_UNUSED( ulLen );
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbyDst );
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbySrc );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   /* TODO: we want to take a screen that is say bright white on blue,
            and change the attributes only for a section of the screen
            to white on black.
   */

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   /* TODO: Is there a way to change screen buffers?
            ie: can we write somewhere without it going to the screen
            and then update the screen from this buffer at a later time?
            We will initially want to copy the current screen to this buffer.
   */
}

void hb_gt_DispEnd()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   /* TODO: How to change the size of the screen? */

   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );
}

void hb_gt_Replicate( BYTE byChar, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) byChar, ulLen));

   /* TODO: this will write character c nlength times to the screen.
            Note that it is not used yet
            If there is no native function that supports this, it is
            already handled in a generic way by higher level functions.
   */

   HB_SYMBOL_UNUSED( byChar );
   HB_SYMBOL_UNUSED( ulLen );
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO: under dos, the background 'intensity' bit can be switched
            from intensity to 'blinking'
            does this work under your platform?
   */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   /* TODO: set the bit if it's supported */

   HB_SYMBOL_UNUSED( bBlink );
}
