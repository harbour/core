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

#include "gtapi.h"

void hb_gt_Init( void )
{
   /* TODO: Is anything required to initialize the video subsystem? */
}

void hb_gt_Done( void )
{
   /* TODO: */
}

BOOL hb_gt_IsColor( void )
{
   /* TODO: How to detect this? */
   return TRUE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   /* TODO: How many columns on screen? */
   return 0;
}

USHORT hb_gt_GetScreenHeight( void )
{
   /* TODO: How many rows on screen? */
   return 0;
}

void hb_gt_SetPos( USHORT uiRow, USHORT uiCol )
{
   /* TODO: How to reposition the cursor? */
}

USHORT hb_gt_Col( void )
{
   /* TODO: What Column is the cursor on? */
   return 0;
}

USHORT hb_gt_Row( void )
{
   /* TODO: What Row is the cursor on? */
   return 0;
}

static void hb_gt_GetCursorSize( char * start, char * end )
{
   /* TODO: if your system supports the concept of cursor scan lines,
            fill this in - otherwise, you need some way to detect the
            size of the current screen cursor. */
   *start = 0;
   *end = 0;
}

USHORT hb_gt_GetCursorStyle( void )
{
   /* TODO: What shape is the cursor? */
   USHORT rc = 0;
/*
   char start, end;
   if( !visible )
   {
      rc = SC_NONE;
   }
   else
   {
*/
      /* example from the dos driver */
/*
      hb_gt_GetCursorSize( &start, &end )

      if( ( start == 32 ) && ( end == 32 ) )
         rc = SC_NONE;

      else if( ( start == 6 ) && ( end == 7 ) )
         rc = SC_NORMAL;

      else if( ( start == 4 ) && ( end == 7 ) )
         rc = SC_INSERT;

      else if( ( start == 0 ) && ( end == 7 ) )
         rc = SC_SPECIAL1;

      else if( ( start == 0 ) && ( end == 3 ) )
         rc = SC_SPECIAL2;
   }
*/
   return rc;
}

void hb_gt_SetCursorStyle( USHORT uiStyle )
{
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
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst )
{
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc )
{
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   /* TODO: we want to take a screen that is say bright white on blue,
            and change the attributes only for a section of the screen
            to white on black.
   */
}

void hb_gt_DrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   /* TODO: similar to above - see gtwin.c for an idea */
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
}

void hb_gt_DispBegin( void )
{
   /* TODO: Is there a way to change screen buffers?
            ie: can we write somewhere without it going to the screen
            and then update the screen from this buffer at a later time?
            We will initially want to copy the current screen to this buffer.
   */
}

void hb_gt_DispEnd()
{
   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   /* TODO: How to change the size of the screen? */
}

void hb_gt_Replicate( BYTE byChar, ULONG ulLen )
{
   /* TODO: this will write character c nlength times to the screen.
            Note that it is not used yet
            If there is no native function that supports this, it is
            already handled in a generic way by higher level functions.
   */

}

BOOL hb_gt_GetBlink()
{
   /* TODO: under dos, the background 'intensity' bit can be switched
            from intensity to 'blinking'
            does this work under your platform?
   */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   /* TODO: set the bit if it's supported */
}
