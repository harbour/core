/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for OS/2 compilers
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

/*
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#define INCL_VIO
#define INCL_NOPMAPI

#include <string.h>
#include <os2.h>
#include "hbapigt.h"

static char hb_gt_GetCellSize( void );
static void hb_gt_SetCursorSize( char start, char end, int visible );
/*
static void hb_gt_GetCursorSize( char * start, char * end );
*/

void hb_gt_Init( void )
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

BOOL hb_gt_IsColor( void )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   return vi.fbType != 0;        /* 0 = monochrom-compatible mode */
}

USHORT hb_gt_GetScreenWidth( void )
{
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   return vi.col;
}

USHORT hb_gt_GetScreenHeight( void )
{
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   return vi.row;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   VioSetCurPos( ( USHORT ) iRow, ( USHORT ) iCol, 0 );
}

SHORT hb_gt_Row( void )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   VioGetCurPos( &y, &x, 0 );
   return ( SHORT ) y;
}

SHORT hb_gt_Col( void )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   VioGetCurPos( &y, &x, 0 );
   return ( SHORT ) x;
}


void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
/* Chen Kedem <niki@actcom.co.il> */

   BYTE bCell[ 2 ];                            /* character/attribute pair */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRigth, (int) attr, sVert, sHoriz));

   bCell [ 0 ] = ' ';
   bCell [ 1 ] = attr;
   if( ( sVert | sHoriz ) == 0 )               /* both zero, clear region */
      VioScrollUp ( usTop, usLeft, usBottom, usRight, 0xFFFF, bCell, 0 );
   else
   {
      if( sVert > 0 )                        /* scroll up */
         VioScrollUp ( usTop, usLeft, usBottom, usRight, sVert, bCell, 0 );
      else if( sVert < 0 )                   /* scroll down */
         VioScrollDn ( usTop, usLeft, usBottom, usRight, -sVert, bCell, 0 );

      if( sHoriz > 0 )                       /* scroll left */
         VioScrollLf ( usTop, usLeft, usBottom, usRight, sHoriz, bCell, 0 );
      else if( sHoriz < 0 )                  /* scroll right */
         VioScrollRt ( usTop, usLeft, usBottom, usRight, -sHoriz, bCell, 0 );
   }
}

/* QUESTION: not been used, do we need this function ? */
/* Answer: In the dos version, this gets called by hb_gt_GetCursorStyle()
   as that function is written below, we don't need this */

/*
static void hb_gt_GetCursorSize( char * start, char * end )
{
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));

   VioGetCurType( &vi, 0 );
   *start = vi.yStart;
   *end = vi.cEnd;
}
*/

static void hb_gt_SetCursorSize( char start, char end, int visible )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d, %d)", (int) start, (int) end, visible));

   vi.yStart = start;
   vi.cEnd = end;
   vi.cx = 0;
   vi.attr = ( visible ? 0 : -1 );
   VioSetCurType( &vi, 0 );
}

static char hb_gt_GetCellSize()
{
/* Chen Kedem <niki@actcom.co.il> */
   char rc ;
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCellSize()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   rc = ( char )( vi.row ? ( vi.vres / vi.row ) - 1 : 0 );
   return rc;
}

USHORT hb_gt_GetCursorStyle( void )
{
/* Chen Kedem <niki@actcom.co.il> */
   int rc;
   char cellsize;
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   VioGetCurType( &vi, 0 );

   if( vi.attr )
      rc = SC_NONE;
   else
   {
      cellsize = hb_gt_GetCellSize();

      if( vi.yStart == 0 && vi.cEnd == 0 )
         rc = SC_NONE;

      else if( ( vi.yStart == cellsize - 1 || vi.yStart == cellsize - 2 ) && vi.cEnd == cellsize )
         rc = SC_NORMAL;

      else if( vi.yStart == cellsize / 2 && vi.cEnd == cellsize )
         rc = SC_INSERT;

      else if( vi.yStart == 0 && vi.cEnd == cellsize )
         rc = SC_SPECIAL1;

      else if( vi.yStart == 0 && vi.cEnd == cellsize / 2 )
         rc = SC_SPECIAL2;

      else
         rc = SC_NONE;
   }

   return rc;
}

void hb_gt_SetCursorStyle( USHORT style )
{
/* Chen Kedem <niki@actcom.co.il> */
   char cellsize;
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

   cellsize = hb_gt_GetCellSize();
   switch( style )
   {
   case SC_NONE:
      hb_gt_SetCursorSize( 0, 0, 0 );
      break;

   case SC_NORMAL:
      hb_gt_SetCursorSize( cellsize - 1, cellsize, 1 );
      break;

   case SC_INSERT:
      hb_gt_SetCursorSize( cellsize / 2, cellsize, 1 );
      break;

   case SC_SPECIAL1:
      hb_gt_SetCursorSize( 0, cellsize, 1 );
      break;

   case SC_SPECIAL2:
      hb_gt_SetCursorSize( 0, cellsize / 2, 1 );
      break;

   default:
      break;
   }
}

void hb_gt_Puts( USHORT usRow, USHORT usCol, BYTE attr, BYTE * str, ULONG len )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) attr, str, len));

   VioWrtCharStrAtt( ( char * ) str, ( USHORT ) len, usRow, usCol, ( BYTE * ) &attr, 0 );
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *dest )
{
   USHORT width, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRigth, dest));

   width = ( USHORT ) ( ( usRight - usLeft + 1 ) * 2 );
   for( y = usTop; y <= usBottom; y++ )
   {
      VioReadCellStr( dest, &width, y, usLeft, 0 );
      dest += width;
   }
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *srce )
{
   USHORT width, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRigth, srce));

   width = ( USHORT ) ( ( usRight - usLeft + 1 ) * 2 );
   for( y = usTop; y <= usBottom; y++ )
   {
      VioWrtCellStr( srce, width, y, usLeft, 0 );
      srce += width;
   }
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
/* Chen Kedem <niki@actcom.co.il> */
   /*
      TODO: work with DispBegin DispEnd
   */

   USHORT width, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", usTop, usLeft, usBottom, usRigth, (int) attr));

   /*
      assume top level check that coordinate are all valid and fall
      within visible screen, else if width cannot be fit on current line
      it is going to warp to the next line
   */
   width = ( USHORT ) ( usRight - usLeft + 1 );
   for( y = usTop; y <= usBottom; y++ )
      VioWrtNAttr( &attr, width, y, usLeft, 0 );
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

void hb_gt_DispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   VioGetMode( &vi, 0 );        /* fill structure with current settings */
   vi.row = uiRows;
   vi.col = uiCols;
   return ( BOOL ) VioSetMode( &vi, 0 );   /* 0 = Ok, other = Fail */
}

void hb_gt_Replicate( BYTE c, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) c, ulLen));

  /* TODO: this will write character c nlength times to the screen.
           Note that it is not used yet
           If there is no native function that supports this, it is
           already handled in a generic way by higher level functions.
  */

   c = ' ';
   ulLen = 0;
}

BOOL hb_gt_GetBlink()
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* get intensity/blink toggle */
   VioGetState( &vi, 0 );
   return ( vi.fs == 0 );               /* 0 = blink, 1 = intens      */
}

void hb_gt_SetBlink( BOOL bBlink )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* set intensity/blink toggle */
   vi.fs   = ( bBlink ? 0 : 1 );        /* 0 = blink, 1 = intens      */
   VioSetState( &vi, 0 );
}
