/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on ncurses.
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
 *
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

#include <curses.h>

#include "gtapi.h"
#include "inkey.ch"

static void gt_GetMaxRC(int* r, int* c);
static void gt_GetRC(int* r, int* c);
static void gt_SetRC(int r, int c);

static int iKeyTable[] = {
    KEY_DOWN, K_DOWN,
    KEY_UP, K_UP,
    KEY_LEFT, K_LEFT,
    KEY_RIGHT, K_RIGHT,
    KEY_HOME, K_HOME,
    KEY_END, K_END,
    KEY_BACKSPACE, K_BS,
    KEY_IC, K_INS,
    KEY_DC, K_DEL,
    KEY_NPAGE, K_PGDN,
    KEY_PPAGE, K_PGUP,
    KEY_F(1), K_F1,
    KEY_F(2), K_F2,
    KEY_F(3), K_F3,
    KEY_F(4), K_F4,
    KEY_F(5), K_F5,
    KEY_F(6), K_F6,
    KEY_F(7), K_F7,
    KEY_F(8), K_F8,
    KEY_F(9), K_F9,
    KEY_F(10), K_F10,
    KEY_F(11), K_SH_F1,
    KEY_F(12), K_SH_F2,
    KEY_F(13), K_SH_F3,
    KEY_F(14), K_SH_F4,
    KEY_F(15), K_SH_F5,
    KEY_F(16), K_SH_F6,
    KEY_F(17), K_SH_F7,
    KEY_F(18), K_SH_F8,
    KEY_F(19), K_SH_F9,
    KEY_F(20), K_SH_F10,
    KEY_F(21), K_CTRL_F1,
    KEY_BTAB, K_SH_TAB
     };


void hb_gt_Init( void )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

  initscr();
  if( has_colors() )
  {
    int i;
    start_color();
    for( i = 1; i <= COLOR_PAIRS; i++ )
    {
      init_pair( i, i % COLORS, i / COLORS );
    }
    /* NOTE: color order=
	COLOR_BLACK
	COLOR_RED
	COLOR_GREEN
	COLOR_YELLOW
	COLOR_BLUE
	COLOR_MAGENTA
	COLOR_CYAN
	COLOR_WHITE
    */	
  }
  cbreak();
  noecho();
  nodelay(stdscr, 1);
  keypad( stdscr, TRUE );
}

void hb_gt_Done( void )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Done()"));

  refresh();
  endwin();
}

int hb_gt_ReadKey( void )
{
   int ch;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey()")); 

   ch = getch();
   if (ch == ERR) {
     ch = 0;
   }
   else
   {
       int i;
       for( i=0; i<(sizeof(iKeyTable)/sizeof(int)); i++ )
           if( iKeyTable[ i++ ] == ch )
	       return iKeyTable[ i ];
   }
      
   return ch;
}

BOOL hb_gt_IsColor( void )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

  return has_colors();	/* returns TRUE or FALSE */
}

USHORT hb_gt_GetScreenWidth( void )
{
  int r, c;

  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

  gt_GetMaxRC(&r, &c);
  return c;
}

USHORT hb_gt_GetScreenHeight( void )
{
  int r, c;

  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

  gt_GetMaxRC(&r, &c);
  return r;
}

void hb_gt_SetPos( USHORT uiRow, USHORT uiCol )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hu, %hu)", uiRow, uiCol));

  gt_SetRC(uiRow, uiCol);
}

USHORT hb_gt_Col( void )
{
  int r, c;

  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

  gt_GetRC(&r, &c);
  return c;
}

USHORT hb_gt_Row( void )
{
  int r, c;

  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

  gt_GetRC(&r, &c);
  return r;
}

USHORT hb_gt_GetCursorStyle( void )
{
  USHORT usOldCursor = curs_set( 0 );
  
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));
  curs_set( usOldCursor );
 
  return (usOldCursor ? SC_INSERT : SC_NONE);
}

void hb_gt_SetCursorStyle( USHORT uiStyle )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

  /* TODO: How to set the cursor shape? */
  if( uiStyle == SC_NONE )
      curs_set( 0 );
  else
      curs_set( 1 );
}

void hb_gt_Puts( USHORT uiRow,
		 USHORT uiCol,
		 BYTE byAttr,
		 BYTE * pbyStr,
		 ULONG ulLen )
{
  ULONG i;

  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

  move(uiRow, uiCol);
  attron( COLOR_PAIR( byAttr ) );
  for (i = 0; i < ulLen; ++i) {
    addch(pbyStr[i]);
  }
}

void hb_gt_GetText( USHORT uiTop,
		    USHORT uiLeft,
		    USHORT uiBottom,
		    USHORT uiRight,
		    BYTE * pbyDst )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

  /* TODO */
}

void hb_gt_PutText( USHORT uiTop,
		    USHORT uiLeft,
		    USHORT uiBottom,
		    USHORT uiRight,
		    BYTE * pbySrc )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

  /* TODO */
}

void hb_gt_SetAttribute( USHORT uiTop,
			 USHORT uiLeft,
			 USHORT uiBottom,
			 USHORT uiRight,
			 BYTE byAttr )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

  /* TODO: we want to take a screen that is say bright white on blue,
     and change the attributes only for a section of the screen
     to white on black.
  */
}

void hb_gt_Scroll( USHORT uiTop,
		   USHORT uiLeft,
		   USHORT uiBottom,
		   USHORT uiRight,
		   BYTE byAttr,
		   SHORT iRows,
		   SHORT iCols )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

  /* TODO */
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
  return TRUE;
}

void hb_gt_Replicate( BYTE byChar, ULONG ulLen )
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) byChar, ulLen));

  /* TODO: this will write character c nlength times to the screen.
     Note that it is not used yet
     If there is no native function that supports this, it is
     already handled in a generic way by higher level functions.
  */

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
   if( bBlink )
      attron( A_BLINK );
   else
      attroff( A_BLINK );
}


static void gt_GetMaxRC(int* r, int* c)
{
  int y, x;
  getmaxyx(stdscr, y, x);
  *r = y;
  *c = x;
}

static void gt_GetRC(int* r, int* c)
{
  int y, x;
  getyx(stdscr, y, x);
  *r = y;
  *c = x;
}

static void gt_SetRC(int r, int c)
{
  move(r, c);
  refresh();
}
