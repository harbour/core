/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Mouse Subsystem for DOS
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *                Luiz Rafael Culik <Culik@sl.conex.net>
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

#include "hbapigt.h"

/* C callable low-level interface */

static BOOL s_bPresent = FALSE;          /* Is there a mouse ? */
static int  s_iButtons = 0;              /* Mouse buttons */
static BOOL s_bCursorVisible = FALSE;    /* Is mouse cursor visible ? */
static int  s_iInitCol = 0;              /* Init mouse pos */
static int  s_iInitRow = 0;              /* Init mouse pos */

void hb_mouse_Init( void )
{
   union REGS regs;

   regs.x.ax = 0;
   HB_DOS_INT86( 0x33, &regs, &regs );
   s_bPresent = regs.x.ax;
   s_iButtons = regs.x.bx;

   if( s_bPresent )
   {
      s_iInitCol = hb_mouse_Col();
      s_iInitRow = hb_mouse_Row();
   }
}

void hb_mouse_Exit( void )
{
   hb_mouse_SetPos( s_iInitRow, s_iInitCol );
   hb_mouse_SetBounds( 0, 0, hb_gtMaxCol(), hb_gtMaxRow() );
}

BOOL hb_mouse_IsPresent( void )
{
   return s_bPresent;
}

void hb_mouse_Show( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 1;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_bCursorVisible = TRUE;
   }

}

void hb_mouse_Hide( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 2;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_bCursorVisible = FALSE;
   }
}

int hb_mouse_Col( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.x.cx / 8;
   }
   else
      return -1;
}

int hb_mouse_Row( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.x.dx / 8;
   }
   else
      return -1;
}

void hb_mouse_SetPos( int iRow, int iCol )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 4;
      regs.x.cx = iRow * 8;
      regs.x.dx = iCol * 8;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 5;
      regs.x.bx = iButton;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.x.bx ? TRUE : FALSE;
   }
   else
      return FALSE;
}

int hb_mouse_CountButton( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.x.bx;
   }
   else
      return 0;
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   if( s_bPresent )
   {
      union REGS regs;

      iLeft *= 8;
      iRight *= 8;

      regs.x.ax = 7;
      regs.x.cx = iLeft;
      regs.x.dx = iRight;
      HB_DOS_INT86( 0x33, &regs, &regs );

      iTop *= 8;
      iBottom *= 8;

      regs.x.ax = 8;
      regs.x.cx = iTop;
      regs.x.dx = iBottom;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.x.ax = 7;
      HB_DOS_INT86( 0x33, &regs, &regs );
      *piLeft = regs.x.cx / 8;
      *piRight = regs.x.dx / 8;

      regs.x.ax = 8;
      HB_DOS_INT86( 0x33, &regs, &regs );
      *piTop = regs.x.cx / 8;
      *piBottom = regs.x.dx / 8;
   }
}

