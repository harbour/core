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

#include "hbapigt.h"

/* C callable low-level interface */

static BOOL s_bPresent = FALSE;          /* Is there a mouse ? */
static BOOL s_bCursorVisible = FALSE;    /* Is mouse cursor visible ? */
static int  s_iButtons = 0;              /* Mouse buttons */
static int  s_iInitCol = 0;              /* Init mouse pos */
static int  s_iInitRow = 0;              /* Init mouse pos */

void hb_mouse_Init( void )
{
   union REGS regs;

   regs.HB_XREGS.ax = 0;
   HB_DOS_INT86( 0x33, &regs, &regs );
   s_bPresent = regs.HB_XREGS.ax;
   s_iButtons = regs.HB_XREGS.bx;

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

      regs.HB_XREGS.ax = 1;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_bCursorVisible = TRUE;
   }

}

void hb_mouse_Hide( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 2;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_bCursorVisible = FALSE;
   }
}

int hb_mouse_Col( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.HB_XREGS.cx / 8;
   }
   else
      return -1;
}

int hb_mouse_Row( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.HB_XREGS.dx / 8;
   }
   else
      return -1;
}

void hb_mouse_SetPos( int iRow, int iCol )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 4;
      regs.HB_XREGS.cx = iRow * 8;
      regs.HB_XREGS.dx = iCol * 8;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 5;
      regs.HB_XREGS.bx = iButton;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.HB_XREGS.bx ? TRUE : FALSE;
   }
   else
      return FALSE;
}

int hb_mouse_CountButton( void )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      return regs.HB_XREGS.bx;
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

      regs.HB_XREGS.ax = 7;
      regs.HB_XREGS.cx = iLeft;
      regs.HB_XREGS.dx = iRight;
      HB_DOS_INT86( 0x33, &regs, &regs );

      iTop *= 8;
      iBottom *= 8;

      regs.HB_XREGS.ax = 8;
      regs.HB_XREGS.cx = iTop;
      regs.HB_XREGS.dx = iBottom;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   if( s_bPresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 7;
      HB_DOS_INT86( 0x33, &regs, &regs );
      *piLeft = regs.HB_XREGS.cx / 8;
      *piRight = regs.HB_XREGS.dx / 8;

      regs.HB_XREGS.ax = 8;
      HB_DOS_INT86( 0x33, &regs, &regs );
      *piTop = regs.HB_XREGS.cx / 8;
      *piBottom = regs.HB_XREGS.dx / 8;
   }
}

