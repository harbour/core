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

/* TOFIX: Change this to something better */
/*#define BORLANDC */
#define MOUSE_INTERRUPT 0x33
#if defined(__DJGPP__) || defined(__BORLANDC__)
   #include <dos.h>
#endif

#include "mouseapi.h"
#include "gtapi.h"

/* C callable low-level interface */

BOOL s_bPresent = FALSE;          /* Is there a mouse ? */
int  s_iButtons = 0;              /* Mouse buttons */
int  s_iCursorVisible = 0;        /* Is mouse cursor visible ? */
int  s_iInitCol = 0;              /* Init mouse pos */
int  s_iInitRow = 0;              /* Init mouse pos */

void hb_mouse_Init( void )
{
   /* TODO: */

#if defined(__DJGPP__) || defined(__BORLANDC__)
      union REGS Mousereg;
      Mousereg.x.ax=0;
      int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
      s_bPresent=Mousereg.x.ax;
      s_iButtons= Mousereg.x.bx;
   if( s_bPresent )
   {
      s_iInitCol = hb_mouse_Col();
      s_iInitRow = hb_mouse_Row();
   }

#endif

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
   /* TODO: */

   if( s_bPresent )
   {
#if defined(__DJGPP__) ||defined(__BORLANDC__)
    union REGS Mousereg;
    Mousereg.x.ax=1;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
    s_iCursorVisible = TRUE;
#endif
   }

}

void hb_mouse_Hide( void )
{
   /* TODO: */

   if( s_bPresent )
   {
#if defined(__DJGPP__)||defined(__BORLANDC__)
    union REGS Mousereg;
    Mousereg.x.ax=2;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
    s_iCursorVisible = FALSE;
#endif
   }
}

int hb_mouse_Col( void )
{
   /* TODO: */

   if( s_bPresent )
   {

      int iCol;
#if defined(__DJGPP__)||defined(__BORLANDC__)
    union REGS Mousereg;
    Mousereg.x.ax=3;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
    iCol=Mousereg.x.cx;
    return iCol / 8;
#else
      return 0;
#endif
   }

   return -1;
}

int hb_mouse_Row( void )
{
   if( s_bPresent )
   {
      int iRow;
#if defined(__DJGPP__)||defined(__BORLANDC__)
    union REGS Mousereg;
    Mousereg.x.ax=3;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
    iRow=Mousereg.x.dx;
#else
      return 0;
#endif
   }

   return -1;
}

void hb_mouse_SetPos( int iRow, int iCol )
{
   /* TODO: */

   if( s_bPresent )
   {
    union REGS Mousereg;
    Mousereg.x.ax=4;
    Mousereg.x.cx=iRow*8;
    Mousereg.x.dx=iCol*8;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);

   }
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   /* TODO: */

   if( s_bPresent )
   {

      int iReturn = 0;
#if defined(__DJGPP__) ||defined(__BORLANDC__)

    union REGS Mousereg;
    Mousereg.x.ax=5;
    Mousereg.x.bx=iButton;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
    iReturn=Mousereg.x.bx;
    return ( iReturn);
#else
      return FALSE;
#endif
   }

   return FALSE;
}

int hb_mouse_CountButton( void )
{
   /* TODO: */

   int iButton = 0;

   if( s_bPresent )
   {
#if defined(__DJGPP__) || defined(__BORLANDC__)

    union REGS Mousereg;
    Mousereg.x.ax=3;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);
    iButton=Mousereg.x.bx;
#endif
   }

   return iButton;
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   /* TODO: */

   if( s_bPresent )
   {
#if defined(__DJGPP__) ||defined(__BORLANDC__)
    union REGS Mousereg;

      iLeft *= 8;
      iRight *= 8;
    Mousereg.x.ax=7;
    Mousereg.x.cx=iLeft;
    Mousereg.x.dx=iRight;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);

    iTop *= 8;
    iBottom *= 8;

    Mousereg.x.ax=8;
    Mousereg.x.cx=iTop;
    Mousereg.x.dx=iBottom;
    int86(MOUSE_INTERRUPT,&Mousereg,&Mousereg);


#endif
   }
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   /* TODO: */

   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

