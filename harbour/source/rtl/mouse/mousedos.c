/*
   Harbour Project source code

   Harbour mouse support

   Copyright(C) 1999 by Jose Lalin.
   http://www.Harbour-Project.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   You can contact me at: dezac@corevia.com

   The Follow Functions or Copyright(C) 1999 Luiz Rafael Culik (culik@sl.conex.net)
   int   hb_MouseButton(void);
   int   hb_MouseGetPage(void);
   void   hb_MouseSetPage(int iPage)
   void hb_MouseDefCursor(int nCurType,nScrMask,nCurMask)
   void hb_MouseSetCoord(int nX, int nY)
   void hb_MouseSetLanguage(int nLang)
   char hb_MouseVersion(void)
   HARBOUR HB_MBPRESSED(void)
   HARBOUR HB_MGETPAGE(void)
   HARBOUR HB_MSETPAGE(void)
   HARBOUR HB_MDEFCURSOR(void)
   HARBOUR HB_MSETCOORDS(void)
   HARBOUR HB_MSETLANG(void);
   HARBOUR HB_MGETLANG(void)
   HARBOUR HB_MVERSION(void)
   V 1.11 Luiz Rafael Fixed Another bug on  hb_MouseVersion() . Now it returning the Correct Value
   V 1.1 Luiz Rafael  Fixed bug on  hb_MouseVersion() and hb_MouseSetCoord()
   V 1.0 Luiz Rafael  Initial Release 
*/

#pragma inline



#include "hbdefs.h"
#include "extend.h"
#include "gtapi.h"
#include "mouse.h"
#include "init.h"
#include "string.h"
#include "stdlib.h"
#include "dos.h"
#include "stdio.h"
/* Is there a mouse ? */
int iMouse = 0;

/* Mouse buttons */
int iButtons = 0;

/* Is mouse cursor visible ? */
int iCursorVisible = 0;

/* Init mouse pos */
int iInitCol = 0;
int iInitRow = 0;


void hb_MouseSetLanguage(int nLang)
{
   asm
     {
     mov ax,34
     mov bx,nLang
     int MOUSE_INTERRUPT
     }
}

int hb_MouseGetLanguage(void)
{
   int nLang;

   asm
     {
     mov ax,35
     int MOUSE_INTERRUPT
     mov nLang,bx

     }
     return nLang;
}

void hb_MouseSetCoord(int nX, int nY)
{
   union REGS regs;

   nX*=8;
   nY*=8;

   regs.x.ax=4 ;
   regs.x.cx=nY ;
   regs.x.dx=nX;
   int86(MOUSE_INTERRUPT,&regs,&regs);
/*
   asm
      {
      mox ax,4
      mov cx,nY
      mov dx,nX
      int MOUSE_INTERRUPT
      }
      */
}
void hb_MouseDefCursor(int nCurType,int nScrMask,int nCurMask)
{
   asm
      {
      mov ax,10
      mov bx,nCurType
      mov cx,nScrMask
      mov dx,nCurMask
      int MOUSE_INTERRUPT
      }
}

int hb_MouseGetPage()
{
   int iPage;
   asm
      {
      mov ax,30
      int MOUSE_INTERRUPT
      mov iPage,bx
      }
   return iPage;
}

void hb_MouseSetPage(int iPage)
{
   asm
      {
      mov ax,29
      mov bx,iPage
      int MOUSE_INTERRUPT
      }

}

void hb_MouseInit( void )
{
   asm
   {
      xor ax, ax
      int MOUSE_INTERRUPT
      mov iMouse, ax
      mov iButtons, bx
   }

   if( iMouse )
   {
      iInitCol  = hb_MouseCol();
      iInitRow  = hb_MouseRow();
   }
}

int hb_MouseButton()
{
   int iButton;
   if (iMouse)
   {
      asm
         {
            mov ax,3
            int MOUSE_INTERRUPT
            mov iButton,bx
         }

   }
             return iButton;
}

void hb_MouseDone( void )
{
  hb_MouseSetPos( iInitRow, iInitCol );
  hb_MouseSetBounds( 0, 0, hb_gtMaxCol(), hb_gtMaxRow() );
}

int hb_MousePresent( void )
{
   return( iMouse );
}

void hb_MouseShow( void )
{
   if( iMouse )
   {
      asm
      {
         mov ax, 1
         int MOUSE_INTERRUPT
      }
      iCursorVisible = TRUE;
   }
}

void hb_MouseHide( void )
{
   if( iMouse )
   {
      asm
      {
         mov ax, 2
         int MOUSE_INTERRUPT
      }
      iCursorVisible = FALSE;
   }
}

int hb_MouseCol( void )
{
   int iCol;

   if( iMouse )
   {
      asm
      {
         mov ax, 3
         int MOUSE_INTERRUPT
         mov iCol, cx
      }
      return( iCol / 8 );
   }
   return( -1 );
}

int hb_MouseRow( void )
{
   int iRow;

   if( iMouse )
   {
      asm
      {
         mov ax, 3
         int MOUSE_INTERRUPT
         mov iRow, dx
      }
      return( iRow / 8 );
   }
   return( -1 );
}

void hb_MouseSetPos( int iRow, int iCol )
{
   if( iMouse )
   {
      iRow *= 8;
      iCol *= 8;

      asm
      {
         mov ax, 4
         mov cx, iRow
         mov dx, iCol
         int MOUSE_INTERRUPT
      }
   }
}

int hb_MouseSetCursor( int iVisible )
{
   if( iVisible )
      hb_MouseShow();
   else
      hb_MouseHide();

   return( iCursorVisible );
}

void hb_MouseSetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   if( iMouse )
   {
      iLeft *= 8;
      iRight *= 8;

      asm
      {
         mov ax, 7
         mov cx, iLeft
         mov dx, iRight
         int MOUSE_INTERRUPT
      }

      iTop *= 8;
      iBottom *= 8;

      asm
      {
         mov ax, 8
         mov cx, iTop
         mov dx, iBottom
         int MOUSE_INTERRUPT
      }
   }
}

char *hb_MouseVersion(void)
{
   union REGS regs;
   char Major[4],Minor[4], Version[10];
   char *cReturn;
   int nMajor,nMinor;

   regs.x.ax=0x024;
   int86(MOUSE_INTERRUPT,&regs,&regs);
   nMajor=regs.h.bh;
   nMinor=regs.h.bl; 
   itoa(nMajor,Major,10);
   itoa(nMinor,Minor,10);
   strcpy(Version,Major);
   strcat(Version,".");
   strcat(Version,Minor);
   strcpy(cReturn,Version);

   return cReturn;

}
