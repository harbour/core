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
*/

#ifndef HB_MOUSE_H_
#define HB_MOUSE_H_

#define MOUSE_INTERRUPT   0x33

void  hb_MouseInit( void );
void  hb_MouseDone( void );
int   hb_MousePresent( void );
void  hb_MouseShow( void );
void  hb_MouseHide( void );
int   hb_MouseCol( void );
int   hb_MouseRow( void );
void  hb_MouseSetPos( int, int );
int   hb_MouseSetCursor( int );
void  hb_MouseSetBounds( int, int, int, int );
int   hb_MouseButton(void);
int   hb_MouseGetPage(void);
void  hb_MouseSetPage(int );
void  hb_MouseDefCursor(int ,int,int );
void  hb_MouseSetCoord(int , int );
void  hb_MouseSetLanguage(int );
int   hb_MouseGetLanguage(void);
char *hb_MouseVersion(void);
HARBOUR HB_MPRESENT( void );
HARBOUR HB_MSHOW( void );
HARBOUR HB_MHIDE( void );
HARBOUR HB_MCOL( void );
HARBOUR HB_MROW( void );
HARBOUR HB_MSETPOS( void );
HARBOUR HB_MSETCURSOR( void );
HARBOUR HB_MSETBOUNDS( void );
HARBOUR HB_MBPRESSED(void);
HARBOUR HB_MGETPAGE(void);
HARBOUR HB_MSETPAGE(void);
HARBOUR HB_MDEFCURSOR(void);
HARBOUR HB_MSETCOORDS(void) ;
HARBOUR HB_MSETLANG(void);
HARBOUR HB_MGETLANG(void);
HARBOUR HB_MVERSION(void);
#endif /* HB_MOUSE_H_ */
