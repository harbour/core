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
   int  hb_MouseButton(void);
   int  hb_MouseGetPage(void);
   void hb_MouseSetPage(int iPage)
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



#include <dos.h>
#include "hbdefs.h"
#include "extend.h"
#include "gtapi.h"
#include "mouse.h"
#include "init.h"
HB_INIT_SYMBOLS_BEGIN( Mouse__InitSymbols )
{"MPRESENT",FS_PUBLIC,"HB_PRESENT",0},
{"MSHOW",FS_PUBLIC,"HB_MSHOW",0},
{"MHIDE",FS_PUBLIC,"HB_MHIDE",0},
{"MCOL",FS_PUBLIC,"HB_MCOL",0},
{"MROW",FS_PUBLIC,"HB_MROW",0},
{"MSETPOS",FS_PUBLIC,"HB_MSETPOS",0},
{"MSETCURSOR",FS_PUBLIC,"HB_MSETCURSOR",0},
{"MSETBOUND",FS_PUBLIC,"HB_MSETBOUNDS",0},
{"MPRESSED",FS_PUBLIC,"HB_MBPRESSED",0},
{"MGETPAGE",FS_PUBLIC,"HB_MGETPAGE",0},
{"MSETPAGE",FS_PUBLIC,"HB_MSETPAGE",0},
{"MDEFCURSOR",FS_PUBLIC,"HB_MDEFCURSOR",0},
{"MSETCURSOR",FS_PUBLIC,"HB_MSETCOORDS",0},
{"MSETLANG",FS_PUBLIC,"HB_MSETLANG",0},
{"MGETLANG",FS_PUBLIC,"HB_MGETLANG",0},
{"MVERSION",FS_PUBLIC,"HB_MVERSION",0}
HB_INIT_SYMBOLS_END( Mouse__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup Mouse__InitSymbols
#endif


HARBOUR HB_MPRESENT( void )
{
   hb_retni( hb_MousePresent() );
}

HARBOUR HB_MSHOW( void )
{
   hb_MouseShow();
}

HARBOUR HB_MHIDE( void )
{
   hb_MouseHide();
}

HARBOUR HB_MCOL( void )
{
   hb_retni( hb_MouseCol() );
}

HARBOUR HB_MROW( void )
{
   hb_retni( hb_MouseRow() );
}

HARBOUR HB_MSETPOS( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
         hb_MouseSetPos( hb_parni( 1 ), hb_parni( 2 ) );
}

HARBOUR HB_MSETCURSOR( void )
{
   if( ISLOG( 1 ) )
      hb_retl( hb_MouseSetCursor( hb_parl( 1 ) ) );
}

HARBOUR HB_MSETBOUNDS( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) && ISNUM( 4 ) )
      hb_MouseSetBounds( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}
/*  $DOC$
 *  $FUNCNAME$
 *  HB_MBPRESSED()
 *  $CATEGORY$
 *  Mouse
 *  $ONELINER$
 *    Get the last mouse button number
 *  $SYNTAX$
 *  <var>:=MPRESSED() --> lButton
 *  $ARGUMENTS$
 *
 *  $RETURNS$
 *   The last mouse button pressed
 *  $DESCRIPTION$
 *
 *   This function will return  the last mouse button pressed
 *   1 for left button,2 for the right button
 *   3 for both left and right button, 4 for the middle button
 *   5 for both left and middle button, 6 for both right and middle button
 *   7 for all 3 button
 *
 *  $EXAMPLES$
 *    x:=mPressed()
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */

HARBOUR HB_MBPRESSED(void)
{
   hb_retni( hb_MouseButton());
}

HARBOUR HB_MSETPAGE(void)
{
   if( ISNUM( 1 ))
      hb_MouseSetPage(hb_parni(1));
}

HARBOUR HB_MGETPAGE()
{
   int iPage;
   hb_retni(hb_MouseGetPage());
}

HARBOUR HB_MDEFCURSOR(void)
{
 hb_MouseDefCursor( hb_parni(1),hb_parni(2),hb_parni(3));

}

HARBOUR HB_MSETCOORDS(void)
{
   if( ISNUM(1) &&ISNUM(2) )
      hb_MouseSetCoord(hb_parni(1),hb_parni(2));
}

HARBOUR HB_MSETLANG(void)
{
   if (ISNUM(1))
      hb_MouseSetLanguage(hb_parni(1));
}

HARBOUR HB_MVERSION(void)
{
   hb_retclen(hb_MouseVersion(),4);
}
