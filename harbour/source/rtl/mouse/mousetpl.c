/*
   Harbour Project source code

   Harbour mouse support

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


   V 1.0 Luiz Rafael  Initial Release 

   TODO:Everything!
*/

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
}

int hb_MouseGetLanguage(void)
{
     return 0;
}

void hb_MouseSetCoord(int nX, int nY)
{
   nX*=8;
   nY*=8;

}

void hb_MouseDefCursor(int nCurType,int nScrMask,int nCurMask)
{
}

int hb_MouseGetPage()
{
   return 0;
}

void hb_MouseSetPage(int iPage)
{
}

void hb_MouseInit( void )
{
}

int hb_MouseButton()
{
   return 0;
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
}

void hb_MouseHide( void )
{
}

int hb_MouseCol( void )
{
   return( -1 );
}

int hb_MouseRow( void )
{
   return( -1 );
}

void hb_MouseSetPos( int iRow, int iCol )
{
}

int hb_MouseSetCursor( int iVisible )
{
}

void hb_MouseSetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
}

char *hb_MouseVersion(void)
{
}
