/*
 * $Id$
 */

/*
   Harbour Project source code

   mouse.c Support functions for Nanfor Library

   Copyright 2000  Luiz Rafael Culik <Culik@sl.conex.net>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

*/

#include "extend.h"
#include "dos.h"
#include "hbapiitm.h"
#include "hbapigt.h"

HB_FUNC(_MGET_PAGE)
{
   int iPage;
#if defined(HB_OS_DOS)

   {
   
      union REGS regs;
      regs.HB_XREGS.ax=0x1E;
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#else
   {
      iPage=0;
   }
#endif
   {
      hb_retni(iPage);
   }

}


HB_FUNC(_MSET_PAGE)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0x1D;
      regs.HB_XREGS.bx=hb_parni(1);
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}


HB_FUNC(_MGET_MVERSION)
{
   int iMinor;
   int iType;
   int iIRQ;
   int iMajor;

#if defined(HB_OS_DOS)

   {
      union REGS regs;
      
      regs.HB_XREGS.ax = 0x24;
      HB_DOS_INT86( 0x33, &regs, &regs );

      iMinor = regs.h.bl;
      iType = regs.h.ch;
      iIRQ = regs.h.cl;
      iMajor = regs.h.bh;
   }

#else

   {
      iMinor = 0;
      iType = 0;
      iIRQ = 0;
      iMajor = 0;
   }

#endif

   {   
      PHB_ITEM pArray = hb_itemArrayNew( 4 );
      
      PHB_ITEM pMinor = hb_itemPutNI( NULL, iMinor );
      PHB_ITEM pType = hb_itemPutNI( NULL, iType );
      PHB_ITEM pIRQ = hb_itemPutNI( NULL, iIRQ ); 
      PHB_ITEM pMajor = hb_itemPutNI( NULL, iMajor );

      hb_itemArrayPut( pArray, 1, pMinor );
      hb_itemArrayPut( pArray, 2, pType );
      hb_itemArrayPut( pArray, 3, pIRQ );
      hb_itemArrayPut( pArray, 4, pMajor );

      hb_itemReturn( pArray );

      hb_itemRelease( pMajor );
      hb_itemRelease( pIRQ );
      hb_itemRelease( pType );
      hb_itemRelease( pMinor );

      hb_itemRelease( pArray );
   }


}


HB_FUNC(_MGET_HORISPEED)
{
   int iSpeed;

#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0x1B;
      HB_DOS_INT86(0x33,&regs,&regs);
      iSpeed=regs.HB_XREGS.bx;
   }
#else
   {
      iSpeed=0;
   }
#endif
   {
      hb_retni(iSpeed);
   }
}
HB_FUNC(_MGET_VERSPEED)
{
   int iSpeed;
#if defined(HB_OS_DOS)
   {  
      union REGS regs;
      regs.HB_XREGS.ax=0x1B;
      HB_DOS_INT86(0x33,&regs,&regs);
      iSpeed=regs.HB_XREGS.cx;
   }
#else
   {
      iSpeed=0;
   }
#endif
   {
      hb_retni(iSpeed);
   }
}
HB_FUNC(_MGET_DOUBLESPEED)
{
   int iSpeed;
#if defined(HB_OS_DOS)
   {  
      union REGS regs;
      regs.HB_XREGS.ax=0x1B;
      HB_DOS_INT86(0x33,&regs,&regs);
      iSpeed=regs.HB_XREGS.dx;
   }
#else
   {
      iSpeed=0;
   }
#endif
   {
      hb_retni(iSpeed);
   }
}

HB_FUNC(_MSET_SENSITIVE) //nHoriz,nVert,nDouble)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0x1A;
      regs.HB_XREGS.bx=hb_parni(1);
      regs.HB_XREGS.cx=hb_parni(2);
      regs.HB_XREGS.dx=hb_parni(3);
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}
HB_FUNC(_MSE_CONOFF) //nTop*8,nLeft*8,nBotton*8,nRight*8)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0x1A;
      regs.HB_XREGS.cx=hb_parni(2);
      regs.HB_XREGS.dx=hb_parni(1);
      regs.HB_XREGS.si=hb_parni(4);
      regs.HB_XREGS.di=hb_parni(3);
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}

HB_FUNC(_MGET_MICS)
{
int iHori;
int iVert;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0x0B;
      HB_DOS_INT86(0x33,&regs,&regs);
      iHori=regs.HB_XREGS.cx;
      iVert=regs.HB_XREGS.dx;
   }
#else
   {
      iHori=0;
      iVert=0;
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew(2);
      PHB_ITEM pHori = hb_itemPutNI(NULL,iHori);
      PHB_ITEM pVert = hb_itemPutNI(NULL,iVert);


      hb_itemArrayPut( pArray, 1, pHori );
      hb_itemArrayPut( pArray, 2, pVert );
      hb_itemReturn(pArray);

      hb_itemRelease(pArray);
      hb_itemRelease(pHori);
      hb_itemRelease(pVert);

   }
}


HB_FUNC(_M_RESET)
{
   int iMouse;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0;
      HB_DOS_INT86(0x33,&regs,&regs);
      iMouse=regs.HB_XREGS.ax;
   }
#else
   {
      iMouse=0;
   }
#endif
   {
      hb_retl(iMouse);
   }

}
HB_FUNC( _MSE_SHOWCURS)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=1;
      HB_DOS_INT86(0x33,&regs,&regs);
    }
#endif
}

HB_FUNC( _MSE_MHIDECRS)
   {
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=2;
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}

HB_FUNC( _MSE_GETPOS)

{
int iHori;
int iVert;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=3;
      HB_DOS_INT86(0x33,&regs,&regs);
      iHori=regs.HB_XREGS.cx;
      iVert=regs.HB_XREGS.dx;
   }
#else
   {
      iHori=0;
      iVert=0;
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew(2);
      PHB_ITEM pHori = hb_itemPutNI(NULL,iHori);
      PHB_ITEM pVert = hb_itemPutNI(NULL,iVert);


      hb_itemArrayPut( pArray, 1, pHori );
      hb_itemArrayPut( pArray, 2, pVert );
      hb_itemReturn(pArray);

      hb_itemRelease(pArray);
      hb_itemRelease(pHori);
      hb_itemRelease(pVert);

   }

}

HB_FUNC( _M_GETX)
{
   int iRow;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=3;
      HB_DOS_INT86(0x33,&regs,&regs);
      iRow=regs.HB_XREGS.dx;
   }
#else
   {
      iRow=0;
   }
#endif
   {
      hb_retni(iRow);
   }

}

HB_FUNC( _M_GETY)
{
   int iCol ;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=3;
      HB_DOS_INT86(0x33,&regs,&regs);
      iCol=regs.HB_XREGS.cx;
   }
#else
   {
      iCol=0;
   }
#endif
   {
      hb_retni(iCol);
   }

}

HB_FUNC( _M_MSETPOS)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=4;
      regs.HB_XREGS.cx=hb_parni(1);
      regs.HB_XREGS.dx=hb_parni(2);
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}

HB_FUNC( _M_MSETCOORD)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=4;
      regs.HB_XREGS.cx=hb_parni(1);
      regs.HB_XREGS.dx=hb_parni(2);
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif   
}

HB_FUNC( _M_MXLIMIT)
{
   int iMaxRow;
   int iMinRow;

#if defined(HB_OS_DOS)
   {
      union REGS regs;
      iMaxRow=hb_parni(2);
      iMinRow=hb_parni(1);

      regs.HB_XREGS.ax=7;
      regs.HB_XREGS.cx=iMinRow;
      regs.HB_XREGS.dx=iMaxRow;

      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}


HB_FUNC( _M_MYLIMIT)
{
   int iMaxCol;
   int iMinCol;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      iMaxCol=hb_parni(2);
      iMinCol=hb_parni(1);
      regs.HB_XREGS.ax=8;

      regs.HB_XREGS.cx=iMinCol;
      regs.HB_XREGS.dx=iMaxCol;
      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}

HB_FUNC( _M_MBUTPRS)
{
   int inX;
   int inY;
   int inButton ;
   BOOL lStatus;
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=6;
      regs.HB_XREGS.bx=hb_parni(1);
      HB_DOS_INT86(0x33,&regs,&regs);

      inY=regs.HB_XREGS.cx;
      inX=regs.HB_XREGS.dx;
      inButton=regs.HB_XREGS.bx;
      lStatus=regs.HB_XREGS.ax;
   }
#else
   {
      inY=0;
      inX=0;
      inButton=0;
      lStatus=0;
}
#endif
   {
      PHB_ITEM pArray =     hb_itemArrayNew(4);
      PHB_ITEM pY =         hb_itemPutNI(NULL,inY);
      PHB_ITEM pX =         hb_itemPutNI(NULL,inX);
      PHB_ITEM pButton =    hb_itemPutNI(NULL,inButton);
      PHB_ITEM pStatus =    hb_itemPutNI(NULL,lStatus);
      hb_itemArrayPut( pArray, 1, pButton ); /* NOTE: I've changed 1 to 3 */
      hb_itemArrayPut( pArray, 2, pX );
      hb_itemArrayPut( pArray, 3, pY );
      hb_itemArrayPut( pArray, 4, pStatus ); /* NOTE: I've changed 1 to 3 */
      hb_itemReturn(pArray);
   
      hb_itemRelease(pArray);
      hb_itemRelease(pX);
      hb_itemRelease(pY);
      hb_itemRelease(pStatus);
      hb_itemRelease(pButton);
   }   
}

HB_FUNC( _M_MDEFCRS)
{
#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=0x0A;
      regs.HB_XREGS.bx=hb_parni(1);
      regs.HB_XREGS.cx=hb_parni(2);
      regs.HB_XREGS.dx=hb_parni(3);

      HB_DOS_INT86(0x33,&regs,&regs);
   }
#endif
}



HB_FUNC( _M_MGETCOORD)
{
int inX;
int inY;
int inButton;

#if defined(HB_OS_DOS)
   {
      union REGS regs;
      regs.HB_XREGS.ax=3; 
      HB_DOS_INT86(0x33,&regs,&regs); 

      inButton=regs.HB_XREGS.bx;
      inY=regs.HB_XREGS.cx;
      inX=regs.HB_XREGS.dx;

   }

#else

   {
      inX=0;
      inY=0;
      inButton=0;

   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew(3);

      PHB_ITEM pnY =  hb_itemPutNI(NULL,inY);
      PHB_ITEM pnX =    hb_itemPutNI(NULL,inX);
      PHB_ITEM pnButton=    hb_itemPutNI(NULL,inButton);

      hb_itemArrayPut( pArray, 1, pnX );
      hb_itemArrayPut( pArray, 2, pnY );
      hb_itemArrayPut( pArray, 3, pnButton ); 

      hb_itemReturn(pArray);

      hb_itemRelease(pArray);
      hb_itemRelease(pnY);
      hb_itemRelease(pnX);
      hb_itemRelease(pnButton);
   }

}
