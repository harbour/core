/*
 * Harbour Project source code:
 *   NF functions: ft_SetAttr(), ft_RevAttr(), ft_RevChr()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_SETATTR )
{
   hb_gtSetAttribute( hb_parni( 1 ),
                      hb_parni( 2 ),
                      hb_parni( 3 ),
                      hb_parni( 4 ),
                      hb_parni( 5 ) );
}

HB_FUNC( FT_REVATTR )
{
   int iTop    = hb_parni( 1 );   /* Defaults to zero on bad type */
   int iLeft   = hb_parni( 2 );   /* Defaults to zero on bad type */
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();
   int iBottom = hb_parnidef( 3, iMaxRow );
   int iRight  = hb_parnidef( 4, iMaxCol );

   if( iTop < 0 )
      iTop = 0;
   if( iLeft < 0 )
      iLeft = 0;
   if( iBottom > iMaxRow )
      iBottom = iMaxRow;
   if( iRight > iMaxCol )
      iRight = iMaxCol;

   if( iTop <= iBottom && iLeft <= iRight )
   {
      hb_gtDispBegin();

      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int       iColor;
            HB_BYTE   bAttr;
            HB_USHORT usChar;
            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            iColor = ( iColor << 4 ) | ( iColor >> 4 );
            hb_gtPutChar( iTop, iCol, iColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }

      hb_gtDispEnd();
   }
}

HB_FUNC( FT_REVCHR )
{
   int iRow = hb_parni( 1 );
   int iCol = hb_parni( 2 );

   int       iColor;
   HB_BYTE   bAttr;
   HB_USHORT usChar;

   hb_gtGetChar( iRow, iCol, &iColor, &bAttr, &usChar );
   iColor = ( iColor << 4 ) | ( iColor >> 4 );
   hb_gtPutChar( iRow, iCol, iColor, bAttr, usChar );
}
