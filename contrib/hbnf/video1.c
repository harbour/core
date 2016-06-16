/* Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
   Copyright 2012 Viktor Szakats (vszakats.net/harbour)
   See COPYING.txt for licensing terms. */

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
   int iTop    = hb_parni( 1 );  /* Defaults to zero on bad type */
   int iLeft   = hb_parni( 2 );  /* Defaults to zero on bad type */
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
