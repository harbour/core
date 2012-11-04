/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video functions:
 *
 * SAYDOWN(), SAYSPREAD(), SAYMOVEIN(), SCREENSTR(), STRSCREEN()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
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
#include "hbdate.h"

HB_FUNC( SAYDOWN )
{
   HB_SIZE nLen = hb_parclen( 1 );

   if( nLen )
   {
      const char * szText = hb_parc( 1 );
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = hb_parnldef( 2, 4 );

      hb_gtGetPos( &iRow, &iCol );
      if( HB_ISNUM( 3 ) )
         iRow = hb_parni( 3 );
      if( HB_ISNUM( 4 ) )
         iCol = hb_parni( 4 );
      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         int iColor = hb_gtGetCurrColor();

         if( nLen > ( HB_SIZE ) ( iMaxRow - iRow + 1 ) )
            nLen = ( HB_SIZE ) ( iMaxRow - iRow + 1 );

         hb_gtBeginWrite();
         while( nLen-- )
         {
            hb_gtPutChar( iRow++, iCol, iColor, 0, ( HB_UCHAR ) *szText++ );
            if( lDelay )
            {
               hb_gtEndWrite();
               hb_idleSleep( ( double ) lDelay / 1000 );
               hb_gtBeginWrite();
            }
         }
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

HB_FUNC( SAYSPREAD )
{
   HB_SIZE nLen = hb_parclen( 1 );

   if( nLen )
   {
      const char * szText = hb_parc( 1 );
      HB_SIZE nPos, ul;
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = hb_parnldef( 2, 4 );

      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();
      hb_gtGetPos( &iRow, &iCol );
      if( HB_ISNUM( 3 ) )
         iRow = hb_parni( 3 );
      else
         hb_gtGetPos( &iRow, &iCol );
      iCol = HB_ISNUM( 4 ) ? hb_parni( 4 ) : ( iMaxCol >> 1 );

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         int iColor = hb_gtGetCurrColor();

         nPos = nLen >> 1;
         nLen = nLen & 1;
         if( ! nLen )
         {
            nLen = 2;
            --nPos;
         }

         hb_gtBeginWrite();
         do
         {
            for( ul = 0; ul < nLen && iCol + ( int ) ul <= iMaxCol; ++ul )
               hb_gtPutChar( iRow, iCol + ( int ) ul, iColor, 0, ( HB_UCHAR ) szText[ nPos + ul ] );
            nLen += 2;
            if( lDelay )
            {
               hb_gtEndWrite();
               hb_idleSleep( ( double ) lDelay / 1000 );
               hb_gtBeginWrite();
            }
         }
         while( nPos-- && iCol-- );
         /* CT3 does not respect iCol in the above condition */
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

HB_FUNC( SAYMOVEIN )
{
   int iLen = ( int ) hb_parclen( 1 );

   if( iLen )
   {
      const char * szText = hb_parc( 1 );
      HB_SIZE nChars, ul;
      int iRow, iCol, iMaxRow, iMaxCol, iNewCol;
      long lDelay;
      HB_BOOL fBack;

      lDelay = hb_parnldef( 2, 4 );
      fBack = hb_parl( 5 );

      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();
      hb_gtGetPos( &iRow, &iCol );
      if( HB_ISNUM( 3 ) )
         iRow = hb_parni( 3 );
      if( HB_ISNUM( 4 ) )
         iCol = hb_parni( 4 );
      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         int iColor = hb_gtGetCurrColor();

         iNewCol = iCol + iLen;
         if( fBack )
            iCol += iLen - 1;
         else
            szText += iLen - 1;
         nChars = 1;

         hb_gtBeginWrite();
         do
         {
            if( fBack )
            {
               if( iCol <= iMaxCol )
               {
                  for( ul = 0; ul < nChars; ++ul )
                     hb_gtPutChar( iRow, iCol + ( int ) ul, iColor, 0, ( HB_UCHAR ) szText[ ul ] );
               }
               --iCol;
            }
            else
            {
               for( ul = 0; ul < nChars; ++ul )
                  hb_gtPutChar( iRow, iCol + ( int ) ul, iColor, 0, ( HB_UCHAR ) szText[ ul ] );
               --szText;
            }
            if( ( int ) nChars + iCol <= iMaxCol )
               ++nChars;

            if( lDelay )
            {
               hb_gtEndWrite();
               hb_idleSleep( ( double ) lDelay / 1000 );
               hb_gtBeginWrite();
            }
         }
         while( --iLen );
         hb_gtSetPos( iRow, iNewCol );
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

HB_FUNC( CLEARSLOW )
{
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();
   int iTop, iLeft, iBottom, iRight;
   HB_UCHAR ucChar;
   long lDelay;

   lDelay  = hb_parnl( 1 );

   iTop    = hb_parni( 2 );
   iLeft   = hb_parni( 3 );
   iBottom = hb_parnidef( 4, iMaxRow );
   iRight  = hb_parnidef( 5, iMaxCol );

   if( HB_ISNUM( 6 ) )
      ucChar = ( HB_UCHAR ) hb_parni( 6 );
   else if( HB_ISCHAR( 6 ) )
      ucChar = ( HB_UCHAR ) hb_parc( 6 )[ 0 ];
   else
      ucChar = ( HB_UCHAR ) hb_gtGetClearChar();

   if( iTop >= 0 && iLeft >= 0 && iTop <= iBottom && iLeft <= iRight )
   {
      char pszFrame[ 2 ];
      int iColor = hb_gtGetCurrColor();
      double dX, dY, dXX, dYY;

      pszFrame[ 0 ] = ( char ) ucChar;
      pszFrame[ 1 ] = '\0';

      dX = iRight - iLeft + 1;
      dY = iBottom - iTop + 1;
      if( dX > dY )
      {
         dY /= dX;
         dX = 1;
      }
      else
      {
         dX /= dY;
         dY = 1;
      }
      dXX = dYY = 0;

      hb_gtBeginWrite();
      for( ;; )
      {
         hb_gtBoxEx( iTop, iLeft, iBottom, iRight, pszFrame, iColor );
         if( lDelay )
         {
            hb_gtEndWrite();
            hb_idleSleep( ( double ) lDelay / 1000 );
            hb_gtBeginWrite();
         }

         if( iTop >= iBottom && iLeft >= iRight )
            break;

         if( iTop < iBottom )
         {
            dYY += dY;
            if( dYY >= 1 )
            {
               iTop++;
               if( iBottom > iTop )
                  iBottom--;
               dYY -= 1;
            }
         }
         if( iLeft < iRight )
         {
            dXX += dX;
            if( dXX >= 1 )
            {
               iLeft++;
               if( iRight > iLeft )
                  iRight--;
            }
         }
      }
      hb_gtEndWrite();
   }
}

HB_FUNC( SCREENSTR )
{
   int iRow, iCol, iMaxRow, iMaxCol, iC;
   char * pBuffer, * szText;
   HB_SIZE nSize, ulCount = HB_SIZE_MAX;

   hb_gtGetPos( &iRow, &iCol );
   if( HB_ISNUM( 1 ) )
      iRow = hb_parni( 1 );
   if( HB_ISNUM( 2 ) )
      iCol = hb_parni( 2 );
   if( HB_ISNUM( 3 ) )
      ulCount = hb_parns( 3 );
   iMaxRow = hb_gtMaxRow();
   iMaxCol = hb_gtMaxCol();

   if( iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol && ulCount )
   {
      nSize = ( HB_SIZE ) ( iMaxRow - iRow + 1 ) * ( iMaxCol - iCol + 1 );
      if( nSize > ulCount )
         nSize = ulCount;
      ulCount = nSize;
      nSize <<= 1;
      szText = pBuffer = ( char * ) hb_xgrab( nSize + 1 );
      do
      {
         iC = iCol;
         do
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;
            hb_gtGetChar( iRow, iC, &iColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            *szText++ = ( char ) iColor;
         }
         while( --ulCount && ++iC <= iMaxCol );
      }
      while( ulCount && ++iRow <= iMaxRow );

      hb_retclen_buffer( pBuffer, nSize );
   }
   else
      hb_retc_null();
}

HB_FUNC( STRSCREEN )
{
   HB_SIZE nLen = hb_parclen( 1 );

   if( nLen & 1 )
      nLen--;

   if( nLen )
   {
      const char * szText = hb_parc( 1 );
      int iRow, iCol, iMaxRow, iMaxCol, iC;

      hb_gtGetPos( &iRow, &iCol );
      if( HB_ISNUM( 2 ) )
         iRow = hb_parni( 2 );
      if( HB_ISNUM( 3 ) )
         iCol = hb_parni( 3 );
      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();

      if( iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol )
      {
         hb_gtBeginWrite();
         do
         {
            iC = iCol;
            do
            {
               HB_USHORT usChar = ( HB_UCHAR ) *szText++;
               int iColor = ( HB_UCHAR ) *szText++;
               hb_gtPutChar( iRow, iC, iColor, 0, usChar );
               nLen -= 2;
            }
            while( nLen && ++iC <= iMaxCol );
         }
         while( nLen && ++iRow <= iMaxRow );
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

/*
 * __HBCT_DSPTIME() is helper functions for SHOWTIME()
 */
HB_FUNC( __HBCT_DSPTIME )
{
   int iRow, iCol;
   int iColor, iLen;
   char szTime[ 10 ];

   iRow = hb_parni( 1 );
   iCol = hb_parni( 2 );
   if( HB_ISNUM( 4 ) )
      iColor = hb_parni( 4 );
   else if( HB_ISCHAR( 4 ) )
   {
      iColor = hb_gtColorToN( hb_parc( 4 ) );
      if( iColor == -1 )
         iColor = 0;
   }
   else
      iColor = hb_gtGetClearColor();

   hb_dateTimeStr( szTime );
   iLen = 8;

   if( hb_parl( 3 ) )
      iLen -= 3;

   if( hb_parl( 5 ) )
   {
      int iHour = ( szTime[ 0 ] - '0' ) * 10 + ( szTime[ 1 ] - '0' );

      if( hb_parl( 6 ) )
         szTime[ iLen++ ] = iHour >= 12 ? 'p' : 'a';
      if( iHour > 12 )
         iHour -= 12;
      else if( iHour == 0 )
         iHour = 12;
      szTime[ 0 ] = ( char ) ( iHour / 10 ) + '0';
      szTime[ 1 ] = ( char ) ( iHour % 10 ) + '0';
   }

   if( szTime[ 0 ] == '0' )
      szTime[ 0 ] = ' ';

   hb_gtPutText( iRow, iCol, szTime, iLen, iColor );
}
