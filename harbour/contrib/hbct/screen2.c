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
#include "hbdate.h"

HB_FUNC( SAYDOWN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      const char * szText = hb_parc( 1 );
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = HB_ISNUM( 2 ) ? hb_parnl( 2 ) : 4;

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

         if( ulLen > ( ULONG ) ( iMaxRow - iRow + 1 ) )
            ulLen = ( ULONG ) ( iMaxRow - iRow + 1 );

         hb_gtBeginWrite();
         while( ulLen-- )
         {
            hb_gtPutChar( iRow++, iCol, iColor, 0, ( UCHAR ) *szText++ );
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
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      const char * szText = hb_parc( 1 );
      ULONG ulPos, ul;
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = HB_ISNUM( 2 ) ? hb_parnl( 2 ) : 4;

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

         ulPos = ulLen >> 1;
         ulLen = ulLen & 1;
         if( !ulLen )
         {
            ulLen = 2;
            --ulPos;
         }

         hb_gtBeginWrite();
         do
         {
            for( ul = 0; ul < ulLen && iCol + ( int ) ul <= iMaxCol; ++ul )
               hb_gtPutChar( iRow, iCol + ( int ) ul, iColor, 0, ( UCHAR ) szText[ulPos + ul] );
            ulLen += 2;
            if( lDelay )
            {
               hb_gtEndWrite();
               hb_idleSleep( ( double ) lDelay / 1000 );
               hb_gtBeginWrite();
            }
         }
         while( ulPos-- && iCol-- );
         /* CT3 does not respect iCol in the above condition */
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

HB_FUNC( SAYMOVEIN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      const char * szText = hb_parc( 1 );
      ULONG ulChars, ul;
      int iRow, iCol, iMaxRow, iMaxCol, iNewCol;
      long lDelay;
      BOOL fBack;

      lDelay = HB_ISNUM( 2 ) ? hb_parnl( 2 ) : 4;
      fBack = HB_ISLOG( 5 ) && hb_parl( 5 );

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

         iNewCol = iCol + ( int ) ulLen;
         if( fBack )
            iCol += ulLen - 1;
         else
            szText += ulLen - 1;
         ulChars = 1;

         hb_gtBeginWrite();
         do
         {
            if( fBack )
            {
               if( iCol <= iMaxCol )
               {
                  for( ul = 0; ul < ulChars; ++ul )
                     hb_gtPutChar( iRow, iCol + ( int ) ul, iColor, 0, ( UCHAR ) szText[ul] );
               }
               --iCol;
            }
            else
            {
               for( ul = 0; ul < ulChars; ++ul )
                  hb_gtPutChar( iRow, iCol + ( int ) ul, iColor, 0, ( UCHAR ) szText[ul] );
               --szText;
            }
            if( ( int ) ulChars + iCol <= iMaxCol )
               ++ulChars;

            if( lDelay )
            {
               hb_gtEndWrite();
               hb_idleSleep( ( double ) lDelay / 1000 );
               hb_gtBeginWrite();
            }
         }
         while( --ulLen );
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
   UCHAR ucChar;
   long lDelay;

   lDelay  = hb_parnl( 1 );

   iTop    = hb_parni( 2 );
   iLeft   = hb_parni( 3 );
   iBottom = HB_ISNUM( 4 ) ? hb_parni( 4 ) : iMaxRow;
   iRight  = HB_ISNUM( 5 ) ? hb_parni( 5 ) : iMaxCol;

   if( HB_ISNUM( 6 ) )
      ucChar = ( UCHAR ) hb_parni( 6 );
   else if( HB_ISCHAR( 6 ) )
      ucChar = ( UCHAR ) hb_parc( 6 )[0];
   else
      ucChar = ( UCHAR ) hb_gtGetClearChar();

   if( iTop >= 0 && iLeft >= 0 && iTop <= iBottom && iLeft <= iRight )
   {
      char pszFrame[ 2 ];
      int iColor = hb_gtGetCurrColor();
      double dX, dY, dXX, dYY;

      pszFrame[0] = ( char ) ucChar;
      pszFrame[1] = '\0';

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
   ULONG ulSize, ulCount = ULONG_MAX;

   hb_gtGetPos( &iRow, &iCol );
   if( HB_ISNUM( 1 ) )
      iRow = hb_parni( 1 );
   if( HB_ISNUM( 2 ) )
      iCol = hb_parni( 2 );
   if( HB_ISNUM( 3 ) )
      ulCount = hb_parnl( 3 );
   iMaxRow = hb_gtMaxRow();
   iMaxCol = hb_gtMaxCol();

   if( iRow >= 0 && iRow <= iMaxRow && iCol >= 0 && iCol <= iMaxCol && ulCount )
   {
      ulSize = ( ULONG ) ( iMaxRow - iRow + 1 ) * ( iMaxCol - iCol + 1 );
      if( ulSize > ulCount )
         ulSize = ulCount;
      ulCount = ulSize;
      ulSize <<= 1;
      szText = pBuffer = ( char * ) hb_xgrab( ulSize + 1 );
      do
      {
         iC = iCol;
         do
         {
            int iColor;
            BYTE bAttr;
            USHORT usChar;
            hb_gtGetChar( iRow, iC, &iColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            *szText++ = ( char ) iColor;
         }
         while( --ulCount && ++iC <= iMaxCol );
      }
      while( ulCount && ++iRow <= iMaxRow );

      hb_retclen_buffer( pBuffer, ulSize );
   }
   else
      hb_retc_null();
}

HB_FUNC( STRSCREEN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen & 1 )
      ulLen--;

   if( ulLen )
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
               USHORT usChar = ( UCHAR ) *szText++;
               int iColor = ( UCHAR ) *szText++;
               hb_gtPutChar( iRow, iC, iColor, 0, usChar );
               ulLen -= 2;
            }
            while( ulLen && ++iC <= iMaxCol );
         }
         while( ulLen && ++iRow <= iMaxRow );
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

/*
 * _HB_CTDSPTIME() is helper functions for SHOWTIME()
 */
HB_FUNC( _HB_CTDSPTIME )
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

   if( HB_ISLOG( 3 ) && hb_parl( 3 ) )
      iLen -= 3;

   if( HB_ISLOG( 5 ) && hb_parl( 5 ) )
   {
      int iHour = ( szTime[0] - '0' ) * 10 + ( szTime[1] - '0' );

      if( HB_ISLOG( 6 ) && hb_parl( 6 ) )
         szTime[iLen++] = iHour >= 12 ? 'p' : 'a';
      if( iHour > 12 )
         iHour -= 12;
      else if( iHour == 0 )
         iHour = 12;
      szTime[0] = ( char ) ( iHour / 10 ) + '0';
      szTime[1] = ( char ) ( iHour % 10 ) + '0';
   }

   if( szTime[0] == '0' )
      szTime[0] = ' ';

   hb_gtPutText( iRow, iCol, szTime, iLen, iColor );
}
