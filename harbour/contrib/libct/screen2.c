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
#include "hbgtcore.h"
#include "hbdate.h"

HB_FUNC( SAYDOWN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      UCHAR * szText = ( UCHAR * ) hb_parc( 1 );
      SHORT sRow, sCol;
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = ISNUM( 2 ) ? hb_parnl( 2 ) : 4;

      hb_gtGetPos( &sRow, &sCol );
      iRow = ISNUM( 3 ) ? hb_parni( 3 ) : ( int ) sRow;
      iCol = ISNUM( 4 ) ? hb_parni( 4 ) : ( int ) sCol;
      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         BYTE bColor = hb_gt_GetColor();

         if( ulLen > ( ULONG ) ( iMaxRow - iRow + 1 ) )
            ulLen = ( ULONG ) ( iMaxRow - iRow + 1 );

         hb_gtBeginWrite();
         while( ulLen-- )
         {
            hb_gtPutChar( iRow++, iCol, bColor, 0, *szText++ );
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

   hb_retc( NULL );
}

HB_FUNC( SAYSPREAD )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      UCHAR * szText = ( UCHAR * ) hb_parc( 1 );
      ULONG ulPos, ul;
      SHORT sRow, sCol;
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;

      lDelay = ISNUM( 2 ) ? hb_parnl( 2 ) : 4;

      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();
      hb_gtGetPos( &sRow, &sCol );
      iRow = ISNUM( 3 ) ? hb_parni( 3 ) : ( int ) sRow;
      iCol = ISNUM( 4 ) ? hb_parni( 4 ) : ( iMaxCol >> 1 );

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         BYTE bColor = hb_gt_GetColor();

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
               hb_gtPutChar( iRow, iCol + ul, bColor, 0, szText[ulPos + ul] );
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

   hb_retc( NULL );
}

HB_FUNC( SAYMOVEIN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      UCHAR * szText = ( UCHAR * ) hb_parc( 1 );
      ULONG ulChars, ul;
      SHORT sRow, sCol;
      int iRow, iCol, iMaxRow, iMaxCol;
      long lDelay;
      BOOL fBack;

      lDelay = ISNUM( 2 ) ? hb_parnl( 2 ) : 4;
      fBack = ISLOG( 5 ) && hb_parl( 5 );

      iMaxRow = hb_gtMaxRow();
      iMaxCol = hb_gtMaxCol();
      hb_gtGetPos( &sRow, &sCol );
      iRow = ISNUM( 3 ) ? hb_parni( 3 ) : ( int ) sRow;
      iCol = ISNUM( 4 ) ? hb_parni( 4 ) : ( int ) sCol;

      if( iRow >= 0 && iCol >= 0 && iRow <= iMaxRow && iCol <= iMaxCol )
      {
         BYTE bColor = hb_gt_GetColor();

         sRow = iRow;
         sCol = iCol + ulLen;
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
                     hb_gtPutChar( iRow, iCol + ul, bColor, 0, szText[ul] );
               }
               --iCol;
            }
            else
            {
               for( ul = 0; ul < ulChars; ++ul )
                  hb_gtPutChar( iRow, iCol + ul, bColor, 0, szText[ul] );
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
         hb_gtSetPos( sRow, sCol );
         hb_gtEndWrite();
      }
   }

   hb_retc( NULL );
}

HB_FUNC( SCREENSTR )
{
   SHORT sRow, sCol, sMaxRow, sMaxCol, sC;
   char * pBuffer, * szText;
   ULONG ulSize, ulCount = ULONG_MAX;

   hb_gtGetPos( &sRow, &sCol );
   if( ISNUM( 1 ) )
      sRow = ( SHORT ) hb_parni( 1 );
   if( ISNUM( 2 ) )
      sCol = ( SHORT ) hb_parni( 2 );
   if( ISNUM( 3 ) )
      ulCount = hb_parnl( 3 );
   sMaxRow = ( SHORT ) hb_gtMaxRow();
   sMaxCol = ( SHORT ) hb_gtMaxCol();

   if( sRow >= 0 && sRow <= sMaxRow && sCol >= 0 && sCol <= sMaxCol && ulCount )
   {
      ulSize = ( ULONG ) ( sMaxRow - sRow + 1 ) * ( sMaxCol - sCol + 1 );
      if( ulSize > ulCount )
         ulSize = ulCount;
      ulCount = ulSize;
      ulSize <<= 1;
      szText = pBuffer = ( char * ) hb_xgrab( ulSize + 1 );
      do
      {
         sC = sCol;
         do
         {
            BYTE bColor, bAttr;
            USHORT usChar;
            hb_gtGetChar( sRow, sC, &bColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            *szText++ = ( char ) bColor;
         }
         while( ++sC <= sMaxCol && --ulCount );
      }
      while( ++sRow <= sMaxRow && ulCount );

      hb_retclen_buffer( pBuffer, ulSize );
   }
   else
      hb_retc( NULL );
}

HB_FUNC( STRSCREEN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen & 1 )
      ulLen--;

   if( ulLen )
   {
      UCHAR * szText = ( UCHAR * ) hb_parc( 1 );
      SHORT sRow, sCol, sMaxRow, sMaxCol, sC;

      hb_gtGetPos( &sRow, &sCol );
      if( ISNUM( 2 ) )
         sRow = ( SHORT ) hb_parni( 2 );
      if( ISNUM( 3 ) )
         sCol = ( SHORT ) hb_parni( 3 );
      sMaxRow = ( SHORT ) hb_gtMaxRow();
      sMaxCol = ( SHORT ) hb_gtMaxCol();

      if( sRow >= 0 && sRow <= sMaxRow && sCol >= 0 && sCol <= sMaxCol )
      {
         hb_gtBeginWrite();
         do
         {
            sC = sCol;
            do
            {
               USHORT usChar = *szText++;
               BYTE bColor = *szText++;
               hb_gtPutChar( sRow, sC, bColor, 0, usChar );
               ulLen -= 2;
            }
            while( ulLen && ++sC <= sMaxCol );
         }
         while( ulLen && ++sRow <= sMaxRow );
         hb_gtEndWrite();
      }
   }

   hb_retc( NULL );
}

/*
 * _HB_CTDSPTIME() is helper functions for SHOWTIME()
 */
HB_FUNC( _HB_CTDSPTIME )
{
   SHORT sRow, sCol;
   int iColor, iLen, i;
   char szTime[ 10 ];

   sRow = ( SHORT ) hb_parni( 1 );
   sCol = ( SHORT ) hb_parni( 2 );
   if( ISNUM( 4 ) )
      iColor = hb_parni( 4 );
   else if( ISCHAR( 4 ) )
      iColor = hb_gtColorToN( hb_parc( 4 ) );
   else
      iColor = hb_gt_GetClearColor();

   hb_dateTimeStr( szTime );
   iLen = 8;

   if( ISLOG( 3 ) && hb_parl( 3 ) )
      iLen -= 3;

   if( ISLOG( 5 ) && hb_parl( 5 ) )
   {
      int iHour = ( szTime[0] - '0' ) * 10 + ( szTime[1] - '0' );

      if( ISLOG( 6 ) && hb_parl( 6 ) )
         szTime[iLen++] = iHour >= 12 ? 'p' : 'a';
      if( iHour > 12 )
         iHour -= 12;
      else if( iHour == 0 )
         iHour = 12;
      szTime[0] = ( iHour / 10 ) + '0';
      szTime[1] = ( iHour % 10 ) + '0';
   }

   if( szTime[0] == '0' )
      szTime[0] = ' ';

   for( i = 0; i < iLen; ++sCol, ++i )
      hb_gt_PutScrChar( sRow, sCol, iColor, 0, szTime[i] );
   hb_gt_Flush();
}
