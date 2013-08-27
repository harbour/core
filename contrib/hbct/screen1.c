/*
 * Harbour Project source code:
 *   CT3 video functions:
 *
 * ScreenAttr(), ScreenMix(), SayScreen(),
 * ClearWin(), InvertWin(), UnTextWin(), CharWin(), ColorWin(), ColorRepl()
 *
 *   and Harbour extension:
 *
 * ScreenText()
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

#include "hbdefs.h"
#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( SCREENATTR )
{
   int iRow, iCol;
   int iColor;
   HB_BYTE bAttr;
   HB_USHORT usChar;

   hb_gtGetPos( &iRow, &iCol );
   if( HB_ISNUM( 1 ) )
      iRow = hb_parni( 1 );
   if( HB_ISNUM( 2 ) )
      iCol = hb_parni( 2 );

   if( hb_gtGetChar( iRow, iCol, &iColor, &bAttr, &usChar ) != HB_SUCCESS )
      iColor = 0;

   hb_retni( iColor );
}

HB_FUNC( SCREENMIX )
{
   HB_SIZE nLen = hb_parclen( 1 );

   if( nLen )
   {
      const char * szText = hb_parc( 1 );
      const char * szAttr;
      HB_SIZE nAttr = hb_parclen( 2 ), ul = 0;
      int iRow, iCol, i;

      if( nAttr == 0 )
      {
         szAttr = " ";
         nAttr = 1;
      }
      else
         szAttr = hb_parc( 2 );

      hb_gtGetPos( &iRow, &iCol );
      if( HB_ISNUM( 3 ) )
         iRow = hb_parni( 3 );
      if( HB_ISNUM( 4 ) )
         iCol = hb_parni( 4 );

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= hb_gtMaxRow() && iCol <= hb_gtMaxCol() )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;
         HB_WCHAR wc;
         PHB_CODEPAGE cdp = hb_gtHostCP();
         HB_SIZE nIndex = 0;

         hb_gtBeginWrite();
         i = iCol;
         for( ;; )
         {
            if( hb_gtGetChar( iRow, i, &iColor, &bAttr, &usChar ) != HB_SUCCESS )
            {
               if( ++iRow > hb_gtMaxRow() )
                  break;
               i = iCol;
            }
            else if( HB_CDPCHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
               hb_gtPutChar( iRow, i++, ( HB_UCHAR ) szAttr[ ul ], 0, wc );
            else
               break;
            if( ++ul == nAttr )
               ul = 0;
         }
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

HB_FUNC( SAYSCREEN )
{
   HB_SIZE nLen = hb_parclen( 1 );

   if( nLen )
   {
      const char * szText = hb_parc( 1 );
      int iRow, iCol, i;

      hb_gtGetPos( &iRow, &iCol );
      if( HB_ISNUM( 2 ) )
         iRow = hb_parni( 2 );
      if( HB_ISNUM( 3 ) )
         iCol = hb_parni( 3 );

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= hb_gtMaxRow() && iCol <= hb_gtMaxCol() )
      {
         PHB_CODEPAGE cdp = hb_gtHostCP();
         HB_SIZE nIndex = 0;

         hb_gtBeginWrite();
         i = iCol;
         for( ;; )
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;
            HB_WCHAR wc;
            if( hb_gtGetChar( iRow, i, &iColor, &bAttr, &usChar ) != HB_SUCCESS )
            {
               if( ++iRow > hb_gtMaxRow() )
                  break;
               i = iCol;
            }
            else if( HB_CDPCHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
               hb_gtPutChar( iRow, i++, iColor, bAttr, wc );
            else
               break;
         }
         hb_gtEndWrite();
      }
   }

   hb_retc_null();
}

static HB_BOOL hb_ctGetWinCord( int * piTop, int * piLeft,
                                int * piBottom, int * piRight )
{
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();

   hb_gtGetPosEx( piTop, piLeft );

   if( HB_ISNUM( 1 ) )
      *piTop = hb_parni( 1 );
   if( HB_ISNUM( 2 ) )
      *piLeft   = hb_parni( 2 );
   if( HB_ISNUM( 3 ) )
   {
      *piBottom = hb_parni( 3 );
      if( *piBottom > iMaxRow )
         *piBottom = iMaxRow;
   }
   else
      *piBottom = iMaxRow;
   if( HB_ISNUM( 4 ) )
   {
      *piRight = hb_parni( 4 );
      if( *piRight > iMaxCol )
         *piRight = iMaxCol;
   }
   else
      *piRight = iMaxCol;

   return *piTop >= 0 && *piLeft >= 0 &&
          *piTop <= *piBottom && *piLeft <= *piRight;
}

static int hb_ctGetClearChar( int iParam )
{
   int iChar;

   if( HB_ISNUM( iParam ) )
      iChar = hb_parni( iParam );
   else if( HB_ISCHAR( iParam ) )
      iChar = ( HB_UCHAR ) hb_parc( iParam )[ 0 ];
   else
      iChar = ( int ) hb_gtGetClearChar();

   return iChar;
}

static int hb_ctGetClearColor( int iParam )
{
   int iColor;

   if( HB_ISNUM( iParam ) )
      iColor = hb_parni( iParam );
   else if( HB_ISCHAR( iParam ) )
   {
      iColor = hb_gtColorToN( hb_parc( iParam ) );
      if( iColor == -1 )
         iColor = 0;
   }
   else
      iColor = hb_gtGetClearColor();

   return iColor;
}

HB_FUNC( CLEARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      int iColor, iChar;

      iColor = hb_ctGetClearColor( 5 );
      iChar = hb_ctGetClearChar( 6 );

      hb_gtScrollEx( iTop, iLeft, iBottom, iRight, iColor, iChar, 0, 0 );
   }

   hb_retc_null();
}

HB_FUNC( INVERTWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;

            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            iColor = ( iColor & 0x88 ) |
                     ( ( iColor & 0x07 ) << 4 ) |
                     ( ( iColor >> 4 ) & 0x07 );
            hb_gtPutChar( iTop, iCol, iColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc_null();
}

HB_FUNC( UNTEXTWIN )
{
   int iTop, iLeft, iBottom, iRight;
   HB_USHORT usRepl, usInit, usEnd;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      usRepl = ( HB_USHORT ) hb_ctGetClearChar( 5 );

      if( HB_ISNUM( 6 ) )
         usInit = ( HB_USHORT ) hb_parni( 6 );
      else if( hb_parclen( 6 ) > 0 )
         usInit = ( HB_UCHAR ) hb_parc( 6 )[ 0 ];
      else
         usInit = 176;

      if( HB_ISNUM( 7 ) )
         usEnd = ( HB_USHORT ) hb_parni( 7 );
      else if( hb_parclen( 7 ) > 0 )
         usEnd = ( HB_UCHAR ) hb_parc( 7 )[ 0 ];
      else
         usEnd = 223;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;

            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            if( usInit <= usEnd ? ( usChar < usInit || usChar > usEnd ) :
                                  ( usChar > usEnd && usChar < usInit ) )
               hb_gtPutChar( iTop, iCol, iColor, bAttr, usRepl );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc_null();
}

HB_FUNC( CHARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      HB_USHORT usNewChar, usOldChar = 0;
      HB_BOOL fAll = HB_FALSE;

      usNewChar = ( HB_USHORT ) hb_ctGetClearChar( 5 );

      if( HB_ISNUM( 6 ) )
         usOldChar = ( HB_USHORT ) hb_parni( 6 );
      else if( hb_parclen( 6 ) > 0 )
         usOldChar = ( HB_UCHAR ) hb_parc( 6 )[ 0 ];
      else
         fAll = HB_TRUE;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;

            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            if( fAll || usChar == usOldChar )
               hb_gtPutChar( iTop, iCol, iColor, bAttr, usNewChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc_null();
}

HB_FUNC( COLORWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      int iNewColor, iOldColor = 0;
      HB_BOOL fAll = HB_FALSE;

      iNewColor = hb_ctGetClearColor( 5 );

      if( HB_ISNUM( 6 ) || HB_ISCHAR( 6 ) )
         iOldColor = hb_ctGetClearColor( 6 );
      else
         fAll = HB_TRUE;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;

            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            if( fAll || iColor == iOldColor )
               hb_gtPutChar( iTop, iCol, iNewColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc_null();
}

HB_FUNC( SCREENTEXT ) /* HB_EXTENSION */
{
   int iTop, iLeft, iBottom, iRight;
   char * pBuffer, * szText;
   HB_SIZE nSize;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      nSize = ( HB_SIZE ) ( iBottom - iTop + 1 ) * ( iRight - iLeft + 1 );
      szText = pBuffer = ( char * ) hb_xgrab( nSize + 1 );
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int iColor;
            HB_BYTE bAttr;
            HB_USHORT usChar;
            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            ++iCol;
         }
         ++iTop;
      }
      hb_retclen_buffer( pBuffer, nSize );
   }
   else
      hb_retc_null();
}

HB_FUNC( COLORREPL )
{
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();
   int iRow = 0, iCol;
   int iNewColor, iOldColor = 0;
   HB_BOOL fAll = HB_FALSE;

   iNewColor = hb_ctGetClearColor( 1 );

   if( HB_ISNUM( 2 ) || HB_ISCHAR( 2 ) )
      iOldColor = hb_ctGetClearColor( 2 );
   else
      fAll = HB_TRUE;

   hb_gtBeginWrite();
   while( iRow <= iMaxRow )
   {
      iCol = 0;
      while( iCol <= iMaxCol )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         hb_gtGetChar( iRow, iCol, &iColor, &bAttr, &usChar );
         if( fAll || iColor == iOldColor )
            hb_gtPutChar( iRow, iCol, iNewColor, bAttr, usChar );
         ++iCol;
      }
      ++iRow;
   }
   hb_gtEndWrite();

   hb_retc_null();
}
