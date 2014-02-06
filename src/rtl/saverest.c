/*
 * Harbour Project source code:
 * SaveScreen(), RestScreen() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "hbapi.h"
#include "hbapigt.h"

static void hb_getScreenRange( int * piMin, int * piMax,
                               HB_BOOL fNoCheck, HB_BOOL fVertical )
{
   int iFrom, iTo, iMax;

   if( fVertical )
   {
      iMax  = hb_gtMaxRow();
      iFrom = hb_parni( 1 );
      iTo   = hb_parnidef( 3, iMax );
   }
   else
   {
      iMax  = hb_gtMaxCol();
      iFrom = hb_parni( 2 );
      iTo   = hb_parnidef( 4, iMax );
   }

   if( iFrom < 0 )
      iFrom = 0;
   else if( iFrom > iMax && ! fNoCheck )
      iFrom = iMax;

   if( iTo < 0 )
      iTo = 0;
   else if( iTo > iMax && ! fNoCheck )
      iTo = iMax;

   if( iFrom > iTo )
   {
      *piMin = iTo;
      *piMax = iFrom;
   }
   else
   {
      *piMin = iFrom;
      *piMax = iTo;
   }
}

HB_FUNC( SAVESCREEN )
{
   int iTop, iLeft, iBottom, iRight;
   HB_SIZE nSize;
   void * pBuffer;
   HB_BOOL fNoCheck = HB_FALSE;

   hb_getScreenRange( &iTop, &iBottom, fNoCheck, HB_TRUE );
   hb_getScreenRange( &iLeft, &iRight, fNoCheck, HB_FALSE );

   hb_gtRectSize( iTop, iLeft, iBottom, iRight, &nSize );
   pBuffer = hb_xgrab( nSize + 1 );

   hb_gtSave( iTop, iLeft, iBottom, iRight, pBuffer );
   hb_retclen_buffer( ( char * ) pBuffer, nSize );
}

HB_FUNC( RESTSCREEN )
{
   if( HB_ISCHAR( 5 ) )
   {
      int iTop, iLeft, iBottom, iRight;
      HB_SIZE nSize, nLen;
      void * pBuffer = NULL;
      const char * pBufStr = hb_parc( 5 );
      HB_BOOL fNoCheck = HB_FALSE;

      hb_getScreenRange( &iTop, &iBottom, fNoCheck, HB_TRUE );
      hb_getScreenRange( &iLeft, &iRight, fNoCheck, HB_FALSE );

      nLen = hb_parclen( 5 );
      hb_gtRectSize( iTop, iLeft, iBottom, iRight, &nSize );
      if( nLen < nSize )
      {
         pBuffer = hb_xgrab( nSize );
         memcpy( pBuffer, pBufStr, nLen );
         memset( ( char * ) pBuffer + nLen, 0, nSize - nLen );
         pBufStr = pBuffer;
      }

      hb_gtRest( iTop, iLeft, iBottom, iRight, pBufStr );

      if( pBuffer )
         hb_xfree( pBuffer );
   }
}
