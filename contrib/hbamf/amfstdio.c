/*
 * Harbour Project source code:
 * Reading AMFIO data from standard input pipe
 *
 * Copyright 2011 Ilina Stoilkovska <anili100/at/gmail.com>
 * Copyright 2012 Aleksander Czajczynski <hb/at/fki.pl>
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
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbapifs.h"

#define SINGLEBUF  32768
#define MAXLEN     ( 16 * 1024 * 1024 )

static int s_nCount = 0;

static void countCheck( int n )
{
   /* yes, this is flow-control */

   s_nCount += n;

   while( s_nCount >= SINGLEBUF )
   {
      hb_conOutStd( "\0\0\0\0", 4 );
      s_nCount -= SINGLEBUF;
   }
}

HB_FUNC( AMFSTDIO_READ )
{
   char *     pszStrIn     = ( char * ) hb_xgrab( SINGLEBUF );
   char *     pszLenPrefix = ( char * ) hb_xgrab( 5 );
   char *     pszBuf;    /* = ( char * ) hb_xgrab( SINGLEBUF ); */
   char *     pszTmp = pszLenPrefix;
   HB_USHORT  nBytes;
   int        nTotal = 0;
   int        nLen;
   int        nToRead;
   HB_FHANDLE hStdIn = hb_fsGetOsHandle( HB_STDIN_HANDLE );

   while( nTotal < 4 )
   {
      nToRead = ( s_nCount + 4 - nTotal > SINGLEBUF ? SINGLEBUF - s_nCount : 4 - nTotal );
      nBytes  = hb_fsRead( hStdIn, pszStrIn, ( HB_USHORT ) nToRead );

      countCheck( nBytes );

      memcpy( pszTmp, pszStrIn, nBytes );
      nTotal += nBytes;
      pszTmp  = pszLenPrefix + nTotal;
   }

   pszLenPrefix[ 4 ] = '\0';
   nLen = HB_GET_LE_UINT32( pszLenPrefix );

   if( nLen >= MAXLEN )
   {
      hb_ret();
      return;
   }

   nTotal = 0;
   pszBuf = ( char * ) hb_xgrab( nLen + 1 );
   pszTmp = pszBuf;

   while( nTotal < nLen )
   {
      /*
       * here it's being decided that nToRead is never over 32768 bytes long,
       * so hb_fsRead() is fine, no hb_fsReadLarge() needed
       */

      if( nLen - nTotal > SINGLEBUF )
         nToRead = SINGLEBUF - s_nCount;
      else
         nToRead = ( s_nCount + nLen - nTotal > SINGLEBUF ? SINGLEBUF - s_nCount : nLen - nTotal );

      nBytes = hb_fsRead( hStdIn, pszStrIn, ( HB_USHORT ) nToRead );

      countCheck( nBytes );

      memcpy( pszTmp, pszStrIn, nBytes );
      nTotal += nBytes;
      pszTmp  = pszBuf + nTotal;
   }

   hb_xfree( pszStrIn );
   hb_xfree( pszLenPrefix );
   hb_retclen_buffer( pszBuf, nLen );
}
