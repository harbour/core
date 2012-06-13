/*
 * $Id$
 */

/*******
 *
 *  by Ilina Stoilkovska <anili100/at/gmail.com> 2011
 *     Aleksander Czajczynski <hb/at/fki.pl> 2012
 *
 *  Reading AMFIO data from standard input pipe
 *
 *
 ********/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbapifs.h"

#define SINGLEBUF 32768
#define MAXLEN    ( 16 * 1024 * 1024 )

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
   char *     pszStrIn      = ( char * ) hb_xgrab( SINGLEBUF );
   char *     pszLenPrefix  = ( char * ) hb_xgrab( 5 );
   char *     pszBuf;    /* = ( char * ) hb_xgrab( SINGLEBUF ); */
   char *     pszTmp      = pszLenPrefix;
   HB_USHORT  nBytes;
   int        nTotal      = 0;
   int        nLen;
   int        nToRead;
   HB_FHANDLE hStdIn      = hb_fsGetOsHandle( HB_STDIN_HANDLE );

   while( nTotal < 4 )
   {
      nToRead  = ( s_nCount + 4 - nTotal > SINGLEBUF ? SINGLEBUF - s_nCount : 4 - nTotal );
      nBytes   = hb_fsRead( hStdIn, pszStrIn, ( HB_USHORT ) nToRead );

      countCheck( nBytes );

      memcpy( pszTmp, pszStrIn, nBytes );
      nTotal    += nBytes;
      pszTmp    = pszLenPrefix + nTotal;
   }

   pszLenPrefix[ 4 ] = '\0';
   nLen              = HB_GET_LE_UINT32( pszLenPrefix );

   if( nLen >= MAXLEN )
   {
      hb_ret();
      return;
   }

   nTotal     = 0;
   pszBuf     = ( char * ) hb_xgrab( nLen + 1 );
   pszTmp     = pszBuf;

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
      nTotal    += nBytes;
      pszTmp    = pszBuf + nTotal;
   }

   hb_xfree( pszStrIn );
   hb_xfree( pszLenPrefix );
   hb_retclen_buffer( pszBuf, nLen );
}
