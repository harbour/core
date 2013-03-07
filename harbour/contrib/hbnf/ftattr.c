/*
 * $Id$
 */

/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   03 Oct 1992 14:35:14   GLENN
 *  Ted Means made modifications to support use of DispBegin()/DispEnd().
 *
 *     Rev 1.1   15 Aug 1991 23:07:58   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.0   12 Jun 1991 01:30:20   GLENN
 *  Initial revision.
 *
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_SAVEATT )
{
   int iTop    = hb_parni( 1 );        /* Defaults to zero on bad type */
   int iLeft   = hb_parni( 2 );        /* Defaults to zero on bad type */
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();
   int iBottom = hb_parnidef( 3, iMaxRow );
   int iRight  = hb_parnidef( 4, iMaxRow );

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
      HB_SIZE nSize;
      char *  pBuffer;
      char *  pAttrib;

      nSize   = ( iBottom - iTop + 1 ) * ( iRight - iLeft + 1 );
      pBuffer = pAttrib = ( char * ) hb_xgrab( nSize + 1 );
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int       iColor;
            HB_BYTE   bAttr;
            HB_USHORT usChar;
            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            *pBuffer++ = ( char ) iColor;
            ++iCol;
         }
         ++iTop;
      }
      hb_retclen_buffer( pAttrib, nSize );
   }
   else
      hb_retc_null();
}

/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   03 Oct 1992 14:33:46   GLENN
 *  Ted Means made modifications so these functions will work with
 *  dispBegin() and dispEnd().
 *
 *     Rev 1.1   15 Aug 1991 23:08:02   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.0   12 Jun 1991 01:30:14   GLENN
 *  Initial revision.
 *
 */

HB_FUNC( FT_RESTATT )
{
   HB_SIZE nLen = hb_parclen( 5 );

   if( nLen )
   {
      int iTop    = hb_parni( 1 );            /* Defaults to zero on bad type */
      int iLeft   = hb_parni( 2 );            /* Defaults to zero on bad type */
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
         const char * pAttrib = hb_parc( 5 );

         hb_gtDispBegin();

         while( nLen && iTop <= iBottom )
         {
            int iCol = iLeft;
            while( nLen && iCol <= iRight )
            {
               int       iColor;
               HB_BYTE   bAttr;
               HB_USHORT usChar;
               hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
               iColor = ( HB_UCHAR ) *pAttrib++;
               hb_gtPutChar( iTop, iCol, iColor, bAttr, usChar );
               ++iCol;
               --nLen;
            }
            ++iTop;
         }

         hb_gtDispEnd();
      }
   }
}
