/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    example of Harbour codepage using UTF8 encoding
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define HB_UTF8EX_SORT

#include "hbapi.h"
#include "hbapicdp.h"

#include "uc16def.c"

#ifdef HB_UTF8EX_SORT
#  include "utf8sort.c"
#endif

static HB_CDP_GET_FUNC( UTF8_get )
{
   HB_SIZE nIndex = *pnIndex;
   int n = 0;

   HB_SYMBOL_UNUSED( cdp );

   *wc = 0;
   while( nIndex < nLen )
   {
      if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) pSrc[ nIndex ], &n, wc ) )
         ++nIndex;
      if( n == 0 )
      {
         *pnIndex = nIndex;
         return HB_TRUE;
      }
   }
   if( n != 0 )
   {
      *pnIndex = nIndex;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HB_CDP_PUT_FUNC( UTF8_put )
{
   int i = hb_cdpUTF8CharSize( wc );

   HB_SYMBOL_UNUSED( cdp );

   if( *pnIndex + i <= nLen )
   {
      hb_cdpU16CharToUTF8( &pDst[ *pnIndex ], wc );
      *pnIndex += i;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HB_CDP_LEN_FUNC( UTF8_len )
{
   HB_SYMBOL_UNUSED( cdp );

   return hb_cdpUTF8CharSize( wc );
}

static HB_CDP_UPPER_FUNC( UTF8_upper )
{
   HB_WCHAR wcUP;

   HB_SYMBOL_UNUSED( cdp );

   wcUP = s_uc_upper( wc );
   return wcUP ? wcUP : wc;
}

static HB_CDP_LOWER_FUNC( UTF8_lower )
{
   HB_WCHAR wcLO;

   HB_SYMBOL_UNUSED( cdp );

   wcLO = s_uc_lower( wc );
   return wcLO ? wcLO : wc;
}

static HB_CDP_FLAGS_FUNC( UTF8_flags )
{
   HB_SYMBOL_UNUSED( cdp );

   return s_uc_flags( wc );
}

static HB_CDP_CMP_FUNC( UTF8_cmp )
{
   int iRet;

#ifdef HB_UTF8EX_SORT

   HB_SIZE nPos1 = 0, nPos2 = 0;
   HB_WCHAR wc1, wc2;

   iRet = 0;
   for( ;; )
   {
      if( !HB_CDPCHAR_GET( cdp, szSecond, nLenSecond, &nPos2, &wc2 ) )
      {
         if( fExact && HB_CDPCHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
            iRet = 1;
         break;
      }
      if( !HB_CDPCHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
      {
         iRet = -1;
         break;
      }
      if( wc1 != wc2 )
      {
         HB_USHORT us1 = s_uniSort[ wc1 ], us2 = s_uniSort[ wc2 ];
         if( us1 != us2 )
         {
            iRet = us1 < us2 ? -1 : 1;
            break;
         }
      }
   }

#else

   HB_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   HB_SYMBOL_UNUSED( cdp );

   iRet = memcmp( szFirst, szSecond, nLen );
   if( iRet == 0 )
   {
      if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }
#endif

   return iRet;
}

static HB_CDP_CMP_FUNC( UTF8_cmpi )
{
   int iRet = 0;

#ifdef HB_UTF8EX_SORT

   HB_SIZE nPos1 = 0, nPos2 = 0;
   HB_WCHAR wc1, wc2;

   iRet = 0;
   for( ;; )
   {
      if( !HB_CDPCHAR_GET( cdp, szSecond, nLenSecond, &nPos2, &wc2 ) )
      {
         if( fExact && HB_CDPCHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
            iRet = 1;
         break;
      }
      if( !HB_CDPCHAR_GET( cdp, szFirst, nLenFirst, &nPos1, &wc1 ) )
      {
         iRet = -1;
         break;
      }
      if( wc1 != wc2 )
      {
         HB_USHORT us1 = s_uniSort[ HB_CDPCHAR_UPPER( cdp, wc1 ) ],
                   us2 = s_uniSort[ HB_CDPCHAR_UPPER( cdp, wc2 ) ];
         if( us1 != us2 )
         {
            iRet = us1 < us2 ? -1 : 1;
            break;
         }
      }
   }

#else

   HB_SIZE nLen = nLenFirst < nLenSecond ? nLenFirst : nLenSecond;

   while( nLen-- )
   {
      HB_UCHAR u1 = cdp->upper[ ( HB_UCHAR ) * szFirst++ ],
               u2 = cdp->upper[ ( HB_UCHAR ) * szSecond++ ];
      if( u1 != u2 )
      {
         iRet = ( u1 < u2 ) ? -1 : 1;
         break;
      }
   }

   if( iRet == 0 )
   {
      if( nLenSecond > nLenFirst )
         iRet = -1;
      else if( fExact && nLenSecond < nLenFirst )
         iRet = 1;
   }
#endif

   return iRet;
}


static void hb_cp_init( PHB_CODEPAGE cdp )
{
   HB_UCHAR * flags, * upper, * lower;
   int i;

   cdp->buffer = ( HB_UCHAR * ) hb_xgrab( 0x300 );
   cdp->flags = flags = ( HB_UCHAR * ) cdp->buffer;
   cdp->upper = upper = ( HB_UCHAR * ) cdp->buffer + 0x100;
   cdp->lower = lower = ( HB_UCHAR * ) cdp->buffer + 0x200;

   for( i = 0; i < 0x100; ++i )
   {
      flags[ i ] = 0;
      if( HB_ISDIGIT( i ) )
         flags[ i ] |= HB_CDP_DIGIT;
      if( HB_ISALPHA( i ) )
         flags[ i ] |= HB_CDP_ALPHA;
      if( HB_ISUPPER( i ) )
         flags[ i ] |= HB_CDP_UPPER;
      if( HB_ISLOWER( i ) )
         flags[ i ] |= HB_CDP_LOWER;
      upper[ i ] = ( HB_UCHAR ) HB_TOUPPER( i );
      lower[ i ] = ( HB_UCHAR ) HB_TOLOWER( i );
   }
}

#define HB_CP_RAW

#define HB_CP_ID              UTF8EX
#define HB_CP_INFO            "UTF-8 extended"
#define HB_CP_UNITB           HB_UNITB_437

/* use character indexes instead of bytes ones */
#define HB_CP_CHARIDX
/* CHR(), ASC() and similar functions operates on Unicode values instead of bytes */
#define HB_CP_CHARUNI
/* UTF-8 string encoding */
#define HB_CP_UTF8

#define HB_CP_GET_FUNC        UTF8_get
#define HB_CP_PUT_FUNC        UTF8_put
#define HB_CP_LEN_FUNC        UTF8_len

#define HB_CP_FLAGS_FUNC      UTF8_flags
#define HB_CP_UPPER_FUNC      UTF8_upper
#define HB_CP_LOWER_FUNC      UTF8_lower

#define HB_CP_CMP_FUNC        UTF8_cmp
#define HB_CP_CMPI_FUNC       UTF8_cmpi

#define s_flags               NULL
#define s_upper               NULL
#define s_lower               NULL
#define s_sort                NULL

#define HB_CP_INIT hb_cp_init

/* include CP registration code */
#include "hbcdpreg.h"
