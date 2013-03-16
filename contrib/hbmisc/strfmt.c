/*
 * Harbour Project source code:
 * StrFormat() function
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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

#define HB_STRFORMAT_PARNUM_MAX_  9
#define POS_TO_PAR( pos )  ( pos + 2 )

typedef struct
{
   char *       raw;
   const char * buffer;
   HB_SIZE      nLen;
   HB_BOOL      bFreeReq;
} STRPAR;

/* TODO: Add support for embedded PICTURE string in mask string. */
/* TODO: Add a way to control trimming. (but 99.99% of the time, trimming is actually useful) */

HB_FUNC( STRFORMAT )
{
   int nParNum = hb_pcount();

   if( nParNum >= 1 )
   {
      const char * pszMask  = hb_parcx( 1 );
      HB_SIZE      nMaskLen = hb_parclen( 1 );
      HB_SIZE      nMaskPos;
      int          nPos;

      HB_SIZE nRetValLen;
      char *  pszRetVal;
      char *  pszRetValSave;

      STRPAR strpar[ HB_STRFORMAT_PARNUM_MAX_ ];

      memset( strpar, 0, sizeof( strpar ) );

      nParNum--;

      if( nParNum > HB_STRFORMAT_PARNUM_MAX_ )
         nParNum = HB_STRFORMAT_PARNUM_MAX_;

      /* Calculate length of return value */
      nRetValLen = 0;
      for( nMaskPos = 0; nMaskPos < nMaskLen; nMaskPos++ )
      {
         if( pszMask[ nMaskPos ] == '%' )
         {
            nMaskPos++;

            if( pszMask[ nMaskPos ] == '%' )
               nRetValLen++;
            else if( pszMask[ nMaskPos ] >= '1' && pszMask[ nMaskPos ] <= ( int ) ( nParNum + '0' ) )
            {
               nPos = pszMask[ nMaskPos ] - '1';

               strpar[ nPos ].raw = hb_itemString( hb_param( POS_TO_PAR( nPos ), HB_IT_ANY ), &strpar[ nPos ].nLen, &strpar[ nPos ].bFreeReq );

               /* AllTrim() */
               strpar[ nPos ].nLen   = hb_strRTrimLen( strpar[ nPos ].raw, strpar[ nPos ].nLen, HB_FALSE );
               strpar[ nPos ].buffer = hb_strLTrim( strpar[ nPos ].raw, &strpar[ nPos ].nLen );

               nRetValLen += strpar[ nPos ].nLen;
            }
         }
         else
            nRetValLen++;
      }

      /* Assemble return value */
      pszRetVal = pszRetValSave = ( char * ) hb_xgrab( nRetValLen + 1 );
      for( nMaskPos = 0; nMaskPos < nMaskLen; nMaskPos++ )
      {
         if( pszMask[ nMaskPos ] == '%' )
         {
            nMaskPos++;

            if( pszMask[ nMaskPos ] == '%' )
               *pszRetVal++ = pszMask[ nMaskPos ];
            else if( pszMask[ nMaskPos ] >= '1' && pszMask[ nMaskPos ] <= ( int ) ( nParNum + '0' ) )
            {
               nPos = pszMask[ nMaskPos ] - '1';

               memcpy( pszRetVal, strpar[ nPos ].buffer, strpar[ nPos ].nLen );
               pszRetVal += strpar[ nPos ].nLen;
            }
         }
         else
            *pszRetVal++ = pszMask[ nMaskPos ];
      }

      hb_retclen_buffer( pszRetValSave, nRetValLen );

      /* Free parameter buffers */
      for( nPos = 0; nPos < HB_STRFORMAT_PARNUM_MAX_; nPos++ )
      {
         if( strpar[ nPos ].raw && strpar[ nPos ].bFreeReq )
            hb_xfree( strpar[ nPos ].raw );
      }
   }
   else
      hb_retc_null();
}
