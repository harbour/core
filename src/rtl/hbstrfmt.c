/*
 * Harbour Project source code:
 * hb_StrFormat() functions
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "hbapierr.h"

typedef struct
{
   char *  pData;
   HB_SIZE nLen;
   HB_SIZE nMax;
} BUFFERTYPE;

static void bufadd( BUFFERTYPE * pBuf, const char * pAdd, HB_SIZE nLen )
{
   if( pBuf->nLen + nLen >= pBuf->nMax )
   {
      pBuf->nMax += ( pBuf->nMax >> 1 ) + nLen;
      pBuf->pData = ( char * ) hb_xrealloc( pBuf->pData, pBuf->nMax );
   }
   memcpy( pBuf->pData + pBuf->nLen, pAdd, nLen );
   pBuf->nLen += nLen;
   pBuf->pData[ pBuf->nLen ] = '\0';
}

static void hb_itemHexStr( PHB_ITEM pItem, char * pStr, HB_BOOL fUpper )
{
   HB_MAXUINT nValue, nTmp;
   int iLen;

   nValue = nTmp = hb_itemGetNInt( pItem );

   iLen = 0;
   do
   {
      ++iLen;
      nTmp >>= 4;
   }
   while( nTmp );

   pStr[ iLen ] = '\0';
   do
   {
      int iDigit = ( int ) ( nValue & 0x0F );
      pStr[ --iLen ] = ( char ) ( iDigit + ( iDigit < 10 ? '0' :
                                             ( fUpper ? 'A' : 'a' ) - 10 ) );
      nValue >>= 4;
   }
   while( iLen );
}

PHB_ITEM hb_strFormat( PHB_ITEM pItemReturn, PHB_ITEM pItemFormat, int iCount, PHB_ITEM * pItemArray )
{
   BUFFERTYPE  buffer;
   PHB_ITEM    pItem;
   const char  *pFmt, *pFmtEnd, *pFmtSave;
   int         i, iParam, iParamNo, iWidth, iDec;
   HB_SIZE     nSize;
   HB_BOOL     fLeftAlign, fForceSign, fPadZero, fSpaceSign, fSign;

   pFmt = hb_itemGetCPtr( pItemFormat );
   nSize = hb_itemGetCLen( pItemFormat );
   pFmtEnd = pFmt + nSize;

   buffer.nMax = nSize + 16;
   buffer.nLen = 0;
   buffer.pData = ( char * ) hb_xgrab( buffer.nMax );
   buffer.pData[ 0 ] = '\0';

   iParam = 0;
   while( pFmt < pFmtEnd )
   {
      if( *pFmt != '%' )
      {
         bufadd( &buffer, pFmt++, 1 );
         continue;
      }

      pFmtSave = pFmt;

      if( ++pFmt >= pFmtEnd )
         continue;

      if( *pFmt == '%' )
      {
         bufadd( &buffer, pFmt++, 1 );
         continue;
      }

      iWidth = iDec = -1;
      fLeftAlign = fForceSign = fPadZero = fSpaceSign = 0;

      /* parse parameter number */
      iParamNo = 0;
      while( HB_ISDIGIT( *pFmt ) )
         iParamNo = iParamNo * 10 + *pFmt++ - '0';

      if( iParamNo > 0 && *pFmt == '$' )
      {
         pFmt++;
      }
      else
      {
         iParamNo = -1;
         pFmt = pFmtSave + 1;
      }

      /* Parse flags */
      do
      {
         switch( *pFmt )
         {
            case '-':
               fLeftAlign = 1;
               continue;
            case '+':
               fForceSign = 1;
               continue;
            case ' ':
               fSpaceSign = 1;
               continue;
            case '0':
               fPadZero = 1;
               continue;
         }
         break;
      }
      while( *++pFmt );

      /* Parse width */
      if( HB_ISDIGIT( *pFmt ) )
      {
         iWidth = 0;
         while( HB_ISDIGIT( *pFmt ) )
            iWidth = iWidth * 10 + *pFmt++ - '0';
      }

      /* Parse decimals */
      if( *pFmt == '.' )
      {
         pFmt++;
         iDec = 0;
         if( HB_ISDIGIT( *pFmt ) )
         {
            while( HB_ISDIGIT( *pFmt ) )
               iDec = iDec * 10 + *pFmt++ - '0';
         }
      }

      /* Parse specifier */
      if( *pFmt == 'c' || *pFmt == 'd' || *pFmt == 'x' || *pFmt == 'X' ||
          *pFmt == 'f' || *pFmt == 's' )
      {
         if( iParamNo == -1 )
            iParamNo = ++iParam;

         pItem = ( iParamNo > iCount ) ? NULL : pItemArray[ iParamNo - 1 ];
      }
      else
         pItem = NULL;

      switch( *pFmt )
      {
         case 'c':
         {
            char  buf[ 1 ];

            buf[ 0 ] = ( char ) hb_itemGetNI( pItem );
            if( fLeftAlign )
            {
               bufadd( &buffer, buf, 1 );
            }
            if( iWidth > 1 )
            {
               for( i = 1; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }
            if( ! fLeftAlign )
            {
               bufadd( &buffer, buf, 1 );
            }
            break;
         }

         case 'd':
         case 'x':
         case 'X':
         {
            char  * pStr = NULL;
            const char * pStr2;
            int   iSize, iExtra;

            fSign = 0;
            if( pItem && HB_IS_NUMERIC( pItem ) )
            {
               iSize = sizeof( HB_MAXINT ) * 3 + 1;
               pStr2 = pStr = ( char * ) hb_xgrab( iSize + 1 );
               if( *pFmt == 'd' )
                  hb_itemStrBuf( pStr, pItem, iSize, 0 );
               else
                  hb_itemHexStr( pItem, pStr, *pFmt == 'X' );

               while( *pStr2 == ' ' )
                  pStr2++;
               iSize = ( int ) strlen( pStr2 );
               if( *pStr2 == '-' )
               {
                  fSign = 1;
                  iSize--;
                  pStr2++;
               }
            }
            else if( pItem && HB_IS_LOGICAL( pItem ) )
            {
               iSize = 1;
               if( hb_itemGetL( pItem ) )
                  pStr2 = "1";
               else
                  pStr2 = "0";
            }
            else
            {
               iSize = 1;
               pStr2 = "0";
            }

            iExtra = 0;
            if( fForceSign || fSpaceSign || fSign )
               iExtra = 1;

            /* If decimals is set, zero padding flag is ignored */
            if( iDec >= 0 )
               fPadZero = 0;

            if( fLeftAlign )
            {
               /* Zero padding is ignored on left Align */
               /* ForceSign has priority over SpaceSign */
               if( fSign )
                  bufadd( &buffer, "-", 1 );
               else if( fForceSign )
                  bufadd( &buffer, "+", 1 );
               else if( fSpaceSign )
                  bufadd( &buffer, " ", 1 );

               for( i = iSize; i < iDec; i++ )
                  bufadd( &buffer, "0", 1 );

               bufadd( &buffer, pStr2, ( HB_SIZE ) iSize );
               if( iDec > iSize )
                  iSize = iDec;
               for( i = iSize + iExtra; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }
            else
            {
               /* Right align */
               if( fPadZero )
               {
                  /* ForceSign has priority over SpaceSign */
                  if( fSign )
                     bufadd( &buffer, "-", 1 );
                  else if( fForceSign )
                     bufadd( &buffer, "+", 1 );
                  else if( fSpaceSign )
                     bufadd( &buffer, " ", 1 );

                  for( i = iSize + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, "0", 1 );

                  bufadd( &buffer, pStr2, strlen( pStr2 ) );
               }
               else
               {
                  for( i = ( iSize > iDec ? iSize : iDec ) + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, " ", 1 );

                  /* ForceSign has priority over SpaceSign */
                  if( fSign )
                     bufadd( &buffer, "-", 1 );
                  else if( fForceSign )
                     bufadd( &buffer, "+", 1 );
                  else if( fSpaceSign )
                     bufadd( &buffer, " ", 1 );

                  for( i = iSize; i < iDec; i++ )
                     bufadd( &buffer, "0", 1 );

                  bufadd( &buffer, pStr2, ( HB_SIZE ) iSize );
               }
            }

            if( pStr )
               hb_xfree( pStr );
            break;
         }

         case 'f':
         {
            char  * pStr = NULL;
            const char * pStr2;
            int   iSize, iExtra, iD;

            if( pItem && HB_IS_NUMERIC( pItem ) )
            {
               hb_itemGetNLen( pItem, &iSize, &iD );

               if( iDec != -1 )
               {
                  iSize += iDec - iD + 1;
                  iD = iDec;
               }

               /* Let 255 be a limit for number length */
               if( iSize > 255 )
                  iSize = 255;
               if( iD > 253 )
                  iD = 253;
               if( iSize < iD + 2 )
                  iSize = iD + 2;

               pStr2 = pStr = ( char * ) hb_xgrab( iSize + 1 );
               hb_itemStrBuf( pStr, pItem, iSize, iD );

               if( pStr[ 0 ] == '*' && iSize < 255 )
               {
                  pStr2 = pStr = ( char * ) hb_xrealloc( pStr, 256 );
                  hb_itemStrBuf( pStr, pItem, 255, iD );
               }
               while( *pStr2 == ' ' )
                  pStr2++;
               iSize = ( int ) strlen( pStr2 );
            }
            else
            {
               iSize = 1;
               pStr2 = "0";
            }

            iExtra = 0;
            if( ( fForceSign || fSpaceSign ) && *pStr2 != '-' )
               iExtra = 1;

            if( fLeftAlign )
            {
               /* Zero padding is ignored on left Align */
               if( *pStr2 != '-' )
               {
                  /* ForceSign has priority over SpaceSign */
                  if( fForceSign )
                     bufadd( &buffer, "+", 1 );
                  else
                  {
                     if( fSpaceSign )
                        bufadd( &buffer, " ", 1 );
                  }
               }
               bufadd( &buffer, pStr2, ( HB_SIZE ) iSize );
               for( i = iSize + iExtra; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }
            else
            {
               /* Right align */
               if( fPadZero )
               {
                  if( *pStr2 == '-' )
                  {
                     bufadd( &buffer, pStr2++, 1 );
                  }
                  else
                  {
                     /* ForceSign has priority over SpaceSign */
                     if( fForceSign )
                        bufadd( &buffer, "+", 1 );
                     else
                     {
                        if( fSpaceSign )
                           bufadd( &buffer, " ", 1 );
                     }
                  }
                  for( i = iSize + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, "0", 1 );

                  bufadd( &buffer, pStr2, strlen( pStr2 ) );
               }
               else
               {
                  for( i = iSize + iExtra; i < iWidth; i++ )
                     bufadd( &buffer, " ", 1 );

                  if( *pStr2 != '-' )
                  {
                     /* ForceSign has priority over SpaceSign */
                     if( fForceSign )
                        bufadd( &buffer, "+", 1 );
                     else
                     {
                        if( fSpaceSign )
                           bufadd( &buffer, " ", 1 );
                     }
                  }
                  bufadd( &buffer, pStr2, ( HB_SIZE ) iSize );
               }
            }

            if( pStr )
               hb_xfree( pStr );
            break;
         }

         case 's':
         {
            const char * pStr = hb_itemGetCPtr( pItem );

            nSize = hb_itemGetCLen( pItem );
            if( iDec >= 0 )
            {
               if( ( HB_SIZE ) iDec < nSize )
                  nSize = iDec;
            }
            if( fLeftAlign )
               bufadd( &buffer, pStr, nSize );

            if( iWidth > 1 )
            {
               for( i = ( int ) nSize; i < iWidth; i++ )
                  bufadd( &buffer, " ", 1 );
            }

            if( ! fLeftAlign )
               bufadd( &buffer, pStr, nSize );

            break;
         }

         default:
            bufadd( &buffer, pFmtSave, pFmt - pFmtSave + 1 );
      }
      pFmt++;
   }

   pItemReturn = hb_itemPutCL( pItemReturn, buffer.pData, buffer.nLen );
   hb_xfree( buffer.pData );
   return pItemReturn;
}

HB_FUNC( HB_STRFORMAT )
{
   PHB_ITEM   pFormat = hb_param( 1, HB_IT_STRING );
   int        i, iParams = hb_pcount();
   PHB_ITEM * pItemArray = NULL;

   if( pFormat )
   {
      if( iParams > 1 )
      {
         pItemArray = ( PHB_ITEM * ) hb_xgrab( ( iParams - 1 ) * sizeof( PHB_ITEM ) );
         for( i = 1; i < iParams; i++ )
            pItemArray[ i - 1 ] = hb_param( i + 1, HB_IT_ANY );
      }

      hb_itemReturnRelease( hb_strFormat( NULL, pFormat, iParams - 1, pItemArray ) );

      if( iParams > 1 )
         hb_xfree( pItemArray );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
