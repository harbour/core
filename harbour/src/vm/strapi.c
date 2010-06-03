/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    string API functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbvmopt.h"
#include "hbapistr.h"
#include "hbapiitm.h"
#include "hbstack.h"


static const HB_WCHAR s_szConstStr[ 1 ] = { 0 };

HB_SIZE hb_wstrlen( const HB_WCHAR * szText )
{
   HB_SIZE ulLen = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_wstrlen(%p)", szText));

   if( szText )
   {
      while( szText[ ulLen ] )
         ++ulLen;
   }

   return ulLen;
}

HB_SIZE hb_wstrnlen( const HB_WCHAR * szText, HB_SIZE count )
{
   HB_SIZE ulLen = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_wstrnlen(%p,%lu)", szText, count));

   if( szText )
   {
      while( count-- && szText[ ulLen ] )
         ++ulLen;
   }

   return ulLen;
}

int hb_wstrcmp( const HB_WCHAR * s1, const HB_WCHAR * s2 )
{
   int rc = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_wstrcmp(%p, %p)", s1, s2));

   for( ;; )
   {
      if( *s1 != *s2 )
      {
         rc = ( *s1 < *s2 ? -1 : 1 );
         break;
      }
      else if( *s1 == 0 )
         break;

      s1++;
      s2++;
   }

   return rc;
}

int hb_wstrncmp( const HB_WCHAR * s1, const HB_WCHAR * s2, HB_SIZE count )
{
   int rc = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_wstrncmp(%p, %p, %lu)", s1, s2, count));

   while( count-- )
   {
      if( *s1 != *s2 )
      {
         rc = ( *s1 < *s2 ? -1 : 1 );
         break;
      }
      else if( *s1 == 0 )
         break;

      s1++;
      s2++;
   }

   return rc;
}

HB_WCHAR * hb_wstrdup( const HB_WCHAR * szText )
{
   HB_WCHAR * pszDest;
   HB_SIZE ulSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_wstrdup(%p)", szText));

   ulSize = ( hb_wstrlen( szText ) + 1 ) * sizeof( HB_WCHAR );
   pszDest = ( HB_WCHAR * ) hb_xgrab( ulSize );

   memcpy( pszDest, szText, ulSize );

   return pszDest;
}

HB_WCHAR * hb_wstrndup( const HB_WCHAR * szText, HB_SIZE ulLen )
{
   HB_WCHAR * pszDest;
   HB_SIZE ulSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_wstrndup(%p,%lu)", szText,ulLen));

   ulSize = hb_wstrlen( szText );
   if( ulSize < ulLen )
      ulLen = ulSize;
   ulSize = ulLen * sizeof( HB_WCHAR );
   pszDest = ( HB_WCHAR * ) hb_xgrab( ulSize + sizeof( HB_WCHAR ) );
   memcpy( pszDest, szText, ulSize );
   pszDest[ ulLen ] = 0;

   return pszDest;
}

HB_WCHAR * hb_wstrunshare( void ** phStr, const HB_WCHAR * pStr, HB_SIZE ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrunshare(%p,%p,%lu)", phStr, pStr, ulLen));

   if( pStr == NULL || phStr == NULL || *phStr == NULL )
      return NULL;

   if( ulLen > 0 &&
       ( *phStr == ( void * ) s_szConstStr || hb_xRefCount( *phStr ) > 1 ) )
   {
      HB_WCHAR * pszDest = ( HB_WCHAR * ) hb_xgrab( ( ulLen + 1 ) *
                                                    sizeof( HB_WCHAR ) );
      memcpy( pszDest, pStr, ulLen * sizeof( HB_WCHAR ) );
      pszDest[ ulLen ] = 0;
      if( *phStr != ( void * ) s_szConstStr )
         hb_xRefDec( *phStr );
      * phStr = ( void * ) pszDest;

      return pszDest;
   }

   return ( HB_WCHAR * ) pStr;
}

char * hb_strunshare( void ** phStr, const char * pStr, HB_SIZE ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strunshare(%p,%p,%lu)", phStr, pStr, ulLen));

   if( pStr == NULL || phStr == NULL || *phStr == NULL )
      return NULL;

   if( ulLen > 0 &&
       ( *phStr == ( void * ) s_szConstStr || hb_xRefCount( *phStr ) > 1 ) )
   {
      char * pszDest = ( char * ) hb_xgrab( ( ulLen + 1 ) * sizeof( char ) );
      memcpy( pszDest, pStr, ulLen * sizeof( char ) );
      pszDest[ ulLen ] = 0;
      if( *phStr != ( void * ) s_szConstStr )
         hb_xRefDec( *phStr );
      * phStr = ( void * ) pszDest;

      return pszDest;
   }

   return ( char * ) pStr;
}

const char * hb_strnull( const char * str )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strnull(%p)", str));

   return str ? str : "";
}

const HB_WCHAR * hb_wstrnull( const HB_WCHAR * str )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_wstrnull(%p)", str));

   return str ? str : s_szConstStr;
}

void hb_strfree( void * hString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strfree(%p)", hString));

   if( hString && hString != ( void * ) s_szConstStr )
      hb_xRefFree( hString );
}




const char * hb_itemGetStr( PHB_ITEM pItem, void * cdp, void ** phString, HB_SIZE * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStr(%p,%p,%p,%p)", pItem, cdp, phString, pulLen));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      const char * pString;
      char * pFree = NULL;
      HB_SIZE ulSize = 0;

      pString = hb_cdpnDup3( pItem->item.asString.value,
                             pItem->item.asString.length,
                             NULL, pulLen, &pFree, &ulSize,
                             hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
      if( pFree != NULL )
         * phString = ( void * ) pFree;
      else if( pItem->item.asString.allocated == 0 )
         * phString = ( void * ) s_szConstStr;
      else
      {
         * phString = ( void * ) pItem->item.asString.value;
         hb_xRefInc( pItem->item.asString.value );
      }
      return pString;
   }

   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}

const char * hb_itemGetStrUTF8( PHB_ITEM pItem, void ** phString, HB_SIZE * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrUTF8(%p,%p,%p)", pItem, phString, pulLen));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE ulLen = hb_cdpStrAsUTF8Len( cdp, HB_FALSE,
                                          pItem->item.asString.value,
                                          pItem->item.asString.length, 0 );
      if( pulLen )
         *pulLen = ulLen;

      if( ulLen != pItem->item.asString.length )
      {
         char * pszUtf8 = ( char * ) hb_xgrab( ulLen + 1 );
         hb_cdpStrToUTF8( cdp, HB_FALSE,
                          pItem->item.asString.value, pItem->item.asString.length,
                          pszUtf8, ulLen + 1 );
         * phString = ( void * ) pszUtf8;
         return pszUtf8;
      }

      if( pItem->item.asString.allocated != 0 )
      {
         * phString = ( void * ) pItem->item.asString.value;
         hb_xRefInc( pItem->item.asString.value );
      }
      else
         * phString = ( void * ) s_szConstStr;
      return pItem->item.asString.value;
   }

   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_itemGetStrU16( PHB_ITEM pItem, int iEndian,
                                   void ** phString, HB_SIZE * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrU16(%p,%d,%p,%p)", pItem, iEndian, phString, pulLen));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      HB_WCHAR * pszU16;
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE ulLen = hb_cdpStrAsU16Len( cdp, HB_FALSE,
                                         pItem->item.asString.value,
                                         pItem->item.asString.length, 0 );
      if( pulLen )
         *pulLen = ulLen;

      if( ulLen == 0 )
      {
         * phString = ( void * ) s_szConstStr;
         return s_szConstStr;
      }

      pszU16 = ( HB_WCHAR * ) hb_xgrab( ( ulLen + 1 ) * sizeof( HB_WCHAR ) );
      hb_cdpStrToU16( cdp, HB_FALSE, iEndian,
                      pItem->item.asString.value, pItem->item.asString.length,
                      pszU16, ulLen + 1 );

      * phString = ( void * ) pszU16;
      return pszU16;
   }

   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}


HB_SIZE hb_itemCopyStr( PHB_ITEM pItem, void * cdp, char * pStrBuffer, HB_SIZE ulSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStr(%p,%p,%p,%lu)", pItem, cdp, pStrBuffer, ulSize));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         return hb_cdpTransTo( pItem->item.asString.value,
                               pItem->item.asString.length,
                               pStrBuffer, ulSize,
                               hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
      else
         return hb_cdpnDup2Len( pItem->item.asString.value,
                                pItem->item.asString.length,
                                ulSize, hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
   }
   else if( pStrBuffer && ulSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}

HB_SIZE hb_itemCopyStrUTF8( PHB_ITEM pItem, char * pStrBuffer, HB_SIZE ulSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStrUTF8(%p,%p,%lu)", pItem, pStrBuffer, ulSize));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         ulSize = hb_cdpStrToUTF8( hb_vmCDP(), HB_FALSE,
                                   pItem->item.asString.value,
                                   pItem->item.asString.length,
                                   pStrBuffer, ulSize );
      else
         ulSize = hb_cdpStrAsUTF8Len( hb_vmCDP(), HB_FALSE,
                                      pItem->item.asString.value,
                                      pItem->item.asString.length, ulSize );
      return ulSize;
   }
   else if( pStrBuffer && ulSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}


HB_SIZE hb_itemCopyStrU16( PHB_ITEM pItem, int iEndian, HB_WCHAR * pStrBuffer, HB_SIZE ulSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStrU16(%p,%d,%p,%lu)", pItem, iEndian, pStrBuffer, ulSize));

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         ulSize = hb_cdpStrToU16( hb_vmCDP(), HB_FALSE, iEndian,
                                  pItem->item.asString.value,
                                  pItem->item.asString.length,
                                  pStrBuffer, ulSize );
      else
         ulSize = hb_cdpStrAsU16Len( hb_vmCDP(), HB_FALSE,
                                     pItem->item.asString.value,
                                     pItem->item.asString.length, ulSize );
      return ulSize;
   }
   else if( pStrBuffer && ulSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}


PHB_ITEM hb_itemPutStrLen( PHB_ITEM pItem, void * cdp, const char * pStr, HB_SIZE ulLen )
{
   char * pszText;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLen(%p,%p,%p,%lu)", pItem, cdp, pStr, ulLen));

   if( ulLen == 0 )
      return hb_itemPutC( pItem, NULL );

   pszText = hb_cdpnDup( pStr, &ulLen, ( PHB_CODEPAGE ) cdp, hb_vmCDP() );

   return hb_itemPutCLPtr( pItem, pszText, ulLen );
}

PHB_ITEM hb_itemPutStrLenUTF8( PHB_ITEM pItem, const char * pStr, HB_SIZE ulLen )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE ulDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenUTF8(%p,%p,%lu)", pItem, pStr, ulLen));

   if( ulLen == 0 )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   ulDest = hb_cdpUTF8AsStrLen( cdp, HB_FALSE, pStr, ulLen, 0 );
   pszDest = ( char * ) hb_xgrab( ulDest + 1 );
   hb_cdpUTF8ToStr( cdp, HB_FALSE, pStr, ulLen, pszDest, ulDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, ulDest );
}

PHB_ITEM hb_itemPutStrLenU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr, HB_SIZE ulLen )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE ulDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenU16(%p,%d,%p,%lu)", pItem, iEndian, pStr, ulLen));

   if( ulLen == 0 )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   ulDest = hb_cdpU16AsStrLen( cdp, HB_FALSE, pStr, ulLen, 0 );
   pszDest = ( char * ) hb_xgrab( ulDest + 1 );
   hb_cdpU16ToStr( cdp, HB_FALSE, iEndian, pStr, ulLen, pszDest, ulDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, ulDest );
}


PHB_ITEM hb_itemPutStr( PHB_ITEM pItem, void * cdp, const char * pStr )
{
   char * pszText;
   HB_SIZE ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStr(%p,%p,%p)", pItem, cdp, pStr));

   if( pStr == NULL )
      return hb_itemPutC( pItem, NULL );

   ulLen = ( HB_SIZE ) strlen( pStr );
   pszText = hb_cdpnDup( pStr, &ulLen, ( PHB_CODEPAGE ) cdp, hb_vmCDP() );

   return hb_itemPutCLPtr( pItem, pszText, ulLen );
}

PHB_ITEM hb_itemPutStrUTF8( PHB_ITEM pItem, const char * pStr )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE ulDest, ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrUTF8(%p,%p)", pItem, pStr));

   if( pStr == NULL )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   ulLen = ( HB_SIZE ) strlen( pStr );
   ulDest = hb_cdpUTF8AsStrLen( cdp, HB_FALSE, pStr, ulLen, 0 );
   pszDest = ( char * ) hb_xgrab( ulDest + 1 );
   hb_cdpUTF8ToStr( cdp, HB_FALSE, pStr, ulLen, pszDest, ulDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, ulDest );
}

PHB_ITEM hb_itemPutStrU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE ulDest, ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrU16(%p,%d,%p)", pItem, iEndian, pStr));

   if( pStr == NULL )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   ulLen = hb_wstrlen( pStr );
   ulDest = hb_cdpU16AsStrLen( cdp, HB_FALSE, pStr, ulLen, 0 );
   pszDest = ( char * ) hb_xgrab( ulDest + 1 );
   hb_cdpU16ToStr( cdp, HB_FALSE, iEndian, pStr, ulLen, pszDest, ulDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, ulDest );
}




const char * hb_arrayGetStr( PHB_ITEM pArray, HB_SIZE ulIndex, void * cdp,
                             void ** phString, HB_SIZE * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetStr(%p, %lu, %p, %p, %p)", pArray, ulIndex, cdp, phString, pulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetStr( pArray->item.asArray.value->pItems + ulIndex - 1,
                            cdp, phString, pulLen );
   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}

const char * hb_arrayGetStrUTF8( PHB_ITEM pArray, HB_SIZE ulIndex,
                                 void ** phString, HB_SIZE * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetStrUTF8(%p, %lu, %p, %p)", pArray, ulIndex, phString, pulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetStrUTF8( pArray->item.asArray.value->pItems + ulIndex - 1,
                                phString, pulLen );
   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_arrayGetStrU16( PHB_ITEM pArray, HB_SIZE ulIndex, int iEndian,
                                    void ** phString, HB_SIZE * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetStrU16(%p, %lu, %d, %p, %p)", pArray, ulIndex, iEndian, phString, pulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetStrU16( pArray->item.asArray.value->pItems + ulIndex - 1,
                               iEndian, phString, pulLen );
   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}


HB_BOOL hb_arraySetStrLen( PHB_ITEM pArray, HB_SIZE ulIndex, void * cdp,
                           const char * pStr, HB_SIZE ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrLen(%p, %lu, %p, %p, %lu)", pArray, ulIndex, cdp, pStr, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutStrLen( pArray->item.asArray.value->pItems + ulIndex - 1, cdp,
                        pStr, ulLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrLenUTF8( PHB_ITEM pArray, HB_SIZE ulIndex,
                               const char * pStr, HB_SIZE ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrLenUTF8(%p, %lu, %p, %lu)", pArray, ulIndex, pStr, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutStrLenUTF8( pArray->item.asArray.value->pItems + ulIndex - 1,
                            pStr, ulLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrLenU16( PHB_ITEM pArray, HB_SIZE ulIndex, int iEndian,
                              const HB_WCHAR * pStr, HB_SIZE ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrLenU16(%p, %lu, %d, %p, %lu)", pArray, ulIndex, iEndian, pStr, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutStrLenU16( pArray->item.asArray.value->pItems + ulIndex - 1,
                           iEndian, pStr, ulLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}


HB_BOOL hb_arraySetStr( PHB_ITEM pArray, HB_SIZE ulIndex, void * cdp, const char * pStr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStr(%p, %lu, %p, %p)", pArray, ulIndex, cdp, pStr));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutStr( pArray->item.asArray.value->pItems + ulIndex - 1, cdp, pStr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrUTF8( PHB_ITEM pArray, HB_SIZE ulIndex, const char * pStr)
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrUTF8(%p, %lu, %p)", pArray, ulIndex, pStr));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutStrUTF8( pArray->item.asArray.value->pItems + ulIndex - 1, pStr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrU16( PHB_ITEM pArray, HB_SIZE ulIndex, int iEndian, const HB_WCHAR * pStr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetStrU16(%p, %lu, %d, %p)", pArray, ulIndex, iEndian, pStr));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutStrU16( pArray->item.asArray.value->pItems + ulIndex - 1, iEndian, pStr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}




const char * hb_parstr( int iParam, void * cdp, void ** phString, HB_SIZE * pulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parstr(%d,%p,%p,%p)", iParam, cdp, phString, pulLen));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStr( pItem, cdp, phString, pulLen );
   }

   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}

const char * hb_parstr_utf8( int iParam, void ** phString, HB_SIZE * pulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parstr_utf8(%d,%p,%p)", iParam, phString, pulLen));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStrUTF8( pItem, phString, pulLen );
   }

   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_parstr_u16( int iParam, int iEndian,
                                void ** phString, HB_SIZE * pulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parstr_u16(%d,%d,%p,%p)", iParam, iEndian, phString, pulLen));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStrU16( pItem, iEndian, phString, pulLen );
   }

   if( pulLen )
      * pulLen = 0;
   * phString = NULL;

   return NULL;
}


void hb_retstr( void * cdp, const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstr(%p,%s)", cdp, szText));

   hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText,
                     szText ? ( HB_SIZE ) strlen( szText ) : 0 );
}

void hb_retstr_utf8( const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstr_utf8(%s)", szText));

   hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText,
                         szText ? ( HB_SIZE ) strlen( szText ) : 0 );
}

void hb_retstr_u16( int iEndian, const HB_WCHAR * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstr_u16(%d,%p)", iEndian, szText));

   hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText,
                        hb_wstrlen( szText ) );
}


void hb_retstrlen( void * cdp, const char * szText, HB_SIZE ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen(%p,%s,%lu)", cdp, szText, ulLen));

   hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText, ulLen );
}

void hb_retstrlen_utf8( const char * szText, HB_SIZE ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen_utf8(%s,%lu)", szText, ulLen));

   hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText, ulLen );
}

void hb_retstrlen_u16( int iEndian, const HB_WCHAR * szText, HB_SIZE ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen_u16(%d,%p,%lu)", iEndian, szText, ulLen));

   hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText, ulLen );
}


int hb_storstr( void * cdp, const char * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storstr(%p,%s,%d)", cdp, szText, iParam));

   if( iParam == -1 )
   {
      hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText,
                        szText ? ( HB_SIZE ) strlen( szText ) : 0 );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLen( hb_itemUnRef( pItem ), cdp, szText,
                           szText ? ( HB_SIZE ) strlen( szText ) : 0 );
         return 1;
      }
   }

   return 0;
}

int hb_storstr_utf8( const char * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storstr_utf8(%s,%d)", szText, iParam));

   if( iParam == -1 )
   {
      hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText,
                            szText ? ( HB_SIZE ) strlen( szText ) : 0 );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenUTF8( hb_itemUnRef( pItem ), szText,
                               szText ? ( HB_SIZE ) strlen( szText ) : 0 );
         return 1;
      }
   }

   return 0;
}

int hb_storstr_u16( int iEndian, const HB_WCHAR * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storstr_u16(%d,%p,%d)", iEndian, szText, iParam));

   if( iParam == -1 )
   {
      hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian,
                           szText, hb_wstrlen( szText ) );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenU16( hb_itemUnRef( pItem ), iEndian,
                              szText, hb_wstrlen( szText ) );
         return 1;
      }
   }

   return 0;
}


int hb_storstrlen( void * cdp, const char * szText, HB_SIZE ulLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storstrlen(%p,%s,%lu,%d)", cdp, szText, ulLen, iParam));

   if( iParam == -1 )
   {
      hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText, ulLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLen( hb_itemUnRef( pItem ), cdp, szText, ulLen );
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen_utf8( const char * szText, HB_SIZE ulLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storstrlen_utf8(%s,%lu,%d)", szText, ulLen, iParam));

   if( iParam == -1 )
   {
      hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText, ulLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenUTF8( hb_itemUnRef( pItem ), szText, ulLen );
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen_u16( int iEndian, const HB_WCHAR * szText, HB_SIZE ulLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storstrlen_u16(%d,%p,%lu,%d)", iEndian, szText, ulLen, iParam));

   if( iParam == -1 )
   {
      hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText, ulLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenU16( hb_itemUnRef( pItem ), iEndian, szText, ulLen );
         return 1;
      }
   }

   return 0;
}
