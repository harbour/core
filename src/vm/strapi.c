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

#include "hbvmopt.h"
#include "hbapistr.h"
#include "hbapiitm.h"
#include "hbstack.h"


static const HB_WCHAR s_szConstStr[ 1 ] = { 0 };

HB_SIZE hb_wstrlen( const HB_WCHAR * szText )
{
   HB_SIZE nLen = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrlen(%p)", szText ) );

   if( szText )
   {
      while( szText[ nLen ] )
         ++nLen;
   }

   return nLen;
}

HB_SIZE hb_wstrnlen( const HB_WCHAR * szText, HB_SIZE nCount )
{
   HB_SIZE nLen = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrnlen(%p,%" HB_PFS "u)", szText, nCount ) );

   if( szText )
   {
      while( nCount-- && szText[ nLen ] )
         ++nLen;
   }

   return nLen;
}

int hb_wstrcmp( const HB_WCHAR * s1, const HB_WCHAR * s2 )
{
   int rc = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrcmp(%p, %p)", s1, s2 ) );

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

int hb_wstrncmp( const HB_WCHAR * s1, const HB_WCHAR * s2, HB_SIZE nCount )
{
   int rc = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrncmp(%p, %p, %" HB_PFS "u)", s1, s2, nCount ) );

   while( nCount-- )
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

HB_WCHAR * hb_wstrncpy( HB_WCHAR * pDest, const HB_WCHAR * pSource, HB_SIZE nLen )
{
   HB_WCHAR * pBuf = pDest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrncpy(%p, %p, %" HB_PFS "u)", pDest, pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && ( *pDest++ = *pSource++ ) != '\0' )
      nLen--;

   return pBuf;
}

HB_WCHAR * hb_wstrncat( HB_WCHAR * pDest, const HB_WCHAR * pSource, HB_SIZE nLen )
{
   HB_WCHAR * pBuf = pDest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_strncat(%p, %p, %" HB_PFS "u)", pDest, pSource, nLen ) );

   pDest[ nLen ] = '\0';

   while( nLen && *pDest )
   {
      pDest++;
      nLen--;
   }

   while( nLen && ( *pDest++ = *pSource++ ) != '\0' )
      nLen--;

   return pBuf;
}

HB_WCHAR * hb_wstrdup( const HB_WCHAR * szText )
{
   HB_WCHAR * pszDest;
   HB_SIZE nSize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrdup(%p)", szText ) );

   nSize = ( hb_wstrlen( szText ) + 1 ) * sizeof( HB_WCHAR );
   pszDest = ( HB_WCHAR * ) hb_xgrab( nSize );

   memcpy( pszDest, szText, nSize );

   return pszDest;
}

HB_WCHAR * hb_wstrndup( const HB_WCHAR * szText, HB_SIZE nLen )
{
   HB_WCHAR * pszDest;
   HB_SIZE nSize;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrndup(%p,%" HB_PFS "u)", szText, nLen ) );

   nSize = hb_wstrlen( szText );
   if( nSize < nLen )
      nLen = nSize;
   nSize = nLen * sizeof( HB_WCHAR );
   pszDest = ( HB_WCHAR * ) hb_xgrab( nSize + sizeof( HB_WCHAR ) );
   memcpy( pszDest, szText, nSize );
   pszDest[ nLen ] = 0;

   return pszDest;
}

HB_WCHAR * hb_wstrunshare( void ** phStr, const HB_WCHAR * pStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrunshare(%p,%p,%" HB_PFS "u)", phStr, pStr, nLen ) );

   if( pStr == NULL || phStr == NULL || *phStr == NULL )
      return NULL;

   if( nLen > 0 &&
       ( *phStr == ( void * ) s_szConstStr || hb_xRefCount( *phStr ) > 1 ) )
   {
      HB_WCHAR * pszDest = ( HB_WCHAR * ) hb_xgrab( ( nLen + 1 ) *
                                                    sizeof( HB_WCHAR ) );
      memcpy( pszDest, pStr, nLen * sizeof( HB_WCHAR ) );
      pszDest[ nLen ] = 0;
      if( *phStr != ( void * ) s_szConstStr )
         hb_xRefDec( *phStr );
      *phStr = ( void * ) pszDest;

      return pszDest;
   }

   return ( HB_WCHAR * ) pStr;
}

char * hb_strunshare( void ** phStr, const char * pStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_strunshare(%p,%p,%" HB_PFS "u)", phStr, pStr, nLen ) );

   if( pStr == NULL || phStr == NULL || *phStr == NULL )
      return NULL;

   if( nLen > 0 &&
       ( *phStr == ( void * ) s_szConstStr || hb_xRefCount( *phStr ) > 1 ) )
   {
      char * pszDest = ( char * ) hb_xgrab( ( nLen + 1 ) * sizeof( char ) );
      memcpy( pszDest, pStr, nLen * sizeof( char ) );
      pszDest[ nLen ] = 0;
      if( *phStr != ( void * ) s_szConstStr )
         hb_xRefDec( *phStr );
      *phStr = ( void * ) pszDest;

      return pszDest;
   }

   return ( char * ) pStr;
}

const char * hb_strnull( const char * str )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_strnull(%p)", str ) );

   return str ? str : "";
}

const HB_WCHAR * hb_wstrnull( const HB_WCHAR * str )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_wstrnull(%p)", str ) );

   return str ? str : s_szConstStr;
}

void hb_strfree( void * hString )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_strfree(%p)", hString ) );

   if( hString && hString != ( void * ) s_szConstStr )
      hb_xRefFree( hString );
}




const char * hb_itemGetStr( PHB_ITEM pItem, void * cdp, void ** phString, HB_SIZE * pnLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetStr(%p,%p,%p,%p)", pItem, cdp, phString, pnLen ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      const char * pString;
      char * pFree = NULL;
      HB_SIZE nSize = 0;

      pString = hb_cdpnDup3( pItem->item.asString.value,
                             pItem->item.asString.length,
                             NULL, pnLen, &pFree, &nSize,
                             hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
      if( pFree != NULL )
         *phString = ( void * ) pFree;
      else if( pItem->item.asString.allocated == 0 )
         *phString = ( void * ) s_szConstStr;
      else
      {
         *phString = ( void * ) pItem->item.asString.value;
         hb_xRefInc( pItem->item.asString.value );
      }
      return pString;
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * hb_itemGetStrUTF8( PHB_ITEM pItem, void ** phString, HB_SIZE * pnLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetStrUTF8(%p,%p,%p)", pItem, phString, pnLen ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE nLen = hb_cdpStrAsUTF8Len( cdp,
                                         pItem->item.asString.value,
                                         pItem->item.asString.length, 0 );
      if( pnLen )
         *pnLen = nLen;

      if( nLen != pItem->item.asString.length )
      {
         char * pszUtf8 = ( char * ) hb_xgrab( nLen + 1 );
         hb_cdpStrToUTF8( cdp,
                          pItem->item.asString.value, pItem->item.asString.length,
                          pszUtf8, nLen + 1 );
         *phString = ( void * ) pszUtf8;
         return pszUtf8;
      }

      if( pItem->item.asString.allocated != 0 )
      {
         *phString = ( void * ) pItem->item.asString.value;
         hb_xRefInc( pItem->item.asString.value );
      }
      else
         *phString = ( void * ) s_szConstStr;
      return pItem->item.asString.value;
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_itemGetStrU16( PHB_ITEM pItem, int iEndian,
                                   void ** phString, HB_SIZE * pnLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemGetStrU16(%p,%d,%p,%p)", pItem, iEndian, phString, pnLen ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      HB_WCHAR * pszU16;
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE nLen = hb_cdpStrAsU16Len( cdp, pItem->item.asString.value,
                                             pItem->item.asString.length, 0 );
      if( pnLen )
         *pnLen = nLen;

      if( nLen == 0 )
      {
         *phString = ( void * ) s_szConstStr;
         return s_szConstStr;
      }

      pszU16 = ( HB_WCHAR * ) hb_xgrab( ( nLen + 1 ) * sizeof( HB_WCHAR ) );
      hb_cdpStrToU16( cdp, iEndian,
                      pItem->item.asString.value, pItem->item.asString.length,
                      pszU16, nLen + 1 );

      *phString = ( void * ) pszU16;
      return pszU16;
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


HB_SIZE hb_itemCopyStr( PHB_ITEM pItem, void * cdp, char * pStrBuffer, HB_SIZE nSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemCopyStr(%p,%p,%p,%" HB_PFS "u)", pItem, cdp, pStrBuffer, nSize ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         return hb_cdpTransTo( pItem->item.asString.value,
                               pItem->item.asString.length,
                               pStrBuffer, nSize,
                               hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
      else
         return hb_cdpnDup2Len( pItem->item.asString.value,
                                pItem->item.asString.length,
                                nSize, hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
   }
   else if( pStrBuffer && nSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}

HB_SIZE hb_itemCopyStrUTF8( PHB_ITEM pItem, char * pStrBuffer, HB_SIZE nSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemCopyStrUTF8(%p,%p,%" HB_PFS "u)", pItem, pStrBuffer, nSize ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         nSize = hb_cdpStrToUTF8( hb_vmCDP(),
                                  pItem->item.asString.value,
                                  pItem->item.asString.length,
                                  pStrBuffer, nSize );
      else
         nSize = hb_cdpStrAsUTF8Len( hb_vmCDP(),
                                     pItem->item.asString.value,
                                     pItem->item.asString.length, nSize );
      return nSize;
   }
   else if( pStrBuffer && nSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}


HB_SIZE hb_itemCopyStrU16( PHB_ITEM pItem, int iEndian, HB_WCHAR * pStrBuffer, HB_SIZE nSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_itemCopyStrU16(%p,%d,%p,%" HB_PFS "u)", pItem, iEndian, pStrBuffer, nSize ) );

   if( pItem && HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         nSize = hb_cdpStrToU16( hb_vmCDP(), iEndian,
                                 pItem->item.asString.value,
                                 pItem->item.asString.length,
                                 pStrBuffer, nSize );
      else
         nSize = hb_cdpStrAsU16Len( hb_vmCDP(),
                                    pItem->item.asString.value,
                                    pItem->item.asString.length, nSize );
      return nSize;
   }
   else if( pStrBuffer && nSize )
      pStrBuffer[ 0 ] = '\0';

   return 0;
}


PHB_ITEM hb_itemPutStrLen( PHB_ITEM pItem, void * cdp, const char * pStr, HB_SIZE nLen )
{
   char * pszText;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutStrLen(%p,%p,%p,%" HB_PFS "u)", pItem, cdp, pStr, nLen ) );

   if( nLen == 0 )
      return hb_itemPutC( pItem, NULL );

   pszText = hb_cdpnDup( pStr, &nLen, ( PHB_CODEPAGE ) cdp, hb_vmCDP() );

   return hb_itemPutCLPtr( pItem, pszText, nLen );
}

PHB_ITEM hb_itemPutStrLenUTF8( PHB_ITEM pItem, const char * pStr, HB_SIZE nLen )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE nDest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutStrLenUTF8(%p,%p,%" HB_PFS "u)", pItem, pStr, nLen ) );

   if( nLen == 0 )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   nDest = hb_cdpUTF8AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) hb_xgrab( nDest + 1 );
   hb_cdpUTF8ToStr( cdp, pStr, nLen, pszDest, nDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, nDest );
}

PHB_ITEM hb_itemPutStrLenU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr, HB_SIZE nLen )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE nDest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutStrLenU16(%p,%d,%p,%" HB_PFS "u)", pItem, iEndian, pStr, nLen ) );

   if( nLen == 0 )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   nDest = hb_cdpU16AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) hb_xgrab( nDest + 1 );
   hb_cdpU16ToStr( cdp, iEndian, pStr, nLen, pszDest, nDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, nDest );
}


PHB_ITEM hb_itemPutStr( PHB_ITEM pItem, void * cdp, const char * pStr )
{
   char * pszText;
   HB_SIZE nLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutStr(%p,%p,%p)", pItem, cdp, pStr ) );

   if( pStr == NULL )
      return hb_itemPutC( pItem, NULL );

   nLen = strlen( pStr );
   pszText = hb_cdpnDup( pStr, &nLen, ( PHB_CODEPAGE ) cdp, hb_vmCDP() );

   return hb_itemPutCLPtr( pItem, pszText, nLen );
}

PHB_ITEM hb_itemPutStrUTF8( PHB_ITEM pItem, const char * pStr )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE nDest, nLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutStrUTF8(%p,%p)", pItem, pStr ) );

   if( pStr == NULL )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   nLen = strlen( pStr );
   nDest = hb_cdpUTF8AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) hb_xgrab( nDest + 1 );
   hb_cdpUTF8ToStr( cdp, pStr, nLen, pszDest, nDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, nDest );
}

PHB_ITEM hb_itemPutStrU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   HB_SIZE nDest, nLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_itemPutStrU16(%p,%d,%p)", pItem, iEndian, pStr ) );

   if( pStr == NULL )
      return hb_itemPutC( pItem, NULL );

   cdp = hb_vmCDP();
   nLen = hb_wstrlen( pStr );
   nDest = hb_cdpU16AsStrLen( cdp, pStr, nLen, 0 );
   pszDest = ( char * ) hb_xgrab( nDest + 1 );
   hb_cdpU16ToStr( cdp, iEndian, pStr, nLen, pszDest, nDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, nDest );
}




const char * hb_arrayGetStr( PHB_ITEM pArray, HB_SIZE nIndex, void * cdp,
                             void ** phString, HB_SIZE * pnLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetStr(%p, %" HB_PFS "u, %p, %p, %p)", pArray, nIndex, cdp, phString, pnLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetStr( pArray->item.asArray.value->pItems + nIndex - 1,
                            cdp, phString, pnLen );
   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * hb_arrayGetStrUTF8( PHB_ITEM pArray, HB_SIZE nIndex,
                                 void ** phString, HB_SIZE * pnLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetStrUTF8(%p, %" HB_PFS "u, %p, %p)", pArray, nIndex, phString, pnLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetStrUTF8( pArray->item.asArray.value->pItems + nIndex - 1,
                                phString, pnLen );
   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_arrayGetStrU16( PHB_ITEM pArray, HB_SIZE nIndex, int iEndian,
                                    void ** phString, HB_SIZE * pnLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetStrU16(%p, %" HB_PFS "u, %d, %p, %p)", pArray, nIndex, iEndian, phString, pnLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetStrU16( pArray->item.asArray.value->pItems + nIndex - 1,
                               iEndian, phString, pnLen );
   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


HB_BOOL hb_arraySetStrLen( PHB_ITEM pArray, HB_SIZE nIndex, void * cdp,
                           const char * pStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetStrLen(%p, %" HB_PFS "u, %p, %p, %" HB_PFS "u)", pArray, nIndex, cdp, pStr, nLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutStrLen( pArray->item.asArray.value->pItems + nIndex - 1, cdp,
                        pStr, nLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrLenUTF8( PHB_ITEM pArray, HB_SIZE nIndex,
                               const char * pStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetStrLenUTF8(%p, %" HB_PFS "u, %p, %" HB_PFS "u)", pArray, nIndex, pStr, nLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutStrLenUTF8( pArray->item.asArray.value->pItems + nIndex - 1,
                            pStr, nLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrLenU16( PHB_ITEM pArray, HB_SIZE nIndex, int iEndian,
                              const HB_WCHAR * pStr, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetStrLenU16(%p, %" HB_PFS "u, %d, %p, %" HB_PFS "u)", pArray, nIndex, iEndian, pStr, nLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutStrLenU16( pArray->item.asArray.value->pItems + nIndex - 1,
                           iEndian, pStr, nLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}


HB_BOOL hb_arraySetStr( PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, const char * pStr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetStr(%p, %" HB_PFS "u, %p, %p)", pArray, nIndex, cdp, pStr ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutStr( pArray->item.asArray.value->pItems + nIndex - 1, cdp, pStr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrUTF8( PHB_ITEM pArray, HB_SIZE nIndex, const char * pStr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetStrUTF8(%p, %" HB_PFS "u, %p)", pArray, nIndex, pStr ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutStrUTF8( pArray->item.asArray.value->pItems + nIndex - 1, pStr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetStrU16( PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, const HB_WCHAR * pStr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetStrU16(%p, %" HB_PFS "u, %d, %p)", pArray, nIndex, iEndian, pStr ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutStrU16( pArray->item.asArray.value->pItems + nIndex - 1, iEndian, pStr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}




const char * hb_parstr( int iParam, void * cdp, void ** phString, HB_SIZE * pnLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_parstr(%d,%p,%p,%p)", iParam, cdp, phString, pnLen ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStr( pItem, cdp, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * hb_parstr_utf8( int iParam, void ** phString, HB_SIZE * pnLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_parstr_utf8(%d,%p,%p)", iParam, phString, pnLen ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStrUTF8( pItem, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_parstr_u16( int iParam, int iEndian,
                                void ** phString, HB_SIZE * pnLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_parstr_u16(%d,%d,%p,%p)", iParam, iEndian, phString, pnLen ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStrU16( pItem, iEndian, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


const char * hb_parastr( int iParam, HB_SIZE nIndex,
                         void * cdp, void ** phString, HB_SIZE * pnLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_parastr(%d,%" HB_PFS "u,%p,%p,%p)", iParam, nIndex, cdp, phString, pnLen ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
         return hb_arrayGetStr( pItem, nIndex, cdp, phString, pnLen );
      else
         return hb_itemGetStr( pItem, cdp, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const char * hb_parastr_utf8( int iParam, HB_SIZE nIndex,
                              void ** phString, HB_SIZE * pnLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_parastr_utf8(%d,%" HB_PFS "u,%p,%p)", iParam, nIndex, phString, pnLen ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
         return hb_arrayGetStrUTF8( pItem, nIndex, phString, pnLen );
      else
         return hb_itemGetStrUTF8( pItem, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}

const HB_WCHAR * hb_parastr_u16( int iParam, HB_SIZE nIndex, int iEndian,
                                 void ** phString, HB_SIZE * pnLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_parastr_u16(%d,%" HB_PFS "u,%d,%p,%p)", iParam, nIndex, iEndian, phString, pnLen ) );

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
         return hb_arrayGetStrU16( pItem, nIndex, iEndian, phString, pnLen );
      else
         return hb_itemGetStrU16( pItem, iEndian, phString, pnLen );
   }

   if( pnLen )
      *pnLen = 0;
   *phString = NULL;

   return NULL;
}


void hb_retstr( void * cdp, const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_retstr(%p,%s)", cdp, szText ) );

   hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText,
                     szText ? strlen( szText ) : 0 );
}

void hb_retstr_utf8( const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_retstr_utf8(%s)", szText ) );

   hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText,
                         szText ? strlen( szText ) : 0 );
}

void hb_retstr_u16( int iEndian, const HB_WCHAR * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_retstr_u16(%d,%p)", iEndian, szText ) );

   hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText,
                        hb_wstrlen( szText ) );
}


void hb_retstrlen( void * cdp, const char * szText, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_retstrlen(%p,%s,%" HB_PFS "u)", cdp, szText, nLen ) );

   hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText, nLen );
}

void hb_retstrlen_utf8( const char * szText, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_retstrlen_utf8(%s,%" HB_PFS "u)", szText, nLen ) );

   hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText, nLen );
}

void hb_retstrlen_u16( int iEndian, const HB_WCHAR * szText, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_retstrlen_u16(%d,%p,%" HB_PFS "u)", iEndian, szText, nLen ) );

   hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText, nLen );
}


int hb_storstr( void * cdp, const char * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_storstr(%p,%s,%d)", cdp, szText, iParam ) );

   if( iParam == -1 )
   {
      hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText,
                        szText ? strlen( szText ) : 0 );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLen( hb_itemUnRef( pItem ), cdp, szText,
                           szText ? strlen( szText ) : 0 );
         return 1;
      }
   }

   return 0;
}

int hb_storstr_utf8( const char * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_storstr_utf8(%s,%d)", szText, iParam ) );

   if( iParam == -1 )
   {
      hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText,
                            szText ? strlen( szText ) : 0 );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenUTF8( hb_itemUnRef( pItem ), szText,
                               szText ? strlen( szText ) : 0 );
         return 1;
      }
   }

   return 0;
}

int hb_storstr_u16( int iEndian, const HB_WCHAR * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_storstr_u16(%d,%p,%d)", iEndian, szText, iParam ) );

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


int hb_storstrlen( void * cdp, const char * szText, HB_SIZE nLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_storstrlen(%p,%s,%" HB_PFS "u,%d)", cdp, szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLen( hb_itemUnRef( pItem ), cdp, szText, nLen );
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen_utf8( const char * szText, HB_SIZE nLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_storstrlen_utf8(%s,%" HB_PFS "u,%d)", szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenUTF8( hb_itemUnRef( pItem ), szText, nLen );
         return 1;
      }
   }

   return 0;
}

int hb_storstrlen_u16( int iEndian, const HB_WCHAR * szText, HB_SIZE nLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_storstrlen_u16(%d,%p,%" HB_PFS "u,%d)", iEndian, szText, nLen, iParam ) );

   if( iParam == -1 )
   {
      hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutStrLenU16( hb_itemUnRef( pItem ), iEndian, szText, nLen );
         return 1;
      }
   }

   return 0;
}
