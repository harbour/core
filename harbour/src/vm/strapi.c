/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    string API functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbvmopt.h"
#include "hbapistr.h"
#include "hbapiitm.h"
#include "hbstack.h"


static const HB_WCHAR s_szConstStr[ 1 ] = { 0 };

static ULONG hb_wstrlen( const HB_WCHAR * szText )
{
   ULONG ulLen = 0;

   if( szText )
   {
      while( szText[ ulLen ] )
         ++ulLen;
   }

   return ulLen;
}

void hb_strfree( void * hString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strfree(%p)", hString));

   if( hString && hString != ( void * ) s_szConstStr )
      hb_xRefFree( hString );
}

void * hb_itemGetStr( PHB_ITEM pItem, void * cdp, const char ** pStrPtr, ULONG * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStr(%p,%p,%p,%p)", pItem, cdp, pStrPtr, pulLen));

   if( HB_IS_STRING( pItem ) )
   {
      char * pFree = NULL;
      ULONG ulSize = 0;

      * pStrPtr = hb_cdpnDup3( pItem->item.asString.value,
                               pItem->item.asString.length,
                               NULL, pulLen, &pFree, &ulSize,
                               hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
      if( pFree != NULL )
         return ( void * ) pFree;
      else if( pItem->item.asString.allocated )
      {
         hb_xRefInc( pItem->item.asString.value );
         return pItem->item.asString.value;
      }
      return ( void * ) s_szConstStr;
   }

   if( pulLen )
      * pulLen = 0;
   * pStrPtr = NULL;

   return NULL;
}

void * hb_itemGetStrUTF8( PHB_ITEM pItem, const char ** pStrPtr, ULONG * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrUTF8(%p,%p,%p)", pItem, pStrPtr, pulLen));

   if( HB_IS_STRING( pItem ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      ULONG ulLen = hb_cdpStrAsUTF8Len( cdp, FALSE,
                                        pItem->item.asString.value,
                                        pItem->item.asString.length, 0 );
      if( pulLen )
         *pulLen = ulLen;

      if( ulLen != pItem->item.asString.length )
      {
         char * pszUtf8 = ( char * ) hb_xgrab( ulLen + 1 );
         hb_cdpStrToUTF8( cdp, FALSE,
                          pItem->item.asString.value, pItem->item.asString.length,
                          pszUtf8, ulLen + 1 );
         *pStrPtr = pszUtf8;
         return pszUtf8;
      }

      *pStrPtr = pItem->item.asString.value;
      if( pItem->item.asString.allocated )
      {
         hb_xRefInc( pItem->item.asString.value );
         return pItem->item.asString.value;
      }
      return ( void * ) s_szConstStr;
   }

   if( pulLen )
      * pulLen = 0;
   * pStrPtr = NULL;

   return NULL;
}

void * hb_itemGetStrU16( PHB_ITEM pItem, int iEndian,
                         const HB_WCHAR ** pStrPtr, ULONG * pulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemGetStrU16(%p,%d,%p,%p)", pItem, iEndian, pStrPtr, pulLen));

   if( HB_IS_STRING( pItem ) )
   {
      HB_WCHAR * pszU16;
      PHB_CODEPAGE cdp = hb_vmCDP();
      ULONG ulLen = hb_cdpStrAsU16Len( cdp, FALSE,
                                       pItem->item.asString.value,
                                       pItem->item.asString.length, 0 );
      if( pulLen )
         *pulLen = ulLen;

      if( ulLen == 0 )
      {
         *pStrPtr = s_szConstStr;
         return ( void * ) s_szConstStr;
      }

      pszU16 = ( HB_WCHAR * ) hb_xgrab( ( ulLen + 1 ) * sizeof( HB_WCHAR ) );
      hb_cdpStrToU16( cdp, FALSE, iEndian,
                      pItem->item.asString.value, pItem->item.asString.length,
                      pszU16, ulLen + 1 );
      *pStrPtr = pszU16;
      return pszU16;
   }

   if( pulLen )
      * pulLen = 0;
   * pStrPtr = NULL;

   return NULL;
}

ULONG hb_itemCopyStr( PHB_ITEM pItem, void * cdp, char * pStrBuffer, ULONG ulSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStr(%p,%p,%p,%lu)", pItem, cdp, pStrBuffer, ulSize));

   if( HB_IS_STRING( pItem ) )
   {
      ulSize = hb_cdpTransTo( pItem->item.asString.value,
                              pItem->item.asString.length,
                              pStrBuffer, ulSize,
                              hb_vmCDP(), ( PHB_CODEPAGE ) cdp );
   }

   return 0;
}

ULONG hb_itemCopyStrUTF8( PHB_ITEM pItem, char * pStrBuffer, ULONG ulSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStrUTF8(%p,%p,%lu)", pItem, pStrBuffer, ulSize));

   if( HB_IS_STRING( pItem ) )
   {
      if( pStrBuffer )
         ulSize = hb_cdpStrToUTF8( hb_vmCDP(), FALSE,
                                   pItem->item.asString.value,
                                   pItem->item.asString.length,
                                   pStrBuffer, ulSize );
      else
         ulSize = hb_cdpStrAsUTF8Len( hb_vmCDP(), FALSE,
                                      pItem->item.asString.value,
                                      pItem->item.asString.length, ulSize );
      return ulSize;
   }

   return 0;
}

ULONG hb_itemCopyStrU16( PHB_ITEM pItem, int iEndian, HB_WCHAR * pStrBuffer, ULONG ulSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_itemCopyStrU16(%p,%d,%p,%lu)", pItem, iEndian, pStrBuffer, ulSize));

   if( HB_IS_STRING( pItem ) && ulSize )
   {
      if( pStrBuffer )
         ulSize = hb_cdpStrToU16( hb_vmCDP(), FALSE, iEndian,
                                  pItem->item.asString.value,
                                  pItem->item.asString.length,
                                  pStrBuffer, ulSize );
      else
         ulSize = hb_cdpStrAsU16Len( hb_vmCDP(), FALSE,
                                     pItem->item.asString.value,
                                     pItem->item.asString.length, ulSize );
      return ulSize;
   }

   return 0;
}

PHB_ITEM hb_itemPutStrLen( PHB_ITEM pItem, void * cdp, const char * pStr, ULONG ulLen )
{
   char * pszText;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLen(%p,%p,%p,%lu)", pItem, cdp, pStr, ulLen));

   pszText = hb_cdpnDup( pStr, &ulLen, ( PHB_CODEPAGE ) cdp, hb_vmCDP() );

   return hb_itemPutCLPtr( pItem, pszText, ulLen );
}

PHB_ITEM hb_itemPutStrLenUTF8( PHB_ITEM pItem, const char * pStr, ULONG ulLen )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   ULONG ulDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenUTF8(%p,%p,%lu)", pItem, pStr, ulLen));

   cdp = hb_vmCDP();
   ulDest = hb_cdpUTF8AsStrLen( cdp, FALSE, pStr, ulLen, 0 );
   pszDest = ( char * ) hb_xgrab( ulDest + 1 );
   hb_cdpUTF8ToStr( hb_vmCDP(), FALSE, pStr, ulLen, pszDest, ulDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, ulDest );
}

PHB_ITEM hb_itemPutStrLenU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr, ULONG ulLen )
{
   PHB_CODEPAGE cdp;
   char * pszDest;
   ULONG ulDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPutStrLenU16(%p,%d,%p,%lu)", pItem, iEndian, pStr, ulLen));

   cdp = hb_vmCDP();
   ulDest = hb_cdpU16AsStrLen( cdp, FALSE, pStr, ulLen, 0 );
   pszDest = ( char * ) hb_xgrab( ulDest + 1 );
   hb_cdpU16ToStr( hb_vmCDP(), FALSE, iEndian, pStr, ulLen, pszDest, ulDest + 1 );

   return hb_itemPutCLPtr( pItem, pszDest, ulDest );
}




void * hb_parstr( int iParam, void * cdp, const char ** pStrPtr, ULONG * pulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parstr(%d,%p,%p,%p)", iParam, cdp, pStrPtr, pulLen));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStr( pItem, cdp, pStrPtr, pulLen );
   }

   if( pulLen )
      *pulLen = 0;
   *pStrPtr = NULL;

   return NULL;
}

void * hb_parstr_utf8( int iParam, const char ** pStrPtr, ULONG * pulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parstr_utf8(%d,%p,%p)", iParam, pStrPtr, pulLen));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStrUTF8( pItem, pStrPtr, pulLen );
   }

   if( pulLen )
      * pulLen = 0;
   * pStrPtr = NULL;

   return NULL;
}

void * hb_parstr_u16( int iParam, int iEndian,
                      const HB_WCHAR ** pStrPtr, ULONG * pulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parstr_u16(%d,%d,%p,%p)", iParam, iEndian, pStrPtr, pulLen));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      return hb_itemGetStrU16( pItem, iEndian, pStrPtr, pulLen );
   }

   if( pulLen )
      * pulLen = 0;
   * pStrPtr = NULL;

   return NULL;
}

void hb_retstr( void * cdp, const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstr(%p,%s)", cdp, szText));

   hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText,
                     szText ? strlen( szText ) : 0 );
}

void hb_retstr_utf8( const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstr_utf8(%s)", szText));

   hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText,
                         szText ? strlen( szText ) : 0 );
}

void hb_retstr_u16( int iEndian, const HB_WCHAR * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstr_u16(%d,%p)", iEndian, szText));

   hb_itemPutStrLenU16( hb_stackReturnItem(), iEndian, szText,
                        hb_wstrlen( szText ) );
}

void hb_retstrlen( void * cdp, const char * szText, ULONG ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen(%p,%s,%lu)", cdp, szText, ulLen));

   hb_itemPutStrLen( hb_stackReturnItem(), cdp, szText, ulLen );
}

void hb_retstrlen_utf8( const char * szText, ULONG ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retstrlen_utf8(%s,%lu)", szText, ulLen));

   hb_itemPutStrLenUTF8( hb_stackReturnItem(), szText, ulLen );
}

void hb_retstrlen_u16( int iEndian, const HB_WCHAR * szText, ULONG ulLen )
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

   HB_TRACE(HB_TR_DEBUG, ("hb_storstr_utf8(%s,%d)", szText, iParam));

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

int hb_storstrlen( void * cdp, const char * szText, ULONG ulLen, int iParam )
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

int hb_storstrlen_utf8( const char * szText, ULONG ulLen, int iParam )
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

int hb_storstrlen_u16( int iEndian, const HB_WCHAR * szText, ULONG ulLen, int iParam )
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
