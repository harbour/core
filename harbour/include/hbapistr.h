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

#ifndef HB_APISTR_H_
#define HB_APISTR_H_

#include "hbapi.h"
#include "hbapicdp.h"

HB_EXTERN_BEGIN

extern HB_EXPORT HB_SIZE hb_wstrlen( const HB_WCHAR * szText );
extern HB_EXPORT HB_SIZE hb_wstrnlen( const HB_WCHAR * szText, HB_SIZE nCount );
extern HB_EXPORT int hb_wstrcmp( const HB_WCHAR * s1, const HB_WCHAR * s2 );
extern HB_EXPORT int hb_wstrncmp( const HB_WCHAR * s1, const HB_WCHAR * s2, HB_SIZE nCount );
extern HB_EXPORT HB_WCHAR * hb_wstrncpy( HB_WCHAR * pDest, const HB_WCHAR * pSource, HB_SIZE nLen );
extern HB_EXPORT HB_WCHAR * hb_wstrncat( HB_WCHAR * pDest, const HB_WCHAR * pSource, HB_SIZE nLen );
extern HB_EXPORT HB_WCHAR * hb_wstrdup( const HB_WCHAR * szText );
extern HB_EXPORT HB_WCHAR * hb_wstrndup( const HB_WCHAR * szText, HB_SIZE nLen );

extern HB_EXPORT char * hb_strunshare( void ** phStr, const char * pStr, HB_SIZE nLen );
extern HB_EXPORT HB_WCHAR * hb_wstrunshare( void ** phStr, const HB_WCHAR * pStr, HB_SIZE nLen );
extern HB_EXPORT const char * hb_strnull( const char * str );
extern HB_EXPORT const HB_WCHAR * hb_wstrnull( const HB_WCHAR * str );


extern HB_EXPORT void hb_strfree( void * hString );

extern HB_EXPORT const char * hb_itemGetStr( PHB_ITEM pItem, void * cdp, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT const char * hb_itemGetStrUTF8( PHB_ITEM pItem, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT const HB_WCHAR * hb_itemGetStrU16( PHB_ITEM pItem, int iEndian, void ** phString, HB_SIZE * pnLen );

extern HB_EXPORT HB_SIZE hb_itemCopyStr( PHB_ITEM pItem, void * cdp, char * pStrBuffer, HB_SIZE nSize );
extern HB_EXPORT HB_SIZE hb_itemCopyStrUTF8( PHB_ITEM pItem, char * pStrBuffer, HB_SIZE nSize );
extern HB_EXPORT HB_SIZE hb_itemCopyStrU16( PHB_ITEM pItem, int iEndian, HB_WCHAR * pStrBuffer, HB_SIZE nSize );

extern HB_EXPORT PHB_ITEM hb_itemPutStrLen( PHB_ITEM pItem, void * cdp, const char * pStr, HB_SIZE nLen );
extern HB_EXPORT PHB_ITEM hb_itemPutStrLenUTF8( PHB_ITEM pItem, const char * pStr, HB_SIZE nLen );
extern HB_EXPORT PHB_ITEM hb_itemPutStrLenU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr, HB_SIZE nLen );

extern HB_EXPORT PHB_ITEM hb_itemPutStr( PHB_ITEM pItem, void * cdp, const char * pStr );
extern HB_EXPORT PHB_ITEM hb_itemPutStrUTF8( PHB_ITEM pItem, const char * pStr );
extern HB_EXPORT PHB_ITEM hb_itemPutStrU16( PHB_ITEM pItem, int iEndian, const HB_WCHAR * pStr );


extern HB_EXPORT const char * hb_arrayGetStr( PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT const char * hb_arrayGetStrUTF8( PHB_ITEM pArray, HB_SIZE nIndex, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT const HB_WCHAR * hb_arrayGetStrU16( PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, void ** phString, HB_SIZE * pnLen );

extern HB_EXPORT HB_BOOL hb_arraySetStrLen( PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, const char * pStr, HB_SIZE nLen );
extern HB_EXPORT HB_BOOL hb_arraySetStrLenUTF8( PHB_ITEM pArray, HB_SIZE nIndex, const char * pStr, HB_SIZE nLen );
extern HB_EXPORT HB_BOOL hb_arraySetStrLenU16( PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, const HB_WCHAR * pStr, HB_SIZE nLen );

extern HB_EXPORT HB_BOOL hb_arraySetStr( PHB_ITEM pArray, HB_SIZE nIndex, void * cdp, const char * pStr );
extern HB_EXPORT HB_BOOL hb_arraySetStrUTF8( PHB_ITEM pArray, HB_SIZE nIndex, const char * pStr );
extern HB_EXPORT HB_BOOL hb_arraySetStrU16( PHB_ITEM pArray, HB_SIZE nIndex, int iEndian, const HB_WCHAR * pStr );


extern HB_EXPORT const char * hb_parstr( int iParam, void * cdp, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT const char * hb_parstr_utf8( int iParam, void ** phString, HB_SIZE * pnLen );
extern HB_EXPORT const HB_WCHAR * hb_parstr_u16( int iParam, int iEndian, void ** phString, HB_SIZE * pnLen );

extern HB_EXPORT void hb_retstr( void * cdp, const char * szText );
extern HB_EXPORT void hb_retstr_utf8( const char * szText );
extern HB_EXPORT void hb_retstr_u16( int iEndian, const HB_WCHAR * szText );

extern HB_EXPORT void hb_retstrlen( void * cdp, const char * szText, HB_SIZE nLen );
extern HB_EXPORT void hb_retstrlen_utf8( const char * szText, HB_SIZE nLen );
extern HB_EXPORT void hb_retstrlen_u16( int iEndian, const HB_WCHAR * szText, HB_SIZE nLen );

extern HB_EXPORT int hb_storstr( void * cdp, const char * szText, int iParam );
extern HB_EXPORT int hb_storstr_utf8( const char * szText, int iParam );
extern HB_EXPORT int hb_storstr_u16( int iEndian, const HB_WCHAR * szText, int iParam );

extern HB_EXPORT int hb_storstrlen( void * cdp, const char * szText, HB_SIZE nLen, int iParam );
extern HB_EXPORT int hb_storstrlen_utf8( const char * szText, HB_SIZE nLen, int iParam );
extern HB_EXPORT int hb_storstrlen_u16( int iEndian, const HB_WCHAR * szText, HB_SIZE nLen, int iParam );

HB_EXTERN_END

#endif /* HB_APISTR_H_ */
