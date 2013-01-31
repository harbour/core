/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Item API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#ifndef HB_APIITM_H_
#define HB_APIITM_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

#define HB_EVAL_PARAM_MAX_ 9

typedef struct
{
   HB_USHORT paramCount;
   PHB_ITEM pItems[ HB_EVAL_PARAM_MAX_ + 1 ];
} HB_EVALINFO, * PHB_EVALINFO;

extern HB_EXPORT PHB_ITEM     hb_evalLaunch    ( PHB_EVALINFO pEvalInfo );
extern HB_EXPORT HB_BOOL      hb_evalNew       ( PHB_EVALINFO pEvalInfo, PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_evalPutParam  ( PHB_EVALINFO pEvalInfo, PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_evalRelease   ( PHB_EVALINFO pEvalInfo );

extern HB_EXPORT void         hb_evalBlock( PHB_ITEM pCodeBlock, ... );
extern HB_EXPORT void         hb_evalBlock0( PHB_ITEM pCodeBlock );
extern HB_EXPORT void         hb_evalBlock1( PHB_ITEM pCodeBlock, PHB_ITEM pParam );

extern HB_EXPORT HB_BOOL      hb_execFromArray ( PHB_ITEM pParam );

extern HB_EXPORT PHB_ITEM     hb_itemDo        ( PHB_ITEM pItem, HB_ULONG ulPCount, ... );
extern HB_EXPORT PHB_ITEM     hb_itemDoC       ( const char * szFunc, HB_ULONG ulPCount, ... );

extern HB_EXPORT PHB_ITEM     hb_itemArrayGet  ( PHB_ITEM pArray, HB_SIZE nIndex );
extern HB_EXPORT PHB_ITEM     hb_itemArrayNew  ( HB_SIZE nLen );
extern HB_EXPORT PHB_ITEM     hb_itemArrayPut  ( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem );
extern HB_EXPORT HB_SIZE      hb_itemCopyC     ( PHB_ITEM pItem, char * szBuffer, HB_SIZE nLen );
extern HB_EXPORT HB_BOOL      hb_itemFreeC     ( char * szText );
extern HB_EXPORT char *       hb_itemGetC      ( PHB_ITEM pItem );
extern HB_EXPORT const char * hb_itemGetCPtr   ( PHB_ITEM pItem );
extern HB_EXPORT HB_SIZE      hb_itemGetCLen   ( PHB_ITEM pItem );
extern HB_EXPORT char *       hb_itemGetDS     ( PHB_ITEM pItem, char * szDate );
extern HB_EXPORT char *       hb_itemGetTS     ( PHB_ITEM pItem, char * szDateTime );
extern HB_EXPORT long         hb_itemGetDL     ( PHB_ITEM pItem );
extern HB_EXPORT double       hb_itemGetTD     ( PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_itemGetTDT    ( PHB_ITEM pItem, long * plJulian, long * plMilliSec );
extern HB_EXPORT HB_BOOL      hb_itemGetL      ( PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_itemGetLX     ( PHB_ITEM pItem );
extern HB_EXPORT double       hb_itemGetND     ( PHB_ITEM pItem );
extern HB_EXPORT double       hb_itemGetNDDec  ( PHB_ITEM pItem, int * piDec );
extern HB_EXPORT int          hb_itemGetNI     ( PHB_ITEM pItem );
extern HB_EXPORT long         hb_itemGetNL     ( PHB_ITEM pItem );
extern HB_EXPORT HB_ISIZ      hb_itemGetNS     ( PHB_ITEM pItem );
extern HB_EXPORT HB_MAXINT    hb_itemGetNInt   ( PHB_ITEM pItem );
extern HB_EXPORT void         hb_itemGetNLen   ( PHB_ITEM pItem, int * piWidth, int * piDec );
extern HB_EXPORT void *       hb_itemGetPtr    ( PHB_ITEM pItem );
extern HB_EXPORT void *       hb_itemGetPtrGC  ( PHB_ITEM pItem, const HB_GC_FUNCS * pFuncs );
extern HB_EXPORT PHB_SYMB     hb_itemGetSymbol ( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_itemNew       ( PHB_ITEM pNull );
extern HB_EXPORT void         hb_itemInit      ( PHB_ITEM pItem );
extern HB_EXPORT HB_USHORT    hb_itemPCount    ( void );
extern HB_EXPORT PHB_ITEM     hb_itemParam     ( HB_USHORT uiParam );
extern HB_EXPORT PHB_ITEM     hb_itemPutC      ( PHB_ITEM pItem, const char * szText );
extern HB_EXPORT PHB_ITEM     hb_itemPutCL     ( PHB_ITEM pItem, const char * szText, HB_SIZE nLen );
extern HB_EXPORT PHB_ITEM     hb_itemPutCConst ( PHB_ITEM pItem, const char * szText );
extern HB_EXPORT PHB_ITEM     hb_itemPutCLConst( PHB_ITEM pItem, const char * szText, HB_SIZE nLen );
extern HB_EXPORT PHB_ITEM     hb_itemPutCPtr   ( PHB_ITEM pItem, char * szText );
extern HB_EXPORT PHB_ITEM     hb_itemPutCLPtr  ( PHB_ITEM pItem, char * szText, HB_SIZE nLen );
extern HB_EXPORT void         hb_itemSetCMemo  ( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_itemPutD      ( PHB_ITEM pItem, int iYear, int iMonth, int iDay );
extern HB_EXPORT PHB_ITEM     hb_itemPutDS     ( PHB_ITEM pItem, const char * szDate );
extern HB_EXPORT PHB_ITEM     hb_itemPutTS     ( PHB_ITEM pItem, const char * szDateTime );
extern HB_EXPORT PHB_ITEM     hb_itemPutDL     ( PHB_ITEM pItem, long lJulian );
extern HB_EXPORT PHB_ITEM     hb_itemPutTD     ( PHB_ITEM pItem, double dTimeStamp );
extern HB_EXPORT PHB_ITEM     hb_itemPutTDT    ( PHB_ITEM pItem, long lJulian, long lMilliSec );
extern HB_EXPORT PHB_ITEM     hb_itemPutL      ( PHB_ITEM pItem, HB_BOOL bValue );
extern HB_EXPORT PHB_ITEM     hb_itemPutND     ( PHB_ITEM pItem, double dNumber );
extern HB_EXPORT PHB_ITEM     hb_itemPutNI     ( PHB_ITEM pItem, int iNumber );
extern HB_EXPORT PHB_ITEM     hb_itemPutNL     ( PHB_ITEM pItem, long lNumber );
extern HB_EXPORT PHB_ITEM     hb_itemPutNS     ( PHB_ITEM pItem, HB_ISIZ nNumber );
extern HB_EXPORT PHB_ITEM     hb_itemPutNInt   ( PHB_ITEM pItem, HB_MAXINT nNumber );
extern HB_EXPORT PHB_ITEM     hb_itemPutNIntLen( PHB_ITEM pItem, HB_MAXINT nNumber, int iWidth );
extern HB_EXPORT PHB_ITEM     hb_itemPutNLen   ( PHB_ITEM pItem, double dNumber, int iWidth, int iDec );
extern HB_EXPORT PHB_ITEM     hb_itemPutNDLen  ( PHB_ITEM pItem, double dNumber, int iWidth, int iDec );
extern HB_EXPORT PHB_ITEM     hb_itemPutNDDec  ( PHB_ITEM pItem, double dNumber, int iDec );
extern HB_EXPORT PHB_ITEM     hb_itemPutNILen  ( PHB_ITEM pItem, int iNumber, int iWidth );
extern HB_EXPORT PHB_ITEM     hb_itemPutNLLen  ( PHB_ITEM pItem, long lNumber, int iWidth );
extern HB_EXPORT PHB_ITEM     hb_itemPutNumType( PHB_ITEM pItem, double dNumber, int iDec, int iType1, int iType2 );
extern HB_EXPORT PHB_ITEM     hb_itemPutPtr    ( PHB_ITEM pItem, void * pValue );
extern HB_EXPORT PHB_ITEM     hb_itemPutPtrGC  ( PHB_ITEM pItem, void * pValue );
extern HB_EXPORT PHB_ITEM     hb_itemPutSymbol ( PHB_ITEM pItem, PHB_SYMB pSym );
extern HB_EXPORT HB_BOOL      hb_itemRelease   ( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_itemReturn    ( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_itemReturnForward( PHB_ITEM pItem );
extern HB_EXPORT void         hb_itemReturnRelease( PHB_ITEM pItem );
extern HB_EXPORT HB_SIZE      hb_itemSize      ( PHB_ITEM pItem );
extern HB_EXPORT HB_TYPE      hb_itemType      ( PHB_ITEM pItem );
extern HB_EXPORT const char * hb_itemTypeStr ( PHB_ITEM pItem );
#ifndef HB_LONG_LONG_OFF
extern HB_EXPORT HB_LONGLONG  hb_itemGetNLL    ( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_itemPutNLL    ( PHB_ITEM pItem, HB_LONGLONG lNumber );
extern HB_EXPORT PHB_ITEM     hb_itemPutNLLLen ( PHB_ITEM pItem, HB_LONGLONG lNumber, int iWidth );
#endif

/* Non Clipper compliant internal API */

extern HB_EXPORT PHB_ITEM     hb_itemParamPtr  ( HB_USHORT uiParam, long lMask );
extern HB_EXPORT HB_BOOL      hb_itemParamStore( HB_USHORT uiParam, PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_itemParamStoreForward( HB_USHORT uiParam, PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_itemParamStoreRelease( HB_USHORT uiParam, PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL      hb_itemEqual( PHB_ITEM pItem1, PHB_ITEM pItem2 );
extern HB_EXPORT int          hb_itemStrCmp    ( PHB_ITEM pFirst, PHB_ITEM pSecond, HB_BOOL bForceExact ); /* our string compare */
extern HB_EXPORT int          hb_itemStrICmp   ( PHB_ITEM pFirst, PHB_ITEM pSecond, HB_BOOL bForceExact ); /* our string compare */
extern HB_EXPORT void         hb_itemCopy      ( PHB_ITEM pDest, PHB_ITEM pSource ); /* copies an item to one place to another respecting its containts */
extern HB_EXPORT void         hb_itemCopyToRef ( PHB_ITEM pDest, PHB_ITEM pSource );
extern HB_EXPORT void         hb_itemCopyFromRef( PHB_ITEM pDest, PHB_ITEM pSource );
extern HB_EXPORT void         hb_itemMove      ( PHB_ITEM pDest, PHB_ITEM pSource ); /* moves the value of an item without incrementing of reference counters, source is cleared */
extern HB_EXPORT void         hb_itemMoveRef   ( PHB_ITEM pDest, PHB_ITEM pSource );
extern HB_EXPORT void         hb_itemMoveToRef ( PHB_ITEM pDest, PHB_ITEM pSource );
extern HB_EXPORT void         hb_itemMoveFromRef( PHB_ITEM pDest, PHB_ITEM pSource );
extern HB_EXPORT void         hb_itemClear     ( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_itemUnRef     ( PHB_ITEM pItem ); /* de-references passed variable */
extern HB_EXPORT PHB_ITEM     hb_itemUnRefOnce ( PHB_ITEM pItem ); /* de-references passed variable, one step*/
extern HB_EXPORT PHB_ITEM     hb_itemUnRefRefer( PHB_ITEM pItem ); /* de-references passed variable, leaving the last reference */
extern HB_EXPORT PHB_ITEM     hb_itemUnRefWrite( PHB_ITEM pItem, PHB_ITEM pSource ); /* de-references passed variable for writing */
extern HB_EXPORT PHB_ITEM     hb_itemUnShare   ( PHB_ITEM pItem ); /* un-share given string item */
extern HB_EXPORT PHB_ITEM     hb_itemUnShareString( PHB_ITEM pItem ); /* un-share given string item - the pItem have to be valid unrefed string item */
extern HB_EXPORT PHB_ITEM     hb_itemReSizeString( PHB_ITEM pItem, HB_SIZE nSize ); /* Resize string buffer of given string item - the pItem have to be valid unrefed string item */
extern HB_EXPORT HB_BOOL      hb_itemGetWriteCL( PHB_ITEM pItem, char ** pszValue, HB_SIZE * pnLen );
extern HB_EXPORT PHB_ITEM     hb_itemClone     ( PHB_ITEM pItem ); /* clone the given item */
extern HB_EXPORT void         hb_itemCloneTo   ( PHB_ITEM pDest, PHB_ITEM pSource ); /* clone the given item */
extern HB_EXPORT char *       hb_itemStr       ( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec ); /* convert a number to a string */
extern HB_EXPORT char *       hb_itemString    ( PHB_ITEM pItem, HB_SIZE * nLen, HB_BOOL * bFreeReq );  /* Convert any scalar to a string */
extern HB_EXPORT HB_BOOL      hb_itemStrBuf    ( char *szResult, PHB_ITEM pNumber, int iSize, int iDec ); /* convert a number to a string */
extern HB_EXPORT PHB_ITEM     hb_itemValToStr  ( PHB_ITEM pItem ); /* Convert any scalar to a string */
extern HB_EXPORT char *       hb_itemPadConv   ( PHB_ITEM pItem, HB_SIZE * pnSize, HB_BOOL * bFreeReq );
extern HB_EXPORT void         hb_itemSwap      ( PHB_ITEM pItem1, PHB_ITEM pItem2 );

extern HB_EXPORT char *       hb_itemSerialize( PHB_ITEM pItem, HB_BOOL fNumSize, HB_SIZE * pnSize );
extern HB_EXPORT PHB_ITEM     hb_itemDeserialize( const char ** pBufferPtr, HB_SIZE * pnSize );

#if defined( _HB_API_INTERNAL_ )

extern PHB_ITEM hb_itemPutPtrRawGC( PHB_ITEM pItem, void * pValue );

#  define hb_itemSetNil( item )           do { \
                                             if( HB_IS_COMPLEX( item ) ) \
                                                hb_itemClear( item ); \
                                             else \
                                                (item)->type = HB_IT_NIL; \
                                          } while( 0 )

#  define hb_itemRawCpy( dst, src )       do { *(dst) = *(src); } while( 0 )

#if 1
#  define hb_itemRawMove( dst, src )      do { \
                                             hb_itemRawCpy( dst, src ); \
                                             (src)->type = HB_IT_NIL; \
                                          } while( 0 )
#else /* _HB_API_INTERNAL_ */
#  define hb_itemRawMove( dst, src )      hb_itemMove( (dst), (src) )
#endif

   /* intentional low level hack to eliminate race condition in
    * unprotected readonly access in few places in core code only.
    * hb_item[Raw]Move() moves HB_ITEM structure members first coping
    * 'type' and then 'item' parts of HB_ITEM. In this macro the order
    * is reverted. [druzus]
    */
#  define hb_itemSafeMove( dst, src )  do { \
                                             (dst)->item = (src)->item; \
                                             (dst)->type = (src)->type; \
                                             (src)->type = HB_IT_NIL; \
                                          } while( 0 )

#else

#  define hb_itemSetNil( item )           hb_itemClear( (item) )

#  define hb_itemRawMove( dst, src )      hb_itemMove( (dst), (src) )

#endif /* _HB_API_INTERNAL_ */

HB_EXTERN_END

#endif /* HB_APIITM_H_ */
