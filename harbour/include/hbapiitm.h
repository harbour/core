/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Item API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_APIITM_H_
#define HB_APIITM_H_

#include "hbapi.h"

#define HB_EVAL_PARAM_MAX_ 9

typedef struct
{
   USHORT   paramCount;
   PHB_ITEM pItems[ HB_EVAL_PARAM_MAX_ + 1 ];
} EVALINFO, * PEVALINFO, * EVALINFO_PTR;

extern PHB_ITEM hb_evalLaunch   ( PEVALINFO pEvalInfo );
extern BOOL     hb_evalNew      ( PEVALINFO pEvalInfo, PHB_ITEM pItem );
extern BOOL     hb_evalPutParam ( PEVALINFO pEvalInfo, PHB_ITEM pItem );
extern BOOL     hb_evalRelease  ( PEVALINFO pEvalInfo );

extern PHB_ITEM hb_itemDo       ( PHB_ITEM pItem, USHORT uiPCount, PHB_ITEM pItemArg1, ... );
extern PHB_ITEM hb_itemDoC      ( char * szFunc, USHORT uiPCount, PHB_ITEM pItemArg1, ... );

extern PHB_ITEM hb_itemArrayGet ( PHB_ITEM pArray, ULONG ulIndex );
extern PHB_ITEM hb_itemArrayNew ( ULONG ulLen );
extern PHB_ITEM hb_itemArrayPut ( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem );
extern ULONG    hb_itemCopyC    ( PHB_ITEM pItem, char * szBuffer, ULONG ulLen );
extern BOOL     hb_itemFreeC    ( char * szText );
extern char *   hb_itemGetC     ( PHB_ITEM pItem );
extern char *   hb_itemGetCPtr  ( PHB_ITEM pItem );
extern ULONG    hb_itemGetCLen  ( PHB_ITEM pItem );
extern char *   hb_itemGetDS    ( PHB_ITEM pItem, char * szDate );
extern long     hb_itemGetDL    ( PHB_ITEM pItem );
extern BOOL     hb_itemGetL     ( PHB_ITEM pItem );
extern double   hb_itemGetND    ( PHB_ITEM pItem );
extern int      hb_itemGetNI    ( PHB_ITEM pItem );
extern long     hb_itemGetNL    ( PHB_ITEM pItem );
extern void     hb_itemGetNLen  ( PHB_ITEM pItem, int * piWidth, int * piDec );
extern void *   hb_itemGetPtr   ( PHB_ITEM pItem );
extern PHB_ITEM hb_itemNew      ( PHB_ITEM pNull );
extern USHORT   hb_itemPCount   ( void );
extern PHB_ITEM hb_itemParam    ( USHORT uiParam );
extern PHB_ITEM hb_itemPutC     ( PHB_ITEM pItem, char * szText );
extern PHB_ITEM hb_itemPutCPtr  ( PHB_ITEM pItem, char * szText, ULONG ulLen );
extern PHB_ITEM hb_itemPutCL    ( PHB_ITEM pItem, char * szText, ULONG ulLen );
extern PHB_ITEM hb_itemPutDS    ( PHB_ITEM pItem, char * szDate );
extern PHB_ITEM hb_itemPutDL    ( PHB_ITEM pItem, long lJulian );
extern PHB_ITEM hb_itemPutL     ( PHB_ITEM pItem, BOOL bValue );
extern PHB_ITEM hb_itemPutND    ( PHB_ITEM pItem, double dNumber );
extern PHB_ITEM hb_itemPutNI    ( PHB_ITEM pItem, int iNumber );
extern PHB_ITEM hb_itemPutNL    ( PHB_ITEM pItem, long lNumber );
extern PHB_ITEM hb_itemPutNLen  ( PHB_ITEM pItem, double dNumber, int iWidth, int iDec );
extern PHB_ITEM hb_itemPutNDLen ( PHB_ITEM pItem, double dNumber, int iWidth, int iDec );
extern PHB_ITEM hb_itemPutNILen ( PHB_ITEM pItem, int iNumber, int iWidth );
extern PHB_ITEM hb_itemPutNLLen ( PHB_ITEM pItem, long lNumber, int iWidth );
extern PHB_ITEM hb_itemPutPtr   ( PHB_ITEM pItem, void * pValue );
extern BOOL     hb_itemRelease  ( PHB_ITEM pItem );
extern PHB_ITEM hb_itemReturn   ( PHB_ITEM pItem );
extern ULONG    hb_itemSize     ( PHB_ITEM pItem );
extern USHORT   hb_itemType     ( PHB_ITEM pItem );

/* Non Clipper compliant internal API */

extern PHB_ITEM hb_itemParamPtr ( USHORT uiParam, int iMask );
extern PHB_ITEM hb_itemReturnPtr( void );
extern int      hb_itemStrCmp   ( PHB_ITEM pFirst, PHB_ITEM pSecond, BOOL bForceExact ); /* our string compare */
extern void     hb_itemCopy     ( PHB_ITEM pDest, PHB_ITEM pSource ); /* copies an item to one place to another respecting its containts */
extern void     hb_itemClear    ( PHB_ITEM pItem );
extern PHB_ITEM hb_itemUnRef    ( PHB_ITEM pItem ); /* de-references passed variable */
extern char *   hb_itemStr      ( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec ); /* convert a number to a string */
extern char *   hb_itemString   ( PHB_ITEM pItem, ULONG * ulLen, BOOL * bFreeReq );  /* Convert any scalar to a string */
extern PHB_ITEM hb_itemValToStr ( PHB_ITEM pItem ); /* Convert any scalar to a string */

#endif /* HB_APIITM_H_ */
