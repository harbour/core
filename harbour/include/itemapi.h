/*
 * $Id$
 */

/*
   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_ITEMAPI_H_
#define HB_ITEMAPI_H_

#include "extend.h"

typedef struct
{
   WORD     type;
   WORD     paramCount;
   PHB_ITEM pItems[ 10 ];
} EVALINFO, * PEVALINFO;

#define ITEM PHB_ITEM

PHB_ITEM hb_evalLaunch( PEVALINFO pEvalInfo );
BOOL     hb_evalNew( PEVALINFO pEvalInfo, PHB_ITEM pItem );
BOOL     hb_evalPutParam( PEVALINFO pEvalInfo, PHB_ITEM pItem );
BOOL     hb_evalRelease( PEVALINFO pEvalInfo );

PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, ULONG ulIndex );
PHB_ITEM hb_itemArrayNew( ULONG ulLen );
PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem );
ULONG    hb_itemCopyC( PHB_ITEM pItem, char *szBuffer, ULONG ulLen );
BOOL     hb_itemFreeC( char *szText );
char *   hb_itemGetC( PHB_ITEM pItem );
char *   hb_itemGetDS( PHB_ITEM pItem, char *szDate );
BOOL     hb_itemGetL( PHB_ITEM pItem );
double   hb_itemGetND( PHB_ITEM pItem );
long     hb_itemGetNL( PHB_ITEM pItem );
PHB_ITEM hb_itemNew( PHB_ITEM pNull );
PHB_ITEM hb_itemParam( WORD wParam );
PHB_ITEM hb_itemPutC( PHB_ITEM pItem, char *szText );
PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, char *nszText, ULONG ulLen );
PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, char *szDate );
PHB_ITEM hb_itemPutL( PHB_ITEM pItem, BOOL bValue );
PHB_ITEM hb_itemPutND( PHB_ITEM pItem, double dNumber );
PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, long lNumber );
BOOL     hb_itemRelease( PHB_ITEM pItem );
PHB_ITEM hb_itemReturn( PHB_ITEM pItem );
ULONG    hb_itemSize( PHB_ITEM pItem );
WORD     hb_itemType( PHB_ITEM pItem );

#endif /* HB_ITEMAPI_H_ */
