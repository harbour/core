/*
 * $Id$
 */

#ifndef ITEMAPI_H_
#define ITEMAPI_H_

#include <extend.h>

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

#endif  /* ITEMAPI_H_ */
