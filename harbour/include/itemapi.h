/*
 * $Id$
 */

#include <extend.h>

typedef struct
{
   WORD   type;
   WORD   paramCount;
   PITEM  pItems[ 10 ];
} EVALINFO, * PEVALINFO;

PITEM hb_evalLaunch( PEVALINFO pEvalInfo );
BOOL hb_evalNew( PEVALINFO pEvalInfo, PITEM pItem );
BOOL hb_evalPutParam( PEVALINFO pEvalInfo, PITEM pItem );
BOOL hb_evalRelease( PEVALINFO pEvalInfo );

PITEM hb_itemArrayGet( PITEM pArray, ULONG ulIndex );
PITEM hb_itemArrayNew( ULONG ulLen );
PITEM hb_itemArrayPut( PITEM pArray, ULONG ulIndex, PITEM pItem );
ULONG hb_itemCopyC( PITEM pItem, char *szBuffer, ULONG ulLen );
BOOL hb_itemFreeC( char *szText );
char * hb_itemGetC( PITEM pItem );
char *hb_itemGetDS( PITEM pItem, char *szDate );
BOOL hb_itemGetL( PITEM pItem );
double hb_itemGetND( PITEM pItem );
long hb_itemGetNL( PITEM pItem );
PITEM hb_itemNew( PITEM pNull );
PITEM hb_itemParam( WORD wParam );
PITEM hb_itemPutC( PITEM pItem, char *szText );
PITEM hb_itemPutCL( PITEM pItem, char *nszText, ULONG ulLen );
PITEM hb_itemPutDS( PITEM pItem, char *szDate );
PITEM hb_itemPutL( PITEM pItem, BOOL bValue );
PITEM hb_itemPutND( PITEM pItem, double dNumber );
PITEM hb_itemPutNL( PITEM pItem, long lNumber );
BOOL  hb_itemRelease( PITEM pItem );
PITEM hb_itemReturn( PITEM pItem );
ULONG hb_itemSize( PITEM pItem );
WORD hb_itemType( PITEM pItem );

