#include <extend.h>

typedef struct
{
   WORD   type;
   WORD   paramCount;
   PITEM  pItems[ 10 ];
} EVALINFO, * PEVALINFO;

PITEM _evalLaunch( PEVALINFO pEvalInfo );
BOOL _evalNew( PEVALINFO pEvalInfo, PITEM pItem );
BOOL _evalPutParam( PEVALINFO pEvalInfo, PITEM pItem );
BOOL _evalRelease( PEVALINFO pEvalInfo );

PITEM _itemArrayGet( PITEM pArray, ULONG ulIndex );
PITEM _itemArrayNew( ULONG ulLen );
PITEM _itemArrayPut( PITEM pArray, ULONG ulIndex, PITEM pItem );
ULONG _itemCopyC( PITEM pItem, char *szBuffer, ULONG ulLen );
BOOL _itemFreeC( char *szText );
char * _itemGetC( PITEM pItem );
char *_itemGetDS( PITEM pItem, char *szDate );
BOOL _itemGetL( PITEM pItem );
double _itemGetND( PITEM pItem );
long _itemGetNL( PITEM pItem );
PITEM _itemNew( PITEM pNull );
PITEM _itemParam( WORD wParam );
PITEM _itemPutC( PITEM pItem, char *szText );
PITEM _itemPutCL( PITEM pItem, char *nszText, ULONG ulLen );
PITEM _itemPutDS( PITEM pItem, char *szDate );
PITEM _itemPutL( PITEM pItem, BOOL bValue );
PITEM _itemPutND( PITEM pItem, double dNumber );
PITEM _itemPutNL( PITEM pItem, long lNumber );
BOOL  _itemRelease( PITEM pItem );
PITEM _itemReturn( PITEM pItem );
ULONG _itemSize( PITEM pItem );
WORD _itemType( PITEM pItem );

