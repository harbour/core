
static LPKEYINFO hb_cdxKeyNew( void );
static void hb_cdxKeyFree( LPKEYINFO pKey );
static int hb_cdxKeyCompare( LPKEYINFO pKey1, LPKEYINFO pKey2, USHORT * EndPos, BOOL Exact );

static LPCDXTAG hb_cdxTagNew( LPCDXINDEX PIF, char * ITN, LONG TagHdr );
static void hb_cdxTagFree( LPCDXTAG pTag );
static void hb_cdxTagIndexTagNew( LPCDXTAG pTag, char * KeyExp, PHB_ITEM pKeyItem,
                                  BYTE bType, USHORT uiLen, char * ForExp,
                                  PHB_ITEM pForItem, BOOL Ascnd, BOOL Uniq );
static void hb_cdxTagDoIndex( LPCDXTAG pTag );
static void hb_cdxTagEmptyIndex( LPCDXTAG pTag );
static void hb_cdxTagTagStore( LPCDXTAG pTag );
static void hb_cdxTagTagOpen( LPCDXTAG pTag, BYTE bCode );
static void hb_cdxTagTagClose( LPCDXTAG pTag );
static LONG hb_cdxTagNewRoot( LPCDXTAG pTag );
static void hb_cdxTagPageLoad( LPCDXTAG pTag, LPPAGEINFO pPage );
static void hb_cdxTagKeyRead( LPCDXTAG pTag, BYTE bTypRead );
static void hb_cdxTagKeyAdd( LPCDXTAG pTag, LPKEYINFO pKey );
static void hb_cdxTagPageStore( LPCDXTAG pTag, LPPAGEINFO PIK );
static void hb_cdxTagExtNodeWrite( LPCDXTAG pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK );
static USHORT hb_cdxTagFillExternalNode( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO PIK,
                                         USHORT kcnt, USHORT ck, LPKEYINFO * p );
static void hb_cdxTagExtNodeBuild( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO pPage );
static void hb_cdxTagTagLoad( LPCDXTAG pTag );
static void hb_cdxTagSetRoot( LPCDXTAG pTag, LPPAGEINFO PIK );
static void hb_cdxTagIntNodeWrite( LPCDXTAG pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK );
static USHORT hb_cdxTagFillInternalNode( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO PIK,
                                         USHORT kcnt, USHORT ck, LPKEYINFO * p );
static void hb_cdxTagIntNodeBuild( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO pPage );
static LONG hb_cdxTagKeyFind( LPCDXTAG pTag, LPKEYINFO pKey );

static LPPAGEINFO hb_cdxPageNew( LPCDXTAG PIT, LPPAGEINFO PIK, LONG FilePosn );
static void hb_cdxPageFree( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadTopKey( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadBottomKey( LPPAGEINFO pPage );
static int hb_cdxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact );
static void hb_cdxPageInsertKey( LPPAGEINFO pPage, LPKEYINFO pKey, BOOL bAddAfter );
static void hb_cdxPagePageStore( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadNextKey( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadPrevKey( LPPAGEINFO pPage );
static LPKEYINFO hb_cdxPageGetKey( LPPAGEINFO pPage, USHORT uiKey );
static void hb_cdxPagePageLoad( LPPAGEINFO pPage );
static int hb_cdxPageRetrieveKey( LPPAGEINFO pPage, LPKEYINFO pKey );
static void hb_cdxPageAddNodeKey( LPPAGEINFO pPage, LPKEYINFO pKey );
static int hb_cdxPageSeekNodeTag( LPPAGEINFO pPage, LONG Tag );
static BOOL hb_cdxPageGetChild( LPPAGEINFO pPage, LONG Tag );
static void hb_cdxPageDeleteKey( LPPAGEINFO pPage );
static void hb_cdxPageReplaceNodeKey( LPPAGEINFO pPage, LPKEYINFO pKey );
static void hb_cdxPageDeleteNodeKey( LPPAGEINFO pPage );

static LPCDXINDEX hb_cdxIndexNew( AREAP pArea );
static void hb_cdxIndexFree( LPCDXINDEX pIndex );
static LONG hb_cdxIndexGetAvailPage( LPCDXINDEX pIndex );
static void hb_cdxIndexResetAvailPage( LPCDXINDEX pIndex );
static void hb_cdxIndexPageRead( LPCDXINDEX pIndex, LONG lPos, void * pBuffer, USHORT uiSize );
static void hb_cdxIndexPageWrite( LPCDXINDEX pIndex, LONG lPos, void * pBuffer, USHORT uiSize );
static void hb_cdxIndexAddTag( LPCDXINDEX pIndex, char * szTagName, char * szKeyExp,
                               PHB_ITEM pKeyItem, BYTE bType, USHORT uiLen, char * szForExp,
                               PHB_ITEM pForItem, BOOL bAscending, BOOL bUnique );

static LPSORTINFO hb_cdxSortNew( LPCDXTAG pTag, BOOL bUnique );
static void hb_cdxSortFree( LPSORTINFO pSort );
static void hb_cdxSortLinkNew( LPSORTINFO pSort, LONG * NewLink );
static void hb_cdxSortGetNewChunk( LPSORTINFO pSort );
static void hb_cdxSortInsertWord( LPSORTINFO pSort, LONG Tag, char * Value,
                                  USHORT uiLen );
static void hb_cdxSortStuffKey( LPSORTINFO pSort, LPSORTDATA * wx, BOOL fTag );
static void hb_cdxSortGetNode( LPSORTINFO pSort, BYTE Character,
                               LONG * NewLink, BOOL fTag );
static LPSORTDATA hb_cdxSortLinkGet( LPSORTINFO pSort, LONG Value );
static void hb_cdxSortDisplayWord( LPSORTINFO pSort );
static void hb_cdxSortRecurseDict( LPSORTINFO pSort, LONG WPtr, LONG WBgn );
static void hb_cdxSortSendWord( LPSORTINFO pSort, BYTE * Value );
static void hb_cdxSortOutputWord( LPSORTINFO pSort, LONG Tag, BYTE * Value,
                                  USHORT uiLen );
static void hb_cdxSortAddToNode( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                 LPKEYINFO Value );
static void hb_cdxSortAddExternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPKEYINFO Value );
static void hb_cdxSortAddInternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPKEYINFO Value );
static LPCDXTAG hb_cdxGetActiveTag( LPCDXINDEX PIF );
static USHORT hb_cdxFindTag( CDXAREAP pArea, LPDBORDERINFO pOrderInfo );
