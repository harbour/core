/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBFCDX RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2000-2002 Horacio Roldan <harbour_ar@yahoo.com.ar> (portions)
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


static LPCDXKEYINFO hb_cdxKeyNew( void );
static void hb_cdxKeyFree( LPCDXKEYINFO pKey );
static int hb_cdxKeyCompare( LPCDXKEYINFO pKey1, LPCDXKEYINFO pKey2, USHORT * EndPos, BOOL Exact );

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
static void hb_cdxTagPageLoad( LPCDXTAG pTag, LPPAGEINFO pPage, short noKeys );
static void hb_cdxTagKeyRead( LPCDXTAG pTag, BYTE bTypRead );
static void hb_cdxTagKeyAdd( LPCDXTAG pTag, LPCDXKEYINFO pKey );
static void hb_cdxTagPageStore( LPCDXTAG pTag, LPPAGEINFO PIK );
static void hb_cdxTagExtNodeWrite( LPCDXTAG pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK );
static USHORT hb_cdxTagFillExternalNode( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO PIK,
                                         USHORT kcnt, USHORT ck, LPCDXKEYINFO * p );
static void hb_cdxTagExtNodeBuild( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO pPage, short noKeys );
static void hb_cdxTagTagLoad( LPCDXTAG pTag );
static void hb_cdxTagSetRoot( LPCDXTAG pTag, LPPAGEINFO PIK );
static void hb_cdxTagIntNodeWrite( LPCDXTAG pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK );
static USHORT hb_cdxTagFillInternalNode( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO PIK,
                                         USHORT kcnt, USHORT ck, LPCDXKEYINFO * p );
static void hb_cdxTagIntNodeBuild( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO pPage );
static LONG hb_cdxTagKeyFind( LPCDXTAG pTag, LPCDXKEYINFO pKey );

static LPPAGEINFO hb_cdxPageNew( LPCDXTAG PIT, LPPAGEINFO PIK, LONG FilePosn );
static void hb_cdxPageFree( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadTopKey( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadBottomKey( LPPAGEINFO pPage );
static int hb_cdxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPCDXKEYINFO pKey, BOOL bExact );
static void hb_cdxPageInsertKey( LPPAGEINFO pPage, LPCDXKEYINFO pKey, BOOL bAddAfter );
static void hb_cdxPagePageStore( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadNextKey( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadPrevKey( LPPAGEINFO pPage );
static LPCDXKEYINFO hb_cdxPageGetKey( LPPAGEINFO pPage, USHORT uiKey );
static void hb_cdxPagePageLoad( LPPAGEINFO pPage );
static int hb_cdxPageRetrieveKey( LPPAGEINFO pPage, LPCDXKEYINFO *pKey );
static void hb_cdxPageAddNodeKey( LPPAGEINFO pPage, LPCDXKEYINFO pKey );
static int hb_cdxPageSeekNodeTag( LPPAGEINFO pPage, LONG Tag );
static BOOL hb_cdxPageGetChild( LPPAGEINFO pPage, LONG Tag );
static void hb_cdxPageDeleteKey( LPPAGEINFO pPage );
static void hb_cdxPageReplaceNodeKey( LPPAGEINFO pPage, LPCDXKEYINFO pKey );
static void hb_cdxPageDeleteNodeKey( LPPAGEINFO pPage );

static LPCDXINDEX hb_cdxIndexNew( AREAP pArea );
static void hb_cdxIndexFree( LPCDXINDEX pIndex );
static LONG hb_cdxIndexGetAvailPage( LPCDXINDEX pIndex );
static void hb_cdxIndexResetAvailPage( LPCDXINDEX pIndex );
static void hb_cdxIndexPageRead( LPCDXINDEX pIndex, LONG lPos, void * pBuffer, USHORT uiSize );
static void hb_cdxIndexPageWrite( LPCDXINDEX pIndex, LONG lPos, void * pBuffer, USHORT uiSize );
static LPCDXTAG hb_cdxIndexAddTag( LPCDXINDEX pIndex, char * szTagName, char * szKeyExp,
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
                                 LPCDXKEYINFO Value );
static void hb_cdxSortAddExternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPCDXKEYINFO Value );
static void hb_cdxSortAddInternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPCDXKEYINFO Value );

/* static LPCDXTAG hb_cdxGetActiveTag( LPCDXINDEX PIF ); */
static LPCDXTAG hb_cdxGetActiveTag( CDXAREAP pArea );
static USHORT hb_cdxFindTag( CDXAREAP pArea, LPDBORDERINFO pOrderInfo );
static LPCDXTAG hb_cdxGetTagByNumber(CDXAREAP pArea,  USHORT uiTag );
static USHORT hb_cdxGetTagNumber(CDXAREAP pArea, LPCDXTAG pFindTag);
static PHB_ITEM hb_cdxKeyGetItem( LPCDXKEYINFO pKey, PHB_ITEM pItem, USHORT uiType );
static LPCDXTAG hb_cdxReorderTagList ( LPCDXTAG TagList );
static ERRCODE hb_cdxGoEof( CDXAREAP pArea );
static BOOL hb_cdxTopScope( LPCDXTAG pTag, LPCDXKEYINFO pKey );
static BOOL hb_cdxBottomScope( LPCDXTAG pTag, LPCDXKEYINFO pKey );
static void hb_cdxTagClearScope( LPCDXTAG pTag, USHORT nScope );


static USHORT hb_cdxIndexCheckVersion( LPCDXINDEX pIndex );
static USHORT hb_cdxIndexUnLockRead( LPCDXINDEX pIndex, LPCDXTAG pTag );
static USHORT hb_cdxIndexLockRead( LPCDXINDEX pIndex, LPCDXTAG pTag );
static USHORT hb_cdxIndexLockWrite ( LPCDXINDEX pIndex, LPCDXTAG pTag );
static USHORT hb_cdxIndexUnLockWrite ( LPCDXINDEX pIndex, LPCDXTAG pTag );
static void hb_cdxIndexDelTag( LPCDXINDEX pIndex, char * szTagName );

static int hb_cdxKeyValCompare( LPCDXTAG pTag, char * pKeyVal1, BYTE keyLen1,
      char * pKeyVal2, BYTE keyLen2, USHORT * pEndPos, BOOL Exact );
static void hb_cdxMacroRun( AREAP pArea, HB_MACRO_PTR pMacro );
static ERRCODE cdxError( CDXAREAP pArea, USHORT uiGenCode, USHORT uiSubCode, char * filename, USHORT uiFlags );
static void hb_cdxIndexReindex( LPCDXINDEX pIndex );
static ERRCODE hb_cdxOrdListClear( CDXAREAP pArea, int iComplete );

