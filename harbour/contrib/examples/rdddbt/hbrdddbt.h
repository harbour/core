/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * DBFDBT RDD
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
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

#ifndef HB_RDDDBT_H_
#define HB_RDDDBT_H_

#include "hbdbf.h"
#include "hbdbferr.h"
#include "hbapirdd.h"
#define HB_EXTERNAL_RDDDBF_USE
#include "hbrdddbf.h"

HB_EXTERN_BEGIN

/* MEMO constants and defaults */
#define DBT_MEMOEXT                             ".dbt"
#define DBT_BLOCKSIZE                              512
#define DBT_LOCKPOS                        0x00000000L
#define DBT_LOCKSIZE                       0x00000001L

/*
 *  DBFDBT WORKAREA
 *  ------------
 *  The Workarea Structure of DBFDBT RDD
 *
 */

/* we don't have to change DBFAREA to create DBTAREA */
typedef DBFAREA DBTAREA;
typedef DBTAREA * LPDBTAREA;
#ifndef DBTAREAP
#define DBTAREAP LPDBTAREA
#endif


/*
 * -- DBFDBT METHODS --
 */

#define SUPERTABLE                         ( &dbtSuper )

#define hb_dbtBof                                  NULL
#define hb_dbtEof                                  NULL
#define hb_dbtFound                                NULL
#define hb_dbtGoBottom                             NULL
#define hb_dbtGoTo                                 NULL
#define hb_dbtGoToId                               NULL
#define hb_dbtGoTop                                NULL
#define hb_dbtSeek                                 NULL
#define hb_dbtSkip                                 NULL
#define hb_dbtSkipFilter                           NULL
#define hb_dbtSkipRaw                              NULL
#define hb_dbtAddField                             NULL
#define hb_dbtAppend                               NULL
#define hb_dbtCreateFields                         NULL
#define hb_dbtDeleteRec                            NULL
#define hb_dbtDeleted                              NULL
#define hb_dbtFieldCount                           NULL
#define hb_dbtFieldDisplay                         NULL
#define hb_dbtFieldInfo                            NULL
#define hb_dbtFieldName                            NULL
#define hb_dbtFlush                                NULL
#define hb_dbtGetRec                               NULL
static HB_ERRCODE hb_dbtGetValue( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE hb_dbtGetVarLen( DBTAREAP pArea, USHORT uiIndex, ULONG * pLength );
#define hb_dbtGoCold                               NULL
#define hb_dbtGoHot                                NULL
#define hb_dbtPutRec                               NULL
static HB_ERRCODE hb_dbtPutValue( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
#define hb_dbtRecall                               NULL
#define hb_dbtRecCount                             NULL
#define hb_dbtRecInfo                              NULL
#define hb_dbtRecNo                                NULL
#define hb_dbtRecId                                NULL
#define hb_dbtSetFieldExtent                       NULL
#define hb_dbtAlias                                NULL
#define hb_dbtClose                                NULL
#define hb_dbtCreate                               NULL
static HB_ERRCODE hb_dbtInfo( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
#define hb_dbtNewArea                              NULL
#define hb_dbtOpen                                 NULL
#define hb_dbtRelease                              NULL
static HB_ERRCODE hb_dbtStructSize( DBTAREAP pArea, USHORT * uiSize );
#define hb_dbtSysName                              NULL
#define hb_dbtEval                                 NULL
#define hb_dbtPack                                 NULL
#define hb_dbtPackRec                              NULL
#define hb_dbtSort                                 NULL
#define hb_dbtTrans                                NULL
#define hb_dbtTransRec                             NULL
#define hb_dbtZap                                  NULL
#define hb_dbtChildEnd                             NULL
#define hb_dbtChildStart                           NULL
#define hb_dbtChildSync                            NULL
#define hb_dbtSyncChildren                         NULL
#define hb_dbtClearRel                             NULL
#define hb_dbtForceRel                             NULL
#define hb_dbtRelArea                              NULL
#define hb_dbtRelEval                              NULL
#define hb_dbtRelText                              NULL
#define hb_dbtSetRel                               NULL
#define hb_dbtOrderListAdd                         NULL
#define hb_dbtOrderListClear                       NULL
#define hb_dbtOrderListDelete                      NULL
#define hb_dbtOrderListFocus                       NULL
#define hb_dbtOrderListRebuild                     NULL
#define hb_dbtOrderCondition                       NULL
#define hb_dbtOrderCreate                          NULL
#define hb_dbtOrderDestroy                         NULL
#define hb_dbtOrderInfo                            NULL
#define hb_dbtClearFilter                          NULL
#define hb_dbtClearLocate                          NULL
#define hb_dbtClearScope                           NULL
#define hb_dbtCountScope                           NULL
#define hb_dbtFilterText                           NULL
#define hb_dbtScopeInfo                            NULL
#define hb_dbtSetFilter                            NULL
#define hb_dbtSetLocate                            NULL
#define hb_dbtSetScope                             NULL
#define hb_dbtSkipScope                            NULL
#define hb_dbtLocate                               NULL
#define hb_dbtCompile                              NULL
#define hb_dbtError                                NULL
#define hb_dbtEvalBlock                            NULL
#define hb_dbtRawLock                              NULL
#define hb_dbtLock                                 NULL
#define hb_dbtUnLock                               NULL
#define hb_dbtCloseMemFile                         NULL
static HB_ERRCODE hb_dbtCreateMemFile( DBTAREAP pArea, LPDBOPENINFO pCreateInfo );
#define hb_dbtGetValueFile                         NULL
static HB_ERRCODE hb_dbtOpenMemFile( DBTAREAP pArea, LPDBOPENINFO pOpenInfo );
#define hb_dbtPutValueFile                         NULL
#define hb_dbtReadDBHeader                         NULL
#define hb_dbtWriteDBHeader                        NULL
#define hb_dbtInit                                 NULL
#define hb_dbtExit                                 NULL
#define hb_dbtDrop                                 NULL
#define hb_dbtExists                               NULL
static HB_ERRCODE hb_dbtRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem );
#define hb_dbtWhoCares                             NULL

HB_EXTERN_END

#endif /* HB_RDDDBT */
