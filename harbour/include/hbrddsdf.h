/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SDF RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_RDDSDF_H_
#define HB_RDDSDF_H_

#include "hbapirdd.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/*
 * -- SDF METHODS --
 */

#define hb_sdfBof                                  NULL
#define hb_sdfEof                                  NULL
#define hb_sdfFound                                NULL
#define hb_sdfGoBottom                             NULL
#define hb_sdfGoTo                                 NULL
#define hb_sdfGoToId                               NULL
#define hb_sdfGoTop                                NULL
#define hb_sdfSeek                                 NULL
#define hb_sdfSkip                                 NULL
#define hb_sdfSkipFilter                           NULL
#define hb_sdfSkipRaw                              NULL
#define hb_sdfAddField                             NULL
#define hb_sdfAppend                               NULL
#define hb_sdfCreateFields                         NULL
#define hb_sdfDeleteRec                            NULL
#define hb_sdfDeleted                              NULL
#define hb_sdfFieldCount                           NULL
#define hb_sdfFieldDisplay                         NULL
#define hb_sdfFieldInfo                            NULL
#define hb_sdfFieldName                            NULL
#define hb_sdfFlush                                NULL
#define hb_sdfGetRec                               NULL
#define hb_sdfGetValue                             NULL
#define hb_sdfGetVarLen                            NULL
#define hb_sdfGoCold                               NULL
#define hb_sdfGoHot                                NULL
#define hb_sdfPutRec                               NULL
#define hb_sdfPutValue                             NULL
#define hb_sdfRecall                               NULL
#define hb_sdfRecCount                             NULL
#define hb_sdfRecInfo                              NULL
#define hb_sdfRecNo                                NULL
#define hb_sdfSetFieldExtent                       NULL
#define hb_sdfAlias                                NULL
#define hb_sdfClose                                NULL
#define hb_sdfCreate                               NULL
#define hb_sdfInfo                                 NULL
#define hb_sdfNewArea                              NULL
#define hb_sdfOpen                                 NULL
#define hb_sdfRelease                              NULL
#define hb_sdfStructSize                           NULL
#define hb_sdfSysName                              NULL
#define hb_sdfEval                                 NULL
#define hb_sdfPack                                 NULL
#define hb_sdfPackRec                              NULL
#define hb_sdfSort                                 NULL
#define hb_sdfTrans                                NULL
#define hb_sdfTransRec                             NULL
#define hb_sdfZap                                  NULL
#define hb_sdfChildEnd                             NULL
#define hb_sdfChildStart                           NULL
#define hb_sdfChildSync                            NULL
#define hb_sdfSyncChildren                         NULL
#define hb_sdfClearRel                             NULL
#define hb_sdfForceRel                             NULL
#define hb_sdfRelArea                              NULL
#define hb_sdfRelEval                              NULL
#define hb_sdfRelText                              NULL
#define hb_sdfSetRel                               NULL
#define hb_sdfOrderListAdd                         NULL
#define hb_sdfOrderListClear                       NULL
#define hb_sdfOrderListDelete                      NULL
#define hb_sdfOrderListFocus                       NULL
#define hb_sdfOrderListRebuild                     NULL
#define hb_sdfOrderCondition                       NULL
#define hb_sdfOrderCreate                          NULL
#define hb_sdfOrderDestroy                         NULL
#define hb_sdfOrderInfo                            NULL
#define hb_sdfClearFilter                          NULL
#define hb_sdfClearLocate                          NULL
#define hb_sdfClearScope                           NULL
#define hb_sdfCountScope                           NULL
#define hb_sdfFilterText                           NULL
#define hb_sdfScopeInfo                            NULL
#define hb_sdfSetFilter                            NULL
#define hb_sdfSetLocate                            NULL
#define hb_sdfSetScope                             NULL
#define hb_sdfSkipScope                            NULL
#define hb_sdfCompile                              NULL
#define hb_sdfError                                NULL
#define hb_sdfEvalBlock                            NULL
#define hb_sdfRawLock                              NULL
#define hb_sdfLock                                 NULL
#define hb_sdfUnLock                               NULL
#define hb_sdfCloseMemFile                         NULL
#define hb_sdfCreateMemFile                        NULL
#define hb_sdfGetValueFile                         NULL
#define hb_sdfOpenMemFile                          NULL
#define hb_sdfPutValueFile                         NULL
#define hb_sdfReadDBHeader                         NULL
#define hb_sdfWriteDBHeader                        NULL
#define hb_sdfWhoCares                             NULL

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_RDDSDF_H_ */
