/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DELIMITED RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_RDDDEL_H_
#define HB_RDDDEL_H_

#include "hbapirdd.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/*
 * -- DELIMITED METHODS --
 */

#define hb_delimBof                                  NULL
#define hb_delimEof                                  NULL
#define hb_delimFound                                NULL
#define hb_delimGoBottom                             NULL
#define hb_delimGoTo                                 NULL
#define hb_delimGoToId                               NULL
#define hb_delimGoTop                                NULL
#define hb_delimSeek                                 NULL
#define hb_delimSkip                                 NULL
#define hb_delimSkipFilter                           NULL
#define hb_delimSkipRaw                              NULL
#define hb_delimAddField                             NULL
#define hb_delimAppend                               NULL
#define hb_delimCreateFields                         NULL
#define hb_delimDeleteRec                            NULL
#define hb_delimDeleted                              NULL
#define hb_delimFieldCount                           NULL
#define hb_delimFieldDisplay                         NULL
#define hb_delimFieldInfo                            NULL
#define hb_delimFieldName                            NULL
#define hb_delimFlush                                NULL
#define hb_delimGetRec                               NULL
#define hb_delimGetValue                             NULL
#define hb_delimGetVarLen                            NULL
#define hb_delimGoCold                               NULL
#define hb_delimGoHot                                NULL
#define hb_delimPutRec                               NULL
#define hb_delimPutValue                             NULL
#define hb_delimRecAll                               NULL
#define hb_delimRecCount                             NULL
#define hb_delimRecInfo                              NULL
#define hb_delimRecNo                                NULL
#define hb_delimSetFieldExtent                       NULL
#define hb_delimAlias                                NULL
#define hb_delimClose                                NULL
#define hb_delimCreate                               NULL
#define hb_delimInfo                                 NULL
#define hb_delimNewArea                              NULL
#define hb_delimOpen                                 NULL
#define hb_delimRelease                              NULL
#define hb_delimStructSize                           NULL
#define hb_delimSysName                              NULL
#define hb_delimEval                                 NULL
#define hb_delimPack                                 NULL
#define hb_delimPackRec                              NULL
#define hb_delimSort                                 NULL
#define hb_delimTrans                                NULL
#define hb_delimTransRec                             NULL
#define hb_delimZap                                  NULL
#define hb_delimChildEnd                             NULL
#define hb_delimChildStart                           NULL
#define hb_delimChildSync                            NULL
#define hb_delimSyncChildren                         NULL
#define hb_delimClearRel                             NULL
#define hb_delimForceRel                             NULL
#define hb_delimRelArea                              NULL
#define hb_delimRelEval                              NULL
#define hb_delimRelText                              NULL
#define hb_delimSetRel                               NULL
#define hb_delimOrderListAdd                         NULL
#define hb_delimOrderListClear                       NULL
#define hb_delimOrderListDelete                      NULL
#define hb_delimOrderListFocus                       NULL
#define hb_delimOrderListRebuild                     NULL
#define hb_delimOrderCondition                       NULL
#define hb_delimOrderCreate                          NULL
#define hb_delimOrderDestroy                         NULL
#define hb_delimOrderInfo                            NULL
#define hb_delimClearFilter                          NULL
#define hb_delimClearLocate                          NULL
#define hb_delimClearScope                           NULL
#define hb_delimCountScope                           NULL
#define hb_delimFilterText                           NULL
#define hb_delimScopeInfo                            NULL
#define hb_delimSetFilter                            NULL
#define hb_delimSetLocate                            NULL
#define hb_delimSetScope                             NULL
#define hb_delimSkipScope                            NULL
#define hb_delimCompile                              NULL
#define hb_delimError                                NULL
#define hb_delimEvalBlock                            NULL
#define hb_delimRawLock                              NULL
#define hb_delimLock                                 NULL
#define hb_delimUnLock                               NULL
#define hb_delimCloseMemFile                         NULL
#define hb_delimCreateMemFile                        NULL
#define hb_delimGetValueFile                         NULL
#define hb_delimOpenMemFile                          NULL
#define hb_delimPutValueFile                         NULL
#define hb_delimReadDBHeader                         NULL
#define hb_delimWriteDBHeader                        NULL
#define hb_delimWhoCares                             NULL

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_RDDDEL_H_ */