/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Default RDD module
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

#ifndef HB_RDDWRK_H_
#define HB_RDDWRK_H_

#include "hbapirdd.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/*
 * -- METHODS --
 */

extern ERRCODE hb_waBof( AREAP pArea, BOOL * pBof );
extern ERRCODE hb_waEof( AREAP pArea, BOOL * pEof );
extern ERRCODE hb_waFound( AREAP pArea, BOOL * pFound );
#define hb_waGoBottom                                       hb_waUnsupported
#define hb_waGoTo                           ( DBENTRYP_UL ) hb_waUnsupported
#define hb_waGoToId                          ( DBENTRYP_I ) hb_waUnsupported
#define hb_waGoTop                                          hb_waUnsupported
#define hb_waSeek                          ( DBENTRYP_BIB ) hb_waUnsupported
extern ERRCODE hb_waSkip( AREAP pArea, LONG lToSkip );
extern ERRCODE hb_waSkipFilter( AREAP pArea, LONG lUpDown );
#define hb_waSkipRaw                         ( DBENTRYP_L ) hb_waUnsupported
extern ERRCODE hb_waAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo );
#define hb_waAppend                          ( DBENTRYP_B ) hb_waUnsupported
extern ERRCODE hb_waCreateFields( AREAP pArea, PHB_ITEM pStruct );
#define hb_waDeleteRec                                      hb_waUnsupported
#define hb_waDeleted                        ( DBENTRYP_BP ) hb_waUnsupported
extern ERRCODE hb_waFieldCount( AREAP pArea, USHORT * uiFields );
#define hb_waFieldDisplay                   ( DBENTRYP_VF ) hb_waUnsupported
extern ERRCODE hb_waFieldInfo( AREAP pArea, USHORT uiIndex, USHORT uiType, PHB_ITEM pItem );
extern ERRCODE hb_waFieldName( AREAP pArea, USHORT uiIndex, void * szName );
#define hb_waFlush                                          hb_waUnsupported
#define hb_waGetRec                         ( DBENTRYP_PP ) hb_waUnsupported
#define hb_waGetValue                       ( DBENTRYP_SI ) hb_waUnsupported
#define hb_waGetVarLen                     ( DBENTRYP_SVL ) hb_waUnsupported
#define hb_waGoCold                                         hb_waUnsupported
#define hb_waGoHot                                          hb_waUnsupported
#define hb_waPutRec                          ( DBENTRYP_P ) hb_waUnsupported
#define hb_waPutValue                       ( DBENTRYP_SI ) hb_waUnsupported
#define hb_waRecall                                         hb_waUnsupported
#define hb_waRecCount                      ( DBENTRYP_ULP ) hb_waUnsupported
#define hb_waRecInfo                       ( DBENTRYP_ISI ) hb_waUnsupported
#define hb_waRecNo                           ( DBENTRYP_I ) hb_waUnsupported
extern ERRCODE hb_waSetFieldExtent( AREAP pArea, USHORT uiFieldExtent );
extern ERRCODE hb_waAlias( AREAP pArea, BYTE * szAlias );
extern ERRCODE hb_waClose( AREAP pArea );
#define hb_waCreate                         ( DBENTRYP_VP ) hb_waUnsupported
extern ERRCODE hb_waInfo( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem );
extern ERRCODE hb_waNewArea( AREAP pArea );
#define hb_waOpen                           ( DBENTRYP_VP ) hb_waUnsupported
extern ERRCODE hb_waRelease( AREAP pArea );
extern ERRCODE hb_waStructSize( AREAP pArea, USHORT * uiSize );
extern ERRCODE hb_waSysName( AREAP pArea, BYTE * pBuffer );
extern ERRCODE hb_waEval( AREAP pArea, LPDBEVALINFO pEvalInfo );
#define hb_waPack                                           hb_waUnsupported
#define hb_waPackRec                       ( DBENTRYP_LSP ) hb_waUnsupported
#define hb_waSort                           ( DBENTRYP_VS ) hb_waUnsupported
extern ERRCODE hb_waTrans( AREAP pArea, LPDBTRANSINFO pTransInfo );
extern ERRCODE hb_waTransRec( AREAP pArea, LPDBTRANSINFO pTransInfo );
#define hb_waZap                                            hb_waUnsupported
extern ERRCODE hb_waChildEnd( AREAP pArea, LPDBRELINFO pRelInfo );
extern ERRCODE hb_waChildStart( AREAP pArea, LPDBRELINFO pRelInfo );
#define hb_waChildSync                      ( DBENTRYP_VR ) hb_waUnsupported
extern ERRCODE hb_waSyncChildren( AREAP pArea );
extern ERRCODE hb_waClearRel( AREAP pArea );
#define hb_waForceRel                                       hb_waUnsupported
extern ERRCODE hb_waRelArea( AREAP pArea, USHORT uiRelNo, void * pRelArea );
extern ERRCODE hb_waRelEval( AREAP pArea, LPDBRELINFO pRelInfo );
extern ERRCODE hb_waRelText( AREAP pArea, USHORT uiRelNo, void * pExpr );
extern ERRCODE hb_waSetRel( AREAP pArea, LPDBRELINFO pRelInfo );
#define hb_waOrderListAdd                   ( DBENTRYP_OI ) hb_waUnsupported
#define hb_waOrderListClear                                 hb_waUnsupported
#define hb_waOrderListDelete                ( DBENTRYP_VP ) hb_waUnsupported
#define hb_waOrderListFocus                 ( DBENTRYP_OI ) hb_waUnsupported
#define hb_waOrderListRebuild                               hb_waUnsupported
extern ERRCODE hb_waOrderCondition( AREAP pArea, LPDBORDERCONDINFO param );
#define hb_waOrderCreate                   ( DBENTRYP_VOC ) hb_waUnsupported
#define hb_waOrderDestroy                   ( DBENTRYP_OI ) hb_waUnsupported
//#define hb_waOrderInfo                            ( DBENTRYP_OII ) hb_waNull
extern ERRCODE hb_waOrderInfo( AREAP pArea, USHORT index, LPDBORDERINFO param );
extern ERRCODE hb_waClearFilter( AREAP pArea );
extern ERRCODE hb_waClearLocate( AREAP pArea );
#define hb_waClearScope                                     hb_waUnsupported
#define hb_waCountScope                   ( DBENTRYP_VPLP ) hb_waUnsupported
extern ERRCODE hb_waFilterText( AREAP pArea, PHB_ITEM pFilter );
#define hb_waScopeInfo                      ( DBENTRYP_SI ) hb_waUnsupported
extern ERRCODE hb_waSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo );
extern ERRCODE hb_waSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo );
#define hb_waSetScope                       ( DBENTRYP_VOS ) hb_waUnsupported
#define hb_waSkipScope                     ( DBENTRYP_VPL ) hb_waUnsupported
extern ERRCODE hb_waCompile( AREAP pArea, BYTE * pExpr );
extern ERRCODE hb_waError( AREAP pArea, PHB_ITEM pError );
extern ERRCODE hb_waEvalBlock( AREAP pArea, PHB_ITEM pBlock );
#define hb_waRawLock                       ( DBENTRYP_VSP ) hb_waUnsupported
#define hb_waLock                           ( DBENTRYP_VL ) hb_waUnsupported
#define hb_waUnLock                         ( DBENTRYP_UL ) hb_waUnsupported
#define hb_waCloseMemFile                                   hb_waUnsupported
#define hb_waCreateMemFile                  ( DBENTRYP_VP ) hb_waUnsupported
#define hb_waGetValueFile                 ( DBENTRYP_SVPB ) hb_waUnsupported
#define hb_waOpenMemFile                    ( DBENTRYP_VP ) hb_waUnsupported
#define hb_waPutValueFile                  ( DBENTRYP_SVP ) hb_waUnsupported
#define hb_waReadDBHeader                                   hb_waUnsupported
#define hb_waWriteDBHeader                                  hb_waUnsupported

#define hb_rddExit                         (DBENTRYP_I0)    NULL
#define hb_rddDrop                         (DBENTRYP_I1)    hb_waUnsupported
#define hb_rddExists                       (DBENTRYP_I2)    hb_waUnsupported

#define hb_waWhoCares                      ( DBENTRYP_SVP ) hb_waUnsupported

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_RDDWRK_H_ */
