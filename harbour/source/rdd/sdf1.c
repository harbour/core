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

#include "hbapi.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbrddsdf.h"

HB_FUNC( _SDFC );
HB_FUNC( SDF_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( sdf1__InitSymbols )
{ "_SDFC",            HB_FS_PUBLIC, HB_FUNCNAME( _SDFC ), NULL },
{ "SDF_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( SDF_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( sdf1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_sdf1__InitSymbols = sdf1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup sdf1__InitSymbols
#endif

static RDDFUNCS sdfSuper = { NULL };

static RDDFUNCS sdfTable = { hb_sdfBof,
                             hb_sdfEof,
                             hb_sdfFound,
                             hb_sdfGoBottom,
                             hb_sdfGoTo,
                             hb_sdfGoToId,
                             hb_sdfGoTop,
                             hb_sdfSeek,
                             hb_sdfSkip,
                             hb_sdfSkipFilter,
                             hb_sdfSkipRaw,
                             hb_sdfAddField,
                             hb_sdfAppend,
                             hb_sdfCreateFields,
                             hb_sdfDeleteRec,
                             hb_sdfDeleted,
                             hb_sdfFieldCount,
                             hb_sdfFieldDisplay,
                             hb_sdfFieldInfo,
                             hb_sdfFieldName,
                             hb_sdfFlush,
                             hb_sdfGetRec,
                             hb_sdfGetValue,
                             hb_sdfGetVarLen,
                             hb_sdfGoCold,
                             hb_sdfGoHot,
                             hb_sdfPutRec,
                             hb_sdfPutValue,
                             hb_sdfRecall,
                             hb_sdfRecCount,
                             hb_sdfRecInfo,
                             hb_sdfRecNo,
                             hb_sdfSetFieldExtent,
                             hb_sdfAlias,
                             hb_sdfClose,
                             hb_sdfCreate,
                             hb_sdfInfo,
                             hb_sdfNewArea,
                             hb_sdfOpen,
                             hb_sdfRelease,
                             hb_sdfStructSize,
                             hb_sdfSysName,
                             hb_sdfEval,
                             hb_sdfPack,
                             hb_sdfPackRec,
                             hb_sdfSort,
                             hb_sdfTrans,
                             hb_sdfTransRec,
                             hb_sdfZap,
                             hb_sdfChildEnd,
                             hb_sdfChildStart,
                             hb_sdfChildSync,
                             hb_sdfSyncChildren,
                             hb_sdfClearRel,
                             hb_sdfForceRel,
                             hb_sdfRelArea,
                             hb_sdfRelEval,
                             hb_sdfRelText,
                             hb_sdfSetRel,
                             hb_sdfOrderListAdd,
                             hb_sdfOrderListClear,
                             hb_sdfOrderListDelete,
                             hb_sdfOrderListFocus,
                             hb_sdfOrderListRebuild,
                             hb_sdfOrderCondition,
                             hb_sdfOrderCreate,
                             hb_sdfOrderDestroy,
                             hb_sdfOrderInfo,
                             hb_sdfClearFilter,
                             hb_sdfClearLocate,
                             hb_sdfClearScope,
                             hb_sdfCountScope,
                             hb_sdfFilterText,
                             hb_sdfScopeInfo,
                             hb_sdfSetFilter,
                             hb_sdfSetLocate,
                             hb_sdfSetScope,
                             hb_sdfSkipScope,
                             hb_sdfCompile,
                             hb_sdfError,
                             hb_sdfEvalBlock,
                             hb_sdfRawLock,
                             hb_sdfLock,
                             hb_sdfUnLock,
                             hb_sdfCloseMemFile,
                             hb_sdfCreateMemFile,
                             hb_sdfGetValueFile,
                             hb_sdfOpenMemFile,
                             hb_sdfPutValueFile,
                             hb_sdfReadDBHeader,
                             hb_sdfWriteDBHeader,
                             hb_sdfWhoCares
                           };


/*
 * -- SDF METHODS --
 */



HB_FUNC( _SDFC )
{
}

HB_FUNC( SDF_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("SDF_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if( pTable )
      hb_retni( hb_rddInherit( pTable, &sdfTable, &sdfSuper, 0 ) );
   else
      hb_retni( FAILURE );
}
