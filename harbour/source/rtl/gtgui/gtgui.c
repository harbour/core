/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Mini GT for GUI programs.
 *    Now it supports only low level TONE and CLIPBOARD code for W32
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


/* NOTE: User programs should never call this layer directly! */


#define HB_OS_WIN_32_USED
#include "hbapi.h"


/*
 * This GT should be called GUI but we introduce a hack to make
 * Windows users happy ;-) and we will change its name to the
 * one used by default GT REQUESTed by our RTL library, [druzus]
 */

#if defined( HB_OS_WIN_32 )

#if defined(HB_GT_DEFAULT)
#  define HB_GT_NAME HB_GT_DEFAULT
#elif defined(HB_GT_LIB)
#  define HB_GT_NAME HB_GT_LIB
#else
#  define HB_GT_NAME WIN
#endif

#else

#define HB_GT_NAME   GUI

#endif

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

/* *********************************************************************** */

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_gui_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_gui_Tone(%lf, %lf)", dFrequency, dDuration));

#if defined( HB_OS_WIN_32 )
   hb_gt_w32_Tone( dFrequency, dDuration );
#else
   HB_GTSUPER_TONE( dFrequency, dDuration );
#endif
}

/* *********************************************************************** */

static BOOL hb_gt_gui_Info( int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_gui_Info(%d,%p)", iType, pInfo ) );

   switch( iType )
   {
#if defined( HB_OS_WIN_32 )
      case GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_w32_SetClipboard( CF_TEXT, hb_itemGetCPtr( pInfo->pNewVal ),
                                    hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            char * szClipboardData;
            ULONG ulLen;

            if( hb_gt_w32_GetClipboard( CF_TEXT, &szClipboardData, &ulLen ) )
            {
               pInfo->pResult = hb_itemPutCPtr( pInfo->pResult,
                                                szClipboardData, ulLen );
            }
            else
            {
               pInfo->pResult = hb_itemPutC( pInfo->pResult, "" );
            }
         }
         break;
#endif
      default:
         return HB_GTSUPER_INFO( iType, pInfo );
   }

   return TRUE;
}

/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Tone                       = hb_gt_gui_Tone;
   pFuncTable->Info                       = hb_gt_gui_Info;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

HB_GT_ANNOUNCE( HB_GT_NAME )

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif
