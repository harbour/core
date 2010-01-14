/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Mini GT for GUI programs.
 *    Now it supports only low level TONE and CLIPBOARD code for Windows
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

#define HB_GT_NAME   GUI

#define HB_OS_WIN_USED
#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

/* *********************************************************************** */

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

/* *********************************************************************** */

static const char * hb_gt_gui_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_gui_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Windows dummy console for GUI programs";
}

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_gui_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_gui_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

#if defined( HB_OS_WIN )
   HB_SYMBOL_UNUSED( pGT );
   hb_gt_winapi_tone( dFrequency, dDuration );
#else
   HB_GTSUPER_TONE( pGT, dFrequency, dDuration );
#endif
}

/* *********************************************************************** */

static BOOL hb_gt_gui_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_gui_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
#if defined( HB_OS_WIN )
      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_gt_winapi_setClipboard( CF_TEXT,
                                       hb_itemGetCPtr( pInfo->pNewVal ),
                                       hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            char * szClipboardData;
            HB_SIZE ulLen;

            if( hb_gt_winapi_getClipboard( CF_TEXT, &szClipboardData, &ulLen ) )
            {
               pInfo->pResult = hb_itemPutCLPtr( pInfo->pResult,
                                                 szClipboardData, ulLen );
            }
            else
            {
               pInfo->pResult = hb_itemPutC( pInfo->pResult, NULL );
            }
         }
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_winapi_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_winapi_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;
#endif
      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return TRUE;
}

/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Version                    = hb_gt_gui_Version;
   pFuncTable->Tone                       = hb_gt_gui_Tone;
   pFuncTable->Info                       = hb_gt_gui_Info;

   return TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
