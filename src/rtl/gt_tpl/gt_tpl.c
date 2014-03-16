/*
 * Harbour Project source code:
 * {Video subsystem template}
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* TODO: include any standard headers here */

#include "hbgtcore.h"
#include "hbinit.h"
static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )

static void hb_gt_tpl_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_Init(%p,%p,%p,%p)", pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   /* TODO: */

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
}

static void hb_gt_tpl_Exit( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_Exit(%p)", pGT ) );

   HB_GTSUPER_EXIT( pGT );

   /* TODO: */
}


static int hb_gt_tpl_ReadKey( PHB_GT pGT, int iEventMask )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iEventMask );

   /* TODO: check the input queue (incoming mouse and keyboard events)
            and return the inkey code if any */

   return 0;
}

static const char * hb_gt_tpl_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Terminal: (template)";
}

static HB_BOOL hb_gt_tpl_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_SetMode(%p,%d,%d)", pGT, iRows, iCols ) );

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );

   /* TODO: if possible change the size of the screen and return HB_TRUE */

   return HB_FALSE;
}

static void hb_gt_tpl_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   int iColor;
   HB_BYTE bAttr;
   HB_USHORT usChar;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   while( iSize-- )
   {
      if( ! HB_GTSELF_GETSCRCHAR( pGT, iRow, iCol, &iColor, &bAttr, &usChar ) )
         break;
      /* TODO: display usChar at iRow, iCol position with color bColor */
      ++iCol;
   }
}

static void hb_gt_tpl_Refresh( PHB_GT pGT )
{
   int iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_tpl_Refresh(%p)", pGT ) );

   HB_GTSUPER_REFRESH( pGT );
   HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );

   /* TODO: set cursor position and shape */
}


/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init    = hb_gt_tpl_Init;
   pFuncTable->Exit    = hb_gt_tpl_Exit;
   pFuncTable->ReadKey = hb_gt_tpl_ReadKey;
   pFuncTable->Version = hb_gt_tpl_Version;
   pFuncTable->SetMode = hb_gt_tpl_SetMode;
   pFuncTable->Redraw  = hb_gt_tpl_Redraw;
   pFuncTable->Refresh = hb_gt_tpl_Refresh;

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
