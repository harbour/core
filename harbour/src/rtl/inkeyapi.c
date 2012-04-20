/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Inkey GT API
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2002 Walter Negro <anegro@overnet.com.ar>
 *    hb_inkeySetLast()
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbgtcore.h"

int hb_inkey( HB_BOOL fWait, double dSeconds, int iEventMask )
{
   int iKey = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%d, %f, %d)", (int) fWait, dSeconds, iEventMask));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iKey = HB_GTSELF_INKEYGET( pGT, fWait, dSeconds, iEventMask );
      hb_gt_BaseFree( pGT );
   }
   return iKey;
}

void hb_inkeyPut( int iKey )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", iKey));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_INKEYPUT( pGT, iKey );
      hb_gt_BaseFree( pGT );
   }
}

void hb_inkeyIns( int iKey )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyIns(%d)", iKey));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_INKEYINS( pGT, iKey );
      hb_gt_BaseFree( pGT );
   }
}

int hb_inkeyLast( int iEventMask )
{
   int iKey = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast(%d)", iEventMask));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iKey = HB_GTSELF_INKEYLAST( pGT, iEventMask );
      hb_gt_BaseFree( pGT );
   }
   return iKey;
}

int hb_inkeyNext( int iEventMask )
{
   int iKey = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext(%d)", iEventMask));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iKey = HB_GTSELF_INKEYNEXT( pGT, iEventMask );
      hb_gt_BaseFree( pGT );
   }
   return iKey;
}

void hb_inkeyPoll( void )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPoll()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_INKEYPOLL( pGT );
      hb_gt_BaseFree( pGT );
   }
}

int hb_inkeySetLast( int iKey )
{
   int iLast = 0;
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetLast(%d)", iKey));

   pGT = hb_gt_Base();
   if( pGT )
   {
      iLast = HB_GTSELF_INKEYSETLAST( pGT, iKey );
      hb_gt_BaseFree( pGT );
   }
   return iLast;
}

void hb_inkeySetText( const char * szText, HB_SIZE nLen )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetText(%s,%" HB_PFS "u)", szText, nLen));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_INKEYSETTEXT( pGT, szText, nLen );
      hb_gt_BaseFree( pGT );
   }
}

void hb_inkeyReset( void )
{
   PHB_GT pGT;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset()"));

   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_INKEYRESET( pGT );
      hb_gt_BaseFree( pGT );
   }
}

void hb_inkeySetCancelKeys( int iCancelKey, int iCancelKeyEx )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetCancelKeys(%d,%d)", iCancelKey, iCancelKeyEx));

/*
   s_InkeyAltC = iCancelKey;
   s_InkeyAltCEx = iCancelKeyEx;
*/
   HB_SYMBOL_UNUSED( iCancelKey );
   HB_SYMBOL_UNUSED( iCancelKeyEx );
}

HB_SIZE hb_inkeyKeyString( int iKey, char * buffer, HB_SIZE nSize )
{
   HB_SIZE nLen = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyKeyString(%d,%p, %" HB_PFS "u)", iKey, buffer, nSize));

   if( HB_INKEY_ISUNICODE( iKey ) )
   {
      nLen = hb_cdpTextPutU16( hb_vmCDP(), buffer, nSize, HB_INKEY_VALUE( iKey ) );
   }
   else
   {
      if( HB_INKEY_ISCHAR( iKey ) )
         iKey = HB_INKEY_VALUE( iKey );
      if( iKey >= 32 && iKey <= 255 )
      {
         PHB_CODEPAGE cdp = hb_vmCDP();
         nLen = hb_cdpTextPutU16( cdp, buffer, nSize,
                                  hb_cdpGetU16( cdp, ( HB_UCHAR ) iKey ) );
      }
   }
   return nLen;
}

int hb_inkeyKeyStd( int iKey )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyKeyStd(%d)", iKey));

   if( HB_INKEY_ISEXT( iKey ) )
   {
      if( HB_INKEY_ISUNICODE( iKey ) )
      {
         HB_WCHAR wc = ( HB_WCHAR ) HB_INKEY_VALUE( iKey );
         if( wc )
         {
            HB_UCHAR uc = hb_cdpGetUC( hb_vmCDP(), wc, 0 );
            if( uc != 0 )
               iKey = uc;
         }
         else
            iKey = 0;
      }
      else if( HB_INKEY_ISMOUSEPOS( iKey ) )
         iKey = K_MOUSEMOVE;
      else
         iKey = HB_INKEY_VALUE( iKey );
   }
   return iKey;
}
