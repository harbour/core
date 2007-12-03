/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Inkey GT API
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbgtcore.h"

HB_EXPORT int  hb_inkey( BOOL fWait, double dSeconds, int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%d, %f, %d)", (int) fWait, dSeconds, iEventMask));

   return hb_inkey_Get( fWait, dSeconds, iEventMask );
}

HB_EXPORT void hb_inkeyPut( int iKey )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", iKey));

   hb_inkey_Put( iKey );
}

HB_EXPORT int  hb_inkeyLast( int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast(%d)", iEventMask));

   return hb_inkey_Last( iEventMask );
}

HB_EXPORT int  hb_inkeyNext( int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext(%d)", iEventMask));

   return hb_inkey_Next( iEventMask );
}

HB_EXPORT void hb_inkeyPoll( void )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyPoll()") );

   hb_inkey_Poll();
}

HB_EXPORT int  hb_inkeySetLast( int iKey )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetLast(%d)", iKey));

   return hb_inkey_SetLast( iKey );
}

HB_EXPORT void hb_inkeySetText( const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetText(%s,%lu)", szText, ulLen));

   hb_inkey_SetText( szText, ulLen );
}

HB_EXPORT void hb_inkeyReset( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset()"));

   hb_inkey_Reset();
}

HB_EXPORT void hb_inkeySetCancelKeys( int iCancelKey, int iCancelKeyEx )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeySetCancelKeys(%d,%d)", iCancelKey, iCancelKeyEx));

/*
   s_InkeyAltC = iCancelKey;
   s_InkeyAltCEx = iCancelKeyEx;
*/
   HB_SYMBOL_UNUSED( iCancelKey );
   HB_SYMBOL_UNUSED( iCancelKeyEx );
}
