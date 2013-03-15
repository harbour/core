/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    xHarbour compatible WITH OBJECT internal functions
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbvmint.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"

static PHB_ITEM hb_vmWithObjectItem( HB_ISIZ nLevel )
{
   HB_ISIZ nOffset = hb_stackWithObjectOffset();

   while( nOffset && nLevel > 0 )
   {
      HB_ISIZ * pnOffset = ( HB_ISIZ * ) hb_itemGetPtr( hb_stackItem( nOffset + 1 ) );
      if( ! pnOffset )
         break;
      --nLevel;
      nOffset = *pnOffset;
   }

   return ( nOffset && ! nLevel ) ? hb_stackItem( nOffset ) : NULL;
}

static HB_ISIZ hb_vmWithObjectCount( void )
{
   HB_ISIZ nOffset = hb_stackWithObjectOffset(), nCount = 0;

   while( nOffset )
   {
      HB_ISIZ * pnOffset = ( HB_ISIZ * ) hb_itemGetPtr( hb_stackItem( nOffset + 1 ) );
      if( ! pnOffset )
         break;
      ++nCount;
      nOffset = *pnOffset;
   }

   return nCount;
}



HB_FUNC( HB_QWITH )
{
   hb_itemReturn( hb_vmWithObjectItem( hb_parns( 1 ) ) );
}

HB_FUNC( HB_WITHOBJECTCOUNTER )
{
   hb_retns( hb_vmWithObjectCount() );
}

HB_FUNC( HB_RESETWITH )
{
   PHB_ITEM pItem = hb_vmWithObjectItem( 0 );

   if( hb_pcount() >= 1 && pItem )
      hb_itemMove( pItem, hb_stackItemFromBase( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 1607, NULL, HB_ERR_FUNCNAME, 0, NULL );
}
