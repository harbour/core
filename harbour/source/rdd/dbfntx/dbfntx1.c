/*
 * $Id$

   Copyright(C) 1999 by Bruno Cantero.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: bruno@issnet.net
 */

#include "extend.h"
#include "init.h"
#include "itemapi.h"
#include "rddapi.h"
#include "rddsys.ch"

HARBOUR HB__DBFNTX( void );
HARBOUR HB_DBFNTX_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbfntx1__InitSymbols )
{ "_DBFNTX",             FS_PUBLIC, HB__DBFNTX,             0 },
{ "DBFNTX_GETFUNCTABLE", FS_PUBLIC, HB_DBFNTX_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbfntx1__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup dbfntx1__InitSymbols
#endif

static RDDFUNCS ntxSuper = { 0 };

static RDDFUNCS ntxTable = { 0 };

HARBOUR HB__DBFNTX( void )
{
}

HARBOUR HB_DBFNTX_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &ntxTable, &ntxSuper, ( BYTE * ) "DBF" ) );
   else
      hb_retni( FAILURE );
}
