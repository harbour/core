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

static ERRCODE Info( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   switch( uiIndex )
   {
      case DBI_TABLEEXT:
         hb_itemPutC( pItem, ".DBF" );
         break;
   }
   return SUCCESS;
}

static RDDFUNCS ntxSuper = { 0 };

static RDDFUNCS ntxTable = { 0,                    /* Super Bof */
                             0,                    /* Super Eof */
                             0,                    /* Super Found */
                             0,                    /* Super GoBottom */
                             0,                    /* Super GoTo */
                             0,                    /* Super GoToId */
                             0,                    /* Super GoTop */
                             0,                    /* Super Skip */
                             0,                    /* Super AddField */
                             0,                    /* Super CreateFields */
                             0,                    /* Super DeleteRec */
                             0,                    /* Super Deleted */
                             0,                    /* Super FieldCount */
                             0,                    /* Super FieldName */
                             0,                    /* Super Flush */
                             0,                    /* Super GetValue */
                             0,                    /* Super Recall */
                             0,                    /* Super RecCount */
                             0,                    /* Super SetFieldExtent */
                             0,                    /* Super RecNo */
                             0,                    /* Super Close */
                             0,                    /* Super Create */
                             Info,
                             0,                    /* Super NewArea */
                             0,                    /* Super Open */
                             0,                    /* Super Release */
                             0,                    /* Super StructSize */
                             0,                    /* Super SysName */
                             0                     /* Super WriteDBHeader */
                           };

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
