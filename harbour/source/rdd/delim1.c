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
#include "rddapi.h"
#include "rddsys.ch"

HARBOUR HB__DELIM( void );
HARBOUR HB_DELIM_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( delim1__InitSymbols )
{ "_DELIM",             FS_PUBLIC, HB__DELIM,             0 },
{ "DELIM_GETFUNCTABLE", FS_PUBLIC, HB_DELIM_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( delim1__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup delim1__InitSymbols
#endif

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling DELIM: Bof()\n" );
   return SUCCESS;
}

static ERRCODE Eof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling DELIM: Eof()\n" );
   return SUCCESS;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   printf( "Calling DELIM: Found()\n" );
   return SUCCESS;
}

static ERRCODE GoBottom( AREAP pArea )
{
   printf( "Calling DELIM: GoBottom()\n" );
   return SUCCESS;
}

static ERRCODE GoTo( AREAP pArea, LONG lRecNo )
{
   printf( "Calling DELIM: GoTo()\n" );
   return SUCCESS;
}

static ERRCODE GoTop( AREAP pArea )
{
   printf( "Calling DELIM: GoTop()\n" );
   return SUCCESS;
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling DELIM: Skip()\n" );
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   printf( "Calling DELIM: Close()\n" );
   return SUCCESS;
}

static ERRCODE Info( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   printf( "Calling DELIM: Info()\n" );
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   printf( "Calling DELIM: Open()\n" );
   return SUCCESS;
}

static ERRCODE WriteDBHeader( AREAP pArea )
{
   printf( "Calling DELIM: WriteDBHeader()\n" );
   return SUCCESS;
}

static RDDFUNCS delimSuper = { 0 };

static RDDFUNCS delimTable = { Bof,
                               Eof,
                               Found,
                               GoBottom,
                               GoTo,
                               GoTop,
                               Skip,
                               0,                /* Super AddField */
                               0,                /* Super CreateFields */
                               0,                /* Super SetFieldExtent */
                               Close,
                               0,                /* Super Create */
                               Info,
                               0,                /* Super NewArea */
                               Open,
                               0,                /* Super Release */
                               0,                /* Super StructSize */
                               0,                /* Super SysName */
                               WriteDBHeader
			   };

HARBOUR HB__DELIM( void )
{
}

HARBOUR HB_DELIM_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &delimTable, &delimSuper, 0 ) );
   else
      hb_retni( FAILURE );
}
