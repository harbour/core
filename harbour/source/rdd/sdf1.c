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

HARBOUR HB__SDF( void );
HARBOUR HB_SDF_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( sdf1__InitSymbols )
{ "_SDF",             FS_PUBLIC, HB__SDF,             0 },
{ "SDF_GETFUNCTABLE", FS_PUBLIC, HB_SDF_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( sdf1__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup sdf1__InitSymbols
#endif

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling SDF: Bof()\n" );
   return SUCCESS;
}

static ERRCODE Eof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling SDF: Eof()\n" );
   return SUCCESS;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   printf( "Calling SDF: Found()\n" );
   return SUCCESS;
}

static ERRCODE GoBottom( AREAP pArea )
{
   printf( "Calling SDF: GoBottom()\n" );
   return SUCCESS;
}

static ERRCODE GoTo( AREAP pArea, LONG lRecNo )
{
   printf( "Calling SDF: GoTo()\n" );
   return SUCCESS;
}

static ERRCODE GoTop( AREAP pArea )
{
   printf( "Calling SDF: GoTop()\n" );
   return SUCCESS;
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling SDF: Skip()\n" );
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   printf( "Calling SDF: Close()\n" );
   return SUCCESS;
}

static ERRCODE Create( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   printf( "Calling SDF: Create()\n" );
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   printf( "Calling SDF: Open()\n" );
   return SUCCESS;
}

static ERRCODE Release( AREAP pArea )
{
   printf( "Calling SDF: Release()\n" );
   return SUCCESS;
}

static ERRCODE StructSize( AREAP pArea, USHORT * uiSize )
{
   printf( "Calling SDF: StructSize()\n" );
   return SUCCESS;
}

static ERRCODE WriteDBHeader( AREAP pArea )
{
   printf( "Calling SDF: WriteDBHeader()\n" );
   return SUCCESS;
}

static RDDFUNCS sdfSuper = { 0 };

static RDDFUNCS sdfTable = { Bof,
			     Eof,
			     Found,
			     GoBottom,
			     GoTo,
			     GoTop,
			     Skip,
			     Close,
			     Create,
			     Open,
			     Release,
			     StructSize,
			     WriteDBHeader
			   };

HARBOUR HB__SDF( void )
{
}

HARBOUR HB_SDF_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &sdfTable, &sdfSuper, 0 ) );
   else
      hb_retni( FAILURE );
}
