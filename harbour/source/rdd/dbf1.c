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
#include "rddsys.ch"
#include "rdd.api"

HARBOUR HB__DBF( void );
HARBOUR HB_DBF_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbf1__InitSymbols )
{ "_DBF",             FS_PUBLIC, HB__DBF,             0 },
{ "DBF_GETFUNCTABLE", FS_PUBLIC, HB_DBF_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbf1__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup dbf1__InitSymbols
#endif

static ERRCODE Bof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling DBF: Bof()\n" );
   return SUCCESS;
}

static ERRCODE Eof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling DBF: Eof()\n" );
   return SUCCESS;
}

static ERRCODE Found( AREAP pArea, BOOL * pFound )
{
   printf( "Calling DBF: Found()\n" );
   return SUCCESS;
}

static ERRCODE GoBottom( AREAP pArea )
{
   printf( "Calling DBF: GoBottom()\n" );
   return SUCCESS;
}

static ERRCODE GoTo( AREAP pArea, LONG lRecNo )
{
   printf( "Calling DBF: GoTo()\n" );
   return SUCCESS;
}

static ERRCODE GoTop( AREAP pArea )
{
   printf( "Calling DBF: GoTop()\n" );
   return SUCCESS;
}

static ERRCODE Skip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling DBF: Skip()\n" );
   return SUCCESS;
}

static ERRCODE Close( AREAP pArea )
{
   printf( "Calling DBF: Close()\n" );
   return SUCCESS;
}

static ERRCODE Create( AREAP pArea, DBOPENINFOP pCreateInfo )
{
   printf( "Calling DBF: Create()\n" );
   return SUCCESS;
}

static ERRCODE Open( AREAP pArea, DBOPENINFOP pOpenInfo )
{
   printf( "Calling DBF: Open()\n" );
   return SUCCESS;
}

static ERRCODE StructSize( AREAP pArea, USHORT * uiSize )
{
   printf( "Calling DBF: StructSize()\n" );
   return SUCCESS;
}

static RDDFUNCS dbfSuper = { 0 };

static RDDFUNCS dbfTable = { Bof,
			     Eof,
			     Found,
			     GoBottom,
			     GoTo,
			     GoTop,
			     Skip,
			     Close,
			     Create,
			     Open,
			     StructSize
			   };

HARBOUR HB__DBF( void )
{
}

HARBOUR HB_DBF_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &dbfTable, &dbfSuper, 0 ) );
   else
      hb_retni( FAILURE );
}
