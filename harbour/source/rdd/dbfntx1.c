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

#include <extend.h>
#include <init.h>
#include <rdd.api>

HARBOUR HB_DBFNTX_GETFUNCTABLE( void );

HB_INIT_SYMBOLS_BEGIN( dbfntx1__InitSymbols )
{ "DBFNTX_GETFUNCTABLE", FS_PUBLIC, HB_DBFNTX_GETFUNCTABLE, 0 }
HB_INIT_SYMBOLS_END( dbfntx1__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup dbfntx1__InitSymbols
#endif

ERRCODE dbfntxBof( AREAP pArea, BOOL * pBof )
{
   printf( "Calling dbfntxBof()\n" );
   return SUCCESS;
}

ERRCODE dbfntxEof( AREAP pArea, BOOL * pEof )
{
   printf( "Calling dbfntxEof()\n" );
   return SUCCESS;
}

ERRCODE dbfntxFound( AREAP pArea, BOOL * pFound )
{
   printf( "Calling dbfntxFound()\n" );
   return SUCCESS;
}

ERRCODE dbfntxGoBottom( AREAP pArea )
{
   printf( "Calling dbfntxGoBottom()\n" );
   return SUCCESS;
}

ERRCODE dbfntxGoTo( AREAP pArea, LONG lRecNo )
{
   printf( "Calling dbfntxGoTo()\n" );
   return SUCCESS;
}

ERRCODE dbfntxGoTop( AREAP pArea )
{
   printf( "Calling dbfntxGoTop()\n" );
   return SUCCESS;
}

ERRCODE dbfntxSkip( AREAP pArea, LONG lToSkip )
{
   printf( "Calling dbfntxSkip()\n" );
   return SUCCESS;
}

ERRCODE dbfntxClose( AREAP pArea )
{
   printf( "Calling dbfntxClose()\n" );
   return SUCCESS;
}

ERRCODE dbfntxCreate( AREAP pArea, DBOPENINFOP pCreateInfo )
{
   printf( "Calling dbfntxCreate()\n" );
   return SUCCESS;
}

ERRCODE dbfntxOpen( AREAP pArea, DBOPENINFOP pOpenInfo )
{
   printf( "Calling dbfntxOpen()\n" );
   return SUCCESS;
}


static RDDFUNCS ntxSuper = { 0 };

static RDDFUNCS ntxTable = { dbfntxBof,
			     dbfntxEof,
			     dbfntxFound,
			     dbfntxGoBottom,
			     dbfntxGoTo,
			     dbfntxGoTop,
			     dbfntxSkip,
			     dbfntxClose,
			     dbfntxCreate,
			     dbfntxOpen
			   };

HARBOUR HB_REQUEST_DBFNTX1( void )
{
}

HARBOUR HB_DBFNTX_GETFUNCTABLE( void )
{
   RDDFUNCS * pTable;

   *( USHORT * ) hb_parnl( 1 ) = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_rddInherit( pTable, &ntxTable, &ntxSuper, 0 );
   hb_retni( SUCCESS );
}
