/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SDF RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#define SUPERTABLE ( &sdfSuper )

#include "hbapi.h"
#include "hbinit.h"
#include "hbapirdd.h"
#include "rddsys.ch"

HB_FUNC( _SDFC );
HB_FUNC( SDF_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( sdf1__InitSymbols )
{ "_SDFC",            HB_FS_PUBLIC, HB_FUNCNAME( _SDFC ), NULL },
{ "SDF_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( SDF_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( sdf1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_sdf1__InitSymbols = sdf1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup sdf1__InitSymbols
#endif

static RDDFUNCS sdfSuper = { 0 };

/*
 * -- SDF METHODS --
 */

static RDDFUNCS sdfTable = { 0 };

HB_FUNC( _SDFC )
{
}

HB_FUNC( SDF_GETFUNCTABLE )
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

