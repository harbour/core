/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DELIMITED RDD module
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

#define SUPERTABLE ( &delimSuper )

#include "hbapi.h"
#include "hbinit.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "rddsys.ch"

HB_FUNC( _DELIMC );
HB_FUNC( DELIM_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( delim1__InitSymbols )
{ "_DELIMC",            HB_FS_PUBLIC, HB_FUNCNAME( _DELIMC ), NULL },
{ "DELIM_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( DELIM_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( delim1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_delim1__InitSymbols = delim1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup delim1__InitSymbols
#endif

static RDDFUNCS delimSuper = { 0 };

/*
 * -- DELIM METHODS --
 */

static RDDFUNCS delimTable = { 0 };

HB_FUNC( _DELIMC )
{
}

HB_FUNC( DELIM_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &delimTable, &delimSuper, 0 ) );
   else
      hb_retni( FAILURE );
}

