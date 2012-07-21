/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */
#include "sxapi.h"

HB_FUNC( SX_SYSPROP )
{
   int i = hb_parni( 2 );

   if( HB_ISNIL( 2 ) )
      sx_SysProp( ( WORD ) hb_parni( 1 ), ( PVOID ) NULL );
   else
      hb_retni( sx_SysProp(
                   ( WORD ) hb_parni( 1 ), /* One of the predefined constant values. */
                   ( void * ) i ) );
}

HB_FUNC( SX_RDDDRIVER )
{
   int      iDriverName;
   char *   cRDD[] = { "SDENTX", "SDEFOX", "SDENSX", "SDENSX_DBT" };

   if( hb_pcount() == 0 )
      iDriverName = sx_SysProp( SDE_SP_GETDRIVER, ( PVOID ) NULL );
   else
   {
      int iWorkArea = hb_parni( 1 );
      iDriverName = sx_SysProp( SDE_SP_GETDRIVER, ( void * ) iWorkArea );
   }

   hb_retc( cRDD[ iDriverName - 1 ] );
}
