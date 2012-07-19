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

HB_FUNC( SX_SELECT )
{
   if( HB_ISNUM( 1 ) )
      hb_retnl( sx_Select( ( WORD ) hb_parni( 1 ) ) );

   /* Added in Beta 0.2 2003-05-08 */
   else if( HB_ISCHAR( 1 ) )
      hb_retnl( sx_Select( sx_WorkArea( ( PBYTE ) hb_parc( 1 ) ) ) );
   else
      hb_retnl( 1 );
}

/* For C Calls 2003.05.08 */
WORD _sx_select( PHB_ITEM vParam )
{
   WORD iSelected = SX_DUMMY_NUMBER;

   if( HB_IS_NUMERIC( vParam ) )
      iSelected = sx_Select( ( WORD ) hb_itemGetNI( vParam ) );
   else if( HB_IS_STRING( vParam ) )
      iSelected = sx_Select( sx_WorkArea( ( PBYTE ) hb_itemGetCPtr( vParam ) ) );

   if( iSelected == SX_DUMMY_NUMBER )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOALIAS, NULL, "_sx_select" );

   return iSelected;
}
