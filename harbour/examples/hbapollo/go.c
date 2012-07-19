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

HB_FUNC( SX_GO )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GO" );
   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );
   if( HB_ISNUM( 1 ) )
      sx_Go( hb_parnl( 1 ) );
   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GOBOTTOM )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GOBOTTOM" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_GoBottom();

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GOTOP )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GOTOP" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_GoTop();

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
