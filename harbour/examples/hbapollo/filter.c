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

HB_FUNC( SX_DBFILTER )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_DBFILTER" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_DBFilter() );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_SETFILTER )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_SETFILTER" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   sx_SetFilter( ( PBYTE ) hb_parc( 1 ) /* cpExpression */ );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_CLEARFILTER )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_CLEARFILTER" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   sx_SetFilter( ( PBYTE ) 0 );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FILTERALIAS )
{
   hb_retl( sx_FilterAlias( ( PBYTE ) hb_parc( 1 ) /* cpAliasName */,
                            ( PBYTE ) hb_parc( 2 ) /* cpFieldName */
                            ) );
}
