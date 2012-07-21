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

HB_FUNC( SX_SETPASSWORD )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   PBYTE cpPassword;

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
      cpPassword = ( PBYTE ) hb_parc( 1 );
   else
      cpPassword = ( PBYTE ) 0;

   sx_SetPassword( cpPassword );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_SETQUERYBIT )
{
   sx_SetQueryBit( hb_parnl( 1 ), hb_parl( 1 ) );
}

HB_FUNC( SX_SETRELATION )
{
   WORD  iWorkArea     = SX_DUMMY_NUMBER;
   WORD  uiChildArea   = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_SETRELATION" );

   if( ! HB_ISNIL( 3 ) )
      iWorkArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

   if( HB_ISNUM( 1 ) )
      uiChildArea = ( WORD ) hb_parni( 1 );
   else if( HB_ISCHAR( 1 ) )
      uiChildArea = sx_WorkArea( ( PBYTE ) hb_parc( 1 ) );

   if( uiChildArea != SX_DUMMY_NUMBER )
      sx_SetRelation( uiChildArea, ( PBYTE ) hb_parc( 2 ) /* cpKeyExpr */ );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_CLEARRELATION )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_CLEARRELATION" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_SetRelation( 0, ( PBYTE ) " " );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_CLEARSCOPE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_CLEARSCOPE" );

   if( ! HB_ISNIL( 3 ) )
      iWorkArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

   hb_retl( sx_SetScope( ( PBYTE ) 0, ( PBYTE ) 0 ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_SETSCOPE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_SETSCOPE" );

   if( ! HB_ISNIL( 3 ) )
      iWorkArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

   hb_retl( sx_SetScope( ( PBYTE ) hb_parc( 1 ) /* cpLowVal */,
                         ( PBYTE ) hb_parc( 2 ) /* cpHighVal */
                         ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
