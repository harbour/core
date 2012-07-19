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

HB_BOOL _sx_Eval( PHB_ITEM pItem )
{
   hb_vmPushEvalSym();
   hb_vmPush( pItem );
   hb_vmDo( 0 );
   return hb_itemGetL( hb_stackReturnItem() );
}

HB_FUNC( SX_EVALDATE )
{
   WORD     iWorkArea = SX_DUMMY_NUMBER;
   char *   szDateStr;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_EVALDATE" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   szDateStr = ( char * ) sx_EvalString( ( PBYTE ) hb_parc( 1 ) );

   hb_retds( szDateStr );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_EVALLOGICAL )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_EVALLOGICAL" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retl( sx_EvalLogical( ( PBYTE ) hb_parc( 1 ) /* cpExpression */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_EVALNUMERIC )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_EVALNUMERIC" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retnd( sx_EvalNumeric( ( PBYTE ) hb_parc( 1 ) /* cpExpression */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_EVALSTRING )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_EVALSTRING" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_EvalString( ( PBYTE ) hb_parc( 1 ) /* cpExpression */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_EVALTEST )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_EVALTEST" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retni( sx_EvalTest( ( PBYTE ) hb_parc( 1 ) /* cpExpression */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
