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

HB_FUNC( SX_FIELDCOUNT )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_retni( 0 );
      return;
   }

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( sx_FieldCount() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FCOUNT )
{
   HB_FUNCNAME( SX_FIELDCOUNT ) ();
}

HB_FUNC( SX_FIELDDECIMALS )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_retni( -1 );
      return;
   }

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
      hb_retni( sx_FieldDecimals( ( PBYTE ) hb_parc( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retni( sx_FieldDecimals( ( PBYTE ) sx_FieldName( ( WORD ) hb_parni( 1 ) ) ) );
   else
      hb_retni( -1 );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FIELDNAME )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_retc_null();
      return;
   }

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISNUM( 1 ) )
      hb_retc( ( char * ) sx_FieldName( ( WORD ) hb_parni( 1 ) ) );
   else
      hb_retc_null();

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FIELDNUM )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_retni( 0 );
      return;
   }

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
      hb_retni( sx_FieldNum( ( PBYTE ) hb_parc( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
   {
      int iField = hb_parni( 1 );
      if( iField <= sx_FieldCount() )
         hb_retni( iField );
      else
         hb_retni( 0 );
   }
   else
      hb_retni( 0 );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FIELDPOS )
{
   HB_FUNCNAME( SX_FIELDNUM ) ();
}

HB_FUNC( SX_FIELDOFFSET )
{
   PHB_ITEM vParam     = hb_param( 2, HB_IT_ANY );
   WORD     iWorkArea  = SX_DUMMY_NUMBER;

   if( vParam && ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( vParam );

   hb_retni( sx_FieldOffset( ( PBYTE ) hb_parc( 1 ) ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FIELDTYPE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_FIELDTYPE" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
      hb_retc( ( char * ) sx_FieldType( ( PBYTE ) hb_parc( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retc( ( char * ) sx_FieldType( ( PBYTE ) sx_FieldName( ( WORD ) hb_parni( 1 ) ) ) );
   else
      hb_retc_null();

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FIELDWIDTH )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_retni( 0 );
      return;
   }

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
      hb_retni( sx_FieldWidth( ( PBYTE ) hb_parc( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retni( sx_FieldWidth( ( PBYTE ) sx_FieldName( ( WORD ) hb_parni( 1 ) ) ) );
   else
      hb_retni( -1 );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
