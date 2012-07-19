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

HB_FUNC( SX_SEEK )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_SEEK" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISCHAR( 1 ) )
      hb_retl( sx_Seek( ( PBYTE ) hb_parc( 1 ) ) );
   else
      hb_retl( HB_FALSE );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_SEEKBIN )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_SEEKBIN" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retl( sx_SeekBin( ( PBYTE ) hb_parc( 1 ) /* cpKeyValue */,
                        ( HB_USHORT ) hb_parclen( 1 )    /* uiLength */
                        ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );

   /*
      Description

      Searches the current order for a supplied key. The key may contain binary
      zeroes. Some xBase programmers become very innovative in inventing data
      storage algorithms that bypass the standard xBase character fields.
      These data storage functions usually involve some type of binary
      representation to conserve space (e.g., representing time in two bytes -
      the first byte hours and the second minutes). If indexes are constructed
      on these fields, the search value cannot be passed as a string because the
      value may contain a binary zero - which normally signifies the end of a
      string.

      Parameters

      cpKeyValue:
      The binary value to search for.  The search key value may be a partial
      key (if sx_SetExact is not True).

      uiLength: The length of the search key.
    */
}

HB_FUNC( SX_LOCATE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, "SX_LOCATE" );
   if( ! HB_ISNIL( 4 ) )
      iWorkArea = _sx_select( hb_param( 4, HB_IT_ANY ) );

   hb_retnl( sx_Locate( ( PBYTE ) hb_parc( 1 ) /* cpExpression */,
                        ( SHORT ) hb_parni( 2 ) /* iDirection */, hb_parl( 3 ) /* bContinue */
                        ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FOUND )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_retl( HB_FALSE );
      return;
   }

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retl( sx_Found() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
