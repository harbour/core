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

void _sx_DelOpenInfo( const char * szAlias )
{
   if( Opened_DBF_Property )
   {
      HB_USHORT uiSize;
      for( uiSize = 0; uiSize < ( HB_USHORT ) hb_arrayLen( Opened_DBF_Property ); uiSize++ )
      {
         PHB_ITEM pInfo      = hb_arrayGetItemPtr( Opened_DBF_Property, uiSize + 1 );
         char *   cAliasInfo = _sx_upper( ( char * ) hb_arrayGetCPtr( pInfo, 3 ) );

         if( cAliasInfo )
         {
            if( strcmp( cAliasInfo, szAlias ) == 0 )
            {
               hb_arrayDel( Opened_DBF_Property, uiSize + 1 );
               hb_arraySize( Opened_DBF_Property, hb_arrayLen( Opened_DBF_Property ) - 1 );
               break;
            }
         }
      }
   }
}

HB_FUNC( SX_CLOSE )
{
   if( _sx_Used() )
   {
      WORD           iWorkArea = SX_DUMMY_NUMBER;
      const char *   szAlias;

      if( ! HB_ISNIL( 1 ) )
         iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

      szAlias = ( const char * ) sx_Alias( 0 );

      sx_Close();

      _sx_DelOpenInfo( szAlias );

      if( ( ! ( iWorkArea == SX_DUMMY_NUMBER ) ) && _sx_Used() )
         sx_Select( iWorkArea );
   }
}
