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

/*
   File name:
   This program is part of sxApi Library
   Copyright: Andi Jahja 2003
   Last update: 2003-07-03
 */
#include "sxapi.h"

#define COMMA_DELIMITED 21
#define SDF_DELIMITED   22

HB_FUNC( SX_COPYFILE )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_COPYFILE" );
   else
   {
      if( HB_ISCHAR( 1 ) )
      {
         WORD iWorkArea = SX_DUMMY_NUMBER;

         if( ! HB_ISNIL( 2 ) )
            iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

         hb_retl( sx_CopyFile( ( PBYTE ) hb_parc( 1 ) ) );

         if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
            sx_Select( iWorkArea );
      }
      else
         hb_retl( HB_FALSE );
   }
}

HB_FUNC( SX_COPYFILETEXT )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_COPYFILETEXT" );

   if( HB_ISCHAR( 1 ) )
   {
      WORD  iWorkArea  = SX_DUMMY_NUMBER;
      int   iDelimiter = COMMA_DELIMITED; /* Defaulted to Comma */

      if( ! HB_ISNIL( 3 ) )
         iWorkArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

      if( HB_ISNUM( 2 ) )
      {
         iDelimiter = hb_parni( 2 );
         if( iDelimiter < COMMA_DELIMITED && iDelimiter > SDF_DELIMITED )
            iDelimiter = COMMA_DELIMITED;
      }

      hb_retl( sx_CopyFileText( ( PBYTE ) hb_parc( 1 ), ( WORD ) iDelimiter ) );

      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
         sx_Select( iWorkArea );
   }
   else
      hb_retl( HB_FALSE );
}
