/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SAVESCREEN(), RESTSCREEN() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( SAVESCREEN )
{
   USHORT uiTop    = ISNUM( 1 ) ? hb_parni( 1 ) : 0;      
   USHORT uiLeft   = ISNUM( 2 ) ? hb_parni( 2 ) : 0;      
   USHORT uiBottom = ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow();
   USHORT uiRight  = ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol();

   USHORT uiSize;
   void * pBuffer;

   hb_gtRectSize( uiTop, uiLeft, uiBottom, uiRight, &uiSize );
   pBuffer = hb_xgrab( uiSize );

   hb_gtSave( uiTop, uiLeft, uiBottom, uiRight, pBuffer );
   hb_retclen( ( char * ) pBuffer, uiSize );

   hb_xfree( ( char * ) pBuffer );
}

HB_FUNC( RESTSCREEN )
{
   if( ISCHAR( 5 ) )
      hb_gtRest( ISNUM( 1 ) ? hb_parni( 1 ) : 0,            
                 ISNUM( 2 ) ? hb_parni( 2 ) : 0,            
                 ISNUM( 3 ) ? hb_parni( 3 ) : hb_gtMaxRow(),
                 ISNUM( 4 ) ? hb_parni( 4 ) : hb_gtMaxCol(),
                 ( void * ) hb_parc( 5 ) );
}

