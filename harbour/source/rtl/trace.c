/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Clipper tracing API.
 *
 * Copyright 1999 Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>
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


/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB_HB_TRACEENABLE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"

HARBOUR HB_HB_TRACEENABLE( void )
{
   hb_retl( hb_traceenabled() );

   if( ISLOG( 1 ) )
      hb_traceenable( hb_parl( 1 ) ? 1 : 0 );
}

HARBOUR HB_HB_TRACEON( void )
{
   hb_traceon();
}

HARBOUR HB_HB_TRACEOFF( void )
{
   hb_traceoff();
}

HARBOUR HB_HB_TRACELEVEL( void )
{
   hb_retni( hb_tracelevel( ISNUM( 1 ) ? hb_parni( 1 ) : -1 ) );
}
