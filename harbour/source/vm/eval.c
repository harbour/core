/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * EVAL() functions and DO command
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"

HB_FUNC( EVAL )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_BLOCK );

   if( pItem )
   {
      USHORT uiPCount = hb_pcount();
      USHORT uiParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pItem );
      /* NOTE: hb_param(0 function cannot be used for parameter access
       * because we need to pass the references too.
       * hb_param() is dereferencing the passed parameters
      */
      for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
        hb_vmPush( hb_stack.pBase + 1 + uiParam );
      hb_vmDo( uiPCount - 1 );
   }
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, "EVAL" );
}

