/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DO command/function
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
#include "hbstack.h"

/* NOTE: DO() as a function is a Harbour extension. [vszakats] */

HB_FUNC( DO )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( HB_IS_STRING( pItem ) )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pItem ) );

      if( pDynSym )
      {
         USHORT uiPCount = hb_pcount();
         USHORT uiParam;

         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();
         for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
            hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
         hb_vmDo( uiPCount - 1 );
      }
      else
      {
         PHB_ITEM pArgsArray = hb_arrayFromParams( hb_stack.pBase );

         hb_errRT_BASE( EG_NOFUNC, 1001, NULL, hb_itemGetCPtr( pItem ), 1, pArgsArray );
         hb_itemRelease( pArgsArray );
      }
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      USHORT uiPCount = hb_pcount();
      USHORT uiParam;

      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pItem );
      for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
      hb_vmDo( uiPCount - 1 );
   }
   else if( HB_IS_SYMBOL( pItem ) )
   {
      USHORT uiPCount = hb_pcount();
      USHORT uiParam;

      hb_vmPushSymbol( pItem->item.asSymbol.value );
      hb_vmPushNil();
      for( uiParam = 2; uiParam <= uiPCount; uiParam++ )
         hb_vmPush( hb_param( uiParam, HB_IT_ANY ) );
      hb_vmDo( uiPCount - 1 );
   }
   else
   {
      PHB_ITEM pArgsArray = hb_arrayFromParams( hb_stack.pBase );

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "DO", 1, pArgsArray );
      hb_itemRelease( pArgsArray );
   }
}

