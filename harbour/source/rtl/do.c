/*
 * $Id$
    Harbour Project source code

   This file is a part of Harbour Runtime Library and it contains code
   that defined DO function ussed in DO <proc> WITH statement.

   Copyright (C) 1999 Ryszard Glab
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
 */

#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "ctoharb.h"

HARBOUR HB_DO( void )
{
   PHB_ITEM pItem;

   pItem =hb_param( 1, IT_ANY );
   if( IS_STRING(pItem) )
   {
      PHB_DYNS pDynSym;

      pDynSym =hb_dynsymGet( pItem->item.asString.value );
      if( pDynSym )
      {
         int i;

         PushSymbol( pDynSym->pSymbol );
         PushNil();
         for( i = 2; i <= hb_pcount(); i++ )
            Push( hb_param( i, IT_ANY ) );
         Do( hb_pcount() - 1 );
      }
      else
         hb_errorRT_BASE( EG_NOFUNC, 1001, NULL, pItem->item.asString.value );
   }
   else if( IS_BLOCK(pItem) )
   {
      int i;

      PushSymbol( &symEval );
      Push( pItem );
      for( i = 2; i <= hb_pcount(); i++ )
         Push( hb_param( i, IT_ANY ) );
      Do( hb_pcount() - 1 );
   }
   else if( IS_SYMBOL(pItem) )
   {
      int i;

      PushSymbol( pItem->item.asSymbol.value );
      PushNil();
      for( i = 2; i <= hb_pcount(); i++ )
         Push( hb_param( i, IT_ANY ) );
      Do( hb_pcount() - 1 );
   }
   else
      hb_errorRT_BASE( EG_ARG, 3012, NULL, "DO" );
}
