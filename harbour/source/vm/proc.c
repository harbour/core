/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PROCNAME(), PROCLINE() and PROCFILE() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    PROCFILE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"

HB_FUNC( PROCNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 2 ];

   hb_retc( hb_procname( hb_parni( 1 ) + 1, szName ) );
}

HB_FUNC( PROCLINE )
{
   int iLevel = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   if( iLevel == -1 )
      hb_retni( pBase->item.asSymbol.lineno );
   else
      hb_retni( 0 );
}

#ifdef HB_C52_UNDOC

/* NOTE: Clipper undocumented function, which always returns an empty
         string. [vszakats] */

HB_FUNC( PROCFILE )
{
   hb_retc( "" );
}

#endif

/* NOTE: szName size must be an at least:
         HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 2 [vszakats] */

char * hb_procname( int iLevel, char * szName )
{
   PHB_ITEM pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   if( iLevel == -1 )
   {
      if( ( pBase + 1 )->type == HB_IT_ARRAY )  /* it is a method name */
      {
         strcpy( szName, hb_objGetClsName( pBase + 1 ) );
         strcat( szName, ":" );
         strcat( szName, pBase->item.asSymbol.value->szName );
      }
      else
         strcpy( szName, pBase->item.asSymbol.value->szName );
   }
   else
      strcpy( szName, "" );

   return szName;
}

