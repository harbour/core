/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_PVALUE() function
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

#include "extend.h"
#include "errorapi.h"
#include "itemapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *      HB_PVALUE()
 *  $CATEGORY$
 *      Parameter Checks
 *  $ONELINER$
 *      Retrieves the value of an argument.
 *  $SYNTAX$
 *      HB_PVALUE( <nArg> ) --> <xExp>
 *  $ARGUMENTS$
 *      A number that indicates the argument to check.
 *  $RETURNS$
 *      Returns the value stored by an argument.
 *  $DESCRIPTION$
 *      This function is useful to check the value stored in an argument.
 *  $EXAMPLES$
 *      See Test
 *  $TESTS$
 *      function Test( nValue, cString )
 *         if PCount() == 2
 *            ? hb_PValue( 1 ), nValue
 *            ? hb_PValue( 2 ), cString
 *         endif
 *      return nil
  *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      HB_PVALUE() is a new function and hence not CA-Clipper compliant.
 *  $SEEALSO$
 *      PCOUNT()
 *  $END$
 */

HARBOUR HB_HB_PVALUE( void )
{
   USHORT uiParam = hb_parni( 1 );
   PHB_ITEM pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase; /* Skip function + self */

   if( uiParam && uiParam <= pBase->item.asSymbol.paramcnt ) /* Valid number */
      hb_itemReturn( pBase + 1 + uiParam );
   else
      hb_errRT_BASE( EG_ARG, 3011, NULL, "HB_PVALUE" );
}

