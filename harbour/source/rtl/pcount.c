/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PCOUNT() function
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

#include "extend.h"

/*  $DOC$
 *  $FUNCNAME$
 *      PCOUNT
 *  $CATEGORY$
 *
 *  $ONELINER$
 *      Retrieves the number of arguments passed to a function.
 *  $SYNTAX$
 *      PCOUNT() --> <nArgs>
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      Returns a number that indicates the number of arguments
 *      passed to a function or procedure.
 *  $DESCRIPTION$
 *      This function is useful to check if a function or procedure
 *      has received the required number of arguments.
 *  $EXAMPLES$
 *      See Test
 *  $TESTS$
 *      function Test( xExp )
 *         if PCount() == 0
 *            ? "This function needs a parameter"
 *         else
 *            ? xExp
 *         endif
 *      return nil
  *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      PCOUNT() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      PVALUE
 *  $END$
 */

HARBOUR HB_PCOUNT( void )
{
   /* Skip current function */
   PHB_ITEM pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase;

   hb_retni( pBase->item.asSymbol.paramcnt );
}
