/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PROCNAME(), PROCLINE() and PROCFILE() functions
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB_PROCFILE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"

/*  $DOC$
 *  $FUNCNAME$
 *      PROCNAME()
 *  $CATEGORY$
 *      Misc
 *  $ONELINER$
 *      Gets the name of the current function on the stack
 *  $SYNTAX$
 *      PROCNAME( <nLevel> ) --> <cProcName>
 *  $ARGUMENTS$
 *      <nLevel> is the function level required.
 *  $RETURNS$
 *      Return the name of the function that it is being executed.
 *  $DESCRIPTION$
 *      This function look at the top of the stack and gets the current
 *      executed function if no arguments are passed. Otherwise it returns
 *      the name of the function or procedure at <nLevel>.
 *  $EXAMPLES$
 *      See Test
 *  $TESTS$
 *      This test will show the functions and procedures in stack
 *      before executing it.
 *      function Test()
 *         LOCAL n := 1
 *         while !Empty( ProcName( n ) )
 *            ? ProcName( n++ )
 *         end do
 *      return nil
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      PROCNAME() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      PROCLINE(),PROCFILE()
 *  $END$
 */

HARBOUR HB_PROCNAME( void )
{
   int iLevel = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   if( ( iLevel == -1 ) )
   {
      if( ( pBase + 1 )->type == IT_ARRAY )  /* it is a method name */
      {
         char * szProcName;

         szProcName = ( char * ) hb_xgrab( strlen( hb_objGetClsName( pBase + 1 ) ) + 1 +
                                strlen( pBase->item.asSymbol.value->szName ) + 1 );
         strcpy( szProcName, hb_objGetClsName( pBase + 1 ) );
         strcat( szProcName, ":" );
         strcat( szProcName, pBase->item.asSymbol.value->szName );
         hb_retc( szProcName );
         hb_xfree( ( void * ) szProcName );
      }
      else
         hb_retc( pBase->item.asSymbol.value->szName );
   }
   else
      hb_retc( "" );
}

/*  $DOC$
 *  $FUNCNAME$
 *      PROCLINE()
 *  $CATEGORY$
 *      Misc
 *  $ONELINER$
 *      Gets the line number of the current function on the stack
 *  $SYNTAX$
 *      PROCLINE( <nLevel> ) --> <nLine>
 *  $ARGUMENTS$
 *      <nLevel> is the function level required.
 *  $RETURNS$
 *      Return the line number of the function that it is being executed.
 *  $DESCRIPTION$
 *      This function look at the top of the stack and gets the current
 *      line number of executed function if no arguments are passed.
 *      Otherwise it returns the line number of the function or procedure
 *      at <nLevel>.
 *  $EXAMPLES$
 *      See Test
 *  $TESTS$
 *      function Test()
 *         ? ProcLine( 0 )
 *         ? ProcName( 2 )
 *      return nil
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      PROCLINE() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      PROCNAME() PROCFILE()
 *  $END$
 */

HARBOUR HB_PROCLINE( void )
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

/*  $DOC$
 *  $FUNCNAME$
 *      PROCFILE()
 *  $CATEGORY$
 *      Misc
 *  $ONELINER$
 *      This function allways returns an empty string.
 *  $SYNTAX$
 *      PROCFILE( <xExp> ) --> <cEmptyString>
 *  $ARGUMENTS$
 *      <xExp> is any valid type.
 *  $RETURNS$
 *      Return and empty string
 *  $DESCRIPTION$
 *      This function is added to the RTL for full compatibility. It allways
 *      returns an empty string.
 *  $EXAMPLES$
 *      ? ProcFile()
 *  $TESTS$
 *      function Test()
 *         ? ProcFile()
 *         ? ProcFile( NIL )
 *         ? ProcFile( 2 )
 *      return nil
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      PROCFILE() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      PROCNAME() PROCLINE()
 *  $END$
 */

/* NOTE: Clipper undocumented function, which always returns an empty
         string. */

HARBOUR HB_PROCFILE( void )
{
   hb_retc( "" );
}
