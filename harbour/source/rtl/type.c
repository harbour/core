/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TYPE() function
 *
 * Copyright 1999 Ryszard Glab
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

/*  $DOC$
 *  $FUNCNAME$
 *      TYPE()
 *  $CATEGORY$
 *      Misc
 *  $ONELINER$
 *      Retrieves the type of an expression
 *  $SYNTAX$
 *      TYPE( <cExp> ) --> <cReturnType>
 *  $ARGUMENTS$
 *      <cExp> must be a character expression.
 *  $RETURNS$
 *      Returns a string indicating the type of the passed expression.
 *
 *    "A"   - array
 *    "B"   - block
 *    "C"   - string
 *    "D"   - date
 *    "L"   - logical
 *    "M"   - memo
 *    "N"   - numeric
 *    "O"   - object
 *    "U"   - NIL, local, or static variable, or not linked-in function
 *    "UE"  - syntax error in the expression or invalid arguments
 *    "UI"  - function with non-reserved name was requested
 *  $DESCRIPTION$
 *      This function returns a string which represents the data type
 *      of the argument. The argument can be any valid Harbour expression.
 *      If there is a syntax error in passed expression then "UE" is returned.
 *      If there is a call for any non-reserved Harbour function then "UI"
 *      is returned (in other words there is no call for passed UDF function
 *      during a data type determination - this is Clipper compatible
 *      behavior). Additionally if requested user defined function is not 
 *      linked into executable then "U" is returned.
 *
 *      The data type of expression is checked by invoking a macro compiler
 *      and by evaluation of generated code (if there is no syntax errors).
 *      This causes that TYPE() cannot determine a type of local or static
 *      variables - only symbols visible at runtime can be checked.
 *
 *      Notice the subtle difference between TYPE and VALTYPE functions.
 *      VALTYPE() function doesn't call a macro compiler - it simply checks
 *      the type of passed argument of any type. TYPE() requires a string
 *      argument with a valid Harbour expression - the data type of this
 *      expression is returned.
 * 
 *    Notes:
 *      - Incompatibility with Clipper:
 *          In the following code:
 * 
 *          PRIVATE lCond := 0
 *          ? TYPE( "IIF( lCond, 'true', MyUDF() )" )
 * 
 *          Clipper will print "UE" - in Harbour the output will be "UI"
 *
 *      - if "UI" is returned then the syntax of the expression is
 *          correct. However invalid arguments can be passed to
 *          function/procedure that will cause runtime errors during
 *          evaluation of expression.
 *
 *  $EXAMPLES$
 *       ? TYPE( "{ 1, 2 }" )                            //prints "A"
 *       ? TYPE( "IIF(.T., SUBSTR('TYPE',2,1), .F.)" )   //prints "C"
 *       ? TYPE( "AT( 'OK', MyUDF())>0" )                //prints "UI"
 *       ? TYPE( "{ 1, 2 }[ 5 ]" )                       //prints "UE"
 *
 *       //--------------------------------------------------------
 *
 *       LOCAL c
 *       PRIVATE a:="A", b:="B"
 *       ? TYPE( "a + b + c" )     //prints: "U" ('C' variable is a local one)
 *
 *       //--------------------------------------------------------
 *
 *       LOCAL cFilter := SPACE( 60 )
 *       ACCEPT "Enter filter expression:" TO cFilter
 *       IF( TYPE( cFilter ) $ "CDLMN" ) )
 *          // this is a valid expression
 *          SET FILTER TO &cFilter
 *       ENDIF
 *
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      VALTYPE()
 *  $END$
 */

HARBOUR HB_TYPE( void )
{
   HB_ITEM_PTR pItem = hb_param( 1, IT_STRING );

   if( pItem )
   {
      hb_retc( hb_macroGetType( pItem ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1121, NULL, "TYPE" );
   }
}
