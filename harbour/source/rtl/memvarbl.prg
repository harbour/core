/* 
 * $Id$
 */

/*
 * Harbour Project source code:
 * MEMVARBLOCK() function
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

#include "common.ch"
#include "memvars.ch"

/*  $DOC$
 *  $FUNCNAME$
 *	MEMVARBLOCK
 *  $CATEGORY$
 *	variables management
 *  $ONELINER$
 *	Returns a codeblock that sets/gets a value of memvar variable
 *  $SYNTAX$
 *	MEMVARBLOCK( <cMemvarName> )
 *  $ARGUMENTS$
 *	<cMemvarName> - a string that contains the name of variable
 *  $RETURNS$
 *	a codeblock that sets/get the value of variable
 *  $DESCRIPTION$
 *	This function returns a codeblock that sets/gets the value of
 *	PRIVATE or PUBLIC variable. When this codeblock is evaluated
 *	without any parameters passed then it returns the current value
 *	of given variable. If the second parameter is passed for
 *	the codeblock evaluation then its value is used to set the new
 *	value of given variable - the passed value is also returned
 *	as a value of the codeblock evaluation.
 *  $EXAMPLES$
 *	PROCEDURE MAIN()
 *	LOCAL cbSetGet
 *	PUBLIC xPublic
 *	
 *	cbSetGet = MEMVARBLOCK( "xPublic" )
 *	EVAL( cbSetGet, "new value" )
 *	? "Value of xPublic variable", EVAL( cbSetGet )
 *
 *	RETURN
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *	__MVGET, __MVPUT
 *  $END$
 */

FUNCTION MEMVARBLOCK( cMemvar )

   IF ISCHARACTER( cMemvar ) .AND. __mvSCOPE( cMemvar ) > MV_ERROR
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      RETURN {| x | iif( x == NIL, __mvGET( cMemvar ), __mvPUT( cMemvar, x ) ) }
#else
      RETURN {| x | iif( PCount() == 0, __mvGET( cMemvar ), __mvPUT( cMemvar, x ) ) }
#endif
   ENDIF

   RETURN NIL
