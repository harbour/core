/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FIELDBLOCK() and FIELDWBLOCK() functions
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    FIELDBLOCK() documentation
 *    FIELDWBLOCK() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      FIELDBLOCK()
 *  $CATEGORY$
 *      Code Block
 *  $ONELINER$
 *      Return a code block that sets/gets a value for a given field
 *  $SYNTAX$
 *      FIELDBLOCK( <cFieldName> ) --> bFieldBlock
 *  $ARGUMENTS$
 *      <cFieldName> is a string that contain the field name.
 *  $RETURNS$
 *      FIELDBLOCK() return a code block that when evaluate could retrieve
 *      field value or assigning a new value to the field. If <cFieldName>
 *      is not specified or from type other than character, FIELDBLOCK()
 *      return NIL.
 *  $DESCRIPTION$
 *      FIELDBLOCK() return a code block that sets/gets the value of field.
 *      When this code block is evaluated without any parameters passed then
 *      it returns the current value of the given field. If the code block
 *      is evaluated with a parameter, than its value is used to set a new
 *      value to the field, this value is also return by the block. If the
 *      block is evaluate and there is no field with the name <cFieldName>
 *      in the current work area, the code block return NIL.
 *
 *      Note that FIELDBLOCK() works on the current work area, if you need
 *      a specific work area code block use FIELDWBLOCK() instead.
 *  $EXAMPLES$
 *      // open a file named Test that have a field named "name"
 *      LOCAL bField
 *      bFiled := FIELDBLOCK( "name" )
 *      USE Test
 *      ? 'Original value of field "name" :', EVAL( bField )
 *      EVAL( bField, "Mr X new name" )
 *      ? 'New value for the field "name" :', EVAL( bField )
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      If the block is evaluate and there is no field with the name
 *      <cFieldName> in the current work area, the code block return NIL.
 *
 *      CA-Clipper would raise BASE/1003 error if the field does not exist.
 *  $SEEALSO$
 *      EVAL(),FIELDWBLOCK(),MEMVARBLOCK()
 *  $END$
 */

FUNCTION FIELDBLOCK( cFieldName )

   IF ISCHARACTER( cFieldName )
      RETURN {| x | iif( x == NIL, FieldGet( FieldPos( cFieldName ) ),;
                                   FieldPut( FieldPos( cFieldName ), x ) ) }
   ENDIF

   RETURN NIL

/*  $DOC$
 *  $FUNCNAME$
 *      FIELDWBLOCK()
 *  $CATEGORY$
 *      Code Block
 *  $ONELINER$
 *      Return a sets/gets code block for field in a given work area
 *  $SYNTAX$
 *      FIELDWBLOCK( <cFieldName>, <nWorkArea> ) --> bFieldBlock
 *  $ARGUMENTS$
 *      <cFieldName> is a string that contain the field name.
 *
 *      <nWorkArea> is the work area number in which <cFieldName> exist.
 *  $RETURNS$
 *      FIELDWBLOCK() return a code block that when evaluate could retrieve
 *      field value or assigning a new value for a field in a given work
 *      area. If <cFieldName> is not specified or from type other than
 *      character, or if <nWorkArea> is not specified or is not numeric
 *      FIELDWBLOCK() return NIL.
 *  $DESCRIPTION$
 *      FIELDWBLOCK() return a code block that sets/gets the value of field
 *      from a given work area. When this code block is evaluated without
 *      any parameters passed then it returns the current value of the given
 *      field. If the code block is evaluated with a parameter, than its
 *      value is used to set a new value to the field, this value is also
 *      return by the block. If the block is evaluate and there is no field
 *      with the name <cFieldName> in work area number <nWorkArea>, the code
 *      block return NIL.
 *  $EXAMPLES$
 *      LOCAL bField
 *      // this block work on the field "name" that exist on work area 2
 *      bFiled := FIELDBLOCK( "name", 2 )
 *      // open a file named One in work area 1
 *      // that have a field named "name"
 *      SELECT 1
 *      USE One
 *      // open a file named Two in work area 2
 *      // it also have a field named "name"
 *      SELECT 2
 *      USE Two
 *      SELECT 1
 *      ? "Original names: ", One->name, Two->name
 *      ? "Name value for file Two :", EVAL( bField )
 *      EVAL( bField, "Two has new name" )
 *      ? "and now: ", One->name, Two->name
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      If the block is evaluate and there is no field with the name
 *      <cFieldName> in the given work area, the code block return NIL.
 *
 *      CA-Clipper would raise BASE/1003 error if the field does not exist.
 *  $SEEALSO$
 *      EVAL(),FIELDBLOCK(),MEMVARBLOCK()
 *  $END$
 */

FUNCTION FIELDWBLOCK( cFieldName, nWorkArea )

   IF ISCHARACTER( cFieldName ) .AND. ISNUMBER( nWorkArea )
      RETURN {| x | iif( x == NIL, ( nWorkArea )->( FieldGet( FieldPos( cFieldName ) ) ),;
                                   ( nWorkArea )->( FieldPut( FieldPos( cFieldName ), x ) ) ) }
   ENDIF

   RETURN NIL
