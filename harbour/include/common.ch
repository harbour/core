/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for common macros
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

#ifndef HB_COMMON_CH_
#define HB_COMMON_CH_

/* Friendly logical aliases */
#define TRUE                    .T.
#define FALSE                   .F.
#define YES                     .T.
#define NO                      .F.

/* Type checking macros */
#translate ISNIL( <xValue> )       => ( <xValue> == NIL )
#translate ISARRAY( <xValue> )     => ( ValType( <xValue> ) == "A" )
#translate ISBLOCK( <xValue> )     => ( ValType( <xValue> ) == "B" )
#translate ISCHARACTER( <xValue> ) => ( ValType( <xValue> ) == "C" )
#translate ISDATE( <xValue> )      => ( ValType( <xValue> ) == "D" )
#translate ISLOGICAL( <xValue> )   => ( ValType( <xValue> ) == "L" )
#translate ISMEMO( <xValue> )      => ( ValType( <xValue> ) == "M" )
#translate ISNUMBER( <xValue> )    => ( ValType( <xValue> ) == "N" )
#translate ISOBJECT( <xValue> )    => ( ValType( <xValue> ) == "O" ) 

/* DEFAULT and UPDATE commands */
#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;
                                IF <v1> == NIL ; <v1> := <x1> ; END ;
                                [; IF <vn> == NIL ; <vn> := <xn> ; END ]

#command UPDATE <v1> IF <exp> TO <v2> => ;
                                IF <exp> ; <v1> := <v2> ; END

#endif /* HB_COMMON_CH_ */
