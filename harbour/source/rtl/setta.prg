/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SETTYPEAHEAD() undocumented function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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

/*  $DOC$
 *  $FUNCNAME$
 *      SETTYPEAHEAD()
 *  $CATEGORY$
 *      Enviroment
 *  $ONELINER$
 *      Sets the typeahead buffer to given size.
 *  $SYNTAX$
 *      SETTYPEAHEAD( <nSize> ) --> <nPreviousSize>
 *  $ARGUMENTS$
 *      <nSize> is a valid typeahead size.
 *  $RETURNS$
 *      The previous state of _SET_TYPEAHEAD
 *  $DESCRIPTION$
 *      This function sets the typeahead buffer to a valid given size as is
 *      Set( _SET_TYPEAHEAD ) where used.
 *  $EXAMPLES$
 *      // Sets typeahead to 12
 *      SetTypeahead( 12 )
 *  $TESTS$
 *      
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      SETTYPEAHEAD() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      __ACCEPT() datai.ngo:__INPUT()
 *  $END$
 */

FUNCTION SetTypeahead( nSize )
   RETURN Set( _SET_TYPEAHEAD, nSize )
