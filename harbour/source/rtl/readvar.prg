/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * READVAR() function
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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
 *      READVAR()
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Return variable name of current GET or MENU
 *  $SYNTAX$
 *      READVAR( [<cVarName>] ) --> cOldVarName
 *  $ARGUMENTS$
 *      <cVarName> is a new variable name to set.
 *  $RETURNS$
 *      READVAR() return the old variable name. If no variable previously
 *      was set, READVAR() return "".
 *  $DESCRIPTION$
 *      READVAR() is set inside a READ or MENU TO command to hold the
 *      uppercase name of the GET / MENU TO variable, and re-set back to old
 *      value when those commands finished. You should not normally set a
 *      variable name but rather use it to retrieve the name of a GET
 *      variable when executing a VALID or WHEN clause, or during SET KEY
 *      execution and you are inside a READ or MENU TO.
 *  $EXAMPLES$
 *      // display a menu, press F1 to view the MENU TO variable name
 *      CLS
 *      @ 1, 10 PROMPT "blood sucking insect that infect beds   "
 *      @ 2, 10 PROMPT "germ; virus infection                   "
 *      @ 3, 10 PROMPT "defect; snag; (source of) malfunctioning"
 *      @ 4, 10 PROMPT "small hidden microphone                 "
 *      @ 6, 10 SAY "(Press F1 for a hint)"
 *      SET KEY 28 TO ShowVar
 *      MENU TO What_Is_Bug
 *
 *      PROCEDURE ShowVar
 *      ALERT( READVAR() )        // WHAT_IS_BUG in red ALERT() box
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      READVAR() works exactly like CA-Clipper's READKEY(), note however,
 *      that the <cVarName> parameter is not documented and used internally
 *      by CA-Clipper.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      @...GET  comm.ngo:'@...PROMPT'  comm.ngo:'MENU TO'  READ  comm.ngo:'SET KEY'  datai.ngo:__AtPrompt()  datai.ngo:__MenuTo()
 *  $END$
 */

#include "common.ch"

FUNCTION ReadVar( cVarName )
   STATIC s_cVarName := ""

   LOCAL cOldVarName := s_cVarName

   IF ISCHARACTER( cVarName )
      s_cVarName := cVarName
   ENDIF

   RETURN cOldVarName
