/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Main Harbour initialization functions CLIPINIT()
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    CLIPINIT() documentation
 *    __SETHELPK() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbsetup.ch"
#include "inkey.ch"

ANNOUNCE SysInit

/* NOTE: For complete compatibility */
PROCEDURE CLIPPER520
   RETURN

#ifdef HB_COMPAT_C53

/* NOTE: For complete compatibility */
PROCEDURE CLIPPER530
   RETURN

#endif

/*  $DOC$
 *  $FUNCNAME$
 *      CLIPINIT()
 *  $CATEGORY$
 *      Internal
 *  $ONELINER$
 *      Initialize various Harbour sub-systems
 *  $SYNTAX$
 *      CLIPINIT() --> NIL
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      CLIPINIT() always return NIL.
 *  $DESCRIPTION$
 *      CLIPINIT() is one of the pre-defined INIT PROCEDURE and is executed
 *      at program startup. It declare an empty MEMVAR PUBLIC array called
 *      GetList that is going to be used by the Get system, It activate the
 *      default error handler, and (at least for the moment) call the
 *      function that set the default help key.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      It is said that CLIPINIT() should not call that function that set
 *      the default help key since CA-Clipper do it in some other place.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *       INIT PROCEDURE 
 *  $END$
 */

INIT PROCEDURE ClipInit

   MEMVAR GetList

   PUBLIC GetList := {}

   ErrorSys()

   /* TOFIX: In Clipper this function is not called from here CLIPINIT(). */
   /* NOTE: In Clipper __SETHELPK() is called *after* ERRORSYS(). */
   __SetHelpK()

   RETURN

/*  $DOC$
 *  $FUNCNAME$
 *      __SetHelpK()
 *  $CATEGORY$
 *      Internal
 *  $ONELINER$
 *      Set F1 as the default help key
 *  $SYNTAX$
 *      __SetHelpK() --> NIL
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      __SetHelpK() always return NIL.
 *  $DESCRIPTION$
 *      Set F1 to execute a function called HELP if such a function is
 *      linked into the program.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __SetHelpK() works exactly like CA-Clipper's __SetHelpK()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      __XHelp(),SET KEY,SETKEY()
 *  $END$
 */

PROCEDURE __SetHelpK

   SET KEY K_F1 TO __XHELP

   RETURN
