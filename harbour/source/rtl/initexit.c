/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __QUIT() function
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
#include "ctoharb.h"

/*  $DOC$
 *  $FUNCNAME$
 *      __QUIT()
 *  $CATEGORY$
 *      Events
 *  $ONELINER$
 *      Terminates an application.
 *  $SYNTAX$
 *      __QUIT() --> NIL
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      NIL
 *  $DESCRIPTION$
 *      This function terminates the current application and returns
 *      to the system.
 *  $EXAMPLES$
 *      See Test
 *  $TESTS$
 *      function EndApp( lYesNo )
 *         if lYesNo
 *            __Quit()
 *         endif
 *      return nil
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      __QUIT() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      QUIT 
 *  $END$
 */

HARBOUR HB___QUIT( void )
{
   hb_vmRequestQuit();
}
