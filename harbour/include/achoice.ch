/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for ACHOICE() function
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

#ifndef _ACHOICE_CH
#define _ACHOICE_CH

/* User callback status codes */
#define AC_IDLE         0       /* Idle                                        */
#define AC_HITTOP       1       /* Attempt to move above the first item        */
#define AC_HITBOTTOM    2       /* Attempt to move below the last item         */
#define AC_EXCEPT       3       /* Keystroke exception                         */
#define AC_NOITEM       4       /* There's no selectable item                  */

/* User callback return codes */
#define AC_ABORT        0       /* Abort ACHOICE() and return zero             */
#define AC_SELECT       1       /* Select current item and return it's index   */
#define AC_CONT         2       /* Continue ACHOICE()                          */
#define AC_GOTO         3       /* Search first chars for the last pressed key */
#define AC_REDRAW       4       /* Redraw ACHOICE()                            */

#endif /* _ACHOICE_CH */
