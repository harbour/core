/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for MEMOEDIT() function
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

#ifndef _MEMOEDIT_CH
#define _MEMOEDIT_CH

/* User callback status modes */
#define ME_IDLE         0       /* Idle, all keys processed        */
#define ME_UNKEY        1       /* Unknown key, memo unaltered     */
#define ME_UNKEYX       2       /* Unknown key, memo altered       */
#define ME_INIT         3       /* Initialization mode             */
#define ME_REQUEST      4       /* Memoedit requests an input from */
                                /* the user function, e.g. after   */
                                /* ME_PASTE                        */ /* Xbase++ extension */

/* User callback return codes */
#define ME_DEFAULT      0       /* Perform default action          */
#define ME_IGNORE       32      /* Ignore unknown key              */
#define ME_DATA         33      /* Treat unknown key as data       */
#define ME_TOGGLEWRAP   34      /* Toggle word-wrap mode           */
#define ME_TOGGLESCROLL 35      /* Toggle scrolling mode           */
#define ME_WORDRIGHT    100     /* Perform word-right operation    */
#define ME_BOTTOMRIGHT  101     /* Perform bottom-right operation  */
#define ME_PASTE        110     /* Paste string into buffer        */ /* Xbase++ extension */

/* NOTE: Return codes 1-31 cause MEMOEDIT() to perform the */
/*       edit action corresponding to the key whose value is returned. */

#endif /* _MEMOEDIT_CH */
