/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for version information
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#ifndef HB_VER_H_
#define HB_VER_H_

#if defined(SIMPLEX)
   #define HB_VER_LEX "SimpLex"
#else
   #define HB_VER_LEX "Flex"
#endif
#define HB_VER_MAJOR    0       /* Major version number */
#define HB_VER_MINOR    36      /* Minor version number */
#define HB_VER_REVISION " "     /* Revision letter */
#define HB_VER_BUILD    36      /* Build number */
#define HB_VER_YEAR     2001    /* Build year */
#define HB_VER_MONTH    02      /* Build month */
#define HB_VER_DAY      20      /* Build day */

#endif /* HB_VER_H_ */
