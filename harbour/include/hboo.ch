/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for low-level object engine
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

/* NOTE: This file is also used by C code. */

#ifndef HB_OO_CH_
#define HB_OO_CH_

/* Message types */
#define HB_OO_MSG_METHOD        0
#define HB_OO_MSG_DATA          1
#define HB_OO_MSG_CLASSDATA     2
#define HB_OO_MSG_INLINE        3
#define HB_OO_MSG_VIRTUAL       4
#define HB_OO_MSG_SUPER         5
#define HB_OO_MSG_ONERROR       6

/* Data */
#define HB_OO_DATA_SYMBOL       1
#define HB_OO_DATA_VALUE        2

/* ClassData */
#define HB_OO_CLSD_SYMBOL       1
#define HB_OO_CLSD_VALUE        2

#endif /* HB_OO_CH_ */

