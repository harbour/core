/*
  $Id$
*/

/*
 * Harbour Project source code:
 * HB_BTree Harbour API header.
 *
 * Copyright 2002-2010 April White <april@users.sourceforge.net>
 * www - http://harbour-project.org
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

#ifndef HB_BTREE_CH
#define HB_BTREE_CH

/* NOTE: This file is also used by C code. */

/* creation/open control flags */
#define HB_BTREE_READONLY    1  /* for new: creates file w/ RO attrib; for create: opens file for read */
#define HB_BTREE_EXCLUSIVE   2
#define HB_BTREE_SHARED      4

#define HB_BTREE_UNIQUE    128
#define HB_BTREE_CASELESS  256
#define HB_BTREE_INMEMORY  512

#define HB_BTREEINFO_ALL       0
#define HB_BTREEINFO_FILENAME  1
#define HB_BTREEINFO_PAGESIZE  2
#define HB_BTREEINFO_KEYSIZE   3
#define HB_BTREEINFO_MAXKEYS   4
#define HB_BTREEINFO_MINKEYS   5
#define HB_BTREEINFO_FLAGS     6
#define HB_BTREEINFO_KEYCOUNT  7
#define HB_BTREEINFO__SIZE     7  /*  do not use!  */

/* error codes (SubCode) */
#define HB_BTREE_EC_UNKNOWN      1
#define HB_BTREE_EC_INVALIDARG   2
#define HB_BTREE_EC_TREEHANDLE   3
#define HB_BTREE_EC_WRITEERROR   4
#define HB_BTREE_EC_STACKSKIP    5


/* TODO: add commands/translates */

#endif
