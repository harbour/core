/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for MEMORY() function
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

#ifndef HB_MEMORY_CH_
#define HB_MEMORY_CH_

/* Parameters for Memory() function */

/* Standard */
#define HB_MEM_CHAR             0   /* Free Variable Space (KB) */
#define HB_MEM_BLOCK            1   /* Largest String (KB) */
#define HB_MEM_RUN              2   /* RUN Memory (KB) */

/* CA-Cl*pper undocumented */
#define HB_MEM_VM               3   /* Virtual Memory (KB) */
#define HB_MEM_EMS              4   /* Free Expanded Memory (KB) (?) */
#define HB_MEM_FM             101   /* Fixed Memory/Heap (KB) (?) */
#define HB_MEM_FMSEGS         102   /* Segments in Fixed Memory/Heap (?) */
#define HB_MEM_SWAP           103   /* Free Swap Memory (KB) */
#define HB_MEM_CONV           104   /* Free Conventional (KB) */
#define HB_MEM_EMSUSED        105   /* Used Expanded Memory (KB) (?) */

/* Harbour extensions */
#define HB_MEM_USED          1001   /* Memory used (bytes) */
#define HB_MEM_USEDMAX       1002   /* Maximum memory used (bytes) */
#define HB_MEM_STACKITEMS    1003   /* Total items on the stack */
#define HB_MEM_STACK         1004   /* Total memory size used by the stack (bytes) */
#define HB_MEM_STACK_TOP     1005   /* Total items currently on the stack */

#endif /* HB_MEMORY_CH_ */

