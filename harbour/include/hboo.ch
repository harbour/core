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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 JF Lefebvre <jfl@mafact.com> and RA Cuylen <rac@mafact.com>
 *    Many enhancements (scopes, class methods)
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: This file is also used by C code. */

#ifndef HB_OO_CH_
#define HB_OO_CH_

/* Method or Data attribute (nScope)*/
#define HB_OO_CLSTP_EXPORTED    1 /* No comment, default */
#define HB_OO_CLSTP_PROTECTED   2 /* Only usable from one of the object's method (even sublclassed object) */
#define HB_OO_CLSTP_HIDDEN      4 /* Only usable from one of the object's method (and not from sublclassed one) */
#define HB_OO_CLSTP_CTOR        8 /* Constructor  (Not yet used) */
#define HB_OO_CLSTP_READONLY   16 /* No comment */
#define HB_OO_CLSTP_SHARED     32 /* Allow a classvar (or classmethod) to be shared by all the subclasses.
                                     Not the default behaviour as each subclass will have its own copy by default. */
#define HB_OO_CLSTP_CLASS      64 /* The related message is a superobject call, uidata is the superclass handle
                                     pInitValue contain one superclass object instance (absolutely needed for Inline msg and class data) */
#define HB_OO_CLSTP_SUPER     128 /* The related message is inherited from a superclass */

/* Message types */
#define HB_OO_MSG_METHOD        0
#define HB_OO_MSG_DATA          1
#define HB_OO_MSG_CLASSDATA     2
#define HB_OO_MSG_INLINE        3
#define HB_OO_MSG_VIRTUAL       4
#define HB_OO_MSG_SUPER         5
#define HB_OO_MSG_ONERROR       6
#define HB_OO_MSG_CLSMTHD       7 /* for the future */

/* Data */
#define HB_OO_DATA_SYMBOL       1
#define HB_OO_DATA_VALUE        2
#define HB_OO_DATA_TYPE         3
#define HB_OO_DATA_SCOPE        4

/* ClassData */
#define HB_OO_CLSD_SYMBOL       1
#define HB_OO_CLSD_VALUE        2
#define HB_OO_CLSD_TYPE         3
#define HB_OO_CLSD_SCOPE        4

/* Method */
#define HB_OO_MTHD_SYMBOL       1
#define HB_OO_MTHD_PFUNCTION    2
#define HB_OO_MTHD_SCOPE        3

/* ClassMethod */ /* for the future */
#define HB_OO_CLSM_SYMBOL       1
#define HB_OO_CLSM_PFUNCTION    2
#define HB_OO_CLSM_SCOPE        3

#endif /* HB_OO_CH_ */

