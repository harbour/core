/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for low-level object engine
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 JF Lefebvre <jfl@mafact.com> and RA Cuylen <rac@mafact.com>
 *    Many enhancements (scopes, class methods)
 *
 * See COPYING.txt for licensing terms.
 *
 */

/* NOTE: This file is also used by C code. */

#ifndef HB_OO_CH_
#define HB_OO_CH_

/* Used by objfunc.prg (__objGetMsgList) and classes.c (hb___msgClsSel()) */
#define HB_MSGLISTALL   0
#define HB_MSGLISTCLASS 1
#define HB_MSGLISTPURE  2

/* Method or Data attribute (nScope)*/
#define HB_OO_CLSTP_EXPORTED        1 /* No comment, default */
#define HB_OO_CLSTP_PROTECTED       2 /* Only usable from one of the object's method (even sublclassed object) */
#define HB_OO_CLSTP_HIDDEN          4 /* Only usable from one of the object's method (and not from sublclassed one) */
#define HB_OO_CLSTP_CTOR            8 /* Constructor  (Not yet used) */
#define HB_OO_CLSTP_READONLY       16 /* No comment */
#define HB_OO_CLSTP_SHARED         32 /* Allow a classvar (or classmethod) to be shared by all the subclasses.
                                         Not the default behaviour as each subclass will have its own copy by default. */
#define HB_OO_CLSTP_CLASS          64 /* message is class message not object */
#define HB_OO_CLSTP_SUPER         128 /* The related message is inherited from a superclass */
#define HB_OO_CLSTP_PERSIST       256 /* Message is persistent (PROPERTY) */
#define HB_OO_CLSTP_NONVIRTUAL    512 /* Non Virtual message - should not be covered
                                         by subclass(es) messages with the same name when executed
                                         from a given class message */
#define HB_OO_CLSTP_OVERLOADED   1024 /* message overload NONVIRTUAL one */
#define HB_OO_CLSTP_SYNC         2048 /* message synchronized by object or class mutex */

/* Message types */
#define HB_OO_MSG_METHOD        0
#define HB_OO_MSG_DATA          1
#define HB_OO_MSG_CLASSDATA     2
#define HB_OO_MSG_INLINE        3
#define HB_OO_MSG_VIRTUAL       4
#define HB_OO_MSG_SUPER         5
#define HB_OO_MSG_ONERROR       6
#define HB_OO_MSG_CLSMTHD       7 /* for the future */
#define HB_OO_MSG_ASSIGN        8
#define HB_OO_MSG_ACCESS        9
#define HB_OO_MSG_CLSASSIGN    10
#define HB_OO_MSG_CLSACCESS    11
#define HB_OO_MSG_REALCLASS    12
#define HB_OO_MSG_DESTRUCTOR   13
#define HB_OO_MSG_INITIALIZED  14 /* initialized class data: HB_OO_MSG_CLASSDATA */
#define HB_OO_MSG_PERFORM      15
#define HB_OO_MSG_DELEGATE     16

/* to make xHarbour users happy ;-) */
#define HB_OO_PROPERTY          32
#define HB_OO_MSG_PROPERTY      HB_OO_PROPERTY + HB_OO_MSG_DATA      /* Auto management of DATA */
#define HB_OO_MSG_CLASSPROPERTY HB_OO_PROPERTY + HB_OO_MSG_CLASSDATA /* Auto management of CLASSDATA */

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

/* Delegate messages */
#define HB_OO_DELEG_SYMBOL      1
#define HB_OO_DELEG_MESSAGE     2
#define HB_OO_DELEG_OBJECT      3
#define HB_OO_DELEG_SCOPE       4

#endif /* HB_OO_CH_ */
