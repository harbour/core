/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for dynmaic PCODE modules (HRB) options
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.  If not, write to
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

/* NOTE: This file is also used by C code. */

#ifndef HB_HRB_CH_
#define HB_HRB_CH_

#define HB_HRB_BIND_DEFAULT      0x0   /* do not overwrite any functions, ignore
                                          public HRB functions if functions with
                                          the same names already exist in HVM */

#define HB_HRB_BIND_LOCAL        0x1   /* do not overwrite any functions
                                          but keep local references, so
                                          if module has public function FOO and
                                          this function exists also in HVM
                                          then the function in HRB is converted
                                          to STATIC one */

#define HB_HRB_BIND_OVERLOAD     0x2   /* overload all existing public functions */

#define HB_HRB_BIND_FORCELOCAL   0x3   /* convert all public functions to STATIC ones */

#define HB_HRB_BIND_MODEMASK     0x3   /* HB_HRB_BIND_* mode mask */

#define HB_HRB_BIND_LAZY         0x4   /* lazy binding with external public
                                          functions allows to load .hrb files
                                          with unresolved or cross function
                                          references */


#define HB_HRB_FUNC_PUBLIC       0x1   /* locally defined public functions */
#define HB_HRB_FUNC_STATIC       0x2   /* locally defined static functions */
#define HB_HRB_FUNC_LOCAL        0x3   /* locally defined functions */
#define HB_HRB_FUNC_EXTERN       0x4   /* external functions used in HRB module */


#endif /* HB_HRB_CH_ */
