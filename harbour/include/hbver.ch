/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for HB_VERSION() function
 *
 * Copyright 2008 Viktor Szakats <viktor.szakats@syenar.hu>
 * www - http://www.harbour-project.org
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

#ifndef HB_VER_CH_
#define HB_VER_CH_

/* hb_version() parameters. */
#define HB_V_HARBOUR         0  /* default */
#define HB_V_COMPILER        1
#define HB_V_VER_MAJOR       2
#define HB_V_VER_MINOR       3
#define HB_V_VER_REV         4
#define HB_V_VER_STATUS      5
#define HB_V_VER_COUNT       6
#define HB_V_DATE_TIME       7
#define HB_V_DATE            8
#define HB_V_TIME            9
#define HB_V_PCODE_VER       10
#define HB_V_PCODE_VER_STR   11
#define HB_V_CHANGELOG_LAST  12
#define HB_V_CHANGELOG_REV   13
#define HB_V_FLAG_HARBOUR    14
#define HB_V_FLAG_C          15
#define HB_V_FLAG_LINKER     16
#define HB_V_BITWIDTH        17
#define HB_V_ENDIANNESS      18

/* hb_version( HB_V_ENDIANNESS ) return values. */
#define HB_V_ENDIAN_LITTLE   1
#define HB_V_ENDIAN_BIG      2
#define HB_V_ENDIAN_PDP      3

#endif /* HB_VER_CH_ */
