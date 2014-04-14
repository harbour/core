/*
 * Harbour Project source code:
 * Header file for hb_Version() function
 *
 * Copyright 2008 Viktor Szakats (vszakats.net/harbour)
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* hb_Version() parameters. */
#define HB_VERSION_HARBOUR          0  /* default */
#define HB_VERSION_COMPILER         1
#define HB_VERSION_MAJOR            2
#define HB_VERSION_MINOR            3
#define HB_VERSION_RELEASE          4
#define HB_VERSION_STATUS           5
#define HB_VERSION_REVISION         6
#define HB_VERSION_COMMIT_INFO      7
#define HB_VERSION_ID               8
#define HB_VERSION_PCODE_VER        9
#define HB_VERSION_PCODE_VER_STR    10
#define HB_VERSION_BUILD_DATE_STR   11
#define HB_VERSION_BUILD_DATE       12
#define HB_VERSION_BUILD_TIME       13
#define HB_VERSION_BUILD_PLAT       22
#define HB_VERSION_BUILD_COMP       23
#define HB_VERSION_FLAG_PRG         14
#define HB_VERSION_FLAG_C           15
#define HB_VERSION_FLAG_LINKER      16
#define HB_VERSION_BITWIDTH         17
#define HB_VERSION_ENDIANNESS       18
#define HB_VERSION_MT               19
#define HB_VERSION_SHARED           26 /* Last. Please continue from here. */
#define HB_VERSION_UNIX_COMPAT      20
#define HB_VERSION_PLATFORM         21
#define HB_VERSION_CPU              24
#define HB_VERSION_COMPILER_CPP     25
#define HB_VERSION_MAX_             26

#if defined( HB_LEGACY_LEVEL5 )
#define HB_VERSION_CHANGELOG_LAST   HB_VERSION_COMMIT_INFO
#define HB_VERSION_CHANGELOG_ID     HB_VERSION_ID
#endif

/* hb_Version( HB_VERSION_ENDIANNESS ) return values. */
#define HB_VERSION_ENDIAN_LITTLE    1
#define HB_VERSION_ENDIAN_BIG       2
#define HB_VERSION_ENDIAN_PDP       3

#endif /* HB_VER_CH_ */
