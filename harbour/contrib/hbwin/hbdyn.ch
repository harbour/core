/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header for .dll support
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#ifndef HBDYN_CH_
#define HBDYN_CH_

/* WIN_DLLCALL() C calling convention */
#define HB_DYN_CALLCONV_STDCALL             0x000000
#define HB_DYN_CALLCONV_CDECL               0x100000

/* WIN_DLLCALL() string encoding */
#define HB_DYN_ENC_ASCII                    0x000000
#define HB_DYN_ENC_UTF8                     0x010000
#define HB_DYN_ENC_UTF16                    0x040000
#define HB_DYN_ENC_RAW                      0x080000

/* WIN_DLLCALL() C types */
#define HB_DYN_CTYPE_DEFAULT                0x000000
#define HB_DYN_CTYPE_CHAR                   0x000001
#define HB_DYN_CTYPE_CHAR_UNSIGNED          0x000011
#define HB_DYN_CTYPE_CHAR_PTR               0x000101
#define HB_DYN_CTYPE_CHAR_UNSIGNED_PTR      0x000111
#define HB_DYN_CTYPE_SHORT                  0x000002
#define HB_DYN_CTYPE_SHORT_UNSIGNED         0x000012
#define HB_DYN_CTYPE_SHORT_PTR              0x000102
#define HB_DYN_CTYPE_SHORT_UNSIGNED_PTR     0x000112
#define HB_DYN_CTYPE_INT                    0x000003
#define HB_DYN_CTYPE_INT_UNSIGNED           0x000013
#define HB_DYN_CTYPE_INT_PTR                0x000103
#define HB_DYN_CTYPE_INT_UNSIGNED_PTR       0x000113
#define HB_DYN_CTYPE_LONG                   0x000004
#define HB_DYN_CTYPE_LONG_UNSIGNED          0x000014
#define HB_DYN_CTYPE_LONG_PTR               0x000104
#define HB_DYN_CTYPE_LONG_UNSIGNED_PTR      0x000114
#define HB_DYN_CTYPE_LLONG                  0x000005
#define HB_DYN_CTYPE_LLONG_UNSIGNED         0x000015
#define HB_DYN_CTYPE_LLONG_PTR              0x000105
#define HB_DYN_CTYPE_LLONG_UNSIGNED_PTR     0x000115
#define HB_DYN_CTYPE_FLOAT                  0x000006
#define HB_DYN_CTYPE_FLOAT_PTR              0x000106
#define HB_DYN_CTYPE_DOUBLE                 0x000007
#define HB_DYN_CTYPE_DOUBLE_PTR             0x000107
#define HB_DYN_CTYPE_BOOL                   0x000008
#define HB_DYN_CTYPE_BOOL_PTR               0x000108
#define HB_DYN_CTYPE_VOID                   0x000009
#define HB_DYN_CTYPE_VOID_PTR               0x000109
#define HB_DYN_CTYPE_STRUCTURE              0x00000A
#define HB_DYN_CTYPE_STRUCTURE_PTR          0x00010A

#endif /* HBDYN_CH_ */
