/*
 * Harbour Project source code:
 * Dynamic call (high-level header)
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#ifndef HBDYN_CH_
#define HBDYN_CH_

/* C calling conventions */
#define HB_DYN_CALLCONV_CDECL               0x0000000 /* C default */
#define HB_DYN_CALLCONV_STDCALL             0x0100000 /* Windows API default */
#define HB_DYN_CALLCONV_SYSCALL             0x0200000 /* OS/2 32-bit default */

/* String encodings */
#define HB_DYN_ENC_ASCII                    0x0000000
#define HB_DYN_ENC_UTF8                     0x0010000
#define HB_DYN_ENC_UTF16                    0x0020000
#define HB_DYN_ENC_RAW                      0x0040000

/* Misc options */
#define HB_DYC_OPT_NONE                     0x0000000
#define HB_DYC_OPT_NULLTERM                 0x1000000

/* C types */
#define HB_DYN_CTYPE_DEFAULT                0x0000000
#define HB_DYN_CTYPE_CHAR                   0x0000001
#define HB_DYN_CTYPE_CHAR_UNSIGNED          0x0000011
#define HB_DYN_CTYPE_CHAR_PTR               0x0000101
#define HB_DYN_CTYPE_CHAR_UNSIGNED_PTR      0x0000111
#define HB_DYN_CTYPE_SHORT                  0x0000002
#define HB_DYN_CTYPE_SHORT_UNSIGNED         0x0000012
#define HB_DYN_CTYPE_SHORT_PTR              0x0000102
#define HB_DYN_CTYPE_SHORT_UNSIGNED_PTR     0x0000112
#define HB_DYN_CTYPE_INT                    0x0000003
#define HB_DYN_CTYPE_INT_UNSIGNED           0x0000013
#define HB_DYN_CTYPE_INT_PTR                0x0000103
#define HB_DYN_CTYPE_INT_UNSIGNED_PTR       0x0000113
#define HB_DYN_CTYPE_LONG                   0x0000004
#define HB_DYN_CTYPE_LONG_UNSIGNED          0x0000014
#define HB_DYN_CTYPE_LONG_PTR               0x0000104
#define HB_DYN_CTYPE_LONG_UNSIGNED_PTR      0x0000114
#define HB_DYN_CTYPE_LLONG                  0x0000005
#define HB_DYN_CTYPE_LLONG_UNSIGNED         0x0000015
#define HB_DYN_CTYPE_LLONG_PTR              0x0000105
#define HB_DYN_CTYPE_LLONG_UNSIGNED_PTR     0x0000115
#define HB_DYN_CTYPE_FLOAT                  0x0000006
#define HB_DYN_CTYPE_FLOAT_PTR              0x0000106
#define HB_DYN_CTYPE_DOUBLE                 0x0000007
#define HB_DYN_CTYPE_DOUBLE_PTR             0x0000107
#define HB_DYN_CTYPE_BOOL                   0x0000008
#define HB_DYN_CTYPE_BOOL_PTR               0x0000108
#define HB_DYN_CTYPE_VOID                   0x0000009
#define HB_DYN_CTYPE_VOID_PTR               0x0000109
#define HB_DYN_CTYPE_STRUCTURE              0x000000A
#define HB_DYN_CTYPE_STRUCTURE_PTR          0x000010A

#endif /* HBDYN_CH_ */
