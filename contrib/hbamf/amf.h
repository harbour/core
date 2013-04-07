/*
 * Harbour Project source code:
 * AMF3 headers
 * Based on a AmFast C library for Python by Dave Thompson
 *
 * Copyright 2011-2012 Aleksander Czajczynski <hb/at/fki.pl>
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

#ifndef __HBAMF_H
#define __HBAMF_H

/* Things used by both the encoder and the decoder. */

/* Use to test for endianness at run time. */
/* #define is_bigendian() ((*(char*)&endian_test) == 0) */

/* ---- Harbour support */

#define OBJAMF_VER         0

#define OBJAMF_VAR_COUNT   5
#define OBJAMF_VAR_VER     1
#define OBJAMF_VAR_NAME    2
#define OBJAMF_VAR_HASH    3

/* ---- AMF3 */

/* Valid AMF3 integer range */
#define MIN_INT            -268435457
#define MAX_INT            268435456

/* Reference bit */
#define REFERENCE_BIT      0x01

/* Empty string */
#define EMPTY_STRING_TYPE  0x01

/* Object Headers */
#define STATIC             0x03
#define DYNAMIC            0x0B
#define EXTERNALIZABLE     0x07

/* Type markers */
#define UNDEFINED_TYPE     0x00
#define NULL_TYPE          0x01
#define FALSE_TYPE         0x02
#define TRUE_TYPE          0x03
#define INT_TYPE           0x04
#define DOUBLE_TYPE        0x05
#define STRING_TYPE        0x06
#define XML_DOC_TYPE       0x07
#define DATE_TYPE          0x08
#define ARRAY_TYPE         0x09
#define OBJECT_TYPE        0x0A
#define XML_TYPE           0x0B
#define BYTE_ARRAY_TYPE    0x0C
#define AMF3_AMF0          0x11

#endif
