/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

/* NOTE: This file is also used by C code. */

#ifndef HB_ZEBRA_CH_
#define HB_ZEBRA_CH_

/* Barcode types */
#define HB_ZEBRA_TYPE_EAN13                 1
#define HB_ZEBRA_TYPE_EAN8                  2
#define HB_ZEBRA_TYPE_UPCA                  3
#define HB_ZEBRA_TYPE_UPCE                  4
#define HB_ZEBRA_TYPE_CODE128               5
#define HB_ZEBRA_TYPE_CODE93                6
#define HB_ZEBRA_TYPE_CODE39                7
#define HB_ZEBRA_TYPE_CODE11                8
#define HB_ZEBRA_TYPE_CODABAR               9
#define HB_ZEBRA_TYPE_ITF                   10
#define HB_ZEBRA_TYPE_MSI                   11

#define HB_ZEBRA_TYPE_PDF417                257
#define HB_ZEBRA_TYPE_DATAMATRIX            258
#define HB_ZEBRA_TYPE_QRCODE                259

/* Generate errors */
#define HB_ZEBRA_ERROR_INVALIDCODE          1
#define HB_ZEBRA_ERROR_BADCHECKSUM          2
#define HB_ZEBRA_ERROR_TOOLARGE             3
#define HB_ZEBRA_ERROR_ARGUMENT             4

/* Draw errors */
#define HB_ZEBRA_ERROR_INVALIDZEBRA         101

/* Generate flags */
#define HB_ZEBRA_FLAG_CHECKSUM              1
#define HB_ZEBRA_FLAG_WIDE2                 0x00  /* Dummy flag - default */
#define HB_ZEBRA_FLAG_WIDE2_5               0x40
#define HB_ZEBRA_FLAG_WIDE3                 0x80

/* Draw flags */

/* Barcode dependent flags */
#define HB_ZEBRA_FLAG_PDF417_TRUNCATED      0x0100
#define HB_ZEBRA_FLAG_PDF417_LEVEL_MASK     0xF000
#define HB_ZEBRA_FLAG_PDF417_LEVEL0         0x1000
#define HB_ZEBRA_FLAG_PDF417_LEVEL1         0x2000
#define HB_ZEBRA_FLAG_PDF417_LEVEL2         0x3000
#define HB_ZEBRA_FLAG_PDF417_LEVEL3         0x4000
#define HB_ZEBRA_FLAG_PDF417_LEVEL4         0x5000
#define HB_ZEBRA_FLAG_PDF417_LEVEL5         0x6000
#define HB_ZEBRA_FLAG_PDF417_LEVEL6         0x7000
#define HB_ZEBRA_FLAG_PDF417_LEVEL7         0x8000
#define HB_ZEBRA_FLAG_PDF417_LEVEL8         0x9000

#define HB_ZEBRA_FLAG_DATAMATRIX_SQUARE     0x0100
#define HB_ZEBRA_FLAG_DATAMATRIX_RECTANGLE  0x0200

#define HB_ZEBRA_FLAG_QR_LEVEL_MASK         0x0700
#define HB_ZEBRA_FLAG_QR_LEVEL_L            0x0100
#define HB_ZEBRA_FLAG_QR_LEVEL_M            0x0200
#define HB_ZEBRA_FLAG_QR_LEVEL_Q            0x0300
#define HB_ZEBRA_FLAG_QR_LEVEL_H            0x0400

#endif /* HB_ZEBRA_CH_ */
