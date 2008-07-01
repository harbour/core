/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Compression related functions
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR ZLIB BELOW.
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

/* This file is based upon ZLIB source code, whose copyright holder is:
 *
 * Copyright (C) 1995-2002 Jean-loup Gailly.
 *
 * Also, this file includes code slices from adler32.c for advanced CRC
 * Holder of copyright for this code is:
 *
 * Copyright (C) 1995-2002 Mark Adler
 *
 * ZLIB (containing adler32 code) can be found at:
 * http://www.gzip.org/zlib/
 */

#ifndef HB_COMPRESS_CH
#define HB_COMPRESS_CH

#define HB_Z_OK            0
#define HB_Z_STREAM_END    1
#define HB_Z_NEED_DICT     2
#define HB_Z_ERRNO        (-1)
#define HB_Z_STREAM_ERROR (-2)
#define HB_Z_DATA_ERROR   (-3)
#define HB_Z_MEM_ERROR    (-4)
#define HB_Z_BUF_ERROR    (-5)
#define HB_Z_VERSION_ERROR (-6)
/* Return codes for the compression/decompression functions. Negative
 * values are errors, positive values are used for special but normal events.
 */

#define HB_Z_NO_COMPRESSION         0
#define HB_Z_BEST_SPEED             1
#define HB_Z_BEST_COMPRESSION       9
#define HB_Z_DEFAULT_COMPRESSION  (-1)

#endif
