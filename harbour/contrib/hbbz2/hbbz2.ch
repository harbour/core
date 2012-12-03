/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * BZIP2 header file
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_BZ2_CH_
#define HB_BZ2_CH_

#define HB_BZ_OK                    0
#define HB_BZ_RUN_OK                1
#define HB_BZ_FLUSH_OK              2
#define HB_BZ_FINISH_OK             3
#define HB_BZ_STREAM_END            4
#define HB_BZ_SEQUENCE_ERROR        ( -1 )
#define HB_BZ_PARAM_ERROR           ( -2 )
#define HB_BZ_MEM_ERROR             ( -3 )
#define HB_BZ_DATA_ERROR            ( -4 )
#define HB_BZ_DATA_ERROR_MAGIC      ( -5 )
#define HB_BZ_IO_ERROR              ( -6 )
#define HB_BZ_UNEXPECTED_EOF        ( -7 )
#define HB_BZ_OUTBUFF_FULL          ( -8 )
#define HB_BZ_CONFIG_ERROR          ( -9 )

#define HB_BZ_COMPRESSION_SPEED     1
#define HB_BZ_COMPRESSION_SIZE      9
#define HB_BZ_COMPRESSION_DEFAULT   9

#endif /* HB_BZ2_CH_ */
