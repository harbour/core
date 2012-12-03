/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Header file for CT for floppy / hard disk functions
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef _CTDISK_CH
#define _CTDISK_CH

#define FA_NORMAL           0       /*   */
#define FA_READONLY         1       /* R */
#define FA_HIDDEN           2       /* H */
#define FA_SYSTEM           4       /* S */
#define FA_VOLUME           8       /* V */
#define FA_DIRECTORY        16      /* D */
#define FA_ARCHIVE          32      /* A */

#define FA_DEVICE           64      /* I */
#define FA_TEMPORARY        256     /* T */
#define FA_SPARSE           512     /* P */
#define FA_REPARSE          1024    /* L */
#define FA_COMPRESSED       2048    /* C */
#define FA_OFFLINE          4096    /* O */
#define FA_NOTINDEXED       8192    /* X */
#define FA_ENCRYPTED        16384   /* E */
#define FA_VOLCOMP          32768   /* M */

#endif /* _CTDISK_CH */
