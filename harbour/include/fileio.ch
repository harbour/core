/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for file management functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#ifndef _FILEIO_CH
#define _FILEIO_CH

/* File create flags */
#define FC_NORMAL     0        /* No file attributes are set      */
#define FC_READONLY   1        /* Read-only file attribute is set */
#define FC_HIDDEN     2        /* Hidden file attribute is set    */
#define FC_SYSTEM     4        /* System file attribute is set    */

/* File access flags */
#define FO_READ       0        /* File is opened for reading             */
#define FO_WRITE      1        /* File is opened for writing             */
#define FO_READWRITE  2        /* File is opened for reading and writing */

/* File sharing flags */
#define FO_COMPAT     0        /* No sharing specified                               */
#define FO_EXCLUSIVE  16       /* Deny further attempts to open the file             */
#define FO_DENYWRITE  32       /* Deny further attempts to open the file for writing */
#define FO_DENYREAD   48       /* Deny further attempts to open the file for reading */
#define FO_DENYNONE   64       /* Do not deny any further attempts to open the file  */
#define FO_SHARED     FO_DENYNONE

/* File seek mode flags */
#define FS_SET        0        /* Seek from beginning of file    */
#define FS_RELATIVE   1        /* Seek from current file poitner */
#define FS_END        2        /* Seek from end of file          */

/* File mode flags */
#define FD_BINARY     1        /* Binary mode (raw)  */
#define FD_RAW        FD_BINARY
#define FD_TEXT       2        /* Text mode (cooked) */
#define FD_COOKED     FD_TEXT
#define FD_ASCII      FD_TEXT

/* File system error codes */
#define F_ERROR       ( -1 )   /* Unspecified error */

/* HB_DISKSPACE() types */
#define HB_DISK_AVAIL 0
#define HB_DISK_FREE  1
#define HB_DISK_USED  2
#define HB_DISK_TOTAL 3

#endif /* _FILEIO_CH */

