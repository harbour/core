/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for file management functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#ifndef _FILEIO_CH
#define _FILEIO_CH

/* File create flags */
#define FC_NORMAL          0           /* No file attributes are set      */
#define FC_READONLY        1           /* Read-only file attribute is set */
#define FC_HIDDEN          2           /* Hidden file attribute is set    */
#define FC_SYSTEM          4           /* System file attribute is set    */

/* File attributes flags */
#define HB_FA_ALL          0x00000000

#define HB_FA_READONLY     0x00000001  /* R */
#define HB_FA_HIDDEN       0x00000002  /* H */
#define HB_FA_SYSTEM       0x00000004  /* S */
#define HB_FA_LABEL        0x00000008  /* V */
#define HB_FA_DIRECTORY    0x00000010  /* D | S_ISDIR() */
#define HB_FA_ARCHIVE      0x00000020  /* A | S_ISREG() */
#define HB_FA_DEVICE       0x00000040  /* I | S_ISBLK() */
#define HB_FA_NORMAL       0x00000080  /*   */

#define HB_FA_TEMPORARY    0x00000100  /* T | S_ISFIFO()??? */
#define HB_FA_SPARSE       0x00000200  /* P | S_ISSOCK()??? */
#define HB_FA_REPARSE      0x00000400  /* L | S_ISLNK() */
#define HB_FA_COMPRESSED   0x00000800  /* C | S_ISCHR()??? */
#define HB_FA_OFFLINE      0x00001000  /* O */
#define HB_FA_NOTINDEXED   0x00002000  /* X */
#define HB_FA_ENCRYPTED    0x00004000  /* E */
#define HB_FA_VOLCOMP      0x00008000  /* M volume supports compression. */

/* POSIX file permission */
#define HB_FA_SUID         0x08000000  /* 4000 set user ID on execution */
#define HB_FA_SGID         0x04000000  /* 2000 set group ID on execution */
#define HB_FA_SVTX         0x02000000  /* 1000 sticky bit */
#define HB_FA_RUSR         0x01000000  /* 0400 read by owner */
#define HB_FA_WUSR         0x00800000  /* 0200 write by owner */
#define HB_FA_XUSR         0x00400000  /* 0100 execute/search by owner */
#define HB_FA_RGRP         0x00200000  /* 0040 read by group */
#define HB_FA_WGRP         0x00100000  /* 0020 write by group */
#define HB_FA_XGRP         0x00080000  /* 0010 execute/search by group */
#define HB_FA_ROTH         0x00040000  /* 0004 read by others */
#define HB_FA_WOTH         0x00020000  /* 0002 write by others */
#define HB_FA_XOTH         0x00010000  /* 0001 execute/search by others */

/* File access flags */
#define FO_READ            0           /* File is opened for reading             */
#define FO_WRITE           1           /* File is opened for writing             */
#define FO_READWRITE       2           /* File is opened for reading and writing */

/* File open flags */
#define FO_CREAT           0x0100      /* create and open file */
#define FO_TRUNC           0x0200      /* open with truncation */
#define FO_EXCL            0x0400      /* create and open only if file doesn't exist */

/* File sharing flags */
#define FO_COMPAT          0           /* No sharing specified                               */
#define FO_EXCLUSIVE       16          /* Deny further attempts to open the file             */
#define FO_DENYWRITE       32          /* Deny further attempts to open the file for writing */
#define FO_DENYREAD        48          /* Deny further attempts to open the file for reading */
#define FO_DENYNONE        64          /* Do not deny any further attempts to open the file  */
#define FO_SHARED          FO_DENYNONE

/* File seek mode flags */
#define FS_SET             0           /* Seek from beginning of file    */
#define FS_RELATIVE        1           /* Seek from current file pointer */
#define FS_END             2           /* Seek from end of file          */

/* File mode flags */
#define FD_BINARY          1           /* Binary mode (raw)  */
#define FD_RAW             FD_BINARY
#define FD_TEXT            2           /* Text mode (cooked) */
#define FD_COOKED          FD_TEXT
#define FD_ASCII           FD_TEXT

/* File system error codes */
#define F_ERROR            ( -1 )      /* Unspecified error */

/* hb_DiskSpace() types */
#define HB_DISK_AVAIL      0
#define HB_DISK_FREE       1
#define HB_DISK_USED       2
#define HB_DISK_TOTAL      3

#endif /* _FILEIO_CH */
