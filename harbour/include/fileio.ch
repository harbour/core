/* 
 * $Id$
 */

/* NOTE: This file is also used by C code. */

/*
   Harbour Project source code

   This module contains the defines needed for Harbour programs to use
   the FILE management functions.

   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.5    David G. Holm               Added my email address.
   V 1.4    David G. Holm               Added copyright and license header,
                                        along with a complete version history.
   V 1.3    Victor Szel                 Undocumented change.
   V 1.2    David G. Holm               Corrected RCS Id keyword.
   V 1.1    David G. Holm               Committed to CVS.
   V 1.0    David G. Holm               Initial version.
*/

#ifndef _FILEIO_CH
#define _FILEIO_CH

/* File create flags */
#define FC_NORMAL     0x0000   /* No file attributes are set      */
#define FC_READONLY   0x0001   /* Read-only file attribute is set */
#define FC_HIDDEN     0x0002   /* Hidden file attribute is set    */
#define FC_SYSTEM     0x0004   /* System file attribute is set    */

/* File locking flags */
#define FL_LOCK       0x0000   /* Lock a region   */
#define FL_UNLOCK     0x0001   /* Unlock a region */

/* File access flags */
#define FO_READ       0x0000   /* File is opened for reading             */
#define FO_WRITE      0x0001   /* File is opened for writing             */
#define FO_READWRITE  0x0002   /* File is opened for reading and writing */

/* File sharing flags */
#define FO_COMPAT     0x0000   /* No sharing specified                               */
#define FO_EXCLUSIVE  0x0010   /* Deny further attempts to open the file             */
#define FO_DENYWRITE  0x0020   /* Deny further attempts to open the file for writing */
#define FO_DENYREAD   0x0030   /* Deny further attempts to open the file for reading */
#define FO_DENYNONE   0x0040   /* Do not deny any further attempts to open the file  */
#define FO_SHARED     FO_DENYNONE

/* File inheritance flags */
#define FO_INHERITED  0x0000   /* Spawned processes can inherit this file handle     */
#define FO_PRIVATE    0x0080   /* Spawned processes can not inherit this file handle */

/* File mode flags */
#define FS_SET        0x0000   /* Seek from beginning of file    */
#define FS_RELATIVE   0x0001   /* Seek from current file poitner */
#define FS_END        0x0002   /* Seek from end of file          */

/* File system error codes */
#define FS_ERROR      (-1)     /* Unspecified error */

/* Extended file open mode flags */
#define FXO_TRUNCATE  0x0100   /* Create (truncate if exists) */
#define FXO_APPEND    0x0200   /* Create (append if exists)   */
#define FXO_FORCEEXT  0x0800   /* Force default extension     */
#define FXO_DEFAULTS  0x1000   /* Use SET command defaults    */
#define FXO_DEVICERAW 0x2000   /* Open devices in raw mode    */

#endif /* _FILEIO_CH */
