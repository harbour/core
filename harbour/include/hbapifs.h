/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Filesys API
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

#ifndef HB_APIFS_H_
#define HB_APIFS_H_

#include "hbapi.h"
#include "fileio.ch"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

#define FS_ERROR F_ERROR

typedef int    FHANDLE;

/* File locking flags */
#define FL_LOCK       0x0000   /* Lock a region   */
#define FL_UNLOCK     0x0001   /* Unlock a region */

/* File inheritance flags */
#define FO_INHERITED  0x0000   /* Spawned processes can inherit this file handle     */
#define FO_PRIVATE    0x0080   /* Spawned processes can not inherit this file handle */

/* Extended file open mode flags */
#define FXO_TRUNCATE  0x0100   /* Create (truncate if exists) */
#define FXO_APPEND    0x0200   /* Create (append if exists)   */
#define FXO_FORCEEXT  0x0800   /* Force default extension     */
#define FXO_DEFAULTS  0x1000   /* Use SET command defaults    */
#define FXO_DEVICERAW 0x2000   /* Open devices in raw mode    */

extern BOOL     hb_fsChDir      ( BYTE * pszDirName ); /* change working directory */
extern USHORT   hb_fsChDrv      ( BYTE nDrive ); /* change working drive */
extern void     hb_fsClose      ( FHANDLE hFileHandle ); /* close a file */
extern void     hb_fsCommit     ( FHANDLE hFileHandle ); /* commit updates of a file */
extern FHANDLE  hb_fsCreate     ( BYTE * pszFileName, USHORT uiAttribute ); /* create a file */
extern FHANDLE  hb_fsCreateTemp ( const BYTE * pszDir, const BYTE * pszPrefix, USHORT uiAttribute ); /* create a temporary file from components */
extern BYTE *   hb_fsCurDir     ( USHORT uiDrive ); /* retrieve a static pointer containing current directory for specified drive */
extern USHORT   hb_fsCurDirBuff ( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen ); /* copy current directory for given drive into a buffer */
extern BYTE     hb_fsCurDrv     ( void ); /* retrieve current drive number */
extern int      hb_fsDelete     ( BYTE * pszFileName ); /* delete a file */
extern BOOL     hb_fsEof        ( FHANDLE hFileHandle ); /* determine if an open file is position at end-of-file */
extern USHORT   hb_fsError      ( void ); /* retrieve file system error */
extern BOOL     hb_fsFile       ( BYTE * pszFileName ); /* determine if a file exists */
extern ULONG    hb_fsFSize      ( BYTE * pszFileName, BOOL bUseDirEntry ); /* determine the size of a file */
extern FHANDLE  hb_fsExtOpen    ( BYTE * pszFileName, BYTE * pDefExt,
                                  USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError ); /* open a file using default extension and a list of paths */
extern USHORT   hb_fsIsDrv      ( BYTE nDrive ); /* determine if a drive number is a valid drive */
extern BOOL     hb_fsIsDevice   ( FHANDLE hFileHandle ); /* determine if a file is attached to a device (console?) */
extern BOOL     hb_fsLock       ( FHANDLE hFileHandle, ULONG ulStart,
                                  ULONG ulLength, USHORT uiMode ); /* request a lock on a portion of a file */
extern BOOL     hb_fsMkDir      ( BYTE * pszDirName ); /* create a directory */
extern FHANDLE  hb_fsOpen       ( BYTE * pszFileName, USHORT uiFlags ); /* open a file */
extern USHORT   hb_fsRead       ( FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount ); /* read contents of a file into a buffer (<=64K) */
extern ULONG    hb_fsReadLarge  ( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount ); /* read contents of a file into a buffer (>64K) */
extern BOOL     hb_fsRmDir      ( BYTE * pszDirName ); /* remove a directory */
extern int      hb_fsRename     ( BYTE * pszOldName, BYTE * pszNewName ); /* rename a file */
extern ULONG    hb_fsSeek       ( FHANDLE hFileHandle, LONG lOffset, USHORT uiMode ); /* reposition an open file */
extern ULONG    hb_fsTell       ( FHANDLE hFileHandle ); /* retrieve the current position of a file */
extern void     hb_fsTempName   ( BYTE * pszBuffer, const BYTE * pszDir, const BYTE * pszPrefix ); /* create a temporary file name in a buffer */
extern void     hb_fsSetDevMode ( FHANDLE hFileHandle, USHORT uiDevMode ); /* change the device mode of a file (text/binary) */
extern void     hb_fsSetDevRaw  ( FHANDLE hFileHandle ); /* change the device mode of a file to raw (binary) */
extern void     hb_fsSetDevText ( FHANDLE hFileHandle ); /* change the device mode of a file to text */
extern void     hb_fsSetError   ( USHORT uiError ); /* set the file system error number */
extern USHORT   hb_fsWrite      ( FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount ); /* write to an open file from a buffer (<=64K) */
extern ULONG    hb_fsWriteLarge ( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount ); /* write to an open file from a buffer (>64K) */

#define hb_fsFLock( h, s, l )   hb_fsLock( h, s, l, FL_LOCK )
#define hb_fsFUnlock( h, s, l ) hb_fsLock( h, s, l, FL_UNLOCK )

/* Filename support */
typedef struct
{
   char   szBuffer[ _POSIX_PATH_MAX + 3 + 10 ]; /* TOFIX: +10 is for the drive letter support, and should be changed to some manifest constant */
   char * szPath;
   char * szName;
   char * szExtension;
   char * szDrive;
} HB_FNAME, * PHB_FNAME, * HB_FNAME_PTR;

extern PHB_FNAME hb_fsFNameSplit( char * pszFileName ); /* Split given filename into path, name and extension */
extern char *    hb_fsFNameMerge( char * pszFileName, PHB_FNAME pFileName ); /* This function joins path, name and extension into a string with a filename */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_APIFS_H_ */
