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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * ChangeLog:
 *
 * V 1.22   David G. Holm               Added FS_ERROR.
 * V 1.21   David G. Holm               Added hb_fsFile().
 * V 1.7    Victor Szakats              #include <x> changed to #include "x".
 * V 1.6    David G. Holm               Added my email address.
 * V 1.5    David G. Holm               Added copyright and license header,
 *                                      along with a complete version history.
 * V 1.4    Victor Szakats              Undocumented change.
 * V 1.3    Gonzalo A. Diethelm         Ensured that all Harbour functions
 *                                      are declared as HB_FUNCTION( void );
 * V 1.2    David G. Holm               Corrected RCS Id keyword.
 * V 1.1    David G. Holm               Committed to CVS.
 * V 1.0    David G. Holm               Initial version.
 *
 */

#ifndef HB_APIFS_H_
#define HB_APIFS_H_

#include "hbapi.h"
#include "fileio.ch"

#if defined(__cplusplus)
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

extern BOOL     hb_fsChDir      ( BYTE * pszDirName );
extern USHORT   hb_fsChDrv      ( BYTE nDrive );
extern void     hb_fsClose      ( FHANDLE hFileHandle );
extern void     hb_fsCommit     ( FHANDLE hFileHandle );
extern FHANDLE  hb_fsCreate     ( BYTE * pszFileName, USHORT uiAttribute );
extern FHANDLE  hb_fsCreateTemp ( const BYTE * pszDir, const BYTE * pszPrefix, USHORT uiAttribute );
extern BYTE *   hb_fsCurDir     ( USHORT uiDrive );
extern USHORT   hb_fsCurDirBuff ( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen );
extern BYTE     hb_fsCurDrv     ( void );
extern int      hb_fsDelete     ( BYTE * pszFileName );
extern double   hb_fsDiskSpace  ( USHORT uiDrive, USHORT uiType );
extern USHORT   hb_fsError      ( void );
extern BOOL     hb_fsFile       ( BYTE * pszFileName );
extern ULONG    hb_fsFSize      ( BYTE * pszFileName, BOOL bUseDirEntry );
extern FHANDLE  hb_fsExtOpen    ( BYTE * pszFileName, BYTE * pDefExt,
                                  USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError );
extern USHORT   hb_fsIsDrv      ( BYTE nDrive );
extern BOOL     hb_fsIsDevice   ( FHANDLE hFileHandle );
extern BOOL     hb_fsLock       ( FHANDLE hFileHandle, ULONG ulStart,
                                  ULONG ulLength, USHORT uiMode );
extern BOOL     hb_fsMkDir      ( BYTE * pszDirName );
extern FHANDLE  hb_fsOpen       ( BYTE * pszFileName, USHORT uiFlags );
extern USHORT   hb_fsRead       ( FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount );
extern ULONG    hb_fsReadLarge  ( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount );
extern BOOL     hb_fsRmDir      ( BYTE * pszDirName );
extern int      hb_fsRename     ( BYTE * pszOldName, BYTE * pszNewName );
extern ULONG    hb_fsSeek       ( FHANDLE hFileHandle, LONG lOffset, USHORT uiMode );
extern ULONG    hb_fsTell       ( FHANDLE hFileHandle );
extern void     hb_fsTempName   ( BYTE * pszBuffer, const BYTE * pszDir, const BYTE * pszPrefix );
extern void     hb_fsSetDevMode ( FHANDLE hFileHandle, USHORT uiDevMode );
extern void     hb_fsSetDevRaw  ( FHANDLE hFileHandle );
extern void     hb_fsSetDevText ( FHANDLE hFileHandle );
extern void     hb_fsSetError   ( USHORT uiError );
extern USHORT   hb_fsWrite      ( FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount );
extern ULONG    hb_fsWriteLarge ( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount );

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

#if defined(__cplusplus)
}
#endif

#endif /* HB_APIFS_H_ */
