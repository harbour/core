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
 * V 1.7    Victor Szel                 #include <x> changed to #include "x".
 * V 1.6    David G. Holm               Added my email address.
 * V 1.5    David G. Holm               Added copyright and license header,
 *                                      along with a complete version history.
 * V 1.4    Victor Szel                 Undocumented change.
 * V 1.3    Gonzalo A. Diethelm         Ensured that all Harbour functions
 *                                      are declared as HB_FUNCTION( void );
 * V 1.2    David G. Holm               Corrected RCS Id keyword.
 * V 1.1    David G. Holm               Committed to CVS.
 * V 1.0    David G. Holm               Initial version.
 *
 */

#ifndef HB_FILESYS_H_
#define HB_FILESYS_H_

#include "extend.h"
#include "fileio.ch"

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

/* Filename support */
typedef struct
{
   char   szBuffer[ _POSIX_PATH_MAX + 3 ];
   char * szPath;
   char * szName;
   char * szExtension;
} HB_FNAME, * PHB_FNAME, * HB_FNAME_PTR;

extern BOOL     hb_fsChDir      ( BYTE * pDirName );
extern USHORT   hb_fsChDrv      ( BYTE nDrive );
extern void     hb_fsClose      ( FHANDLE hFileHandle );
extern void     hb_fsCommit     ( FHANDLE hFileHandle );
extern FHANDLE  hb_fsCreate     ( BYTE * pFilename, USHORT uiAttribute );
extern BYTE *   hb_fsCurDir     ( USHORT uiDrive );
extern USHORT   hb_fsCurDirBuff ( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen );
extern BYTE     hb_fsCurDrv     ( void );
extern int      hb_fsDelete     ( BYTE * pFilename );
extern USHORT   hb_fsError      ( void );
extern BOOL     hb_fsFile       ( BYTE * pFilename );
extern FHANDLE  hb_fsExtOpen    ( BYTE * pFilename, BYTE * pDefExt,
                                  USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError );
extern USHORT   hb_fsIsDrv      ( BYTE nDrive );
extern BOOL     hb_fsIsDevice   ( FHANDLE hFileHandle );
extern BOOL     hb_fsLock       ( FHANDLE hFileHandle, ULONG ulStart,
                                  ULONG ulLength, USHORT uiMode );
extern BOOL     hb_fsMkDir      ( BYTE * pDirName );
extern FHANDLE  hb_fsOpen       ( BYTE * pFilename, USHORT uiFlags );
extern USHORT   hb_fsRead       ( FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount );
extern ULONG    hb_fsReadLarge  ( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount );
extern BOOL     hb_fsRmDir      ( BYTE * pDirName );
extern int      hb_fsRename     ( BYTE * pOldName, BYTE * pNewName );
extern ULONG    hb_fsSeek       ( FHANDLE hFileHandle, LONG lOffset, USHORT uiMode );
extern ULONG    hb_fsTell       ( FHANDLE hFileHandle );
extern void     hb_fsSetDevMode ( FHANDLE hFileHandle, USHORT uiDevMode );
extern void     hb_fsSetDevRaw  ( FHANDLE hFileHandle );
extern void     hb_fsSetDevText ( FHANDLE hFileHandle );
extern void     hb_fsSetError   ( USHORT uiError );
extern USHORT   hb_fsWrite      ( FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount );
extern ULONG    hb_fsWriteLarge ( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount );

extern PHB_FNAME hb_fsFNameSplit ( char * szFilename ); /* Split given filename into path, name and extension */
extern char *    hb_fsFNameMerge ( char * szFileName, PHB_FNAME pFileName ); /* This function joins path, name and extension into a string with a filename */

#endif /* HB_FILESYS_H_ */
