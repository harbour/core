/*
 * $Id$
 */

#ifndef HB_FILESYS_H_
#define HB_FILESYS_H_

#include <extend.h>
#include <fileio.ch>

typedef void * ERRORP;
typedef int    FHANDLE;
typedef PBYTE  BYTEP;

BOOL    hb_fsChDir  ( BYTEP fpDirName );
USHORT  hb_fsChDrv  ( BYTEP nDrive );
void    hb_fsClose  ( FHANDLE hFileHandle );
void    hb_fsCommit ( FHANDLE hFileHandle );
FHANDLE hb_fsCreate ( BYTEP fpFilename, USHORT uiAttribute );
BYTEP   hb_fsCurDir ( USHORT uiDrive );
BYTE    hb_fsCurDrv ( void );
void    hb_fsDelete ( BYTEP fpFilename );
USHORT  hb_fsError  ( void );
FHANDLE hb_fsExtOpen( BYTEP fpFilename, BYTEP fpDefExt,
                      USHORT uiFlags, BYTEP fpPaths, ERRORP pError );
USHORT  hb_fsIsDrv  ( BYTE nDrive );
BOOL    hb_fsLock   ( FHANDLE hFileHandle, ULONG ulStart,
                      ULONG ulLength, USHORT uiMode );
BOOL    hb_fsMkDir  ( BYTEP fpDirName );
FHANDLE hb_fsOpen   ( BYTEP fpFilename, USHORT uiFlags );
USHORT  hb_fsRead   ( FHANDLE hFileHandle, BYTEP fpBuff, USHORT uiCount );
BOOL    hb_fsRmDir  ( BYTEP fpDirName );
void    hb_fsRename ( BYTEP fpOldName, BYTEP fpNewName );
ULONG   hb_fsSeek   ( FHANDLE hFileHandle, LONG lOffset, USHORT uiMode );
USHORT  hb_fsWrite  ( FHANDLE hFileHandle, BYTEP fpBuff, USHORT uiCount );

#endif /* HB_FILESYS_H_ */
