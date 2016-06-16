/*
 * Header file for the Filesys API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

HB_EXTERN_BEGIN

#define FS_ERROR ( HB_FHANDLE ) F_ERROR

/* File locking flags */
#define FL_LOCK       0x0000   /* Lock a region */
#define FL_UNLOCK     0x0001   /* Unlock a region */
#define FL_MASK       0x00FF   /* Mask for lock type */

/* Extended file locking flags */
#define FLX_EXCLUSIVE HB_FLX_EXCLUSIVE  /* Exclusive lock */
#define FLX_SHARED    HB_FLX_SHARED     /* Shared lock */
#define FLX_WAIT      HB_FLX_WAIT       /* Wait for lock until success */

/* File inheritance flags */
#define FO_INHERITED  0x0000   /* Spawned processes can inherit this file handle */
#define FO_PRIVATE    0x0080   /* Spawned processes can not inherit this file handle */

/* Extended file open mode flags */
#define FXO_TRUNCATE  0x0100   /* Create (truncate if exists) */
#define FXO_APPEND    0x0200   /* Create (append if exists) */
#define FXO_UNIQUE    0x0400   /* Create unique file FO_EXCL ??? */
#define FXO_FORCEEXT  0x0800   /* Force default extension */
#define FXO_DEFAULTS  0x1000   /* Use SET command defaults */
#define FXO_DEVICERAW 0x2000   /* Open devices in raw mode */
/* Harbour extension */
#define FXO_NOSEEKPOS FXO_DEVICERAW /* seek pos not needed in regular file */
#define FXO_SHARELOCK 0x4000        /* emulate MS-DOS SH_DENY* mode in POSIX OS */
#define FXO_COPYNAME  0x8000        /* copy final szPath into pszFileName */

/* these definitions should be cleared,
 * now they only help to clean lower level code
 */
#define HB_FA_FIFO            HB_FA_TEMPORARY   /* S_ISFIFO() */
#define HB_FA_FILE            HB_FA_ARCHIVE     /* S_ISREG() */
#define HB_FA_BLKDEVICE       HB_FA_DEVICE      /* S_ISBLK() */
#define HB_FA_CHRDEVICE       HB_FA_COMPRESSED  /* S_ISCHR() */
#define HB_FA_SOCKET          HB_FA_SPARSE      /* S_ISSOCK() */
#define HB_FA_LINK            HB_FA_REPARSE     /* S_ISLNK() */

#define HB_FA_UGVS            ( HB_FA_SUID | HB_FA_SGID | HB_FA_SVTX )
#define HB_FA_RWXU            ( HB_FA_RUSR | HB_FA_WUSR | HB_FA_XUSR )
#define HB_FA_RWXG            ( HB_FA_RGRP | HB_FA_WGRP | HB_FA_XGRP )
#define HB_FA_RWXO            ( HB_FA_ROTH | HB_FA_WOTH | HB_FA_XOTH )

#if defined( HB_OS_VXWORKS ) && ! defined( S_ISVTX )
   #define S_ISVTX 0
#endif

/* macros to convert Harbour attributes to POSIX ones */
#define HB_FA_POSIX_SID(a)    ( ( ( ( a ) & HB_FA_SVTX ) ? S_ISVTX : 0 ) | \
                                ( ( ( a ) & HB_FA_SGID ) ? S_ISGID : 0 ) | \
                                ( ( ( a ) & HB_FA_SUID ) ? S_ISUID : 0 ) )
#define HB_FA_POSIX_OTH(a)    ( ( ( ( a ) & HB_FA_XOTH ) ? S_IXOTH : 0 ) | \
                                ( ( ( a ) & HB_FA_WOTH ) ? S_IWOTH : 0 ) | \
                                ( ( ( a ) & HB_FA_ROTH ) ? S_IROTH : 0 ) )
#define HB_FA_POSIX_GRP(a)    ( ( ( ( a ) & HB_FA_XGRP ) ? S_IXGRP : 0 ) | \
                                ( ( ( a ) & HB_FA_WGRP ) ? S_IWGRP : 0 ) | \
                                ( ( ( a ) & HB_FA_RGRP ) ? S_IRGRP : 0 ) )
#define HB_FA_POSIX_USR(a)    ( ( ( ( a ) & HB_FA_XUSR ) ? S_IXUSR : 0 ) | \
                                ( ( ( a ) & HB_FA_WUSR ) ? S_IWUSR : 0 ) | \
                                ( ( ( a ) & HB_FA_RUSR ) ? S_IRUSR : 0 ) )

#define HB_FA_POSIX_ATTR(a)   ( HB_FA_POSIX_OTH(a) | \
                                HB_FA_POSIX_GRP(a) | \
                                HB_FA_POSIX_USR(a) | \
                                HB_FA_POSIX_SID(a) )

extern HB_EXPORT HB_BOOL    hb_fsChDir       ( const char * pszDirName ); /* change working directory */
extern HB_EXPORT HB_ERRCODE hb_fsChDrv       ( int iDrive ); /* change working drive */
extern HB_EXPORT void       hb_fsClose       ( HB_FHANDLE hFileHandle ); /* close a file */
extern HB_EXPORT void       hb_fsCommit      ( HB_FHANDLE hFileHandle ); /* commit updates of a file */
extern HB_EXPORT HB_FHANDLE hb_fsCreate      ( const char * pszFileName, HB_FATTR ulAttr ); /* create a file */
extern HB_EXPORT HB_FHANDLE hb_fsCreateEx    ( const char * pszFileName, HB_FATTR ulAttr, HB_USHORT uiFlags ); /* create a file, with specific open mode */
extern HB_EXPORT HB_FHANDLE hb_fsCreateTemp  ( const char * pszDir, const char * pszPrefix, HB_FATTR ulAttr, char * pszName ); /* create a temporary file from components */
extern HB_EXPORT HB_FHANDLE hb_fsCreateTempEx( char * pszName, const char * pszDir, const char * pszPrefix, const char * pszExt, HB_FATTR ulAttr ); /* create a temporary file from components */
extern HB_EXPORT HB_ERRCODE hb_fsTempDir     ( char * pszDir ); /* full buffer with system temp directory (or empty on error) */
extern HB_EXPORT const char * hb_fsCurDir    ( int iDrive ); /* retrieve a static pointer containing current directory for specified drive */
extern HB_EXPORT HB_ERRCODE hb_fsCurDirBuff  ( int iDrive, char * pbyBuffer, HB_SIZE nLen ); /* copy current directory for given drive into a buffer */
extern HB_EXPORT void       hb_fsBaseDirBuff ( char * pbyBuffer ); /* retrieve the base dir of the executable */
extern HB_EXPORT int        hb_fsCurDrv      ( void ); /* retrieve current drive number */
extern HB_EXPORT HB_BOOL    hb_fsDelete      ( const char * pszFileName ); /* delete a file */
extern HB_EXPORT HB_BOOL    hb_fsEof         ( HB_FHANDLE hFileHandle ); /* determine if an open file is position at end-of-file */
extern HB_EXPORT HB_ERRCODE hb_fsError       ( void ); /* retrieve file system error */
extern HB_EXPORT HB_ERRCODE hb_fsOsError     ( void ); /* retrieve system dependant file system error */
extern HB_EXPORT HB_BOOL    hb_fsFile        ( const char * pszFileName ); /* determine if a file exists */
extern HB_EXPORT HB_BOOL    hb_fsIsDirectory ( const char * pszFileName );
extern HB_EXPORT HB_FOFFSET hb_fsFSize       ( const char * pszFileName, HB_BOOL bUseDirEntry ); /* determine the size of a file */
extern HB_EXPORT HB_FHANDLE hb_fsExtOpen     ( const char * pszFileName, const char * pDefExt,
                                               HB_FATTR nFlags, const char * pPaths, PHB_ITEM pError ); /* open a file using default extension and a list of paths */
extern HB_EXPORT char *     hb_fsExtName     ( const char * pszFileName, const char * pDefExt,
                                               HB_FATTR nExFlags, const char * pPaths ); /* convert file name for hb_fsExtOpen, caller must free the returned buffer */
extern HB_EXPORT HB_ERRCODE hb_fsIsDrv       ( int iDrive ); /* determine if a drive number is a valid drive */
extern HB_EXPORT HB_BOOL    hb_fsIsDevice    ( HB_FHANDLE hFileHandle ); /* determine if a file is attached to a device (console?) */
extern HB_EXPORT HB_BOOL    hb_fsLock        ( HB_FHANDLE hFileHandle, HB_ULONG ulStart, HB_ULONG ulLength, HB_USHORT uiMode ); /* request a lock on a portion of a file */
extern HB_EXPORT HB_BOOL    hb_fsLockLarge   ( HB_FHANDLE hFileHandle, HB_FOFFSET nStart,
                                               HB_FOFFSET nLength, HB_USHORT uiMode ); /* request a lock on a portion of a file using 64bit API */
extern HB_EXPORT int        hb_fsLockTest    ( HB_FHANDLE hFileHandle, HB_FOFFSET nStart,
                                               HB_FOFFSET nLength, HB_USHORT uiMode );
extern HB_EXPORT HB_BOOL    hb_fsMkDir       ( const char * pszDirName ); /* create a directory */
extern HB_EXPORT HB_FHANDLE hb_fsOpen        ( const char * pszFileName, HB_USHORT uiFlags ); /* open a file */
extern HB_EXPORT HB_FHANDLE hb_fsOpenEx      ( const char * pszFileName, HB_USHORT uiFlags, HB_FATTR nAttr ); /* open or create a file with given attributes */
extern HB_EXPORT HB_USHORT  hb_fsRead        ( HB_FHANDLE hFileHandle, void * pBuff, HB_USHORT uiCount ); /* read contents of a file into a buffer (<=64K) */
extern HB_EXPORT HB_SIZE    hb_fsReadLarge   ( HB_FHANDLE hFileHandle, void * pBuff, HB_SIZE nCount ); /* read contents of a file into a buffer (>64K) */
extern HB_EXPORT HB_SIZE    hb_fsReadAt      ( HB_FHANDLE hFileHandle, void * pBuff, HB_SIZE nCount, HB_FOFFSET nOffset ); /* read from given offset contents of a file into a buffer (>64K) */
extern HB_EXPORT HB_BOOL    hb_fsRmDir       ( const char * pszDirName ); /* remove a directory */
extern HB_EXPORT HB_BOOL    hb_fsRename      ( const char * pszOldName, const char * pszNewName ); /* rename a file */
extern HB_EXPORT HB_ULONG   hb_fsSeek        ( HB_FHANDLE hFileHandle, HB_LONG lOffset, HB_USHORT uiMode ); /* reposition an open file */
extern HB_EXPORT HB_FOFFSET hb_fsSeekLarge   ( HB_FHANDLE hFileHandle, HB_FOFFSET nOffset, HB_USHORT uiFlags ); /* reposition an open file using 64bit API */
extern HB_EXPORT HB_FOFFSET hb_fsTell        ( HB_FHANDLE hFileHandle ); /* retrieve the current position of a file */
extern HB_EXPORT HB_FOFFSET hb_fsGetSize     ( HB_FHANDLE hFileHandle ); /* retrieve the size of a file, it may change current seek position */
extern HB_EXPORT int        hb_fsSetDevMode  ( HB_FHANDLE hFileHandle, int iDevMode ); /* change the device mode of a file (text/binary) */
extern HB_EXPORT HB_BOOL    hb_fsGetFileTime ( const char * pszFileName, long * plJulian, long * plMillisec );
extern HB_EXPORT HB_BOOL    hb_fsSetFileTime ( const char * pszFileName, long lJulian, long lMillisec );
extern HB_EXPORT HB_BOOL    hb_fsGetAttr     ( const char * pszFileName, HB_FATTR * pulAttr );
extern HB_EXPORT HB_BOOL    hb_fsSetAttr     ( const char * pszFileName, HB_FATTR ulAttr );
extern HB_EXPORT HB_BOOL    hb_fsGetCWD      ( char * pszBuffer, HB_SIZE nSize );
extern HB_EXPORT HB_BOOL    hb_fsSetCWD      ( const char * pszDirName );
extern HB_EXPORT void       hb_fsSetError    ( HB_ERRCODE uiError ); /* set the file system OS error number */
extern HB_EXPORT void       hb_fsSetIOError  ( HB_BOOL fResult, HB_USHORT uiOperation ); /* set the file system error number after IO operation */
extern HB_EXPORT HB_BOOL    hb_fsTruncAt     ( HB_FHANDLE hFileHandle, HB_FOFFSET nOffset ); /* truncate file to given size */
extern HB_EXPORT HB_USHORT  hb_fsWrite       ( HB_FHANDLE hFileHandle, const void * pBuff, HB_USHORT uiCount ); /* write to an open file from a buffer (<=64K) */
extern HB_EXPORT HB_SIZE    hb_fsWriteLarge  ( HB_FHANDLE hFileHandle, const void * pBuff, HB_SIZE nCount ); /* write to an open file from a buffer (>64K) */
extern HB_EXPORT HB_SIZE    hb_fsWriteAt     ( HB_FHANDLE hFileHandle, const void * pBuff, HB_SIZE nCount, HB_FOFFSET nOffset ); /* write to an open file at given offset from a buffer (>64K) */
extern HB_EXPORT HB_FHANDLE hb_fsPOpen       ( const char * pszFileName, const char * pszMode );
extern HB_EXPORT HB_BOOL    hb_fsPipeCreate  ( HB_FHANDLE hPipe[ 2 ] );
extern HB_EXPORT HB_BOOL    hb_fsPipeUnblock ( HB_FHANDLE hPipeHandle );
extern HB_EXPORT HB_SIZE    hb_fsPipeIsData  ( HB_FHANDLE hPipeHandle, HB_SIZE nBufferSize, HB_MAXINT nTimeOut );
extern HB_EXPORT HB_SIZE    hb_fsPipeRead    ( HB_FHANDLE hPipeHandle, void * buffer, HB_SIZE nSize, HB_MAXINT nTimeOut );
extern HB_EXPORT HB_SIZE    hb_fsPipeWrite   ( HB_FHANDLE hPipeHandle, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeOut );
extern HB_EXPORT int        hb_fsIsPipeOrSock( HB_FHANDLE hPipeHandle );
extern HB_EXPORT HB_FHANDLE hb_fsGetOsHandle ( HB_FHANDLE hFileHandle );
extern HB_EXPORT HB_ERRCODE hb_fsGetFError   ( void ); /* get FError() flag */
extern HB_EXPORT void       hb_fsSetFError   ( HB_ERRCODE uiError ); /* set FError() flag */
extern HB_EXPORT HB_BOOL    hb_fsNameExists  ( const char * pszFileName ); /* check if a name exists in the filesystem (wildcard chars not accepted). */
extern HB_EXPORT HB_BOOL    hb_fsFileExists  ( const char * pszFileName ); /* check if a file exists (wildcard chars not accepted). */
extern HB_EXPORT HB_BOOL    hb_fsDirExists   ( const char * pszDirName ); /* check if a directory exists (wildcard chars not accepted). */
extern HB_EXPORT HB_BOOL    hb_fsCopy        ( const char * pszSource, const char * pszDest ); /* copy file */
extern HB_EXPORT double     hb_fsDiskSpace   ( const char * pszDirName, HB_USHORT uiType );
extern HB_EXPORT PHB_ITEM   hb_fsDirectory   ( const char * pszDirSpec, const char * pszAttributes, HB_BOOL fDateTime );
extern HB_EXPORT HB_BOOL    hb_fsLink        ( const char * pszExisting, const char * pszNewFile ); /* create hard link */
extern HB_EXPORT HB_BOOL    hb_fsLinkSym     ( const char * pszTarget, const char * pszNewFile ); /* create symbolic (soft) link */
extern HB_EXPORT char *     hb_fsLinkRead    ( const char * pszFileName ); /* returns the link pointed to */

#define hb_fsFLock( h, s, l )   hb_fsLock( h, s, l, FL_LOCK )
#define hb_fsFUnlock( h, s, l ) hb_fsLock( h, s, l, FL_UNLOCK )

#if defined( HB_OS_UNIX ) && ! defined( HB_USE_SHARELOCKS_OFF )
   #define HB_USE_SHARELOCKS
   #define HB_SHARELOCK_POS          0x7fffffffUL
   #define HB_SHARELOCK_SIZE         0x1UL
   #if defined( HB_USE_BSDLOCKS_OFF )
      #undef HB_USE_BSDLOCKS
   #elif defined( HB_OS_LINUX ) && \
         ! defined( __WATCOMC__ ) && ! defined( HB_USE_BSDLOCKS )
       /* default usage of BSD locks in *BSD systems for emulating
        * MS-DOS/Windows DENY_* flags has been disabled because tests
        * on FreeBSD 6.2 and macOS shows that this implementation
        * can create self deadlock when used simultaneously with
        * POSIX locks - thanks to Phil and Lorenzo for locating the
        * problem and tests [druzus]
        */
      #define HB_USE_BSDLOCKS
   #endif
#endif

#define HB_MAX_DRIVE_LENGTH   10
#define HB_MAX_FILE_EXT       10

/* FileName support */
typedef struct
{
   const char * szPath;
   const char * szName;
   const char * szExtension;
   const char * szDrive;
   char   szBuffer[ HB_PATH_MAX + HB_MAX_DRIVE_LENGTH + 6 ];
} HB_FNAME, * PHB_FNAME;

extern HB_EXPORT PHB_FNAME  hb_fsFNameSplit( const char * pszFileName ); /* Split given filename into path, name and extension */
extern HB_EXPORT char *     hb_fsFNameMerge( char * pszFileName, PHB_FNAME pFileName ); /* This function joins path, name and extension into a string with a filename */

/* Searchable path support */
typedef struct _HB_PATHNAMES
{
   char * szPath;
   struct _HB_PATHNAMES * pNext;
   HB_BOOL fFree;
} HB_PATHNAMES;

extern HB_EXPORT void       hb_fsAddSearchPath( const char * szPath, HB_PATHNAMES ** pSearchList );
extern HB_EXPORT void       hb_fsFreeSearchPath( HB_PATHNAMES * pSearchList );

extern HB_EXPORT HB_BOOL    hb_spFile( const char * pszFileName, char * pRetPath );
extern HB_EXPORT HB_BOOL    hb_spFileExists( const char * pszFileName, char * pRetPath );
extern HB_EXPORT HB_FHANDLE hb_spOpen( const char * pszFileName, HB_USHORT uiFlags );
extern HB_EXPORT HB_FHANDLE hb_spCreate( const char * pszFileName, HB_FATTR ulAttr );
extern HB_EXPORT HB_FHANDLE hb_spCreateEx( const char * pszFileName, HB_FATTR ulAttr, HB_USHORT uiFlags );

/* File Find API structure */
typedef struct
{
   char        szName[ HB_PATH_MAX ];
   char        szDate[ 9 ]; /* in YYYYMMDD format */
   char        szTime[ 9 ]; /* in HH:MM:SS format */
   long        lDate;
   long        lTime;
   HB_FATTR    attr;
   HB_FOFFSET  size;

#if defined( _HB_FFIND_INTERNAL_ )
   /* Private */
   const char * pszFileMask;
   HB_FATTR     attrmask;
   HB_BOOL      bFirst;
   char *       pszFree;

   void * info; /* Pointer to the platform specific find info */
#endif
} HB_FFIND, * PHB_FFIND;

/* File Find API functions */
extern HB_EXPORT PHB_FFIND hb_fsFindFirst( const char * pszFileName, HB_FATTR ulAttrMask );
extern HB_EXPORT HB_BOOL   hb_fsFindNext( PHB_FFIND ffind );
extern HB_EXPORT void      hb_fsFindClose( PHB_FFIND ffind );

/* functions to create, wait and terminate processes */
extern HB_EXPORT HB_FHANDLE hb_fsProcessOpen( const char * pszFileName,
                                              HB_FHANDLE * phStdin, HB_FHANDLE * phStdout,
                                              HB_FHANDLE * phStderr,
                                              HB_BOOL fDetach, HB_ULONG * pulPID );
extern HB_EXPORT int        hb_fsProcessRun( const char * pszFileName,
                                             const char * pStdInBuf, HB_SIZE nStdInLen,
                                             char ** pStdOutPtr, HB_SIZE * pnStdOut,
                                             char ** pStdErrPtr, HB_SIZE * pnStdErr,
                                             HB_BOOL fDetach );
extern HB_EXPORT int        hb_fsProcessValue( HB_FHANDLE hProcess, HB_BOOL fWait );
extern HB_EXPORT HB_BOOL    hb_fsProcessClose( HB_FHANDLE hProcess, HB_BOOL fGentle );

/* Misc helper functions */
extern HB_EXPORT HB_FATTR   hb_fsAttrFromRaw( HB_FATTR raw_attr );
extern HB_EXPORT HB_FATTR   hb_fsAttrToRaw( HB_FATTR ulAttr );
extern HB_EXPORT HB_FATTR   hb_fsAttrEncode( const char * szAttr );
extern HB_EXPORT char *     hb_fsAttrDecode( HB_FATTR ulAttr, char * szAttr );

extern HB_EXPORT HB_BOOL      hb_fsMaxFilesError( void );
extern HB_EXPORT const char * hb_fsNameConv( const char * pszFileName, char ** pszFree );
#if defined( HB_OS_WIN )
extern HB_EXPORT HB_WCHAR *   hb_fsNameConvU16( const char * pszFileName );
#endif
#if defined( HB_OS_OS2 )
extern HB_EXPORT HB_BOOL  hb_isWSeB( void );
extern HB_EXPORT HB_ULONG hb_fsOS2DosOpen( const char * pszFileName,
                                           HB_FHANDLE * pHFile, HB_ULONG * pulAction,
                                           HB_ULONG nInitSize, HB_ULONG ulAttribute,
                                           HB_ULONG fsOpenFlags, HB_ULONG fsOpenMode );
extern HB_EXPORT HB_ULONG hb_fsOS2DosOpenL( const char * pszFileName,
                                            HB_FHANDLE * pHFile, HB_ULONG * pulAction,
                                            HB_FOFFSET nInitSize, HB_ULONG ulAttribute,
                                            HB_ULONG fsOpenFlags, HB_ULONG fsOpenMode );
extern HB_EXPORT HB_ULONG hb_fsOS2DosSetFileLocksL( HB_FHANDLE hFile,
                                                    void * pflUnlock, void * pflLock,
                                                    HB_ULONG timeout, HB_ULONG flags );
extern HB_EXPORT HB_ULONG hb_fsOS2DosSetFilePtrL( HB_FHANDLE hFile, HB_FOFFSET nPos,
                                                 HB_ULONG method, HB_FOFFSET * pnCurPos );
extern HB_EXPORT HB_ULONG hb_fsOS2DosSetFileSizeL( HB_FHANDLE hFile, HB_FOFFSET nSize );
extern HB_EXPORT HB_BOOL  hb_fsOS2QueryPathInfo( const char * pszPathName,
                                                 HB_FOFFSET * pnSize, HB_FATTR * pnAttr,
                                                 long * plJulian, long * plMillisec );
#endif

/* Harbour file functions with shared file handles and locks
 * (buffers in the future)
 */

#if defined( _HB_FILE_IMPLEMENTATION_ ) || defined( _HB_FILE_INTERNAL_ )

   #define HB_FILE_TYPE_MAX    128

   struct _HB_FILE;
   typedef struct _HB_FILE * PHB_FILE;
   typedef const struct _HB_FILE_FUNCS * PHB_FILE_FUNCS;

   typedef struct _HB_FILE_FUNCS
   {
      HB_BOOL     ( * Accept )      ( PHB_FILE_FUNCS pFuncs, const char * pszFileName );

      HB_BOOL     ( * Exists )      ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, char * pRetPath );
      HB_BOOL     ( * Delete )      ( PHB_FILE_FUNCS pFuncs, const char * pszFileName );
      HB_BOOL     ( * Rename )      ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, const char * pszNewName );
      HB_BOOL     ( * Copy )        ( PHB_FILE_FUNCS pFuncs, const char * pszSrcFile, const char * pszDstFile );

      HB_BOOL     ( * DirExists )   ( PHB_FILE_FUNCS pFuncs, const char * pszDirName );
      HB_BOOL     ( * DirMake )     ( PHB_FILE_FUNCS pFuncs, const char * pszDirName );
      HB_BOOL     ( * DirRemove )   ( PHB_FILE_FUNCS pFuncs, const char * pszDirName );
      double      ( * DirSpace )    ( PHB_FILE_FUNCS pFuncs, const char * pszDirName, HB_USHORT uiType );
      PHB_ITEM    ( * Directory )   ( PHB_FILE_FUNCS pFuncs, const char * pszDirSpec, const char * pszAttr );

      HB_BOOL     ( * TimeGet )     ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, long * plJulian, long * plMillisec );
      HB_BOOL     ( * TimeSet )     ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, long lJulian, long lMillisec );
      HB_BOOL     ( * AttrGet )     ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR * pulAttr );
      HB_BOOL     ( * AttrSet )     ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, HB_FATTR ulAttr );

      HB_BOOL     ( * Link )        ( PHB_FILE_FUNCS pFuncs, const char * pszExisting, const char * pszNewName );
      HB_BOOL     ( * LinkSym )     ( PHB_FILE_FUNCS pFuncs, const char * pszTarget, const char * pszNewName );
      char *      ( * LinkRead )    ( PHB_FILE_FUNCS pFuncs, const char * pszFileName );

      PHB_FILE    ( * Open )        ( PHB_FILE_FUNCS pFuncs, const char * pszFileName, const char * pDefExt,
                                      HB_FATTR nExFlags, const char * pPaths, PHB_ITEM pError );

      void        ( * Close )       ( PHB_FILE pFile );
      HB_BOOL     ( * Lock )        ( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType );
      int         ( * LockTest )    ( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType );
      HB_SIZE     ( * Read )        ( PHB_FILE pFile, void * buffer, HB_SIZE nSize, HB_MAXINT nTimeout );
      HB_SIZE     ( * Write )       ( PHB_FILE pFile, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeout );
      HB_SIZE     ( * ReadAt )      ( PHB_FILE pFile, void * buffer, HB_SIZE nSize, HB_FOFFSET nOffset );
      HB_SIZE     ( * WriteAt )     ( PHB_FILE pFile, const void * buffer, HB_SIZE nSize, HB_FOFFSET nOffset );
      HB_BOOL     ( * TruncAt )     ( PHB_FILE pFile, HB_FOFFSET nOffset );
      HB_FOFFSET  ( * Seek )        ( PHB_FILE pFile, HB_FOFFSET nOffset, HB_USHORT uiFlags );
      HB_FOFFSET  ( * Size )        ( PHB_FILE pFile );
      HB_BOOL     ( * Eof )         ( PHB_FILE pFile );
      void        ( * Flush )       ( PHB_FILE pFile, HB_BOOL fDirty );
      void        ( * Commit )      ( PHB_FILE pFile );
      HB_BOOL     ( * Configure )   ( PHB_FILE pFile, int iIndex, PHB_ITEM pValue );
      HB_FHANDLE  ( * Handle )      ( PHB_FILE pFile );
   }
   HB_FILE_FUNCS;

   extern HB_EXPORT HB_BOOL hb_fileRegisterFull( const HB_FILE_FUNCS * pFuncs );
   extern HB_EXPORT HB_BOOL hb_fileRegisterPart( HB_FILE_FUNCS * pFuncs );
#else
   typedef void * PHB_FILE;
#endif

extern HB_EXPORT HB_BOOL      hb_fileExists     ( const char * pszFileName, char * pRetPath );
extern HB_EXPORT HB_BOOL      hb_fileDelete     ( const char * pszFileName );
extern HB_EXPORT HB_BOOL      hb_fileRename     ( const char * pszFileName, const char * pszNewName );
extern HB_EXPORT HB_BOOL      hb_fileCopy       ( const char * pszSrcFile, const char * pszDstFile );

extern HB_EXPORT HB_BOOL      hb_fileDirExists  ( const char * pszDirName );
extern HB_EXPORT HB_BOOL      hb_fileDirMake    ( const char * pszDirName );
extern HB_EXPORT HB_BOOL      hb_fileDirRemove  ( const char * pszDirName );
extern HB_EXPORT double       hb_fileDirSpace   ( const char * pszDirName, HB_USHORT uiType );
extern HB_EXPORT PHB_ITEM     hb_fileDirectory  ( const char * pszDirSpec, const char * pszAttr );

extern HB_EXPORT HB_FOFFSET   hb_fileSizeGet    ( const char * pszFileName, HB_BOOL bUseDirEntry );
extern HB_EXPORT HB_BOOL      hb_fileTimeGet    ( const char * pszFileName, long * plJulian, long * plMillisec );
extern HB_EXPORT HB_BOOL      hb_fileTimeSet    ( const char * pszFileName, long lJulian, long lMillisec );
extern HB_EXPORT HB_BOOL      hb_fileAttrGet    ( const char * pszFileName, HB_FATTR * pulAttr );
extern HB_EXPORT HB_BOOL      hb_fileAttrSet    ( const char * pszFileName, HB_FATTR ulAttr );

extern HB_EXPORT HB_BOOL      hb_fileLink       ( const char * pszExisting, const char * pszNewName );
extern HB_EXPORT HB_BOOL      hb_fileLinkSym    ( const char * pszTarget, const char * pszNewName );
extern HB_EXPORT char *       hb_fileLinkRead   ( const char * pszFileName );

extern HB_EXPORT PHB_FILE     hb_fileExtOpen    ( const char * pszFileName, const char * pDefExt,
                                                  HB_FATTR nExFlags, const char * pPaths,
                                                  PHB_ITEM pError );
extern HB_EXPORT void         hb_fileClose      ( PHB_FILE pFile );
extern HB_EXPORT HB_BOOL      hb_fileLock       ( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType );
extern HB_EXPORT int          hb_fileLockTest   ( PHB_FILE pFile, HB_FOFFSET nStart, HB_FOFFSET nLen, int iType );
extern HB_EXPORT HB_SIZE      hb_fileRead       ( PHB_FILE pFile, void * buffer, HB_SIZE nSize, HB_MAXINT nTimeout );
extern HB_EXPORT HB_SIZE      hb_fileWrite      ( PHB_FILE pFile, const void * buffer, HB_SIZE nSize, HB_MAXINT nTimeout );
extern HB_EXPORT HB_SIZE      hb_fileReadAt     ( PHB_FILE pFile, void * buffer, HB_SIZE nSize, HB_FOFFSET nOffset );
extern HB_EXPORT HB_SIZE      hb_fileWriteAt    ( PHB_FILE pFile, const void * buffer, HB_SIZE nSize, HB_FOFFSET nOffset );
extern HB_EXPORT HB_BOOL      hb_fileTruncAt    ( PHB_FILE pFile, HB_FOFFSET nOffset );
extern HB_EXPORT HB_FOFFSET   hb_fileSeek       ( PHB_FILE pFile, HB_FOFFSET nOffset, HB_USHORT uiFlags );
extern HB_EXPORT HB_FOFFSET   hb_fileSize       ( PHB_FILE pFile );
extern HB_EXPORT HB_BOOL      hb_fileEof        ( PHB_FILE pFile );
extern HB_EXPORT void         hb_fileFlush      ( PHB_FILE pFile, HB_BOOL fDirty );
extern HB_EXPORT void         hb_fileCommit     ( PHB_FILE pFile );
extern HB_EXPORT HB_BOOL      hb_fileConfigure  ( PHB_FILE pFile, int iIndex, PHB_ITEM pValue );
extern HB_EXPORT HB_FHANDLE   hb_fileHandle     ( PHB_FILE pFile );

extern HB_EXPORT PHB_FILE     hb_fileCreateTemp ( const char * pszDir, const char * pszPrefix,
                                                  HB_FATTR ulAttr, char * pszName );
extern HB_EXPORT PHB_FILE     hb_fileCreateTempEx( char * pszName,
                                                   const char * pszDir,
                                                   const char * pszPrefix,
                                                   const char * pszExt,
                                                   HB_FATTR ulAttr );
extern HB_EXPORT PHB_FILE     hb_filePOpen( const char * pszFileName, const char * pszMode );
extern HB_EXPORT PHB_FILE     hb_fileFromHandle( HB_FHANDLE hFile );
extern HB_EXPORT HB_BOOL      hb_fileDetach( PHB_FILE pFile );
extern HB_EXPORT HB_BOOL      hb_fileIsLocal( PHB_FILE pFile );
extern HB_EXPORT HB_BOOL      hb_fileIsLocalName( const char * pszFileName );
extern HB_EXPORT HB_SIZE      hb_fileResult( HB_SIZE nSize );
extern HB_EXPORT HB_BYTE *    hb_fileLoad( const char * pszFileName, HB_SIZE nMaxSize, HB_SIZE * pnSize );
extern HB_EXPORT HB_BYTE *    hb_fileLoadData( PHB_FILE pFile, HB_SIZE nMaxSize, HB_SIZE * pnSize );

/* interface to PRG level hb_vf*() file pointer items */
extern HB_EXPORT PHB_FILE     hb_fileParam( int iParam );
extern HB_EXPORT PHB_FILE     hb_fileParamGet( int iParam );
extern HB_EXPORT PHB_FILE     hb_fileItemGet( PHB_ITEM pItem );
extern HB_EXPORT PHB_ITEM     hb_fileItemPut( PHB_ITEM pItem, PHB_FILE pFile );
extern HB_EXPORT void         hb_fileItemClear( PHB_ITEM pItem );

#define HB_FILE_ERR_UNSUPPORTED  ( ( HB_ERRCODE ) FS_ERROR )

/* wrapper to fopen() which calls hb_fsNameConv() */
extern HB_EXPORT FILE *       hb_fopen( const char *path, const char *mode );

HB_EXTERN_END

#endif /* HB_APIFS_H_ */
