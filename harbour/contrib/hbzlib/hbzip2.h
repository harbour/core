/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Zlib API,
 *
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
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


#ifndef HB_APIZLIB_H_
#define HB_APIZLIB_H_
#define HB_OS_WIN_32_USED
#include <hbsetup.h>
#include <hbapi.h>
#include <hbapiitm.h>
#include <hbapifs.h>
#include <hbapigt.h>
#include <hbvmpub.h>
#include <hbvm.h>

#include <time.h>

#ifdef __cplusplus
#include <ZipArchive.h>
extern "C" {
#endif


#define filePos 1
#define Lenght 2
#define Method 3
#define Size 4
#define Ratio 5
#define Date 6
#define Time 7
#define Crc32 8
#define Attr  9
#define WRITEBUFFERSIZE (16384)
#define MAXFILENAME (256)
typedef struct _HB_ZIP_INTERNAL{
int iWrite;
int iExtract;
int iRead;
char * szComment;
PHB_ITEM pItem;
} HB_ZIP_INTERNAL,* PHB_ZIP_INTERNAL,* HB_ZIP_INTERNAL_PTR;
#ifndef LPCTSTR
typedef const char *LPCSTR;
typedef LPCSTR LPCTSTR;
#endif
extern char     *hb___CheckFile( char * szFile);
extern PHB_ITEM hb___GetFilesNamesFromZip(char *szFile,BOOL iMode);
extern void     hb_____GetTime(struct tm *tz);
extern int      hb_CmpPkSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern int      hb_CmpPkSpanStd(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern char     *hb_getPassWord(char *szName);
extern int      hb___GetNumbersofFilestoUnzip(char *szFile);
extern int      hb___SetCallbackFunc(PHB_ITEM pFunc);
extern int      hb_CmpTdSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,PHB_ITEM pDiskBlock,int iSpanSize ,BOOL bPath ,BOOL bDrive,PHB_ITEM pProgress);

extern int      hb_CompressFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern int      hb_CompressFileStd(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern int      hb_CmpTdSpanStd(char *szFile,char * szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,PHB_ITEM pDiskBlock,int iSpanSize,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern int      hb_UnzipAll(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pDiskBlock,PHB_ITEM pProgress);
extern int      hb_UnzipOne(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,char *szFiletoExtract,PHB_ITEM pProgress);
extern int      hb_UnzipSel(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pArray,PHB_ITEM pProgress);
extern int      hb_UnzipOneIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,int uiCount,PHB_ITEM pProgress);
extern int      hb_UnzipSelIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pSelArray,PHB_ITEM pProgress);
extern int      hb_DeleteOne(char *szFile,char *szFiletoDelete);
extern int      hb_DeleteSel(char *szFile,PHB_ITEM pArray,BOOL bCase);
extern int      hb_DeleteOneIndex(char *szFile,int uiCount);
extern int      hb_TestForPKS(char *szFile);
extern void     hb_SetZipBuff(int a,int b,int c);
extern void     hb_SetZipComment(char *szComment);
extern char     *hb_GetZipComment(char *szFile);
extern BOOL     hb_IsPassWord(char *szName);
extern unsigned long    GetCurrentFileSize(   LPCTSTR szFile);
extern BOOL     hb_SaveZipFileFromMemory(char *szFile);
extern BOOL     hb_CreateZipInMemory(char *szFileToCompress,char *szFile);

#ifdef __cplusplus
}
#endif
#endif /* HB_APIEXT_H_ */
