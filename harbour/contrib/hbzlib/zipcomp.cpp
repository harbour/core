
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zlib low level interface for Harbour
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
#define HB_OS_WIN_32_USED
#include "hbzip2.h"

extern PHB_ITEM pArray;
PHB_ITEM pDiskStatus=NULL;
PHB_ITEM pProgressInfo=NULL;
int iTotal=0;
CZipMemFile mf;
#ifdef __cplusplus
extern "C" {
#endif
extern HB_ZIP_INTERNAL pZipI;
bool     hb_SetProgressofTdSpan(DWORD , int iSoFar, void* pData);

int   hb_CompressFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    uLong uiPos;
    char szNewFile[MAXFILENAME];   
    int iCause=0;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    CZipArchive szZip;
    BOOL bReturn = true;
    DWORD dwSize=0;
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    if (pZipI.szComment != NULL)
        szZip.SetGlobalComment(pZipI.szComment);
    try {
        if (bFileExist && bOverWrite){
              szZip.Open(szFile,CZipArchive::zipCreate,0);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreate,0);
             }
             else {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
            }
        }

      }
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;
    bReturn=false;
	}
     catch(...){}
     if (HB_IS_BLOCK(pProgress))
        pProgressInfo=pProgress;
        for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) ;uiCount++)
        {
                const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
                dwSize=GetCurrentFileSize(szDummy);
                uiPos=uiCount;
                try {
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);

                     }
                     if (uiPos== hb_arrayLen(pArray))
                                iTotal+=dwSize;
                     if (!HB_IS_BLOCK(pProgress))
                     {
                     #if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                        if (bDrive)
                           szZip.AddNewFileDrv(szDummy, iCompLevel, true,NULL,NULL,65536);
                     #endif
                        if (bPath)
                           szZip.AddNewFile(szDummy, iCompLevel, true,NULL,NULL,65536);
                        if (!bDrive && !bPath)
                           szZip.AddNewFile(szDummy, iCompLevel, false,NULL,NULL,65536);
                        }
                     else
                     {
#if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                        if (bDrive)
                           szZip.AddNewFileDrv(szDummy, iCompLevel, true,hb_SetProgressofTdSpan,&(PHB_ITEM)pProgress,65536);
#endif
                        if (bPath)
                           szZip.AddNewFile(szDummy, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                        if (!bDrive && !bPath)
                           szZip.AddNewFile(szDummy, iCompLevel, false,hb_SetProgressofTdSpan,&pProgress,65536);
                      }
/*                      iTotal+=dwSize;*/
                      if (uiPos== hb_arrayLen(pArray))
                                            iTotal-=dwSize;
                      else
                        iTotal+=dwSize;
                }
                catch(...){}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,hb_arrayGetCPtr(pArray,uiCount));
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCount);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }
              
      }
    try {
    szZip.Close();
    }
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;
    bReturn=false;
	}
     catch(...){}
   hb_itemRelease(pProgressInfo    );
    return bReturn;  /* to avoid warning */
}

 int   hb_CmpTdSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,PHB_ITEM pDiskBlock,int iSpanSize,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    
    int iCause=0;
    BOOL bReturn=true;
    uLong uiPos;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    DWORD dwSize=0;
    if (pDiskBlock !=NULL){
        pDiskStatus=pDiskBlock;
        }
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    if (pZipI.szComment != NULL)
        szZip.SetGlobalComment(pZipI.szComment);

    if (iSpanSize ==0) {
        iSpanSize=1457664;
        }
        /*
    try {
               szZip.Open(szFile,CZipArchive::zipCreateSpan,iSpanSize);
      }
      */
      try {
        if (bFileExist && bOverWrite){
              szZip.Open(szFile,CZipArchive::zipCreateSpan,iSpanSize);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreateSpan,iSpanSize);
             }
             else {
             iCause=0;
             bReturn=false;
             return bReturn;
            }
        }
}
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;
    bReturn=false;
	}
     catch(...){}
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;
        
        for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) ;uiCount++)
        {
                const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
                dwSize=GetCurrentFileSize(szDummy);
                 uiPos=uiCount;
                try {
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (uiPos== hb_arrayLen(pArray))
                                iTotal+=dwSize;

                     if (!HB_IS_BLOCK(pProgress))
                     {

                     #if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv(szDummy, iCompLevel, true,NULL,NULL,65536);
                     #endif
                     if (bPath)
                        szZip.AddNewFile(szDummy, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile(szDummy, iCompLevel, false,NULL,NULL,65536);
                       }
                     else
                     {
#if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                        if (bDrive)
                           szZip.AddNewFileDrv(szDummy, iCompLevel, true,hb_SetProgressofTdSpan,&(PHB_ITEM)pProgress,65536);
#endif
                        if (bPath)
                           szZip.AddNewFile(szDummy, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                        if (!bDrive && !bPath)
                           szZip.AddNewFile(szDummy, iCompLevel, false,hb_SetProgressofTdSpan,&pProgress,65536);
                      }
                      if (uiPos== hb_arrayLen(pArray))
                                            iTotal-=dwSize;
                      else
                        iTotal+=dwSize;


                }
                catch(...){}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,hb_arrayGetCPtr(pArray,uiCount));
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCount);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }
              
      }
    try {
    szZip.Close();
    }
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;
    bReturn=false;
	}
     catch(...){}
    pDiskStatus=NULL    ;
    return  bReturn;  /* to avoid warning */
}
/*
bool hb_SetProgressofTdSpan(DWORD , int iSoFar, void* pData){

      int iReturn=1;
      iSoFar+=iTotal;
      PHB_ITEM pDisk;
      pDisk=  hb_itemPutNL(NULL,iTotal);
                   hb_vmEvalBlockV( pProgressInfo, 1,pDisk );
                   hb_itemRelease(pDisk);
      return iReturn;    

}
*/
bool hb_SetProgressofTdSpan(DWORD a, int iSoFar, void* pData){

      int iReturn=1;
/*      iSoFar+=iTotal;*/

      PHB_ITEM pDisk;
      PHB_ITEM pTotal =hb_itemPutNL(NULL,a);
      pDisk=  hb_itemPutNL(NULL,iSoFar);
                 hb_vmEvalBlockV( pProgressInfo, 2,pDisk,pTotal);
                   hb_itemRelease(pDisk);
                   hb_itemRelease(pTotal);

      return iReturn;    

}

int   hb_CompressFileStd(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];   
    int iCause=0;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    BOOL    bReturn=true;
    DWORD dwSize=0;
    CZipArchive szZip;
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    if (pZipI.szComment != NULL)
        szZip.SetGlobalComment(pZipI.szComment);

    try {
        if (bFileExist && bOverWrite){
              szZip.Open(szFile,CZipArchive::zipCreate,0);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreate,0);
             }
             else {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
            }
        }

      }
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;
    bReturn=false;
	}
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

                try {
                     dwSize=GetCurrentFileSize(szFiletoCompress);
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (!HB_IS_BLOCK(pProgress))
                     {

                     #if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv((const char*)szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     #endif   
                     if (bPath)
                        szZip.AddNewFile((const char*)szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile((const char*)szFiletoCompress, iCompLevel, false,NULL,NULL,65536);
                     }
                     else {
                     #if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv((const char*)szFiletoCompress, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                     #endif   
                     if (bPath)
                        szZip.AddNewFile((const char*)szFiletoCompress, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile((const char*)szFiletoCompress, iCompLevel, false,hb_SetProgressofTdSpan,&pProgress,65536);

                     }
                       iTotal+=dwSize;
                }
                catch(...){}
                if(pBlock !=NULL){
                   PHB_ITEM pFileName=hb_itemPutC(NULL,szFiletoCompress );
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);

                }
              
     
    try {
    szZip.Close();
    }
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;
    bReturn=false;
	}
     catch(...){}
    
    return     bReturn;  /* to avoid warning */
}
 int   hb_CmpTdSpanStd(char *szFile,char * szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,PHB_ITEM pDiskBlock,int iSpanSize,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    
    int iCause=0;
    BOOL bTdSpan=FALSE;
    BOOL bReturn=true;
    DWORD dwSize=0;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    if (pDiskBlock !=NULL){
        pDiskStatus=pDiskBlock;
        }

    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    if (pZipI.szComment != NULL)
        szZip.SetGlobalComment(pZipI.szComment);

    if (iSpanSize ==0) {
        iSpanSize=1457664;
        }
        try {
        if (bFileExist && bOverWrite){
            szZip.Open(szFile,CZipArchive::zipCreateSpan,iSpanSize);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreateSpan,iSpanSize);
            }
            else {
                iCause=0;
                return false;
            }
        }
    }
    catch (CZipException& e)
	{
        iCause=e.m_iCause       ;
	}
    catch(...){}
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

    try {
        if (szPassWord != NULL){
            szZip.SetPassword(szPassWord);
        }
                     dwSize=GetCurrentFileSize(szFiletoCompress);
                     if (!HB_IS_BLOCK(pProgress))
                     {

                     #if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv((const char*) szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     #endif
                     if (bPath)
                        szZip.AddNewFile((const char*) szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile((const char*) szFiletoCompress, iCompLevel, false,NULL,NULL,65536);
                     }
                     else {
                        #if defined(HB_OS_WIN_32) || defined(__MINGW32__)
                        if (bDrive)
                           szZip.AddNewFileDrv((const char*) szFiletoCompress, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                        #endif
                        if (bPath)
                           szZip.AddNewFile((const char*) szFiletoCompress, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                        if (!bDrive && !bPath)
                           szZip.AddNewFile((const char*) szFiletoCompress, iCompLevel, false,hb_SetProgressofTdSpan,&pProgress,65536);
                     }
                     iTotal+=dwSize;
                     

    }
    catch(...){}
    if(pBlock !=NULL){
        PHB_ITEM pFileName=hb_itemPutC(NULL,szFiletoCompress );
        hb_vmEvalBlockV( pBlock, 1, pFileName );
        hb_itemRelease(pFileName);
    }
    try {
        szZip.Close();
    }
    catch (CZipException& e)
	{
        iCause=e.m_iCause       ;
         bReturn=false;
	}
    catch(...){}
    pDiskStatus=NULL    ;
    return true;  /* to avoid warning */
}
BOOL hb_CreateZipInMemory(char *szFileToCompress,char *szFile)
{
BOOL bReturn=FALSE;
CZipArchive zip;
CZipMemFile mf1;
CZipFile f;
zip.Open(mf1, CZipArchive::zipCreate);
zip.AddNewFile(szFileToCompress, 8, true,NULL,NULL,65536);
zip.Close();
if (szFile  !=NULL)
   {
   if (f.Open(szFile, CZipFile::modeWrite|CZipFile::modeCreate, false))
   {
   int iLen = mf1.GetLength();
   BYTE* b = mf1.Detach();
	f.Write(b, iLen);
	f.Close();
	// must free detached memory
	free(b);
   bReturn=TRUE;
   }
}
return bReturn;
}

BOOL hb_SaveZipFileFromMemory(char *szFile)
{
CZipArchive zip;
CZipFile f;
BOOL bReturn=FALSE;
if (f.Open(szFile, CZipFile::modeWrite|CZipFile::modeCreate, false))
{
   int iLen = mf.GetLength();
   BYTE* b = mf.Detach();
	f.Write(b, iLen);
	f.Close();
	// must free detached memory
	free(b);
   bReturn=TRUE;
}
return bReturn;
}
#if defined(HB_OS_WIN_32) || defined(__MINGW32__)
DWORD GetCurrentFileSize(   LPCTSTR szFile)
{
   DWORD dwFileSize=0;
   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

            hFind = FindFirstFile(szFile,&hFilesFind);
                  if (hFind != INVALID_HANDLE_VALUE){
                      if (dwFlags & hFilesFind.dwFileAttributes) {
                         if(hFilesFind.nFileSizeHigh>0)
                              return ((hFilesFind.nFileSizeHigh*MAXDWORD)+hFilesFind.nFileSizeLow);    
                         else
                              return (hFilesFind.nFileSizeLow);
                       }

         }

   FindClose(hFind);
   return -1;
   }
#endif
#ifdef __cplusplus
}
#endif

