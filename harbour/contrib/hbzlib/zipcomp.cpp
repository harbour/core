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

#include "hbzip2.h"

extern PHB_ITEM pArray;
PHB_ITEM pDiskStatus=NULL;
int iTotal=0;
#ifdef __cplusplus
extern "C" {
#endif
extern HB_ZIP_INTERNAL pZipI;
/*bool  hb_SetProgressofTdSpan(DWORD , int iSoFar, void* pData);*/
int   hb_CompressFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];   
    int iCause=0;
    BOOL bFileExist=hb_fsFile(szFile);
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

	}
     catch(...){}
        for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) ;uiCount++)
        {
                const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
                try {
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (bDrive)
                        szZip.AddNewFileDrv(szDummy, iCompLevel, true,NULL,NULL,65536);
                     if (bPath)
                        szZip.AddNewFile(szDummy, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile(szDummy, iCompLevel, false,NULL,NULL,65536);
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

	}
     catch(...){}
    
    return iCause;  /* to avoid warning */
}

 int   hb_CmpTdSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,PHB_ITEM pDiskBlock,int iSpanSize,BOOL bPath,BOOL bDrive)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    
    int iCause=0;

    BOOL bFileExist=hb_fsFile(szFile);
    if (pDiskBlock !=NULL){
        pDiskStatus=pDiskBlock;
        }
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    if (pZipI.szComment != NULL)
        szZip.SetGlobalComment(pZipI.szComment);

    if (iSpanSize ==NULL) {
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
             return iCause;
            }
        }
}
    catch (CZipException* e)
	{
    iCause=e->m_iCause       ;

	}
     catch(...){}
        
        for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) ;uiCount++)
        {
                const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
                try {
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (bDrive)
                        szZip.AddNewFileDrv(szDummy, iCompLevel, true,NULL,NULL,65536);
                     if (bPath)
                        szZip.AddNewFile(szDummy, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile(szDummy, iCompLevel, false,NULL,NULL,65536);

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

	}
     catch(...){}
    pDiskStatus=NULL    ;
    return iCause;  /* to avoid warning */
}
/*
bool hb_SetProgressofTdSpan(DWORD , int iSoFar, void* pData){
      CZipStorage * pStorage=(CZipStorage*)pData;
      int iReturn=1;
      DWORD iVolumeLeft=pStorage->m_uCurrentVolSize;
      PHB_ITEM pDisk=NULL;
      PHB_ITEM pVolume=hb_itemPutNL(NULL,iVolumeLeft);
      int iPorcent=0;
      int iWrite=(pStorage->m_iBytesWritten*100);
      iTotal+=iSoFar;
      iPorcent=iWrite/iVolumeLeft;
      if (iPorcent==0 || pStorage->m_iBytesWritten==0 ){
        iWrite=        (iVolumeLeft*100);
        }
      pDisk=  hb_itemPutNL(NULL,iWrite);
                   hb_vmEvalBlockV( pDiskStatus, 2,pDisk ,pVolume);
                   hb_itemRelease(pDisk);
                   hb_itemRelease(pVolume);
    return iReturn;    

}
*/
int   hb_CompressFileStd(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];   
    int iCause=0;
    BOOL bFileExist=hb_fsFile(szFile);
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

	}
                try {
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (bDrive)
                        szZip.AddNewFileDrv((const char*)szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (bPath)
                        szZip.AddNewFile((const char*)szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile((const char*)szFiletoCompress, iCompLevel, false,NULL,NULL,65536);

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

	}
     catch(...){}
    
    return iCause;  /* to avoid warning */
}
 int   hb_CmpTdSpanStd(char *szFile,char * szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,PHB_ITEM pDiskBlock,int iSpanSize,BOOL bPath,BOOL bDrive)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    
    int iCause=0;
    BOOL bTdSpan=FALSE;
    BOOL bFileExist=hb_fsFile(szFile);
    if (pDiskBlock !=NULL){
        pDiskStatus=pDiskBlock;
        }
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    if (pZipI.szComment != NULL)
        szZip.SetGlobalComment(pZipI.szComment);

    if (iSpanSize ==NULL) {
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
                return iCause;
            }
        }
    }
    catch (CZipException& e)
	{
        iCause=e.m_iCause       ;
	}
    catch(...){}
    try {
        if (szPassWord != NULL){
            szZip.SetPassword(szPassWord);
        }
                     if (bDrive)
                        szZip.AddNewFileDrv((const char*) szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (bPath)
                        szZip.AddNewFile((const char*) szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile((const char*) szFiletoCompress, iCompLevel, false,NULL,NULL,65536);

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
	}
    catch(...){}
    pDiskStatus=NULL    ;
    return iCause;  /* to avoid warning */
}

#ifdef __cplusplus
}
#endif

