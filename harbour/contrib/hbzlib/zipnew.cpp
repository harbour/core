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
char  szTempTime[80];
PHB_ITEM pArray=NULL;
static PHB_ITEM pChangeDiskBlock;
extern PHB_ITEM pProgressInfo;
extern int iTotal;
int hb_CheckSpamMode(char * szFile);
/* hb_itemRelease(pChangeDiskBlock); */
#ifdef __cplusplus
extern "C" {
bool hb_SetCallBack(DWORD iNumber, int , void* pData);
bool hb_SetProgress(DWORD , int iSoFar, void* pData);
extern bool     hb_SetProgressofTdSpan(DWORD , int iSoFar, void* pData);
bool hb_SetProgressofUnc(DWORD , int iSoFar, void* pData);
HB_ZIP_INTERNAL pZipI;
#endif
int  hb_CmpPkSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    
    int iCause=0;
    uLong uiPos;
    DWORD dwSize=0;
    BOOL bReturn=true;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    try {
        if (bFileExist && bOverWrite){
              szZip.Open(szFile,CZipArchive::zipCreateSpan,0);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreateSpan,0);
             }           
             else {
             iCause=0;
             return false;
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

                     #if defined(__WIN32__) || defined(__MINGW32__)
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
                     #if defined(__WIN32__) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv(szDummy, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
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
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

    return  bReturn;  /* to avoid warning */
}


PHB_ITEM hb___GetFilesNamesFromZip(char *szFile,BOOL iMode)
{
        char szFileNameinZip[_POSIX_PATH_MAX];
        int iNumbersOfFiles;
        int iReturn=true;
        CZipArchive szZip;
        int uiCount;

        PHB_ITEM pItem=NULL;

int iOMode=0;
iOMode=hb_CheckSpamMode(szFile);
if (pZipI.iWrite>0) {
    szZip.SetAdvanced(pZipI.iWrite,pZipI.iExtract,pZipI.iRead);

}
    
     try {
        if(iOMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iOMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iOMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (...)    {	}

if (iReturn) {
        iNumbersOfFiles=szZip.GetNoEntries();
        pArray=hb_itemArrayNew( iNumbersOfFiles );
        time_t theTime;
        tm *SzTime;

 for(uiCount=0;uiCount<iNumbersOfFiles;uiCount++)
 {
		CZipFileHeader fh;
        szZip.GetFileInfo(fh, (WORD)uiCount);
        
                if (iMode)
                {
                    
                    const char *  szFileNameInZip;
                    CZipString szTempString;
                    PHB_ITEM pTempArray=hb_itemArrayNew(9);
                    char szAttr[5];
                    char szTime[5];
                    char *szMethod;
                    char szCRC[8];
                    int iRatio=0;
                    int iMeth=fh.m_uMethod;
                  int iLen;
                  int iCount=0;
                  int iiCount=0;
                  DWORD uAttr = fh.GetSystemAttr();
                    szTempString =(LPCTSTR)fh.GetFileName();                  
                    szFileNameInZip=(const char *)szTempString;
                    pItem=hb_itemPutC(NULL,(char *)szFileNameInZip);
                    hb_itemArrayPut(pTempArray,filePos,pItem);
                    hb_itemRelease(pItem);
      #if defined(__WIN32__)
        szAttr[0] = uAttr & FILE_ATTRIBUTE_READONLY ? _T('r') : _T('-');
        szAttr[1] = uAttr & FILE_ATTRIBUTE_HIDDEN ? _T('h') : _T('-');
        szAttr[2] = uAttr & FILE_ATTRIBUTE_SYSTEM ? _T('s') : _T('w');
        szAttr[3] = (uAttr & FILE_ATTRIBUTE_DIRECTORY) ? _T('D') : uAttr & FILE_ATTRIBUTE_ARCHIVE ? _T('a'): _T('-');
        #endif
        szAttr[4] = fh.IsEncrypted() ? _T('*') : _T(' ');

                      if (fh.m_uUncomprSize>0) {

                        pItem=hb_itemPutNL(NULL,fh.m_uUncomprSize);
                        hb_itemArrayPut(pTempArray,Lenght,pItem);
                        hb_itemRelease(pItem);
                        pItem=hb_itemPutNL(NULL,fh.m_uComprSize);
                        hb_itemArrayPut(pTempArray,Size,pItem);
                        hb_itemRelease(pItem);
                        iRatio=100-((fh.m_uComprSize*100)/fh.m_uUncomprSize);
                        if (iRatio <0){
                            iRatio=0;
                            }
                        pItem=hb_itemPutNL(NULL,iRatio);
                        hb_itemArrayPut(pTempArray,Ratio,pItem);
                        hb_itemRelease(pItem);
                        }
                        else {
                        pItem=hb_itemPutNL(NULL,fh.m_uUncomprSize);
                        hb_itemArrayPut(pTempArray,Lenght,pItem);
                        hb_itemRelease(pItem);
                        pItem=hb_itemPutNL(NULL,fh.m_uComprSize);
                        hb_itemArrayPut(pTempArray,Size,pItem);
                        hb_itemRelease(pItem);
                        iRatio=0;
                        pItem=hb_itemPutNL(NULL,iRatio);
                        hb_itemArrayPut(pTempArray,Ratio,pItem);
                        hb_itemRelease(pItem);
                        }
#if defined(__WIN32__)
        if (iMeth==0  || uAttr & FILE_ATTRIBUTE_DIRECTORY) {
                  szMethod="Stored";
            }
#endif
        if (iMeth==Z_DEFLATED)       {
            uInt iLevel=(uInt)((fh.m_uFlag & 0x6)/2);
            if (iLevel==0)                           {
                    szMethod="DeflatN";
                    }
            else if (iLevel==1) {
                    szMethod="DeflatX";
                    }
            else if ((iLevel==2) || (iLevel==3)) {
                                    szMethod="DeflatF";
                    }
           else {
                  szMethod="Unknow";
                  }
            }
                    pItem=hb_itemPutC(NULL,szMethod);
                    hb_itemArrayPut(pTempArray,Method,pItem);
                    hb_itemRelease(pItem);
            
                        sprintf(szCRC,"%8.8lx\n",(uLong)fh.m_uCrc32);

                        pItem=hb_itemPutCL(NULL,szCRC,8);
                        hb_itemArrayPut(pTempArray,Crc32,pItem);
                        hb_itemRelease(pItem);

                        pItem=hb_itemPutD(NULL,(long) (fh.m_uModDate >> 9) +1980 ,     (long)  ((fh.m_uModDate & ~0xFE00) >> 5) ,(long)fh.m_uModDate & ~0xFFE0);
                       /* (long)file_info.tmu_date.tm_year  ,(long)file_info.tmu_date.tm_mon + 1,(long)file_info.tmu_date.tm_mday);*/
                        hb_itemArrayPut(pTempArray,Date,pItem);
                        hb_itemRelease(pItem);
                        theTime=fh.GetTime();
                        SzTime= localtime(&theTime);
                        hb_____GetTime(SzTime);
                      iLen=strlen(szTempTime);

                      for(iCount=10;iCount<16;iCount++) {
                         if( (iCount>10) && (iCount<16)) {
                                szTime[iiCount]=szTempTime[iCount];
                                iiCount++;
                            }
                        }
                    pItem=hb_itemPutCL(NULL,szTime,5);
                    hb_itemArrayPut(pTempArray,Time,pItem);
                    hb_itemRelease(pItem);
                    pItem=hb_itemPutCL(NULL,szAttr,5);
                    hb_itemArrayPut(pTempArray,Attr,pItem);
                    hb_itemRelease(pItem);
               hb_itemArrayPut(pArray,uiCount+1,pTempArray);
                    hb_itemRelease(pTempArray);
                    
                }   
                else  {
                    const char *  szFileNameInZip;
                    CZipString szTempString=(LPCTSTR)fh.GetFileName();
                    szFileNameInZip=(const char *)szTempString;
                pItem=hb_itemPutC(NULL,(char *) szFileNameInZip);
                hb_itemArrayPut(pArray,uiCount+1,pItem);
                hb_itemRelease(pItem);
                }
}
}
            szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

          hb_itemReturn(  pArray);
}

 char *hb___CheckFile( char * szFile)
    {
        int uiCount,uiLen;
        int uiDot_Found=0;
        uiLen=strlen(szFile);

        for (uiCount=0;uiCount<uiLen;uiCount++)
            if (szFile[uiCount]=='.')
                uiDot_Found=1;

        if (uiDot_Found==0)
            strcat(szFile,".zip");

        return szFile;

    }


 void  hb_____GetTime(struct tm *tz)
{
  struct tm t;
  t.tm_sec    = tz->tm_sec;
  t.tm_min    = tz->tm_min;
  t.tm_hour   = tz->tm_hour;
  t.tm_mday   = tz->tm_mday;
  t.tm_mon    = tz->tm_mon;
  t.tm_year   = tz->tm_year;
  t.tm_wday   = 4;
  t.tm_yday   = 0;
  t.tm_isdst  = 0;
  strcpy(szTempTime, asctime(&t));

}
BOOL hb_IsPassWord(char *szFile)
{
bool bReturn=false;
CZipArchive szZip;
int iNumbersOfFiles;
int iMode=0;
iMode=hb_CheckSpamMode(szFile);
int iCause=0;
bool iReturn=true;
		CZipFileHeader fh;
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
                     }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                            }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                              }
                else {
                    iReturn =false;
                     }
                  }
             }
    }
    catch (CZipException& e)    {
      iCause=e.m_iCause       ;
	}

iNumbersOfFiles=szZip.GetNoEntries();
szZip.GetFileInfo(fh, (WORD)0);
if (fh.IsEncrypted()){
    bReturn=true;
    }
szZip.Close();
return bReturn;
}


int hb___GetNumbersofFilestoUnzip(char *szFile)
{
        int iNumbersOfFiles=0;
        CZipArchive szZip;

        szZip.SetSpanCallback(hb_SetCallBack,NULL);
        szZip.Open(szFile,CZipArchive::zipOpen,0);
        iNumbersOfFiles=szZip.GetNoEntries();
        szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

        return iNumbersOfFiles;
}
            
int  hb_CmpPkSpanStd(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    CZipArchive szZip;    
    int iCause=0;
    DWORD dwSize=0;
    BOOL bReturn=true;
    BOOL bFileExist=hb_fsFile((BYTE*)szFile);
    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
    try {
        if (bFileExist && bOverWrite){
              szZip.Open(szFile,CZipArchive::zipCreateSpan,0);
        }
        else{
            if (!bFileExist) {
                szZip.Open(szFile,CZipArchive::zipCreateSpan,0);
             }           
             else {
             iCause=0;
             return false;
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

                try {
                     dwSize=GetCurrentFileSize(szFiletoCompress);
                    if (szPassWord != NULL){
                        szZip.SetPassword(szPassWord);
                     }
                     if (!HB_IS_BLOCK(pProgress))
                     {
                     
                     #if defined(__WIN32__) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv(szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                        #endif
                     if (bPath)
                        szZip.AddNewFile(szFiletoCompress, iCompLevel, true,NULL,NULL,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile(szFiletoCompress, iCompLevel, false,NULL,NULL,65536);
                        }
                        else
                        {
                     #if defined(__WIN32__) || defined(__MINGW32__)
                     if (bDrive)
                        szZip.AddNewFileDrv(szFiletoCompress, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                        #endif
                     if (bPath)
                        szZip.AddNewFile(szFiletoCompress, iCompLevel, true,hb_SetProgressofTdSpan,&pProgress,65536);
                     if (!bDrive && !bPath)
                        szZip.AddNewFile(szFiletoCompress, iCompLevel, false,hb_SetProgressofTdSpan,&pProgress,65536);

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
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
    
    return     bReturn;  /* to avoid warning */
}


int hb___SetCallbackFunc(PHB_ITEM pFunc)
{
pChangeDiskBlock=pFunc;
pZipI.pItem=pFunc;
return true;
}
bool hb_SetCallBack(DWORD iNumber, int , void* pData)
{
/*    PHB_ITEM p=(PHB_ITEM)*pData;*/
    PHB_ITEM pDisk=hb_itemPutNL(NULL,iNumber);
    int iReturn=1;
                   hb_vmEvalBlockV( pChangeDiskBlock, 1,pDisk );
                   hb_itemRelease(pDisk);
/*                   hb_itemRelease(p);*/
    return iReturn;    

}
bool hb_SetProgress(DWORD , int iSoFar, void* pData){
/*    CProgressInfo* p = static_cast<CProgressInfo*>(pData);
	iSoFar += p->m_iTotalSoFar;
	//iTotal = p->m_iTotal;

	p->m_pProgress->SetPos(iSoFar);
	p->m_pProgress->RedrawWindow();
	return true;
*/
return TRUE;
}           
int hb_UnzipAll(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pDiskBlock,PHB_ITEM pProgress){
bool iReturn=true;
uLong uiCount=0;
int iCause=0;
int iMode=true;
CZipArchive szZip;
iTotal=0;
if (pDiskBlock){
    pChangeDiskBlock=pDiskBlock;
}
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
iMode=hb_CheckSpamMode(szFile);
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
                     }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                            }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                              }
                else {
                    iReturn =false;
                     }
                  }
             }
    }
    catch (CZipException& e)    {
      iCause=e.m_iCause       ;
	}

 if (iReturn) {

    for (uiCount=0;uiCount<=szZip.GetNoEntries();uiCount++){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        

        szZip.GetFileInfo(fh, (WORD)uiCount);
                    szTempString =(LPCTSTR)fh.GetFileName();                  
                    szFileNameInZip=(const char *)szTempString;
               iTotal=fh.m_uUncomprSize        ;
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {
         
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,NULL,NULL);
            }
            else
            {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,hb_SetProgressofUnc,(void*)&pProgress);
            }
            
        }
    catch (CZipException& e)
	{
      iCause=e.m_iCause       ;
	}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCount);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }

    }

    }
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

return iReturn;
}
int   hb_UnzipOne(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,char *szFiletoExtract,PHB_ITEM pProgress)
{
bool iReturn=true;
int uiCount=0;
int iCause=0;
int iMode=0;
iTotal=0;
   CZipArchive szZip;
    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
iMode=hb_CheckSpamMode(szFile) ;
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     /*
    try {
        if (hb_CheckSpamMode(szFile) !=-2) {
           szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else{            
               szZip.Open(szFile,CZipArchive::zipOpen,1);
            }
      }
*/
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }

    catch (CZipException& e)    {
      iCause=e.m_iCause       ;
	}
/*        if (iCause != 0){
            szZip.Close();
    }*/
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

        uiCount = szZip.FindFile((LPCTSTR)szFiletoExtract,false);
        if (uiCount ==-1){
        uiCount = szZip.FindFile((LPCTSTR)szFiletoExtract,true);
        }
        if (uiCount >=0){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();
        iTotal=fh.m_uUncomprSize;
        szFileNameInZip=(const char *)szTempString;
   
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,NULL,NULL);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL, hb_SetProgressofUnc,(void*)&pProgress);

            }
        }
    catch (CZipException& e)
	{
      iCause=e.m_iCause       ;
	}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }

    }
    szZip.Close();
    if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

return iReturn;
}

int   hb_DeleteOne(char *szFile,char *szFiletoDelete)
{
bool iReturn;
int uiCount=0;
int iCause=0;
   CZipArchive szZip;
int iMode=0;
iMode=hb_CheckSpamMode(szFile);
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1 ||iMode == -2) {
                    iReturn =false;
             }
        }
     }

    catch (CZipException e)    {
      iCause=e.m_iCause       ;
	}
        uiCount = szZip.FindFile((LPCTSTR)szFiletoDelete,false);
        if (uiCount ==-1){
        uiCount = szZip.FindFile((LPCTSTR)szFiletoDelete,true);
        }
        if (uiCount >=0){
		CZipFileHeader fh;
        szZip.GetFileInfo(fh, (WORD)uiCount);
        if (szZip.DeleteFile((WORD)uiCount))
            iReturn = true;
        else
            iReturn = false;
        }
            szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

return iReturn;
}

int   hb_DeleteSel(char *szFile,PHB_ITEM pArray,BOOL bCase)
{
    bool iReturn=true;
    int uiCount=0;
    int iCause=0;
    CZipArchive szZip;
    CZipStringArray  aFiles;
int iMode=0;
iMode=hb_CheckSpamMode(szFile);
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1 ||iMode == -2) {
                    iReturn =false;
             }
             }
    }

    catch (CZipException e)    {
        iCause=e.m_iCause       ;
	}
    if (iReturn){
    for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) ;uiCount++) {
        const char *szDummy = (char *)hb_arrayGetCPtr(pArray,uiCount) ;
        aFiles.Add(szDummy);
    }
    if  (bCase)
        szZip.DeleteFiles(aFiles,true);
    else
        szZip.DeleteFiles(aFiles,false);
}
    szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

    return iReturn;
}


int hb_UnzipSel(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pSelArray,PHB_ITEM pProgress)
{
bool iReturn=true;
int uiCount=0;
int iCause=0;
CZipArchive szZip;
int iMode=0;
iTotal=0;
iMode=hb_CheckSpamMode(szFile);
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (CZipException* e)	{
      iCause=e->m_iCause       ;
	}
    if (iReturn)  {
        for (iCause=0;(iCause<= hb_arrayLen(pSelArray)) ;iCause++){
        uiCount = szZip.FindFile((LPCTSTR)hb_arrayGetCPtr(pSelArray,iCause),false);
        if (uiCount ==-1){
        uiCount = szZip.FindFile((LPCTSTR)hb_arrayGetCPtr(pSelArray,iCause),true);
        }
        if (uiCount >=0){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();                  
        szFileNameInZip=(const char *)szTempString;
         iTotal=fh.m_uUncomprSize   ;
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,NULL,NULL);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,hb_SetProgressofUnc,(void*)&pProgress);
            }
        }
    catch (CZipException* e)
	{
      iCause=e->m_iCause       ;
	}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }

        }
}
    }
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
    szZip.Close();
return iReturn;
}

int  hb_TestForPKS(char *szFile)
{
    return hb_CheckSpamMode(szFile);
}
void hb_SetZipBuff(int a,int b,int c)
{
if (a && b && c){
   pZipI.iWrite = a>= 65535  ?  a : 65535;
   pZipI.iExtract = b>=16384 ? b : 16384;
   pZipI.iRead= c >=32768 ? c : 32768;
}
}
void hb_SetZipComment(char *szComment)
{
pZipI.szComment=szComment;
}
char * hb_GetZipComment(char *szFile)
{
const char *szReturn;
bool iReturn=true;
CZipString szTemp;
int iCause=0;
CZipArchive szZip;
int iMode=0;
iMode=hb_CheckSpamMode(szFile);
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (CZipException* e)	{
      iCause=e->m_iCause       ;
	}
if (iReturn) {
    szTemp=szZip.GetGlobalComment();
    szReturn=(const char *) szTemp;
}
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

szZip.Close();
return (char*)szReturn;

}

int   hb_UnzipOneIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,int uiCount,PHB_ITEM pProgress)
{
bool iReturn=true;
int iCause=0;
int iMode=0;
iTotal=0;
uiCount--;
    CZipArchive szZip;
    szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
iMode=hb_CheckSpamMode(szFile) ;
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     /*
    try {
        if (hb_CheckSpamMode(szFile) !=-2) {
           szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else{            
               szZip.Open(szFile,CZipArchive::zipOpen,1);
            }
      }
*/
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }

    catch (CZipException& e)    {
      iCause=e.m_iCause       ;
	}
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

        if (uiCount >=0){
        CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();
        iTotal=fh.m_uUncomprSize;
        szFileNameInZip=(const char *)szTempString;
   
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,NULL,NULL);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL, hb_SetProgressofUnc,(void*)&pProgress);

            }
        }
    catch (CZipException& e)
	{
      iCause=e.m_iCause       ;
	}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }

    }
    szZip.Close();
    if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

return iReturn;
}
int hb_UnzipSelIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pSelArray,PHB_ITEM pProgress)
{
bool iReturn=true;
int uiCount=0;
int iCause=0;
CZipArchive szZip;
int iMode=0;
iTotal=0;
iMode=hb_CheckSpamMode(szFile);
    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
     }
     if (HB_IS_BLOCK(pProgress))
     pProgressInfo=pProgress;

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                }
                else {
                    iReturn =false;
                }
             }
             }
    }
    catch (CZipException* e)	{
      iCause=e->m_iCause       ;
	}
    if (iReturn)  {
        for (iCause=0;(iCause<= hb_arrayLen(pSelArray)) ;iCause++){
        uiCount= hb_arrayGetNI(pSelArray,iCause)-1;
        if (uiCount >=0){
		CZipFileHeader fh;
        const char *  szFileNameInZip;
        CZipString szTempString;        
        szZip.GetFileInfo(fh, (WORD)uiCount);
        szTempString =(LPCTSTR)fh.GetFileName();                  
        szFileNameInZip=(const char *)szTempString;
         iTotal=fh.m_uUncomprSize   ;
        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,NULL,NULL);
            }
            else {
            szZip.SetPassword(szPassWord);
            szZip.ExtractFile((WORD)uiCount,(LPCTSTR)szPath,bWithPath,NULL,hb_SetProgressofUnc,(void*)&pProgress);
            }
        }
    catch (CZipException* e)
	{
      iCause=e->m_iCause       ;
	}
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,(char *)szFileNameInZip);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }

        }
}
    }
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }
    szZip.Close();
return iReturn;
}

int   hb_DeleteOneIndex(char *szFile,int uiCount)
{
bool iReturn;

int iCause=0;
uiCount--;
   CZipArchive szZip;
int iMode=0;
iMode=hb_CheckSpamMode(szFile);
     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
        }
        else {
            if (iMode ==-1 ||iMode == -2) {
                    iReturn =false;
             }
        }
     }

    catch (CZipException e)    {
      iCause=e.m_iCause       ;
	}
        if (uiCount >=0){
		CZipFileHeader fh;
        szZip.GetFileInfo(fh, (WORD)uiCount);
        if (szZip.DeleteFile((WORD)uiCount))
            iReturn = true;
        else
            iReturn = false;
        }
            szZip.Close();
     if (pChangeDiskBlock){
    hb_itemRelease(pChangeDiskBlock);
    }

return iReturn;
}

#ifdef __cplusplus
}
#endif
int hb_CheckSpamMode(char * szFile)
{
CZipArchive szZip;

int iReturn = 0;
int iCause=0;
szZip.SetSpanCallback(hb_SetCallBack,(void*) &pChangeDiskBlock);
try{  iCause=0; szZip.Open(szFile,CZipArchive::zipOpen,0);}
catch(CZipException &e) {
 if (e.m_iCause == CZipException::cdirNotFound) {
    szZip.Close(true);
    iReturn=114;
    return iReturn;
    }
 if (e.m_iCause == CZipException::noCallback) {
    szZip.Close(true);
    iReturn=103;
    return iReturn;
    
}
}
    iReturn =szZip.GetSpanMode();
    szZip.Close();

    return iReturn;
}
bool hb_SetProgressofUnc(DWORD a, int iSoFar, void* pData){

      int iReturn=1;
/*      iSoFar+=iTotal;*/

      PHB_ITEM pDisk;
      PHB_ITEM pTotal =hb_itemPutNL(NULL,iTotal);
      pDisk=  hb_itemPutNL(NULL,iSoFar);
                 hb_vmEvalBlockV( pProgressInfo, 2,pDisk,pTotal);
                   hb_itemRelease(pDisk);
                   hb_itemRelease(pTotal);

      return iReturn;    

}
