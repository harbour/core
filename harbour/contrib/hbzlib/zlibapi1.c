/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Low level zip functions for Zlib dll Call
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>

#ifdef HB_OS_UNIX
# include <unistd.h>
# include <utime.h>
# include <sys/types.h>
# include <sys/stat.h>
#else
# include <direct.h>
# include <io.h>
#endif
#include "hbzip.h"
#include "zip.h"
#define WRITEBUFFERSIZE (16384)
#define MAXFILENAME (256)
int iZipOk;
int Opt_OverWrite=1;
int opt_compress_level=Z_DEFAULT_COMPRESSION;
int err=0;
int Size_Buf=0;
int errclose;
void *cBuffer=NULL;
zipFile szZipFile;
#if defined(HB_OS_WIN_32)
uLong hb___filetime(char *f, tm_zip *tmzip, uLong *dt)
{
  int iRet = 0;
  HB_SYMBOL_UNUSED(tmzip);
  {
      FILETIME ftLocal;
      HANDLE hFind;
      WIN32_FIND_DATA  ff32;

      hFind = FindFirstFile(f,&ff32);
      if (hFind != INVALID_HANDLE_VALUE)
      {
        FileTimeToLocalFileTime(&(ff32.ftLastWriteTime),&ftLocal);
        FileTimeToDosDateTime(&ftLocal,((LPWORD)dt)+1,((LPWORD)dt)+0);
        FindClose(hFind);
        iRet = 1;
      }
  }
  return iRet;
}
#elif defined(HB_OS_UNIX)
uLong hb___filetime(char *f, tm_zip *tmzip, uLong *dt)
{
  int ret=0;
  struct stat s;        /* results of stat() */
  struct tm* filedate;
  time_t tm_t=0;
  
  if (strcmp(f,"-")!=0)
  {
    char name[MAXFILENAME];
    int len = strlen(f);
    strcpy(name, f);
    if (name[len - 1] == '/')
      name[len - 1] = '\0';
    /* not all systems allow stat'ing a file with / appended */
    if (stat(name,&s)==0)
    {
      tm_t = s.st_mtime;
      ret = 1;
    }
  }
  filedate = localtime(&tm_t);

  tmzip->tm_sec  = filedate->tm_sec;
  tmzip->tm_min  = filedate->tm_min;
  tmzip->tm_hour = filedate->tm_hour;
  tmzip->tm_mday = filedate->tm_mday;
  tmzip->tm_mon  = filedate->tm_mon ;
  tmzip->tm_year = filedate->tm_year;

  return ret;
}
#else
uLong hb___filetime(char *f, tm_zip *tmzip, uLong *dt)
{
    return 0;
}
#endif

char *hb___CheckFile( char * szFile)
    {
        int uiCount,uiLen;
        int uiDot_Found=0;
        iZipOk=1;
        uiLen=strlen(szFile);

        for (uiCount=0;uiCount<uiLen;uiCount++)
            if (szFile[uiCount]=='.')
                uiDot_Found=1;

        if (uiDot_Found==0)
            strcat(szFile,".zip");

        return szFile;

    }

int hb___CompressOneFile(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite)

{
    int uiCount;
    char szNewFile[MAXFILENAME];
    strcpy(szNewFile,szFile);

    Size_Buf = WRITEBUFFERSIZE;
    if(iCompLevel != NULL){
        opt_compress_level=iCompLevel;
    }
    cBuffer = (void*) hb_xalloc(Size_Buf);
    if (cBuffer==NULL)
    {
          return ZIP_INTERNALERROR;
    }
    if (iZipOk==1)
    {

        szZipFile = zipOpen(szNewFile,1);
        if (szZipFile == NULL)   {
           err= ZIP_ERRNO;
        }
        else  {
                FHANDLE nFileHandle;
                int size_read;
                char *filenameinzip = szFiletoCompress;
                zip_fileinfo szZipFileInfo;

                szZipFileInfo.tmz_date.tm_sec = szZipFileInfo.tmz_date.tm_min = szZipFileInfo.tmz_date.tm_hour = 0;
                szZipFileInfo.tmz_date.tm_mday = szZipFileInfo.tmz_date.tm_mon = szZipFileInfo.tmz_date.tm_year = 0;
                szZipFileInfo.dosDate = 0;
                szZipFileInfo.internal_fa = 0;
                szZipFileInfo.external_fa = 0;
                hb___filetime(filenameinzip,&szZipFileInfo.tmz_date,&szZipFileInfo.dosDate);
                if(pBlock !=NULL){
                   PHB_ITEM pFileName=hb_itemPutC(NULL,szFiletoCompress );
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }


                err = zipOpenNewFileInZip(szZipFile,filenameinzip,&szZipFileInfo,
                                 NULL,0,NULL,0,NULL /* comment*/,
                                 (opt_compress_level != 0) ? Z_DEFLATED : 0,
                                 opt_compress_level);

                if (err == ZIP_OK)
                {
                    nFileHandle = hb_fsOpen(filenameinzip,FO_READ+FO_COMPAT);
                    if (nFileHandle==NULL)
                    {
                        err=ZIP_ERRNO;

                    }
                    hb_fsSetDevRaw(nFileHandle);
                }

                if (err == ZIP_OK)
                    do
                    {
                        err = ZIP_OK;

                        size_read = hb_fsRead(nFileHandle,cBuffer,Size_Buf);
                        if (size_read < Size_Buf)

                            if (hb_fsEof(nFileHandle)==0)                        {

                            err = ZIP_ERRNO;
                        }

                        if (size_read>0)
                        {
                            err = zipWriteInFileInZip (szZipFile,cBuffer,size_read);
                            if (err<0)
                            {
/*                                printf("error in writing %s in the zipfile\n",
                                                 filenameinzip);*/
                            }
                                
                        }
                    } while ((err == ZIP_OK) && (size_read>0));

                hb_fsClose(nFileHandle);
                if (err<0)
                    err=ZIP_ERRNO;
                else
                {                    
                    err = zipCloseFileInZip(szZipFile);
                    if (err!=ZIP_OK)
                    return err;
        }

        errclose = zipClose(szZipFile,NULL);
        if (errclose != ZIP_OK)
            return errclose;
    
 }
}

    hb_xfree( (void *) cBuffer ) ;
    return 1; /*   to avoid warning */
}

int   hb___CompressMultipleFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite)
{
    uLong uiCount;
    char szNewFile[MAXFILENAME];
    strcpy(szNewFile,szFile);

    Size_Buf = WRITEBUFFERSIZE;
    if(iCompLevel != NULL){
        opt_compress_level=iCompLevel;
    }

    cBuffer = (void*) hb_xalloc(Size_Buf);
    if (cBuffer==NULL)
    {
          return ZIP_INTERNALERROR;
    }
    if (iZipOk==1)
    {
/*        zipFile szZipFile;*/
        err=0;
/*        ULONG nSize=hb_arrayLen(pArray) ;*/
        szZipFile = zipOpen(szNewFile,0);
        if (szZipFile == NULL)
        {
            err= ZIP_ERRNO;
        }

        for (uiCount=1;(uiCount<= hb_arrayLen(pArray)) && (err==ZIP_OK);uiCount++)
        {
              
                FHANDLE nFileHandle;
                int size_read;
                zip_fileinfo szZipFileInfo;

                char *szDummy = hb_arrayGetCPtr(pArray,uiCount) ;
                const char *filenameinzip =szDummy;
/*                hb_itemFreeC(szDummy);*/
                szZipFileInfo.tmz_date.tm_sec = szZipFileInfo.tmz_date.tm_min = szZipFileInfo.tmz_date.tm_hour = 0;
                szZipFileInfo.tmz_date.tm_mday = szZipFileInfo.tmz_date.tm_min = szZipFileInfo.tmz_date.tm_year = 0;
                szZipFileInfo.dosDate = 0;
                szZipFileInfo.internal_fa = 0;
                szZipFileInfo.external_fa = 0;
                hb___filetime((char *)filenameinzip,&szZipFileInfo.tmz_date,&szZipFileInfo.dosDate);
                if(pBlock !=NULL){

                   PHB_ITEM pFileName=hb_itemPutC(NULL,hb_arrayGetCPtr(pArray,uiCount));
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCount);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }
                err = zipOpenNewFileInZip(szZipFile,filenameinzip,&szZipFileInfo,
                                 NULL,0,NULL,0,NULL /* comment*/,
                                 (opt_compress_level != 0) ? Z_DEFLATED : 0,
                                 opt_compress_level);

                if (err == ZIP_OK)                {

                nFileHandle = hb_fsOpen((char*)  filenameinzip,FO_READ+FO_COMPAT);
                                    
                    if (nFileHandle==NULL)
                    {
                        err=ZIP_ERRNO;
                    }

                        hb_fsSetDevRaw(nFileHandle);

                }

                if (err == ZIP_OK)
                    do
                    {
                        err = ZIP_OK;

                        size_read = hb_fsRead(nFileHandle,cBuffer,Size_Buf);
                        if (size_read < Size_Buf)
                            if (hb_fsEof(nFileHandle)==0)
                        {

                            err = ZIP_ERRNO;
                        }

                        if (size_read>0)
                        {
                            err = zipWriteInFileInZip (szZipFile,cBuffer,size_read);
                            if (err<0)
                            {
                            }
                                
                        }
                    } while ((err == ZIP_OK) && (size_read>0));

                hb_fsClose(nFileHandle);
                if (err<0)
                {
                    err=ZIP_ERRNO;
                    }
                else                {                    
                    err = zipCloseFileInZip(szZipFile);
                    if (err!=ZIP_OK){
/*                        printf("error in closing %s in the zipfile\n",
                                    filenameinzip);*/
                        return -1;


            }
        }

    }
            errclose = zipClose(szZipFile,NULL);
   }

    hb_xfree( (void *) cBuffer ) ;

    return 1;  /* to avoid warning */

}
