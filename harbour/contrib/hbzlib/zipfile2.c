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

#include "hbzip.h"
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
#include "unzip.h"
#define CASESENSITIVITY (0)
#define WRITEBUFFERSIZE (8192)
extern int err;
extern int Size_Buf;
uLong uiCounter;
unzFile szUnzipFile=NULL;

void hb____ChangeFileDate(char *filename,uLong dosdate,tm_unz tmu_date)
{
#if defined(HB_OS_WIN_32)
  HANDLE hFile;
  FILETIME ftm,ftLocal,ftCreate,ftLastAcc,ftLastWrite;
  HB_SYMBOL_UNUSED(tmu_date);        
  hFile = CreateFile(filename,GENERIC_READ | GENERIC_WRITE,
                      0,NULL,OPEN_EXISTING,0,NULL);
  GetFileTime(hFile,&ftCreate,&ftLastAcc,&ftLastWrite);
  DosDateTimeToFileTime((WORD)(dosdate>>16),(WORD)dosdate,&ftLocal);
  LocalFileTimeToFileTime(&ftLocal,&ftm);
  SetFileTime(hFile,&ftm,&ftLastAcc,&ftm);
  CloseHandle(hFile);
#elif defined(HB_OS_UNIX)
  struct utimbuf ut;
  struct tm newdate;
  newdate.tm_sec = tmu_date.tm_sec;
  newdate.tm_min=tmu_date.tm_min;
  newdate.tm_hour=tmu_date.tm_hour;
  newdate.tm_mday=tmu_date.tm_mday;
  newdate.tm_mon=tmu_date.tm_mon;
  if (tmu_date.tm_year > 1900)
      newdate.tm_year=tmu_date.tm_year - 1900;
  else
      newdate.tm_year=tmu_date.tm_year ;
  newdate.tm_isdst=-1;
  ut.actime=ut.modtime=mktime(&newdate);
  utime(filename,&ut);

#endif

}


/* mymkdir and change_file_date are not 100 % portable
   As I don't know well Unix, I wait feedback for the unix portion */
int hb___MakeDir(char *szNewDirectory)
{
  char *szBuffer ;
  char *szTemp;
  int  uiLen = strlen(szNewDirectory);  

  if (uiLen <= 0) 
    return 0;

  szBuffer = (void*)hb_xalloc(uiLen+1);
  strcpy(szBuffer,szNewDirectory);
  
  if (szBuffer[uiLen-1] == '/') {
    szBuffer[uiLen-1] = '\0';
  }
  if (hb_fsMkDir(szBuffer))
    {
      hb_xfree((void*) szBuffer);
      return 1;
    }

  szTemp = szBuffer+1;
  while (1)
    {
      char szHold;
      int iResult;
      while(*szTemp && *szTemp != '\\' && *szTemp != '/')
        szTemp++;
      szHold = *szTemp;
      *szTemp = 0;
      iResult=hb_fsMkDir(szBuffer);
      if (( iResult== -1) && (errno == ENOENT))
        {
          hb_xfree((void*) szBuffer);
          return 0;
        }
      if (szHold == 0)
        break;
      *szTemp++ = szHold;
    }
  hb_xfree((void*) szBuffer);
  return 1;
}

int hb___ExtractOneFile(unzFile szUnzipFile,const char* filename,BOOL opt_extract_without_path,BOOL opt_overwrite,PHB_ITEM pBlock)
{
    err = UNZ_OK;
    if (unzLocateFile(szUnzipFile,filename,CASESENSITIVITY)!=UNZ_OK)
    {
        return 2;
    }

    if (hb___ExtractCurrentFile(szUnzipFile,opt_extract_without_path,
                                      opt_overwrite,pBlock) == UNZ_OK)
        return 1;
    else
        return 0;
}

int hb___Extract(unzFile szUnzipFile,BOOL bExtractPath,BOOL opt_overwrite,PHB_ITEM pBlock)
{
        unz_global_info szGlobalUnzipInfo;

        err = unzGetGlobalInfo (szUnzipFile,&szGlobalUnzipInfo);

        if (err!=UNZ_OK) {
/*                printf("error %d with zipfile in unzGetGlobalInfo \n",err);*/
}
        for (uiCounter=1;uiCounter<=szGlobalUnzipInfo.number_entry;uiCounter++)
	{

        if (hb___ExtractCurrentFile(szUnzipFile,bExtractPath,
                                      opt_overwrite, pBlock) != UNZ_OK)
                                      {
            break;
                 }
                if (uiCounter+1<=szGlobalUnzipInfo.number_entry)
		{
                        err = unzGoToNextFile(szUnzipFile);
			if (err!=UNZ_OK)
			{
				break;
			}
		}
	}

        return 1;
}




BOOL hb___unZipFiles(char *szFile,PHB_ITEM pBlock,BOOL bExtractPath)
{
        const char *szZipFileName=NULL;
        const char *szFilename_to_Extract=NULL;
	int i;
        BOOL opt_do_extract=1;

	int opt_overwrite=0;
        char szFilename_Try[512];
/*        unzFile szUnzipFile=NULL;*/

                if (szZipFileName == NULL)
                {
                    szZipFileName = szFile;
                }

        if (szZipFileName!=NULL)
	{
                strcpy(szFilename_Try,szZipFileName);
                szUnzipFile = unzOpen(szZipFileName);
                if (szUnzipFile==NULL)
		{
                        strcat(szFilename_Try,".zip");
                        szUnzipFile = unzOpen(szFilename_Try);
		}
	}

        if (szUnzipFile==NULL)
	{
/*                printf("Cannot open %s or %s.zip\n",szZipFileName,szZipFileName);*/
		exit (1);
	}

        if (opt_do_extract)
    {
        if (szFilename_to_Extract == NULL)
                    return hb___Extract(szUnzipFile,bExtractPath,opt_overwrite,pBlock);
        else
            return hb___ExtractOneFile(szUnzipFile,szFilename_to_Extract,
                                      bExtractPath,opt_overwrite,pBlock);
    }
        unzCloseCurrentFile(szUnzipFile);

        return 1;  /* to avoid warning */
}

int hb___ExtractCurrentFile(unzFile szUnzipFile,BOOL popt_extract_without_path,BOOL popt_overwrite,PHB_ITEM pBlock)
{
	char filename_inzip[256];
	char* filename_withoutpath;
	char* p;
        char NewFileToWrite[256];
    FHANDLE nFileHandle;
    BYTE * szBuffer;

	
	unz_file_info file_info;

      err=UNZ_OK;
        err = unzGetCurrentFileInfo(szUnzipFile,&file_info,filename_inzip,sizeof(filename_inzip),NULL,0,NULL,0);

	if (err!=UNZ_OK)
	{
		return err;
	}

    Size_Buf = WRITEBUFFERSIZE;
    szBuffer = (void*) hb_xalloc(Size_Buf);
    if (szBuffer==NULL)
    {
        return UNZ_INTERNALERROR;
    }
        strcpy(NewFileToWrite,filename_inzip);
	p = filename_withoutpath = filename_inzip;
	while ((*p) != '\0')
	{
		if (((*p)=='/') || ((*p)=='\\'))
			filename_withoutpath = p+1;
		p++;
	}

	if ((*filename_withoutpath)=='\0')
	{
                if (popt_extract_without_path)
		{
                        hb_fsMkDir(filename_inzip);
		}
	}
	else
	{

                char* write_filename;
		int skip=0;

                if ((popt_extract_without_path)) {

			write_filename = filename_inzip;
                        
                        }
                else     {
 
			write_filename = filename_withoutpath;
                        
                        }
                if(pBlock !=NULL){
                   PHB_ITEM pFileName=hb_itemPutC(NULL, (char *)write_filename);
                   PHB_ITEM pFilePos=hb_itemPutNI(NULL,uiCounter);
                   hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                   hb_itemRelease(pFileName);
                   hb_itemRelease(pFilePos);
                }


                err = unzOpenCurrentFile(szUnzipFile);
		if (err!=UNZ_OK)
		{
		}


		if ((skip==0) && (err==UNZ_OK))
		{
                        nFileHandle=hb_fsCreate((char *) write_filename,FC_NORMAL);


            /* some zipfile don't contain directory alone before file */
            if ((nFileHandle==-1) && ((popt_extract_without_path)) && 
                                (filename_withoutpath!=(char*)filename_inzip))
            {
                char c=*(filename_withoutpath-1);
                *(filename_withoutpath-1)='\0';
                hb___MakeDir((char *)write_filename);
                *(filename_withoutpath-1)=c;

                 nFileHandle=hb_fsCreate((char *)write_filename,FC_NORMAL);
            }

		}

                if (nFileHandle!=NULL)
                {

			do
			{
                                err = unzReadCurrentFile(szUnzipFile,szBuffer,Size_Buf);
				if (err<0)	
				{

					break;
				}
				if (err>0)
                                        if (hb_fsWrite(nFileHandle,szBuffer,err)==0)
					{

                        err=UNZ_ERRNO;
						break;
					}
			}
			while (err>0);
                        hb_fsClose(nFileHandle);
			if (err==0) 
                                hb____ChangeFileDate(write_filename,file_info.dosDate,
					             file_info.tmu_date);
		}

        if (err==UNZ_OK)
        {
                    err = unzCloseCurrentFile (szUnzipFile);
		    if (err!=UNZ_OK)
		    {

		    }
        }
        else
            unzCloseCurrentFile(szUnzipFile); /* don't lose the error */       
	}

    hb_xfree((void*)szBuffer);    
    return err;
}


int hb___GetNumbersofFilestoUnzip(char *szFile)
{
/*int iNumbersOfFiles;*/

        const char *szZipFileName=NULL;
        char szFilename_Try[512];
        unz_global_info szGlobalUnzipInfo;
       szZipFileName = szFile;
/*                if (szZipFileName == NULL)
                {
                    szZipFileName = szFile;
                }
  */
        if (szZipFileName!=NULL)
	{
                strcpy(szFilename_Try,szZipFileName);
                szUnzipFile = unzOpen(szZipFileName);
                if (szUnzipFile==NULL)
		{
                        strcat(szFilename_Try,".zip");
                        szUnzipFile = unzOpen(szFilename_Try);
		}
	}

        if (szUnzipFile==NULL)
	{
		exit (1);
	}
        err = unzGetGlobalInfo (szUnzipFile,&szGlobalUnzipInfo);
/*        if (err==ZIP_OK)                                {
        iNumbersOfFiles=szGlobalUnzipInfo.number_entry;
}
*/
        unzCloseCurrentFile(szUnzipFile);

/*        return iNumbersOfFiles;   to avoid warning */
        return szGlobalUnzipInfo.number_entry;
}
PHB_ITEM hb___GetFilesNamesFromZip(char *szFile)
{
        const char *szZipFileName=NULL;
        char szFilename_Try[512];
        char szFileNameinZip[256];
        int iNumbersOfFiles;
        PHB_ITEM pItem=NULL;
        PHB_ITEM pArray=NULL;
        uLong uiCount;
        unz_global_info szGlobalUnzipInfo;
                if (szZipFileName == NULL)
                {
                    szZipFileName = szFile;
                }

        if (szZipFileName!=NULL)
	{
                strcpy(szFilename_Try,szZipFileName);
                szUnzipFile = unzOpen(szZipFileName);
                if (szUnzipFile==NULL)
		{
                        strcat(szFilename_Try,".zip");
                        szUnzipFile = unzOpen(szFilename_Try);
		}
	}

        if (szUnzipFile==NULL)
	{
                exit(1);
	}
        err = unzGetGlobalInfo (szUnzipFile,&szGlobalUnzipInfo);
        if (err==ZIP_OK)                                {
        iNumbersOfFiles=szGlobalUnzipInfo.number_entry;
        pArray=hb_itemArrayNew( iNumbersOfFiles );
}
 for(uiCount=0;uiCount<iNumbersOfFiles;uiCount++) {
                 unz_file_info file_info;
                err = unzGetCurrentFileInfo(szUnzipFile,&file_info,szFileNameinZip,sizeof(szFileNameinZip),NULL,0,NULL,0);
		if (err!=UNZ_OK)
		{
			break;
		}
                pItem=hb_itemPutC(NULL,szFileNameinZip);
                hb_itemArrayPut(pArray,uiCount+1,pItem);
                hb_itemRelease(pItem);
                if ((uiCount+1)<iNumbersOfFiles)
		{
                        err = unzGoToNextFile(szUnzipFile);
			if (err!=UNZ_OK)
			{
				break;
			}

                }

}
        unzCloseCurrentFile(szUnzipFile);
        hb_itemReturn(pArray);

}

