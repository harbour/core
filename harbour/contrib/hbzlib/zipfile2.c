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

#define CASESENSITIVITY (0)
#define WRITEBUFFERSIZE (8192)
extern int err;

void hb____ChangeFileDate(const char *filename,uLong dosdate,tm_unz tmu_date)
{
#ifdef defined(HB_OS_WIN_32)
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

int hb___MyMkdir(const char *DirectoryName)
{
    int uiReturn;
    uiReturn = mkdir(DirectoryName);
    return uiReturn;
}

int hb___MakeDir(char *NewDirectory)
{
  char *szBuffer ;
  char *p;
  int  uiLen = strlen(NewDirectory);  

  if (uiLen <= 0) 
    return 0;

  szBuffer = (void*)hb_xalloc(uiLen+1);
  strcpy(szBuffer,NewDirectory);
  
  if (szBuffer[uiLen-1] == '/') {
    szBuffer[uiLen-1] = '\0';
  }
  if (hb___MyMkdir(szBuffer) == 0)
    {
      hb_xfree((void*) szBuffer);
      return 1;
    }

  p = szBuffer+1;
  while (1)
    {
      char hold;

      while(*p && *p != '\\' && *p != '/')
        p++;
      hold = *p;
      *p = 0;
      if ((hb___MyMkdir(szBuffer) == -1) && (errno == ENOENT))
        {
          hb_xfree((void*) szBuffer);
          return 0;
        }
      if (hold == 0)
        break;
      *p++ = hold;
    }
  hb_xfree((void*) szBuffer);
  return 1;
}

int hb___ExtractOneFile(unzFile uf,const char* filename,int opt_extract_without_path,int opt_overwrite,PHB_ITEM pBlock)
{
    err = UNZ_OK;
    if (unzLocateFile(uf,filename,CASESENSITIVITY)!=UNZ_OK)
    {
        return 2;
    }

    if (hb___ExtractCurrentFile(uf,&opt_extract_without_path,
                                      &opt_overwrite,pBlock) == UNZ_OK)
        return 0;
    else
        return 1;
}

int hb___Extract(unzFile uf,int opt_extract_without_path,int opt_overwrite,PHB_ITEM pBlock)
{
        uLong uiCount;
        unz_global_info szGlobalUnzipInfo;


        err = unzGetGlobalInfo (uf,&szGlobalUnzipInfo);
        if (err!=UNZ_OK) {
/*                printf("error %d with zipfile in unzGetGlobalInfo \n",err);*/
}
        for (uiCount=1;uiCount<=szGlobalUnzipInfo.number_entry;uiCount++)
	{
        if (hb___ExtractCurrentFile(uf,&opt_extract_without_path,
                                      &opt_overwrite, pBlock) != UNZ_OK)
            break;

                if ((uiCount+1)<=szGlobalUnzipInfo.number_entry)
		{
			err = unzGoToNextFile(uf);
			if (err!=UNZ_OK)
			{
				break;
			}
		}
	}

	return 0;
}




int hb___unZipFiles(char *szFile,PHB_ITEM pBlock)
{
        const char *szZipFileName=NULL;
        const char *filename_to_extract=NULL;
	int i;
	int opt_do_list=0;
	int opt_do_extract=1;
    int opt_do_extract_withoutpath=0;
	int opt_overwrite=0;
	char filename_try[512];
	unzFile uf=NULL;

                if (szZipFileName == NULL)
                {
                    szZipFileName = szFile;
                }

        if (szZipFileName!=NULL)
	{
                strcpy(filename_try,szZipFileName);
                uf = unzOpen(szZipFileName);
		if (uf==NULL)
		{
			strcat(filename_try,".zip");
			uf = unzOpen(filename_try);
		}
	}

	if (uf==NULL)
	{
/*                printf("Cannot open %s or %s.zip\n",szZipFileName,szZipFileName);*/
		exit (1);
	}

        if (opt_do_extract==1)
    {
        if (filename_to_extract == NULL)
                    return hb___Extract(uf,opt_do_extract_withoutpath,opt_overwrite,pBlock);
        else
            return hb___ExtractOneFile(uf,filename_to_extract,
                                      opt_do_extract_withoutpath,opt_overwrite,pBlock);
    }
	unzCloseCurrentFile(uf);

	return 0;  /* to avoid warning */
}

int hb___ExtractCurrentFile(unzFile uf,const int* popt_extract_without_path,int* popt_overwrite,PHB_ITEM pBlock)
{
	char filename_inzip[256];
	char* filename_withoutpath;
	char* p;

    FHANDLE fout;
    BYTE * buf;
    uInt size_buf;
	
	unz_file_info file_info;
	uLong ratio=0;
      err=UNZ_OK;
	err = unzGetCurrentFileInfo(uf,&file_info,filename_inzip,sizeof(filename_inzip),NULL,0,NULL,0);

	if (err!=UNZ_OK)
	{
		return err;
	}

    size_buf = WRITEBUFFERSIZE;
    buf = (void*) hb_xalloc(size_buf);
    if (buf==NULL)
    {
        return UNZ_INTERNALERROR;
    }

	p = filename_withoutpath = filename_inzip;
	while ((*p) != '\0')
	{
		if (((*p)=='/') || ((*p)=='\\'))
			filename_withoutpath = p+1;
		p++;
	}

	if ((*filename_withoutpath)=='\0')
	{
		if ((*popt_extract_without_path)==0)
		{
                        hb___MyMkdir(filename_inzip);
		}
	}
	else
	{
		const char* write_filename;
		int skip=0;

		if ((*popt_extract_without_path)==0)
			write_filename = filename_inzip;
		else
			write_filename = filename_withoutpath;

                if(pBlock !=NULL){
                   PHB_ITEM pFileName=hb_itemPutC(NULL, (char *)write_filename);
                   hb_vmEvalBlockV( pBlock, 1, pFileName );
                   hb_itemRelease(pFileName);
                }


		err = unzOpenCurrentFile(uf);
		if (err!=UNZ_OK)
		{
		}


		if ((skip==0) && (err==UNZ_OK))
		{
                        fout=hb_fsCreate((char *) write_filename,FC_NORMAL);


            /* some zipfile don't contain directory alone before file */
            if ((fout==NULL) && ((*popt_extract_without_path)==0) && 
                                (filename_withoutpath!=(char*)filename_inzip))
            {
                char c=*(filename_withoutpath-1);
                *(filename_withoutpath-1)='\0';
                hb___MakeDir((char *)write_filename);
                *(filename_withoutpath-1)=c;

                 fout=hb_fsCreate((char *)write_filename,FC_NORMAL);
            }

		}

		if (fout!=NULL)
                {

			do
			{
				err = unzReadCurrentFile(uf,buf,size_buf);
				if (err<0)	
				{

					break;
				}
				if (err>0)
                                        if (hb_fsWrite(fout,buf,err)==0)
					{

                        err=UNZ_ERRNO;
						break;
					}
			}
			while (err>0);
                        hb_fsClose(fout);
			if (err==0) 
                                hb____ChangeFileDate(write_filename,file_info.dosDate,
					             file_info.tmu_date);
		}

        if (err==UNZ_OK)
        {
		    err = unzCloseCurrentFile (uf);
		    if (err!=UNZ_OK)
		    {

		    }
        }
        else
            unzCloseCurrentFile(uf); /* don't lose the error */       
	}

    hb_xfree((void*)buf);    
    return err;
}


