#include "hbzip.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>

# include <direct.h>
# include <io.h>

#define CASESENSITIVITY (0)
#define WRITEBUFFERSIZE (8192)
extern int err;

void hb____ChangeFileDate(const char *filename,uLong dosdate,tm_unz tmu_date)
{
#ifdef WIN32
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

int hb___ExtractOneFile(unzFile uf,const char* filename,int opt_extract_without_path,int opt_overwrite)
{
    err = UNZ_OK;
    if (unzLocateFile(uf,filename,CASESENSITIVITY)!=UNZ_OK)
    {
        return 2;
    }

    if (hb___ExtractCurrentFile(uf,&opt_extract_without_path,
                                      &opt_overwrite) == UNZ_OK)
        return 0;
    else
        return 1;
}

int hb___Extract(unzFile uf,int opt_extract_without_path,int opt_overwrite)
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
                                      &opt_overwrite) != UNZ_OK)
            break;

                if ((uiCount+1)<szGlobalUnzipInfo.number_entry)
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




int hb___unZipFiles(char *szFile)
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
/*                else if (filename_to_extract==NULL)
                        filename_to_extract = argv[i] ;*/

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
                    return hb___Extract(uf,opt_do_extract_withoutpath,opt_overwrite);
        else
            return hb___ExtractOneFile(uf,filename_to_extract,
                                      opt_do_extract_withoutpath,opt_overwrite);
    }
	unzCloseCurrentFile(uf);

	return 0;  /* to avoid warning */
}

int hb___ExtractCurrentFile(unzFile uf,const int* popt_extract_without_path,int* popt_overwrite)
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


