/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   FILESEEK,FILESIZE,FILEATTR,FILETIME,FILEDATE,SETFATTR CT3 files function
 *
 * Copyright 2001 Luiz Rafael Culik<culik@sl.conex.net>
 *
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
#include <hbapi.h>

#if defined(HB_OS_DOS) 
static struct ffblk fsOldFiles;
#endif
#if defined(HB_OS_OS2) && defined(__GNUC__)

   #include "hb_io.h"

   #if defined(__EMX__)
      #include <emx/syscalls.h>
      #define gethostname __gethostname
   #endif

   #define MAXGETHOSTNAME 256      /* should be enough for a host name */

#elif defined(HB_OS_DOS)

   #if defined(__DJGPP__) || defined(__RSX32__) 
      #include <sys/param.h>  
   #endif
      #include "hb_io.h"
      #include "dos.h"
      #include <dir.h>
#endif
#if defined(__GNUC__) && !defined(__MINGW32__)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <errno.h>
   #include <dirent.h>
   #include <time.h>
   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif

#endif
#if (defined(HB_OS_WIN_32) || defined(__MINGW32__)) && !defined(__CYGWIN__)
static       HANDLE hLastFind;
static       WIN32_FIND_DATA  Lastff32;
LPTSTR GetDate(FILETIME *rTime);
LPTSTR GetTime(FILETIME *rTime);
#include <dir.h>
#endif

#if !defined(FA_ARCH)
   #define FA_RDONLY           1   /* R */
   #define FA_HIDDEN           2   /* H */
   #define FA_SYSTEM           4   /* S */
   #define FA_LABEL            8   /* V */
   #define FA_DIREC           16   /* D */
   #define FA_ARCH            32   /* A */
   #define FA_NORMAL           0
#endif
#if !defined(FA_DEVICE)
   #define FA_DEVICE          64   /* I */
/*   #define FA_NORMAL         128 */  /* N */  /* ignored */ /* Exists in BORLANDC */
   #define FA_TEMPORARY      256   /* T */
   #define FA_SPARSE         512   /* P */
   #define FA_REPARSE       1024   /* L */
   #define FA_COMPRESSED    2048   /* C */
   #define FA_OFFLINE       4096   /* O */
   #define FA_NOTINDEXED    8192   /* X */
   #define FA_ENCRYPTED    16384   /* E */
   #define FA_VOLCOMP      32768   /* M */
#endif

static USHORT osToHarbourMask( USHORT usMask )
{
   USHORT usRetMask;

   HB_TRACE(HB_TR_DEBUG, ("osToHarbourMask(%hu)", usMask));

   usRetMask = usMask;

   /* probably access denied when requesting mode */
   if( usMask == (USHORT) -1 )
      return 0;

#if defined(OS_UNIX_COMPATIBLE)
   /* The use of any particular FA_ define here is meaningless */
   /* they are essentially placeholders */
   usRetMask = 0;
   if( S_ISREG( usMask ) )
      usRetMask |= FA_ARCH;        /* A */
   if( S_ISDIR( usMask ) )
      usRetMask |= FA_DIREC;       /* D */
   if( S_ISLNK( usMask ) )
      usRetMask |= FA_REPARSE;     /* L */
   if( S_ISCHR( usMask ) )
      usRetMask |= FA_COMPRESSED;  /* C */
   if( S_ISBLK( usMask ) )
      usRetMask |= FA_DEVICE;      /* B  (I) */
   if( S_ISFIFO( usMask ) )
      usRetMask |= FA_TEMPORARY;   /* F  (T) */
   if( S_ISSOCK( usMask ) )
      usRetMask |= FA_SPARSE;      /* K  (P) */
#elif defined(HB_OS_OS2)
   usRetMask = 0;
   if( usMask & FILE_ARCHIVED )
      usRetMask |= FA_ARCH;
   if( usMask & FILE_DIRECTORY )
      usRetMask |= FA_DIREC;
   if( usMask & FILE_HIDDEN )
      usRetMask |= FA_HIDDEN;
   if( usMask & FILE_READONLY )
      usRetMask |= FA_RDONLY;
   if( usMask & FILE_SYSTEM )
      usRetMask |= FA_SYSTEM;
#endif

   return usRetMask;
}



HB_FUNC(FILEATTR)
{

#if defined(HB_OS_DOS)
   #if defined(__DJGPP__) || defined(__BORLANDC__)
   {
      char *szFile=hb_parc(1);
      int iAttri=0;

            #if defined(__BORLANDC__) && (__BORLANDC__ >= 1280)
               /* NOTE: _chmod( f, 0 ) => Get attribs
                        _chmod( f, 1, n ) => Set attribs
                        chmod() though, _will_ change the attributes */
               iAttri =  _rtl_chmod( szFile, 0, 0 );
            #elif defined(__BORLANDC__)
               iAttri =  _chmod( szFile, 0, 0 );
            #elif defined(__DJGPP__)
               iAttri =  _chmod( szFile, 0 );
            #endif
      hb_retni(iAttri);
     #endif
}

#elif defined(HB_OS_WIN_32)
     {
      DWORD dAttr;

      LPCTSTR cFile=hb_parc(1);
      dAttr=GetFileAttributes(cFile);
      hb_retnl(dAttr);
      }
#else
{
hb_retnl(-1);
}
#endif

}

HB_FUNC(SETFATTR)
{
#if defined(HB_OS_DOS)
   #if defined(__DJGPP__) || defined(__BORLANDC__)
   {

   int iFlags;
   int iReturn;
   const char *szFile=hb_parc(1);

   if (ISNUM(2))
      iFlags=hb_parni(2);
   else
      iFlags=32;
      switch (iFlags)
      {
      case FA_RDONLY :
         iReturn=_chmod(szFile,1,FA_RDONLY);
         break;
      case FA_HIDDEN :
         iReturn=_chmod(szFile,1,FA_HIDDEN);
         break;
      case FA_SYSTEM :                     
         iReturn=_chmod(szFile,1,FA_SYSTEM);
         break;
      case  (FA_ARCH) :
         iReturn=_chmod(szFile,1,FA_ARCH);
         break;
      case  (FA_RDONLY|FA_ARCH) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_ARCH);
         break;
      case  (FA_RDONLY|FA_SYSTEM) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_SYSTEM);
         break;
      case  (FA_RDONLY|FA_HIDDEN) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_HIDDEN);
         break;

      case  (FA_SYSTEM|FA_ARCH) :
         iReturn=_chmod(szFile,1,FA_SYSTEM|FA_ARCH);
         break;
      case  (FA_SYSTEM|FA_HIDDEN) :
         iReturn=_chmod(szFile,1,FA_SYSTEM|FA_HIDDEN);
         break;
      case  (FA_HIDDEN|FA_ARCH) :
         iReturn=_chmod(szFile,1,FA_HIDDEN|FA_ARCH);
         break;
      case  (FA_RDONLY|FA_SYSTEM|FA_HIDDEN) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_HIDDEN|FA_SYSTEM);
         break;

      case  (FA_RDONLY|FA_ARCH|FA_SYSTEM) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_ARCH|FA_SYSTEM);
         break;
      case  (FA_RDONLY|FA_ARCH|FA_HIDDEN) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_ARCH|FA_HIDDEN);
         break;
      case  (FA_RDONLY|FA_ARCH|FA_HIDDEN|FA_SYSTEM) :
         iReturn=_chmod(szFile,1,FA_RDONLY|FA_ARCH|FA_HIDDEN|FA_SYSTEM);
         break;
}      

      hb_retni(iReturn);
      }
   #endif

#elif defined(HB_OS_WIN_32)
   {
   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   DWORD dwLastError=ERROR_SUCCESS;
   int iAttr;
   LPCTSTR cFile=hb_parc(1);
   BOOL lSuccess;
   if (ISNUM(2))
      iAttr=hb_parni(2);
      if( iAttr & FA_RDONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;

      if( iAttr & FA_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;

      if( iAttr & FA_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;
      if( iAttr & FA_NORMAL )
         dwFlags |=    FILE_ATTRIBUTE_NORMAL;
      lSuccess=SetFileAttributes(cFile,dwFlags);
      if (lSuccess)
         hb_retni(dwLastError);
      else {
         dwLastError=GetLastError();
         switch (dwLastError) {
            case ERROR_FILE_NOT_FOUND :
                hb_retni(-2);
                break;
            case ERROR_PATH_NOT_FOUND :
               hb_retni(-3);
               break;
            case ERROR_ACCESS_DENIED:
               hb_retni(-5);
               break;
              }
         }
}

#else
{
hb_retnl(-1);
}
#endif
}

     HB_FUNC(FILESEEK)
{

   #if defined(HB_OS_WIN_32) && !defined(__CYGWIN__)
   {
      LPCTSTR szFile;
      DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
      int iAttr;
      if (hb_pcount() >=1){
         szFile=hb_parc(1);
         if (ISNUM(2))      {
         iAttr=hb_parnl(2);}
         else{
         iAttr=63;
         }
            if( iAttr & FA_RDONLY )
               dwFlags |= FILE_ATTRIBUTE_READONLY;

            if( iAttr & FA_HIDDEN )
               dwFlags |= FILE_ATTRIBUTE_HIDDEN;
   
            if( iAttr & FA_SYSTEM )
               dwFlags |= FILE_ATTRIBUTE_SYSTEM;
            if( iAttr & FA_NORMAL )
               dwFlags |=    FILE_ATTRIBUTE_NORMAL;

            hLastFind = FindFirstFile(szFile,&Lastff32);
                  if (hLastFind != INVALID_HANDLE_VALUE){
                     hb_retc(Lastff32.cFileName);
                     }

         }
         else {
            if (FindNextFile(hLastFind,&Lastff32))
               hb_retc(Lastff32.cFileName);
            else {
         FindClose(hLastFind);
         hLastFind=NULL;
         }
}
} 
#elif defined(HB_OS_DOS)
   {
      int iFind;
      char *szFiles;
      int iAttr;
      if (hb_pcount()>=1)
      {
         szFiles=hb_parc(1);
            if(ISNUM(2)){
            iAttr=hb_parnl(2);
            }
         else{
             iAttr=32;
             }
          iFind=findfirst(szFiles,&fsOldFiles,iAttr);
          if (!iFind){
              hb_retc(fsOldFiles.ff_name);
          }
      }
      else {
            iFind=findnext(&fsOldFiles);
            if (!iFind)
               hb_retc(fsOldFiles.ff_name);
            #if !defined (__DJGPP__) 
            else
               findclose(&fsOldFiles);
           #endif
      }
   }
#else
{
hb_retc("");
}
#endif
}
HB_FUNC(FILESIZE)
{

#if defined(HB_OS_WIN_32) && !defined(__CYGWIN__)
   {
   DWORD dwFileSize=0;
   LPCTSTR szFile;
   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

      int iAttr;
      if (hb_pcount() >=1){
         szFile=hb_parc(1);
         if (ISNUM(2))      {
            iAttr=hb_parnl(2);
         }
         else{
         iAttr=63;
         }
            if( iAttr & FA_RDONLY )
               dwFlags |= FILE_ATTRIBUTE_READONLY;

            if( iAttr & FA_HIDDEN )
               dwFlags |= FILE_ATTRIBUTE_HIDDEN;
   
            if( iAttr & FA_SYSTEM )
               dwFlags |= FILE_ATTRIBUTE_SYSTEM;
            if( iAttr & FA_NORMAL )
               dwFlags |=    FILE_ATTRIBUTE_NORMAL;

            hFind = FindFirstFile(szFile,&hFilesFind);
                  if (hFind != INVALID_HANDLE_VALUE){
                      if (dwFlags & hFilesFind.dwFileAttributes) {
                         if(hFilesFind.nFileSizeHigh>0)
                              hb_retnl((hFilesFind.nFileSizeHigh*MAXDWORD)+hFilesFind.nFileSizeLow);    
                         else
                              hb_retnl(hFilesFind.nFileSizeLow);
                       }
                   else
                           hb_retnl(-1);
                     }

         }
        
   else
   {
    if(Lastff32.nFileSizeHigh>0)
      dwFileSize=(Lastff32.nFileSizeHigh*MAXDWORD)+Lastff32.nFileSizeLow;    
     else
      dwFileSize=Lastff32.nFileSizeLow;
   hb_retnl(dwFileSize);
   }
}
#elif defined(HB_OS_DOS)
{
   int iFind;
   if (hb_pcount() >0) {
      char *szFiles=hb_parc(1);
      int iAttr=0 ;
      struct ffblk fsFiles;
      
      if (ISNUM(2))
         iAttr=hb_parni(2);
         iFind=findfirst(szFiles,&fsFiles,iAttr);
         if (!iFind)
             if ((iAttr>0) & (iAttr&fsFiles.ff_attrib))
                     hb_retnl(fsFiles.ff_fsize);
             else
                    hb_retnl(fsFiles.ff_fsize);
         else
             hb_retnl(-1);
   }
  else
  {
   hb_retnl(fsOldFiles.ff_fsize);
   }
}
#elif defined(OS_UNIX_COMPATIBLE)
{
   if (hb_pcount() >0) {
   const char *szFile=hb_parc(1);
   USHORT   ushbMask = FA_ARCH;
   USHORT   usFileAttr;
   struct stat sStat;
   if (ISNUM(2))
      ushbMask=hb_parni(2);
   if (stat(szFile,&sStat )!=-1){
       usFileAttr=osToHarbourMask(sStat.st_mode);
       if ((ushbMask>0) & (ushbMask&usFileAttr))
           hb_retnl(sStat.st_size);
       else
           hb_retnl(sStat.st_size);
           }
   else
        hb_retnl(-1);
}
}
#else
{
hb_retl(-1);
}
#endif
}

HB_FUNC(FILEDATE)
{

#if defined(HB_OS_WIN_32) && !defined(__CYGWIN__)
{
   LPCTSTR szFile;
   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

      int iAttr;
      if (hb_pcount() >=1){
         szFile=hb_parc(1);
         if (ISNUM(2))      {
         iAttr=hb_parnl(2);
         }
         else{
         iAttr=63;
         }
            if( iAttr & FA_RDONLY )
               dwFlags |= FILE_ATTRIBUTE_READONLY;

            if( iAttr & FA_HIDDEN )
               dwFlags |= FILE_ATTRIBUTE_HIDDEN;
   
            if( iAttr & FA_SYSTEM )
               dwFlags |= FILE_ATTRIBUTE_SYSTEM;
            if( iAttr & FA_NORMAL )
               dwFlags |=    FILE_ATTRIBUTE_NORMAL;

            hFind = FindFirstFile(szFile,&hFilesFind);
                  if (hFind != INVALID_HANDLE_VALUE)
                      if (dwFlags & hFilesFind.dwFileAttributes) 
                             hb_retds(GetDate(&hFilesFind.ftLastWriteTime));
                      else
                             hb_retds(GetDate(&hFilesFind.ftLastWriteTime));
                       
                   else
                           hb_retds("        ");
                     

         }

 else    {
     LPTSTR szDateString;
     szDateString=GetDate(&Lastff32.ftLastWriteTime);
     hb_retds(szDateString);

 }
}
#elif defined(HB_OS_DOS)
{
   int iFind;
   if (hb_pcount() >0) {
      char *szFiles=hb_parc(1);
      int iAttr=0 ;   
      struct ffblk fsFiles;
      if (ISNUM(2))
         iAttr=hb_parni(2);
         iFind=findfirst(szFiles,&fsFiles,iAttr);
         if (!iFind)
             if ((iAttr>0) & (iAttr&fsFiles.ff_attrib))
                    hb_retd( (long) (fsFiles.ff_fdate >> 9) +1980 ,     (long)  ((fsFiles.ff_fdate & ~0xFE00) >> 5) ,(long)fsFiles.ff_fdate & ~0xFFE0);
             else
                    hb_retd( (long) (fsFiles.ff_fdate >> 9) +1980 ,     (long)  ((fsFiles.ff_fdate & ~0xFE00) >> 5) ,(long)fsFiles.ff_fdate & ~0xFFE0);
            else
                  hb_retds("        ");
}
  else
   hb_retd( (long) (fsOldFiles.ff_fdate >> 9) +1980 ,     (long)  ((fsOldFiles.ff_fdate & ~0xFE00) >> 5) ,(long)fsOldFiles.ff_fdate & ~0xFFE0); 
 }
#elif defined(OS_UNIX_COMPATIBLE)
{
   if (hb_pcount() >0) {
      const char *szFile=hb_parc(1);
      struct stat sStat;
      time_t tm_t=0;
      char szDate[9];
      struct tm *filedate;
      USHORT   ushbMask = FA_ARCH;
      USHORT   usFileAttr;
      if (ISNUM(2))
         ushbMask=hb_parni(2);
      if (stat(szFile,&sStat) != -1) {
         tm_t = sStat.st_mtime;
         filedate = localtime(&tm_t);
         sprintf(szDate,"%04d%02d%02d",filedate->tm_year+1900,filedate->tm_mon+1,filedate->tm_mday);
         usFileAttr=osToHarbourMask(sStat.st_mode);
      if ((ushbMask>0) & (ushbMask&usFileAttr))
         hb_retds(szDate);
      else
         hb_retds(szDate);
           }
   else
        hb_retds("        ");
   }
}
#else
{
                          hb_retds("        ");
}
#endif
}
HB_FUNC(FILETIME)
{

#if defined(HB_OS_WIN_32) && !defined(__CYGWIN__)
{
   LPTSTR szDateString;
   LPCTSTR szFile;
   DWORD dwFlags=FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

      int iAttr;
      if (hb_pcount() >=1){
         szFile=hb_parc(1);
         if (ISNUM(2))      {
         iAttr=hb_parnl(2);
         }
         else{
         iAttr=63;
         }
            if( iAttr & FA_RDONLY )
               dwFlags |= FILE_ATTRIBUTE_READONLY;

            if( iAttr & FA_HIDDEN )
               dwFlags |= FILE_ATTRIBUTE_HIDDEN;
   
            if( iAttr & FA_SYSTEM )
               dwFlags |= FILE_ATTRIBUTE_SYSTEM;
            if( iAttr & FA_NORMAL )
               dwFlags |=    FILE_ATTRIBUTE_NORMAL;

            hFind = FindFirstFile(szFile,&hFilesFind);
                  if (hFind != INVALID_HANDLE_VALUE)
                      if (dwFlags & hFilesFind.dwFileAttributes) 
                             hb_retc(GetTime(&hFilesFind.ftLastWriteTime));
                      else
                             hb_retc(GetTime(&hFilesFind.ftLastWriteTime));
                       
                   else
                           hb_retc("  :  ");
                     

         }

   else {
   szDateString=GetTime(&Lastff32.ftLastWriteTime);

   hb_retc(szDateString);
}
 }
 
#elif defined(HB_OS_DOS)
{
   
   char szTime[7];
   int iFind;
   if (hb_pcount() >0) {
      char *szFiles=hb_parc(1);
      int iAttr=0 ;   
      struct ffblk fsFiles;
      if (ISNUM(2))
         iAttr=hb_parni(2);
         iFind=findfirst(szFiles,&fsFiles,iAttr);
         if (!iFind){
             if ((iAttr>0) & (iAttr&fsFiles.ff_attrib))
                    sprintf(szTime,"%2.2u:%2.2u",(fsFiles.ff_ftime >> 11) & 0x1f,(fsFiles.ff_ftime>> 5) & 0x3f);
             else
                    sprintf(szTime,"%2.2u:%2.2u",(fsFiles.ff_ftime >> 11) & 0x1f,(fsFiles.ff_ftime>> 5) & 0x3f);
                         hb_retc(szTime);
             }

         else
            hb_retc("  :  ");
   }
   else {
   sprintf(szTime,"%2.2u:%2.2u",(fsOldFiles.ff_ftime >> 11) & 0x1f,(fsOldFiles.ff_ftime>> 5) & 0x3f);
   hb_retc(szTime);
   }
}
#elif defined(OS_UNIX_COMPATIBLE)
{ 
   const char *szFile=hb_parc(1);
   struct stat sStat;
   time_t tm_t=0;
   char szTime[9];
   struct tm *ft;
   stat(szFile,&sStat   );
   tm_t = sStat.st_mtime;

   sprintf( szTime, "%02d:%02d:%02d",ft->tm_hour, ft->tm_min, ft->tm_sec );
   hb_retc(szTime);
}

#else
{
            hb_retc("  :  ");
}
#endif

}

#if (defined(HB_OS_WIN_32) || defined(__MINGW32__)) && !defined(__CYGWIN__)

#include <tchar.h>

    LPTSTR GetDate(FILETIME *rTime)
    {
        static const LPTSTR tszFormat = "yyyyMMdd"; 
        FILETIME ft;
        int iSize;        
        if (FileTimeToLocalFileTime(rTime, &ft))
        {
            SYSTEMTIME time;
            if (FileTimeToSystemTime(&ft, &time))
            {
                
                if ( iSize = GetDateFormat(NULL, 0, &time, tszFormat, NULL, 0))
                {
                     LPTSTR tszDateString;
                    if ( tszDateString = (LPTSTR)malloc(iSize+sizeof(TCHAR)))
                    {
                        if (GetDateFormat(NULL, 0, &time, tszFormat, tszDateString, iSize))
                            return tszDateString;
                        free(tszDateString);
                    }
                }
            }
        }
        
        return NULL;
    }

    LPTSTR GetTime(FILETIME *rTime)
    {
        static const LPTSTR tszFormat = "HH':'mm";/*_T("MM'\\'dd'\\'yyyy");*/
        FILETIME ft;
        
        if (FileTimeToLocalFileTime(rTime, &ft))
        {
            SYSTEMTIME time;
            if (FileTimeToSystemTime(&ft, &time))
            {
                int iSize;
                if ( iSize = GetTimeFormat(NULL, 0, &time, tszFormat, NULL, 0))
                {
                     LPTSTR tszDateString;
                    if ( tszDateString = (LPTSTR)malloc(iSize+sizeof(TCHAR)))
                    {
                        if (GetTimeFormat(NULL, 0, &time, tszFormat, tszDateString, iSize))
                            return tszDateString;
                        free(tszDateString);
                    }
                }
            }
        }
        
        return NULL;
    }



#endif
