/*
 * $Id$
 */

/*
 *  DIR.C: Returns a Harbour array of specified directory contents filtered
 *         by the optional file and attribute mask.
 *
 * Latest mods:
 * 1.28   19990722   ptucker   Corrected? NT Extended modes
 * 1.21   19990722   ptucker   Implimented directory for MSVC
 *                             Includes new attributes
 * 1.20   19990722   ptucker   Corrected hang when attribute types have
 *                             been requested.
 *
 */

#if defined(__IBMCPP__)
   #define INCL_DOSFILEMGR
   #define INCL_DOSERRORS
#endif

#include "hbsetup.h"
#include "extend.h"
#include <string.h>
#include <ctype.h>
#include "itemapi.h"
#include "init.h"

#if defined(__GNUC__)
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <fcntl.h>
  #include <errno.h>
  #include <dirent.h>
  #include <time.h>

  #include <unistd.h>
  #if defined(__DJGPP__)
    #include <io.h>
  #endif

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif

#endif

#if defined(__WATCOMC__) || defined( _MSC_VER )
  #include <sys/stat.h>
  #include <share.h>
  #include <fcntl.h>
  #include <io.h>
  #include <errno.h>
  #include <direct.h>
  #include <time.h>

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif
#endif

#if defined(__IBMCPP__)
  #include <sys/stat.h>
/*  #include <share.h>  */
/*  #include <fcntl.h>  */
/*  #include <io.h>     */
/*  #include <errno.h>  */
/*  #include <direct.h> */
  #include <time.h>

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif
#endif

#if defined(__BORLANDC__)
  #include <sys\stat.h>
  #include <io.h>
  #include <fcntl.h>
  #include <share.h>
  #include <dirent.h>
  #include <dir.h>
  #include <dos.h>
  #include <time.h>
  #include <errno.h>

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
    #ifndef S_IEXEC
      #define S_IEXEC  0x0040 /* owner may execute <directory search> */
    #endif
    #ifndef S_IRWXU
      #define S_IRWXU  0x01c0 /* RWE permissions mask for owner */
    #endif
    #ifndef S_IRUSR
      #define S_IRUSR  0x0100 /* owner may read */
    #endif
    #ifndef S_IWUSR
      #define S_IWUSR  0x0080 /* owner may write */
    #endif
    #ifndef S_IXUSR
      #define S_IXUSR  0x0040 /* owner may execute <directory search> */
    #endif
  #endif
#endif

#if !defined(FA_RDONLY)
    #define FA_RDONLY       1
    #define FA_HIDDEN       2
    #define FA_SYSTEM       4
    #define FA_LABEL        8
    #define FA_DIREC        16
    #define FA_ARCH         32
#endif
/* this may not work, but lets find out (only implimented for msvc) */
#if !defined(FA_ENCRYPTED)
    #define FA_ENCRYPTED    64
    #define FA_NORMAL       128
    #define FA_TEMPORARY    256
    #define FA_SPARSE       512
    #define FA_REPARSE      1024
    #define FA_COMPRESSED   2048
    #define FA_OFFLINE      4096
    #define FA_NOTINDEXED   8192
    #define FA_VOLCOMP      32764
#endif

HARBOUR HB_DIRECTORY(void);

HB_INIT_SYMBOLS_BEGIN( Dir__InitSymbols )
{ "DIRECTORY", FS_PUBLIC, HB_DIRECTORY, 0 }
HB_INIT_SYMBOLS_END( Dir__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup Dir__InitSymbols
#endif


HARBOUR HB_DIRECTORY( void )
{
#if defined(HAVE_POSIX_IO)

   PHB_ITEM arg1_it = hb_param(1,IT_STRING);
   PHB_ITEM arg2_it = hb_param(2,IT_STRING);

   struct stat statbuf;
   struct tm *ft;

#if defined(_MSC_VER )
   struct _finddata_t entry;
   long hFile;
#elif defined(__IBMCPP__)
   FILEFINDBUF3 entry;
   HDIR         hFind = HDIR_CREATE;
   ULONG        fileTypes = FILE_ARCHIVED | FILE_DIRECTORY | FILE_SYSTEM | FILE_HIDDEN | FILE_READONLY;
   ULONG        findSize = sizeof( entry );
   ULONG        findCount = 1;
#else
   struct dirent *entry;
   DIR  * dir;
#endif

   char   fullfile[_POSIX_PATH_MAX+1];
   char   filename[_POSIX_PATH_MAX+1];
   char   pattern[_POSIX_PATH_MAX+1];
   char   dirname[_POSIX_PATH_MAX+1];
   char   string[_POSIX_PATH_MAX+1];
   char   pfname[_POSIX_PATH_MAX+1];
   char   pfext[_POSIX_PATH_MAX+1];
   char   fname[_POSIX_PATH_MAX+1];
   char   fext[_POSIX_PATH_MAX+1];
   char   filesize[10];
   char   ddate[9];
   char   ttime[9];
   char   aatrib[8];
   int    attrib;
   long   fsize;
   time_t ftime;
   char * pos;

   PHB_ITEM  pdir;
   PHB_ITEM  psubarray;
   PHB_ITEM  pfilename;
   PHB_ITEM  psize;
   PHB_ITEM  pdate;
   PHB_ITEM  ptime;
   PHB_ITEM  pattr;

   dirname[0] = '\0';
   pattern[0] = '\0';

   if( arg1_it )
   {
      strcpy(string, hb_parc(1));
      pos = strrchr(string,OS_PATH_DELIMITER);
      if( pos )
      {
         strcpy(pattern,(pos+1));
         *(pos+1) = '\0';
         strcpy(dirname,string);
      }
      else
      {
         strcpy(pattern,string);
         strcpy(dirname,".X");
         dirname[1] = OS_PATH_DELIMITER;
      }
   }
   if (strlen(pattern) < 1)
      strcpy(pattern,"*.*");
   if (strlen(dirname) < 1)
   {
      strcpy(dirname,".X");
      dirname[1] = OS_PATH_DELIMITER;
   }

   if (strlen(pattern) > 0)
   {
      strcpy(string,pattern);
      pos = strrchr(string,'.');
      if( pos )
      {
         strcpy(pfext,(pos+1));
         *pos = '\0';
         strcpy(pfname,string);
      }
      else
      {
         strcpy(pfname,string);
         pfext[0] = '\0';
      }
   }

/* redundant
   if (strlen(pfext) < 1)
      pfext[0] = '\0';
*/

   if (strlen(pfname) < 1)
      strcpy(pfname,"*");

/* debug code
   printf("\n dirname pattern %s %s ",dirname,pattern);
   printf("\n pfname pfext %s %s ",pfname,pfext);
   while(0==getchar());
*/
   /* should have drive,directory in dirname and filespec in pattern */

   tzset();
   pdir = hb_itemArrayNew(0);

#if defined(_MSC_VER)

   strcpy(string,dirname);
   strcat(string,pattern);
   if( (hFile = _findfirst( string, &entry )) != -1L )
   {

    do
    {
      strcpy(string,entry.name);
#elif defined(__IBMCPP__)
   strcpy(string,dirname);
   strcat(string,pattern);
   if( DosFindFirst( string, &hFind, fileTypes, &entry, findSize, &findCount, FIL_STANDARD ) == NO_ERROR && findCount > 0 )
   {
    do
    {
      strcpy(string,entry.achName);
#else
   dir = opendir( dirname );
   if (NULL == dir)
   {
      /* TODO: proper error handling */
      printf("\n invalid dirname %s ",dirname);
      while(0==getchar());
   }

   /* now put everything into an array */
   while ((entry = readdir( dir )) != NULL)
   {
      strcpy(string,entry->d_name);

#endif
      pos = strrchr(string,'.');
      if( pos )
      {
         strcpy(fext,(pos+1));
         *pos = '\0';
         strcpy(fname,string);
      }
      else
      {
         strcpy(fname,string);
         fext[0] = '\0';
      }

/* redundant
      if (strlen(fext) < 1)
         fext[0] = '\0';
 */

      if (strlen(fname) < 1)
         strcpy(fname,"*");

/*   debug code
      printf("\n fname fext %s %s ",fname,fext);
      while(0==getchar());
 */
      if (hb_strMatchRegExp( fname,pfname) && hb_strMatchRegExp( fext,pfext))
      {
         attrib      = 0;
         aatrib[0]   = '\0';
         filesize[0] = '\0';

#if defined(_MSC_VER)
         strcpy(filename,entry.name);
#elif defined(__IBMCPP__)
         strcpy( filename, entry.achName );
#else
         strcpy(filename,entry->d_name);
#endif
         strcpy(fullfile,dirname);
         strcat(fullfile,filename);

         if (-1 == stat(fullfile,&statbuf))
         {
            /* TODO: proper error handling */
            printf("\n invalid file %s ",fullfile);
            while(0==getchar());
         }
         else
         {
/* This might be a problem under Novell when the file is a directory */
/* needs test */
            fsize = statbuf.st_size;
            sprintf(filesize, "%ld", fsize);

            ftime = statbuf.st_mtime;
            ft = localtime(&ftime);
            sprintf(ddate, "%04d%02d%02d",
                    ft->tm_year+1900, ft->tm_mon + 1, ft->tm_mday);

            sprintf(ttime, "%02d:%02d:%02d",
                    ft->tm_hour, ft->tm_min, ft->tm_sec);

/* debug code

            printf("\n name date time    %s %s %s ",filename,ddate,ttime);
            while(0==getchar());
 */

         }

#if defined(OS_UNIX_COMPATIBLE)
/* GNU C on Linux or on other UNIX */
         aatrib[ 0 ] = '\0';
         if( S_ISREG(statbuf.st_mode) )
           strcat( aatrib, "A" );
         if( S_ISDIR(statbuf.st_mode) )
           strcat( aatrib, "D" );
         if( S_ISLNK(statbuf.st_mode) )
           strcat( aatrib, "L" );
         if( S_ISCHR(statbuf.st_mode) )
           strcat( aatrib, "C" );
         if( S_ISBLK(statbuf.st_mode) )
           strcat( aatrib, "B" );
         if( S_ISFIFO(statbuf.st_mode) )
           strcat( aatrib, "F" );
         if( S_ISSOCK(statbuf.st_mode) )
           strcat( aatrib, "K" );
#else
   #if defined(__IBMCPP__)
         attrib = entry.attrFile;
         if( attrib & FILE_ARCHIVED )
            strcat( aatrib, "A" );
         if( attrib & FILE_DIRECTORY )
            strcat( aatrib, "D" );
         if( attrib & FILE_HIDDEN )
            strcat( aatrib, "H" );
         if( attrib & FILE_READONLY )
            strcat( aatrib, "R" );
         if( attrib & FILE_SYSTEM )
            strcat( aatrib, "S" );
   #else
      #if defined(_MSC_VER)
         attrib = entry.attrib;
      #elif defined(__BORLANDC__) || defined(__DJGPP__)
         attrib = _chmod(fullfile,0);
      #else
         attrib = 0;
      #endif
         if (attrib & FA_ARCH)
            strcat(aatrib,"A");
         if (attrib & FA_DIREC)
            strcat(aatrib,"D");
         if (attrib & FA_HIDDEN)
            strcat(aatrib,"H");
         if (attrib & FA_RDONLY)
            strcat(aatrib,"R");
         if (attrib & FA_SYSTEM)
            strcat(aatrib,"S");
         if (attrib & FA_LABEL)
         {
            strcat(aatrib,"V");
            if (attrib & FA_VOLCOMP)
               strcat(aatrib,"L");  /* volume supports compression. */
         }
/* some of these are known to work under NT - I picked the letters to use.*/
/* needs testing on a Novell drive */
#if defined(USE_NT)
         if (attrib & FA_ENCRYPTED)
            strcat(aatrib,"E");
/*       if (attrib & FA_NORMAL) */
/*          strcat(aatrib,"N");  */
         if (attrib & FA_TEMPORARY)
            strcat(aatrib,"T");
         if (attrib & FA_SPARSE)
            strcat(aatrib,"P");
         if (attrib & FA_REPARSE)
            strcat(aatrib,"Z");
         if (attrib & FA_COMPRESSED)
            strcat(aatrib,"C");
         if (attrib & FA_OFFLINE)
            strcat(aatrib,"O");
         if (attrib & FA_NOTINDEXED)
            strcat(aatrib,"X");
#endif
   #endif
#endif
         /* TODO: attribute match rtn */
         pos = string;
         if( arg2_it && hb_parclen(2) >= 1)
         {
            strcpy(string, hb_parc(2));
            while (*pos != '\0')
            {
               *pos = (char)toupper(*pos);
               pos++;
            }
            pos = strchr(string,*aatrib);
         }

         if ( pos )
         {
             /* array  cname, csize, ddate, ctime, cattributes */
             pfilename = hb_itemPutC(NULL,filename);
             psize     = hb_itemPutC(NULL,filesize);
             pdate     = hb_itemPutDS(NULL,ddate);
             ptime     = hb_itemPutC(NULL,ttime);
             pattr     = hb_itemPutC(NULL,aatrib);
             psubarray = hb_itemArrayNew(5);
             hb_itemArrayPut(psubarray,1,pfilename);
             hb_itemArrayPut(psubarray,2,psize);
             hb_itemArrayPut(psubarray,3,pdate);
             hb_itemArrayPut(psubarray,4,ptime);
             hb_itemArrayPut(psubarray,5,pattr);

             hb_arrayAdd(pdir,psubarray);

             hb_itemRelease(pfilename);
             hb_itemRelease(psize);
             hb_itemRelease(pdate);
             hb_itemRelease(ptime);
             hb_itemRelease(pattr);
             hb_itemRelease(psubarray);
         }
      }
   }
#if defined(_MSC_VER)
   while( _findnext( hFile, &entry ) == 0 );
   _findclose( hFile );
#elif defined(__IBMCPP__)
   while( DosFindNext (hFind, &entry, findSize, &findCount) == NO_ERROR && findCount > 0 );
   DosFindClose( hFind );
#else
   closedir( dir );
#endif

   hb_itemCopy( &stack.Return, pdir ); /* DIRECTORY() returns an array */

   hb_itemRelease(pdir);

#if defined(_MSC_VER) || defined(__IBMCPP__)

   }
#endif
#endif /* HAVE_POSIX_IO */
}
