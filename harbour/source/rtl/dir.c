/*
 * $Id$
 */

#include <extend.h>
#include <string.h>
#include <itemapi.h>

#if defined(__GNUC__) || defined(__DJGPP__)
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <unistd.h>
  #include <fcntl.h>
  #include <errno.h>
  #include <dirent.h>

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif

  #define PATH_SEPARATOR '/'
#endif

#if defined(__WATCOMC__)
  #include <sys/stat.h>
  #include <share.h>
  #include <fcntl.h>
  #include <io.h>
  #include <errno.h>
  #include <direct.h>

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif

  #define PATH_SEPARATOR '\\'
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

  #define PATH_SEPARATOR '\\'
#endif


#ifdef __BORLANDC__
   #undef DIRECTORY
#endif

HARBOUR DIRECTORY( void )
{
#if defined(HAVE_POSIX_IO)
   PHB_ITEM arg1_it = _param(1,IT_STRING);
   PHB_ITEM arg2_it = _param(2,IT_STRING);

   extern STACK  stack;

   struct stat statbuf;
   struct dirent *entry;
   struct tm *ft;

   char   fullfile[_POSIX_PATH_MAX+1];
   char   pattern[_POSIX_PATH_MAX+1];
   char   dirname[_POSIX_PATH_MAX+1];
   char   filename[_POSIX_PATH_MAX+1];
   char   pfname[_POSIX_PATH_MAX+1];
   char   pfext[_POSIX_PATH_MAX+1];
   char   fname[_POSIX_PATH_MAX+1];
   char   fext[_POSIX_PATH_MAX+1];
   char   filesize[10];
   char   yyear[5];
   char   mmonth[3];
   char   dday[3];
   char   ddate[9];
   char   hh[3];
   char   mm[3];
   char   ss[3];
   char   ttime[9];
   int    attrib;
   char   aatrib[7];
   char   string[_POSIX_PATH_MAX+1];
   char * pos;
   long   nsize;
   long   fsize;
   int    i;
   DIR  * dir;
   time_t ftime;

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
      strcpy(string, _parc(1));
      pos = strrchr(string,PATH_SEPARATOR);
      if( pos )
      {
         strcpy(pattern,(pos+1));
         string[pos-string+1] = '\0';
         strcpy(dirname,string);
      }
      else
      {
         strcpy(pattern,string);
         strcpy(dirname,".X");
         dirname[1] = PATH_SEPARATOR;
      }
   }
   if (strlen(pattern) < 1)
      strcpy(pattern,"*.*");
   if (strlen(dirname) < 1)
   {
      strcpy(dirname,".X");
      dirname[1] = PATH_SEPARATOR;
   }

   if (strlen(pattern) > 0)
   {
      strcpy(string,pattern);
      pos = strrchr(string,'.');
      if( pos )
      {
         strcpy(pfext,(pos+1));
         string[pos-string] = '\0';
         strcpy(pfname,string);
      }
      else
      {
         strcpy(pfname,string);
         pfext[0] = '\0';
      }
   }
   if (strlen(pfext) < 1)
      pfext[0] = '\0';
   if (strlen(pfname) < 1)
      strcpy(pfname,"*");

/* debug code
   printf("\n dirname pattern %s %s ",dirname,pattern);
   printf("\n pfname pfext %s %s ",pfname,pfext);
   while(0==getchar());
*/
   /* should have drive,directory in dirname and filespec in pattern */

   pdir = hb_itemArrayNew(0);
   dir = opendir( dirname );
   if (NULL == dir)
   {
      printf("\n invalid dirname %s ",dirname);
      while(0==getchar());
   }

   /* now put everything into an array */
   while ((entry = readdir( dir )) != NULL)
   {

      strcpy(string,entry->d_name);
      pos = strrchr(string,'.');
      if( pos )
      {
         strcpy(fext,(pos+1));
         string[pos-string] = '\0';
         strcpy(fname,string);
      }
      else
      {
         strcpy(fname,string);
         fext[0] = '\0';
      }
      if (strlen(fext) < 1)
         fext[0] = '\0';
      if (strlen(fname) < 1)
         strcpy(fname,"*");

/*   debug code
      printf("\n fname fext %s %s ",fname,fext);
      while(0==getchar());
*/
      if (StrMatchDos( fname,pfname) && StrMatchDos( fext,pfext))
      {

         ddate[0] = '\0';
         ttime[0] = '\0';
         aatrib[0] = '\0';
         filesize[0] = '\0';
         filename[0] = '\0';
         attrib = 0;
         fullfile[0] = '\0';
         strcat(filename,entry->d_name);
         strcat(fullfile,dirname);
         strcat(fullfile,entry->d_name);

         if (-1 == stat(fullfile,&statbuf))
         {
            printf("\n invalid file %s ",fullfile);
            while(0==getchar());
         }

         fsize = statbuf.st_size;
         ltoa(fsize,filesize,10);
         ftime = statbuf.st_mtime;
         ft = localtime(&ftime);

         itoa(ft->tm_year+1900,yyear,10);
         strcat(ddate,yyear);
         itoa(ft->tm_mon,mmonth,10);
         if (strlen(mmonth) < 2)
         {
            strcat(mmonth,"0");
            strrev(mmonth);
         }
         strcat(ddate,mmonth);
         itoa(ft->tm_mday,dday,10);
         if (strlen(dday) < 2)
         {
            strcat(dday,"0");
            strrev(dday);
         }
         strcat(ddate,dday);
         itoa(ft->tm_hour,hh,10);
         if (strlen(hh) < 2)
         {
            strcat(hh,"0");
            strrev(hh);
         }
         strcat(ttime,hh);
         strcat(ttime,":");
         itoa(ft->tm_min,mm,10);
         if (strlen(mm) < 2)
         {
            strcat(mm,"0");
            strrev(mm);
         }
         strcat(ttime,mm);
         strcat(ttime,":");
         itoa(ft->tm_sec,ss,10);
         if (strlen(ss) < 2)
         {
            strcat(ss,"0");
            strrev(ss);
         }
         strcat(ttime,ss);

/* debug code
         printf("\n name date time    %s %s %s ",entry,ddate,ttime);
         while(0==getchar());
*/
         /* TODO: seems to not clear on root entries ? */
         attrib = _chmod(fullfile,0);
         if (attrib & FA_ARCH)
            strcat(aatrib,"A");
         if (attrib & FA_DIREC)
            strcat(aatrib,"D");
         if (attrib & FA_HIDDEN)
            strcat(aatrib,"H");
         if (attrib & FA_LABEL)
            strcat(aatrib,"V");
         if (attrib & FA_RDONLY)
            strcat(aatrib,"R");
         if (attrib & FA_SYSTEM)
            strcat(aatrib,"S");

         /* TODO: attribute match rtn */
         pos = 0;
         if( arg2_it )
         {
            strcpy(string, _parc(2));
            pos = strchr(strupr(string),*aatrib);
         }
         else
            pos = 1;

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
   closedir( dir );

   ItemCopy( &stack.Return, pdir ); /* DIRECTORY() returns an array */

   hb_itemRelease(pdir);

#endif /* HAVE_POSIX_IO */
   return;
}

static  BOOL  StrMatchDos (char *pszString, char *pszMask)
{
   while (*pszMask && *pszString)
   {
      if (*pszMask == '*')
      {
         while (*pszMask == '*')
             pszMask++;
         if (!(*pszMask))
            return (TRUE);
         else
            if (*pszMask == '?')
               pszString++;
            else
            {
               while (strupr(pszString) != strupr(pszMask))
               {
                  if (!(*(++pszString)))
                     return (FALSE);
               }
               while (strupr(pszString) == strupr(pszMask))
               {
                  if (!(*(++pszString)))
                     break;
               }
               pszMask++;
            }
      }
      else
         if (strupr(pszMask) != strupr(pszString) && *pszMask != '?')
            return (FALSE);
         else
         {
            pszMask++;
            pszString++;
         }
   }
   return !((!(*pszString) && *pszMask && *pszMask != '*') ||
           (!(*pszMask) && *pszString));
}

