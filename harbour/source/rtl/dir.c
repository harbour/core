/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DIRECTORY() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 *  DIR.C: Returns a Harbour array of specified directory contents filtered
 *         by the optional file and attribute mask.
 *
 * Latest mods:
 * 1.53   19990917   dholm     Moved normal hb_itemReturn() and ...Release()
 *                             calls out of the MSC, IBM & MingW32 #if block.
 * 1.49   19990915   dholm     Added __MINGW32__ support
 * 1.46   19990915   ptucker   Return results are now fully compatible
 *                             particularly using MSVC - other os's need
 *                             testing.
 *                             Converted attribute handling and Added
 *                             conversion functions.
 * 1.44   19990911   dholm     Changed file size to numeric, like Clipper.
 * 1.28   19990722   ptucker   Corrected? NT Extended modes.
 * 1.21   19990722   ptucker   Implimented directory for MSVC.
 *                             Includes new attributes.
 * 1.20   19990722   ptucker   Corrected hang when attribute types have
 *                             been requested.
 *
 */

/*
 * Notes from the fringe... <ptucker@sympatico.ca>
 *
 * Clipper is a bit schizoid with the treatment of file attributes, but we've
 * emulated that weirdness here for your viewing amusement.
 *
 * In Clippers' homeworld of DOS, there are essentially 5 basic attributes:
 * 'A'rchive, 'H'idden, 'S'ystem, 'R'eadonly and 'D'irectory.  In addition, a
 * file can have no attributes, and only 1 file can have the 'V'olume label.
 *
 * For a given file request, you will receive any files that match the
 * passed filemask.  Included in this list are files which have attributes
 * matching the requested attribute as well as files that have no attribute,
 * or that have the 'A'rchive, or 'R'eadOnly attribute.
 *
 * The exception is Directory entries - these will always be excluded
 * even if they have the requested bit set. (Unless of course, you request "D"
 * as an attribute as well)
 *
 * The only valid characters that can be passed as an attribute request are
 * any of "DHS". Anything else is already implied, so it is ignored. Except
 * under NT, which may accept other attributes, but it is still a work in
 * progress - NT that is ;-).
 *
 * "V" is also valid, but is a special case - you will get back 1 entry only
 * that describes the volume label for the drive implied by the filemask.
 *
 * Differences from the 'standard':
 * Where supported, filenames will be returned in the same case as they
 * are stored in the directory.  Clipper (and VO too) will convert the
 * names to upper case.
 * Where supported, filenames will be the full filename as supported by
 * the os in use.  Under an MS Windows implimentation, an optional
 * 3rd parameter to Directory will allow you to receive the normal '8.3'
 * filename.
 *
 * TODO: - Volume label support
 *       - check that path support vis stat works on all platforms
 *       - UNC Support? ie: dir \\myserver\root
 *       - Use hb_fsFNameSplit()/Merge() for filename composing/decomposing.
 *
 */

#if defined(__IBMCPP__)
   #define INCL_DOSFILEMGR
   #define INCL_DOSERRORS
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
   #define WIN32_LEAN_AND_MEAN
   #include <windows.h>
   #if defined(__MINGW32__)
      #define HB_DONT_DEFINE_BASIC_TYPES
   #endif
#endif

#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "directry.ch"

#if defined(__GNUC__) && ! defined(__MINGW32__)
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

#if defined(__CYGWIN__)
   #include <time.h>
#endif

#if defined(__WATCOMC__) || defined(_MSC_VER) || defined(__MINGW32__)
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
   #define FA_RDONLY           1   /* R */
   #define FA_HIDDEN           2   /* H */
   #define FA_SYSTEM           4   /* S */
   #define FA_LABEL            8   /* V */
   #define FA_DIREC           16   /* D */
   #define FA_ARCH            32   /* A */
#endif
/* these work under NT but are otherwise used as placeholders for
   non MS o/s support */
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

/* Conversion functions  *** commented out functions not finished-not needed */
static USHORT osToHarbourMask( USHORT usMask )
{
   USHORT usRetMask;

   usRetMask = usMask;

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
#else
   #if defined(__IBMCPP__)
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
#endif

   return usRetMask;
}

static USHORT HarbourToOsMask( USHORT usMask )
{
   USHORT usRetMask = usMask;

#if defined(OS_UNIX_COMPATIBLE)
/* TODO: Need to look into this one */
/* what to do with Hidden and System? */

   usRetMask = 0;
   if( usMask & FA_ARCH )
      usRetMask = S_IFREG;    /* 0x0000  (numbers as in ms impimentation) */
   if( usMask & FA_DIREC )
      usRetMask |= S_IFDIR;   /* 0x3000 */
   if( usMask & FA_REPARSE )
      usRetMask |= S_IFLNK;   /* -not in ms- cygwin=120000 decimal */
   if( usMask & FA_COMPRESSED )
      usRetMask |= S_IFCHR;   /* 0x2000 */
   if( usMask & FA_DEVICE )    /* s_isblk... */
      usRetMask |= S_IFBLK;   /* 0x1000 */
   if( usMask & FA_TEMPORARY ) /* s_isfifo... */
      usRetMask |= S_IFIFO;   /* 0x4000 */
   if( usMask & FA_SPARSE )    /* s_issock... */
      usRetMask |= S_IFSOCK;  /* -not in ms- cygwin=140000 decimal! */
   if( usMask & FA_LABEL )
      usRetMask |= 0;	/* TODO: check this: S_IFLABEL; */ /* 0x5000 */
   if( usMask & FA_RDONLY )
      usRetMask &= ~S_IWUSR;   /* clear the WRITE permission bit */

#else
   #if defined(__IBMCPP__)
/* TODO: Need more! -> FA_LABEL */
      usRetMask = 0;
      if( usMask & FA_ARCH )
         usRetMask |= FILE_ARCHIVED;
      if( usMask & FA_DIREC )
         usRetMask |= FILE_DIRECTORY;
      if( usMask & FA_HIDDEN )
         usRetMask |= FILE_HIDDEN;
      if( usMask & FA_RDONLY )
         usRetMask |= FILE_READONLY;
      if( usMask & FA_SYSTEM )
         usRetMask |= FILE_SYSTEM;
   #endif
#endif

   return usRetMask;
}

/*
static USHORT osAttributesToMask( BYTE * byAttrib )
{
   USHORT usRetMask = 0;

#if defined(OS_UNIX_COMPATIBLE)
#else
   #if defined(__IBMCPP__)
   #else
      #if defined(_MSC_VER) || defined(__MINGW32__)
      #elif defined(__BORLANDC__) || defined(__DJGPP__)
      #else
      #endif
      #if defined(USE_NT)
      #endif
   #endif
#endif

   return usRetMask;
}

static BYTE * osMaskToAttributes( USHORT usMask, BYTE * byAttrib )
{
   char * cAttrib = ( char * ) byAttrib;

#if defined(OS_UNIX_COMPATIBLE)
#else
   #if defined(__IBMCPP__)
   #else
      #if defined(_MSC_VER) || defined(__MINGW32__)
      #elif defined(__BORLANDC__) || defined(__DJGPP__)
      #else
      #endif
      #if defined(USE_NT)
      #endif
   #endif
#endif

   return byAttrib;
}
*/

static USHORT HarbourAttributesToMask( BYTE * byAttrib )
{
   BYTE * pos = byAttrib;
   BYTE c;
   USHORT usMask = 0;

   while( ( c = toupper( *pos++ ) ) != '\0' )
   {
      switch( c )
      {
         case 'A': usMask |= FA_ARCH;       break;
         case 'D': usMask |= FA_DIREC;      break;
         case 'H': usMask |= FA_HIDDEN;     break;
         case 'R': usMask |= FA_RDONLY;     break;
         case 'S': usMask |= FA_SYSTEM;     break;
         case 'V': usMask |= FA_LABEL;      break;
         /* extensions */
/*       case 'N': usMask |= FA_NORMAL;     break; */
         case 'O': usMask |= FA_OFFLINE;    break;
         case 'T': usMask |= FA_TEMPORARY;  break;
         case 'I': usMask |= FA_DEVICE;     break;
         case 'M': usMask |= FA_VOLCOMP;    break;
         case 'E': usMask |= FA_ENCRYPTED;  break;
         case 'X': usMask |= FA_NOTINDEXED; break;
         case 'C': usMask |= FA_COMPRESSED; break;
         case 'L': usMask |= FA_REPARSE;    break;
         case 'P': usMask |= FA_SPARSE;     break;
      }
   }
   return usMask;
}

static BYTE * HarbourMaskToAttributes( USHORT usMask, BYTE * byAttrib )
{
   char * cAttrib = ( char * ) byAttrib;

   *cAttrib = '\0';
   if( usMask & FA_RDONLY )
      strcat( cAttrib, "R" );
   if( usMask & FA_DIREC )
      strcat( cAttrib, "D" );
   if( usMask & FA_HIDDEN )
      strcat( cAttrib, "H" );
   if( usMask & FA_SYSTEM )
      strcat( cAttrib, "S" );
   if( usMask & FA_ARCH )
      strcat( cAttrib, "A" );
   if( usMask & FA_LABEL )
   {
      strcat( cAttrib, "V" );
      if( usMask & FA_VOLCOMP )
         strcat( cAttrib, "M" );  /* volume supports compression. */
   }
/* thse can be returned under NT with NTFS - I picked the letters to use.*/
/* needs testing on a Novell drive */
/* PLEASE! If these cause you trouble let me know! <ptucker@sympatico.ca> */

   if( usMask & FA_DEVICE )
      strcat( cAttrib, "I" );
/* if( usMask & FA_NORMAL )      */
/*    strcat( cAttrib, "N" );   */
   if( usMask & FA_TEMPORARY )
      strcat( cAttrib, "T" );
   if( usMask & FA_SPARSE )
      strcat( cAttrib, "P" );
   if( usMask & FA_REPARSE )
      strcat( cAttrib, "L" );
   if( usMask & FA_COMPRESSED )
      strcat( cAttrib, "C" );
   if( usMask & FA_OFFLINE )
      strcat( cAttrib, "O" );
   if( usMask & FA_NOTINDEXED )
      strcat( cAttrib, "X" );
   if( usMask & FA_ENCRYPTED )
      strcat( cAttrib, "E" );

   return byAttrib;
}

/* NOTE: The third (lEightDotThree) parameter is a Harbour extension. */

HARBOUR HB_DIRECTORY( void )
{
#if defined(HAVE_POSIX_IO)

   PHB_ITEM pDirSpec = hb_param( 1, IT_STRING );
   PHB_ITEM pAttributes = hb_param( 2, IT_STRING );
   PHB_ITEM pEightDotThree = hb_param( 3, IT_LOGICAL );

   char     fullfile[ _POSIX_PATH_MAX + 1 ];
   char     filename[ _POSIX_PATH_MAX + 1 ];
   char     pattern[ _POSIX_PATH_MAX + 1 ];
   char     dirname[ _POSIX_PATH_MAX + 1 ];
   char     string[ _POSIX_PATH_MAX + 1 ];
   char     pfname[ _POSIX_PATH_MAX + 1 ];
   char     pfext[ _POSIX_PATH_MAX + 1 ];
   char     fname[ _POSIX_PATH_MAX + 1 ];
   char     fext[ _POSIX_PATH_MAX + 1 ];
   BOOL     bEightDotThree;
   char     ddate[ 9 ];
   char     ttime[ 9 ];
   char     aatrib[ 17 ];
   int      attrib;
   long     fsize;
   time_t   ftime;
   char *   pos;
   int      iDirnameLen;
   USHORT   usosMask;
   USHORT   ushbMask = FA_ARCH;
   PHB_ITEM pDir;

   struct stat statbuf;
   struct tm * ft;

#if defined(_MSC_VER ) || defined(__MINGW32__)
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

   dirname[ 0 ] = '\0';
   pattern[ 0 ] = '\0';

   /* Get the passed attributes and convert them to Harbour Flags */
   if( pAttributes && hb_itemGetCLen( pAttributes ) >= 1 )
      ushbMask |= HarbourAttributesToMask( ( BYTE * ) hb_itemGetCPtr( pAttributes ) );

   /* Translate Harbour Flags into OS specific flags */
   usosMask = HarbourToOsMask( ushbMask );

   /* Do we want 8.3 support? */
   bEightDotThree = ( pEightDotThree ? hb_itemGetL( pEightDotThree ) : FALSE );

   pattern[ 0 ] = '\0';

   /* TODO: add supporting code */
   /* if you request the volume label, that's all you get */
   if( ushbMask & FA_LABEL )
   {
      /* get rid of anything else */
      ushbMask = FA_LABEL;

      if( pDirSpec )
      {
         strcpy( string, hb_itemGetCPtr( pDirSpec ) );
         pos = strrchr( string, ':' );
         if( pos )
            *( ++pos ) = '\0';
         else
            string[ 0 ] = '\0';

         strcpy( pattern, string );
      }
   }
   else
   {
      if( pDirSpec )
      {
         strcpy( string, hb_itemGetCPtr( pDirSpec ) );
         pos = strrchr( string, OS_PATH_DELIMITER );
         if( pos )
         {
            strcpy( pattern, pos + 1 );
            *( pos + 1 ) = '\0';
            strcpy( dirname, string );
         }
         else
         {
            strcpy( pattern, string );
            strcpy( dirname, ".X" );
            dirname[ 1 ] = OS_PATH_DELIMITER;
         }
      }
      if( !*pattern )
         strcpy( pattern, "*.*" );
   }

   iDirnameLen = strlen( dirname );
   if( iDirnameLen < 1 )
   {
      strcpy( dirname, ".X" );
      dirname[ 1 ] = OS_PATH_DELIMITER;
      iDirnameLen = 2;
   }

   if( strlen( pattern ) > 0 )
   {
      strcpy( string, pattern );
      pos = strrchr( string, '.' );
      if( pos )
      {
         strcpy( pfext, pos + 1 );
         *pos = '\0';
         strcpy( pfname, string );
      }
      else
      {
         strcpy( pfname, string );
         pfext[ 0 ] = '\0';
      }
   }

   if( strlen( pfname ) < 1 )
      strcpy( pfname, "*" );

/* debug code
   printf( "\n dirname pattern %s %s ", dirname, pattern );
   printf( "\n pfname pfext %s %s ", pfname, pfext );
   while( 0 == getchar() );
*/
   /* should have drive,directory in dirname and filespec in pattern */

   tzset();
   pDir = hb_itemArrayNew( 0 );

#if defined(_MSC_VER) || defined(__MINGW32__)

   strcpy( string, dirname );
   strcat( string, pattern );

   if( ( hFile = _findfirst( string, &entry ) ) != -1L )
   {
      do
      {
         strcpy( string, dirname );
         strcat( string, entry.name );

         /* this needs the full path to the file */
         if( bEightDotThree )
            GetShortPathName( string, string, _POSIX_PATH_MAX );

#elif defined(__IBMCPP__)
   strcpy( string, dirname );
   strcat( string, pattern );
   if( DosFindFirst( string, &hFind, fileTypes, &entry, findSize, &findCount, FIL_STANDARD ) == NO_ERROR && findCount > 0 )
   {
      do
      {
         strcpy( string, entry.achName );
#else
   #if defined(__WATCOMC__)
   /* opendir in Watcom doesn't like the path delimiter at the end of a string */
      dirname[ iDirnameLen ] = '.';
      dirname[ iDirnameLen + 1 ] = '\0';
   #endif
   dir = opendir( dirname );
   #if defined(__WATCOMC__)
      dirname[ iDirnameLen ] = '\0';
   #endif

   if( NULL == dir )
   {
/*   debug code
      printf( "\n invalid dirname %s ", dirname );
      while( 0 == getchar() );
*/
      hb_itemReturn( pDir );
      hb_itemRelease( pDir );
      return;
   }

   /* now put everything into an array */
   while( ( entry = readdir( dir ) ) != NULL )
   {
      strcpy( string, entry->d_name );

#endif
      pos = strrchr( string, OS_PATH_DELIMITER );
      if( pos )
         pos = strrchr( pos + 1, '.' );
      else
         pos = strrchr( string, '.' );

      if( pos && ! ( pos == &string[ 0 ] ) )
      {
         strcpy( fext, pos + 1 );
         *pos = '\0';
      }
      else
         fext[ 0 ] = '\0';

      pos = strrchr( string, OS_PATH_DELIMITER );
      if( pos )
         strcpy( fname, pos + 1 );
      else
         strcpy( fname, string );

      if( !*fname )
         strcpy( fname, "*" );

/*   debug code
      printf( "\n fname: %s fext: %s ", fname, fext );
      while( 0 == getchar() );
*/
      if( hb_strMatchRegExp( fname, pfname ) && hb_strMatchRegExp( fext, pfext ) )
      {
         attrib = 0;

#if defined(_MSC_VER) || defined(__MINGW32__)
         /* due to short-name support: reconstruct the filename */
         if( bEightDotThree )
         {
            pos = strrchr( string, OS_PATH_DELIMITER );
            if( pos )
            {
               ++pos;
               if( ! *pos || ( *pos == '.' && ! pos[ 1 ] ) )
                  strcat( string, "." );
            }
            strcpy( filename, string );     /* entry.name ); */

            if( *fext )
            {
               strcat( filename, "." );
               strcat( filename, fext );
            }
            *fullfile = '\0';
         }
         else
         {
            strcpy( fullfile, dirname );
            strcpy( filename, entry.name );
         }

#elif defined(__IBMCPP__)
         strcpy( filename, entry.achName );
         strcpy( fullfile, dirname );
#else
         strcpy( filename, entry->d_name );
         strcpy( fullfile, dirname );
#endif
         strcat( fullfile, filename );


         if( -1 != stat( fullfile, &statbuf ) )
         {
            fsize = statbuf.st_size;
            ftime = statbuf.st_mtime;

            #if defined(OS_UNIX_COMPATIBLE)
               /* GNU C on Linux or on other UNIX */
               attrib = osToHarbourMask( statbuf.st_mode );
            #else
               #if defined(__IBMCPP__)
                  attrib = entry.attrFile;
               #else
                  #if defined(_MSC_VER) || defined(__MINGW32__)
                     attrib = entry.attrib;
                     if( bEightDotThree )
                     {
                         /* need to strip off the path */
                         pos = strrchr( filename, OS_PATH_DELIMITER );
                         if( pos )
                            strcpy( filename, ++pos );
                     }
                  #elif defined(__BORLANDC__) || defined(__DJGPP__)
                     attrib = _chmod( fullfile, 0 );
                  #else
                     attrib = 0;
                  #endif
               #endif
               attrib = osToHarbourMask( attrib );
               if( attrib & FA_DIREC )
               {
                  /* MS says size for a Directory is undefined.
                     Novell uses these bits for other purposes
                   */
                  fsize = 0;
               }
            #endif

            ft = localtime( &ftime );
            sprintf( ddate, "%04d%02d%02d",
               ft->tm_year + 1900, ft->tm_mon + 1, ft->tm_mday );

            sprintf( ttime, "%02d:%02d:%02d",
               ft->tm_hour, ft->tm_min, ft->tm_sec );

/* debug code
            printf( "\n name date time    %s %s %s ", filename, ddate, ttime );
            while( 0 == getchar() );
*/

         }
/* debug code
         else
         {
            printf( "\n invalid file %s ", fullfile );
            while( 0 == getchar() );
         }
*/

         if( !( ( ( ushbMask & FA_HIDDEN ) == 0 && ( attrib & FA_HIDDEN ) > 0 ) ||
                ( ( ushbMask & FA_SYSTEM ) == 0 && ( attrib & FA_SYSTEM ) > 0 ) ||
                ( ( ushbMask & FA_DIREC  ) == 0 && ( attrib & FA_DIREC  ) > 0 ) ) )
         {
            PHB_ITEM  pSubarray = hb_itemArrayNew( F_LEN );

            PHB_ITEM  pFilename = hb_itemPutC( NULL, filename );
            PHB_ITEM  pSize = hb_itemPutNL( NULL, fsize );
            PHB_ITEM  pDate = hb_itemPutDS( NULL, ddate );
            PHB_ITEM  pTime = hb_itemPutC( NULL, ttime );
            PHB_ITEM  pAttr = hb_itemPutC( NULL, ( char * ) HarbourMaskToAttributes( attrib, ( BYTE * ) aatrib ) );

            hb_itemArrayPut( pSubarray, F_NAME, pFilename );
            hb_itemArrayPut( pSubarray, F_SIZE, pSize );
            hb_itemArrayPut( pSubarray, F_DATE, pDate );
            hb_itemArrayPut( pSubarray, F_TIME, pTime );
            hb_itemArrayPut( pSubarray, F_ATTR, pAttr );

            /* NOTE: Simply ignores the situation where the array length
                     limit is reached. */
            hb_arrayAdd( pDir, pSubarray );

            hb_itemRelease( pFilename );
            hb_itemRelease( pSize );
            hb_itemRelease( pDate );
            hb_itemRelease( pTime );
            hb_itemRelease( pAttr );
            hb_itemRelease( pSubarray );
         }
      }
   }
#if defined(_MSC_VER) || defined(__MINGW32__)
   while( _findnext( hFile, &entry ) == 0 );
   _findclose( hFile );
#elif defined(__IBMCPP__)
   while( DosFindNext( hFind, &entry, findSize, &findCount ) == NO_ERROR && findCount > 0 );
   DosFindClose( hFind );
#else
   closedir( dir );
#endif

#if defined(_MSC_VER) || defined(__IBMCPP__) || defined(__MINGW32__)
   }
#endif
/*   debug code
      printf( "\n normal return" );
      while( 0 == getchar() );
*/
   hb_itemReturn( pDir ); /* DIRECTORY() returns an array */
   hb_itemRelease( pDir );

#endif /* HAVE_POSIX_IO */
}

