/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OS(), VERSION() functions
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    Support for determining the windows version
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    Support for determining many windows flavours
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: The following #ifdef block for __IBMCPP__ must
         be ahead of any and all #include statements!
*/

#if defined(__IBMCPP__)
   #define INCL_DOSMISC
#endif

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapierr.h"
#include "hbver.h"

#if defined(HB_OS_WIN_32)
   #if ! defined(VER_PLATFORM_WIN32_WINDOWS)
      #define VER_PLATFORM_WIN32_WINDOWS 1
   #endif
   #if ! defined(VER_PLATFORM_WIN32_CE)
      #define VER_PLATFORM_WIN32_CE 3
   #endif
#endif

#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(_MSC_VER) || defined(__DJGPP__) || defined(__MINGW32__)
   #include <dos.h>
   #include <stdlib.h>
#endif

#if defined(HB_OS_UNIX)
   #include <sys/utsname.h>
#endif

#if defined(__WATCOMC__)
   #include <i86.h>
   #if defined(__386__) && !defined(__WINDOWS_386__)
      #define INT_86 int386
   #else
      #define INT_86 int86
   #endif
#else
   #if defined(__EMX__)
      #define INT_86 _int86
   #else
      #define INT_86 int86
   #endif
#endif

/* NOTE: OS() function, as a primary goal will detect the version number
         of the target platform. As an extra it may also detect the host OS.
         The latter is mainly an issue in DOS, where the host OS can be OS/2
         WinNT/2K, Win3x, Win9x, DOSEMU, Desqview, etc. [vszakats] */

#define HB_OS_BUFFER_LEN 128

char * hb_os( void )
{
   char * pszOS;

   HB_TRACE(HB_TR_DEBUG, ("hb_os(%hu)", uiMode));

   pszOS = ( char * ) hb_xgrab( HB_OS_BUFFER_LEN );

#if defined(HB_OS_DOS)

   {
      union REGS regs;

      regs.h.ah = 0x30;
      INT_86( 0x21, &regs, &regs );

      sprintf( pszOS, "DOS %d.%02d", regs.h.al, regs.h.ah );

      /* Host OS detection: Windows 2.x, 3.x, 95/98 */

      {
         #if defined(__BORLANDC__) || defined(_MSC_VER)
            regs.x.ax = 0x1600;
         #else
            regs.w.ax = 0x1600;
         #endif
         INT_86( 0x2F, &regs, &regs );

         if( regs.h.al != 0x00 && regs.h.al != 0x80 )
         {
            char szHost[ 128 ] = "";

            if( regs.h.al == 0x01 || regs.h.al == 0xFF )
               sprintf( szHost, " (Windows 2.x)" );
            else
               sprintf( szHost, " (Windows %d.%02d)", regs.h.al, regs.h.ah );

            strcat( pszOS, szHost );
         }
      }

      /* Host OS detection: Windows NT/2000 */

      {
         #if defined(__BORLANDC__) || defined(_MSC_VER)
            regs.x.ax = 0x3306;
         #else
            regs.w.ax = 0x3306;
         #endif
         INT_86( 0x21, &regs, &regs );

         #if defined(__BORLANDC__) || defined(_MSC_VER)
            if( regs.x.bx == 0x3205 )
         #else
            if( regs.w.bx == 0x3205 )
         #endif
               strcat( pszOS, " (Windows NT/2000)" );
      }

      /* Host OS detection: OS/2 */

      {
         regs.h.ah = 0x30;
         INT_86( 0x21, &regs, &regs );

         if( regs.h.al >= 10 )
         {
            char szHost[ 128 ] = "";

            if( regs.h.al == 20 && regs.h.ah > 20 )
               sprintf( szHost, " (OS/2 %d.%02d)", regs.h.ah / 10, regs.h.ah % 10 );
            else
               sprintf( szHost, " (OS/2 %d.%02d)", regs.h.al / 10, regs.h.ah );

            strcat( pszOS, szHost );
         }
      }
   }

#elif defined(HB_OS_OS2)

   {
      unsigned long aulQSV[ QSV_MAX ] = { 0 };
      APIRET rc = DosQuerySysInfo( 1L, QSV_MAX, ( void * ) aulQSV, sizeof( ULONG ) * QSV_MAX );

      if( rc == 0 )
         sprintf( pszOS, "OS/2 %d.%02d%c",
            aulQSV[ QSV_VERSION_MAJOR ] / 10,
            aulQSV[ QSV_VERSION_MINOR ],
            ( aulQSV[ QSV_VERSION_REVISION ] > 0 && aulQSV[ QSV_VERSION_REVISION ] < 26 ) ? '@' + aulQSV[ QSV_VERSION_REVISION ] : 0 );
      else
         sprintf( pszOS, "OS/2" );
   }

#elif defined(HB_OS_WIN_32)

   {
      OSVERSIONINFO osVer;

      char * pszName;
      char szBuild[ 128 ] = "";
      int iVerMajor;
      int iVerMinor;
      int iVerLetter;

      osVer.dwOSVersionInfoSize = sizeof( osVer );

      if( GetVersionEx( &osVer ) )
      {
         iVerMajor = osVer.dwMajorVersion;
         iVerMinor = osVer.dwMinorVersion;
         iVerLetter = LOWORD( osVer.dwBuildNumber );

         switch( osVer.dwPlatformId )
         {
            case VER_PLATFORM_WIN32_WINDOWS:

               if( iVerMajor == 4 && iVerMinor == 0 && iVerLetter == 950 )
                  pszName = "Windows 95";
               else if( iVerMajor == 4 && iVerMinor > 0 &&
                     iVerLetter > 950 && iVerLetter <= 1080 )
                  pszName = "Windows 95 SP1";
               else if( iVerMajor == 4 && iVerMinor < 10 &&
                     iVerLetter > 1080 )
                  pszName = "Windows 95 OSR2"; /* Formerly: "Windows 95 SP2" */
               else if( iVerMajor == 4 && iVerMinor == 10 &&
                     iVerLetter == 1998 )
                  pszName = "Windows 98";
               else if( iVerMajor == 4 && iVerMinor == 10 &&
                    iVerLetter > 1998 && iVerLetter < 2183 )
                  pszName = "Windows 98 SP1";
               else if( iVerMajor > 4 && iVerLetter >= 2183 )
                  pszName = "Windows 98 SE";
               else
                  pszName = "Windows";

               strncpy( szBuild, osVer.szCSDVersion, sizeof( szBuild ) );
               szBuild[ sizeof( szBuild ) - 1 ] = '\0';

               break;

            case VER_PLATFORM_WIN32_NT:

               if( iVerMajor == 3 && iVerMinor == 10 )
                  pszName = "Windows NT 3.1";
               else if( iVerMajor == 3 && iVerMinor == 50 )
                  pszName = "Windows NT 3.5";
               else if( iVerMajor == 3 && iVerMinor == 51 )
                  pszName = "Windows NT 3.51";
               else if( iVerMajor == 4 )
                  pszName = "Windows NT 4";
               else if( iVerMajor == 5 )
                  pszName = "Windows 2000";
               else
                  pszName = "Windows NT";

               if( osVer.szCSDVersion )
               {
                  int i = 0;
                  int iServicePack;

                  while( osVer.szCSDVersion[ i ] != '\0' && ( osVer.szCSDVersion[ i ] < '0' || osVer.szCSDVersion[ i ] > '9' ) )
                     i++;

                  iServicePack = atoi( &osVer.szCSDVersion[ i ] );

                  if( iServicePack )
                     sprintf( szBuild, " SP%i", iServicePack );
               }

               break;

            case VER_PLATFORM_WIN32s:
               pszName = "Windows 32s";
               break;

            case VER_PLATFORM_WIN32_CE:
               pszName = "Windows CE";
               break;

            default:
               pszName = "Windows";
               break;
         }
      }
      else
      {
         iVerMajor = osVer.dwMajorVersion;
         iVerMinor = osVer.dwMinorVersion;
         iVerLetter = LOWORD( osVer.dwBuildNumber );
         pszName = "Windows";
      }

      sprintf( pszOS, "%s%s %d.%02d.%04d", pszName, szBuild, iVerMajor, iVerMinor, iVerLetter );
   }

#elif defined(HB_OS_UNIX)

   {
      struct utsname un;

      uname( &un );

      sprintf( pszOS, "%s %s", un.sysname, un.release );
   }

#elif defined(HB_OS_MAC)

   {
      strcpy( pszOS, "MacOS compatible" );
   }

#else

   {
      strcpy( pszOS, "(unknown)" );
   }

#endif

   return pszOS;
}

/* The caller must free the returned buffer. */

#define HB_VERSION_BUFFER_LEN 80

/*  Support for determining some compiler version/revision by Jose Lalin
    dezac@corevia.com
*/

char * hb_version( USHORT uiMode )
{
   char * pszVersion;

   HB_TRACE(HB_TR_DEBUG, ("hb_version(%hu)", uiMode));

   pszVersion = ( char * ) hb_xgrab( HB_VERSION_BUFFER_LEN );

   sprintf( pszVersion, "Harbour %d.%d%s Intl. (Build %d)  (%04d.%02d.%02d)",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY );

   if( uiMode != 0 )
   {
      /* Optionally include the Compiler name and version, if available. */
      char * compiler;
      int version;
      int revision;

   #if defined(__IBMC__) || defined(__IBMCPP__)

      #if defined(__IBMC__)
         version = __IBMC__;
      #else
         version = __IBMCPP__;
      #endif

      if( version >= 300 )
          compiler = "IBM Visual Age C++";
      else
          compiler = "IBM C++";

      version /= 100;
      revision = version % 100;

   #elif defined(__BORLANDC__)

      compiler = "Borland C++";
      #if (__BORLANDC__ == 1040)
         /* Version 3.1 */
         version = 3;
         revision = 1;
      #elif (__BORLANDC__ >= 1280)
         /* Version 5.x */
         version = __BORLANDC__ >> 8;
         revision = ( __BORLANDC__ & 0xFF ) >> 4;
      #else
         /* Version 4.x */
         version = __BORLANDC__ >> 8;
         revision = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
      #endif

   #elif defined(__TURBOC__)

      compiler = "Borland Turbo C";
      version = __TURBOC__ >> 8;
      revision = __TURBOC__ & 0xFF;

   #elif defined(_MSC_VER)

      compiler = "Microsoft C/C++";
      version = _MSC_VER / 100;
      revision = _MSC_VER % 100;

   #elif defined(__MPW__)

      compiler = "MPW C";
      version = __MPW__ / 100;
      revision = __MPW__ % 100;

   #elif defined(__WATCOMC__)

      compiler = "Watcom C/C++";
      version = __WATCOMC__ / 100;
      revision = __WATCOMC__ % 100;

   #elif defined(__DJGPP__)

      compiler = "Delorie GCC";
      version = __GNUC__;
      revision = __GNUC_MINOR__;

   #elif defined(__CYGWIN__)

      compiler = "Cygnus GCC (Cygwin)";
      version = __GNUC__;
      revision = __GNUC_MINOR__;


   #elif defined(__MINGW32__)

      compiler = hb_xgrab( 80 );
      sprintf( compiler, "Cygnus GCC (Mingw32 %g)", __MINGW32__ );
      version = __GNUC__;
      revision = __GNUC_MINOR__;

   #elif defined(__GNUC__)

      #if defined(__EMX__)
         compiler = "GNU C/EMX C";
      #else
         compiler = "GNU C";
      #endif

      version = __GNUC__;
      revision = __GNUC_MINOR__;

   #else

      compiler = ( char * ) NULL;
      version = 0;
      revision = 0;

   #endif

      if( compiler )
      {
         strncat( pszVersion, " (", HB_VERSION_BUFFER_LEN );
         strncat( pszVersion, compiler, HB_VERSION_BUFFER_LEN );
         if( version )
         {
            char buf[ 40 ];
            sprintf( buf, "(%d.%d)", version, revision );
            strncat( pszVersion, " ", HB_VERSION_BUFFER_LEN );
            strncat( pszVersion, buf, HB_VERSION_BUFFER_LEN );
         }
         strncat( pszVersion, ")", HB_VERSION_BUFFER_LEN );
         pszVersion[ HB_VERSION_BUFFER_LEN - 1 ] = '\0';
      }
   #if defined(__MINGW32__)
      hb_xfree( compiler );
   #endif
   }

   return pszVersion;
}

HB_FUNC( OS )
{
   char * pszString = hb_os();
   hb_retc( pszString );
   hb_xfree( pszString );
}

HB_FUNC( VERSION )
{
   char * pszString = hb_version( hb_pcount() > 0 ? 1 : 0 );
   hb_retc( pszString );
   hb_xfree( pszString );
}

