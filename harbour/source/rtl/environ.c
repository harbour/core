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
 *    Support for determining the window version by
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    Support for determining many windows flavours by 
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

#if defined(_WINDOWS_) || defined(WINNT)
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

#if defined(__GNUC__) && !defined(__DJGPP__) && ! defined(__MINGW32__)
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

HARBOUR HB_OS( void )
{
   char * cformat = "%s %d.%02d%c";

#if defined(__MPW__)
/* TODO: not implemented yet */
   hb_retc( "MacOS" );
#else
   int hb_osmajor = -1, hb_osminor = -1, hb_osletter = -1;
   char * hb_os = NULL;
   char version[ 128 ];
#if defined(__IBMCPP__)

   unsigned long aulQSV[ QSV_MAX ] = { 0 };
   APIRET rc = DosQuerySysInfo( 1L, QSV_MAX, ( void * ) aulQSV, sizeof( ULONG ) * QSV_MAX );
   if( ! rc )
   {
      hb_osmajor  = aulQSV[ QSV_VERSION_MAJOR ] / 10;
      hb_osminor  = aulQSV[ QSV_VERSION_MINOR ];
      hb_osletter = ( aulQSV[ QSV_VERSION_REVISION ] > 0 && aulQSV[ QSV_VERSION_REVISION ] < 26 ) ? '@' + aulQSV[ QSV_VERSION_REVISION ] : 0;
   }
   hb_os = "OS/2";

#else

#if defined(__GNUC__) && !defined(__DJGPP__) && !defined(__MINGW32__)

   struct utsname un;

   uname( &un );

   #if defined(HARBOUR_GCC_OS2)
      sprintf( version, "%s %s", un.sysname, un.version );
   #else
      sprintf( version, "%s %s", un.sysname, un.release );
   #endif

   hb_os      = "";
   hb_osmajor = -2;

#else

/* TODO: add MSVC support but MSVC cannot detect any OS except Windows! */
#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(_MSC_VER) || defined(__MINGW32__)

#if defined(_WINDOWS_) || defined(__MINGW32__)

   OSVERSIONINFO osVer; /* for GetVersionEx() */
   char szBuild[ 128 ] = "";

   cformat = "%s%s %d.%02d.%04d";
   osVer.dwOSVersionInfoSize = sizeof( osVer );

   if( GetVersionEx( &osVer ) )
   {
      switch( osVer.dwPlatformId )
      {
         case VER_PLATFORM_WIN32_WINDOWS:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = LOWORD( osVer.dwBuildNumber );

            if( hb_osmajor == 4 && hb_osminor == 0 && hb_osletter == 950 )
               hb_os = "Windows 95";
            else if( hb_osmajor == 4 && hb_osminor > 0 &&
                  hb_osletter > 950 && hb_osletter <= 1080 )
               hb_os = "Windows 95 SP1";
            else if( hb_osmajor == 4 && hb_osminor < 10 &&
                  hb_osletter > 1080 )
               hb_os = "Windows 95 OSR2"; /* Formerly: "Windows 95 SP2" */
            else if( hb_osmajor == 4 && hb_osminor == 10 &&
                  hb_osletter == 1998 )
               hb_os = "Windows 98";
            else if( hb_osmajor == 4 && hb_osminor == 10 &&
                 hb_osletter > 1998 && hb_osletter < 2183 )
               hb_os = "Windows 98 SP1";
            else if( hb_osmajor > 4 && hb_osletter >= 2183 )
               hb_os = "Windows 98 SE";
            else
               hb_os = "Windows";

            strncpy( szBuild, osVer.szCSDVersion, sizeof( szBuild ) );
            szBuild[ sizeof( szBuild ) - 1 ] = '\0';

            break;

         case VER_PLATFORM_WIN32_NT:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = LOWORD( osVer.dwBuildNumber );

            if( hb_osmajor == 3 && hb_osminor == 10 )
               hb_os = "Windows NT 3.1";
            else if( hb_osmajor == 3 && hb_osminor == 50 )
               hb_os = "Windows NT 3.5";
            else if( hb_osmajor == 3 && hb_osminor == 51 )
               hb_os = "Windows NT 3.51";
            else if( hb_osmajor == 4 )
               hb_os = "Windows NT 4";
            else if( hb_osmajor == 5 )
               hb_os = "Windows 2000";
            else
               hb_os = "Windows NT";

            if( osVer.szCSDVersion )
            {
               int i = 0;
               WORD wServicePack;

               while( osVer.szCSDVersion[ i ] != '\0' && ( osVer.szCSDVersion[ i ] < '0' || osVer.szCSDVersion[ i ] > '9' ) )
                  i++;
               wServicePack = ( WORD )( atoi( &osVer.szCSDVersion[ i ] ) );

               if( wServicePack )
                  sprintf( szBuild, " SP%i", wServicePack );
            }

            /* TODO: Add support for:
                      * NT Stand Alone Server
                      * NT Enterprise Edition
                      * NT Terminal Server
                      * NT Primary Domain Controller
                      * NT Backup Domain Controller

               It can be done with:
                RegOpenKey( "System\CurrentControlSet\Control\ProductOptions", ... )
                RegQueryValueEx( "ProductType", ..., szBuffer )
            */
            break;

         case VER_PLATFORM_WIN32s:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = LOWORD( osVer.dwBuildNumber );
            hb_os = "Windows 32s";
            break;

         case VER_PLATFORM_WIN32_CE:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = LOWORD( osVer.dwBuildNumber );
            hb_os = "Windows CE";
            break;
      }
   }
#else
#if defined(_MSC_VER)
      if( _osmode == _WIN_MODE )
      {
         hb_os = "Windows";
         hb_osmajor = _osmajor;
         hb_osminor = _osminor
         hb_osletter = 0;
      }
#else
      /* detect Windows */
      _AX = 0x160A;
      geninterrupt( 0x2F );
      if( _AX == 0 )
      {
         hb_osmajor = _BX / 256;
         hb_osminor = _BX % 256;
         hb_osletter = 0;
         hb_os = "Windows";
      }
#endif /* _MSC_VER */
      else
      {
         hb_os = "DOS";
         hb_osmajor = _osmajor;
         hb_osminor = _osminor;
         hb_osletter = 0;
      }

#endif /* defined(_WINDOWS_) */

#else

   union REGS regs;

   /* detect OS/2 */
   regs.h.ah = 0x30;

   INT_86( 0x21, &regs, &regs );

   if( regs.h.al >= 10 )
   {
      hb_os = "OS/2";
      if( regs.h.al == 20 && regs.h.ah > 20 )
      {
         hb_osmajor = regs.h.ah / 10;
         hb_osminor = regs.h.ah % 10;
      }
      else
      {
         hb_osmajor = regs.h.al / 10;
         hb_osminor = regs.h.ah;
      }
      hb_osletter = 0;
   }
   else
   {
      hb_osmajor = _osmajor;
      hb_osminor = _osminor;
      regs.w.ax = 0x160A;

      INT_86( 0x2F, &regs, &regs );

      if( regs.w.ax == 0 )
      {
         hb_os = "Windows";
         hb_osmajor = regs.w.bx / 256;
         hb_osminor = regs.w.bx % 256;
         hb_osletter = 0;
      }
      else
      {
         hb_os = "DOS";
         hb_osletter = 0;
      }
   }
#endif /* __TURBOC__ or __BORLANDC__ or _MSC_VER 0r __DJGPP__ */
#if defined(__DJGPP__)
   hb_os = hb_xgrab( strlen( _os_flavor ) + 1 );
   strcpy( hb_os, _os_flavor );
   hb_osmajor = _osmajor;
   hb_osminor = _osminor;
   hb_osletter = 0;
#endif

   /* TODO: detect other OSes */

#endif /* __GNUC__ */
#endif /* __IBMCPP__ */

   if( ! hb_os ) strcpy( version, "Unknown" );
   else if( hb_osmajor == -1 ) strcpy( version, hb_os );
   else if( hb_osmajor == -2 ) { /* NOP */ }
#if defined(_WINDOWS_) || defined(__MINGW32__)
   else sprintf( version, cformat, hb_os, szBuild, hb_osmajor, hb_osminor, hb_osletter );
#else
   else sprintf( version, cformat, hb_os, hb_osmajor, hb_osminor, hb_osletter );
#endif
   hb_retc( version );
#if defined(__DJGPP__)
   hb_xfree( hb_os );
#endif

#endif /* __MPW__ */
}

/* The caller must free the returned buffer. */

#define HB_VERSION_BUFFER_LEN 80

/*  Support for determining some compiler version/revision by Jose Lalin
    dezac@corevia.com
*/

char * hb_version( USHORT uiMode )
{
   char * pszVersion;

/*   HB_TRACE(("hb_version(%hu)", uiMode));  */

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
         revision = ( __BORLANDC__ & 0xff ) >> 4;
      #else
         /* Version 4.x */
         version = __BORLANDC__ >> 8;
         revision = ( __BORLANDC__ - 1 & 0xff) >> 4;
      #endif

   #elif defined(__TURBOC__)

      compiler = "Borland Turbo C";
      version = __TURBOC__ >> 8;
      revision = __TURBOC__ & 0xff;

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

HARBOUR HB_VERSION( void )
{
   char * pszVersion = hb_version( hb_pcount() > 0 ? 1 : 0 );
   hb_retc( pszVersion );
   hb_xfree( pszVersion );
}

