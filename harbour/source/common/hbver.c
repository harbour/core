/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Version detection functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    hb_verPlatform() (support for determining the windows version)
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_verPlatform() (support for determining many windows flavours)
 *    hb_verCompiler() (support for determining some compiler version/revision)
 *
 * Copyright 2000-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_verPlatform() (support for detecting Windows NT on DOS)
 *    hb_verPlatform() (rearrangment and cleanup)
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSMISC

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbver.h"

#if defined(HB_OS_WIN_32)

   #include <ctype.h>
   #ifndef VER_PLATFORM_WIN32_WINDOWS
      #define VER_PLATFORM_WIN32_WINDOWS 1
   #endif
   #ifndef VER_PLATFORM_WIN32_CE
      #define VER_PLATFORM_WIN32_CE 3
   #endif

#elif defined(HB_OS_UNIX)

   #include <sys/utsname.h>

#endif

/* NOTE: OS() function, as a primary goal will detect the version number
         of the target platform. As an extra it may also detect the host OS.
         The latter is mainly an issue in DOS, where the host OS can be OS/2
         WinNT/2K, Win3x, Win9x, DOSEMU, Desqview, etc. [vszakats] */

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * hb_verPlatform( void )
{
   char * pszPlatform;

   HB_TRACE(HB_TR_DEBUG, ("hb_verPlatform()"));

   /* NOTE: Must be larger than 128, which is the maximum size of
            osVer.szCSDVersion (Win32). [vszakats] */
   pszPlatform = ( char * ) hb_xgrab( 256 );

#if defined(HB_OS_DOS)

   {
      union REGS regs;

      regs.h.ah = 0x30;
      HB_DOS_INT86( 0x21, &regs, &regs );

      sprintf( pszPlatform, "DOS %d.%02d", regs.h.al, regs.h.ah );

      /* Host OS detection: Windows 2.x, 3.x, 95/98 */

      {
         regs.HB_XREGS.ax = 0x1600;
         HB_DOS_INT86( 0x2F, &regs, &regs );

         if( regs.h.al != 0x00 && regs.h.al != 0x80 )
         {
            char szHost[ 128 ];

            if( regs.h.al == 0x01 || regs.h.al == 0xFF )
               sprintf( szHost, " (Windows 2.x)" );
            else
               sprintf( szHost, " (Windows %d.%02d)", regs.h.al, regs.h.ah );

            strcat( pszPlatform, szHost );
         }
      }

      /* Host OS detection: Windows NT/2000 */

      {
         regs.HB_XREGS.ax = 0x3306;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.HB_XREGS.bx == 0x3205 )
            strcat( pszPlatform, " (Windows NT/2000)" );
      }

      /* Host OS detection: OS/2 */

      {
         regs.h.ah = 0x30;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.h.al >= 10 )
         {
            char szHost[ 128 ];

            if( regs.h.al == 20 && regs.h.ah > 20 )
               sprintf( szHost, " (OS/2 %d.%02d)", regs.h.ah / 10, regs.h.ah % 10 );
            else
               sprintf( szHost, " (OS/2 %d.%02d)", regs.h.al / 10, regs.h.ah );

            strcat( pszPlatform, szHost );
         }
      }
   }

#elif defined(HB_OS_OS2)

   {
      unsigned long aulQSV[ QSV_MAX ] = { 0 };
      APIRET rc;

      rc = DosQuerySysInfo( 1L, QSV_MAX, ( void * ) aulQSV, sizeof( ULONG ) * QSV_MAX );

      if( rc == 0 )
      {
         /* is this OS/2 2.x ? */
         if( aulQSV[ QSV_VERSION_MINOR - 1 ] < 30 )
         {
            sprintf( pszPlatform, "OS/2 %ld.%02ld",
               aulQSV[ QSV_VERSION_MAJOR - 1 ] / 10,
               aulQSV[ QSV_VERSION_MINOR - 1 ] );
         }
         else
            sprintf( pszPlatform, "OS/2 %2.2f",
               ( float ) aulQSV[ QSV_VERSION_MINOR - 1 ] / 10 );
      }
      else
         sprintf( pszPlatform, "OS/2" );
   }

#elif defined(HB_OS_WIN_32)

   {
      OSVERSIONINFO osVer;

      osVer.dwOSVersionInfoSize = sizeof( osVer );

      if( GetVersionEx( &osVer ) )
      {
         char * pszName = "Windows";

         switch( osVer.dwPlatformId )
         {
            case VER_PLATFORM_WIN32_WINDOWS:

               if( osVer.dwMajorVersion == 4 && osVer.dwMinorVersion < 10 )
                  pszName = "Windows 95";
               else if( osVer.dwMajorVersion == 4 && osVer.dwMinorVersion == 10 )
                  pszName = "Windows 98";

               break;

            case VER_PLATFORM_WIN32_NT:

               if( osVer.dwMajorVersion == 5 )
                  pszName = "Windows 2000";
               else
                  pszName = "Windows NT";

               break;

            case VER_PLATFORM_WIN32s:
               pszName = "Windows 32s";
               break;

            case VER_PLATFORM_WIN32_CE:
               pszName = "Windows CE";
               break;
         }

         sprintf( pszPlatform, "%s %lu.%02lu.%04d",
            pszName,
            ( ULONG ) osVer.dwMajorVersion,
            ( ULONG ) osVer.dwMinorVersion,
            ( USHORT ) LOWORD( osVer.dwBuildNumber ) );

         /* Add service pack/other info */

         if( osVer.szCSDVersion )
         {
            int i;

            /* Skip the leading spaces (Win95B, Win98) */
            for( i = 0; osVer.szCSDVersion[ i ] != '\0' && isspace( ( int ) osVer.szCSDVersion[ i ] ); i++ );

            if( osVer.szCSDVersion[ i ] != '\0' )
            {
               strcat( pszPlatform, " " );
               strcat( pszPlatform, osVer.szCSDVersion + i );
            }
         }
      }
      else
         sprintf( pszPlatform, "Windows" );
   }

#elif defined(HB_OS_UNIX)

   {
      struct utsname un;

      uname( &un );

      sprintf( pszPlatform, "%s %s", un.sysname, un.release );
   }

#elif defined(HB_OS_MAC)

   {
      strcpy( pszPlatform, "MacOS compatible" );
   }

#else

   {
      strcpy( pszPlatform, "(unknown)" );
   }

#endif

   return pszPlatform;
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * hb_verCompiler( void )
{
   char * pszCompiler;
   char * pszName;
   int iVerMajor;
   int iVerMinor;

   HB_TRACE(HB_TR_DEBUG, ("hb_verCompiler()"));

   pszCompiler = ( char * ) hb_xgrab( 80 );

#if defined(__IBMC__) || defined(__IBMCPP__)

   #if defined(__IBMC__)
      iVerMajor = __IBMC__;
   #else
      iVerMajor = __IBMCPP__;
   #endif

   if( iVerMajor >= 300 )
      pszName = "IBM Visual Age C++";
   else
      pszName = "IBM C++";

   iVerMajor /= 100;
   iVerMinor = iVerMajor % 100;

#elif defined(_MSC_VER)

   #if (_MSC_VER >= 800)
      pszName = "Microsoft Visual C/C++";
   #else
      pszName = "Microsoft C/C++";
   #endif

   iVerMajor = _MSC_VER / 100;
   iVerMinor = _MSC_VER % 100;

#elif defined(__BORLANDC__)

   pszName = "Borland C++";
   #if (__BORLANDC__ == 1040) /* Version 3.1 */
      iVerMajor = 3;
      iVerMinor = 1;
   #elif (__BORLANDC__ >= 1280) /* Version 5.x */
      iVerMajor = __BORLANDC__ >> 8;
      iVerMinor = ( __BORLANDC__ & 0xFF ) >> 4;
   #else /* Version 4.x */
      iVerMajor = __BORLANDC__ >> 8;
      iVerMinor = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
   #endif

#elif defined(__TURBOC__)

   pszName = "Borland Turbo C";
   iVerMajor = __TURBOC__ >> 8;
   iVerMinor = __TURBOC__ & 0xFF;

#elif defined(__MPW__)

   pszName = "MPW C";
   iVerMajor = __MPW__ / 100;
   iVerMinor = __MPW__ % 100;

#elif defined(__WATCOMC__)

   pszName = "Watcom C/C++";
   iVerMajor = __WATCOMC__ / 100;
   iVerMinor = __WATCOMC__ % 100;

#elif defined(__GNUC__)

   #if defined(__DJGPP__)
      pszName = "Delorie GNU C";
   #elif defined(__CYGWIN__)
      pszName = "Cygnus Cygwin GNU C";
   #elif defined(__MINGW32__)
      pszName = "Cygnus MinGW GNU C";
   #elif defined(__RSX32__)
      pszName = "EMX/RSXNT/DOS GNU C";
   #elif defined(__RSXNT__)
      pszName = "EMX/RSXNT/Win32 GNU C";
   #elif defined(__EMX__)
      pszName = "EMX GNU C";
   #else
      pszName = "GNU C";
   #endif

   iVerMajor = __GNUC__;
   iVerMinor = __GNUC_MINOR__;

#else

   pszName = ( char * ) NULL;
   iVerMajor = 0;
   iVerMinor = 0;

#endif

   if( pszName )
      sprintf( pszCompiler, "%s %hd.%hd", pszName, iVerMajor, iVerMinor );
   else
      strcpy( pszCompiler, "(unknown)" );

#if defined(__DJGPP__)

   {
      char szSub[ 32 ];
      sprintf( szSub, " (DJGPP %i.%02i)", ( int ) __DJGPP__, ( int ) __DJGPP_MINOR__ );
      strcat( pszCompiler, szSub );
   }

#elif defined(__BORLANDC__)

   {
      char szSub[ 32 ];
      /* QUESTION: Is there any better, safer, more official way to detect
                   the bit depth of the C compiler ? [vszakats] */
      sprintf( szSub, " (%i bit)", ( int ) ( sizeof( int ) * 8 ) );
      strcat( pszCompiler, szSub );
   }

#endif

   return pszCompiler;
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * hb_verHarbour( void )
{
   char * pszVersion;

   HB_TRACE(HB_TR_DEBUG, ("hb_verHarbour()"));

   pszVersion = ( char * ) hb_xgrab( 80 );

   sprintf( pszVersion, "Harbour %d.%d%s Intl. (Build %d) (%04d.%02d.%02d) (%s)",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY, HB_VER_LEX );

   return pszVersion;
}

void hb_verBuildInfo( void )
{
   hb_conOutErr( "Harbour Build Info", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
   hb_conOutErr( "---------------------------", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   {
      char * pszVersion = hb_verHarbour();
      hb_conOutErr( "Version: ", 0 );
      hb_conOutErr( pszVersion, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verCompiler();
      hb_conOutErr( "Compiler: ", 0 );
      hb_conOutErr( pszVersion, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verPlatform();
      hb_conOutErr( "Platform: ", 0 );
      hb_conOutErr( pszVersion, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Harbour extensions: ", 0 );
#if defined( HB_EXTENSION )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Clipper 5.2e undocumented: ", 0 );
#if defined( HB_C52_UNDOC )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Clipper 5.2e strict compatibility: ", 0 );
#if defined( HB_C52_STRICT )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Clipper 5.3x compatible extensions: ", 0 );
#if defined( HB_COMPAT_C53 )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Alaska Xbase++ compatible extensions: ", 0 );
#if defined( HB_COMPAT_XPP )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Visual Objects compatible extensions: ", 0 );
#if defined( HB_COMPAT_VO )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Multisoft Flagship compatible extensions: ", 0 );
#if defined( HB_FLAGSHIP_VO )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Microsoft FoxPro compatible extensions: ", 0 );
#if defined( HB_FOXPRO_VO )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "dBase compatible extensions: ", 0 );
#if defined( HB_DBASE_VO )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Object file generation support: ", 0 );
#if defined( HARBOUR_OBJ_GENERATION )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "ANSI C usage: ", 0 );
#if defined( HARBOUR_STRICT_ANSI_C )
   hb_conOutErr( "Strict", 0 );
#else
   hb_conOutErr( "Non strict", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "C++ mode: ", 0 );
#if defined(__cplusplus)
   hb_conOutErr( "On", 0 );
#else
   hb_conOutErr( "Off", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Compiler YACC debug mode: ", 0 );
#if defined( HARBOUR_YYDEBUG )
   hb_conOutErr( "On", 0 );
#else
   hb_conOutErr( "Off", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   {
      char buffer[ 64 ];
      sprintf( buffer, "Maximum symbol name length: %i", HB_SYMBOL_NAME_LEN );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   hb_conOutErr( "---------------------------", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
}

