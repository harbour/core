/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Environment functions (OS(), VERSION(), __RUN(), GETENV(), etc.)
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
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    HB___RUN()
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB_GETE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: The following #ifdef block for __IBMCPP__ must
         be ahead of any and all #include statements!
*/

#ifdef __IBMCPP__
   #define INCL_DOSMISC
#endif

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#if defined(__BORLANDC__) && defined(_Windows) && ! defined(VER_PLATFORM_WIN32_WINDOWS)
   #define VER_PLATFORM_WIN32_WINDOWS 1
#endif

#include "extend.h"
#include "errorapi.h"
#include "hbver.h"

#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(_MSC_VER) || defined(__DJGPP__) || defined(__MINGW32__)
   #include <dos.h>
   #include <stdlib.h>
#endif

#if defined(__GNUC__) && !defined(__DJGPP__) && ! defined(__MINGW32__)
   #include <sys/utsname.h>
#endif

#ifdef __WATCOMC__
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
   char *cformat = "%s %d.%02d%c";

#ifdef __MPW__
/* TODO: not implemented yet */
   hb_retc( "MacOS" );
#else
   int hb_osmajor = -1, hb_osminor = -1, hb_osletter = -1;
   char * hb_os = NULL;
   char version[ 128 ];
#ifdef __IBMCPP__

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

#if defined(_Windows) || defined(WINNT)

/* NOTE: Support for determining the window version by Luiz Rafael Culik
   Culik@sl.conex.net
*/

   OSVERSIONINFO osVer; /* for GetVersionEx() */

   osVer.dwOSVersionInfoSize = sizeof( osVer );

   if( GetVersionEx( &osVer ) )
   {
      switch( osVer.dwPlatformId )
      {
         case VER_PLATFORM_WIN32_WINDOWS:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = osVer.dwBuildNumber;
            hb_os = "Windows 95/98";
            break;

         case VER_PLATFORM_WIN32_NT:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = osVer.dwBuildNumber;
            hb_os = "Windows NT";
            break;

         case VER_PLATFORM_WIN32s:
            hb_osmajor = osVer.dwMajorVersion;
            hb_osminor = osVer.dwMinorVersion;
            hb_osletter = osVer.dwBuildNumber;
            hb_os = "Windows 32s";
            break;
      }
      cformat = "%s %d.%02d.%04d";
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

#endif /* defined(_Windows) */

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
#ifdef __DJGPP__
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
   else sprintf( version, cformat, hb_os, hb_osmajor, hb_osminor, hb_osletter );
   hb_retc( version );
#ifdef __DJGPP__
   hb_xfree( hb_os );
#endif

#endif /* __MPW__ */
}

/* The caller must free the returned buffer. */

#define HB_VERSION_BUFFER_LEN 80


char * hb_version( USHORT uiMode )
{
   char * pszVersion = ( char * ) hb_xgrab( HB_VERSION_BUFFER_LEN );

   sprintf( pszVersion, "Harbour %d.%d Intl. (Build %d%s)  (%04d.%02d.%02d)",
      hb_major, hb_minor, hb_build, hb_revision, hb_year, hb_month, hb_day );

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

      #ifdef __EMX__
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

HARBOUR HB_GETENV( void )
{
   if( hb_pcount() == 1 )
   {
      char * szName = hb_parc( 1 );
      long lName = hb_parclen( 1 );

      while( lName && szName[ lName - 1 ] == '=' )
      {
         /* strip the '=' or else it will clear the variable! */
         szName[ lName - 1 ] = '\0';
         lName--;
      }
      if( lName )
      {
         char * Value = getenv( szName );
         hb_retc( Value ? Value : "" );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

/* NOTE: Undocumented Clipper function */

HARBOUR HB_GETE( void )
{
   HB_GETENV();
}

/*  $DOC$
 *  $FUNCNAME$
 *      __RUN
 *  $SYNTAX$
 *      __RUN( <cCommand> )
 *  $ARGUMENTS$
 *      <cCommand> Command to execute
 *  $DESCRIPTION$
 *      This command runs an external program. Please make sure that you have
 *      enough free memory to be able to run the external program.
 *      Do not use it to run Terminate and Stay Resident programs (in case of DOS)
 *      since it cause several problems
 *  $EXAMPLES$
 *      __Run( "edit " + cMyTextFile )    // Runs an external editor
 *      __Run( "command" )                // Gives a DOS shell (DOS only)
 *  $FILES$
 *      source/rtl/environ.c
 *      Run an external program
 *  $SEEALSO$
 *  $END$
 */

HARBOUR HB___RUN( void )
{
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__) || defined(_MSC_VER) || defined(__IBMCPP__) || defined(HARBOUR_GCC_OS2) || defined(__CYGWIN__) || defined(__MINGW32__)
   if( ISCHAR( 1 ) )
      system( hb_parc( 1 ) );
#else
   hb_errRT_BASE_Ext1( EG_UNSUPPORTED, 9999, NULL, "__RUN", 0, EF_CANDEFAULT );
#endif
}

