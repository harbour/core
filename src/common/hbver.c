/*
 * Version detection functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    hb_verPlatform() (support for determining the Windows version)
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_verPlatform() (support for determining many Windows flavours)
 *    hb_verCompiler() (support for determining some compiler version/revision)
 * Copyright 2000-2014 Viktor Szakats (vszakats.net/harbour)
 *    hb_verCPU(), hb_verHostBitWidth(), hb_iswinver(), hb_iswinsp()
 *    hb_verPlatform() (support for detecting Windows NT on DOS, Wine, post-Windows 8, cleanups)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbapi.h"
#include "hbmemory.ch"

#if defined( HB_OS_WIN )

   #include <windows.h>
   #include "hbwinuni.h"
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif

   #ifndef VER_PLATFORM_WIN32_WINDOWS
   #define VER_PLATFORM_WIN32_WINDOWS  1
   #endif
   #ifndef VER_PLATFORM_WIN32_CE
   #define VER_PLATFORM_WIN32_CE  3
   #endif

   #ifndef VER_NT_WORKSTATION
   #define VER_NT_WORKSTATION  0x0000001
   #endif
   #ifndef VER_NT_DOMAIN_CONTROLLER
   #define VER_NT_DOMAIN_CONTROLLER  0x0000002
   #endif
   #ifndef VER_NT_SERVER
   #define VER_NT_SERVER  0x0000003
   #endif

   #ifndef VER_MINORVERSION
   #define VER_MINORVERSION  0x0000001
   #endif
   #ifndef VER_MAJORVERSION
   #define VER_MAJORVERSION  0x0000002
   #endif
   #ifndef VER_BUILDNUMBER
   #define VER_BUILDNUMBER   0x0000004
   #endif
   #ifndef VER_SERVICEPACKMINOR
   #define VER_SERVICEPACKMINOR  0x0000010
   #endif
   #ifndef VER_SERVICEPACKMAJOR
   #define VER_SERVICEPACKMAJOR  0x0000020
   #endif

   #ifndef VER_PRODUCT_TYPE
   #define VER_PRODUCT_TYPE  0x0000080
   #endif
   #ifndef VER_EQUAL
   #define VER_EQUAL  1
   #endif
   #ifndef VER_GREATER_EQUAL
   #define VER_GREATER_EQUAL  3
   #endif

   #ifndef SM_SERVERR2
   #define SM_SERVERR2  89
   #endif

#elif defined( HB_OS_OS2 )
   #define INCL_DOSMISC
   #include <os2.h>
#elif defined( HB_OS_DOS )
   #include <dos.h>
#elif defined( HB_OS_UNIX ) && ! defined( __CEGCC__ )
   #include <sys/utsname.h>
#endif

const char * hb_verCPU( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_verCPU()" ) );

#if   defined( HB_CPU_X86 )
   return "x86";
#elif defined( HB_CPU_X86_64 )
   return "x86-64";
#elif defined( HB_CPU_IA_64 )
   return "IA-64";
#elif defined( HB_CPU_PPC )
   return "PPC";
#elif defined( HB_CPU_PPC_64 )
   return "PPC64";
#elif defined( HB_CPU_SPARC )
   return "SPARC";
#elif defined( HB_CPU_SPARC_64 )
   return "SPARC64";
#elif defined( HB_CPU_ARM )
   return "ARM";
#elif defined( HB_CPU_ARM_64 )
   return "ARM64";
#elif defined( HB_CPU_MIPS )
   return "MIPS";
#elif defined( HB_CPU_SH )
   return "SuperH";
#elif defined( HB_CPU_ZARCH )
   return "z/Architecture";
#elif defined( HB_CPU_PARISC )
   return "PA-RISC";
#elif defined( HB_CPU_ALPHA )
   return "Alpha";
#elif defined( HB_CPU_POWER )
   return "POWER";
#elif defined( HB_CPU_M68K )
   return "m68k";
#elif defined( HB_CPU_SYS370 )
   return "System/370";
#elif defined( HB_CPU_SYS390 )
   return "System/390";
#else
   return "(unrecognized)";
#endif
}

static HB_BOOL s_win_iswow64( void )
{
   HB_BOOL bRetVal = HB_FALSE;

   #if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_64 )
   {
      typedef BOOL ( WINAPI * P_ISWOW64PROCESS )( HANDLE, PBOOL );

      P_ISWOW64PROCESS pIsWow64Process;

      HMODULE hModule = GetModuleHandle( TEXT( "kernel32" ) );

      if( hModule )
         pIsWow64Process = ( P_ISWOW64PROCESS ) HB_WINAPI_GETPROCADDRESS( hModule, "IsWow64Process" );
      else
         pIsWow64Process = NULL;

      if( pIsWow64Process )
      {
         BOOL bIsWow64 = FALSE;

         if( ! pIsWow64Process( GetCurrentProcess(), &bIsWow64 ) )
         {
            /* Try alternative method? */
         }

         if( bIsWow64 )
            bRetVal = HB_TRUE;
      }
   }
   #endif

   return bRetVal;
}

const char * hb_verHostCPU( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_verHostCPU()" ) );

   if( s_win_iswow64() )
      return "x86-64";

   return hb_verCPU();
}

int hb_verHostBitWidth( void )
{
   int nBits;

   /* Inherit the bit width we're building for */
   #if   defined( HB_ARCH_64BIT )
      nBits = 64;
   #elif defined( HB_ARCH_32BIT )
      nBits = 32;
   #elif defined( HB_ARCH_16BIT )
      nBits = 16;
   #else
      nBits = 0;
   #endif

   if( s_win_iswow64() )
      nBits = 64;

   return nBits;
}

/* NOTE: OS() function, as a primary goal will detect the version number
         of the target platform. As an extra it may also detect the host OS.
         The latter is mainly an issue in DOS, where the host OS can be OS/2
         WinNT/2K, Win3x, Win9x, DOSEMU, Desqview, etc. [vszakats] */

/* NOTE: The caller must free the returned buffer. [vszakats] */

/* NOTE: The first word of the returned string must describe
         the OS family as used in __PLATFORM__*. Latter macro
         will in fact be formed from the string returned
         by this function. [vszakats] */

/* NOTE: As it appears in __PLATFORM__* macro */
const char * hb_verPlatformMacro( void )
{
#if   defined( HB_OS_WIN_CE ) /* Must precede HB_OS_WIN */
   return "WINCE";            /* TODO: Change this to WCE for consistency? */
#elif defined( HB_OS_WIN )
   return "WINDOWS";          /* TODO: Change this to WIN for consistency? */
#elif defined( HB_OS_DOS )
   return "DOS";
#elif defined( HB_OS_OS2 )
   return "OS2";
#elif defined( HB_OS_LINUX )
   return "LINUX";
#elif defined( HB_OS_DARWIN )
   return "DARWIN";
#elif defined( HB_OS_BSD )
   return "BSD";
#elif defined( HB_OS_SUNOS )
   return "SUNOS";
#elif defined( HB_OS_HPUX )
   return "HPUX";
#elif defined( HB_OS_BEOS )
   return "BEOS";
#elif defined( HB_OS_QNX )
   return "QNX";
#elif defined( HB_OS_VXWORKS )
   return "VXWORKS";
#elif defined( HB_OS_SYMBIAN )
   return "SYMBIAN";
#elif defined( HB_OS_CYGWIN )
   return "CYGWIN";
#else
   return NULL;
#endif
}

#if defined( HB_OS_WIN )

static HB_BOOL s_fWinVerInit = HB_FALSE;

static HB_BOOL s_fWin11    = HB_FALSE;
static HB_BOOL s_fWin10    = HB_FALSE;
static HB_BOOL s_fWin81    = HB_FALSE;
static HB_BOOL s_fWin8     = HB_FALSE;
static HB_BOOL s_fWin7     = HB_FALSE;
static HB_BOOL s_fWinVista = HB_FALSE;
static HB_BOOL s_fWin2K3   = HB_FALSE;
static HB_BOOL s_fWin2K    = HB_FALSE;
static int     s_iWinNT    = 0;
static int     s_iWin9x    = 0;
static int     s_iWine     = 0;

#if ! defined( HB_OS_WIN_CE )

#if ( defined( _MSC_VER ) && _MSC_VER < 1300 ) && ! defined( __POCC__ )

   typedef struct _OSVERSIONINFOEXW
   {
      DWORD dwOSVersionInfoSize;
      DWORD dwMajorVersion;
      DWORD dwMinorVersion;
      DWORD dwBuildNumber;
      DWORD dwPlatformId;
      WCHAR szCSDVersion[ 128 ];
      WORD  wServicePackMajor;
      WORD  wServicePackMinor;
      WORD  wSuiteMask;
      BYTE  wProductType;
      BYTE  wReserved;
   } OSVERSIONINFOEXW, * LPOSVERSIONINFOEXW;
#endif

typedef BOOL ( WINAPI * _HB_VERIFYVERSIONINFO )( LPOSVERSIONINFOEXW, DWORD, DWORDLONG );
typedef ULONGLONG ( WINAPI * _HB_VERSETCONDITIONMASK )( ULONGLONG, DWORD, BYTE );

static HB_BOOL s_fVerInfoInit = HB_TRUE;
static _HB_VERIFYVERSIONINFO   s_pVerifyVersionInfo   = NULL;
static _HB_VERSETCONDITIONMASK s_pVerSetConditionMask = NULL;

static HB_BOOL s_hb_winVerifyVersionInit( void )
{
   if( s_fVerInfoInit )
   {
      HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
      if( hModule )
      {
         s_pVerifyVersionInfo = ( _HB_VERIFYVERSIONINFO ) HB_WINAPI_GETPROCADDRESS( hModule, "VerifyVersionInfoW" );
         s_pVerSetConditionMask = ( _HB_VERSETCONDITIONMASK ) HB_WINAPI_GETPROCADDRESS( hModule, "VerSetConditionMask" );
      }
      s_fVerInfoInit = HB_FALSE;
   }

   return s_pVerifyVersionInfo &&
          s_pVerSetConditionMask;
}

#endif

static void s_hb_winVerInit( void )
{
#if ! defined( HB_OS_WIN_CE )
   s_fWin10    = hb_iswinver( 10, 0, 0, HB_TRUE );
   if( s_fWin10 )
      s_fWin11 = hb_iswinbuild( 22000, HB_TRUE );
   s_fWin81    = hb_iswinver( 6, 3, 0, HB_TRUE );
   s_fWin8     = hb_iswinver( 6, 2, 0, HB_TRUE );
   s_fWin7     = hb_iswinver( 6, 1, 0, HB_TRUE );
   s_fWinVista = hb_iswinver( 6, 0, 0, HB_TRUE );
   s_fWin2K3   = hb_iswinver( 5, 2, VER_NT_SERVER, HB_TRUE ) || hb_iswinver( 5, 2, VER_NT_DOMAIN_CONTROLLER, HB_TRUE );
   s_fWin2K    = hb_iswinver( 5, 0, 0, HB_TRUE );


#if !( defined( HB_OS_WIN_64 ) || ( defined( _MSC_VER ) && _MSC_VER > 1310 ) )
   {
      OSVERSIONINFO osvi;
      osvi.dwOSVersionInfoSize = sizeof( osvi );
      if( GetVersionEx( &osvi ) )
      {
         /* NOTE: Value is VER_PLATFORM_WIN32_CE on WinCE */
         if( osvi.dwPlatformId != VER_PLATFORM_WIN32_WINDOWS )
            s_iWin9x = 0;
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion < 10 )
            s_iWin9x = 5;  /* 95 */
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 )
            s_iWin9x = 8;  /* 98 */
         else
            s_iWin9x = 9;  /* ME */

         if( osvi.dwPlatformId != VER_PLATFORM_WIN32_NT )
            s_iWinNT = 0;
         else if( osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 )
            s_iWinNT = 3;  /* 3.51 */
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 )
            s_iWinNT = 4;  /* 4.0 */
         else
            s_iWinNT = 5;  /* 2000/XP/2003 */
      }
   }
#endif

   {
      /* NOTE: Unofficial Wine detection.
               https://www.mail-archive.com/wine-devel@winehq.org/msg48659.html */
      HMODULE hntdll = GetModuleHandle( TEXT( "ntdll.dll" ) );
      if( hntdll && HB_WINAPI_GETPROCADDRESS( hntdll, "wine_get_version" ) )
         s_iWine = 1;
   }

   if( s_fWin10 && ! s_iWine )
   {
      /* reusing s_iWinNT to store build number on Win10+ */

#if 0
      typedef LONG NTSTATUS, * PNTSTATUS;
      #define STATUS_SUCCESS (0x00000000)
      typedef NTSTATUS ( WINAPI * _HB_RTLGETVERSION )( LPOSVERSIONINFOW );
      HMODULE hntdll = GetModuleHandle( TEXT( "ntdll.dll" ) );

      _HB_RTLGETVERSION pRtlGetVersion;
      if( hntdll )
      {
         pRtlGetVersion := ( _HB_RTLGETVERSION ) HB_WINAPI_GETPROCADDRESS( hntdll, "RtlGetVersion" ) )
         OSVERSIONINFOW ovi = { 0 };
         ovi.dwOSVersionInfoSize = sizeof( ovi );
         if( STATUS_SUCCESS == pRtlGetVersion( &ovi ) )
         {
            s_iWinNT = ( int ) ovi.dwBuildNumber;
         }
      }
#endif

      /* NOTE: NT system version is always mapped into process (user-mode)
               memory, though build number is there only on Win10 and up.
               https://www.geoffchappell.com/studies/windows/km/ntoskrnl/inc/api/ntexapi_x/kuser_shared_data/index.htm */

      MEMORY_BASIC_INFORMATION minfo;
      minfo.Protect = 0;
      if( VirtualQuery( ( void * ) 0x7FFE0000, &minfo, sizeof( minfo ) ) &&
          ( minfo.Protect & ( PAGE_READONLY | PAGE_READWRITE | PAGE_WRITECOPY |
            PAGE_EXECUTE_READ | PAGE_EXECUTE_READWRITE | PAGE_EXECUTE_WRITECOPY ) ) &&
          ! ( minfo.Protect & ( PAGE_GUARD | PAGE_NOACCESS ) ) )
         s_iWinNT = ( int ) * ( PULONG ) ( 0x7FFE0000 + 0x0260 );
      else
         s_iWinNT = 1; /* unknown NT emulator */

      /* COMPAT: this seems much simpler than dyn-calling GetVersionEx
                 or WDK RtlGetVersion (having in mind deprecation warnings,
                 regressions from obscure compilers with conflicting headers)
                 If this ever causes a GPF on memory read, please revert,
                 or migrate to APIs. Mem-addr is correct for AMD64, ARM64 too.
                 WINE is intentionally excluded for reason, where an unknown
                 build may not support this. */

   }
   else if( s_fWin2K )
      s_iWinNT = 5;
#endif

   s_fWinVerInit = HB_TRUE;
}

#elif defined( HB_OS_DOS )

static HB_BOOL s_fWinVerInit = HB_FALSE;

static HB_BOOL s_fWin11    = HB_FALSE;
static HB_BOOL s_fWin10    = HB_FALSE;
static HB_BOOL s_fWin81    = HB_FALSE;
static HB_BOOL s_fWin8     = HB_FALSE;
static HB_BOOL s_fWin7     = HB_FALSE;
static HB_BOOL s_fWinVista = HB_FALSE;
static HB_BOOL s_fWin2K3   = HB_FALSE;
static HB_BOOL s_fWin2K    = HB_FALSE;
static int     s_iWinNT    = 0;
static int     s_iWin9x    = 0;
static int     s_iWine     = 0;

static void s_hb_winVerInit( void )
{
   union REGS regs;

   /* TODO */
   s_fWin11    = HB_FALSE;
   s_fWin10    = HB_FALSE;
   s_fWin81    = HB_FALSE;
   s_fWin8     = HB_FALSE;
   s_fWin7     = HB_FALSE;
   s_fWinVista = HB_FALSE;
   s_fWin2K3   = s_fWinVista;
   s_fWin2K    = HB_FALSE;

   /* Host OS detection: Windows NT family */

   {
      regs.HB_XREGS.ax = 0x3306;
      HB_DOS_INT86( 0x21, &regs, &regs );

      if( regs.HB_XREGS.bx == 0x3205 )
         s_iWinNT = 5;
   }

   /* Host OS detection: 95/98 */

   if( s_iWinNT == 0 )
   {
      regs.HB_XREGS.ax = 0x1600;
      HB_DOS_INT86( 0x2F, &regs, &regs );

      if( regs.h.al != 0x80 &&
          regs.h.al != 0xFF &&
          regs.h.al >= 4 )
         s_iWin9x = 5;
   }

   s_fWinVerInit = HB_TRUE;
}

#endif

/* NOTE: Must be larger than 128, which is the maximum size of
         osvi.szCSDVersion (Windows). [vszakats] */
#define PLATFORM_BUF_SIZE  255

char * hb_verPlatform( void )
{
   char * pszPlatform;

   HB_TRACE( HB_TR_DEBUG, ( "hb_verPlatform()" ) );

   pszPlatform = ( char * ) hb_xgrab( PLATFORM_BUF_SIZE + 1 );

#if defined( HB_OS_DOS )

   {
      union REGS regs;

      regs.h.ah = 0x30;
      HB_DOS_INT86( 0x21, &regs, &regs );

      hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "DOS %d.%02d", regs.h.al, regs.h.ah );

      /* Host OS detection: Windows 2.x, 3.x, 95/98 */

      {
         regs.HB_XREGS.ax = 0x1600;
         HB_DOS_INT86( 0x2F, &regs, &regs );

         if( regs.h.al != 0x00 && regs.h.al != 0x80 )
         {
            char szHost[ 128 ];

            if( regs.h.al == 0x01 || regs.h.al == 0xFF )
               hb_snprintf( szHost, sizeof( szHost ), " (Windows 2.x)" );
            else
               hb_snprintf( szHost, sizeof( szHost ), " (Windows %d.%02d)", regs.h.al, regs.h.ah );

            hb_strncat( pszPlatform, szHost, PLATFORM_BUF_SIZE );
         }
      }

      /* Host OS detection: Windows NT family */

      {
         regs.HB_XREGS.ax = 0x3306;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.HB_XREGS.bx == 0x3205 )
            hb_strncat( pszPlatform, " (Windows NT)", PLATFORM_BUF_SIZE );
      }

      /* Host OS detection: OS/2 */

      {
         regs.h.ah = 0x30;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.h.al >= 10 )
         {
            char szHost[ 128 ];

            if( regs.h.al == 20 && regs.h.ah > 20 )
               hb_snprintf( szHost, sizeof( szHost ), " (OS/2 %d.%02d)", regs.h.ah / 10, regs.h.ah % 10 );
            else
               hb_snprintf( szHost, sizeof( szHost ), " (OS/2 %d.%02d)", regs.h.al / 10, regs.h.ah );

            hb_strncat( pszPlatform, szHost, PLATFORM_BUF_SIZE );
         }
      }
   }

#elif defined( HB_OS_OS2 )

   {
      unsigned long aulQSV[ QSV_MAX ] = { 0 };
      APIRET rc = DosQuerySysInfo( 1L, QSV_MAX, ( void * ) aulQSV, sizeof( ULONG ) * QSV_MAX );

      if( rc == 0 )
      {
         /* is this OS/2 2.x ? */
         if( aulQSV[ QSV_VERSION_MINOR - 1 ] < 30 )
            hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "OS/2 %ld.%02ld",
                         aulQSV[ QSV_VERSION_MAJOR - 1 ] / 10,
                         aulQSV[ QSV_VERSION_MINOR - 1 ] );
         else
            hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "OS/2 %2.2f",
                         ( float ) aulQSV[ QSV_VERSION_MINOR - 1 ] / 10 );
      }
      else
         hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "OS/2" );
   }

#elif defined( HB_OS_WIN )

   {
      const char * pszName = "";

      OSVERSIONINFO osvi;

      memset( &osvi, 0, sizeof( osvi ) );

#if defined( HB_OS_WIN_CE )
      pszName = " CE";
      osvi.dwOSVersionInfoSize = sizeof( osvi );
      GetVersionEx( &osvi );
#else
      /* Detection of legacy Windows versions */
      switch( hb_iswin9x() )
      {
         case 5:
            osvi.dwMajorVersion = 4;
            osvi.dwMinorVersion = 0;
            pszName = " 95";
            break;
         case 8:
            osvi.dwMajorVersion = 4;
            osvi.dwMinorVersion = 10;
            pszName = " 98";
            break;
         case 9:
            osvi.dwMajorVersion = 4;
            osvi.dwMinorVersion = 90;
            pszName = " ME";
            break;
      }
#endif

      if( pszName[ 0 ] == '\0' )
      {
#if defined( HB_OS_WIN_CE )
         pszName = " CE";
#else
         if( hb_iswin11() )
         {
            osvi.dwMajorVersion = 10;
            osvi.dwMinorVersion = 0;
            if( hb_iswinver( 10, 0, VER_NT_WORKSTATION, HB_FALSE ) )
               pszName = " 11 or newer";
            else if( hb_iswinbuild( 26040, HB_TRUE ) )
               pszName = " Server 2025";
            else
               pszName = " Server 23H2";
         }
         else if( hb_iswin10() )
         {
            osvi.dwMajorVersion = 10;
            osvi.dwMinorVersion = 0;
            if( hb_iswinver( 10, 0, VER_NT_WORKSTATION, HB_FALSE ) )
               pszName = " 10";
            else if( hb_iswinbuild( 20348, HB_TRUE ) )
               pszName = " Server 2022";
            else if( hb_iswinbuild( 17763, HB_TRUE ) )
               pszName = " Server 2019";
            else
               pszName = " Server 2016";
         }
         else if( hb_iswin81() )
         {
            osvi.dwMajorVersion = 6;
            osvi.dwMinorVersion = 3;
            if( hb_iswinver( 6, 3, VER_NT_WORKSTATION, HB_FALSE ) )
               pszName = " 8.1";
            else
               pszName = " Server 2012 R2";
         }
         else if( hb_iswinvista() )
         {
            if( hb_iswin8() )
            {
               osvi.dwMajorVersion = 6;
               osvi.dwMinorVersion = 2;
               if( hb_iswinver( 6, 2, VER_NT_WORKSTATION, HB_FALSE ) )
                  pszName = " 8";
               else
                  pszName = " Server 2012";
            }
            else if( hb_iswinver( 6, 1, 0, HB_FALSE ) )
            {
               osvi.dwMajorVersion = 6;
               osvi.dwMinorVersion = 1;
               if( hb_iswinver( 6, 1, VER_NT_WORKSTATION, HB_FALSE ) )
                  pszName = " 7";
               else
                  pszName = " Server 2008 R2";
            }
            else
            {
               osvi.dwMajorVersion = 6;
               osvi.dwMinorVersion = 0;
               if( hb_iswinver( 6, 0, VER_NT_WORKSTATION, HB_FALSE ) )
                  pszName = " Vista";
               else
                  pszName = " Server 2008";
            }
         }
         else if( hb_iswinver( 5, 2, 0, HB_FALSE ) )
         {
            osvi.dwMajorVersion = 5;
            osvi.dwMinorVersion = 2;
            if( hb_iswinver( 5, 2, VER_NT_WORKSTATION, HB_FALSE ) )
               pszName = " XP x64";
            else if( GetSystemMetrics( SM_SERVERR2 ) != 0 )
               pszName = " Server 2003 R2";
            else
               pszName = " Server 2003";
         }
         else if( hb_iswinver( 5, 1, 0, HB_FALSE ) )
         {
            osvi.dwMajorVersion = 5;
            osvi.dwMinorVersion = 1;
            pszName = " XP";
         }
         else if( hb_iswin2k() )
         {
            osvi.dwMajorVersion = 5;
            osvi.dwMinorVersion = 0;
            pszName = " 2000";
         }
         else
            pszName = " NT";
#endif
      }

      hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "Windows%s%s %lu.%lu",
                   pszName,
                   s_iWine ? " (Wine)" : "",
                   osvi.dwMajorVersion,
                   osvi.dwMinorVersion );

      /* Add service pack/other info */

      if( hb_iswin10() && ! s_iWine )
      {
         /* On Win10+ build number is more significant than Major Minor */
         char szBuild[ 8 ];
         hb_snprintf( szBuild, sizeof( szBuild ), ".%lu", ( DWORD ) s_iWinNT );
         hb_strncat( pszPlatform, szBuild, PLATFORM_BUF_SIZE );
      }
      else if( hb_iswin2k() )
      {
         int tmp;

         for( tmp = 5; tmp > 0; --tmp )
         {
            if( hb_iswinsp( tmp, HB_TRUE ) )
            {
               char szServicePack[ 8 ];
               hb_snprintf( szServicePack, sizeof( szServicePack ), " SP%u", tmp );
               hb_strncat( pszPlatform, szServicePack, PLATFORM_BUF_SIZE );
               break;
            }
         }
      }
#if defined( HB_OS_WIN_CE )
      else
      {
         /* Also for Win9x and NT, but GetVersionEx() is deprecated
            so we avoid it. */
         if( osvi.szCSDVersion[ 0 ] != TEXT( '\0' ) )
         {
            char * pszCSDVersion = HB_OSSTRDUP( osvi.szCSDVersion );
            int i;

            /* Skip the leading spaces (Win95B, Win98) */
            for( i = 0; pszCSDVersion[ i ] != '\0' && HB_ISSPACE( ( int ) pszCSDVersion[ i ] ); i++ )
               ;

            if( pszCSDVersion[ i ] != '\0' )
            {
               hb_strncat( pszPlatform, " ", PLATFORM_BUF_SIZE );
               hb_strncat( pszPlatform, pszCSDVersion + i, PLATFORM_BUF_SIZE );
            }
            hb_xfree( pszCSDVersion );
         }
      }
#endif
   }

#elif defined( __CEGCC__ )
   {
      hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "Windows CE" );
   }
#elif defined( HB_OS_UNIX )

   {
      struct utsname un;

      uname( &un );
#if defined( HB_OS_MINIX )
      hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "%s Release %s Version %s %s",
                   un.sysname, un.release, un.version, un.machine );
#else
      hb_snprintf( pszPlatform, PLATFORM_BUF_SIZE + 1, "%s %s %s", un.sysname, un.release, un.machine );
#endif
   }

#else

   {
      hb_strncpy( pszPlatform, "(unrecognized)", PLATFORM_BUF_SIZE );
   }

#endif

   return pszPlatform;
}

HB_BOOL hb_iswinver( int iMajor, int iMinor, int iType, HB_BOOL fOrUpper )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   if( s_hb_winVerifyVersionInit() )
   {
      OSVERSIONINFOEXW ver;
      DWORD dwTypeMask = VER_MAJORVERSION | VER_MINORVERSION;
      DWORDLONG dwlConditionMask = 0;

      memset( &ver, 0, sizeof( ver ) );
      ver.dwOSVersionInfoSize = sizeof( ver );
      ver.dwMajorVersion = ( DWORD ) iMajor;
      ver.dwMinorVersion = ( DWORD ) iMinor;

      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_MAJORVERSION, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );
      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_MINORVERSION, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );

      /* MSDN says in https://msdn.microsoft.com/library/ms725492
           "If you are testing the major version, you must also test the
            minor version and the service pack major and minor versions."
         However, Wine (as of 1.7.53) breaks on this. Since native Windows
         apparently doesn't care, we're not doing it for now.
         Wine (emulating Windows 7) will erroneously return HB_FALSE from
         these calls:
           hb_iswinver( 6, 1, 0, HB_FALSE );
           hb_iswinver( 6, 1, VER_NT_WORKSTATION, HB_FALSE );
         Removing the Service Pack check, or changing HB_FALSE to HB_TRUE
         in above calls, both fixes the problem. [vszakats] */
#if defined( __HB_DISABLE_WINE_VERIFYVERSIONINFO_BUG_WORKAROUND )
      ver.wServicePackMajor =
      ver.wServicePackMinor = ( WORD ) 0;
      dwTypeMask |= VER_SERVICEPACKMAJOR | VER_SERVICEPACKMINOR;
      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL );
      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_SERVICEPACKMINOR, VER_GREATER_EQUAL );
#endif

      if( iType )
      {
         dwTypeMask |= VER_PRODUCT_TYPE;
         ver.wProductType = ( BYTE ) iType;
         dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_PRODUCT_TYPE, VER_EQUAL );
      }

      return ( HB_BOOL ) s_pVerifyVersionInfo( &ver, dwTypeMask, dwlConditionMask );
   }
#else
   HB_SYMBOL_UNUSED( iMajor );
   HB_SYMBOL_UNUSED( iMinor );
   HB_SYMBOL_UNUSED( iType );
   HB_SYMBOL_UNUSED( fOrUpper );
#endif
   return HB_FALSE;
}

HB_BOOL hb_iswinsp( int iServicePackMajor, HB_BOOL fOrUpper )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   if( s_hb_winVerifyVersionInit() )
   {
      OSVERSIONINFOEXW ver;
      DWORDLONG dwlConditionMask = 0;

      memset( &ver, 0, sizeof( ver ) );
      ver.dwOSVersionInfoSize = sizeof( ver );
      ver.wServicePackMajor = ( WORD ) iServicePackMajor;

      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_SERVICEPACKMAJOR, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );

      return ( HB_BOOL ) s_pVerifyVersionInfo( &ver, VER_SERVICEPACKMAJOR, dwlConditionMask );
   }
#else
   HB_SYMBOL_UNUSED( iServicePackMajor );
   HB_SYMBOL_UNUSED( fOrUpper );
#endif
   return HB_FALSE;
}

HB_BOOL hb_iswinbuild( int iBuildNum, HB_BOOL fOrUpper )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   if( s_hb_winVerifyVersionInit() )
   {
      OSVERSIONINFOEXW ver;
      DWORDLONG dwlConditionMask = 0;

      memset( &ver, 0, sizeof( ver ) );
      ver.dwOSVersionInfoSize = sizeof( ver );
      ver.dwBuildNumber = ( DWORD ) iBuildNum;

      dwlConditionMask = s_pVerSetConditionMask( dwlConditionMask, VER_BUILDNUMBER, fOrUpper ? VER_GREATER_EQUAL : VER_EQUAL );

      return ( HB_BOOL ) s_pVerifyVersionInfo( &ver, VER_BUILDNUMBER, dwlConditionMask );
   }
#else
   HB_SYMBOL_UNUSED( iBuildNum );
   HB_SYMBOL_UNUSED( fOrUpper );
#endif
   return HB_FALSE;
}

int hb_iswine( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_iWine;
#else
   return 0;
#endif
}

HB_BOOL hb_iswin11( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin11;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswin10( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin10;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswin81( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin81;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswin8( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin8;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswin7( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin7;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswinvista( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWinVista;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswin2k3( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin2K3;
#else
   return HB_FALSE;
#endif
}

HB_BOOL hb_iswin2k( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_fWin2K;
#else
   return HB_FALSE;
#endif
}

int hb_iswinnt( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_iWinNT;
#else
   return 0;
#endif
}

int hb_iswin9x( void )
{
#if defined( HB_OS_WIN ) || defined( HB_OS_DOS )
   if( ! s_fWinVerInit )
      s_hb_winVerInit();
   return s_iWin9x;
#else
   return 0;
#endif
}

HB_BOOL hb_iswince( void )
{
#if defined( HB_OS_WIN_CE )
   return HB_TRUE;
#else
   return HB_FALSE;
#endif
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

#define COMPILER_BUF_SIZE  80

char * hb_verCompiler( void )
{
   char * pszCompiler;
   const char * pszName;
   char szSub[ 64 ];
   int iVerMajor;
   int iVerMinor;
   int iVerPatch;
   int iVerMicro = 0;
   int iElements = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_verCompiler()" ) );

   pszCompiler = ( char * ) hb_xgrab( COMPILER_BUF_SIZE );
   szSub[ 0 ] = '\0';

#if defined( __IBMC__ ) || defined( __IBMCPP__ )

   #if defined( __IBMC__ )
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
   iVerPatch = 0;

#elif defined( __POCC__ )

   pszName = "Pelles ISO C Compiler";
   iVerMajor = __POCC__ / 100;
   iVerMinor = __POCC__ % 100;
   iVerPatch = 0;

#elif defined( __XCC__ )

   pszName = "Pelles ISO C Compiler (XCC)";
   iVerMajor = __XCC__ / 100;
   iVerMinor = __XCC__ % 100;
   iVerPatch = 0;

#elif defined( __LCC__ )

   pszName = "Logiciels/Informatique lcc-win32";
   iVerMajor = 0 /* __LCC__ / 100 */;
   iVerMinor = 0 /* __LCC__ % 100 */;
   iVerPatch = 0;

#elif defined( __DMC__ )

   pszName = __DMC_VERSION_STRING__;
   iVerMajor = 0;
   iVerMinor = 0;
   iVerPatch = 0;

#elif defined( __INTEL_COMPILER )

   pszName = "Intel(R) C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __INTEL_COMPILER / 100;
   iVerMinor = ( __INTEL_COMPILER % 100 ) / 10;
   iVerPatch = 0;

#elif defined( __ICL )

   pszName = "Intel(R) C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __ICL / 100;
   iVerMinor = __ICL % 100;
   iVerPatch = 0;

#elif defined( __ICC )

   pszName = "Intel(R) (ICC) C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __ICC / 100;
   iVerMinor = __ICC % 100;
   iVerPatch = 0;

#elif defined( __OPENCC__ )

   pszName = "Open64 C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __OPENCC__;
   iVerMinor = __OPENCC_MINOR__;
#if __OPENCC_PATCHLEVEL__ - 0 <= 0
   #undef __OPENCC_PATCHLEVEL__
   #define __OPENCC_PATCHLEVEL__ 0
#endif
   iVerPatch = __OPENCC_PATCHLEVEL__;

#elif defined( __clang__ ) && defined( __clang_major__ )

   /* NOTE: keep clang detection before msvc detection. */

   pszName = "LLVM/Clang C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __clang_major__;
   iVerMinor = __clang_minor__;
   iVerPatch = __clang_patchlevel__;

   #if ! defined( HB_CPU_X86 ) && ! defined( HB_CPU_X86_64 )
      #define __HB_ARCH_VERSION /* add string supplement for "non-classic" architectures */
   #endif

#elif defined( __clang__ )

   pszName = "LLVM/Clang C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   hb_strncat( szSub, " 1.x", sizeof( szSub ) - 1 );

   iVerMajor = iVerMinor = iVerPatch = 0;

#elif defined( __llvm__ ) && defined( __GNUC__ )

   pszName = "LLVM/GNU C";

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __GNUC__;
   iVerMinor = __GNUC_MINOR__;
   #if defined( __GNUC_PATCHLEVEL__ )
      iVerPatch = __GNUC_PATCHLEVEL__;
   #else
      iVerPatch = 0;
   #endif

#elif defined( _MSC_VER )

   #if _MSC_VER >= 800
      pszName = "Microsoft Visual C";
   #else
      pszName = "Microsoft C";
   #endif

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   #if ! defined( HB_CPU_X86 ) && ! defined( HB_CPU_X86_64 )
      #define __HB_ARCH_VERSION /* add string supplement for "non-classic" architectures */
   #endif

   iVerMajor = _MSC_VER / 100;
   iVerMinor = _MSC_VER % 100;

   #if defined( _MSC_FULL_VER )
      #if _MSC_VER >= 1400
         iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 100000 );
      #else
         iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 10000 );
      #endif
   #else
      iVerPatch = 0;
   #endif

#elif defined( __BORLANDC__ )

   #if __BORLANDC__ >= 0x0590  /* Version 5.9 */
      #if __BORLANDC__ >= 0x0620  /* Version 6.2 */
         pszName = "Borland/Embarcadero C++";
      #else
         pszName = "Borland/CodeGear C++";
      #endif
   #else
      pszName = "Borland C++";
   #endif
   #if   __BORLANDC__ == 0x0400  /* Version 3.0 */
      iVerMajor = 3;
      iVerMinor = 0;
      iVerPatch = 0;
   #elif __BORLANDC__ == 0x0410  /* Version 3.1 */
      iVerMajor = 3;
      iVerMinor = 1;
      iVerPatch = 0;
   #elif __BORLANDC__ == 0x0452  /* Version 4.0 */
      iVerMajor = 4;
      iVerMinor = 0;
      iVerPatch = 0;
   #elif __BORLANDC__ == 0x0460  /* Version 4.5 */
      iVerMajor = 4;
      iVerMinor = 5;
      iVerPatch = 0;
   #elif __BORLANDC__ >= 0x0500  /* Version 5.x */
      iVerMajor = __BORLANDC__ >> 8;
      iVerMinor = ( __BORLANDC__ & 0xFF ) >> 4;
      iVerPatch = __BORLANDC__ & 0xF;
   #else /* Version 4.x */
      iVerMajor = __BORLANDC__ >> 8;
      iVerMinor = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
      iVerPatch = 0;
   #endif

#elif defined( __TURBOC__ )

   pszName = "Borland Turbo C";
   iVerMajor = __TURBOC__ >> 8;
   iVerMinor = __TURBOC__ & 0xFF;
   iVerPatch = 0;

#elif defined( __MPW__ )

   pszName = "MPW C";
   iVerMajor = __MPW__ / 100;
   iVerMinor = __MPW__ % 100;
   iVerPatch = 0;

#elif defined( __WATCOMC__ )

   #if __WATCOMC__ < 1200
      pszName = "Watcom C";
   #else
      pszName = "Open Watcom C";
   #endif

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __WATCOMC__ / 100;
   iVerMinor = __WATCOMC__ % 100;

   #if defined( __WATCOM_REVISION__ )
      iVerPatch = __WATCOM_REVISION__;
   #else
      iVerPatch = 0;
   #endif

#elif defined( __DCC__ )

   pszName = "Wind River Compiler (diab)";

   iVerMajor = ( __VERSION_NUMBER__ / 1000 ) % 10;
   iVerMinor = ( __VERSION_NUMBER__ / 100 ) % 10;
   iVerPatch = ( __VERSION_NUMBER__ / 10 ) % 10;
   iVerMicro = __VERSION_NUMBER__ % 10;
   iElements = 4;

#elif defined( __TINYC__ )

   pszName = "Tiny C Compiler";

   iVerMajor = __TINYC__ / 100;
   iVerMinor = ( __TINYC__ % 100 ) / 10;
   iVerPatch = ( __TINYC__ % 100 ) % 10;

#elif defined( __PCC__ )

   pszName = "Portable C Compiler";

   iVerMajor = __PCC__;
   iVerMinor = __PCC_MINOR__;
   iVerPatch = __PCC_MINORMINOR__;

   #if defined( __GNUC__ )
      hb_snprintf( szSub, sizeof( szSub ), " (GCC %d.%d.%d emul.)",
                   __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__ );
   #endif

#elif defined( __GNUC__ )

   #if defined( __DJGPP__ )
      pszName = "Delorie GNU C";
   #elif defined( __CYGWIN__ )
      pszName = "Cygwin GNU C";
   #elif defined( __MINGW32__ )
      pszName = "MinGW GNU C";
   #elif defined( __RSX32__ )
      pszName = "EMX/RSXNT/DOS GNU C";
   #elif defined( __RSXNT__ )
      pszName = "EMX/RSXNT/Win32 GNU C";
   #elif defined( __EMX__ )
      pszName = "EMX GNU C";
   #else
      pszName = "GNU C";
   #endif

   #if defined( __cplusplus )
      hb_strncpy( szSub, "++", sizeof( szSub ) - 1 );
   #endif

   iVerMajor = __GNUC__;
   iVerMinor = __GNUC_MINOR__;
   #if defined( __GNUC_PATCHLEVEL__ )
      iVerPatch = __GNUC_PATCHLEVEL__;
   #else
      iVerPatch = 0;
   #endif

#elif defined( __SUNPRO_C )

   pszName = "Sun C";
   #if __SUNPRO_C < 0x1000
      iVerMajor = __SUNPRO_C / 0x100;
      iVerMinor = ( __SUNPRO_C & 0xff ) / 0x10;
      iVerPatch = __SUNPRO_C & 0xf;
   #else
      iVerMajor = __SUNPRO_C / 0x1000;
      iVerMinor = __SUNPRO_C / 0x10 & 0xff;
      iVerMinor = iVerMinor / 0x10 * 0xa + iVerMinor % 0x10;
      iVerPatch = __SUNPRO_C & 0xf;
   #endif

#elif defined( __SUNPRO_CC )

   pszName = "Sun C++";
   #if __SUNPRO_CC < 0x1000
      iVerMajor = __SUNPRO_CC / 0x100;
      iVerMinor = ( __SUNPRO_CC & 0xff ) / 0x10;
      iVerPatch = __SUNPRO_CC & 0xf;
   #else
      iVerMajor = __SUNPRO_CC / 0x1000;
      iVerMinor = __SUNPRO_CC / 0x10 & 0xff;
      iVerMinor = iVerMinor / 0x10 * 0xa + iVerMinor % 0x10;
      iVerPatch = __SUNPRO_CC & 0xf;
   #endif

#else

   pszName = NULL;
   iVerMajor = iVerMinor = iVerPatch = 0;

#endif

   if( pszName )
   {
      if( iElements == 4 )
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch, iVerMicro );
      else if( iVerPatch != 0 )
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d.%d", pszName, szSub, iVerMajor, iVerMinor, iVerPatch );
      else if( iVerMajor != 0 || iVerMinor != 0 )
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s %d.%d", pszName, szSub, iVerMajor, iVerMinor );
      else
         hb_snprintf( pszCompiler, COMPILER_BUF_SIZE, "%s%s", pszName, szSub );
   }
   else
      hb_strncpy( pszCompiler, "(unknown)", COMPILER_BUF_SIZE - 1 );

#if defined( __clang_version__ )
   #if defined( __clang_major__ )
      /* prevent dups like "LLVM/Clang C 18.1.8 (18.1.8 )" */
      hb_snprintf( szSub, sizeof( szSub ), "%d.%d.%d ", iVerMajor, iVerMinor, iVerPatch );
      if( ! strstr( szSub, __clang_version__ ) )
      {
   #endif
         if( strstr( __clang_version__, "(" ) )
            /* "2.0 (trunk 103176)" -> "(trunk 103176)" */
            hb_snprintf( szSub, sizeof( szSub ), " %s", strstr( __clang_version__, "(" ) );
         else
            hb_snprintf( szSub, sizeof( szSub ), " (%s)", __clang_version__ );
         hb_strncat( pszCompiler, szSub, COMPILER_BUF_SIZE - 1 );
   #if defined( __clang_major__ )
      }
   #endif
#endif


#if defined( __DJGPP__ )
   hb_snprintf( szSub, sizeof( szSub ), " (DJGPP %i.%02i)", ( int ) __DJGPP__, ( int ) __DJGPP_MINOR__ );
   hb_strncat( pszCompiler, szSub, COMPILER_BUF_SIZE - 1 );
#endif

   #if defined( __HB_ARCH_VERSION )
      hb_strncat( pszCompiler, " ",         COMPILER_BUF_SIZE - 1 );
      hb_strncat( pszCompiler, hb_verCPU(), COMPILER_BUF_SIZE - 1 );
   #elif defined( HB_ARCH_16BIT )
      hb_strncat( pszCompiler, " (16-bit)", COMPILER_BUF_SIZE - 1 );
   #elif defined( HB_ARCH_32BIT )
      hb_strncat( pszCompiler, " (32-bit)", COMPILER_BUF_SIZE - 1 );
   #elif defined( HB_ARCH_64BIT )
      hb_strncat( pszCompiler, " (64-bit)", COMPILER_BUF_SIZE - 1 );
   #endif

   return pszCompiler;
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

/* NOTE:
   CA-Cl*pper 5.2e returns: "Clipper (R) 5.2e Intl. (x216)  (1995.02.07)"
   CA-Cl*pper 5.3b returns: "Clipper (R) 5.3b Intl. (Rev. 338) (1997.04.25)"
 */

char * hb_verHarbour( void )
{
   char * pszVersion;

   HB_TRACE( HB_TR_DEBUG, ( "hb_verHarbour()" ) );

   pszVersion = ( char * ) hb_xgrab( 80 );
   hb_snprintf( pszVersion, 80, "Harbour %d.%d.%d%s (r%lu)",
                HB_VER_MAJOR, HB_VER_MINOR, HB_VER_RELEASE, HB_VER_STATUS,
                ( HB_ULONG ) hb_verRevision() );

   return pszVersion;
}

char * hb_verPCode( void )
{
   char * pszPCode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_verPCode()" ) );

   pszPCode = ( char * ) hb_xgrab( 24 );
   hb_snprintf( pszPCode, 24, "PCode version: %d.%d",
                HB_PCODE_VER >> 8, HB_PCODE_VER & 0xFF );

   return pszPCode;
}

char * hb_verBuildDate( void )
{
   char * pszDate;

   HB_TRACE( HB_TR_DEBUG, ( "hb_verBuildDate()" ) );

   pszDate = ( char * ) hb_xgrab( 64 );
   hb_snprintf( pszDate, 64, "%s %s", __DATE__, __TIME__ );

   return pszDate;
}
