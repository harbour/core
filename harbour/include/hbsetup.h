/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for compiler and runtime configuration
 *
 * Copyright 2000-2009 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * www - http://harbour-project.org
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

#ifndef HB_SETUP_H_
#define HB_SETUP_H_

#include <limits.h>

/* ***********************************************************************
 * Include settings common for .prg and .c files
 */
#include "hbsetup.ch"

/* ***********************************************************************
 * Define PCODE version number
 * HB_PCODE_VER_MIN define minimum supported PCODE by HVM
 */

#define HB_PCODE_VER          0x0003
#define HB_PCODE_VER_MIN      0x0002

/* ***********************************************************************
 * NOTE: You can select the default language modul used by Harbour, by
 *       defining this to a valid language modul identifier.
 */

#ifndef HB_LANG_DEFAULT
   #define HB_LANG_DEFAULT       EN
#endif

/* ***********************************************************************
 * NOTE: You can select the default codepage used by Harbour, by
 *       defining this to a valid codepage modul identifier.
 */

#ifndef HB_CODEPAGE_DEFAULT
   #define HB_CODEPAGE_DEFAULT   EN
#endif

/* ***********************************************************************
 * Enable profiler support in HVM
 * By default this is turned off. Define HB_USE_PROFILER to turn it on.
 */

#ifndef HB_USE_PROFILER
   #define HB_NO_PROFILER
#endif

/* ***********************************************************************
 * This symbol defines if Harbour is compiled using C compiler
 * that support strict ANSI C only
 *
 * The only non ANSI C feature that we are using is an ability
 * to call functions before the 'main' module is called.
 * This trick is used to automatically join all symbol tables defined
 * in run-time support modules and in user defined modules.
 *   If strict ANSI C compability is required then all symbol tables
 * have to be joined manually by calling special function named
 * hb_vm_SymbolInit_<module_name>
 * (for example for myfirst.prg it will be: 'hb_vm_SymbolInit_MYFIRST'
 * The generation of this function is performed by the macro called
 * HB_CALL_ON_STARTUP that is defined in 'hbinit.h'
 *
 * By default we are using extensions to ANSI C (symbol is not defined)
 */
/* #define HB_STRICT_ANSI_C */

/* ***********************************************************************
 * Define this option if you want the /y YACC trace option to be available
 * in the Harbour compiler.
 *
 * Note that if you turn this on, the compiler will slighly grow in size.
 *
 * By default this is turned off.
 */
/* #define HB_YYDEBUG */

/* ***********************************************************************
 * Use native Windows memory allocation functions (HB_OS_WIN)
 * This option can disable compiler memory allocation optimization
 * so you should really have a good reason to enable it
 */

/* #define HB_FM_WIN_ALLOC */

/* ***********************************************************************
 * CPU detection
 */

/* Partially based on:
      http://predef.sourceforge.net/prearch.html
      http://poshlib.hookatooka.com/poshlib/trac.cgi/browser/posh.h
      [vszakats]
 */

#if   defined( __alpha__ ) || \
      defined( __alpha ) || \
      defined( alpha ) || \
      defined( _M_ALPHA )
   #define HB_CPU_ALPHA

#elif defined( __amd64__ ) || \
      defined( __amd64 ) || \
      defined( __AMD64__ ) || \
      defined( __x86_64__ ) || \
      defined( __x86_64 ) || \
      defined( _M_AMD64 ) || \
      defined( _M_X64 ) || \
      defined( __MINGW64__ )
   #define HB_CPU_X86_64

#elif defined( __arm__ ) || \
      defined( __arm ) || \
      defined( ARM ) || \
      defined( _ARM ) || \
      defined( _M_ARM )
   #define HB_CPU_ARM

#elif defined( __hppa__ ) || \
      defined( __hppa ) || \
      defined( hppa )
   #define HB_CPU_PARISC

#elif defined( i386 ) || \
      defined( __i386__ ) || \
      defined( __i386 ) || \
      defined( __386__ ) || \
      defined( _M_IX86 ) || \
      defined( _M_I86 ) || \
      defined( M_I86 ) || \
      defined( __X86__ ) || \
      defined( _X86_ ) || \
      defined( __I86__ ) || \
      defined( __THW_INTEL__ ) || \
      defined( __INTEL__ )
   #define HB_CPU_X86

#elif defined( __ia64__ ) || \
      defined( __ia64 ) || \
      defined( _IA64 ) || \
      defined( __IA64__ ) || \
      defined( _M_IA64 )
   #define HB_CPU_IA_64

#elif defined( __m68k__ ) || \
      defined( M68000 )
   #define HB_CPU_M68K

#elif defined( __mips__ ) || \
      defined( __mips ) || \
      defined( __MIPS__ ) || \
      defined( mips ) || \
      defined( _MIPS ) || \
      defined( __MIPS__ ) || \
      defined( _M_MRX000 ) || \
      defined( _M_MIPS )
   #define HB_CPU_MIPS

#elif defined( __powerpc64__ ) || \
      defined( __ppc64__ ) || \
      defined( _ARCH_PPC64 )
   #define HB_CPU_PPC_64

#elif defined( __powerpc__ ) || \
      defined( __powerpc ) || \
      defined( __POWERPC__ ) || \
      defined( __ppc ) || \
      defined( __ppc__ ) || \
      defined( __PPC__ ) || \
      defined( _ARCH_PPC ) || \
      defined( _M_MPPC ) || \
      defined( _M_PPC )
   #define HB_CPU_PPC

#elif defined( __THW_RS6000 ) || \
      defined( _IBMR2 ) || \
      defined( _POWER ) || \
      defined( _ARCH_PWR ) || \
      defined( _ARCH_PWR2 )
   #define HB_CPU_POWER

#elif defined( __sparc__ ) || \
      defined( __sparc )
   #if defined( __arch64__ ) || \
       defined( __sparcv9 ) || \
       defined( __sparc_v9__ )
      #define HB_CPU_SPARC_64
   #else
      #define HB_CPU_SPARC
   #endif

#elif defined( __sh__ ) || \
      defined( _SH3 ) || \
      defined( __sh4__ ) || \
      defined( __SH4__ ) || \
      defined( _M_SH )
   #define HB_CPU_SH

#elif defined( __370__ ) || \
      defined( __THW_370__ )
   #define HB_CPU_SYS370

#elif defined( __s390__ ) || \
      defined( __s390x__ )
   #define HB_CPU_SYS390

#elif defined( __SYSC_ZARCH__ )
   #define HB_CPU_ZARCH

#endif

/* ***********************************************************************
 * You can select here, what type of main entry will be used in the
 * application (main() or WinMain()).
 *
 * By default the standard C main() function will be used.
 */
/* #define HB_MAIN_STD */
/* #define HB_MAIN_WIN */

/* NOTE:
   Compiler                                _MSC_VER value
   --------                                --------------
   C Compiler version 6.0                  600
   C/C++ compiler version 7.0              700
   Visual C++, Windows, version 1.0        800
   Visual C++, 32-bit, version 1.0         800
   Visual C++, Windows, version 2.0        900
   Visual C++, 32-bit, version 2.x         900
   Visual C++, 32-bit, version 4.0         1000
   Visual C++, 32-bit, version 5.0         1100
   Visual C++, 32-bit, version 6.0         1200
   Visual Studio .NET (2002), version 7.0  1300
   Visual Studio .NET 2003, version 7.1    1310
   Visual Studio 2005, version 8.0         1400
   Visual Studio 2008, version 9.0         1500
   Visual Studio 2010, version 10.0        1600
*/

/* ***********************************************************************
 * Platform detection
 */

#if defined( __WATCOMC__ )
   #if defined( __OS2__ )
      #define HB_OS_OS2
   #elif defined( __NT__ ) || defined( __WINDOWS_386__ ) || defined( __WINDOWS__ )
      #define HB_OS_WIN
   #elif defined( __LINUX__ )
      #define HB_OS_LINUX
   #elif defined( __386__ )
      #define HB_OS_DOS
      #define HB_OS_DOS_32
   #else
      #define HB_OS_DOS
      #define HB_OS_DOS_16
   #endif
#endif

#ifndef HB_OS_DOS
   #if defined( DOS ) || defined( _QC ) || defined( __DOS__ ) || defined( MSDOS ) || defined( __MSDOS__ ) || defined( __RSX32__ )
      #define HB_OS_DOS
      #if defined( __386__ ) || defined( __DJGPP__ )
         #define HB_OS_DOS_32
      #else
         #define HB_OS_DOS_16
      #endif
   #endif
#endif

#if defined( __EMX__ ) && ! defined( __RSXNT__ )
   #define HB_OS_OS2_GCC
#endif
#ifndef HB_OS_OS2
   #if defined( OS2 ) || defined( __OS2__ ) || defined( OS_2 ) || defined( HB_OS_OS2_GCC )
      #define HB_OS_OS2
   #endif
#endif

#ifndef HB_OS_WIN
   #if defined( WINNT ) || defined( _Windows ) || defined( __NT__ ) || defined( _WIN32 ) || defined( _WINDOWS_ ) || defined( __WINDOWS_386__ ) || defined( __WIN32__ )
      #define HB_OS_WIN
   #endif
#endif

/* Sub-option inside HB_OS_WIN */
#ifndef HB_OS_WIN_64
   #if defined( _WIN64 )
      #define HB_OS_WIN_64
   #endif
#endif

/* Sub-option inside HB_OS_WIN */
#ifndef HB_OS_WIN_CE
   #if defined( UNDER_CE ) || defined( __CEGCC__ ) || defined( __MINGW32CE__ )
      #define HB_OS_WIN_CE
   #endif
#endif

#ifndef HB_OS_LINUX
   #if defined( linux ) || defined( __linux ) || defined( __linux__ ) || defined( __gnu_linux__ )
      #define HB_OS_LINUX
   #endif
#endif

#ifndef HB_OS_SUNOS
   #if defined( sun ) || defined( __sun ) || defined( __sun__ )
      #define HB_OS_SUNOS
   #endif
#endif

#ifndef HB_OS_HPUX
   /* HP cc in ANSI mode defines __hpux. GCC defines __hpux__ */
   #if defined( __hpux ) || defined( __hpux__ )
      #define HB_OS_HPUX
   #endif
#endif

#ifndef HB_OS_DARWIN
   #if defined( __APPLE__ ) || defined( __DARWIN__ )
      #define HB_OS_DARWIN
   #endif
#endif

/* Sub-option inside HB_OS_DARWIN */
#ifndef HB_OS_IOS /* Experimental */
   #if defined( HB_OS_DARWIN ) && defined( HB_CPU_ARM )
      #define HB_OS_IOS
   #endif
#endif

#ifndef HB_OS_BSD
   #if defined( __FreeBSD__ ) || defined( __NetBSD__ ) || defined( __OpenBSD__ ) || \
       defined( HB_OS_DARWIN ) || defined( __DragonFly__ )
      #define HB_OS_BSD
   #endif
#endif

#ifndef HB_OS_BEOS
   #if defined( __HAIKU__ ) || defined( __BEOS__ )
      #define HB_OS_BEOS
   #endif
#endif

#ifndef HB_OS_QNX
   #if defined( __QNX__ )
      #define HB_OS_QNX
   #endif
#endif

#ifndef HB_OS_VXWORKS
   #if defined( __VXWORKS__ ) || defined( __vxworks )
      #define HB_OS_VXWORKS
   #endif
#endif

#ifndef HB_OS_SYMBIAN
   #if defined( __symbian__ )
      #define HB_OS_SYMBIAN
   #endif
#endif

#ifndef HB_OS_ANDROID /* Experimental */
   #if defined( __ANDROID__ )
      #define HB_OS_ANDROID
   #endif
#endif

#ifndef HB_OS_CYGWIN
   #if defined( __CYGWIN__ )
      #define HB_OS_CYGWIN
   #endif
#endif

#ifndef HB_OS_MINIX
   #if defined( __minix )
      #define HB_OS_MINIX
   #endif
#endif

#ifndef HB_OS_UNIX
   #if defined( HB_OS_LINUX ) || \
       defined( HB_OS_DARWIN ) || \
       defined( HB_OS_BSD ) || \
       defined( HB_OS_SUNOS ) || \
       defined( HB_OS_HPUX ) || \
       defined( HB_OS_QNX ) || \
       defined( HB_OS_VXWORKS ) || \
       defined( HB_OS_BEOS ) || \
       defined( HB_OS_SYMBIAN ) || \
       defined( HB_OS_ANDROID ) || \
       defined( HB_OS_CYGWIN ) || \
       defined( HB_OS_MINIX )
      #define HB_OS_UNIX
   #endif
#endif

#if defined( HB_OS_VXWORKS )
   #define HB_NO_FNMATCH
   #define HB_USE_SHARELOCKS_OFF
   /* NOTE: Needed to avoid 'implicit bzero() declaration' warnings */
   extern void bzero( char * buffer, int nbytes );
#elif defined( HB_OS_SYMBIAN )
   #define HB_NO_FNMATCH
#endif

/* ***********************************************************************
 * Operating system specific definitions
 */
#if defined( HB_OS_UNIX )
   #define HB_OS_PATH_LIST_SEP_CHR      ':'
   #define HB_OS_PATH_DELIM_CHR         '/'
   #define HB_OS_PATH_DELIM_CHR_STRING  "/"
   #define HB_OS_PATH_DELIM_CHR_LIST    "/"
   #define HB_OS_ALLFILE_MASK           "*"
   #undef  HB_OS_DRIVE_DELIM_CHR
   #undef  HB_OS_HAS_DRIVE_LETTER
   #define HB_OS_EOL_LEN                1
   #define HB_OS_OPT_DELIM_LIST         "-"
   #define HB_ISOPTSEP( c )             ( ( c ) == '-' )
#else
   /* we are assuming here the DOS compatible OS */
   #define HB_OS_PATH_LIST_SEP_CHR      ';'
   #define HB_OS_PATH_DELIM_CHR         '\\'
   #define HB_OS_PATH_DELIM_CHR_STRING  "\\"
   #define HB_OS_PATH_DELIM_CHR_LIST    "\\/:"
   #define HB_OS_ALLFILE_MASK           "*.*"
   #define HB_OS_DRIVE_DELIM_CHR        ':'
   #if defined( HB_OS_WIN_CE )
      #undef  HB_OS_HAS_DRIVE_LETTER
   #else
      #define HB_OS_HAS_DRIVE_LETTER
   #endif
   #define HB_OS_EOL_LEN                2  /* # of bytes in End of Line marker */
   #define HB_OS_OPT_DELIM_LIST         "/-"
   #define HB_ISOPTSEP( c )             ( ( c ) == '-' || ( c ) == '/' )
#endif

#define HB_PATH_MAX     264 /* with trailing 0 byte */

/* ***********************************************************************
 * Here you can force the EOL string to be CRLF
 *
 * By default, the EOL string depends upon the detected platform.
 */
/* #define HB_EOL_CRLF */
#ifdef HB_EOL_CRLF
   #undef HB_OS_EOL_LEN
   #define HB_OS_EOL_LEN 2
#endif

/* ***********************************************************************
 * See also the following files for task specific definitions/settings
 *
 * hbmather.h    - math errors handling
 */

/* ***********************************************************************
 * Extern "C" detection
 */

#if defined( __cplusplus ) && !defined( __IBMCPP__ )
   #define HB_EXTERN_C        extern "C"
   #define HB_EXTERN_BEGIN    extern "C" {
   #define HB_EXTERN_END      }
#else
   #define HB_EXTERN_C
   #define HB_EXTERN_BEGIN
   #define HB_EXTERN_END
#endif

#if defined( __GNUC__ ) && ( __GNUC__ - 0 >= 3 )

   #define HB_DEPRECATED __attribute__ (( __deprecated__ ))

   #define HB_PRINTF_FORMAT( _nStr, _nParam ) \
                     __attribute__ (( format (printf, _nStr, _nParam)))
   #define HB_MALLOC_ATTR \
                     __attribute__ (( malloc ))
   #define HB_PURE_ATTR \
                     __attribute__ (( pure ))
   #define HB_CONST_ATTR \
                     __attribute__ (( const ))
#  if 0
   #define HB_NORETURN_ATTR \
                     __attribute__ (( noreturn ))
#  else
   #define HB_NORETURN_ATTR
#  endif
#  if ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 1 ) ) && \
      !defined( __ICC ) && !defined( __clang__ ) && \
      !defined( __PCC__ ) && \
      !defined( HB_OS_ANDROID ) && \
      !defined( HB_NO_FLATTEN )
   #define HB_FLATTEN_ATTR \
                     __attribute__ (( flatten ))
#  else
   #define HB_FLATTEN_ATTR
#  endif
#  if ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) ) && \
      !defined( __ICC ) && !defined( __OPENCC__ )
   #define HB_ALLOC_SIZE_ATTR( _nParam ) \
                     __attribute__ (( alloc_size (_nParam)))
   #define HB_HOT_ATTR \
                     __attribute__ (( hot ))
   #define HB_COLD_ATTR \
                     __attribute__ (( cold ))
#  else
   #define HB_ALLOC_SIZE_ATTR( _nParam )
   #define HB_HOT_ATTR
   #define HB_COLD_ATTR
#  endif
   #define HB_RESTRICT  __restrict

#else
   #define HB_DEPRECATED
   #define HB_PRINTF_FORMAT( _nStr, _nParam )
   #define HB_MALLOC_ATTR
   #define HB_NORETURN_ATTR
   #define HB_HOT_ATTR
   #define HB_COLD_ATTR
   #define HB_PURE_ATTR
   #define HB_CONST_ATTR
   #define HB_FLATTEN_ATTR
   #define HB_ALLOC_SIZE_ATTR( _nParam )
   #define HB_RESTRICT
#endif

#if defined( __GNUC__ ) || defined( __SUNPRO_C )
   #define _HB_INLINE_  __inline__
#elif defined( __BORLANDC__ ) || defined( _MSC_VER ) || \
      defined( __WATCOMC__ ) || defined( __POCC__ ) || defined( __XCC__ ) || \
      defined( __LCC__ ) || defined( __DMC__ )
   #define _HB_INLINE_  __inline
#else /* __cplusplus */
   #define _HB_INLINE_  inline
#endif

#if defined( __GNUC__ ) && \
    ( ( __GNUC__ > 3 ) || ( ( __GNUC__ == 3 ) && ( __GNUC_MINOR__ >= 2 ) ) )
   #define HB_FORCEINLINE     __inline__ __attribute__((always_inline))
#elif ( defined( _MSC_VER ) && ( _MSC_VER >= 1200 ) ) || \
      ( defined( __POCC__ ) && ( __POCC__ >= 300 ) )
   #define HB_FORCEINLINE     __forceinline
#elif defined( FORCEINLINE )
   #define HB_FORCEINLINE     FORCEINLINE
#else
   #define HB_FORCEINLINE     _HB_INLINE_
#endif

#endif /* HB_SETUP_H_ */
