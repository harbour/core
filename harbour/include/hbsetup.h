/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for compiler and runtime configuration
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#define HB_PCODE_VER          0x0002
#define HB_PCODE_VER_MIN      0x0002

/* ***********************************************************************
 * NOTE: You can select the default language modul used by Harbour, by
 *       defining this to a valid language modul identifier.
 */

#ifndef HB_LANG_DEFAULT
   #define HB_LANG_DEFAULT      EN
#endif

/* ***********************************************************************
 * If you turn this on, the memory subsystem will collect trace information
 * and several statistical data about memory management, it will show
 * these on exit if memory seem to have been leaked. The memory subsystem
 * will also do pointer checks. [vszakats]
 * This should be normally turned off in a final release.
 *
 * Note that if you turn this on, Harbour will be slighlty slower, larger
 * and will consume more memory.
 *
 * By default this is turned on. Define HB_FM_STATISTICS_OFF to turn it off.
 */
#ifndef HB_FM_STATISTICS_OFF
   #define HB_FM_STATISTICS
#endif

/* ***********************************************************************
 * Enable profiler support in HVM
 * By default this is turned off. Define HB_USE_PROFILER to turn it on.
 */

#ifndef HB_USE_PROFILER
   #define HB_NO_PROFILER
#endif

/* ***********************************************************************
 * Use system PCRE library instead of build in. I'm setting it sometimes
 * when I want to use exactly the same PCRE version as installed in     
 * a system.                                                            
 */
/* #define HB_PCRE_REGEX */

/* ***********************************************************************
 * Use old PCRE library which is part of BCC CRTL used to emulate 
 * POSIX regex.
 */
/* #define HB_PCRE_REGEX_BCC */

/* ***********************************************************************
 * Use POSIX regex library which should be part of each POSIX compatible
 * system / C compiler I also use this when I want to support exactly   
 * the same regular expressions as other tools installed in the system  
 */
/* #define HB_POSIX_REGEX */

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
 * Use native Windows memory allocation functions (HB_OS_WIN_32)
 * This option can disable compiler memory allocation optimization
 * so you should really have a good reason to enable it
 */

/* #define HB_FM_WIN32_ALLOC */

/* ***********************************************************************
 * You can select here, what type of main entry will be used in the
 * application (main() or WinMain()).
 *
 * By default the standard C main() function will be used.
 */
/* #define HB_MAIN_STD */
/* #define HB_MAIN_WIN */

/* ***********************************************************************
 * You can set here the maximum symbol name length handled by Harbour
 * compiler and runtime. You can override this setting in the make process.
 * Setting this too low, may result in clashing symbols in Harbour 
 * itself. For strict CA-Cl*pper compatibility a value of 10 would have 
 * to be used, but Harbour won't properly compile using this setting.
 *
 * By default this value is 63
 */
#ifndef HB_SYMBOL_NAME_LEN
   #define HB_SYMBOL_NAME_LEN   63
#endif

/* ***********************************************************************
 * You can enable here support for multiline strings concatenated by ';'
 * f.e.:
 *       cVar := "line 1;
 *       line 2;
 *       line 3"
 * It's not Clipper compatible extension and it's not enabled by default
 * anyhow old Harbour PP worked in such way and people may have some code
 * which needs it
 */
/* #define HB_PP_MULTILINE_STRING */

/* ***********************************************************************
 * Operating system specific definitions
 */
#if ( defined(__GNUC__) && \
      ! ( defined(__DJGPP__) || defined(__EMX__) || defined(__RSXNT__) || \
          defined(_Windows) || defined(_WIN32) || defined(_WINCE) ) ) || \
    ( defined(__WATCOMC__) && defined(__LINUX__) )
   #define HB_OS_UNIX_COMPATIBLE
   #define HB_OS_PATH_LIST_SEP_CHR      ':'
   #define HB_OS_PATH_DELIM_CHR         '/'
   #define HB_OS_PATH_DELIM_CHR_STRING  "/"
   #define HB_OS_PATH_DELIM_CHR_LIST    "/"
   #define HB_OS_ALLFILE_MASK           "*"
   #undef  HB_OS_DRIVE_DELIM_CHR
   #undef  HB_OS_HAS_DRIVE_LETTER
   #define HB_OS_OPT_DELIM_LIST         "-"
   #define HB_OS_EOL_LEN                1
#else
   /* we are assuming here the DOS compatible OS */
   #define HB_OS_PATH_LIST_SEP_CHR      ';'
   #define HB_OS_PATH_DELIM_CHR         '\\'
   #define HB_OS_PATH_DELIM_CHR_STRING  "\\"
   #define HB_OS_PATH_DELIM_CHR_LIST    "\\/:"
   #define HB_OS_ALLFILE_MASK           "*.*"
   #define HB_OS_DRIVE_DELIM_CHR        ':'
   #define HB_OS_HAS_DRIVE_LETTER
   #define HB_OS_OPT_DELIM_LIST         "/-"
   #define HB_OS_EOL_LEN                2  /* # of bytes in End of Line marker */
#endif

#ifndef _POSIX_PATH_MAX
   #define _POSIX_PATH_MAX    255
#endif

#define HB_ISOPTSEP( c ) ( strchr( HB_OS_OPT_DELIM_LIST, ( c ) ) != NULL )

/* Compatibility #defines. These will be removed, so 
   please use the new names in your code. */
#ifdef HB_LEGACY_LEVEL
   #define OS_UNIX_COMPATIBLE           HB_OS_UNIX_COMPATIBLE
   #define OS_PATH_DELIMITER            HB_OS_PATH_DELIM_CHR
#endif

/* ***********************************************************************
 * Platform detection
 */

#if defined(__WATCOMC__)
   #if defined(__OS2__)
      #define HB_OS_OS2
   #elif defined(__NT__) || defined(__WINDOWS_386__) || defined(__WINDOWS__)
      #define HB_OS_WIN_32
   #elif defined(__LINUX__)
      #define HB_OS_LINUX
   #elif defined(__386__)
      #define HB_OS_DOS
      #define HB_OS_DOS_32
   #else
      #define HB_OS_DOS
      #define HB_OS_DOS_16
   #endif
#endif

#ifndef HB_OS_DOS
   #if defined(DOS) || defined(_QC) || defined(__DOS__) || defined(MSDOS) || defined(__MSDOS__) || defined(__RSX32__)
      #define HB_OS_DOS
      #if defined(__386__) || defined(__DJGPP__)
         #define HB_OS_DOS_32
      #else
         #define HB_OS_DOS_16
      #endif
   #endif
#endif

#if defined(__EMX__) && ! defined(__RSXNT__)
   #define HB_OS_OS2_GCC
#endif
#ifndef HB_OS_OS2
   #if defined(OS2) || defined(__OS2__) || defined(OS_2) || defined(HB_OS_OS2_GCC)
      #define HB_OS_OS2
   #endif
#endif

#ifndef HB_OS_WIN_32
   #if defined(WINNT) || defined(_Windows) || defined(__NT__) || defined(_WIN32) || defined(_WINDOWS_) || defined(__WINDOWS_386__) || defined(__WIN32__) || defined(__CYGWIN__)
      #define HB_OS_WIN_32
   #endif
#endif

#ifndef HB_OS_WIN_64
   #if defined(_WIN64)
      #define HB_OS_WIN_64
   #endif
#endif

#if !defined(HB_WINCE) && \
    ( defined(_WINCE) || defined(__CEGCC__) || defined(__MINGW32CE__) )
   #define HB_WINCE
#endif

#ifndef HB_OS_LINUX
   #if defined(linux) || defined(__linux) || defined(__linux__) || defined(__gnu_linux__)
      #define HB_OS_LINUX
   #endif
#endif

#ifndef HB_OS_SUNOS
   #if defined(sun) || defined(__sun) || defined(__sun__)
      #define HB_OS_SUNOS
   #endif
#endif

#ifndef HB_OS_HPUX
   /* HP cc in ANSI mode defines __hpux. GCC defines __hpux__ */
   #if defined(__hpux) || defined(__hpux__)
      #define HB_OS_HPUX
   #endif
#endif

#ifndef HB_OS_MAC
   #if defined(__MPW__)
      #define HB_OS_MAC
   #endif
#endif

#ifndef HB_OS_DARWIN
   #if defined(__APPLE__)
      #define HB_OS_DARWIN
   #endif
#endif

#ifndef HB_OS_BSD
   #if defined( __FreeBSD__ ) || defined( __NetBSD__ ) || defined( __OpenBSD__ ) || \
       defined( HB_OS_DARWIN )
      #define HB_OS_BSD
   #endif
#endif

#ifndef HB_OS_UNIX
   #if defined(HB_OS_UNIX_COMPATIBLE) || \
       defined(HB_OS_LINUX) || \
       defined(HB_OS_DARWIN) || \
       defined(HB_OS_BSD) || \
       defined(HB_OS_SUNOS) || \
       defined(HB_OS_HPUX)
      #define HB_OS_UNIX
   #endif
#endif

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
 * hbmath.h    - math errors handling
 */

/* ***********************************************************************
 * some fixes in compiler header files
 */

#if defined( __DJGPP__ )
   /* Fix DJGPP in call to: toupper(), tolower(), is...()
    */
   #include "hbfixdj.h"

   /* Substitute snprintf() by sprintf() for DJGPP <= 2.03.
    * This is a temporary hack, should implement a C99 snprintf() ourselves.
    */
   #if ( __DJGPP__ < 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ <= 3 ) )
      #define snprintf(s, len, args...) sprintf( (s), ##args )
   #endif
#elif defined( _MSC_VER ) || defined( __DMC__ ) && !defined( __XCC__ )
   #define snprintf _snprintf
#elif defined( __WATCOMC__ )
   #if __WATCOMC__ < 1200
      #define snprintf _bprintf
   #endif
#endif

/* ***********************************************************************
 * Extern "C" detection
 */

#if defined(__cplusplus) && !defined(__IBMCPP__)
   #define HB_EXTERN_C
   #define HB_EXTERN_BEGIN    extern "C" {
   #define HB_EXTERN_END      }
#else
   #define HB_EXTERN_BEGIN
   #define HB_EXTERN_END
#endif

#endif /* HB_SETUP_H_ */
