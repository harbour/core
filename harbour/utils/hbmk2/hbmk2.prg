/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Make (alias hbmk/hbmk2)
 *
 * Copyright 1999-2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 * License extensions:
 *   - This source code must be kept and distributed as part
 *     of the Harbour package and/or the placement of the tool sources
 *     and files must reflect that it is part of Harbour Project.
 *   - Copyright information must always be presented by
 *     projects including this tool or help text.
 *   - Modified versions of the tool must clearly state this
 *     fact on the copyright screen.
 *   - Source code modifications shall always be made available
 *     along with binaries.
 *   - Help text and documentation is licensed under
 *     Creative Commons Attribution-ShareAlike 3.0:
 *     http://creativecommons.org/licenses/by-sa/3.0/
 *
 */

/*
 * Copyright 2003-2007 Przemyslaw Czerpak <druzus@priv.onet.pl>
 *   - gcc and *nix configuration elements
 *   - bash script with similar purpose for gcc family
 *   - entry point override method and detection code for gcc
 *   - RTLink/Blinker/ExoSpace link script parsers
 *   - original POTMerge(), LoadPOTFilesAsHash(), GenHBL() and AutoTrans()
 *   - optimized header time scan algorithm
 *   - shell core runner logic
 *
 * See above for licensing terms.
 *
 */

/* Keeping it tidy */
#pragma -w3
#pragma -es2

/* Optimizations */
#pragma -km+
#pragma -ko+

/*
   Program Library HOWTO:
      http://www.linux.org/docs/ldp/howto/Program-Library-HOWTO/index.html

   Markdown syntax:
      http://daringfireball.net/projects/markdown/syntax

   Markdown to man page converter:
      https://github.com/sunaku/md2man
   Requires Ruby. Install with:
      gem install md2man
   Convert with:
      md2man man.md > man.1
      (man.md should come out from this executable as output, so
      the manual does not have to be updated in two disctinct places)

   Man page HOWTO:
      http://www.schweikhardt.net/man_page_howto.html
   Groff manual:
      http://www.gnu.org/software/groff/manual/html_node/index.html
      http://www.gnu.org/software/groff/manual/groff.pdf
   Troff manual:
      http://cm.bell-labs.com/sys/doc/troff.pdf
 */

/* needed for -u */
#ifndef HB_LEGACY_LEVEL4
#define HB_LEGACY_LEVEL4
#endif

#include "directry.ch"
#include "error.ch"
#include "fileio.ch"
#include "set.ch" /* needed for -u */
#include "simpleio.ch" /* Do not delete this, it is useful for development. */

#include "hbgtinfo.ch"
#include "hbhrb.ch"
#include "hbver.ch"

/* NOTE: Keep this code clean from any kind of contribs and Harbour level
         3rd party library/tool information. This component shall only
         contain hard-wired knowledge on Harbour _core_ (official interfaces
         preferred), C compilers and OS details on the smallest possible level.
         Instead, 3rd party Harbour packages are recommended to maintain
         and provide .hbc files themselves, as part of their standard
         distribution packages. You can find a few such .hbc examples in
         the 'extras' directory. For Harbour contribs, the recommended
         method is to supply and maintain .hbc files in their respective
         directories, usually under tests (or utils, examples). As of this
         writing, most of them has one created.
         Thank you. [vszakats] */

/* TODOs:
   - Support debug/release modes. Some default setting can be set
     accordingly, and user can use it to further tweak settings.
   - Further clean hbmk context var usage (global scope, project scope,
     adding rest of variables).
   - Add a way to fallback to stop if required headers could not be found.
     This needs a way to spec what key headers to look for.
   - Consider renaming the tool to simply 'hb'.
   - Turn off lib grouping by default
   - Avoid adding certain options and input files twice
   - Clean up compiler autodetection and add those few feature only
     found in GNU Make / global.mk, like *nix native autodetection,
     autodetection of watcom cross-build setups, poccarm/pocc64 setups,
     clang, etc.
   - Next gen compiler autodetection:
     1. Gather supported compilers by Harbour installation
        (look for lib/<plat>/*[/<name>] subdirs)
        Show error if nothing is found
     2. Look if any supported compilers are found embedded, in PATH
        or on HB_CCPATH for target <plat>.
        Show error if nothing is found
     3. If HB_COMPILER is set to one of them, select it.
        (TODO: handle multiple installations of the same compiler.
        F.e. embedded mingw and one in PATH, or two versions of MSVC)
     4. If HB_COMPILER is set, but not to one of them, show warning and
        use the highest one on the priority list.
     5. If HB_COMPILER is not set,
        use the highest one on the priority list.
     NOTES: - Priority list: HB_CCPATH, PATH, embedded.
            - Priority list: mingw, msvc, bcc, watcom, pocc, xcc
            - Compilers of native CPU target have higher priority. (extra)
              On x64 Windows: msvc64, msvc, msvcia64, mingw64, mingw, ...
              On x86 Windows: msvc, msvc64, msvcia64, mingw, mingw64, ...
              On IA64 Windows: msvcia64, msvc, msvc64, mingw, mingw64, ...
 */

#ifndef _HBMK_EMBEDDED_

#include "hbextcdp.ch"
#include "hbextlng.ch"

/* For -u support we use 'WHILE' instead of 'DO WHILE',
   'EXTERNAL' instead of 'REQUEST'
   'END' instead of 'END SEQUENCE' */

ANNOUNCE HB_GTSYS
EXTERNAL HB_GT_CGI_DEFAULT
#define _HBMK_GT_DEF_ "GTCGI"

/* Include these for -pause support. */
#if   defined( __PLATFORM__WINCE )
   EXTERNAL HB_GT_WVT
#elif defined( __PLATFORM__WINDOWS )
   EXTERNAL HB_GT_WIN
   EXTERNAL HB_GT_WVT
#elif defined( __PLATFORM__DOS )
   EXTERNAL HB_GT_DOS
#elif defined( __PLATFORM__OS2 )
   EXTERNAL HB_GT_OS2
#elif defined( __PLATFORM__UNIX ) .AND. ! defined( __PLATFORM__VXWORKS ) .AND. ! defined( __PLATFORM__SYMBIAN )
   EXTERNAL HB_GT_TRM
   #if defined( HBMK_WITH_GTXWC )
      EXTERNAL HB_GT_XWC
   #endif
#endif

#endif /* ! _HBMK_EMBEDDED_ */

EXTERNAL hbmk_KEYW

/* needed for -u */
#ifndef HB_SYMBOL_UNUSED
#define HB_SYMBOL_UNUSED( symbol )  ( ( symbol ) )
#endif

#define _SELF_NAME_             "hbmk2"

#define I_( x )                 hb_UTF8ToStr( hb_i18n_gettext( x ) )
#define R_( x )                 ( x ) /* marking for regexps */

#define _TARG_PLAT              1
#define _TARG_COMP              2
#define _TARG_CPU               3

#define _PAR_NEW( cParam, cFileName, nLine ) { cParam, cFileName, nLine }
#define _PAR_cParam             1
#define _PAR_cFileName          2
#define _PAR_nLine              3

#define _WARN_DEF               0 /* Do not set any explicit warning level */
#define _WARN_MAX               1
#define _WARN_YES               2 /* Default level in Harbour build */
#define _WARN_LOW               3 /* Low level, used for 3rd party code in Harbour build */
#define _WARN_NO                4 /* Explicitly disable warnings */

#define _COMPR_OFF              0
#define _COMPR_DEF              1
#define _COMPR_MIN              2
#define _COMPR_MAX              3

#define _HEAD_OFF               0
#define _HEAD_FULL              1
#define _HEAD_NATIVE            2
#define _HEAD_DEP               3

#define _COMPDET_bBlock         1
#define _COMPDET_cCOMP          2
#define _COMPDET_cCCPREFIX      3 /* optional */
#define _COMPDET_cCCSUFFIX      4 /* optional */
#define _COMPDET_cPLAT          5 /* optional */

#define _COMPDETE_bBlock        1
#define _COMPDETE_cPLAT         2
#define _COMPDETE_cCOMP         3
#define _COMPDETE_cCCPREFIX     4
#define _COMPDETE_cCCPATH       5
#define _COMPDETE_bSetup        6

#define _HBMODE_NATIVE          (  0xFFFFFF )
#define _HBMODE_HB10            (  0x010000 )
#define _HBMODE_HB20            (  0x020000 )
#define _HBMODE_HB30            (  0x030000 )
#define _HBMODE_XHB             ( -0x010200 )
#define _HBMODE_RAW_C           ( -1 )

#define _HBMODE_IS_HB( n )      ( n >= _HBMODE_HB10 )
#define _HBMODE_IS_OLDHB( n )   ( n >= _HBMODE_HB10 .AND. n < _HBMODE_NATIVE )
#define _HBMODE_IS_XHB( n )     ( n <= _HBMODE_XHB )

/* Not implemented yet */
#define _CONF_RELEASE           0 /* No debug */
#define _CONF_DEBUG             1 /* Harbour level debug */
#define _CONF_FULLDEBUG         2 /* Harbour + C level debug */

#define _ESC_NONE               0
#define _ESC_DBLQUOTE           1
#define _ESC_SGLQUOTE_WATCOM    2
#define _ESC_NIX                3
#define _ESC_BCKSLASH           4

#define _FNF_BCKSLASH           0
#define _FNF_FWDSLASH           1
#define _FNF_FWDSLASHCYGWIN     2
#define _FNF_FWDSLASHMSYS       3

#define _MACRO_NORM_PREFIX      "$"
#define _MACRO_LATE_PREFIX      "%"
#define _MACRO_PREFIX_ALL       ( _MACRO_NORM_PREFIX + _MACRO_LATE_PREFIX )
#define _MACRO_OPEN             "{"
#define _MACRO_CLOSE            "}"

#define _CMDSUBST_OPEN          "`"
#define _CMDSUBST_CLOSE         _CMDSUBST_OPEN

#define _LNG_MARKER             ( _MACRO_LATE_PREFIX + _MACRO_OPEN + "hb_lng" + _MACRO_CLOSE )

#define _HBMK_ENV_INSTALL_PFX   "HB_INSTALL_PREFIX"
#define _HBMK_ENV_NAME          "HBMK_OPTIONS"
#define _HBMK_AUTOHBC_NAME      "hbmk.hbc"
#define _HBMK_AUTOHBM_NAME      "hbmk.hbm"

#define _HBMK_SPECDIR_COMP      "comp"
#define _HBMK_SPECDIR_CONTRIB   "contrib"
#define _HBMK_SPECDIR_ADDONS    "addons"

#define _HBMK_SIGN_TIMEURL      "http://timestamp.verisign.com/scripts/timstamp.dll"

#define _HBMK_HBEXTREQ          "__HBEXTREQ__"
#define _HBMK_WITH_TPL          "HBMK_WITH_%1$s"
#define _HBMK_HAS_TPL           "HBMK_HAS_%1$s"
#define _HBMK_HAS_TPL_LOCAL     "HBMK_HAS_%1$s_LOCAL"
#define _HBMK_HAS_TPL_HBC       "HBMK_HAS_%1$s"
#define _HBMK_DIR_TPL           "HBMK_DIR_%1$s"
#define _HBMK_PLUGIN            "__HBSCRIPT__HBMK_PLUGIN"
#define _HBMK_SHELL             "__HBSCRIPT__HBSHELL"

#define _HBMK_IMPLIB_EXE_POST   "_exe"
#define _HBMK_IMPLIB_DLL_POST   "_dll"

#define _HBMK_TARGENAME_ADHOC   ".adhoc."

#define _HBMK_REGEX_INCLUDE     R_( '(?:^|;)[ \t]*#[ \t]*(?:incl|inclu|includ|include|import)[ \t]*(\".+?\"' + "|<.+?>|['`].+?')" )
#define _HBMK_REGEX_REQUIRE     R_( '(?:^|;)[ \t]*#[ \t]*require[ \t]*(\".+?\"' + "|'.+?')" )

#define _HBMK_NEST_MAX          10
#define _HBMK_HEAD_NEST_MAX     10

#define _VAR_MODE_SET           1
#define _VAR_MODE_APPEND        2
#define _VAR_MODE_INSERT        3
#define _VAR_MODE_DELETE        4

#if defined( __PLATFORM__WINDOWS )
   #define _OSCONFDIR_ENV_         "APPDATA"
#else
   #define _OSCONFDIR_ENV_         "HOME"
#endif

#if defined( __PLATFORM__DOS )
   #define _CONFDIR_BASE_          "~harbour"
   #define _WORKDIR_BASE_          "~hbmk"
#else
   #define _CONFDIR_BASE_          ".harbour"
   #define _WORKDIR_BASE_          ".hbmk"
#endif
#define _WORKDIR_DEF_           ( _WORKDIR_BASE_ + hb_ps() + hbmk[ _HBMK_cPLAT ] + hb_ps() + hbmk[ _HBMK_cCOMP ] )

#define _BCC_BIN_DETECT()       FindInPath( "bcc32.exe" )

#define HB_ISALPHA( c )         hb_asciiIsAlpha( c )
#define HB_ISFIRSTIDCHAR( c )   ( HB_ISALPHA( c ) .OR. ( c ) == "_" )
#define HB_ISNEXTIDCHAR( c )    ( HB_ISFIRSTIDCHAR(c) .OR. hb_asciiIsDigit( c ) )

#define LEFTEQUAL( l, r )       ( Left( l, Len( r ) ) == r )

/* Logic (hack) to automatically add some libs to their
   right place in the liblist. In case of 'unicows' lib,
   this should be after all app lib and before any Windows
   system libs. [vszakats] */
#define _IS_AUTOLIBSYSPRE( c )  ( hbmk[ _HBMK_cPLAT ] == "win" .AND. Lower( hb_FNameName( c ) ) == "unicows" )

#define _OUT_EOL                e"\n"     /* used when displaying text */
#define _CHR_EOL                Chr( 10 ) /* used when consuming text files */
#define _FIL_EOL                Chr( 10 ) /* used when creating source files */

#ifdef HB_LEGACY_LEVEL4
   #define _HBMK_PLUGIN_APIVER  2
#else
   #define _HBMK_PLUGIN_APIVER  3
#endif

#define _HBMK_IMPLIB_NOTFOUND   -1
#define _HBMK_IMPLIB_OK         0
#define _HBMK_IMPLIB_FAILED     1

#define _CCOMP_PASS_C           1
#define _CCOMP_PASS_CPP         2

#define _HBMK_lQuiet            1
#define _HBMK_lInfo             2
#define _HBMK_nMaxCol           3
#define _HBMK_cPLAT             4
#define _HBMK_cCOMP             5
#define _HBMK_cCPU              6
#define _HBMK_cBUILD            7
#define _HBMK_cGTDEFAULT        8
#define _HBMK_aLIBCOREGT        9
#define _HBMK_aLIBCOREGTDEF     10
#define _HBMK_cGT               11

#define _HBMK_cHB_INSTALL_PFX   12
#define _HBMK_cHB_INSTALL_BIN   13
#define _HBMK_cHB_INSTALL_LIB   14
#define _HBMK_cHB_INSTALL_LI3   15
#define _HBMK_cHB_INSTALL_DYN   16
#define _HBMK_cHB_INSTALL_INC   17
#define _HBMK_cHB_INSTALL_ADD   18
#define _HBMK_cHB_INSTALL_CON   19

#define _HBMK_lGUI              20
#define _HBMK_lMT               21
#define _HBMK_lPIC              22
#define _HBMK_lDEBUG            23
#define _HBMK_nHEAD             24
#define _HBMK_aINCPATH          25
#define _HBMK_lREBUILD          26
#define _HBMK_lCLEAN            27
#define _HBMK_lTRACE            28
#define _HBMK_lDONTEXEC         29
#define _HBMK_nHBMODE           30
#define _HBMK_cUILNG            31
#define _HBMK_aLIBUSER          32
#define _HBMK_aLIBUSERFWK       33
#define _HBMK_aLIBUSERGT        34
#define _HBMK_aLIBUSERSYS       35
#define _HBMK_aLIBUSERSYSPRE    36
#define _HBMK_aLIBFILTEROUT     37
#define _HBMK_aLIBPATH          38
#define _HBMK_aINSTPATH         39
#define _HBMK_aOPTC             40
#define _HBMK_aOPTCUSER         41
#define _HBMK_aOPTCX            42
#define _HBMK_aOPTCPPX          43
#define _HBMK_aOPTPRG           44
#define _HBMK_aOPTRES           45
#define _HBMK_aOPTL             46
#define _HBMK_aOPTLPOST         47
#define _HBMK_aOPTA             48
#define _HBMK_aOPTD             49
#define _HBMK_aOPTDPOST         50
#define _HBMK_aOPTI             51
#define _HBMK_aOPTS             52
#define _HBMK_lCPP              53
#define _HBMK_lSHARED           54
#define _HBMK_lSTATICFULL       55
#define _HBMK_lSHAREDDIST       56
#define _HBMK_lNULRDD           57
#define _HBMK_lMAP              58
#define _HBMK_lBEEP             59
#define _HBMK_lSTRIP            60
#define _HBMK_lOPTIM            61
#define _HBMK_nCOMPR            62
#define _HBMK_nWARN             63
#define _HBMK_lSAFE             64
#define _HBMK_lRUN              65
#define _HBMK_lINC              66
#define _HBMK_lREBUILDPO        67
#define _HBMK_lMINIPO           68
#define _HBMK_lWINUNI           69
#define _HBMK_nCONF             70
#define _HBMK_lIGNOREERROR      71
#define _HBMK_lIMPLIB           72
#define _HBMK_lHBCPPMM          73
#define _HBMK_hDEP              74

#define _HBMK_lCreateLib        75
#define _HBMK_lCreateDyn        76
#define _HBMK_lCreateImpLib     77
#define _HBMK_lCreatePPO        78
#define _HBMK_lCreateHRB        79

#define _HBMK_lDynVM            80

#define _HBMK_lBLDFLGP          81
#define _HBMK_lBLDFLGC          82
#define _HBMK_lBLDFLGL          83

#define _HBMK_cFIRST            84
#define _HBMK_aPRG              85
#define _HBMK_aCH               86
#define _HBMK_aC                87
#define _HBMK_aCPP              88
#define _HBMK_aRESSRC           89
#define _HBMK_aRESCMP           90
#define _HBMK_aOBJUSER          91
#define _HBMK_aICON             92
#define _HBMK_cMANIFEST         93
#define _HBMK_aIMPLIBSRC        94
#define _HBMK_aDEF              95
#define _HBMK_aINSTFILE         96
#define _HBMK_hDEPTS            97
#define _HBMK_aREQUEST          98

#define _HBMK_aPO               99
#define _HBMK_cHBL              100
#define _HBMK_cHBLDir           101
#define _HBMK_aLNG              102
#define _HBMK_cPO               103

#define _HBMK_hPLUGINHRB        104
#define _HBMK_hPLUGINVars       105
#define _HBMK_aPLUGINPars       106
#define _HBMK_hPLUGINExt        107

#define _HBMK_lDEBUGTIME        108
#define _HBMK_lDEBUGINC         109
#define _HBMK_lDEBUGSTUB        110
#define _HBMK_lDEBUGI18N        111
#define _HBMK_lDEBUGDEPD        112
#define _HBMK_lDEBUGPARS        113

#define _HBMK_cCCPATH           114
#define _HBMK_cCCPREFIX         115
#define _HBMK_cCCSUFFIX         116
#define _HBMK_cCCEXT            117

#define _HBMK_cWorkDir          118
#define _HBMK_cWorkDirDynSub    119
#define _HBMK_nCmd_Esc          120
#define _HBMK_nScr_Esc          121
#define _HBMK_nCmd_FNF          122
#define _HBMK_nScr_FNF          123
#define _HBMK_nExitCode         124

#define _HBMK_cPROGDIR          125
#define _HBMK_cPROGNAME         126

#define _HBMK_hAUTOHBC          127 /* trigger header => .hbc associations */
#define _HBMK_hAUTOHBCFOUND     128 /* trigger headers found */

#define _HBMK_aDEPTHBC          129 /* .hbc references found */
#define _HBMK_hDEPTSDIR         130 /* Header dirs found for dependencies */

#define _HBMK_lStopAfterInit    131
#define _HBMK_lStopAfterHarbour 132

#define _HBMK_nCOMPVer          133
#define _HBMK_lDEPIMPLIB        134 /* Generate import libs configured in dependecy specification */
#define _HBMK_lInstForce        135 /* Force to install target even if was up to date */
#define _HBMK_lAutoHBM          136 /* Toggles processing of hbmk.hbm file in current directory */
#define _HBMK_lContainer        137 /* Target type: container */
#define _HBMK_lShowLevel        138 /* Show project nesting level in all output lines */
#define _HBMK_hFiles            139 /* Cache for the header parser (common for C and Harbour) */
#define _HBMK_cDynLibPrefix     140 /* Dynamic lib filename prefix */
#define _HBMK_cDynLibExt        141 /* Dynamic lib filename extension */
#define _HBMK_aLINK             142 /* Links to be created and pointing to the target */
#define _HBMK_hDEPTMACRO        143 /* Links to be created and pointing to the target */
#define _HBMK_cC                144 /* C dialect */
#define _HBMK_cCPP              145 /* C++ dialect */
#define _HBMK_aLIB_BASE_WARN    146

#define _HBMK_aArgs             147
#define _HBMK_nArgTarget        148
#define _HBMK_lPause            149
#define _HBMK_nLevel            150

#define _HBMK_cHBX              151

#define _HBMK_aGT               152
#define _HBMK_cCPPRG            153

#define _HBMK_lSysLoc           154
#define _HBMK_lDumpInfo         155
#define _HBMK_lMarkdown         156
#define _HBMK_lShellMode        157

#define _HBMK_MAX_              157

#define _HBMK_DEP_CTRL_MARKER   ".control." /* must be an invalid path */

#define _HBMKDEP_cName          1
#define _HBMKDEP_aPKG           2
#define _HBMKDEP_aKeyHeader     3
#define _HBMKDEP_cControl       4
#define _HBMKDEP_lOptional      5
#define _HBMKDEP_cINCROOT       6
#define _HBMKDEP_aINCPATH       7
#define _HBMKDEP_aINCPATHLOCAL  8
#define _HBMKDEP_aIMPLIBSRC     9
#define _HBMKDEP_cIMPLIBDST     10
#define _HBMKDEP_cFound         11
#define _HBMKDEP_lFound         12
#define _HBMKDEP_lFoundLOCAL    13
#define _HBMKDEP_cVersion       14
#define _HBMKDEP_lForced        15
#define _HBMKDEP_lDetected      16
#define _HBMKDEP_MAX_           16

#define _EXIT_OK                0
#define _EXIT_UNKNPLAT          1
#define _EXIT_UNKNCOMP          2
#define _EXIT_FAILHBDETECT      3
#define _EXIT_STUBCREATE        5
#define _EXIT_PHASE_COMP        6
#define _EXIT_COMPPRG           _EXIT_PHASE_COMP
#define _EXIT_RUNRES            _EXIT_PHASE_COMP
#define _EXIT_COMPC             _EXIT_PHASE_COMP
#define _EXIT_PHASE_ASSEMBLY    7
#define _EXIT_RUNLINKER         _EXIT_PHASE_ASSEMBLY
#define _EXIT_RUNLIB            _EXIT_PHASE_ASSEMBLY
#define _EXIT_UNSUPPORTED       8
#define _EXIT_WORKDIRCREATE     9
#define _EXIT_HELP              19
#define _EXIT_MISSDEPT          10
#define _EXIT_PLUGINPREALL      20
#define _EXIT_DEEPPROJNESTING   30
#define _EXIT_STOP              50

#define HBMK_IS_IN( str, list ) ( "|" + str + "|" $ "|" + list + "|" )

#define HBMK_ISPLAT( list )     HBMK_IS_IN( hbmk[ _HBMK_cPLAT ], list )
#define HBMK_ISCOMP( list )     HBMK_IS_IN( hbmk[ _HBMK_cCOMP ], list )

#define PathMakeAbsolute( cPathR, cPathA ) hb_PathJoin( cPathA, cPathR )

#ifndef _HBMK_EMBEDDED_

/* Request for runner and shell */
EXTERNAL __HB_EXTERN__

/* Request some functions for plugins */
EXTERNAL HBClass
EXTERNAL __clsLockDef
EXTERNAL __hbdoc_LoadDir
EXTERNAL __hbdoc_ToSource
EXTERNAL __hbdoc_SaveHBD
EXTERNAL hb_regex
EXTERNAL hb_SHA1
EXTERNAL hb_SHA256
EXTERNAL hb_SHA512
EXTERNAL hb_MD5
EXTERNAL hb_CRC32
EXTERNAL hb_blowfishKey
EXTERNAL hb_blowfishEncrypt
EXTERNAL hb_jsonEncode
EXTERNAL hb_jsonDecode
EXTERNAL hb_libExt
EXTERNAL hb_HKeyAt
EXTERNAL hb_HDelAt
EXTERNAL hb_HKeys
EXTERNAL hb_HKeepOrder
EXTERNAL hb_FGetAttr
EXTERNAL hb_FSetAttr

/* For hbshell */

EXTERNAL __dbgEntry

#define HB_HISTORY_LEN          500
#define HB_LINE_LEN             256

#define _HBSH_cDirBase          1
#define _HBSH_cProgName         2
#define _HBSH_hLibExt           3
#define _HBSH_hCH               4
#define _HBSH_hOPTPRG           5
#define _HBSH_hINCPATH          6
#define _HBSH_hCHCORE           7
#define _HBSH_hbmk              8
#define _HBSH_nRow              9
#define _HBSH_nCol              10
#define _HBSH_aHistory          11
#define _HBSH_lPreserveHistory  12
#define _HBSH_lWasLoad          13
#define _HBSH_lInteractive      14
#define _HBSH_lClipperComp      15
#define _HBSH_MAX_              15

/* Trick to make it run if compiled without -n/-n1/-n2
   (or with -n-) option.
   (typically as scripts and precompiled scripts) */
/* NOTE: Avoid file wide STATICs to keep this working */
#if __pragma( n ) < 1
hbmk_local_entry( hb_ArrayToParams( hb_AParams() ) )
#endif

#if defined( HBMK_USE_CUSTMAIN )
/* for hbrun builds (or any builds via .hbp) */
PROCEDURE __hbmk_public_entry( ... )

   hbmk_local_entry( ... )

   RETURN
#elif defined( HBMK_USE_APPMAIN )
/* for GNU Make build (we cannot override default entry, so we use this alternate built-in one */
PROCEDURE _APPMAIN( ... )

   hbmk_local_entry( ... )

   RETURN
#else
/* for scripts and precompiled scripts with -n/-n1/-n2 option */
PROCEDURE __hbmk_fake_entry( ... )

   hbmk_local_entry( ... )

   RETURN
#endif

STATIC PROCEDURE hbmk_local_entry( ... )

   LOCAL aArgsProc
   LOCAL nResult
   LOCAL tmp, tmp1

   LOCAL lPause := hb_gtInfo( HB_GTI_ISGRAPHIC )
   LOCAL lExitStr := .F.

   LOCAL aArgsTarget
   LOCAL nTarget
   LOCAL nTargetTO_DO
   LOCAL nTargetPos
   LOCAL lHadTarget

   LOCAL cParam1L

   /* for temp debug messages */

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   /* Check if we should go into shell mode */

   cParam1L := iif( PCount() >= 1, Lower( hb_PValue( 1 ) ), "" )
   IF ( Right( Lower( hb_FNameName( hb_argv( 0 ) ) ), 5 ) == "hbrun" .OR. ;
        Left( Lower( hb_FNameName( hb_argv( 0 ) ) ), 5 ) == "hbrun" .OR. ;
        cParam1L == "." .OR. ;
        hb_FNameExt( cParam1L ) == ".dbf" .OR. ;
        ( HBMK_IS_IN( hb_FNameExt( cParam1L ), ".hb|.hrb" ) .AND. !( Left( cParam1L, 1 ) == "-" ) ) ) .AND. ;
      !( ! Empty( cParam1L ) .AND. ;
         ( Left( cParam1L, 6 ) == "-hbreg" .OR. ;
           Left( cParam1L, 8 ) == "-hbunreg" ) )
      __hbshell( ... )
      RETURN
   ENDIF

   #if ! __pragma( b )
      __vmNoInternals() /* disable access to VM internals */
   #endif

   /* Expand wildcard project specs */

   aArgsProc := {}
   FOR EACH tmp IN hb_AParams()
      DO CASE
      CASE !( Left( tmp, 1 ) $ "-@" ) .AND. Lower( hb_FNameExt( tmp ) ) == ".hbp"
         FOR EACH tmp1 IN FN_Expand( tmp, .T. )
            AAdd( aArgsProc, tmp1 )
         NEXT
      CASE Empty( hb_FNameExt( tmp ) ) .AND. hb_FileExists( hb_FNameExtSet( tmp, ".hbp" ) )
         AAdd( aArgsProc, hb_FNameExtSet( tmp, ".hbp" ) )
      CASE Lower( Left( tmp, Len( "-target=" ) ) ) == "-target="
         FOR EACH tmp1 IN FN_Expand( SubStr( tmp, Len( "-target=" ) + 1 ), .F. )
            AAdd( aArgsProc, "-target=" + tmp1 )
         NEXT
      OTHERWISE
         AAdd( aArgsProc, tmp )
      ENDCASE
   NEXT

   /* Emulate -hbcmp, -hbcc, -hblnk switches when certain
      self names are detected.
      For compatibility with hbmk script aliases. */

   IF ! Empty( aArgsProc )

      hb_FNameSplit( hb_argv( 0 ),, @tmp )

      tmp := Lower( tmp )

      IF Left( tmp, 1 ) == "x"
         tmp := SubStr( tmp, 2 )
         hb_AIns( aArgsProc, 1, "-xhb", .T. )
      ELSEIF Right( tmp, 2 ) == "10"
         hb_AIns( aArgsProc, 1, "-hb10", .T. )
      ELSEIF Right( tmp, 2 ) == "20"
         hb_AIns( aArgsProc, 1, "-hb20", .T. )
      ELSEIF Right( tmp, 2 ) == "30"
         hb_AIns( aArgsProc, 1, "-hb30", .T. )
      ENDIF

      DO CASE
      CASE tmp == "harbour"                ; hb_AIns( aArgsProc, 1, "-hbraw", .T. )
      CASE Right( tmp, 5 ) == "hbcmp" .OR. ;
           Left(  tmp, 5 ) == "hbcmp" .OR. ;
           tmp == "clipper"                ; hb_AIns( aArgsProc, 1, "-hbcmp", .T. )
      CASE Right( tmp, 4 ) == "hbcc" .OR. ;
           Left(  tmp, 4 ) == "hbcc"       ; hb_AIns( aArgsProc, 1, "-hbcc", .T. )
      CASE Right( tmp, 5 ) == "hblnk" .OR. ;
           Left(  tmp, 5 ) == "hblnk"      ; hb_AIns( aArgsProc, 1, "-hblnk", .T. )
      CASE tmp == "rtlink" .OR. ;
           tmp == "exospace" .OR. ;
           tmp == "blinker"                ; hb_AIns( aArgsProc, 1, "-rtlink", .T. )
      CASE Right( tmp, 5 ) == "hbexe" .OR. ;
           Left(  tmp, 5 ) == "hbexe"      ; AAdd( aArgsProc, "-hbexe" )
      CASE Right( tmp, 5 ) == "hblib" .OR. ;
           Left(  tmp, 5 ) == "hblib"      ; AAdd( aArgsProc, "-hblib" )
      CASE Right( tmp, 5 ) == "hbdyn" .OR. ;
           Left(  tmp, 5 ) == "hbdyn"      ; AAdd( aArgsProc, "-hbdyn" )
      ENDCASE
   ENDIF

   /* Handle multitarget command lines */

   hb_FSetDevMode( hb_gtInfo( HB_GTI_OUTPUTFD ), FD_TEXT )
   hb_FSetDevMode( hb_gtInfo( HB_GTI_ERRORFD ), FD_TEXT )

   nTargetTO_DO := 1
   WHILE .T.

      aArgsTarget := {}
      nTarget := 0
      nTargetPos := 0
      lHadTarget := .F.

      FOR EACH tmp IN aArgsProc
         DO CASE
         CASE !( Left( tmp, 1 ) $ "-@" ) .AND. ;
              Lower( hb_FNameExt( tmp ) ) == ".hbp" .AND. ;
              ! lHadTarget
            ++nTarget
            IF nTarget == nTargetTO_DO
               AAdd( aArgsTarget, tmp )
               nTargetPos := Len( aArgsTarget )
            ENDIF
         CASE Lower( Left( tmp, Len( "-target=" ) ) ) == "-target="
            ++nTarget
            IF nTarget == nTargetTO_DO
               AAdd( aArgsTarget, SubStr( tmp, Len( "-target=" ) + 1 ) )
               nTargetPos := Len( aArgsTarget )
            ENDIF
         OTHERWISE
            IF ! lHadTarget .OR. nTarget == nTargetTO_DO
               AAdd( aArgsTarget, tmp )
            ENDIF
         ENDCASE
      NEXT

      /* Exit if there was no more targets found on the command line */
      IF nTarget < nTargetTO_DO .AND. nTargetTO_DO != 1
         EXIT
      ENDIF

      /* Build one target */
      nResult := __hbmk( aArgsTarget, nTargetPos, 1, @lPause, @lExitStr )

      /* Exit on first failure */
      IF nResult != _EXIT_OK
         EXIT
      ENDIF

      ++nTargetTO_DO
   ENDDO

   IF nResult != _EXIT_OK
      IF lExitStr
         OutErr( hb_StrFormat( _SELF_NAME_ + ": " + I_( "Exit code: %1$d: %2$s" ), nResult, ExitCodeStr( nResult ) ) + _OUT_EOL )
      ENDIF
      IF lPause
         OutStd( I_( "Press any key to continue..." ) )
         Inkey( 0 )
      ENDIF
   ENDIF

   ErrorLevel( nResult )

   RETURN

#else

/* public entry for embedded flavor */
FUNCTION hbmk( ... )
   RETURN __hbmk( ... )

#endif /* ! _HBMK_EMBEDDED_ */

#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )

STATIC PROCEDURE hbmk_COMP_Setup( cARCH, cCOMP, cBasePath )

   /* TODO: Use HB_CCPREFIX instead of PATH modification, where possible. */

   /* NOTE: We have to retain existing PATH as we may need some tools
            from it, like upx compressor. [vszakats] */

   cBasePath := hb_PathNormalize( cBasePath )

   DO CASE
   CASE cARCH == "dos" .AND. cCOMP == "djgpp"

      hb_SetEnv( "DJGPP", cBasePath + hb_ps() + "djgpp.env" )
      hb_SetEnv( "PATH", cBasePath + hb_ps() + "bin" + hb_osPathListSeparator() + GetEnv( "PATH" ) )

   CASE cARCH == "win" .AND. cCOMP == "mingw"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "bin" + hb_osPathListSeparator() + GetEnv( "PATH" ) )

   CASE cARCH == "win" .AND. cCOMP == "pocc"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "Bin" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "Include" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Include" + hb_ps() + "Win" )
      hb_SetEnv( "LIB", cBasePath + hb_ps() + "Lib" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Lib" + hb_ps() + "Win" )

   CASE cARCH == "win" .AND. cCOMP == "pocc64"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "Bin" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "Include" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Include" + hb_ps() + "Win" )
      hb_SetEnv( "LIB", cBasePath + hb_ps() + "Lib" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Lib" + hb_ps() + "Win64" )

   CASE cARCH == "wce" .AND. cCOMP == "poccarm"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "Bin" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "Include" + hb_ps() + "WinCE" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Include" )
      hb_SetEnv( "LIB", cBasePath + hb_ps() + "Lib" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Lib" + hb_ps() + "WinCE" )

   CASE cCOMP == "watcom"

      hb_SetEnv( "WATCOM", cBasePath )
      hb_SetEnv( "EDPATH", cBasePath + hb_ps() + "eddat" )

      #if   defined( __PLATFORM__WINDOWS )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binnt" + hb_osPathListSeparator() + cBasePath + hb_ps() + "binw" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      #elif defined( __PLATFORM__OS2 )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binp" + hb_osPathListSeparator() + cBasePath + hb_ps() + "binw" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      #elif defined( __PLATFORM__DOS )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binw" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      #elif defined( __PLATFORM__LINUX )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binl" + hb_osPathListSeparator() + GetEnv( "PATH" ) )
      #endif

      DO CASE
      CASE cARCH == "win"
         hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "h" + hb_osPathListSeparator() + cBasePath + hb_ps() + "h" + hb_ps() + "nt" )
      CASE cARCH == "os2"
         hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "h" + hb_osPathListSeparator() + cBasePath + hb_ps() + "h" + hb_ps() + "os2" )
         hb_SetEnv( "BEGINLIBPATH", cBasePath + hb_ps() + "binp" + hb_ps() + "dll" )
      CASE cARCH == "dos"
         hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "h" )
      CASE cARCH == "linux"
         hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "lh" )
      ENDCASE

   ENDCASE

   RETURN

#endif

STATIC FUNCTION hbmk_new( lShellMode )

   LOCAL hbmk[ _HBMK_MAX_ ]

   hbmk[ _HBMK_lShellMode ] := lShellMode

   hbmk[ _HBMK_cBUILD ] := ""

   hbmk[ _HBMK_lStopAfterInit ] := .F.
   hbmk[ _HBMK_lStopAfterHarbour ] := .F.

   hbmk[ _HBMK_nExitCode ] := _EXIT_OK

   hbmk[ _HBMK_cWorkDir ] := NIL

   hbmk[ _HBMK_lCreateLib ] := .F.
   Set_lCreateDyn( hbmk, .F. )
   hbmk[ _HBMK_lCreateImpLib ] := .F.
   hbmk[ _HBMK_lCreatePPO ] := .F.
   hbmk[ _HBMK_lCreateHRB ] := .F.

   hbmk[ _HBMK_lDynVM ] := .F.

   hbmk[ _HBMK_lQuiet ] := .F.
   hbmk[ _HBMK_lInfo ] := .F.
   hbmk[ _HBMK_nMaxCol ] := MaxCol()

   hbmk[ _HBMK_cPLAT ] := ""
   hbmk[ _HBMK_cCOMP ] := ""

   hbmk[ _HBMK_lCPP ] := NIL
   hbmk[ _HBMK_lGUI ] := .F.
   hbmk[ _HBMK_lMT ] := .F.
   hbmk[ _HBMK_lPIC ] := .F.
   hbmk[ _HBMK_lDEBUG ] := .F.
   hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
   hbmk[ _HBMK_lREBUILD ] := .F.
   hbmk[ _HBMK_lCLEAN ] := .F.
   hbmk[ _HBMK_lTRACE ] := .F.
   hbmk[ _HBMK_lDONTEXEC ] := .F.
   hbmk[ _HBMK_nHBMODE ] := _HBMODE_NATIVE
   hbmk[ _HBMK_lSHAREDDIST ] := NIL
   hbmk[ _HBMK_lNULRDD ] := .F.
   hbmk[ _HBMK_lMAP ] := .F.
   hbmk[ _HBMK_lBEEP ] := .F.
   hbmk[ _HBMK_lSTRIP ] := .F.
   hbmk[ _HBMK_lOPTIM ] := .T.
   hbmk[ _HBMK_nWARN ] := _WARN_YES
   hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF
   hbmk[ _HBMK_lRUN ] := .F.
   hbmk[ _HBMK_lINC ] := .F.
   hbmk[ _HBMK_lREBUILDPO ] := .F.
   hbmk[ _HBMK_lMINIPO ] := .F.
   hbmk[ _HBMK_nCONF ] := _CONF_RELEASE
   hbmk[ _HBMK_lIGNOREERROR ] := .F.
   hbmk[ _HBMK_lIMPLIB ] := .F.
   hbmk[ _HBMK_lHBCPPMM ] := .F.
   hbmk[ _HBMK_hDEP ] := { => }
   hbmk[ _HBMK_hAUTOHBC ] := { => }
   hbmk[ _HBMK_hAUTOHBCFOUND ] := { => }
   hbmk[ _HBMK_aDEPTHBC ] := {}
   hbmk[ _HBMK_lDEPIMPLIB ] := .T.

   hb_HCaseMatch( hbmk[ _HBMK_hDEP ], .F. )

   hbmk[ _HBMK_lBLDFLGP ] := .F.
   hbmk[ _HBMK_lBLDFLGC ] := .F.
   hbmk[ _HBMK_lBLDFLGL ] := .F.

   hbmk[ _HBMK_hPLUGINHRB ] := { => }
   hbmk[ _HBMK_hPLUGINVars ] := { => }
   hbmk[ _HBMK_aPLUGINPars ] := {}
   hbmk[ _HBMK_hPLUGINExt ] := { => }

   hb_HCaseMatch( hbmk[ _HBMK_hPLUGINExt ], .F. )

   hbmk[ _HBMK_lDEBUGTIME ] := .F.
   hbmk[ _HBMK_lDEBUGINC ] := .F.
   hbmk[ _HBMK_lDEBUGSTUB ] := .F.
   hbmk[ _HBMK_lDEBUGI18N ] := .F.
   hbmk[ _HBMK_lDEBUGDEPD ] := .F.
   hbmk[ _HBMK_lDEBUGPARS ] := .F.

   hbmk[ _HBMK_nCmd_Esc ] := NIL
   hbmk[ _HBMK_nScr_Esc ] := NIL
   hbmk[ _HBMK_nCmd_FNF ] := NIL

   hbmk[ _HBMK_hDEPTSDIR ] := { => }
   hbmk[ _HBMK_hDEPTMACRO ] := { => }

   hbmk[ _HBMK_lInstForce ] := .F.
   hbmk[ _HBMK_lAutoHBM ] := .T.
   hbmk[ _HBMK_lContainer ] := .F.
   hbmk[ _HBMK_lShowLevel ] := .F.

   hbmk[ _HBMK_aLINK ] := {}

   hbmk[ _HBMK_cC ] := ""
   hbmk[ _HBMK_cCPP ] := ""

   hbmk[ _HBMK_aINCPATH ] := {}
   hbmk[ _HBMK_aLIBPATH ] := {}

   hbmk[ _HBMK_lDumpInfo ] := .F.
   hbmk[ _HBMK_lMarkdown ] := .F.

   RETURN hbmk

STATIC PROCEDURE hbmk_init_stage2( hbmk )

   hbmk[ _HBMK_aPRG ] := {}
   hbmk[ _HBMK_aCH ] := {}
   hbmk[ _HBMK_aC ] := {}
   hbmk[ _HBMK_aCPP ] := {}
   hbmk[ _HBMK_hDEPTS ] := { => }
   hbmk[ _HBMK_aOPTPRG ] := {}
   hbmk[ _HBMK_aOPTC ] := {}
   hbmk[ _HBMK_aOPTCUSER ] := {}
   hbmk[ _HBMK_aOPTCX ] := {}
   hbmk[ _HBMK_aOPTCPPX ] := {}
   hbmk[ _HBMK_aOPTRES ] := {}
   hbmk[ _HBMK_aOPTL ] := {}
   hbmk[ _HBMK_aOPTLPOST ] := {}
   hbmk[ _HBMK_aOPTA ] := {}
   hbmk[ _HBMK_aOPTD ] := {}
   hbmk[ _HBMK_aOPTDPOST ] := {}
   hbmk[ _HBMK_aOPTI ] := {}
   hbmk[ _HBMK_aOPTS ] := {}
   hbmk[ _HBMK_aRESSRC ] := {}
   hbmk[ _HBMK_aRESCMP ] := {}
   hbmk[ _HBMK_aLIBUSER ] := {}
   hbmk[ _HBMK_aLIBUSERFWK ] := {}
   hbmk[ _HBMK_aLIBUSERGT ] := {}
   hbmk[ _HBMK_aLIBUSERSYS ] := {}
   hbmk[ _HBMK_aLIBUSERSYSPRE ] := {}
   hbmk[ _HBMK_aLIBFILTEROUT ] := {}
   hbmk[ _HBMK_aOBJUSER ] := {}
   hbmk[ _HBMK_aGT ] := {}
   hbmk[ _HBMK_aICON ] := {}
   hbmk[ _HBMK_cMANIFEST ] := NIL
   hbmk[ _HBMK_aIMPLIBSRC ] := {}
   hbmk[ _HBMK_aDEF ] := {}
   hbmk[ _HBMK_aINSTFILE ] := {}
   hbmk[ _HBMK_aREQUEST ] := {}
   hbmk[ _HBMK_cPROGDIR ] := NIL
   hbmk[ _HBMK_cPROGNAME ] := NIL
   hbmk[ _HBMK_cFIRST ] := NIL
   hbmk[ _HBMK_aPO ] := {}
   hbmk[ _HBMK_cHBL ] := NIL
   hbmk[ _HBMK_cHBLDir ] := ""
   hbmk[ _HBMK_cPO ] := NIL
   hbmk[ _HBMK_aLNG ] := {}
   hbmk[ _HBMK_aINSTPATH ] := {}
   hbmk[ _HBMK_lWINUNI ] := .F.
   hbmk[ _HBMK_cHBX ] := NIL

   RETURN

STATIC FUNCTION hbmk_harbour_dirlayout_detect( hbmk, lIgnoreEnvVar )

   LOCAL tmp

   hbmk[ _HBMK_cHB_INSTALL_LI3 ] := ""

   IF lIgnoreEnvVar
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := ""
      hbmk[ _HBMK_cHB_INSTALL_BIN ] := ""
      hbmk[ _HBMK_cHB_INSTALL_LIB ] := ""
      hbmk[ _HBMK_cHB_INSTALL_INC ] := ""
   ELSE
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := hb_DirSepToOS( GetEnv( _HBMK_ENV_INSTALL_PFX ) )
      hbmk[ _HBMK_cHB_INSTALL_BIN ] := hb_DirSepToOS( GetEnv( "HB_INSTALL_BIN" ) )
      hbmk[ _HBMK_cHB_INSTALL_LIB ] := hb_DirSepToOS( GetEnv( "HB_INSTALL_LIB" ) )
      hbmk[ _HBMK_cHB_INSTALL_INC ] := hb_DirSepToOS( GetEnv( "HB_INSTALL_INC" ) )
   ENDIF
   IF Empty( hbmk[ _HBMK_cHB_INSTALL_PFX ] )
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := hb_DirSepAdd( hb_DirBase() ) + ".."
   ENDIF

   IF hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "include" + ;
                                   hb_ps() + "hbvm.h" )
      /* do nothing */
   /* Detect special non-installed dir layout (after simple 'make') */
   ELSEIF hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + ".." + hb_ps() + ".." + hb_ps() + "include" + ;
                                       hb_ps() + "hbvm.h" )
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + ".." + hb_ps() + ".." + hb_ps()
   /* Detect special multi-host dir layout */
   ELSEIF hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + ".." + hb_ps() + "include" + ;
                                       hb_ps() + "hbvm.h" )
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + ".." + hb_ps()
   /* Detect non-installed dir layout with build name containing sub-dirs */
   ELSEIF PathSepCount( hbmk[ _HBMK_cBUILD ] ) > 0 .AND. ;
          hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + Replicate( ".." + hb_ps(), PathSepCount( hbmk[ _HBMK_cBUILD ] ) ) + ".." + hb_ps() + ".." + hb_ps() + "include" + ;
                                       hb_ps() + "hbvm.h" )
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + Replicate( ".." + hb_ps(), PathSepCount( hbmk[ _HBMK_cBUILD ] ) ) + ".." + hb_ps() + ".." + hb_ps()
   /* Detect special *nix dir layout (/bin, /lib/harbour, /lib64/harbour, /include/harbour) */
   ELSEIF hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "include" + ;
                                       hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) + ;
                                       hb_ps() + "hbvm.h" )
      IF Empty( hbmk[ _HBMK_cHB_INSTALL_BIN ] )
         hbmk[ _HBMK_cHB_INSTALL_BIN ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "bin" )
      ENDIF
      IF Empty( hbmk[ _HBMK_cHB_INSTALL_LIB ] )
         IF hb_DirExists( tmp := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "lib64" + hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) ) )
            hbmk[ _HBMK_cHB_INSTALL_LIB ] := tmp
         ELSE
            hbmk[ _HBMK_cHB_INSTALL_LIB ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "lib" + hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) )
         ENDIF
      ENDIF
      IF Empty( hbmk[ _HBMK_cHB_INSTALL_INC ] )
         hbmk[ _HBMK_cHB_INSTALL_INC ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "include" + hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) )
      ENDIF
   ELSEIF ! hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + hb_ps() + "include" + ;
                                         hb_ps() + "hbvm.h" )
      RETURN .F.
   ENDIF

   RETURN .T.

/* This stage needs COMP and PLAT to be filled */
STATIC PROCEDURE hbmk_harbour_dirlayout_init( hbmk )

   LOCAL tmp
   LOCAL lDOSWinTokens

   IF Empty( hbmk[ _HBMK_cHB_INSTALL_BIN ] )
      /* Autodetect multi-compiler/platform bin structure (also .dlls are in bin dir on non-*nix platforms) */
      IF hb_DirExists( tmp := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + "bin" + ;
                                                hb_ps() + hbmk[ _HBMK_cPLAT ] + ;
                                                hb_ps() + hbmk[ _HBMK_cCOMP ] + ;
                                                hb_DirSepToOS( hbmk[ _HBMK_cBUILD ] ) )
         hbmk[ _HBMK_cHB_INSTALL_BIN ] := tmp
      ELSE
         hbmk[ _HBMK_cHB_INSTALL_BIN ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "bin" )
      ENDIF
   ENDIF
   IF Empty( hbmk[ _HBMK_cHB_INSTALL_LIB ] )
      /* Autodetect multi-compiler/platform lib structure */
      IF hb_DirExists( tmp := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + "lib" + ;
                                                hb_ps() + hbmk[ _HBMK_cPLAT ] + ;
                                                hb_ps() + hbmk[ _HBMK_cCOMP ] + ;
                                                hb_DirSepToOS( hbmk[ _HBMK_cBUILD ] ) )
         hbmk[ _HBMK_cHB_INSTALL_LIB ] := tmp
      ELSE
         hbmk[ _HBMK_cHB_INSTALL_LIB ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "lib" )
      ENDIF
   ENDIF
   IF Empty( hbmk[ _HBMK_cHB_INSTALL_LI3 ] )
      IF hbmk[ _HBMK_cPLAT ] == "win" .AND. ;
         hb_DirExists( tmp := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + "lib" + ;
                                                hb_ps() + "3rd" + ;
                                                hb_ps() + hbmk[ _HBMK_cPLAT ] + ;
                                                hb_ps() + hbmk[ _HBMK_cCOMP ] )
         hbmk[ _HBMK_cHB_INSTALL_LI3 ] := tmp
      ENDIF
   ENDIF
   IF Empty( hbmk[ _HBMK_cHB_INSTALL_INC ] )
      hbmk[ _HBMK_cHB_INSTALL_INC ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "include" )
   ENDIF

   IF hbmk[ _HBMK_cHB_INSTALL_DYN ] == NIL
      IF HBMK_ISPLAT( "win|wce|os2|dos|cygwin" )
         hbmk[ _HBMK_cHB_INSTALL_DYN ] := hbmk[ _HBMK_cHB_INSTALL_BIN ]
      ELSE
         hbmk[ _HBMK_cHB_INSTALL_DYN ] := hbmk[ _HBMK_cHB_INSTALL_LIB ]
      ENDIF
   ENDIF

   hbmk[ _HBMK_cHB_INSTALL_BIN ] := hb_DirSepDel( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_BIN ] ) )
   hbmk[ _HBMK_cHB_INSTALL_LIB ] := hb_DirSepDel( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_LIB ] ) )
   hbmk[ _HBMK_cHB_INSTALL_LI3 ] := hb_DirSepDel( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_LI3 ] ) )
   hbmk[ _HBMK_cHB_INSTALL_DYN ] := hb_DirSepDel( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_DYN ] ) )
   hbmk[ _HBMK_cHB_INSTALL_INC ] := hb_DirSepDel( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_INC ] ) )

   /* Add main Harbour library dir to lib path list */
   AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], hbmk[ _HBMK_cHB_INSTALL_LIB ] )
   /* Locally hosted 3rd party binary libraries */
   AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], hbmk[ _HBMK_cHB_INSTALL_LI3 ] )
   IF ! Empty( hbmk[ _HBMK_cHB_INSTALL_DYN ] ) .AND. ;
      !( hbmk[ _HBMK_cHB_INSTALL_DYN ] == hbmk[ _HBMK_cHB_INSTALL_LIB ] ) .AND. ;
      ! HBMK_ISPLAT( "win|wce|os2|dos|cygwin" )
      AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], hbmk[ _HBMK_cHB_INSTALL_DYN ] )
   ENDIF

   /* Add main Harbour header dir to header path list */
   AAddNotEmpty( hbmk[ _HBMK_aINCPATH ], hbmk[ _HBMK_cHB_INSTALL_INC ] )

   /* Add custom search paths for .hbc files */
   IF ! Empty( hbmk[ _HBMK_cHB_INSTALL_ADD ] := GetEnv( "HB_INSTALL_ADDONS" ) )
      #if defined( __PLATFORM__WINDOWS ) .OR. ;
          defined( __PLATFORM__DOS ) .OR. ;
          defined( __PLATFORM__OS2 )
         lDOSWinTokens := .T.
      #else
         lDOSWinTokens := NIL
      #endif
      FOR EACH tmp IN hb_ATokens( hbmk[ _HBMK_cHB_INSTALL_ADD ], hb_osPathListSeparator(), lDOSWinTokens, lDOSWinTokens )
         IF ! Empty( tmp )
            AAdd( hbmk[ _HBMK_aLIBPATH ], hb_PathNormalize( hb_DirSepAdd( hb_DirSepToOS( tmp ) ) ) + "%{hb_name}" )
         ENDIF
      NEXT
      /* do not use user-supplied dir[s] as default one */
      hbmk[ _HBMK_cHB_INSTALL_ADD ] := ""
   ENDIF
   /* Add default search paths for .hbc files */
   AAdd( hbmk[ _HBMK_aLIBPATH ], hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + _HBMK_SPECDIR_CONTRIB + hb_ps() + "%{hb_name}" )
   AAdd( hbmk[ _HBMK_aLIBPATH ], hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + _HBMK_SPECDIR_ADDONS + hb_ps() + "%{hb_name}" )
#if defined( __PLATFORM__UNIX )
   IF hb_DirExists( tmp := "/opt/harbour" )
      IF Empty( hbmk[ _HBMK_cHB_INSTALL_ADD ] )
         hbmk[ _HBMK_cHB_INSTALL_CON ] := tmp + hb_ps() + _HBMK_SPECDIR_CONTRIB
         hbmk[ _HBMK_cHB_INSTALL_ADD ] := tmp + hb_ps() + _HBMK_SPECDIR_ADDONS
      ENDIF
      AAdd( hbmk[ _HBMK_aLIBPATH ], tmp + hb_ps() + _HBMK_SPECDIR_CONTRIB + "/%{hb_name}" )
      AAdd( hbmk[ _HBMK_aLIBPATH ], tmp + hb_ps() + _HBMK_SPECDIR_ADDONS + "/%{hb_name}" )
   ENDIF
   IF hb_DirExists( tmp := "/usr/local/share/harbour" )
      IF Empty( hbmk[ _HBMK_cHB_INSTALL_ADD ] )
         hbmk[ _HBMK_cHB_INSTALL_CON ] := tmp + hb_ps() + _HBMK_SPECDIR_CONTRIB
         hbmk[ _HBMK_cHB_INSTALL_ADD ] := tmp + hb_ps() + _HBMK_SPECDIR_ADDONS
      ENDIF
      AAdd( hbmk[ _HBMK_aLIBPATH ], tmp + hb_ps() + _HBMK_SPECDIR_CONTRIB + "/%{hb_name}" )
      AAdd( hbmk[ _HBMK_aLIBPATH ], tmp + hb_ps() + _HBMK_SPECDIR_ADDONS + "/%{hb_name}" )
   ENDIF
   IF hb_DirExists( tmp := "/usr/share/harbour" )
      IF Empty( hbmk[ _HBMK_cHB_INSTALL_ADD ] )
         hbmk[ _HBMK_cHB_INSTALL_CON ] := tmp + hb_ps() + _HBMK_SPECDIR_CONTRIB
         hbmk[ _HBMK_cHB_INSTALL_ADD ] := tmp + hb_ps() + _HBMK_SPECDIR_ADDONS
      ENDIF
      AAdd( hbmk[ _HBMK_aLIBPATH ], tmp + hb_ps() + _HBMK_SPECDIR_CONTRIB + "/%{hb_name}" )
      AAdd( hbmk[ _HBMK_aLIBPATH ], tmp + hb_ps() + _HBMK_SPECDIR_ADDONS + "/%{hb_name}" )
   ENDIF
   IF Empty( hbmk[ _HBMK_cHB_INSTALL_ADD ] )
      hbmk[ _HBMK_cHB_INSTALL_CON ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + _HBMK_SPECDIR_CONTRIB
      hbmk[ _HBMK_cHB_INSTALL_ADD ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + _HBMK_SPECDIR_ADDONS
   ENDIF
#endif
   IF Empty( hbmk[ _HBMK_cHB_INSTALL_ADD ] )
      hbmk[ _HBMK_cHB_INSTALL_CON ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + _HBMK_SPECDIR_CONTRIB
      hbmk[ _HBMK_cHB_INSTALL_ADD ] := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + _HBMK_SPECDIR_ADDONS
   ENDIF

   RETURN

STATIC FUNCTION __hbmk( aArgs, nArgTarget, nLevel, /* @ */ lPause, /* @ */ lExitStr )

   LOCAL nStart := hb_MilliSeconds()

   LOCAL hbmk

   LOCAL aLIB_BASE_EXTERN
   LOCAL aLIB_BASE_DEBUG
   LOCAL aLIB_BASE_1
   LOCAL aLIB_BASE_1_MT
   LOCAL aLIB_BASE_2
   LOCAL aLIB_BASE_2_MT
   LOCAL aLIB_BASE_GT
   LOCAL aLIB_BASE_NULRDD
   LOCAL aLIB_BASE_RDD
   LOCAL aLIB_BASE_RDD_MT
   LOCAL aLIB_BASE_CPLR
   LOCAL aLIB_BASE_3
   LOCAL aLIB_BASE_3_MT
   LOCAL cLIB_BASE_PCRE
   LOCAL cLIB_BASE_ZLIB

   LOCAL l_cCSTUB
   LOCAL l_cCPPSTUB
   LOCAL l_cRESSTUB

   LOCAL l_aPRG_TO_DO
   LOCAL l_aC_TO_DO
   LOCAL l_aCPP_TO_DO
   LOCAL l_aCGEN_TO_DO
   LOCAL l_aRESSRC_TO_DO
   LOCAL l_aLIBSHARED
   LOCAL l_aLIBSHAREDPOST := {}
   LOCAL l_aLIB
   LOCAL l_aLIBA
   LOCAL l_aLIBRAW
   LOCAL l_aLIBHB
   LOCAL l_aLIBHBBASE_2 := {}
   LOCAL l_aLIBHBGT
   LOCAL l_aLIB3RD
   LOCAL l_aLIBSYS
   LOCAL l_aLIBSYSCORE := {}
   LOCAL l_aLIBSYSMISC := {}
   LOCAL l_aLIBSTATICPOST := {}
   LOCAL l_aOPTRUN
   LOCAL l_cLIBSELF
   LOCAL l_cIMPLIBDIR
   LOCAL l_cIMPLIBNAME
   LOCAL l_lIMPLIBToProcess := .F.
   LOCAL l_aOBJ
   LOCAL l_aOBJA
   LOCAL l_aCLEAN
   LOCAL l_cMAIN := NIL
   LOCAL l_cVCSDIR
   LOCAL l_cVCSHEAD
   LOCAL l_cTSHEAD
   LOCAL l_cHBSUFFIX := ""
   LOCAL l_lNOHBLIB := .F.
   LOCAL l_lLIBSYSMISC := .T.
   LOCAL l_cCMAIN := NIL
   LOCAL l_lTargetSelected := .F.
   LOCAL l_cDynLibDir

   /* lib ordering tries to satisfy linkers which require this
      (mingw*, linux/gcc, bsd/gcc and dos/djgpp), but this will not solve
      potential problems when users are speccing custom libs themselves
      and expect them to work the same way on all supported platforms/compilers.
      So I decided to readd this feature until we find a solution which
      does not have such bad side-effect.
      [vszakats] */
   LOCAL l_lLIBGROUPING := .T.

   LOCAL l_nJOBS := 1

   LOCAL aCOMPDET := NIL
   LOCAL aCOMPDET_EMBED
   LOCAL aCOMPSUP

   LOCAL cLibPrefix
   LOCAL cLibExt
   LOCAL cObjPrefix
   LOCAL cObjExt
   LOCAL cLibLibExt := ""
   LOCAL cLibLibPrefix := ""
   LOCAL cLibObjPrefix
   LOCAL cDynObjPrefix := NIL
   LOCAL cDefPrefix := NIL
   LOCAL cLibPathPrefix
   LOCAL cLibPathSep
   LOCAL cImpLibExt := ""
   LOCAL cResPrefix
   LOCAL cResExt
   LOCAL cBinExt
   LOCAL cOptPrefix
   LOCAL cOptIncMask
   LOCAL cBin_Cprs
   LOCAL cOpt_Cprs
   LOCAL cOpt_CprsMin
   LOCAL cOpt_CprsMax
   LOCAL cBin_Post := NIL
   LOCAL cOpt_Post
   LOCAL nOpt_Esc
   LOCAL nOpt_FNF
   LOCAL lCHD_Comp := .F.
   LOCAL cCHD_DirOld
   LOCAL cBin_Sign
   LOCAL cOpt_Sign
   LOCAL cOpt_SignID
   LOCAL cOpt_SignPass
   LOCAL aParamPROGNAME

   LOCAL cCommand
   LOCAL aCommand
   LOCAL cOpt_CompC
   LOCAL cOpt_CompCPass
   LOCAL cOpt_CompCLoop
   LOCAL cOpt_Link
   LOCAL cOpt_Res
   LOCAL cOpt_Lib
   LOCAL cOpt_Dyn
   LOCAL cOpt_LibHBX
   LOCAL cBin_CompPRG
   LOCAL cBin_CompC
   LOCAL cBin_CompCPP
   LOCAL cBin_CompCGEN
   LOCAL cBin_Link
   LOCAL cBin_Res
   LOCAL cBin_Lib
   LOCAL cBin_Dyn
   LOCAL cBin_LibHBX
   LOCAL cLibHBX_Regex
   LOCAL bBlk_ImpLib
   LOCAL cPath_CompC
   LOCAL tmp, tmp1, tmp2, tmp3, tmp4, array
   LOCAL cLibBCC_CRTL
   LOCAL cScriptFile
   LOCAL fhnd
   LOCAL cFile
   LOCAL cSuffix
   LOCAL aOBJLIST
   LOCAL cHarbourDyn
   LOCAL hReplace

   LOCAL lSkipBuild := .F.
   LOCAL lStopAfterCComp := .F.
   LOCAL lAcceptCFlag := .F.
   LOCAL lAcceptLDFlag := .F.
   LOCAL lAcceptLDClipper := .F.
   LOCAL lAcceptIFlag := .F.
   LOCAL lHarbourInfo := .F.
   LOCAL lDumpInfoNested := .F.

   LOCAL nHarbourPPO := 0
   LOCAL cHarbourOutputExt
   LOCAL cHarbourOutputDir
   LOCAL cHarbourPPODir := ""

   LOCAL aParams
   LOCAL aParam
   LOCAL cParam
   LOCAL cParamL
   LOCAL cEnv

   LOCAL tTarget
   LOCAL lTargetUpToDate

   LOCAL cDir, cName, cExt

   LOCAL cDL_Version_Alter
   LOCAL cDL_Version

   LOCAL aTO_DO
   LOCAL aThreads
   LOCAL thread

   LOCAL lDoSupportDetection
   LOCAL lDeleteWorkDir := .F.

   LOCAL lHBMAINDLLP

   LOCAL cStdOutErr
   LOCAL aParamINC

   hbmk := hbmk_new( .F. )

   hbmk[ _HBMK_aArgs ] := aArgs
   hbmk[ _HBMK_nArgTarget ] := nArgTarget
   hbmk[ _HBMK_lPause ] := lPause
   hbmk[ _HBMK_nLevel ] := nLevel

   SetUILang( hbmk[ _HBMK_cUILNG ] := GetUILang() )

   IF Empty( aArgs )
      ShowHeader( hbmk )
      ShowHelp( hbmk )
      RETURN _EXIT_HELP
   ENDIF

   /* Process environment */

   cEnv := GetEnv( _HBMK_ENV_NAME )

   /* Compatibility */

   FOR EACH tmp IN ListToArray( hb_DirSepToOS( GetEnv( "HB_USER_LIBPATHS" ) ) )
      cEnv += " -L" + tmp
   NEXT
   FOR EACH tmp IN ListToArray( hb_DirSepToOS( GetEnv( "HB_USER_LIBS" ) ) )
      cEnv += " -l" + tmp
   NEXT
   IF ! Empty( GetEnv( "HB_PLATFORM" ) )
      cEnv += " -plat=" + GetEnv( "HB_PLATFORM" )
   ENDIF
   IF ! Empty( GetEnv( "HB_COMPILER" ) )
      cEnv += " -comp=" + GetEnv( "HB_COMPILER" )
   ENDIF
   IF ! Empty( GetEnv( "HB_CPU" ) )
      cEnv += " -cpu=" + GetEnv( "HB_CPU" )
   ENDIF
   IF ! Empty( GetEnv( "HB_BUILD_NAME" ) )
      cEnv += " -build=" + hb_DirSepToOS( GetEnv( "HB_BUILD_NAME" ) )
   ENDIF
   cEnv := AllTrim( cEnv )

   IF ! Empty( cEnv )
      aArgs := ArrayJoin( ListToArray( cEnv ), aArgs )
   ENDIF

   /* ; */

   FOR EACH cParam IN aArgs

      cParamL := Lower( cParam )

      /* NOTE: Do not forget to make these ignored in the main
               option processing loop. */
      DO CASE
      CASE cParamL == "-quiet" .OR. ;
           cParamL == "--hbdirbin" .OR. ;
           cParamL == "--hbdirdyn" .OR. ;
           cParamL == "--hbdirlib" .OR. ;
           cParamL == "--hbdirinc" .OR. ;
           Left( cParamL, Len( "--hbinfo" ) ) == "--hbinfo"

         hbmk[ _HBMK_lQuiet ] := .T.
         hbmk[ _HBMK_lInfo ] := .F.
         hbmk[ _HBMK_lTRACE ] := .F.

      CASE cParamL             == "-quiet-"    ; hbmk[ _HBMK_lQuiet ] := .F.
      CASE Left( cParamL, 6 )  == "-comp="     ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 7 ), _TARG_COMP )
      CASE Left( cParamL, 6 )  == "-plat="     ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 7 ), _TARG_PLAT )
#ifdef HB_LEGACY_LEVEL4
      CASE Left( cParamL, 10 ) == "-compiler=" ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 11 ), _TARG_COMP ) ; LegacyWarning( hbmk, aParam, "-plat" )
      CASE Left( cParamL, 10 ) == "-platform=" ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 11 ), _TARG_PLAT ) ; LegacyWarning( hbmk, aParam, "-comp" )
#endif
      CASE Left( cParamL, 5 )  == "-cpu="      ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 6 ), _TARG_CPU )
      CASE Left( cParamL, 7 )  == "-build="    ; hbmk[ _HBMK_cBUILD ] := StrTran( hb_DirSepToOS( SubStr( cParam, 8 ) ), hb_ps() )
      CASE Left( cParamL, 6 )  == "-build"     ; hbmk[ _HBMK_lStopAfterHarbour ] := .T.
      CASE Left( cParamL, 8 )  == "-credits"   ; hbmk[ _HBMK_lStopAfterHarbour ] := .T.
      CASE Left( cParamL, 6 )  == "-lang="     ; SetUILang( hbmk[ _HBMK_cUILNG ] := SubStr( cParam, 7 ) )
      CASE Left( cParamL, 4 )  == "-shl"       ; hbmk[ _HBMK_lShowLevel ] := .T.
      CASE Left( cParamL, 7 )  == "-width="

         tmp := Val( SubStr( cParam, 8 ) )
         IF tmp > 40
            hbmk[ _HBMK_nMaxCol ] := tmp
         ELSEIF tmp == 0
            hbmk[ _HBMK_nMaxCol ] := 65535
         ENDIF

      CASE cParamL             == "-hbrun"     ; lSkipBuild := .T. ; hbmk[ _HBMK_lRUN ] := .T.
      CASE cParamL             == "-hbraw"     ; hbmk[ _HBMK_lInfo ] := .F. ; hbmk[ _HBMK_lStopAfterHarbour ] := .T. ; lStopAfterCComp := .T. ; hbmk[ _HBMK_lCreateLib ] := .F. ; Set_lCreateDyn( hbmk, .F. ) ; lAcceptCFlag := .F. ; lAcceptLDFlag := .F.
      CASE cParamL             == "-hbcmp" .OR. ;
           cParamL             == "-clipper"   ; hbmk[ _HBMK_lInfo ] := .F. ; hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .T. ; hbmk[ _HBMK_lCreateLib ] := .F. ; Set_lCreateDyn( hbmk, .F. ) ; lAcceptCFlag := .F. ; lAcceptLDFlag := .F.
      CASE cParamL             == "-hbcc"      ; hbmk[ _HBMK_lInfo ] := .F. ; hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .F. ; lAcceptCFlag := .T.
      CASE cParamL             == "-hblnk"     ; hbmk[ _HBMK_lInfo ] := .F. ; hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .F. ; lAcceptLDFlag := .T.
      CASE cParamL             == "-rtlink" .OR. ;
           cParamL             == "-exospace" .OR. ;
           cParamL             == "-blinker"   ; hbmk[ _HBMK_lInfo ] := .F. ; hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .F. ; lAcceptLDClipper := .T.
      CASE cParamL             == "-info"      ; hbmk[ _HBMK_lInfo ] := .T.
      CASE cParamL             == "-autohbm"   ; hbmk[ _HBMK_lAutoHBM ] := .T.
      CASE cParamL             == "-autohbm-"  ; hbmk[ _HBMK_lAutoHBM ] := .F.
      CASE cParamL             == "-xhb"       ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_XHB
      CASE cParamL             == "-hb10"      ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_HB10
      CASE cParamL             == "-hb20"      ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_HB20
      CASE cParamL             == "-hb30"      ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_HB30
      CASE cParamL             == "-hbc"       ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_RAW_C ; lAcceptCFlag := .T.

      /* -env options used on command line, process only once (=do not process again for subprojects) */
      CASE Left( cParamL, 5 )  == "-env:" .AND. hbmk[ _HBMK_nLevel ] == 1

         ProcEnvOption( SubStr( cParam, 6 ) )

      CASE cParamL == "-help" .OR. cParamL == "--help" .OR. ;
           cParamL == "-h" .OR. cParamL == "-?"

         ShowHeader( hbmk )
         ShowHelp( hbmk, .T. )
         RETURN _EXIT_HELP

      CASE cParamL == "-longhelp" .OR. cParamL == "--longhelp" .OR. ;
           cParamL == "-hh" .OR. cParamL == "-??"

         ShowHeader( hbmk )
         ShowHelp( hbmk, .T., .T. )
         RETURN _EXIT_HELP

      CASE cParamL == "-longhelpmd" .OR. cParamL == "--longhelpmd"

         hbmk[ _HBMK_lMarkdown ] := .T.

         ShowHeader( hbmk )
         ShowHelp( hbmk, .T., .T. )
         RETURN _EXIT_HELP

      CASE Left( cParamL, 8 ) == "-find"

         find_harbour_function( hbmk, aArgs, cParam )
         RETURN _EXIT_OK

      CASE Left( cParamL, 8 ) == "-hbmake="

         convert_hbmake_to_hbp( hbmk, SubStr( cParam, 9 ) )
         RETURN _EXIT_OK

      CASE Left( cParamL, 5 ) == "-xbp="

         convert_xbp_to_hbp( hbmk, SubStr( cParam, 6 ) )
         RETURN _EXIT_OK

      CASE Left( cParamL, 5 ) == "-xhp="

         convert_xhp_to_hbp( hbmk, SubStr( cParam, 6 ) )
         RETURN _EXIT_OK

#if defined( __PLATFORM__WINDOWS )

      CASE Left( cParamL, 6 ) == "-hbreg"

         IF __hbshell_win_reg_self( .T., SubStr( cParamL, 6 + 1 ) == "=global" )
            _hbmk_OutStd( hbmk, "Harbour Script (.hb) registered" )
         ELSE
            _hbmk_OutErr( hbmk, "Error: Registering Harbour Script (.hb)" )
         ENDIF

         RETURN _EXIT_OK

      CASE Left( cParamL, 8 ) == "-hbunreg"

         IF __hbshell_win_reg_self( .F., SubStr( cParamL, 8 + 1 ) == "=global" )
            _hbmk_OutStd( hbmk, "Harbour Script (.hb) unregistered" )
         ELSE
            _hbmk_OutErr( hbmk, "Error: Unregistering Harbour Script (.hb)" )
         ENDIF

         RETURN _EXIT_OK
#endif

      CASE cParamL == "-version" .OR. ;
           cParamL == "--version"

         ShowHeader( hbmk )
         RETURN _EXIT_OK

      ENDCASE
   NEXT

   IF nLevel > _HBMK_NEST_MAX
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Cannot nest projects deeper than %1$d levels" ), _HBMK_NEST_MAX ) )
      RETURN _EXIT_DEEPPROJNESTING
   ENDIF

   IF nLevel > 1
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Building sub-project (level %1$d): %2$s" ), nLevel, hbmk[ _HBMK_aArgs ][ hbmk[ _HBMK_nArgTarget ] ] ) )
      ENDIF
   ENDIF

   IF ! Empty( cEnv )
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing environment options: %1$s" ), cEnv ) )
      ENDIF
   ENDIF

   /* Initialize Harbour libs */

   IF ! _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )

      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
         cDL_Version_Alter := ;
            "-" + ;
            hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) + ;
            hb_ntos( hb_Version( HB_VERSION_MINOR ) )
         cDL_Version := ;
            "." + ;
            hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) + "." + ;
            hb_ntos( hb_Version( HB_VERSION_MINOR ) ) + "." + ;
            hb_ntos( hb_Version( HB_VERSION_RELEASE ) )
      ELSE
         cDL_Version_Alter := ;
            "-" + ;
            hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ], -16 ), 0xFF ) ) + ;
            hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ],  -8 ), 0xFF ) )
         cDL_Version := ;
            "." + ;
            hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ], -16 ), 0xFF ) ) + "." + ;
            hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ],  -8 ), 0xFF ) ) + "." + ;
            hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ],   0 ), 0xFF ) )
      ENDIF

      aLIB_BASE_EXTERN  := { "hbextern" }
      aLIB_BASE_DEBUG   := { "hbdebug" }
      IF .T. .OR. ( hbmk[ _HBMK_nHBMODE ] >= _HBMODE_HB10 .AND. hbmk[ _HBMK_nHBMODE ] <= _HBMODE_HB30 )
         aLIB_BASE_1    := { "hbvm", "hbrtl", "hblang", "hbcpage" }
         aLIB_BASE_1_MT    := iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_HB10, { "hbvmmt", "hbrtl", "hblang", "hbcpage" }, aLIB_BASE_1 )
      ELSE
         aLIB_BASE_1    := { "hbvm", "hbrtl", "hbnat" }
         aLIB_BASE_1_MT    := iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_HB10, { "hbvmmt", "hbrtl", "hbnat" }, aLIB_BASE_1 )
      ENDIF
      aLIB_BASE_2       := { "hbrtl", "hbvm" }
      aLIB_BASE_2_MT    := iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_HB10, { "hbrtl", "hbvmmt" }, aLIB_BASE_2 )
      aLIB_BASE_GT      := { "gtcgi", "gtpca", "gtstd" }
      aLIB_BASE_NULRDD  := { "hbnulrdd" }
      /* Double 'hbrdd' is required for linkers which otherwise need lib grouping. */
      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10
         aLIB_BASE_RDD  := { "hbrdd",             "hbusrrdd", "rddntx", "rddcdx",           "rddfpt", "hbrdd", "hbhsx", "hbsix" }
      ELSE
         aLIB_BASE_RDD  := { "hbrdd", "hbuddall", "hbusrrdd", "rddntx", "rddcdx", "rddnsx", "rddfpt", "hbrdd", "hbhsx", "hbsix" }
      ENDIF
      aLIB_BASE_RDD_MT  := aLIB_BASE_RDD
      aLIB_BASE_CPLR    := { "hbcplr" }
      aLIB_BASE_3       := { "hbmacro", "hbcplr", "hbpp", "hbcommon" }
      aLIB_BASE_3_MT    := aLIB_BASE_3
      cLIB_BASE_PCRE    := "hbpcre"
      cLIB_BASE_ZLIB    := "hbzlib"
   ELSE

      cDL_Version_Alter := ""
      cDL_Version       := ""

      aLIB_BASE_EXTERN  := {}
      aLIB_BASE_DEBUG   := { "debug" }
      aLIB_BASE_1       := { "vm"  , "rtl"  , "lang", "codepage" }
      aLIB_BASE_1_MT    := { "vmmt", "rtlmt", "lang", "codepage" }
      aLIB_BASE_2       := { "rtl"  , "vm"   }
      aLIB_BASE_2_MT    := { "rtlmt", "vmmt" }
      aLIB_BASE_GT      := { "gtcgi", "gtpca", "gtstd" }
      aLIB_BASE_NULRDD  := { "nulsys" }
      aLIB_BASE_RDD     := { "rdd"  , "usrrdd", "dbfntx", "dbfcdx", "dbfnsx", "dbffpt", "rdd"  , "hsx", "hbsix" }
      aLIB_BASE_RDD_MT  := { "rddmt", "usrrdd", "dbfntx", "dbfcdx", "dbfnsx", "dbffpt", "rddmt", "hsx", "hbsix" }
      aLIB_BASE_CPLR    := {}
      aLIB_BASE_3       := { "macro"  , "pp", "common" }
      aLIB_BASE_3_MT    := { "macromt", "pp", "common" }
      cLIB_BASE_PCRE    := "pcrepos"
      cLIB_BASE_ZLIB    := "zlib"

      /* NOTE: 'dbfnsx' was added to xhb on 2009-01-08. We chose to prioritize
               on newer xhb versions, so for older versions, a dummy lib should
               be created. [vszakats] */
   ENDIF

   hbmk[ _HBMK_aLIB_BASE_WARN ] := ArrayAJoin( { ;
      aLIB_BASE_EXTERN , ;
      aLIB_BASE_DEBUG  , ;
      aLIB_BASE_1      , ;
      aLIB_BASE_1_MT   , ;
      aLIB_BASE_2      , ;
      aLIB_BASE_2_MT   , ;
      aLIB_BASE_NULRDD , ;
      aLIB_BASE_RDD    , ;
      aLIB_BASE_RDD_MT , ;
      aLIB_BASE_CPLR   , ;
      aLIB_BASE_3      , ;
      aLIB_BASE_3_MT   , ;
      { cLIB_BASE_PCRE }, ;
      { cLIB_BASE_ZLIB } } )

   hbmk[ _HBMK_nCOMPVer ] := Val( GetEnv( "HB_COMPILER_VER" ) ) /* Format: <15><00>[.<00>] = <major><minor>[.<revision>] */

   /* Autodetect platform */

   IF Empty( hbmk[ _HBMK_cPLAT ] )

      /* NOTE: Keep this in sync manually. All compilers should be listed here,
               which are supported on one platform only. In the future this
               should be automatically extracted from a comp/plat matrix. */
      SWITCH hbmk[ _HBMK_cCOMP ]
      CASE "mingw"
      CASE "mingw64"
      CASE "msvc"
      CASE "msvc64"
      CASE "msvcia64"
      CASE "bcc"
      CASE "bcc64"
      CASE "xcc"
      CASE "pocc"
      CASE "pocc64"
         hbmk[ _HBMK_cPLAT ] := "win"
         EXIT
      CASE "mingwarm"
      CASE "msvcarm"
      CASE "poccarm"
         hbmk[ _HBMK_cPLAT ] := "wce"
         EXIT
      CASE "djgpp"
         hbmk[ _HBMK_cPLAT ] := "dos"
         EXIT
      OTHERWISE
         hbmk[ _HBMK_cPLAT ] := hb_Version( HB_VERSION_BUILD_PLAT )
      ENDSWITCH
      IF ! Empty( hbmk[ _HBMK_cPLAT ] )
         IF hbmk[ _HBMK_lInfo ]
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected platform: %1$s" ), hbmk[ _HBMK_cPLAT ] ) )
         ENDIF
      ENDIF
   ENDIF

   hbmk[ _HBMK_cCCPATH ]   := GetEnv( "HB_CCPATH" )
   hbmk[ _HBMK_cCCPREFIX ] := GetEnv( "HB_CCPREFIX" )
   hbmk[ _HBMK_cCCSUFFIX ] := GetEnv( "HB_CCSUFFIX" )

   #if defined( __PLATFORM__UNIX )
      #if ! defined( __PLATFORM__CYGWIN )
         hbmk[ _HBMK_cCCEXT ] := ""
      #else
         hbmk[ _HBMK_cCCEXT ] := ".exe"
      #endif
   #else
      hbmk[ _HBMK_cCCEXT ] := ".exe"
   #endif

   /* Setup platform dependent data */

   cBin_CompPRG := "harbour" + l_cHBSUFFIX

   DO CASE
   CASE HBMK_ISPLAT( "darwin|bsd|hpux|sunos|beos|qnx|android|vxworks|symbian|linux|cygwin|minix|aix" )
      DO CASE
      CASE hbmk[ _HBMK_cPLAT ] == "linux"
         aCOMPSUP := { "gcc", "clang", "icc", "watcom", "sunpro", "open64", "pcc" }
      CASE hbmk[ _HBMK_cPLAT ] == "darwin"
         aCOMPSUP := { "gcc", "clang", "icc", "pcc" }
      CASE hbmk[ _HBMK_cPLAT ] == "bsd"
         aCOMPSUP := { "gcc", "clang", "pcc" }
      CASE hbmk[ _HBMK_cPLAT ] == "sunos"
         aCOMPSUP := { "gcc", "sunpro", "pcc" }
      CASE hbmk[ _HBMK_cPLAT ] == "android"
         aCOMPSUP := { "gcc", "gccarm" }
      CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
         aCOMPSUP := { "gcc", "diab" }
      CASE hbmk[ _HBMK_cPLAT ] == "aix"
         aCOMPSUP := { "gcc", "icc" }
      CASE hbmk[ _HBMK_cPLAT ] == "minix"
         aCOMPSUP := { "clang", "gcc" }
      OTHERWISE
         aCOMPSUP := { "gcc" }
      ENDCASE
      IF hbmk[ _HBMK_cPLAT ] == "symbian"
         hbmk[ _HBMK_cDynLibPrefix ] := ""
      ELSEIF hbmk[ _HBMK_cPLAT ] == "cygwin"
         hbmk[ _HBMK_cDynLibPrefix ] := "cyg"
      ELSE
         hbmk[ _HBMK_cDynLibPrefix ] := "lib"
      ENDIF
      DO CASE
      CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
         l_aLIBHBGT := {}
         hbmk[ _HBMK_cGTDEFAULT ] := "gtstd"
         cBinExt := ".vxe"
      CASE hbmk[ _HBMK_cPLAT ] == "symbian"
         l_aLIBHBGT := {}
         hbmk[ _HBMK_cGTDEFAULT ] := "gtstd"
         cBinExt := ".exe"
      OTHERWISE
         l_aLIBHBGT := { "gttrm" }
         hbmk[ _HBMK_cGTDEFAULT ] := "gttrm"
         cBinExt := ""
      ENDCASE
      cOptPrefix := "-"
      SWITCH hbmk[ _HBMK_cPLAT ]
      CASE "darwin"  ; hbmk[ _HBMK_cDynLibExt ] := ".dylib" ; EXIT
      CASE "hpux"    ; hbmk[ _HBMK_cDynLibExt ] := ".sl" ; EXIT
      CASE "symbian" ; hbmk[ _HBMK_cDynLibExt ] := ".dll" ; EXIT
      CASE "cygwin"  ; hbmk[ _HBMK_cDynLibExt ] := ".dll" ; EXIT
      OTHERWISE      ; hbmk[ _HBMK_cDynLibExt ] := ".so"
      ENDSWITCH
   CASE hbmk[ _HBMK_cPLAT ] == "dos"
#if ! defined( __PLATFORM__UNIX )
      aCOMPDET := { ;
         { {|| FindInPath( "gcc"      ) }, "djgpp"  }, ;
         { {|| FindInPath( "wcc386"   ) }, "watcom" } }
#endif
      aCOMPSUP := { "djgpp", "gcc", "watcom" }
      l_aLIBHBGT := { "gtdos" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtdos"
      hbmk[ _HBMK_cDynLibPrefix ] := ""
      hbmk[ _HBMK_cDynLibExt ] := "" /* NOTE: This will be reset later if djgpp is detected. */
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE hbmk[ _HBMK_cPLAT ] == "os2"
#if ! defined( __PLATFORM__UNIX )
      aCOMPDET := { ;
         { {|| FindInPath( "gcc"      ) }, "gcc"    }, ;
         { {|| FindInPath( "wcc386"   ) }, "watcom" } }
#endif
      aCOMPSUP := { "gcc", "gccomf", "watcom" }
      l_aLIBHBGT := { "gtos2" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtos2"
      hbmk[ _HBMK_cDynLibPrefix ] := ""
      hbmk[ _HBMK_cDynLibExt ] := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE hbmk[ _HBMK_cPLAT ] == "win"
      /* Order is significant.
         watcom also keeps a cl.exe in its binary dir. */
#if ! defined( __PLATFORM__UNIX )
      aCOMPDET := { ;
         { {|| FindInPath( "arm-mingw32ce-gcc"       ) }, "mingwarm", "arm-mingw32ce-",, "wce" }, ;
         { {|| FindInPath( "arm-wince-mingw32ce-gcc" ) }, "mingwarm", "arm-wince-mingw32ce-",, "wce" }, ;
         { {|| FindInSamePath( "cygstart.exe", "gcc" ) }, "gcc",,, "cygwin" }, ;
         { {|| FindInPath( "gcc-dw2" ) }, "mingw", "", "-dw2" }, ; /* tdragon DWARF-2 build */
         { {|| FindInPath( "x86_64-pc-mingw32-gcc"   ) }, "mingw64" }, ; /* Equation Solution build */
         { {|| FindInPath( "i686-w64-mingw32-gcc"    ) }, "mingw64", "i686-w64-mingw32-" }, ; /* mingw-w64 build */
         { {|| FindInSamePath( "x86_64-w64-mingw32-gcc.exe", "gcc" ) }, "mingw64" }, ; /* mingw-w64 TDM build */
         { {|| FindInPath( "x86_64-w64-mingw32-gcc"  ) }, "mingw64", "x86_64-w64-mingw32-" }, ; /* mingw-w64 build */
         { {|| FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCSUFFIX ] ) }, "mingw" }, ;
         { {|| iif( Empty( GetEnv( "WATCOM" ) ), ;
                    NIL, ;
                    FindInPath( "wcc386" ) ) }, "watcom" }, ;
         { {|| FindInPath( "clarm.exe"  ) }, "msvcarm" }, ;
         { {|| FindInPath( "armasm.exe" ) }, "msvcarm" }, ;
         { {|| FindInPath( "ml64.exe"   ) }, "msvc64" }, ;
         { {|| FindInPath( "ias.exe"    ) }, "msvcia64" }, ;
         { {|| iif( FindInPath( "wcc386"   ) == NIL, ;
                    FindInPath( "cl.exe"   ), ;
                    NIL )                      }, "msvc"    }, ;
         { {|| _BCC_BIN_DETECT()        }, "bcc"    }, ; /* TODO: Add bcc64 autodetection */
         { {|| iif( FindInPath( "dbgeng.lib", GetEnv( "LIB" ) ) != NIL .AND. ( tmp1 := FindInPath( "pocc.exe" ) ) != NIL, tmp1, NIL ) }, "pocc64"  }, ;
         { {|| FindInPath( "pocc.exe" ) }, "pocc"   }, ;
         { {|| iif( ( tmp1 := FindInPath( "icl.exe" ) ) != NIL .AND. "itanium" $ Lower( tmp1 ), tmp1, NIL ) }, "iccia64" }, ;
         { {|| FindInPath( "icl.exe"  ) }, "icc"    }, ;
         { {|| FindInPath( "xCC.exe"  ) }, "xcc"    }, ;
         { {|| FindInPath( "tcc.exe"  ) }, "tcc"    }, ;
         { {|| FindInPath( "dmc.exe"  ) }, "dmc"    } }
#endif
      aCOMPSUP := { ;
         "mingw", "msvc", "clang", "bcc", "watcom", "icc", "pocc", "xcc", "tcc", ;
         "mingw64", "msvc64", "msvcia64", "bcc64", "iccia64", "pocc64" }
      l_aLIBHBGT := { "gtwin", "gtwvt", "gtgui" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtwin"
      hbmk[ _HBMK_cDynLibPrefix ] := ""
      hbmk[ _HBMK_cDynLibExt ] := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
      /* NOTE: Some targets (watcom, pocc/xcc) need kernel32 explicitly. */
      l_aLIBSYSCORE := { "kernel32", "user32", "gdi32", "advapi32", "ws2_32" }
      l_aLIBSYSMISC := { "winspool", "comctl32", "comdlg32", "shell32", "uuid", "ole32", "oleaut32", "mpr", "winmm", "mapi32", "imm32", "msimg32", "wininet" }
   CASE hbmk[ _HBMK_cPLAT ] == "wce"
#if ! defined( __PLATFORM__UNIX )
      aCOMPDET := { ;
         { {|| FindInPath( "clarm.exe"  ) }, "msvcarm" }, ;
         { {|| FindInPath( "armasm.exe" ) }, "msvcarm" }, ;
         { {|| FindInPath( "pocc.exe"   ) }, "poccarm" }, ;
         { {|| FindInPath( "arm-mingw32ce-gcc"       ) }, "mingwarm", "arm-mingw32ce-" }, ;
         { {|| FindInPath( "arm-wince-mingw32ce-gcc" ) }, "mingwarm", "arm-wince-mingw32ce-" }, ;
         { {|| FindInPath( "i386-mingw32ce-gcc"      ) }, "mingw"   , "i386-mingw32ce-" }, ;
         { {|| iif( ! Empty( hbmk[ _HBMK_cCCPREFIX ] ) .OR. ! Empty( hbmk[ _HBMK_cCCSUFFIX ] ), ;
               FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCSUFFIX ] ), ;
               NIL ) }, "mingwarm" } }
#endif
      aCOMPSUP := { "mingwarm", "msvcarm", "poccarm" }
      l_aLIBHBGT := { "gtwvt", "gtgui" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtwvt"
      hbmk[ _HBMK_cDynLibPrefix ] := ""
      hbmk[ _HBMK_cDynLibExt ] := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
      l_aLIBSYSCORE := { "coredll", "ws2" }
      l_aLIBSYSMISC := { "ceshell", "uuid", "ole32", "oleaut32", "wininet", "commdlg", "commctrl" }
   OTHERWISE
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Platform value unknown: %1$s" ), hbmk[ _HBMK_cPLAT ] ) )
      RETURN _EXIT_UNKNPLAT
   ENDCASE

   hbmk[ _HBMK_aLIBCOREGTDEF ] := ArrayJoin( aLIB_BASE_GT, l_aLIBHBGT )
   hbmk[ _HBMK_aLIBCOREGT ] := hbmk[ _HBMK_aLIBCOREGTDEF ]

   /* Setup GUI state for Harbour default */
   SetupForGT( hbmk[ _HBMK_cGTDEFAULT ], NIL, @hbmk[ _HBMK_lGUI ] )

   /* Autodetect Harbour environment */

   IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

      IF ! hbmk_harbour_dirlayout_detect( hbmk, .F. )
         hbmk_OutErr( hbmk, hb_StrFormat( I_( e"Error: %1$s not set, failed to autodetect.\nRun this tool from its original location inside the Harbour installation or set %1$s environment variable to Harbour's root directory." ), _HBMK_ENV_INSTALL_PFX ) )
         RETURN _EXIT_FAILHBDETECT
      ENDIF

      #if defined( __PLATFORM__UNIX )
         /* Detect system locations to enable shared library option by default */
         IF hbmk[ _HBMK_cPLAT ] == "beos"
            hbmk[ _HBMK_lSysLoc ] := ;
               LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_BIN ], "/boot/common"      ) .OR. ;
               LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_BIN ], "/boot/system"      ) .OR. ;
               LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_BIN ], "/boot/home/config" ) .OR. ;
               AScan( ListToArray( GetEnv( "LIBRARY_PATH" ), ":" ), {| tmp | LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_LIB ], tmp ) } ) > 0
         ELSE
            hbmk[ _HBMK_lSysLoc ] := ;
               LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_BIN ], "/usr/local/bin" ) .OR. ;
               LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_BIN ], "/usr/bin"       ) .OR. ;
               AScan( ListToArray( GetEnv( "LD_LIBRARY_PATH" ), ":" ), {| tmp | LEFTEQUAL( hbmk[ _HBMK_cHB_INSTALL_LIB ], tmp ) } ) > 0
         ENDIF
      #else
         hbmk[ _HBMK_lSysLoc ] := .F.
      #endif
   ELSE
      hbmk[ _HBMK_lSysLoc ] := .F.

      hbmk[ _HBMK_cHB_INSTALL_LI3 ] := ""
      hbmk[ _HBMK_cHB_INSTALL_BIN ] := ""
      hbmk[ _HBMK_cHB_INSTALL_LIB ] := ""
      hbmk[ _HBMK_cHB_INSTALL_INC ] := ""
      hbmk[ _HBMK_cHB_INSTALL_PFX ] := ""
   ENDIF

   aCOMPDET_EMBED := {}

   IF HBMK_ISPLAT( "win|wce|dos|os2|linux" )

      #if defined( __PLATFORM__WINDOWS )

         tmp3 := NIL; HB_SYMBOL_UNUSED( tmp3 )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "mingw"    + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win"  , "mingw"   , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "mingw64"  + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win"  , "mingw64" , "i686-w64-mingw32-"   , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "mingw64"  + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win"  , "mingw64" , "x86_64-w64-mingw32-" , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "mingwarm" + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce"  , "mingwarm", "arm-mingw32ce-"      , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "mingwarm" + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce"  , "mingwarm", "arm-wince-mingw32ce-", NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "mingwarm" + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce"  , "mingw"   , "i386-mingw32ce-"     , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "djgpp"    + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc.exe"                    ), tmp1, NIL ) }, "dos"  , "djgpp"   , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "win"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "dos"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "os2"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "linux", "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "pocc"     + hb_ps() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "pocc.exe"                   ), tmp1, NIL ) }, "win"  , "pocc"    , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "pocc"     + hb_ps() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "pocc.exe"                   ), tmp1, NIL ) }, "win"  , "pocc64"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "pocc"     + hb_ps() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "pocc.exe"                   ), tmp1, NIL ) }, "wce"  , "poccarm" , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )

      #elif defined( __PLATFORM__DOS )

         tmp3 := NIL; HB_SYMBOL_UNUSED( tmp3 )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "djgpp"    + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc.exe"                    ), tmp1, NIL ) }, "dos"  , "djgpp"   , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "dos"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "win"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "os2"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "linux", "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )

      #elif defined( __PLATFORM__OS2 )

         tmp3 := NIL; HB_SYMBOL_UNUSED( tmp3 )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "os2"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "win"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "dos"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP + hb_ps() + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "linux", "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )

      #elif defined( __PLATFORM__UNIX )

         IF Empty( hbmk[ _HBMK_cCCPATH ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCPREFIX ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCSUFFIX ] )

            DO CASE
            CASE hbmk[ _HBMK_cCOMP ] == "mingw64"
               FOR EACH tmp IN { "/usr", "/usr/local", "/usr/local/mingw32", "/opt/xmingw", "/opt/cross" }
                  FOR EACH tmp2 IN { "amd64-mingw32msvc" }
                     AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win", "mingw64", tmp2 + "-", tmp + hb_ps() + "bin", NIL } )
                     AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win", "mingw64", "", tmp + hb_ps() + tmp2 + hb_ps() + "bin", NIL } )
                  NEXT
               NEXT
            CASE hbmk[ _HBMK_cPLAT ] == "win" .OR. hbmk[ _HBMK_cCOMP ] == "mingw"
               FOR EACH tmp IN { "/usr", "/usr/local", "/usr/local/mingw32", "/opt/xmingw", "/opt/cross" }
                  FOR EACH tmp2 IN { "i?86-mingw", "i?86-pc-mingw", "i?86-mingw32", "i?86-pc-mingw32", "i?86-mingw32msvc", "i?86-pc-mingw32msvc" }
                     FOR tmp3 := 3 TO 6
                        AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win", "mingw", StrTran( tmp2, "?", hb_ntos( tmp3 ) ) + "-", tmp + hb_ps() + "bin", NIL } )
                        AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win", "mingw", "", tmp + hb_ps() + StrTran( tmp2, "?", hb_ntos( tmp3 ) ) + hb_ps() + "bin", NIL } )
                     NEXT
                  NEXT
               NEXT
            CASE hbmk[ _HBMK_cPLAT ] == "wce"
               AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce", "mingwarm", "arm-mingw32ce-"      , "/opt/mingw32ce/bin"   , NIL } )
               AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce", "mingwarm", "arm-wince-mingw32ce-", "/opt/mingw32ce/bin"   , NIL } )
               AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce", "mingw"   , "i386-mingw32ce-"     , "/opt/x86mingw32ce/bin", NIL } )
            CASE hbmk[ _HBMK_cPLAT ] == "dos"
               AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "dos", "djgpp"   , "i586-pc-msdosdjgpp-" , NIL                    , NIL } )
               AAdd( aCOMPDET_EMBED, { {| cPrefix, tmp1 | iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "dos", "djgpp"   , "i586-pc-msdosdjgpp-" , "/usr/local"           , NIL } )
            ENDCASE
         ENDIF

      #endif
   ENDIF

   /* Autodetect compiler */

   cPath_CompC := NIL

   IF hbmk[ _HBMK_lStopAfterHarbour ]
      /* If we're just compiling .prg to .c we do not need a C compiler. */
      hbmk[ _HBMK_cCOMP ] := ""
   ELSE
      IF Empty( hbmk[ _HBMK_cCOMP ] ) .OR. hbmk[ _HBMK_cCOMP ] == "bld"
         IF Len( aCOMPSUP ) == 1
            hbmk[ _HBMK_cCOMP ] := aCOMPSUP[ 1 ]
         ELSEIF HBMK_ISPLAT( "darwin|bsd|hpux|sunos|beos|qnx|android|vxworks|linux|cygwin|minix|aix" ) .OR. ;
                hbmk[ _HBMK_cCOMP ] == "bld"
            hbmk[ _HBMK_cCOMP ] := hb_Version( HB_VERSION_BUILD_COMP )
            IF AScan( aCOMPSUP, {| tmp | tmp == hbmk[ _HBMK_cCOMP ] } ) == 0
               hbmk[ _HBMK_cCOMP ] := NIL
            ENDIF
         ELSE
            IF Empty( hbmk[ _HBMK_cCOMP ] ) .AND. ! Empty( aCOMPDET )
               lDoSupportDetection := Empty( hbmk[ _HBMK_cHB_INSTALL_LIB ] ) .AND. ;
                                      hb_DirExists( hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + "lib" + hb_ps() + hbmk[ _HBMK_cPLAT ] )
               /* Check compilers */
               FOR tmp := 1 TO Len( aCOMPDET )
                  IF ! Empty( cPath_CompC := Eval( aCOMPDET[ tmp ][ _COMPDET_bBlock ] ) )
                     IF ! lDoSupportDetection .OR. ;
                        hb_DirExists( hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) ) + "lib" + ;
                                                        hb_ps() + hbmk[ _HBMK_cPLAT ] + ;
                                                        hb_ps() + aCOMPDET[ tmp ][ _COMPDET_cCOMP ] + ;
                                                        hb_DirSepToOS( hbmk[ _HBMK_cBUILD ] ) )
                        hbmk[ _HBMK_cCOMP ] := aCOMPDET[ tmp ][ _COMPDET_cCOMP ]
                        IF Len( aCOMPDET[ tmp ] ) >= _COMPDET_cCCPREFIX .AND. aCOMPDET[ tmp ][ _COMPDET_cCCPREFIX ] != NIL
                           hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET[ tmp ][ _COMPDET_cCCPREFIX ]
                        ENDIF
                        IF Len( aCOMPDET[ tmp ] ) >= _COMPDET_cCCSUFFIX .AND. aCOMPDET[ tmp ][ _COMPDET_cCCSUFFIX ] != NIL
                           hbmk[ _HBMK_cCCSUFFIX ] := aCOMPDET[ tmp ][ _COMPDET_cCCSUFFIX ]
                        ENDIF
                        tmp1 := hbmk[ _HBMK_cPLAT ]
                        IF Len( aCOMPDET[ tmp ] ) >= _COMPDET_cPLAT .AND. aCOMPDET[ tmp ][ _COMPDET_cPLAT ] != NIL
                           hbmk[ _HBMK_cPLAT ] := aCOMPDET[ tmp ][ _COMPDET_cPLAT ]
                        ENDIF
                        /* Hack autodetect watcom platform by looking at the header path config. TODO: Do it properly */
                        IF hbmk[ _HBMK_cCOMP ] == "watcom"
                           DO CASE
                           CASE FindInPath( "os2.h", GetEnv( "INCLUDE" ) ) != NIL
                              hbmk[ _HBMK_cPLAT ] := "os2"
                           CASE FindInPath( "dirent.h", GetEnv( "INCLUDE" ) ) != NIL
                              hbmk[ _HBMK_cPLAT ] := "linux"
                           CASE FindInPath( "windows.h", GetEnv( "INCLUDE" ) ) != NIL
                              hbmk[ _HBMK_cPLAT ] := "win"
                           OTHERWISE
                              hbmk[ _HBMK_cPLAT ] := "dos"
                           ENDCASE
                        ENDIF
                        IF !( hbmk[ _HBMK_cPLAT ] == tmp1 ) .AND. hbmk[ _HBMK_lInfo ]
                           _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected platform: %1$s (adjusted)" ), hbmk[ _HBMK_cPLAT ] ) )
                        ENDIF
                        EXIT
                     ELSE
                        IF hbmk[ _HBMK_lInfo ]
                           _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected C compiler '%1$s' skipped because required Harbour core libraries are not found." ), aCOMPDET[ tmp ][ _COMPDET_cCOMP ] ) )
                        ENDIF
                     ENDIF
                  ENDIF
               NEXT
            ENDIF
         ENDIF
         IF Empty( hbmk[ _HBMK_cCOMP ] )
            /* Autodetect embedded installations */
            FOR tmp := 1 TO Len( aCOMPDET_EMBED )
               IF hbmk[ _HBMK_cPLAT ] == aCOMPDET_EMBED[ tmp ][ _COMPDETE_cPLAT ] .AND. ;
                  ! Empty( cPath_CompC := Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bBlock ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPATH ] ) )
                  hbmk[ _HBMK_cCOMP ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCOMP ]
                  hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ]
                  hbmk[ _HBMK_cCCPATH ] := cPath_CompC
                  IF HB_ISBLOCK( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ] )
                     Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ], hbmk[ _HBMK_cPLAT ], hbmk[ _HBMK_cCOMP ], cPath_CompC )
                  ENDIF
                  EXIT
               ENDIF
            NEXT
         ENDIF
         IF ! Empty( hbmk[ _HBMK_cCOMP ] )
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected C compiler: %1$s" ), hbmk[ _HBMK_cCOMP ] ) )
            ENDIF
         ELSE
            IF Empty( aCOMPDET )
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( e"Choose a C compiler by using -comp= option.\nYou have the following choices on your platform: %1$s" ), ArrayToList( aCOMPSUP, ", " ) ) )
            ELSE
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( e"Could not detect any supported C compiler in your PATH.\nSetup one or set -comp= option to one of these values: %1$s" ), ArrayToList( aCOMPSUP, ", " ) ) )
            ENDIF
            RETURN _EXIT_UNKNCOMP
         ENDIF
      ELSE
         IF AScan( aCOMPSUP, {| tmp | tmp == hbmk[ _HBMK_cCOMP ] } ) == 0
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Compiler value unknown: %1$s" ), hbmk[ _HBMK_cCOMP ] ) )
            RETURN _EXIT_UNKNCOMP
         ENDIF
         /* Detect cross platform CCPREFIX and CCPATH if embedded installation is detected */
         FOR tmp := 1 TO Len( aCOMPDET_EMBED )
            IF aCOMPDET_EMBED[ tmp ][ _COMPDETE_cPLAT ] == hbmk[ _HBMK_cPLAT ] .AND. ;
               aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCOMP ] == hbmk[ _HBMK_cCOMP ] .AND. ;
               ! Empty( cPath_CompC := Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bBlock ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPATH ] ) )
               hbmk[ _HBMK_cCCPATH ] := cPath_CompC
               hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ]
               IF HB_ISBLOCK( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ] )
                  Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ], hbmk[ _HBMK_cPLAT ], hbmk[ _HBMK_cCOMP ], cPath_CompC )
               ENDIF
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   IF Empty( hbmk[ _HBMK_cCPU ] )
      hbmk[ _HBMK_cCPU ] := hbmk_CPU( hbmk )
   ENDIF

   /* Tweaks to compiler/platform environments */

   IF hbmk[ _HBMK_cCOMP ] == "bcc" /* TODO: Add support for bcc64 */
      /* NOTE: Hack to tweak bcc setup to include one additional
               compiler lib dir to lib search path. */
      IF Empty( cPath_CompC )
         cPath_CompC := _BCC_BIN_DETECT()
      ENDIF
      IF ! Empty( cPath_CompC )
         /* NOTE: Automatically configure bcc installation with missing configuration. [vszakats]
                  Permanently enabled. Apparently this is still top problem for bcc users. It is
                  also in sync this way with Harbour core build system. */
         IF .T. .OR. ;
            ! hb_FileExists( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Bin" + hb_ps() + "bcc32.cfg" ) .OR. ;
            ! hb_FileExists( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Bin" + hb_ps() + "ilink32.cfg" )
            /* NOTE: BCC 5.8 has different casing: 'include', 'lib', 'psdk' respectively. */
            AAdd( hbmk[ _HBMK_aINCPATH ], hb_PathNormalize( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Include" ) )
            AAdd( hbmk[ _HBMK_aLIBPATH ], hb_PathNormalize( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Lib" ) )
            /* NOTE: BCC 5.8 (and upper ?) thing */
            tmp := hb_PathNormalize( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Include" + hb_ps() + "dinkumware" )
            IF hb_DirExists( tmp )
               AAdd( hbmk[ _HBMK_aINCPATH ], tmp )
            ENDIF
            /* NOTE: BCC 6.5 (and upper ?) thing */
            FOR EACH tmp IN { "crtl", "rtl", "sdk" }
               tmp := hb_PathNormalize( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Include" + hb_ps() + "windows" + hb_ps() + tmp )
               IF hb_DirExists( tmp )
                  AAdd( hbmk[ _HBMK_aINCPATH ], tmp )
               ENDIF
            NEXT
         ENDIF
         AAdd( hbmk[ _HBMK_aLIBPATH ], hb_PathNormalize( hb_FNameDir( cPath_CompC ) + ".." + hb_ps() + "Lib" + hb_ps() + "PSDK" ) )
      ENDIF
   ENDIF

   DO CASE
   CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
      AAdd( hbmk[ _HBMK_aINCPATH ], hb_DirSepToOS( GetEnv( "WIND_BASE" ) + "/target/usr/h" ) )
      AAdd( hbmk[ _HBMK_aINCPATH ], hb_DirSepToOS( GetEnv( "WIND_BASE" ) + "/target/usr/h/wrn/coreip" ) )
#if 0
   CASE hbmk[ _HBMK_cPLAT ] == "bsd"
      IF hb_DirExists( "/usr/local/lib" ) /* For ports */
         AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" )
      ENDIF
      IF hb_DirExists( "/usr/local/include" )
         AAdd( hbmk[ _HBMK_aINCPATH ], "/usr/local/include" )
      ENDIF
      IF hb_DirExists( "/usr/pkg/lib" ) /* For pkgsrc */
         AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/pkg/lib" )
      ENDIF
      IF hb_DirExists( "/usr/pkg/include" )
         AAdd( hbmk[ _HBMK_aINCPATH ], "/usr/pkg/include" )
      ENDIF
#endif
   ENDCASE

   /* Tweaks to compiler setup */

   IF hbmk[ _HBMK_cCOMP ] == "djgpp"
      hbmk[ _HBMK_cDynLibExt ] := ".dxe"
   ENDIF

   /* Detect compiler version (where applicable) */

   IF hbmk[ _HBMK_nCOMPVer ] == 0 .AND. ! Empty( cPath_CompC )

      DO CASE
      CASE ( hbmk[ _HBMK_cPLAT ] == "cygwin" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" )

         IF File( hb_FNameDir( cPath_CompC ) + "i686-pc-cygwin-gcc-3.4" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 34
         ENDIF

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw64" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingwarm" )

         DO CASE
         CASE File( hb_FNameDir( cPath_CompC ) + "mingw32-gcc-4.7" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "i686-w64-mingw32-gcc-4.7" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "x86_64-pc-mingw32-gcc-4.7" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "x86_64-w64-mingw32-gcc-4.7" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 47
         CASE File( hb_FNameDir( cPath_CompC ) + "mingw32-gcc-4.6" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "i686-w64-mingw32-gcc-4.6" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "x86_64-pc-mingw32-gcc-4.6" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "x86_64-w64-mingw32-gcc-4.6" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 46
         CASE File( hb_FNameDir( cPath_CompC ) + "mingw32-gcc-4.5" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "i686-w64-mingw32-gcc-4.5" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "x86_64-pc-mingw32-gcc-4.5" + hb_osFileMask() ) .OR. ;
              File( hb_FNameDir( cPath_CompC ) + "x86_64-w64-mingw32-gcc-4.5" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 45
         CASE File( hb_FNameDir( cPath_CompC ) + "mingw32-gcc-4.4" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 44
         CASE File( hb_FNameDir( cPath_CompC ) + "mingw32-gcc-4.3" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 43
         CASE File( hb_FNameDir( cPath_CompC ) + "mingw32-gcc-3.4" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 34
         ENDCASE

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. HBMK_ISCOMP( "msvc|msvc64|msvcia64|icc|iccia64" ) ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "msvcarm" ) /* NOTE: Cross-platform: wce/ARM on win/x86 */

         /* Compatibility with Harbour GNU Make system */
         IF hbmk[ _HBMK_cCOMP ] == "msvcarm" .AND. "clarm.exe" $ cPath_CompC
            hbmk[ _HBMK_nCOMPVer ] := 1310 /* Visual Studio .NET 2003 */
         ELSE
            DO CASE
            CASE "VC98" $ cPath_CompC
               hbmk[ _HBMK_nCOMPVer ] := 1200
            CASE "2003" $ cPath_CompC
               hbmk[ _HBMK_nCOMPVer ] := 1300
            CASE "8" + hb_ps() $ cPath_CompC /* Visual Studio 2005 */
               hbmk[ _HBMK_nCOMPVer ] := 1400
            CASE "9.0" $ cPath_CompC /* Visual Studio 2008 or Windows SDK 7.0 */
               hbmk[ _HBMK_nCOMPVer ] := 1500
            CASE "10.0" $ cPath_CompC /* Visual Studio 2010 or Windows SDK 7.1 */
               hbmk[ _HBMK_nCOMPVer ] := 1600
            CASE "11.0" $ cPath_CompC /* Visual Studio 2012 */
               hbmk[ _HBMK_nCOMPVer ] := 1700
            OTHERWISE
               hbmk[ _HBMK_nCOMPVer ] := 1400
            ENDCASE
         ENDIF
      ENDCASE
   ENDIF

   /* Finish detecting bin/lib/include dirs */

   hbmk_harbour_dirlayout_init( hbmk )

   /* Display detection results */

   IF hbmk[ _HBMK_lInfo ]
      _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using Harbour: %1$s %2$s %3$s %4$s %5$s %6$s" ), ;
         hbmk[ _HBMK_cHB_INSTALL_BIN ], ;
         hbmk[ _HBMK_cHB_INSTALL_INC ], ;
         hbmk[ _HBMK_cHB_INSTALL_LIB ], ;
         hbmk[ _HBMK_cHB_INSTALL_DYN ], ;
         hbmk[ _HBMK_cHB_INSTALL_CON ], ;
         hbmk[ _HBMK_cHB_INSTALL_ADD ] ) )
      IF ! Empty( cPath_CompC )
         IF Empty( hbmk[ _HBMK_cCCPREFIX ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCSUFFIX ] )
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using C compiler: %1$s" ), cPath_CompC ) )
         ELSE
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using C compiler: %1$s [%2$s...%3$s]" ), cPath_CompC, hbmk[ _HBMK_cCCPREFIX ], hbmk[ _HBMK_cCCSUFFIX ] ) )
         ENDIF
      ENDIF
   ENDIF

   /* Build with shared libs by default, if we're installed to default system locations. */

   IF hbmk[ _HBMK_lSysLoc ] .AND. HBMK_ISPLAT( "darwin|bsd|hpux|sunos|beos|qnx|android|vxworks|linux|cygwin|aix" )
      hbmk[ _HBMK_lSHARED ] := .T.
      hbmk[ _HBMK_lSTATICFULL ] := .F.
   ELSE
      hbmk[ _HBMK_lSHARED ] := .F.
      hbmk[ _HBMK_lSTATICFULL ] := .F.
   ENDIF

   /* Process command line */

   hbmk_init_stage2( hbmk )

   hbmk[ _HBMK_lSAFE ] := HBMK_ISPLAT( "win" ) /* TODO: later enable this for all platforms */

   l_aOPTRUN := {}
   l_aOBJA := {}
   l_cLIBSELF := NIL
   l_cIMPLIBDIR := NIL
   l_cIMPLIBNAME := NIL

   aParams := {}

   /* Process build-time configuration */

   #if defined( HB_HAS_WATT )
      IF hbmk[ _HBMK_cPLAT ] == "dos"
         SWITCH hbmk[ _HBMK_cCOMP ]
         CASE "djgpp"  ; AAdd( hbmk[ _HBMK_aLIBUSERSYS ], "watt" ) ; EXIT
         CASE "watcom" ; AAdd( hbmk[ _HBMK_aLIBUSERSYS ], "wattcpwf" ) ; EXIT
         ENDSWITCH
         IF hb_DirExists( tmp := hb_DirSepToOS( GetEnv( "WATT_ROOT" ) ) + hb_ps() + "lib" )
            AAdd( hbmk[ _HBMK_aLIBPATH ], tmp )
         ENDIF
      ENDIF
   #endif

   /* Process automatic make files in current dir. */
   IF hbmk[ _HBMK_lAutoHBM ] .AND. hb_FileExists( _HBMK_AUTOHBM_NAME )
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing local make script: %1$s" ), _HBMK_AUTOHBM_NAME ) )
      ENDIF
      HBM_Load( hbmk, aParams, _HBMK_AUTOHBM_NAME, 1, .F., _HBMK_AUTOHBM_NAME ) /* Do not allow sub-projects in automatic make file */
   ENDIF

   /* Collect all command line parameters */
   FOR EACH cParam IN aArgs
      DO CASE
      CASE !( Left( cParam, 1 ) == "-" ) .AND. Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" .AND. ;
         !( Lower( hb_FNameExt( cParam ) ) == ".clp" )
         cParam := SubStr( cParam, 2 )
         IF Empty( hb_FNameExt( cParam ) )
            cParam := hb_FNameExtSet( cParam, ".hbm" )
         ENDIF
         IF !( Lower( hb_FNameExt( cParam ) ) == ".hbm" ) .AND. lAcceptLDClipper
            rtlnk_process( hbmk, MemoRead( hb_DirSepToOS( cParam ) ), @hbmk[ _HBMK_cPROGNAME ], @hbmk[ _HBMK_aOBJUSER ], @hbmk[ _HBMK_aLIBUSER ], @hbmk[ _HBMK_aLIBPATH ] )
            IF ! Empty( hbmk[ _HBMK_aOBJUSER ] )
               hb_default( @hbmk[ _HBMK_cFIRST ], hbmk[ _HBMK_aOBJUSER ][ 1 ] )
            ENDIF
         ELSE
            tmp := HBM_Load( hbmk, aParams, hb_DirSepToOS( cParam ), 1, .T., hb_DirSepToOS( cParam ) ) /* Load parameters from script file */
            IF tmp != _EXIT_OK .AND. ;
               tmp != _EXIT_STOP
               RETURN tmp
            ENDIF
         ENDIF
      CASE !( Left( cParam, 1 ) == "-" ) .AND. ;
           ( Lower( hb_FNameExt( cParam ) ) == ".hbm" .OR. ;
             Lower( hb_FNameExt( cParam ) ) == ".hbp" )

         cParam := hb_DirSepToOS( cParam )

         /* search for .hbp files in macro libpaths */
         IF Lower( hb_FNameExt( cParam ) ) == ".hbp" .AND. ! hbmk_hb_FileExists( cParam )
            FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
               IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ tmp .AND. hbmk_hb_FileExists( hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cParam ) )
                  cParam := hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cParam )
                  IF hbmk[ _HBMK_lInfo ]
                     _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Found project reference on library search path: %1$s" ), cParam ) )
                  ENDIF
                  EXIT
               ENDIF
            NEXT
         ENDIF

         tmp := HBM_Load( hbmk, aParams, cParam, 1, .T., cParam ) /* Load parameters from script file */
         IF tmp != _EXIT_OK .AND. ;
            tmp != _EXIT_STOP
            RETURN tmp
         ENDIF
      OTHERWISE
         AAdd( aParams, _PAR_NEW( cParam, "", 0 ) )
      ENDCASE
   NEXT

   /* Process automatic control files. */
   HBC_ProcessAuto( hbmk )

   /* Process command line (2nd pass) */
   FOR EACH aParam IN aParams

      cParam := ArchCompFilter( hbmk, aParam[ _PAR_cParam ], aParam[ _PAR_cFileName ] )
      cParamL := Lower( cParam )

      DO CASE
      CASE Empty( cParam )
         /* do nothing */
      CASE Left( cParamL, 6 )  == "-comp=" .OR. ;
           Left( cParamL, 6 )  == "-plat=" .OR. ;
           Left( cParamL, 10 ) == "-compiler=" .OR. ; /* Compatibility HB_LEGACY_LEVEL4 */
           Left( cParamL, 10 ) == "-platform=" .OR. ; /* Compatibility HB_LEGACY_LEVEL4 */
           Left( cParamL, 5 )  == "-cpu=" .OR. ;
           Left( cParamL, 7 )  == "-build=" .OR. ;
           Left( cParamL, 6 )  == "-lang=" .OR. ;
           Left( cParamL, 4 )  == "-shl" .OR. ;
           Left( cParamL, 7 )  == "-width=" .OR. ;
           cParamL             == "-autohbm" .OR. ;
           cParamL             == "-autohbm-" .OR. ;
           cParamL             == "-hbrun" .OR. ;
           cParamL             == "-hbraw" .OR. ;
           cParamL             == "-hbcmp" .OR. ;
           cParamL             == "-hbcc"  .OR. ;
           cParamL             == "-hblnk" .OR. ;
           cParamL             == "-xhb" .OR. ;
           cParamL             == "-hb10" .OR. ;
           cParamL             == "-hb20" .OR. ;
           cParamL             == "-hb30" .OR. ;
           cParamL             == "-hbc" .OR. ;
           cParamL             == "-clipper" .OR. ;
           cParamL             == "-rtlink" .OR. ;
           cParamL             == "-blinker" .OR. ;
           cParamL             == "-exospace"

         /* Command line option were already processed in the first pass, ignore those. */

         IF ! Empty( aParam[ _PAR_cFileName ] )
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignored option valid only on command-line: %1$s" ), ParamToString( aParam ) ) )
         ENDIF

      /* -env options used inside makefiles */
      CASE Left( cParamL, 5 )  == "-env:"

         IF ! Empty( aParam[ _PAR_cFileName ] )
            ProcEnvOption( SubStr( cParam, 6 ) )
         ENDIF

      CASE cParamL == "-quiet"           ; hbmk[ _HBMK_lQuiet ] := .T. ; hbmk[ _HBMK_lInfo ] := .F.
      CASE cParamL == "-quiet-"

         IF ! hbmk[ _HBMK_lDumpInfo ]
            hbmk[ _HBMK_lQuiet ] := .F.
         ENDIF

      CASE cParamL == "-info"

         IF ! hbmk[ _HBMK_lDumpInfo ]
            hbmk[ _HBMK_lInfo ] := .T.
         ENDIF

      CASE cParamL == "-pause"           ; lPause := .T.
      CASE cParamL == "-exitstr"         ; lExitStr := .T.

      CASE cParamL == "-hbexe"

         IF ! l_lTargetSelected
            l_lTargetSelected := .T.
            hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .F. ; hbmk[ _HBMK_lCreateLib ] := .F. ; Set_lCreateDyn( hbmk, .F. ) ; hbmk[ _HBMK_lCreateImpLib ] := .F.
         ENDIF

      CASE cParamL == "-hblib"

         IF ! l_lTargetSelected
            l_lTargetSelected := .T.
            hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .T. ; hbmk[ _HBMK_lCreateLib ] := .T. ; Set_lCreateDyn( hbmk, .F. ) ; hbmk[ _HBMK_lCreateImpLib ] := .F.
         ENDIF

      CASE cParamL == "-hbdyn"

         IF ! l_lTargetSelected
            l_lTargetSelected := .T.
            hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .T. ; hbmk[ _HBMK_lCreateLib ] := .F. ; Set_lCreateDyn( hbmk, .T. ) ; hbmk[ _HBMK_lCreateImpLib ] := .F. ; hbmk[ _HBMK_lDynVM ] := .F. ; l_lNOHBLIB := .T.
         ENDIF

      CASE cParamL == "-hbdynvm"

         IF ! l_lTargetSelected
            l_lTargetSelected := .T.
            hbmk[ _HBMK_lStopAfterHarbour ] := .F. ; lStopAfterCComp := .T. ; hbmk[ _HBMK_lCreateLib ] := .F. ; Set_lCreateDyn( hbmk, .T. ) ; hbmk[ _HBMK_lCreateImpLib ] := .F. ; hbmk[ _HBMK_lDynVM ] := .T. ; l_lNOHBLIB := .F.
         ENDIF

      CASE cParamL == "-hbcontainer"

         IF ! l_lTargetSelected
            l_lTargetSelected := .T.
            hbmk[ _HBMK_lContainer ] := .T. ; hbmk[ _HBMK_lStopAfterInit ] := .T. ; hbmk[ _HBMK_lCreateLib ] := .F. ; Set_lCreateDyn( hbmk, .F. ) ; hbmk[ _HBMK_lCreateImpLib ] := .F.
         ENDIF

      CASE cParamL == "-hbimplib"

         IF ! l_lTargetSelected
            l_lTargetSelected := .T.
            hbmk[ _HBMK_lCreateImpLib ] := .T. ; lAcceptIFlag := .T.
         ENDIF

      CASE cParamL == "-gui"             ; hbmk[ _HBMK_lGUI ]       := .T.
      CASE cParamL == "-std"             ; hbmk[ _HBMK_lGUI ]       := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-mwindows"        ; hbmk[ _HBMK_lGUI ]       := .T. ; LegacyWarning( hbmk, aParam, "-gui" ) /* Compatibility */
      CASE cParamL == "-mconsole"        ; hbmk[ _HBMK_lGUI ]       := .F. ; LegacyWarning( hbmk, aParam, "-std" ) /* Compatibility */
#endif
      CASE cParamL == "-mt"              ; hbmk[ _HBMK_lMT ]        := .T.
      CASE cParamL == "-st"              ; hbmk[ _HBMK_lMT ]        := .F.
      CASE cParamL == "-shared"          ; hbmk[ _HBMK_lSHARED ]    := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL
      CASE cParamL == "-fullshared"      ; hbmk[ _HBMK_lSHARED ]    := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := .T.
      CASE cParamL == "-fixshared"       ; hbmk[ _HBMK_lSHARED ]    := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := .F.
      CASE cParamL == "-static"          ; hbmk[ _HBMK_lSHARED ]    := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL
      CASE cParamL == "-fullstatic"      ; hbmk[ _HBMK_lSHARED ]    := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .T. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL
      CASE cParamL == "-pic"             ; hbmk[ _HBMK_lPIC ]       := .T.
      CASE cParamL == "-pic-"            ; hbmk[ _HBMK_lPIC ]       := .F.
      CASE cParamL == "-nohblib"         ; l_lNOHBLIB := .T.
      CASE cParamL == "-nohblib-"        ; l_lNOHBLIB := .F.
      CASE cParamL == "-nomiscsyslib"    ; l_lLIBSYSMISC := .F.
      CASE cParamL == "-nomiscsyslib-"   ; l_lLIBSYSMISC := .T.
      CASE cParamL == "-nolibgrouping"   ; l_lLIBGROUPING := .F.
      CASE cParamL == "-nolibgrouping-"  ; l_lLIBGROUPING := .T.
      CASE cParamL == "-bldf"            ; hbmk[ _HBMK_lBLDFLGP ] := hbmk[ _HBMK_lBLDFLGC ] := hbmk[ _HBMK_lBLDFLGL ] := .T.
      CASE cParamL == "-bldf-"           ; hbmk[ _HBMK_lBLDFLGP ] := hbmk[ _HBMK_lBLDFLGC ] := hbmk[ _HBMK_lBLDFLGL ] := .F.
      CASE Left( cParamL, 6 ) == "-bldf="
         cParam := SubStr( cParam, 7 )
         hbmk[ _HBMK_lBLDFLGP ] := "p" $ cParam
         hbmk[ _HBMK_lBLDFLGC ] := "c" $ cParam
         hbmk[ _HBMK_lBLDFLGL ] := "l" $ cParam
      CASE cParamL == "-debug"           ; hbmk[ _HBMK_lDEBUG ]       := .T.
      CASE cParamL == "-debug-"          ; hbmk[ _HBMK_lDEBUG ]       := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nodebug"         ; hbmk[ _HBMK_lDEBUG ]       := .F. ; LegacyWarning( hbmk, aParam, "-debug-" ) /* Compatibility */
#endif
      CASE cParamL == "-optim"           ; hbmk[ _HBMK_lOPTIM ]       := .T.
      CASE cParamL == "-optim-"          ; hbmk[ _HBMK_lOPTIM ]       := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nooptim"         ; hbmk[ _HBMK_lOPTIM ]       := .F. ; LegacyWarning( hbmk, aParam, "-optim-" ) /* Compatibility */
#endif
      CASE cParamL == "-debugtime"       ; hbmk[ _HBMK_lDEBUGTIME ]   := .T.
      CASE cParamL == "-debuginc"        ; hbmk[ _HBMK_lDEBUGINC ]    := .T.
      CASE cParamL == "-debugstub"       ; hbmk[ _HBMK_lDEBUGSTUB ]   := .T.
      CASE cParamL == "-debugi18n"       ; hbmk[ _HBMK_lDEBUGI18N ]   := .T.
      CASE cParamL == "-debugdepd"       ; hbmk[ _HBMK_lDEBUGDEPD ]   := .T.
      CASE cParamL == "-debugpars"       ; hbmk[ _HBMK_lDEBUGPARS ]   := .T.
      CASE cParamL == "-debugrte"        ; nLevel += cParamL /* invalid code to trigger RTE */
      CASE cParamL == "-nulrdd"          ; hbmk[ _HBMK_lNULRDD ]      := .T.
      CASE cParamL == "-nulrdd-"         ; hbmk[ _HBMK_lNULRDD ]      := .F.
      CASE cParamL == "-nodefgt"         ; hbmk[ _HBMK_aLIBCOREGT ]   := {}
      CASE cParamL == "-nodefgt-"        ; hbmk[ _HBMK_aLIBCOREGT ]   := hbmk[ _HBMK_aLIBCOREGTDEF ]
      CASE cParamL == "-map"             ; hbmk[ _HBMK_lMAP ]         := .T.
      CASE cParamL == "-map-"            ; hbmk[ _HBMK_lMAP ]         := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nomap"           ; hbmk[ _HBMK_lMAP ]         := .F. ; LegacyWarning( hbmk, aParam, "-map-" ) /* Compatibility */
#endif
      CASE cParamL == "-implib"          ; hbmk[ _HBMK_lIMPLIB ]      := .T.
      CASE cParamL == "-implib-"         ; hbmk[ _HBMK_lIMPLIB ]      := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-noimplib"        ; hbmk[ _HBMK_lIMPLIB ]      := .F. ; LegacyWarning( hbmk, aParam, "-implib-" ) /* Compatibility */
#endif
      CASE cParamL == "-winuni"          ; hbmk[ _HBMK_lWINUNI ]      := .T.
      CASE cParamL == "-winuni-"         ; hbmk[ _HBMK_lWINUNI ]      := .F.
      CASE cParamL == "-beep"            ; hbmk[ _HBMK_lBEEP ]        := .T.
      CASE cParamL == "-beep-"           ; hbmk[ _HBMK_lBEEP ]        := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nobeep"          ; hbmk[ _HBMK_lBEEP ]        := .F. ; LegacyWarning( hbmk, aParam, "-beep-" ) /* Compatibility */
#endif
      CASE cParamL == "-rebuild"

         hbmk[ _HBMK_lINC ] := .T.
         IF nLevel == 1
            hbmk[ _HBMK_lREBUILD ] := .T.
            IncPointlessPair( hbmk, @aParamINC, aParam, cParamL, "-rebuild" )
         ENDIF

      CASE cParamL == "-rebuildall"

         hbmk[ _HBMK_lINC ] := .T.
         hbmk[ _HBMK_lREBUILD ] := .T.
         IncPointlessPair( hbmk, @aParamINC, aParam, cParamL, "-rebuildall" )

      CASE cParamL == "-rebuildpo"       ; hbmk[ _HBMK_lREBUILDPO ]   := .T.
      CASE cParamL == "-minipo"          ; hbmk[ _HBMK_lMINIPO ]      := .T.
      CASE cParamL == "-minipo-"         ; hbmk[ _HBMK_lMINIPO ]      := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nominipo"        ; hbmk[ _HBMK_lMINIPO ]      := .F. ; LegacyWarning( hbmk, aParam, "-minipo-" ) /* Compatibility */
#endif
      CASE cParamL == "-clean"           ; hbmk[ _HBMK_lINC ]         := .T. ; hbmk[ _HBMK_lCLEAN ] := .T.
      CASE cParamL == "-inc"             ; hbmk[ _HBMK_lINC ]         := .T. ; IncPointlessPair( hbmk, @aParamINC, aParam, cParamL, "-inc" )
      CASE cParamL == "-inc-"            ; hbmk[ _HBMK_lINC ]         := .F. ; aParamINC := NIL
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-noinc"           ; hbmk[ _HBMK_lINC ]         := .F. ; LegacyWarning( hbmk, aParam, "-inc-" ) /* Compatibility */
#endif
      CASE cParamL == "-ignore"          ; hbmk[ _HBMK_lIGNOREERROR ] := .T.
      CASE cParamL == "-ignore-"         ; hbmk[ _HBMK_lIGNOREERROR ] := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-noignore"        ; hbmk[ _HBMK_lIGNOREERROR ] := .F. ; LegacyWarning( hbmk, aParam, "-ignore-" ) /* Compatibility */
#endif
      CASE cParamL == "-hbcppmm"         ; hbmk[ _HBMK_lHBCPPMM ]     := .T.
      CASE cParamL == "-hbcppmm-"        ; hbmk[ _HBMK_lHBCPPMM ]     := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nohbcppmm"       ; hbmk[ _HBMK_lHBCPPMM ]     := .F. ; LegacyWarning( hbmk, aParam, "-hbcppmm-" ) /* Compatibility */
#endif
      CASE cParamL == "-strip"           ; hbmk[ _HBMK_lSTRIP ]       := .T.
      CASE cParamL == "-strip-"          ; hbmk[ _HBMK_lSTRIP ]       := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nostrip"         ; hbmk[ _HBMK_lSTRIP ]       := .F. ; LegacyWarning( hbmk, aParam, "-strip-" ) /* Compatibility */
#endif
      CASE cParamL == "-depimplib"       ; hbmk[ _HBMK_lDEPIMPLIB ]   := .T.
      CASE cParamL == "-depimplib-"      ; hbmk[ _HBMK_lDEPIMPLIB ]   := .F.
      CASE cParamL == "-instforce"       ; hbmk[ _HBMK_lInstForce ]   := .T.
      CASE cParamL == "-instforce-"      ; hbmk[ _HBMK_lInstForce ]   := .F.

      CASE cParamL == "--harbourhelp"    ; AAdd( hbmk[ _HBMK_aOPTPRG ], "--help" ) ; lHarbourInfo := .T.
      CASE cParamL == "-harbourhelp"     ; AAdd( hbmk[ _HBMK_aOPTPRG ], "--help" ) ; lHarbourInfo := .T.
      CASE cParamL == "-build"           ; AAdd( hbmk[ _HBMK_aOPTPRG ], "-build" ) ; lHarbourInfo := .T.
      CASE cParamL == "-credits"         ; AAdd( hbmk[ _HBMK_aOPTPRG ], "-credits" ) ; lHarbourInfo := .T.

      CASE cParamL == "-warn"               ; hbmk[ _HBMK_nWARN ] := _WARN_YES /* synonym to -warn=yes */
      CASE cParamL == "-warn-"              ; hbmk[ _HBMK_nWARN ] := _WARN_NO /* synonym to -warn=no */

      CASE Left( cParamL, 6 ) == "-warn="

         DO CASE
         CASE SubStr( cParamL, 7 ) == "def" ; hbmk[ _HBMK_nWARN ] := _WARN_DEF
         CASE SubStr( cParamL, 7 ) == "yes" ; hbmk[ _HBMK_nWARN ] := _WARN_YES
         CASE SubStr( cParamL, 7 ) == "no"  ; hbmk[ _HBMK_nWARN ] := _WARN_NO
         CASE SubStr( cParamL, 7 ) == "low" ; hbmk[ _HBMK_nWARN ] := _WARN_LOW
         CASE SubStr( cParamL, 7 ) == "max" ; hbmk[ _HBMK_nWARN ] := _WARN_MAX
#ifdef HB_LEGACY_LEVEL4
         OTHERWISE                          ; hbmk[ _HBMK_nWARN ] := _WARN_YES ; LegacyWarning( hbmk, aParam, "-warn=yes" ) /* Compatibility */
#else
         OTHERWISE                          ; InvalidOptionValue( hbmk, aParam )
#endif
         ENDCASE

#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nowarn"             ; hbmk[ _HBMK_nWARN ] := _WARN_NO ; LegacyWarning( hbmk, aParam, "-warn=no" ) /* Compatibility */
#endif

      CASE cParamL == "-safe"               ; hbmk[ _HBMK_lSAFE ] := .T.
      CASE cParamL == "-safe-"              ; hbmk[ _HBMK_lSAFE ] := .F.

      CASE cParamL == "-compr"              ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF /* synonym to -compr=yes */
      CASE cParamL == "-compr-"             ; hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF /* synonym to -compr=no */

      CASE Left( cParamL, 7 ) == "-compr="

         DO CASE
#ifdef HB_LEGACY_LEVEL4
         CASE SubStr( cParamL, 8 ) == "def" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF ; LegacyWarning( hbmk, aParam, "-compr=yes" ) /* Compatibility */
#endif
         CASE SubStr( cParamL, 8 ) == "yes" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
         CASE SubStr( cParamL, 8 ) == "no"  ; hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF
         CASE SubStr( cParamL, 8 ) == "min" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MIN
         CASE SubStr( cParamL, 8 ) == "max" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MAX
#ifdef HB_LEGACY_LEVEL4
         OTHERWISE                          ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF ; LegacyWarning( hbmk, aParam, "-compr=yes" ) /* Compatibility */
#else
         OTHERWISE                          ; InvalidOptionValue( hbmk, aParam )
#endif
         ENDCASE

#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nocompr"            ; hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF ; LegacyWarning( hbmk, aParam, "-compr=no" ) /* Compatibility */
#endif
      CASE Left( cParamL, 6 ) == "-head="

         DO CASE
         CASE SubStr( cParamL, 7 ) == "off"    ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF
         CASE SubStr( cParamL, 7 ) == "full"   ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
         CASE SubStr( cParamL, 7 ) == "native" ; hbmk[ _HBMK_nHEAD ] := _HEAD_NATIVE
         CASE SubStr( cParamL, 7 ) == "dep"    ; hbmk[ _HBMK_nHEAD ] := _HEAD_DEP
#ifdef HB_LEGACY_LEVEL4
         OTHERWISE                             ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL ; LegacyWarning( hbmk, aParam, "-head=full" ) /* Compatibility */
#else
         OTHERWISE                             ; InvalidOptionValue( hbmk, aParam )
#endif
         ENDCASE

#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-head"                  ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL ; LegacyWarning( hbmk, aParam, "-head=full" ) /* Compatibility */
      CASE cParamL == "-head-"                 ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF ; LegacyWarning( hbmk, aParam, "-head=off" ) /* Compatibility */
      CASE cParamL == "-nohead"                ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF ; LegacyWarning( hbmk, aParam, "-head=off" ) /* Compatibility */
#endif

      CASE Left( cParamL, 5 ) == "-cpp="

         DO CASE
         CASE SubStr( cParamL, 6 ) == "def"   ; hbmk[ _HBMK_lCPP ] := NIL
         CASE SubStr( cParamL, 6 ) == "yes"   ; hbmk[ _HBMK_lCPP ] := .T.
         CASE SubStr( cParamL, 6 ) == "no"    ; hbmk[ _HBMK_lCPP ] := .F.
         CASE SubStr( cParamL, 6 ) == "iso98" ; hbmk[ _HBMK_cCPP ] := "iso98"
         CASE SubStr( cParamL, 6 ) == "iso03" ; hbmk[ _HBMK_cCPP ] := "iso03"
         CASE SubStr( cParamL, 6 ) == "iso0x" ; hbmk[ _HBMK_cCPP ] := "iso0x"
         CASE SubStr( cParamL, 6 ) == "gnu98" ; hbmk[ _HBMK_cCPP ] := "gnu98"
         CASE SubStr( cParamL, 6 ) == "gnu0x" ; hbmk[ _HBMK_cCPP ] := "gnu0x"
         CASE SubStr( cParamL, 6 ) == ""      ; hbmk[ _HBMK_cCPP ] := ""
         ENDCASE

         /* dangerous? disable it */
         hbmk[ _HBMK_cCPP ] := ""

      CASE Left( cParamL, 3 ) == "-c="

         DO CASE
         CASE SubStr( cParamL, 4 ) == "iso90"  ; hbmk[ _HBMK_cC ] := "iso90"
         CASE SubStr( cParamL, 4 ) == "iso99"  ; hbmk[ _HBMK_cC ] := "iso99"
         CASE SubStr( cParamL, 4 ) == "iso1x"  ; hbmk[ _HBMK_cC ] := "iso1x"
         CASE SubStr( cParamL, 4 ) == "gnu90"  ; hbmk[ _HBMK_cC ] := "gnu90"
         CASE SubStr( cParamL, 4 ) == "gnu99"  ; hbmk[ _HBMK_cC ] := "gnu99"
         CASE SubStr( cParamL, 4 ) == "gnu1x"  ; hbmk[ _HBMK_cC ] := "gnu1x"
         CASE SubStr( cParamL, 4 ) == ""       ; hbmk[ _HBMK_cC ] := ""
         ENDCASE

         /* dangerous? disable it */
         hbmk[ _HBMK_cC ] := ""

      CASE cParamL == "-cpp"             ; hbmk[ _HBMK_lCPP ]       := .T. /* synonym to -cpp=yes */
      CASE cParamL == "-cpp-"            ; hbmk[ _HBMK_lCPP ]       := .F. /* synonym to -cpp=no */
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-nocpp"           ; hbmk[ _HBMK_lCPP ]       := .F. ; LegacyWarning( hbmk, aParam, "-cpp-" ) /* Compatibility */
#endif

      CASE cParamL == "-run"

         IF hbmk[ _HBMK_nLevel ] == 1
            hbmk[ _HBMK_lRUN ] := .T.
         ENDIF

      CASE cParamL == "-run-"            ; hbmk[ _HBMK_lRUN ]       := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-norun"           ; hbmk[ _HBMK_lRUN ]       := .F. ; LegacyWarning( hbmk, aParam, "-run-" ) /* Compatibility */
#endif

      CASE cParamL == "-trace"

         IF ! hbmk[ _HBMK_lDumpInfo ]
            hbmk[ _HBMK_lTRACE ]     := .T.
         ENDIF

      CASE cParamL == "-trace-"          ; hbmk[ _HBMK_lTRACE ]     := .F.
#ifdef HB_LEGACY_LEVEL4
      CASE cParamL == "-notrace"         ; hbmk[ _HBMK_lTRACE ]     := .F. ; LegacyWarning( hbmk, aParam, "-trace-" ) /* Compatibility */
#endif
      CASE cParamL == "-traceonly"       ; hbmk[ _HBMK_lTRACE ]     := .T. ; hbmk[ _HBMK_lDONTEXEC ] := .T.

      CASE cParamL == "--hbdirbin"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( hbmk[ _HBMK_cHB_INSTALL_BIN ] )

      CASE cParamL == "--hbdirdyn"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( hbmk[ _HBMK_cHB_INSTALL_DYN ] )

      CASE cParamL == "--hbdirlib"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( hbmk[ _HBMK_cHB_INSTALL_LIB ] )

      CASE cParamL == "--hbdirinc"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( hbmk[ _HBMK_cHB_INSTALL_INC ] )

      CASE Left( cParamL, Len( "--hbinfo" ) ) == "--hbinfo"

         hbmk[ _HBMK_lDumpInfo ] := .T.
         lDumpInfoNested := ( SubStr( cParamL, Len( "--hbinfo" ) + 1 ) == "=nested" )

         hbmk[ _HBMK_lQuiet ] := .T.
         hbmk[ _HBMK_lInfo ] := .F.
         hbmk[ _HBMK_lTRACE ] := .F.

      CASE Left( cParamL, Len( "-jobs=" ) ) == "-jobs="

         cParam := SubStr( cParam, Len( "-jobs=" ) + 1 )
         IF hb_mtvm() .AND. Val( cParam ) > 0
            l_nJOBS := Val( cParam )
         ENDIF

         HB_SYMBOL_UNUSED( l_nJOBS )

      CASE Left( cParamL, 5 ) == "-lng="

         cParam := SubStr( cParam, 6 )
         IF ! Empty( cParam )
            hbmk[ _HBMK_aLNG ] := ListToArray( cParam, "," )
            FOR EACH tmp IN hbmk[ _HBMK_aLNG ]
               tmp := AllTrim( tmp )
            NEXT
         ENDIF

      CASE Left( cParamL, 5 ) == "-hbl="

         hbmk[ _HBMK_cHBL ] := hb_DirSepToOS( SubStr( cParam, 6 ) )
         hbmk[ _HBMK_cHBLDir ] := hb_FNameDir( aParam[ _PAR_cFileName ] )

      CASE Left( cParamL, 4 ) == "-po="

         hbmk[ _HBMK_cPO ] := PathMakeAbsolute( hb_DirSepToOS( SubStr( cParam, 5 ) ), hb_FNameDir( aParam[ _PAR_cFileName ] ) )

      CASE Left( cParamL, 4 ) == "-hbl"

         hbmk[ _HBMK_cHBL ] := ""
         hbmk[ _HBMK_cHBLDir ] := ""

      CASE Left( cParamL, 5 ) == "-hbx="

         cParam := MacroProc( hbmk, SubStr( cParam, 6 ), aParam[ _PAR_cFileName ] )
         IF Empty( cParam )
            hbmk[ _HBMK_cHBX ] := NIL
         ELSE
            hbmk[ _HBMK_cHBX ] := PathMakeAbsolute( hb_DirSepToOS( cParam ), hb_FNameDir( aParam[ _PAR_cFileName ] ) )
         ENDIF

      CASE Left( cParamL, 6 ) == "-main="

         IF IsValidHarbourID( cParam := SubStr( cParam, 7 ) )
            l_cMAIN := "@" + cParam
         ELSE
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -main= value ignored: %1$s" ), cParam ) )
         ENDIF

      CASE Left( cParamL, 9 ) == "-request="

         IF IsValidHarbourID( cParam := SubStr( cParam, 10 ) )
            AAddNew( hbmk[ _HBMK_aREQUEST ], Upper( cParam ) )
         ELSE
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -request= value ignored: %1$s" ), cParam ) )
         ENDIF

      CASE Left( cParamL, 3 ) == "-gt"

         cParam := MacroProc( hbmk, SubStr( cParam, 2 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            IF hbmk[ _HBMK_cGT ] == NIL
               IF ! SetupForGT( cParam, @hbmk[ _HBMK_cGT ], @hbmk[ _HBMK_lGUI ] )
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -gt value ignored: %1$s" ), cParam ) )
                  cParam := NIL
               ENDIF
            ENDIF
            IF ! Empty( cParam ) .AND. !( Lower( cParam ) == "gtnul" )
               IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == Lower( cParam ) } ) == 0 .AND. ;
                  AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == Lower( cParam ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], hb_DirSepToOS( cParam ) )
               ENDIF
               IF AScan( hbmk[ _HBMK_aGT ], {| tmp | Lower( tmp ) == Lower( cParam ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aGT ], hb_DirSepToOS( cParam ) )
               ENDIF
            ENDIF
         ENDIF

#if ! defined( __PLATFORM__UNIX )
      CASE Left( cParamL, 2 ) == "/o" .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]

         /* Swallow this switch. We do not pass it to Harbour, as it may badly
            interact with hbmk. */
#endif

      CASE Left( cParam, 2 ) == "-o"

         tmp := SubStr( cParam, 3 )

         IF hbmk[ _HBMK_lStopAfterHarbour ]
            tmp := MacroProc( hbmk, tmp, aParam[ _PAR_cFileName ] )
            IF ! Empty( tmp )
               AAddNotEmpty( hbmk[ _HBMK_aOPTPRG ], "-o" + hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( tmp ), aParam[ _PAR_cFileName ] ) ) )
            ENDIF
         ELSE
            IF ! Empty( tmp )
               tmp := MacroProc( hbmk, tmp, aParam[ _PAR_cFileName ] )
               IF ! Empty( tmp )
                  tmp := hb_DirSepToOS( tmp )
                  hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                  DO CASE
                  CASE Empty( cDir )
                     tmp := hb_PathNormalize( PathMakeAbsolute( tmp, aParam[ _PAR_cFileName ] ) )
                     hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                     IF hbmk[ _HBMK_cPROGDIR ] == NIL
                        hbmk[ _HBMK_cPROGDIR ] := cDir
                     ENDIF
                     hbmk[ _HBMK_cPROGNAME ] := hb_FNameNameExt( tmp )
                     aParamPROGNAME := AClone( aParam )
                  CASE ! Empty( cDir ) .AND. Empty( cName ) .AND. Empty( cExt )
                     hbmk[ _HBMK_cPROGDIR ] := hb_PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
                  OTHERWISE /* ! Empty( cDir ) .AND. !( Empty( cName ) .AND. Empty( cExt ) ) */
                     hbmk[ _HBMK_cPROGDIR ] := hb_PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
                     hbmk[ _HBMK_cPROGNAME ] := hb_FNameNameExt( tmp )
                     aParamPROGNAME := AClone( aParam )
                  ENDCASE
               ENDIF
            ELSE
               hbmk[ _HBMK_cPROGDIR ] := NIL
               hbmk[ _HBMK_cPROGNAME ] := NIL
               aParamPROGNAME := NIL
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-implib=" ) ) == "-implib="

         hbmk[ _HBMK_lIMPLIB ] := .T.

         tmp := SubStr( cParam, Len( "-implib=" ) + 1 )

         IF ! Empty( tmp )
            tmp := MacroProc( hbmk, tmp, aParam[ _PAR_cFileName ] )
            IF ! Empty( tmp )
               tmp := hb_DirSepToOS( tmp )
               hb_FNameSplit( tmp, @cDir, @cName, @cExt )
               DO CASE
               CASE Empty( cDir )
                  tmp := hb_PathNormalize( PathMakeAbsolute( tmp, aParam[ _PAR_cFileName ] ) )
                  hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                  IF l_cIMPLIBDIR == NIL
                     l_cIMPLIBDIR := cDir
                  ENDIF
                  l_cIMPLIBNAME := hb_FNameNameExt( tmp )
               CASE ! Empty( cDir ) .AND. Empty( cName ) .AND. Empty( cExt )
                  l_cIMPLIBDIR := hb_PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
               OTHERWISE /* ! Empty( cDir ) .AND. !( Empty( cName ) .AND. Empty( cExt ) ) */
                  l_cIMPLIBDIR := hb_PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
                  l_cIMPLIBNAME := hb_FNameNameExt( tmp )
               ENDCASE
            ENDIF
         ELSE
            l_cIMPLIBDIR := NIL
            l_cIMPLIBNAME := NIL
         ENDIF

      /* NOTE: Using ':' as value separator to emulate Harbour compiler options */
      /* EXPERIMENTAL */
      CASE Left( cParamL, Len( "-ku:" ) ) == "-ku:"

         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
            cParam := MacroProc( hbmk, SubStr( cParam, Len( "-ku:" ) + 1 ), aParam[ _PAR_cFileName ] )
            IF ! Empty( cParam )
               SWITCH Lower( cParam )
               CASE "utf8"
                  hbmk[ _HBMK_cCPPRG ] := "UTF8"
                  EXIT
               OTHERWISE
                  hbmk[ _HBMK_cCPPRG ] := NIL
                  FOR EACH tmp IN hb_cdpList()
                     IF Lower( cParam ) == Lower( hb_cdpUniID( tmp ) )
                        hbmk[ _HBMK_cCPPRG ] := tmp
                        EXIT
                     ENDIF
                  NEXT
                  IF Empty( hbmk[ _HBMK_cCPPRG ] )
                     IF hbmk[ _HBMK_lInfo ]
                        _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignored unsupported codepage value: %1$s" ), cParam ) )
                     ENDIF
                  ENDIF
               ENDSWITCH
               IF ! Empty( hbmk[ _HBMK_cCPPRG ] )
                  AAddNew( hbmk[ _HBMK_aOPTPRG ], "-ku" )
               ENDIF
            ENDIF
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Option available only when using embedded Harbour compiler: %1$s" ), cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-sign=" ) ) == "-sign="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-sign=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            cOpt_SignID := cParam
         ENDIF

      CASE Left( cParamL, Len( "-signpw=" ) ) == "-signpw="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-signpw=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            cOpt_SignPass := cParam
         ENDIF

      CASE Left( cParamL, Len( "-ln=" ) ) == "-ln="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-ln=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAddNewNotEmpty( hbmk[ _HBMK_aLINK ], hb_DirSepToOS( cParam ) )
         ENDIF

      CASE Left( cParam, 2 ) == "-L" .AND. ;
           Len( cParam ) > 2

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            FOR EACH tmp IN hb_ATokens( cParam, ";" ) /* intentionally not using hb_osPathListSeparator() to keep value portable */
               IF ! Empty( tmp )
                  tmp := hb_DirSepDel( PathMakeAbsolute( hb_DirSepToOS( tmp ), aParam[ _PAR_cFileName ] ) )
                  IF CheckParamLibPath( hbmk, tmp )
                     IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ tmp .OR. hb_DirExists( tmp )
                        AAdd( hbmk[ _HBMK_aLIBPATH ], tmp )
                     ENDIF
                  ELSE
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring explicitly specified core library directory: %1$s (in option %2$s)" ), tmp, ParamToString( aParam ) ) )
                  ENDIF
               ENDIF
            NEXT
         ENDIF

      CASE Left( cParamL, Len( "-instfile=" ) ) == "-instfile="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-instfile=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF inst_split_arg( cParam, @tmp, @cParam )
            FOR EACH cParam IN FN_Expand( PathMakeAbsolute( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { tmp, cParam } )
            NEXT
         ELSE
            /* -instfile=[<group>:] will delete all previous files added to that group */
            FOR EACH tmp1 IN hbmk[ _HBMK_aINSTFILE ] DESCEND
               IF tmp1[ 1 ] == tmp
                  hb_ADel( hbmk[ _HBMK_aINSTFILE ], tmp1:__enumIndex(), .T. )
               ENDIF
            NEXT
         ENDIF

      CASE Left( cParamL, Len( "-instpath=" ) ) == "-instpath=" .AND. ;
           Len( cParamL ) > Len( "-instpath=" )

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-instpath=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF inst_split_arg( cParam, @tmp, @cParam )
            AAddNewINST( hbmk[ _HBMK_aINSTPATH ], { tmp, hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ) ) } )
         ENDIF

      CASE Left( cParamL, Len( "-incpath=" ) ) == "-incpath=" .AND. ;
           Len( cParamL ) > Len( "-incpath=" )

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-incpath=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            tmp := hb_DirSepDel( hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ) ) )
            IF CheckParamInc( hbmk, tmp )
               AAddNew( hbmk[ _HBMK_aINCPATH ], tmp )
            ELSE
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring explicitly specified core header directory: %1$s (in option %2$s)" ), tmp, ParamToString( aParam ) ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, 2 ) == "-i" .AND. ;
           Len( cParamL ) > 2 .AND. !( cParamL == "-i-" )

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            FOR EACH tmp IN hb_ATokens( cParam, ";" ) /* intentionally not using hb_osPathListSeparator() to keep value portable */
               IF ! Empty( tmp )
                  tmp := hb_DirSepDel( hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( tmp ), aParam[ _PAR_cFileName ] ) ) )
                  IF CheckParamInc( hbmk, tmp )
                     AAddNew( hbmk[ _HBMK_aINCPATH ], tmp )
                  ELSE
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring explicitly specified core header directory: %1$s (in option %2$s)" ), tmp, ParamToString( aParam ) ) )
                  ENDIF
               ENDIF
            NEXT
         ENDIF

      CASE Left( cParamL, Len( "-icon=" ) ) == "-icon="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-icon=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aICON ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE Left( cParamL, Len( "-manifest=" ) ) == "-manifest="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-manifest=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            hbmk[ _HBMK_cMANIFEST ] := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ) )
         ENDIF

      CASE Left( cParamL, Len( "-iflag=" ) ) == "-iflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-iflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTI ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-signflag=" ) ) == "-signflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-signflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTS ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-stop" ) ) == "-stop"

         hbmk[ _HBMK_lStopAfterInit ] := .T.
         hbmk[ _HBMK_lRUN ] := .F.
         hbmk[ _HBMK_nExitCode ] := _EXIT_STOP

         IF Left( cParamL, Len( "-stop=" ) ) == "-stop="
            cParam := MacroProc( hbmk, SubStr( cParam, Len( "-stop=" ) + 1 ), aParam[ _PAR_cFileName ] )
            IF ! Empty( cParam )
               OutStd( hb_StrFormat( I_( "%1$s" ), cParam ) + _OUT_EOL )
            ENDIF
         ENDIF
         EXIT

      CASE Left( cParamL, Len( "-echo=" ) ) == "-echo="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-echo=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            OutStd( hb_StrFormat( I_( "%1$s" ), cParam ) + _OUT_EOL )
         ENDIF

      CASE Left( cParamL, Len( "-prgflag=" ) ) == "-prgflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-prgflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            IF SubStr( cParamL, 2 ) == "gh"
               hbmk[ _HBMK_lStopAfterHarbour ] := .T.
               hbmk[ _HBMK_lCreateHRB ] := .T.
            ENDIF
            IF !( SubStr( cParamL, 2, 1 ) == "o" )
               AAddNewNotEmpty( hbmk[ _HBMK_aOPTPRG ], hbmk_hb_DirSepToOS( cParam, 2 ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-cflag=" ) ) == "-cflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-cflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTC ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-cflag+=" ) ) == "-cflag+="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-cflag+=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTCUSER ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-resflag=" ) ) == "-resflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-resflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTRES ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-ldflag=" ) ) == "-ldflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-ldflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTL ], hbmk_hb_DirSepToOS( cParam, 2 ), aParam, .F. )
         ENDIF

      CASE Left( cParamL, Len( "-ldflag+=" ) ) == "-ldflag+="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-ldflag+=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTLPOST ], hbmk_hb_DirSepToOS( cParam, 2 ), aParam, .F. )
         ENDIF

      CASE Left( cParamL, Len( "-dflag=" ) ) == "-dflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-dflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTD ], hbmk_hb_DirSepToOS( cParam, 2 ), aParam, .F. )
         ENDIF

      CASE Left( cParamL, Len( "-dflag+=" ) ) == "-dflag+="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-dflag+=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTDPOST ], hbmk_hb_DirSepToOS( cParam, 2 ), aParam, .F. )
         ENDIF

      CASE Left( cParamL, Len( "-aflag=" ) ) == "-aflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-aflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTA ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-runflag=" ) ) == "-runflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-runflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( l_aOPTRUN, cParam )
         ENDIF

      CASE Left( cParamL, Len( "-pflag=" ) ) == "-pflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-pflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aPLUGINPars ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-pi=" ) ) == "-pi="

         cParam := hb_DirSepToOS( MacroProc( hbmk, SubStr( cParam, Len( "-pi=" ) + 1 ), aParam[ _PAR_cFileName ] ) )
         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPLUGINPars ], cParam )
         NEXT

      CASE Left( cParamL, Len( "-3rd=" ) ) == "-3rd="

         /* Silently ignore these. These options can be used to store options
            processed by other tools allowing them to keep additional information
            in .hbp/.hbm script files. */

      CASE Left( cParamL, Len( "-workdir=" ) ) == "-workdir="

         hbmk[ _HBMK_cWorkDir ] := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, SubStr( cParam, Len( "-workdir=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] ) )

      CASE Left( cParamL, Len( "-vcshead=" ) ) == "-vcshead="

         l_cVCSDIR := hb_FNameDir( aParam[ _PAR_cFileName ] )
         l_cVCSHEAD := PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, SubStr( cParam, Len( "-vcshead=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF Empty( hb_FNameExt( l_cVCSHEAD ) )
            l_cVCSHEAD := hb_FNameExtSet( l_cVCSHEAD, ".ch" )
         ENDIF

      CASE Left( cParamL, Len( "-tshead=" ) ) == "-tshead="

         l_cTSHEAD := PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, SubStr( cParam, Len( "-tshead=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF Empty( hb_FNameExt( l_cTSHEAD ) )
            l_cTSHEAD := hb_FNameExtSet( l_cTSHEAD, ".ch" )
         ENDIF

      CASE Left( cParamL, Len( "-plugin=" ) ) == "-plugin="

         cParam := PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, SubStr( cParam, Len( "-plugin=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF ( tmp := FindInPathPlugIn( cParam ) ) != NIL
            PlugIn_Load( hbmk, tmp )
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Plugin not found: %1$s" ), cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParam, 2 ) == "-l" .AND. ;
           Len( cParam ) > 2 .AND. ;
           !( cParam == "-l-" )

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            cParam := hb_DirSepToOS( cParam )
            IF Left( cParam, 1 ) == "-"
               IF CheckParamLib( hbmk, SubStr( cParam, 2 ), .F., aParam )
                  AAdd( hbmk[ _HBMK_aLIBFILTEROUT ], SubStr( cParam, 2 ) )
               ENDIF
            ELSE
               IF CheckParamLib( hbmk, cParam, .F., aParam )
                  IF _IS_AUTOLIBSYSPRE( cParam )
                     AAdd( hbmk[ _HBMK_aLIBUSERSYSPRE ], cParam )
                  ELSE
                     AAdd( hbmk[ _HBMK_aLIBUSER ], cParam )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

      CASE Left( cParam, 2 ) == "-F" .AND. ;
           Len( cParam ) > 2

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aLIBUSERFWK ], hb_FNameExtSet( hb_DirSepToOS( cParam ) ) )
         ENDIF

      CASE Left( cParam, Len( "-autohbc=" ) ) == "-autohbc="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-autohbc=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF autohbc_split_arg( cParam, @tmp, @cParam )

            cParam := PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )

            IF Empty( hb_FNameExt( tmp ) )
               tmp := hb_FNameExtSet( tmp, ".ch" )
            ENDIF
            IF Empty( hb_FNameExt( cParam ) )
               cParam := hb_FNameExtSet( cParam, ".hbc" )
            ENDIF

            IF ! hb_FileExists( cParam )
               FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                  IF hb_FileExists( hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cParam ) )
                     cParam := hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cParam )
                     EXIT
                  ENDIF
               NEXT
            ENDIF

            hbmk[ _HBMK_hAUTOHBC ][ AllTrim( StrTran( tmp, "\", "/" ) ) ] := cParam
         ENDIF

      CASE Left( cParam, Len( "-deppkgname=" ) ) == "-deppkgname="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-deppkgname=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aPKG ], AllTrim( tmp ) )
         ENDIF

      CASE Left( cParam, Len( "-depkeyhead=" ) ) == "-depkeyhead="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depkeyhead=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aKeyHeader ], AllTrim( StrTran( tmp, "\", "/" ) ) )
         ENDIF

      CASE Left( cParam, Len( "-depoptional=" ) ) == "-depoptional="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depoptional=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            DO CASE
            CASE Lower( tmp ) == "yes" ; hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_lOptional ] := .T.
            CASE Lower( tmp ) == "no"  ; hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_lOptional ] := .F.
            ENDCASE
         ENDIF

      CASE Left( cParam, Len( "-depcontrol=" ) ) == "-depcontrol="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depcontrol=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_cControl ] := AllTrim( tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aINCPATH ], _HBMK_DEP_CTRL_MARKER )
         ENDIF

      CASE Left( cParam, Len( "-depincroot=" ) ) == "-depincroot="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depincroot=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_cINCROOT ] := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( tmp ), aParam[ _PAR_cFileName ] ) )
         ENDIF

      CASE Left( cParam, Len( "-depincpath=" ) ) == "-depincpath="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depincpath=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            FOR EACH tmp1 IN hb_ATokens( tmp, ";" ) /* intentionally not using hb_osPathListSeparator() to keep value portable */
               AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aINCPATH ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( tmp1 ), aParam[ _PAR_cFileName ] ) ) )
            NEXT
         ENDIF

      CASE Left( cParam, Len( "-depincpathlocal=" ) ) == "-depincpathlocal="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depincpathlocal=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aINCPATHLOCAL ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( tmp ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE Left( cParam, Len( "-depimplibs=" ) ) == "-depimplibs="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depimplibs=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            FOR EACH tmp1 IN hb_ATokens( tmp, ";" ) /* intentionally not using hb_osPathListSeparator() to keep value portable */
               AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aIMPLIBSRC ], hb_DirSepToOS( tmp1 ) )
            NEXT
         ENDIF

      CASE Left( cParam, Len( "-depimplibd=" ) ) == "-depimplibd="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depimplibd=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_cIMPLIBDST ] := hb_FNameNameExt( hb_DirSepToOS( tmp ) )
         ENDIF

      CASE Left( cParam, Len( "-depfinish=" ) ) == "-depfinish="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depfinish=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            IF cParam $ hbmk[ _HBMK_hDEP ]
               dep_try_detection( hbmk, hbmk[ _HBMK_hDEP ][ cParam ] )
            ELSE
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Unknown dependency name: %1$s" ), ParamToString( aParam ) ) )
            ENDIF
         ENDIF

      CASE Left( cParam, 1 ) $ cOptPrefix

         DO CASE
         CASE lAcceptLDFlag
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTL ], hbmk_hb_DirSepToOS( cParam, 2 ), aParam, .F. )
         CASE lAcceptCFlag
            IF SubStr( cParamL, 2 ) == "c"
               lStopAfterCComp := .T.
            ELSE
               AAddNotEmpty( hbmk[ _HBMK_aOPTC ], hbmk_hb_DirSepToOS( cParam, 2 ) )
            ENDIF
         CASE lAcceptIFlag
            AAddNotEmpty( hbmk[ _HBMK_aOPTI ], hbmk_hb_DirSepToOS( cParam, 2 ) )
         OTHERWISE
            IF SubStr( cParamL, 2 ) == "gh"
               hbmk[ _HBMK_lStopAfterHarbour ] := .T.
               hbmk[ _HBMK_lCreateHRB ] := .T.

            /* Detect if Harbour is only used as preprocessor (-p + -s options) */
            ELSEIF SubStr( cParamL, 2 ) == "p"
               ++nHarbourPPO
               tmp := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
               IF ! Empty( tmp )
                  tmp := PathMakeAbsolute( hb_DirSepToOS( tmp ), aParam[ _PAR_cFileName ] )
                  hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                  cHarbourPPODir := cDir
               ENDIF
            ELSEIF SubStr( cParamL, 2 ) == "s"
               hbmk[ _HBMK_lStopAfterHarbour ] := .T.
               ++nHarbourPPO
            ENDIF
            IF nHarbourPPO >= 2
               hbmk[ _HBMK_lCreatePPO ] := .T.
            ENDIF
            AAddNewNotEmpty( hbmk[ _HBMK_aOPTPRG ], hbmk_hb_DirSepToOS( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ), 2 ) )
         ENDCASE

      CASE hbmk[ _HBMK_lCreateImpLib ]

         cParam := MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aIMPLIBSRC ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE hb_FNameExt( cParamL ) == ".lib" .OR. ;
           ( ! Empty( hbmk[ _HBMK_cDynLibExt ] ) .AND. hb_FNameExt( cParamL ) == hbmk[ _HBMK_cDynLibExt ] )

         cParam := hb_DirSepToOS( cParam )
         IF CheckParamLib( hbmk, cParam, .F., aParam )
            IF _IS_AUTOLIBSYSPRE( cParam )
               AAdd( hbmk[ _HBMK_aLIBUSERSYSPRE ], cParam )
            ELSE
               AAdd( hbmk[ _HBMK_aLIBUSER ], cParam )
            ENDIF
         ENDIF

      CASE hb_FNameExt( cParamL ) == ".framework"

         AAdd( hbmk[ _HBMK_aLIBUSERFWK ], hb_FNameExtSet( hb_DirSepToOS( cParam ) ) )

      CASE hb_FNameExt( cParamL ) == ".hbc"

         cParam := tmp1 := MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] )
         cParam := PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            IF Empty( HBC_Find( hbmk, cParam ) )
               IF Empty( aParam[ _PAR_cFileName ] )
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot find %1$s" ), tmp1 ) )
               ELSE
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot find %1$s (referenced from %2$s)" ), tmp1, aParam[ _PAR_cFileName ] ) )
               ENDIF
            ENDIF
         ENDIF

      CASE hb_FNameExt( cParamL ) == ".hrb"

         cParam := PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF ( tmp := FindInPathPlugIn( cParam ) ) != NIL
            PlugIn_Load( hbmk, tmp )
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Plugin not found: %1$s" ), cParam ) )
            ENDIF
         ENDIF

      CASE hb_FNameExt( cParamL ) == ".prg" .OR. ;
           hb_FNameExt( cParamL ) == ".hb"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPRG ], cParam )
            hb_default( @hbmk[ _HBMK_cFIRST ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".ch"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAddNew( hbmk[ _HBMK_aCH ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".rc"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aRESSRC ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".res"

         IF HBMK_ISCOMP( "mingw|mingw64|mingwarm" ) .OR. ;
            ( hbmk[ _HBMK_cPLAT ] == "win" .AND. HBMK_ISCOMP( "clang" ) ) .OR. ;
            ( hbmk[ _HBMK_cPLAT ] == "os2" .AND. HBMK_ISCOMP( "gcc|gccomf" ) )
            /* For MinGW/EMX GCC family add .res files as source input, as they
               will need to be converted to coff format with windres (just
               like plain .rc files) before feeding them to gcc. */
            FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aRESSRC ], cParam )
            NEXT
         ELSE
            FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aRESCMP ], cParam )
            NEXT
         ENDIF

      CASE hb_FNameExt( cParamL ) == ".a"

         cParam := PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] )
         AAdd( l_aOBJA, cParam )

      CASE hb_FNameExt( cParamL ) == ".def"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aDEF ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".o" .OR. ;
           hb_FNameExt( cParamL ) == ".obj"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aOBJUSER ], cParam )
            hb_default( @hbmk[ _HBMK_cFIRST ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".cpp" .OR. ;
           hb_FNameExt( cParamL ) == ".cc" .OR. ;
           hb_FNameExt( cParamL ) == ".cxx" .OR. ;
           hb_FNameExt( cParamL ) == ".cx"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aCPP ], cParam )
            hb_default( @hbmk[ _HBMK_cFIRST ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".c" .OR. ;
           hb_FNameExt( cParamL ) == ".m"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aC ], cParam )
            hb_default( @hbmk[ _HBMK_cFIRST ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".d"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            deplst_read( hbmk, hbmk[ _HBMK_hDEPTS ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".po" .OR. ;
           hb_FNameExt( cParamL ) == ".pot"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( hb_DirSepToOS( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPO ], cParam )
         NEXT

      CASE hb_FNameExt( cParamL ) == ".hbl"

         hbmk[ _HBMK_cHBL ] := hb_DirSepToOS( cParam )
         hbmk[ _HBMK_cHBLDir ] := hb_FNameDir( aParam[ _PAR_cFileName ] )

      CASE hb_FNameExt( cParamL ) $ hbmk[ _HBMK_hPLUGINExt ]

         cParam := hb_DirSepToOS( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) )
         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPLUGINPars ], cParam )
         NEXT

      OTHERWISE

         cParam := PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF Empty( hb_FNameExt( cParam ) )
            cParam := hb_FNameExtSet( cParam, ".prg" )
         ELSEIF hb_FNameExt( cParamL ) == ".hbx"
            IF hb_FileExists( cParam )
               AAddNew( hbmk[ _HBMK_aOPTPRG ], "-D" + _HBMK_HBEXTREQ )
               AAdd( hbmk[ _HBMK_aPRG ], cParam )
            ENDIF
            LOOP
         ENDIF
         AAdd( hbmk[ _HBMK_aPRG ], cParam )
         hb_default( @hbmk[ _HBMK_cFIRST ], cParam )

      ENDCASE
   NEXT

   IF hbmk[ _HBMK_lDEBUGPARS ]
      FOR EACH aParam IN aParams
         _hbmk_OutStd( hbmk, hb_StrFormat( "debugpars: %1$3d %2$s", aParam:__enumIndex(), ParamToString( aParam ) ) )
      NEXT
   ENDIF

   IF ! l_lLIBSYSMISC
      l_aLIBSYSMISC := {}
   ENDIF

   IF lHarbourInfo
      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
         /* Use integrated compiler */
         hbmk_hb_compile( hbmk, "harbour", hbmk[ _HBMK_aOPTPRG ] )
      ELSE
         /* Use external compiler */
         cCommand := ;
            FNameEscape( hb_DirSepAdd( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_BIN ] ) ) + cBin_CompPRG + cBinExt, hbmk[ _HBMK_nCmd_Esc ] ) + ;
            iif( Empty( hbmk[ _HBMK_aOPTPRG ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTPRG ] ) )
         hb_processRun( AllTrim( cCommand ) )
      ENDIF
      RETURN _EXIT_OK
   ENDIF

   /* Strip leading @ char of .clp files */
   IF ! Empty( hbmk[ _HBMK_cFIRST ] ) .AND. Left( hbmk[ _HBMK_cFIRST ], 1 ) == "@" .AND. Lower( hb_FNameExt( hbmk[ _HBMK_cFIRST ] ) ) == ".clp"
      hbmk[ _HBMK_cFIRST ] := SubStr( hbmk[ _HBMK_cFIRST ], 2 )
   ENDIF

   IF hbmk[ _HBMK_lCreateDyn ]
      hbmk[ _HBMK_lPIC ] := .T.
   ENDIF

#if 0 /* Disabled to experiment with '-hbdyn -shared' combination. */
   IF hbmk[ _HBMK_lCreateDyn ] .AND. hbmk[ _HBMK_lSHARED ]
      hbmk[ _HBMK_lSHARED ] := .F.
   ENDIF
#endif

   /* Force MT mode off in 1.0.x and xhb/dos compatibility modes. */
   IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
      ( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ) .AND. hbmk[ _HBMK_cPLAT ] == "dos" )
      hbmk[ _HBMK_lMT ] := .F.
   ENDIF

   IF hbmk[ _HBMK_cPLAT ] == "wce"
      hbmk[ _HBMK_lWINUNI ] := .T.
   ENDIF

   /* Start doing the make process. */
   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lCreateImpLib ] .AND. ;
      ( Len( hbmk[ _HBMK_aPLUGINPars ] ) + Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) + Len( hbmk[ _HBMK_aRESSRC ] ) + Len( hbmk[ _HBMK_aRESCMP ] ) + Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ) ) == 0 .AND. ! hbmk[ _HBMK_lContainer ]
      IF hbmk[ _HBMK_lInfo ]
         _hbmk_OutErr( hbmk, I_( "Warning: No source files were specified." ) )
      ENDIF
   ENDIF

   IF hbmk[ _HBMK_cPROGNAME ] != NIL
      cExt := hb_FNameExt( hbmk[ _HBMK_cPROGNAME ] )
      tmp1 := hbmk_TARGETTYPE( hbmk )
      IF ( Lower( cExt ) == ".exe" .AND. tmp1 == "hbexe" ) .OR. ;
         ( Lower( cExt ) == ".dll" .AND. HBMK_IS_IN( tmp1, "hbdyn|hbdynvm" ) ) .OR. ;
         ( HBMK_IS_IN( Lower( cExt ), ".lib|.a" ) .AND. HBMK_IS_IN( tmp1, "hblib|hbimplib" ) )
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Non-portable output filename: %1$s. Delete '%2$s' extension." ), iif( aParamPROGNAME != NIL, ParamToString( aParamPROGNAME ), hbmk[ _HBMK_cPROGNAME ] ), cExt ) )
      ENDIF
   ENDIF

   /* Decide about output name */
   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lCreateImpLib ]

      IF ! hbmk[ _HBMK_lCreateHRB ]
         /* If -o with full name was not specified, let us
            make it the first source file specified. */
         hb_default( @hbmk[ _HBMK_cPROGNAME ], hb_FNameName( hbmk[ _HBMK_cFIRST ] ) )

         /* Combine output dir with output name. */
         IF ! Empty( hbmk[ _HBMK_cPROGDIR ] )
            hb_FNameSplit( hbmk[ _HBMK_cPROGNAME ], @cDir, @cName, @cExt )
            hbmk[ _HBMK_cPROGNAME ] := hb_FNameMerge( iif( Empty( cDir ), hbmk[ _HBMK_cPROGDIR ], cDir ), cName, cExt )
         ENDIF
      ENDIF
   ENDIF

   /* Decide about working dir */
   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lCreateImpLib ] .AND. ! hbmk[ _HBMK_lDumpInfo ]
      IF hbmk[ _HBMK_lINC ]
         hb_default( @hbmk[ _HBMK_cWorkDir ], hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ) + _WORKDIR_DEF_ + hbmk[ _HBMK_cWorkDirDynSub ] )
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            IF ! hb_DirBuild( hbmk[ _HBMK_cWorkDir ] )
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Working directory cannot be created: %1$s" ), hbmk[ _HBMK_cWorkDir ] ) )
               IF hbmk[ _HBMK_lBEEP ]
                  DoBeep( .F. )
               ENDIF
               RETURN _EXIT_WORKDIRCREATE
            ENDIF
            #if ! defined( __PLATFORM__UNIX )
               tmp := hb_AtI( _WORKDIR_BASE_ + hb_ps(), hbmk[ _HBMK_cWorkDir ] + hb_ps() )
               IF tmp > 0
                  hb_FSetAttr( Left( hbmk[ _HBMK_cWorkDir ], tmp - 1 ) + _WORKDIR_BASE_, FC_HIDDEN )
               ENDIF
            #endif
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lStopAfterInit ] .OR. ;
            hbmk[ _HBMK_lStopAfterHarbour ] .OR. ;
            ( lStopAfterCComp .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] )
            /* It is controlled by -o option in these cases */
            hbmk[ _HBMK_cWorkDir ] := ""
         ELSE
            IF hbmk[ _HBMK_cWorkDir ] == NIL
               FClose( hb_FTempCreateEx( @hbmk[ _HBMK_cWorkDir ], NIL, "hbmk_", ".dir" ) )
               FErase( hbmk[ _HBMK_cWorkDir ] )
               IF hb_DirCreate( hbmk[ _HBMK_cWorkDir ] ) != 0
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Temporary Working directory cannot be created: %1$s" ), hbmk[ _HBMK_cWorkDir ] ) )
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _EXIT_WORKDIRCREATE
               ENDIF
               lDeleteWorkDir := .T.
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! hbmk[ _HBMK_lCreateImpLib ]

      /*
         /boot/common/include                        (beos)
         /boot/develop/headers/3rdparty              (beos)

         /usr/local/include                          (darwin Homebrew)
         /opt/local/include                          (darwin MacPorts/DarwinPorts)
         /sw/include                                 (darwin Fink)
         /Library/Frameworks/<pkg>.framework/Headers (darwin)

         /usr/<pkg>/include                          (FHS)
         /usr/include                                (FHS)
         /opt/<pkg>/include                          (FHS)
         /opt/include                                (FHS)
         /usr/local/<pkg>/include                    (FHS)
         /usr/local/include                          (FHS)
      */

      /* Process any package requirements */
      FOR EACH tmp IN hbmk[ _HBMK_hDEP ]
         dep_try_detection( hbmk, tmp )
      NEXT
   ENDIF

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]

      IF hbmk[ _HBMK_cGT ] != NIL .AND. hbmk[ _HBMK_cGT ] == hbmk[ _HBMK_cGTDEFAULT ]
         hbmk[ _HBMK_cGT ] := NIL
      ENDIF

      /* Merge user libs from command line and envvar. Command line has priority. */
      hbmk[ _HBMK_aLIBUSER ] := ArrayAJoin( { hbmk[ _HBMK_aLIBUSERGT ], hbmk[ _HBMK_aLIBUSER ] } )

      IF hbmk[ _HBMK_cPLAT ] == "darwin"
         FOR EACH tmp IN hbmk[ _HBMK_aLIBUSERFWK ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-framework " + tmp )
            AAdd( hbmk[ _HBMK_aOPTD ], "-framework " + tmp )
         NEXT
      ENDIF

      hb_default( @hbmk[ _HBMK_lSHAREDDIST ], hbmk[ _HBMK_lSysLoc ] )

      IF hbmk[ _HBMK_lSHAREDDIST ] .OR. ! HBMK_ISCOMP( "gcc|clang|open64" )
         l_cDynLibDir := ""
      ELSE
         /* Only supported by gcc, clang, open64 compilers. */
         l_cDynLibDir := hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_DYN ] )
      ENDIF
#if 1
      cSuffix := ""
      HB_SYMBOL_UNUSED( cDL_Version )
#else
      cSuffix := cDL_Version
#endif

      IF hbmk[ _HBMK_lMT ] .AND. hbmk[ _HBMK_nHBMODE ] <= _HBMODE_HB20
         cHarbourDyn := iif( HBMK_ISPLAT( "win|os2" ), "harbourm", "harbourmt" )
      ELSE
         /* ST mode or newer than Harbour 2.0, where there is only one harbour lib,
            built in MT mode by default. */
         cHarbourDyn := "harbour"
      ENDIF

      DO CASE
      CASE HBMK_ISPLAT( "darwin|bsd|linux|hpux|beos|qnx|android|vxworks|sunos|minix|aix" )
         IF Empty( l_cDynLibDir )
            l_aLIBSHARED := { cHarbourDyn + cSuffix }
         ELSE
            l_aLIBSHARED := { l_cDynLibDir + hbmk[ _HBMK_cDynLibPrefix ] + cHarbourDyn + cSuffix + hbmk[ _HBMK_cDynLibExt ] }
         ENDIF
      CASE HBMK_ISPLAT( "os2|win|wce" )
         l_aLIBSHARED := { hbmk[ _HBMK_cDynLibPrefix ] + cHarbourDyn }
      OTHERWISE
         l_aLIBSHARED := NIL
      ENDCASE

      /* C compilation/linking */

      l_aLIB3RD := {}
      l_aLIBSYS := {}
      l_aCLEAN := {}

      cOptIncMask := "-I{DI}"

      /* Command macros:

         {LC}     list of C files
         {LR}     list of resource source files (Windows specific)
         {LS}     list of resource binary files (Windows specific)
         {LO}     list of object files
         {LA}     list of object archive (.a) files
         {LL}     list of lib files
         {LF}     list of lib files (as flag)
         {LB}     list of lib files with paths
         {FC}     flags for C compiler (user + automatic)
         {FL}     flags for linker (user + automatic)
         {FS}     flags for code sign tool
         {UT}     url for timestamp (code sign tool)
         {OW}     working dir (when in -inc mode)
         {OD}     output dir
         {OO}     output object (when in -hbcmp mode)
         {OE}     output executable
         {OM}     output map name
         {OI}     output implib name
         {DB}     dir for binaries
         {DI}     dir for includes
         {DL}     dirs for libs
         {SCRIPT} save command line to script and pass it to command as @<filename>
       */

      /* Assemble library list */

      IF ! Empty( hbmk[ _HBMK_cGT ] ) .AND. !( Lower( hbmk[ _HBMK_cGT ] ) == "gtnul" )
         IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == Lower( hbmk[ _HBMK_cGT ] ) } ) == 0 .AND. ;
            AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == Lower( hbmk[ _HBMK_cGT ] ) } ) == 0
            AAdd( hbmk[ _HBMK_aLIBUSERGT ], hbmk[ _HBMK_cGT ] )
         ENDIF
         IF AScan( hbmk[ _HBMK_aGT ], {| tmp | Lower( tmp ) == Lower( hbmk[ _HBMK_cGT ] ) } ) == 0
            AAddNotEmpty( hbmk[ _HBMK_aGT ], hbmk[ _HBMK_cGT ] )
         ENDIF
      ENDIF

      IF l_lNOHBLIB

         aLIB_BASE_EXTERN := {}
         aLIB_BASE_DEBUG := {}
         aLIB_BASE_1 := {}
         aLIB_BASE_1_MT := {}
         aLIB_BASE_2 := {}
         aLIB_BASE_2_MT := {}
         aLIB_BASE_NULRDD := {}
         aLIB_BASE_RDD := {}
         aLIB_BASE_RDD_MT := {}
         aLIB_BASE_CPLR := {}
         aLIB_BASE_3 := {}
         aLIB_BASE_3_MT := {}
         cLIB_BASE_PCRE := NIL
         cLIB_BASE_ZLIB := NIL

         hbmk[ _HBMK_aLIBCOREGT ] := {}
      ENDIF

      #define _HBLIB_FULLPATH( cName ) ( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_LIB ] ) + hb_ps() + cLibLibPrefix + cName + cLibLibExt )

      cLibHBX_Regex := R_( "[\s]_?HB_FUN_([A-Z0-9_]*)[\s]" )

      DO CASE
      /* GCC family */
      CASE ( hbmk[ _HBMK_cPLAT ] == "bsd"     .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "darwin"  .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "hpux"    .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "sunos"   .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux"   .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux"   .AND. hbmk[ _HBMK_cCOMP ] == "icc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "darwin"  .AND. hbmk[ _HBMK_cCOMP ] == "icc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux"   .AND. hbmk[ _HBMK_cCOMP ] == "clang" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "darwin"  .AND. hbmk[ _HBMK_cCOMP ] == "clang" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "bsd"     .AND. hbmk[ _HBMK_cCOMP ] == "clang" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "minix"   .AND. hbmk[ _HBMK_cCOMP ] == "clang" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "beos"    .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "qnx"     .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "android" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "android" .AND. hbmk[ _HBMK_cCOMP ] == "gccarm" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "vxworks" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "symbian" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "cygwin"  .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux"   .AND. hbmk[ _HBMK_cCOMP ] == "open64" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "bsd"     .AND. hbmk[ _HBMK_cCOMP ] == "pcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "darwin"  .AND. hbmk[ _HBMK_cCOMP ] == "pcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux"   .AND. hbmk[ _HBMK_cCOMP ] == "pcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "sunos"   .AND. hbmk[ _HBMK_cCOMP ] == "pcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "minix"   .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "aix"     .AND. hbmk[ _HBMK_cCOMP ] == "gcc" )

         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #elif defined( __PLATFORM__WINDOWS )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         /* TODO: Symbian cross-tools on Windows better "likes" forward slashes. [vszakats] */
         IF hbmk[ _HBMK_cPLAT ] == "vxworks"
            vxworks_env_init( hbmk )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "symbian"
            cLibLibPrefix := ""
         ELSE
            cLibLibPrefix := "lib"
         ENDIF
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         IF hbmk[ _HBMK_cPLAT ] == "darwin"
            cBin_Lib := "libtool"
            cOpt_Lib := "-static {FA} -o {OL} {LO}"
         ELSE
            DO CASE
            CASE hbmk[ _HBMK_cCOMP ] == "icc"
               cBin_Lib := "xiar"
            CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
               cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + hbmk[ _HBMK_cCCSUFFIX ]
            OTHERWISE
               cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar"
            ENDCASE
            IF HBMK_ISPLAT( "hpux|sunos" )
               cOpt_Lib := "{FA} rc {OL} {LO}"
            ELSE
               cOpt_Lib := "{FA} rcs {OL} {LO}"
            ENDIF
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] == "icc"
            cBin_CompCPP := "icpc"
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, "icc" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_GNU_SOURCE" )
         CASE hbmk[ _HBMK_cCOMP ] == "clang"
            cBin_CompC := hbmk[ _HBMK_cCCPREFIX ] + "clang" + hbmk[ _HBMK_cCCSUFFIX ]
            cBin_CompCPP := cBin_CompC
         CASE hbmk[ _HBMK_cCOMP ] == "pcc"
            cBin_CompC := hbmk[ _HBMK_cCCPREFIX ] + "pcc" + hbmk[ _HBMK_cCCSUFFIX ]
         CASE hbmk[ _HBMK_cCOMP ] == "open64"
            cBin_CompCPP := "openCC"
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, "opencc" )
         CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
            cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCSUFFIX ]
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "cc" + hbmk[ _HBMK_cCCSUFFIX ] )
         OTHERWISE
            cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCSUFFIX ]
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCSUFFIX ] )
         ENDCASE
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] == "icc"
            SWITCH hbmk[ _HBMK_nWARN ]
            CASE _WARN_MAX
            CASE _WARN_YES /* AAdd( hbmk[ _HBMK_aOPTC ], "-w2 -Wall" ); EXIT */
            CASE _WARN_LOW
            CASE _WARN_NO
            ENDSWITCH
         ELSE
            SWITCH hbmk[ _HBMK_nWARN ]
            CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall -pedantic" ) ; EXIT
            CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall" )           ; EXIT
            CASE _WARN_LOW
               AAdd( hbmk[ _HBMK_aOPTC ], "-Wmissing-braces -Wreturn-type -Wformat" )
               IF hbmk[ _HBMK_lCPP ] != NIL .AND. ! hbmk[ _HBMK_lCPP ]
                  AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration" )
               ENDIF
               EXIT
            CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
            ENDSWITCH
         ENDIF
         IF hbmk[ _HBMK_lSAFE ]
            IF HBMK_ISPLAT( "linux" )
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-z,relro,-z,now" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,-z,relro,-z,now" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "vxworks"
            AAdd( hbmk[ _HBMK_aOPTC ], "-mrtp" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-mrtp" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-mrtp" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-fno-strict-aliasing" )
            /* TOFIX: Potential collision with -cpp=/-c= options */
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_C99" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_HAS_C9X" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "minix"
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_NETBSD_SOURCE=1" )
         ENDIF
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            /* Symbian gcc cross-compiler (on Windows) crashes if compiling multiple files at once */
            IF !( hbmk[ _HBMK_cPLAT ] == "symbian" ) /* EXPERIMENTAL */
               lCHD_Comp := .T.
               cOpt_CompC += " {LC}"
            ELSE
               IF HBMK_ISPLAT( "linux|bsd" ) .AND. hbmk[ _HBMK_cCOMP ] == "clang"
                  /* NOTE: It is also accepted by darwin/clang */
                  cOpt_CompC += " {IC} -o{OO}"
               ELSE
                  cOpt_CompC += " {IC} -o {OO}"
               ENDIF
            ENDIF
            IF HBMK_ISCOMP( "icc|gcc" )
               AAdd( hbmk[ _HBMK_aOPTC ], "-pipe" )
            ENDIF
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         AAddNotEmpty( hbmk[ _HBMK_aOPTCX ], gcc_opt_lngc_fill( hbmk ) )
         AAddNotEmpty( hbmk[ _HBMK_aOPTCPPX ], gcc_opt_lngcpp_fill( hbmk ) )
         IF hbmk[ _HBMK_cPLAT ] == "darwin"
            cBin_Dyn := cBin_Lib
            cOpt_Dyn := "-dynamic -o {OD} -flat_namespace -undefined suppress -single_module {FD} {DL} {LO} {LS}" /* NOTE: -single_module is now the default in ld/libtool. */
         ELSE
            cBin_Dyn := cBin_CompC
            cOpt_Dyn := "-shared -o {OD} {LO} {FD} {DL} {LS}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF hbmk[ _HBMK_cPLAT ] == "symbian"
            cLibLibExt := ".lib"
         ELSE
            cLibLibExt := ".a"
         ENDIF
         cBin_LibHBX := hbmk[ _HBMK_cCCPREFIX ] + "nm"
         cOpt_LibHBX := "-g" + iif( hbmk[ _HBMK_cPLAT ] == "darwin", "", " --defined-only -C" ) + " {LI}"
         IF hbmk[ _HBMK_cPLAT ] == "darwin"
            cLibHBX_Regex := R_( "[\s]T" ) + cLibHBX_Regex
         ENDIF
         IF l_lLIBGROUPING .AND. HBMK_ISPLAT( "linux|beos|qnx|android|vxworks|cygwin|bsd" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} {LF} -Wl,--end-group" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--start-group {LL} {LB} {LF} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB} {LF}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "{LL} {LB} {LF}" )
            l_aLIBHBBASE_2 := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_2_MT, aLIB_BASE_2 )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            IF hbmk[ _HBMK_cPLAT ] == "darwin"
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-map,{OM}" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,-map,{OM}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,-Map,{OM}" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSTATICFULL ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-static" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "darwin" .AND. hbmk[ _HBMK_cCOMP ] == "gcc"
            /* It is to avoid warning message generated when 'long double' is used
               remove it if you have newer compiler version */
#if 0
            AAdd( hbmk[ _HBMK_aOPTC ], "-Wno-long-double" )
#endif
            IF hbmk[ _HBMK_lSHARED ]
               AAdd( hbmk[ _HBMK_aOPTL ], "-bind_at_load" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "vxworks"
            IF hbmk[ _HBMK_lSHARED ]
               AAdd( hbmk[ _HBMK_aOPTL ], "-shared" ) /* TOFIX: no entry point */
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_lCreateLib ] .OR. HBMK_ISPLAT( "darwin|sunos" )
               DO CASE
               CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
                  cBin_Post := "strip" + hbmk[ _HBMK_cCCSUFFIX ]
               CASE hbmk[ _HBMK_cPLAT ] == "symbian"
                  cBin_Post := hbmk[ _HBMK_cCCPREFIX ] + "strip"
               OTHERWISE
                  cBin_Post := "strip"
               ENDCASE
               IF hbmk[ _HBMK_lCreateDyn ] .OR. hbmk[ _HBMK_lCreateLib ]
                  cOpt_Post := "-S {OB}"
               ELSE
                  cOpt_Post := "{OB}"
               ENDIF
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
            ENDIF
         ENDIF
         IF lStopAfterCComp
            IF ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               IF HBMK_ISPLAT( "darwin|sunos" ) .OR. HBMK_ISCOMP( "pcc" )
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o {OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
               ENDIF
            ENDIF
         ELSE
            IF HBMK_ISPLAT( "darwin|sunos" ) .OR. HBMK_ISCOMP( "pcc" )
               AAdd( hbmk[ _HBMK_aOPTL ], "-o {OE}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
            ENDIF
         ENDIF

         IF hbmk[ _HBMK_lPIC ] .AND. ! HBMK_ISPLAT( "darwin|cygwin" )
            IF HBMK_ISPLAT( "bsd|hpux|sunos|linux|android|aix" )
               AAdd( hbmk[ _HBMK_aOPTC ], "-fPIC" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fpic" )
            ENDIF
         ENDIF

         /* Always inherit/reproduce some flags from self */
         FOR EACH tmp IN { "-mlp64", "-mlp32", "-m64", "-m32" }
            IF tmp $ hb_Version( HB_VERSION_FLAG_C )
               AAddNew( hbmk[ _HBMK_aOPTC ], tmp )
               AAddNew( hbmk[ _HBMK_aOPTL ], tmp )
               AAddNew( hbmk[ _HBMK_aOPTD ], tmp )
               EXIT
            ENDIF
         NEXT

         /* Add system libraries */
         IF ! hbmk[ _HBMK_lSHARED ]
            IF ! HBMK_ISPLAT( "beos|vxworks" )
               AAdd( l_aLIBSYS, "m" )
               IF hbmk[ _HBMK_lMT ]
                  IF ! HBMK_ISPLAT( "qnx|android|minix" )
                     AAdd( l_aLIBSYS, "pthread" )
                  ENDIF
               ENDIF
            ENDIF
            DO CASE
            CASE HBMK_ISPLAT( "linux|cygwin" )
               AAdd( l_aLIBSYS, "dl" )
               AAdd( l_aLIBSYS, "rt" )
            CASE hbmk[ _HBMK_cPLAT ] == "android"
               AAdd( l_aLIBSYS, "dl" )
            CASE hbmk[ _HBMK_cPLAT ] == "sunos"
               AAdd( l_aLIBSYS, "rt" )
               AAdd( l_aLIBSYS, "socket" )
               AAdd( l_aLIBSYS, "nsl" )
               AAdd( l_aLIBSYS, "resolv" )
            CASE hbmk[ _HBMK_cPLAT ] == "hpux"
               AAdd( l_aLIBSYS, "rt" )
            CASE hbmk[ _HBMK_cPLAT ] == "beos"
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/system/lib" )
               AAdd( l_aLIBSYS, "root" )
               AAdd( l_aLIBSYS, "network" )
            CASE hbmk[ _HBMK_cPLAT ] == "qnx"
               AAdd( l_aLIBSYS, "socket" )
            CASE hbmk[ _HBMK_cPLAT ] == "minix"
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/lib" )
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/pkg/lib" )
            ENDCASE

            IF ! Empty( cLIB_BASE_PCRE ) .AND. ! hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_PCRE ) )
               IF hbmk[ _HBMK_cPLAT ] == "bsd"
                 AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" )
               ENDIF
               AAdd( l_aLIBSYS, "pcre" )
               cLIB_BASE_PCRE := NIL
            ENDIF
            IF ! Empty( cLIB_BASE_ZLIB ) .AND. ! hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_ZLIB ) )
               AAdd( l_aLIBSYS, "z" )
               cLIB_BASE_ZLIB := NIL
            ENDIF
         ENDIF

         IF IsGTRequested( hbmk, "gtcrs" )
            /* TOFIX: Sometimes 'ncur194' is needed. */
            AAdd( l_aLIBSYS, iif( HBMK_ISPLAT( "sunos|bsd|minix" ), "curses", "ncurses" ) )
         ENDIF
         IF IsGTRequested( hbmk, "gtsln" )
            IF hbmk[ _HBMK_cPLAT ] == "bsd" .AND. ;
               hb_FileExists( "/usr/pkg/lib/libslang2.so" ) /* For pkgsrc */
               AAdd( l_aLIBSYS, "slang2" )
            ELSE
               AAdd( l_aLIBSYS, "slang" )
            ENDIF
            /* Add paths, where this is not a system component */
            DO CASE
            CASE hbmk[ _HBMK_cPLAT ] == "darwin"
               IF hb_DirExists( "/usr/local/lib" )
                  AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" ) /* For Homebrew */
               ENDIF
               IF hb_DirExists( "/opt/local/lib" )
                  AAddNew( hbmk[ _HBMK_aLIBPATH ], "/opt/local/lib" ) /* For MacPorts (formerly DarwinPorts) */
               ENDIF
               IF hb_DirExists( "/sw/lib" )
                  AAddNew( hbmk[ _HBMK_aLIBPATH ], "/sw/lib" ) /* For Fink */
               ENDIF
            CASE hbmk[ _HBMK_cPLAT ] == "bsd"
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" ) /* For ports */
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/pkg/lib" ) /* For pkgsrc */
            ENDCASE
         ENDIF
         IF IsGTRequested( hbmk, "gtxwc" )
            IF hbmk[ _HBMK_cPLAT ] == "linux" .AND. hb_DirExists( "/usr/X11R6/lib64" )
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib64" )
            ENDIF
            IF hbmk[ _HBMK_cPLAT ] == "minix" .AND. hb_DirExists( "/usr/pkg/X11R6/lib" )
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/pkg/X11R6/lib" )
            ENDIF
            AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib" )
            AAdd( l_aLIBSYS, "X11" )
         ENDIF

         /* Hack needed for OpenBSD to find dynamic libs referenced from harbour dynlib (embedded dirs are ignored) */
         IF hbmk[ _HBMK_cPLAT ] == "bsd" .AND. hbmk[ _HBMK_lSHARED ]
            AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib" )
            AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" )
         ENDIF

         IF hbmk[ _HBMK_cPLAT ] == "cygwin"
            l_aLIBSHAREDPOST := { "hbmainstd" }
            l_aLIBSHARED := { cHarbourDyn + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) }
            IF ! l_lNOHBLIB .AND. ! hbmk[ _HBMK_lCreateDyn ]
               l_aLIBSTATICPOST := l_aLIBSHAREDPOST
            ENDIF
         ENDIF

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw64" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "clang" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "tcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingwarm" )

         hbmk[ _HBMK_nCmd_FNF ] := _FNF_FWDSLASH

         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] == "clang"
            cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "clang++" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ]
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "clang" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ] )
         CASE hbmk[ _HBMK_cCOMP ] == "tcc"
            cBin_CompCPP := "tcc.exe"
            cBin_CompC := cBin_CompCPP
         OTHERWISE
            cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ]
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ] )
         ENDCASE
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
            IF HBMK_ISCOMP( "gcc|mingw" )
               cOpt_CompC += " -march=i586 -mtune=pentiumpro"
            ENDIF
            IF ! hbmk[ _HBMK_lDEBUG ] .AND. !( hbmk[ _HBMK_cCOMP ] == "mingw64" )
               cOpt_CompC += " -fomit-frame-pointer"
            ENDIF
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall -pedantic" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall" )           ; EXIT
         CASE _WARN_LOW
            AAdd( hbmk[ _HBMK_aOPTC ], "-Wmissing-braces -Wreturn-type -Wformat" )
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. ! hbmk[ _HBMK_lCPP ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration" )
            ENDIF
            EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
         ENDSWITCH
         IF hbmk[ _HBMK_lSAFE ]
            IF hbmk[ _HBMK_cPLAT ] == "win"
               /* It is also supported by official mingw 4.4.x and mingw64 4.4.x,
                  but not supported by mingw tdm 4.4.x, so I only enable it on or
                  above 4.5.0 [vszakats] */
               IF hbmk[ _HBMK_nCOMPVer ] > 44
                  AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--nxcompat" )
                  AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--dynamicbase" )
                  AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--nxcompat" )
                  AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--dynamicbase" )
               ENDIF
            ENDIF
         ENDIF
         cOpt_CompC += " {FC}"
         cOptIncMask := "-I{DI}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            IF .T. /* EXPERIMENTAL */
               lCHD_Comp := .T.
               cOpt_CompC += " {LC}"
            ELSE
               cOpt_CompC += " {IC} -o {OO}"
            ENDIF
            AAdd( hbmk[ _HBMK_aOPTC ], "-pipe" )
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         AAddNotEmpty( hbmk[ _HBMK_aOPTCX ], gcc_opt_lngc_fill( hbmk ) )
         AAddNotEmpty( hbmk[ _HBMK_aOPTCPPX ], gcc_opt_lngcpp_fill( hbmk ) )
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-shared -o {OD} {LO} {FD} {IM} {DL} {LS}{SCRIPT_MINGW}"
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {LS} {FL} {IM} {DL}"
         bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_gcc( hbmk, hbmk[ _HBMK_cCCPREFIX ] + "dlltool" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ] + " {FI} -d {ID} -l {OL}", cSourceDLL, cTargetLib, cFlags ) }
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cImpLibExt := cLibLibExt
         IF hbmk[ _HBMK_cCOMP ] == "tcc"
            cBin_Lib := "tiny_libmaker.exe"
         ELSE
            cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + hbmk[ _HBMK_cCCEXT ]
         ENDIF
#if defined( __PLATFORM__WINDOWS )
         hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
#endif
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cLibObjPrefix := NIL
         IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
            cBin_Lib     := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Lib, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompCPP := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompCPP, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompC   := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompC, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_Link    := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Link, hbmk[ _HBMK_nCmd_Esc ] )
         ENDIF
         cBin_LibHBX := hbmk[ _HBMK_cCCPREFIX ] + "nm" + hbmk[ _HBMK_cCCEXT ]
         cOpt_LibHBX := "-g --defined-only -C {LI}"
         IF hbmk[ _HBMK_cCOMP ] == "tcc"
            DO CASE
            CASE hbmk[ _HBMK_cPLAT ] == "wce"
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-subsystem=wince" )
            CASE hbmk[ _HBMK_lGUI ]
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-subsystem=gui" )
               IF ! l_lNOHBLIB
                  l_cCMAIN := "hb_forceLinkMainWin"
               ENDIF
            OTHERWISE
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-subsystem=console" )
            ENDCASE
         ELSE
            IF !( hbmk[ _HBMK_cPLAT ] == "wce" )
               IF hbmk[ _HBMK_lGUI ]
                  AAdd( hbmk[ _HBMK_aOPTL ], "-mwindows" )
                  IF ! l_lNOHBLIB
                     l_cCMAIN := "hb_forceLinkMainWin"
                  ENDIF
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTL ], "-mconsole" )
               ENDIF
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSTATICFULL ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-static" )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,-Map,{OM}" )
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--out-implib,{OI}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--out-implib,{OI}" )
         ENDIF
         IF l_lLIBGROUPING .AND. HBMK_ISCOMP( "mingw|mingw64|mingwarm" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} {LF} -Wl,--end-group" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--start-group {LL} {LB} {LF} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB} {LF}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "{LL} {LB} {LF}" )
            l_aLIBHBBASE_2 := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_2_MT, aLIB_BASE_2 )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_lCreateLib ]
               cBin_Post := "strip"
               cOpt_Post := "-S {OB}"
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
            ENDIF
         ENDIF
         IF lStopAfterCComp
            IF ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
         ENDIF
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         IF hbmk[ _HBMK_lWINUNI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "wce"
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNDER_CE" )
            AAdd( hbmk[ _HBMK_aOPTRES ], "-DUNDER_CE" )
         ENDIF
         DO CASE
         CASE _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            /* NOTE: Newer xhb versions use "-x.y.z" version numbers. */
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "xharbourmt", "xharbour" ) }
         OTHERWISE
            l_aLIBSHARED := { cHarbourDyn + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) }
         ENDCASE

         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            IF ! hbmk[ _HBMK_lGUI ]
               l_aLIBSHAREDPOST := { "mainstd" }
            ENDIF
         ELSE
            IF hbmk[ _HBMK_lGUI ]
               l_aLIBSHAREDPOST := { "hbmainwin" }
            ELSE
               l_aLIBSHAREDPOST := { "hbmainstd" }
            ENDIF
            IF ! l_lNOHBLIB .AND. ! hbmk[ _HBMK_lCreateDyn ]
               l_aLIBSTATICPOST := l_aLIBSHAREDPOST
            ENDIF
         ENDIF

         IF HBMK_ISCOMP( "mingw|mingw64|mingwarm|clang" )
            cBin_Res := hbmk[ _HBMK_cCCPREFIX ] + "windres" + hbmk[ _HBMK_cCCEXT ]
            cResExt := ".reso"
            cOpt_Res := "{FR} {IR} -O coff -o {OS}"
            IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
               cBin_Res := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Res, hbmk[ _HBMK_nCmd_Esc ] )
            ENDIF
         ENDIF

      CASE hbmk[ _HBMK_cPLAT ] == "os2" .AND. HBMK_ISCOMP( "gcc|gccomf" )
         hbmk[ _HBMK_nCmd_FNF ] := _FNF_BCKSLASH
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ] )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall -pedantic" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall" )           ; EXIT
         CASE _WARN_LOW
            AAdd( hbmk[ _HBMK_aOPTC ], "-Wmissing-braces -Wreturn-type -Wformat" )
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. ! hbmk[ _HBMK_lCPP ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration" )
            ENDIF
            EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            cOpt_CompC += " {IC} -o {OO}"
            AAdd( hbmk[ _HBMK_aOPTC ], "-pipe" )
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         AAddNotEmpty( hbmk[ _HBMK_aOPTCX ], gcc_opt_lngc_fill( hbmk ) )
         AAddNotEmpty( hbmk[ _HBMK_aOPTCPPX ], gcc_opt_lngcpp_fill( hbmk ) )
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-shared -o {OD} {LO} {LL} {LB} {LF} {FD} {IM} {DL} {LS}"
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {IM} {DL}"
         bBlk_ImpLib := {| cSourceDLL, cTargetLib | win_implib_copy( hbmk, cSourceDLL, cTargetLib ) }
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         IF hbmk[ _HBMK_cCOMP ] == "gccomf"
            cLibLibExt := ".lib"
            cImpLibExt := cImpLibExt /* ".imp" */
            cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "emxomfar" + hbmk[ _HBMK_cCCEXT ]

            AAdd( hbmk[ _HBMK_aOPTC ], "-Zomf" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-Zomf" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Zomf" )
         ELSE
            cLibLibExt := ".a"
            cImpLibExt := cLibLibExt
            cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + hbmk[ _HBMK_cCCEXT ]
         ENDIF
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,-Map,{OM}" )
         ENDIF
         IF ! hbmk[ _HBMK_lCreateLib ] .AND. hbmk[ _HBMK_lIMPLIB ]
            cBin_Post := "emximp"
            cOpt_Post := "-o {OI} {OB}"
         ENDIF
         AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB} {LF}" )
         l_aLIBHBBASE_2 := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_2_MT, aLIB_BASE_2 )
         IF ! hbmk[ _HBMK_lSHARED ]
            l_aLIBSYS := ArrayJoin( l_aLIBSYS, { "socket" } )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_lCreateLib ]
               IF hbmk[ _HBMK_cCOMP ] == "gccomf"
                  cBin_Post := "stripomf"
               ELSE
                  cBin_Post := "strip"
               ENDIF
               cOpt_Post := "-S {OB}"
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
            ENDIF
         ENDIF
         /* OS/2 needs a space between -o and file name following it */
         IF lStopAfterCComp
            IF ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o {OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o {OE}" )
         ENDIF

         l_aLIBSHAREDPOST := { "hbmainstd" }
         l_aLIBSHARED := { cHarbourDyn }

#if 0 /* Disabled because windres seems to be broken in all gcc builds as of 2010-05-05. [vszakats] */
         cBin_Res := hbmk[ _HBMK_cCCPREFIX ] + "windres" + hbmk[ _HBMK_cCCEXT ]
         cResExt := ".reso"
         IF hbmk[ _HBMK_cCOMP ] == "gccomf"
            cOpt_Res := "{FR} {IR} -O omf -o {OS}"
         ELSE
            cOpt_Res := "{FR} {IR} -o {OS}"
         ENDIF
#endif

         IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
            cBin_Lib     := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Lib, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompCPP := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompCPP, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompC   := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompC, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_Link    := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Link, hbmk[ _HBMK_nCmd_Esc ] )
#if 0
            cBin_Res     := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Res, hbmk[ _HBMK_nCmd_Esc ] )
#endif
         ENDIF

      CASE hbmk[ _HBMK_cPLAT ] == "dos" .AND. hbmk[ _HBMK_cCOMP ] == "djgpp"

         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-gstabs+" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "gpp" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCSUFFIX ] + hbmk[ _HBMK_cCCEXT ] )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall -pedantic" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall" )           ; EXIT
         CASE _WARN_LOW
            AAdd( hbmk[ _HBMK_aOPTC ], "-Wmissing-braces -Wreturn-type -Wformat" )
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. ! hbmk[ _HBMK_lCPP ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration" )
            ENDIF
            EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         AAddNotEmpty( hbmk[ _HBMK_aOPTCX ], gcc_opt_lngc_fill( hbmk ) )
         AAddNotEmpty( hbmk[ _HBMK_aOPTCPPX ], gcc_opt_lngcpp_fill( hbmk ) )
         cBin_Dyn := "dxe3gen"
         cOpt_Dyn := "--whole-archive -U {FD} -o {OD} {DL} {LO} {LL} {LB} {LF} {LS}"
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}{SCRIPT}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + hbmk[ _HBMK_cCCEXT ]
         cOpt_Lib := "{FA} rcs {OL} {LO}{SCRIPT}"
         cBin_LibHBX := hbmk[ _HBMK_cCCPREFIX ] + "nm" + hbmk[ _HBMK_cCCEXT ]
         cOpt_LibHBX := "-g --defined-only -C {LI}"
         IF l_lLIBGROUPING
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} {LF} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB} {LF}" )
            l_aLIBHBBASE_2 := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_2_MT, aLIB_BASE_2 )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ]
            AAdd( hbmk[ _HBMK_aOPTD ], "-Y {OI}" )
         ENDIF
         IF ! hbmk[ _HBMK_lSHARED ]
            l_aLIBSYS := ArrayJoin( l_aLIBSYS, { "m" } )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_lCreateLib ]
               cBin_Post := "strip"
               cOpt_Post := "-S {OB}"
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
            ENDIF
         ENDIF
         IF lStopAfterCComp
            IF ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
         ENDIF

         IF IsGTRequested( hbmk, "gtcrs" )
            AAdd( l_aLIBSYS, "pdcurses" )
         ENDIF

         l_aLIBSHARED := { cHarbourDyn + cLibExt }

         IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
            cBin_Lib     := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Lib, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompCPP := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompCPP, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompC   := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompC, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_Link    := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Link, hbmk[ _HBMK_nCmd_Esc ] )
         ENDIF

      /* Watcom family */
      CASE hbmk[ _HBMK_cCOMP ] == "watcom"

         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #elif defined( __PLATFORM__WINDOWS )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif
         hbmk[ _HBMK_nScr_Esc ] := _ESC_SGLQUOTE_WATCOM

         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         IF hbmk[ _HBMK_cPLAT ] == "linux"
            #if defined( __PLATFORM__UNIX )
               cObjExt := ".o"
            #else
               /* NOTE: This extension is used when building Linux targets on non-Linux hosts. [vszakats] */
               cObjExt := ".obj"
               /* NOTE: Hack to force no extension for binaries. Otherwise they become '.elf'. [vszakats] */
               cBinExt := "."
            #endif
         ELSE
            cObjExt := ".obj"
         ENDIF

         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         cBin_CompCPP := "wpp386" + hbmk[ _HBMK_cCCEXT ]
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            cBin_CompC := cBin_CompCPP
         ELSE
            cBin_CompC := "wcc386" + hbmk[ _HBMK_cCCEXT ]
         ENDIF
         cOpt_CompC := ""
         IF hbmk[ _HBMK_lOPTIM ]
            DO CASE
            CASE hbmk[ _HBMK_cPLAT ] == "linux" ; cOpt_CompC += " -6r -fp6"
            CASE hbmk[ _HBMK_cPLAT ] == "win"   ; cOpt_CompC += " -6s -fp6"
            CASE HBMK_ISPLAT( "dos|os2" )       ; cOpt_CompC += " -5r -fp5"
            ENDCASE
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ELSE
            IF hbmk[ _HBMK_cPLAT ] == "win"
               cOpt_CompC += " -3s"
            ELSEIF HBMK_ISPLAT( "dos|os2|linux" )
               cOpt_CompC += " -3r"
            ENDIF
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-wx" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-w3" ) ; EXIT
         CASE _WARN_LOW
            AAdd( hbmk[ _HBMK_aOPTC ], "-w1 -wcd201 -wcd367 -wcd368" )
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. ! hbmk[ _HBMK_lCPP ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-wcd124 -wcd136" )
            ENDIF
            EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w0" ) ; EXIT
         ENDSWITCH
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "linux" ; cOpt_CompC += " -zq -bt=linux {FC}"
         CASE hbmk[ _HBMK_cPLAT ] == "win"   ; cOpt_CompC += " -zq -bt=nt {FC}"
         CASE hbmk[ _HBMK_cPLAT ] == "dos"   ; cOpt_CompC += " -zq -bt=dos {FC}"
         CASE hbmk[ _HBMK_cPLAT ] == "os2"   ; cOpt_CompC += " -zq -bt=os2 {FC}"
         ENDCASE
         cOptIncMask := "-i{DI}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] ) .OR. !( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ] )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cC ] == "iso99" ; AAdd( hbmk[ _HBMK_aOPTCX ], "-za99" )
         ENDCASE
         IF lStopAfterCComp .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ]
            IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OO}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OD}" )
            ENDIF
         ENDIF
         cBin_Link := "wlink" + hbmk[ _HBMK_cCCEXT ]
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "linux" ; cOpt_Link := "OP quiet SYS linux {FL} NAME {OE} {LO} {DL} {LL} {LB} {LF}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "dos"   ; cOpt_Link := "OP quiet SYS dos32a {FL} NAME {OE} {LO} {DL} {LL} {LB} {LF}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "win"   ; cOpt_Link := "OP quiet {FL} {IM} NAME {OE} {LO} {DL} {LL} {LB} {LF} {LS}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "os2"   ; cOpt_Link := "OP quiet SYS os2v2 {FL} {IM} NAME {OE} {LO} {DL} {LL} {LB} {LF} {LS}{SCRIPT}"
         ENDCASE
         cBin_Dyn := cBin_Link
         cDynObjPrefix := cObjPrefix
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "dos"   ; cBin_Dyn := NIL
         CASE hbmk[ _HBMK_cPLAT ] == "linux" ; cOpt_Dyn := "OP quiet FORM elf dll OP exportall {FD} NAME {OD} {LO} {DL} {LL} {LB} {LF}{SCRIPT}"
            IF hbmk[ _HBMK_lCreateDyn ]
               AAdd( hbmk[ _HBMK_aLIBPATH ], hb_DirSepToOS( GetEnv( "WATCOM") + hb_ps() + "lib386" ) )
               AAdd( hbmk[ _HBMK_aLIBPATH ], hb_DirSepToOS( GetEnv( "WATCOM") + hb_ps() + "lib386" + hb_ps() + "linux" ) )
            ENDIF
         CASE hbmk[ _HBMK_cPLAT ] == "win"   ; cOpt_Dyn := "OP quiet SYS nt_dll {FD} {IM} NAME {OD} {LO} {DL} {LL} {LB} {LF} {LS}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "os2"   ; cOpt_Dyn := "OP quiet SYS os2v2_dll {FD} {IM} NAME {OD} {LO} {DL} {LL} {LB} {LF} {LS}{SCRIPT}"
         ENDCASE
         IF HBMK_ISPLAT( "win|os2" ) .AND. ! Empty( hbmk[ _HBMK_aDEF ] )
            /* TODO: Watcom wlink requires a non-standard internal layout for .def files.
                     We will need a converter and implement on-the-fly conversion
                     to a temp file and pass that via {IM}. */
            cDefPrefix := "@"
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "dos"
            /* workaround for not included automatically CLIB in pure C mode MS-DOS builds */
            AAdd( l_aLIBSYS, "clib3r" )
         ENDIF
         cBin_Lib := "wlib" + hbmk[ _HBMK_cCCEXT ]
         cOpt_Lib := "-q {FA} {OL} {LO}{SCRIPT}"
         cBin_LibHBX := cBin_Lib
         cOpt_LibHBX := "{LI}"
         IF HBMK_ISPLAT( "linux|dos|os2" )
            /* register callconv (-6r, -5r) puts an underscore after names */
            cLibHBX_Regex := R_( "[\s]_?HB_FUN_([A-Z0-9_]*)_[\s]" )
         ENDIF
         IF HBMK_ISPLAT( "win|os2" )
            bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_watcom( hbmk, cBin_Lib + " -q -o={OL} {ID}", cSourceDLL, cTargetLib, cFlags ) }
         ENDIF
         cLibLibExt := cLibExt
         cImpLibExt := cLibLibExt
         cLibObjPrefix := "-+ "
         IF hbmk[ _HBMK_lMT ] .AND. HBMK_ISPLAT( "win|os2" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-bm" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "win"
            IF hbmk[ _HBMK_lGUI ]
               /* NOTE: These could probably be optimized */
               AAdd( hbmk[ _HBMK_aOPTC ], "-bg" )
               AAdd( hbmk[ _HBMK_aOPTL ], "RU nat" )
               AAdd( hbmk[ _HBMK_aOPTL ], "SYS nt_win" )
            ELSE
               /* NOTE: These could probably be optimized */
               AAdd( hbmk[ _HBMK_aOPTC ], "-bc" )
               AAdd( hbmk[ _HBMK_aOPTL ], "RU con" )
               AAdd( hbmk[ _HBMK_aOPTL ], "SYS nt" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-d2" )
            cOpt_Link := "DEBUG ALL " + cOpt_Link
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "OP map" )
            AAdd( hbmk[ _HBMK_aOPTD ], "OP map" )
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ] .AND. HBMK_ISPLAT( "win|os2" ) /* dos? */
            AAdd( hbmk[ _HBMK_aOPTL ], "OP implib={OI}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "OP implib={OI}" )
         ENDIF
         IF HBMK_ISPLAT( "win|os2|dos" )
            AAdd( hbmk[ _HBMK_aOPTA ], "-p=64" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "win"
            IF hbmk[ _HBMK_lWINUNI ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
            ENDIF
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "win"
            IF hbmk[ _HBMK_lCreateDyn ]
               /* NOTE: Hack to avoid link errors when creating dynamic libs for non-Harbour
                        components, typically in '3rd' dirs inside Harbour repository.
                        Please tweak this fix if you know the exact reason. */
               AAdd( l_aLIBSYS, "clib3s" )
            ENDIF
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
            l_aLIBSHARED := { cHarbourDyn + cDL_Version_Alter + cLibExt }

            IF hbmk[ _HBMK_lSHARED ]
               AAdd( hbmk[ _HBMK_aOPTL ], "FILE " + hb_FNameExtSet( hbmk[ _HBMK_cHB_INSTALL_LIB ] + hb_ps() + iif( hbmk[ _HBMK_lGUI ], "hbmainwin", "hbmainstd" ), cLibExt ) )
            ENDIF
         CASE hbmk[ _HBMK_cPLAT ] == "os2"
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
            l_aLIBSHARED := { cHarbourDyn + cLibExt }

            IF hbmk[ _HBMK_lSHARED ]
               /* TOFIX: This line is plain guessing. */
               AAdd( hbmk[ _HBMK_aOPTL ], "FILE " + hb_FNameExtSet( hbmk[ _HBMK_cHB_INSTALL_LIB ] + hb_ps() + iif( hbmk[ _HBMK_lGUI ], "hbmainstd", "hbmainstd" ), cLibExt ) )
            ENDIF
         CASE hbmk[ _HBMK_cPLAT ] == "linux"
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
            l_aLIBSHARED := { hbmk[ _HBMK_cDynLibPrefix ] + cHarbourDyn + cDL_Version + hbmk[ _HBMK_cDynLibExt ] }
         ENDCASE
         IF HBMK_ISPLAT( "win|os2" )
            cBin_Res := "wrc" + hbmk[ _HBMK_cCCEXT ]
            cResExt := ".res"
            cOpt_Res := "-q -r {FR} -zm {IR} -fo={OS}"
            DO CASE
            CASE hbmk[ _HBMK_cPLAT ] == "win" ; cOpt_Res += " -bt=nt" /* default */
            CASE hbmk[ _HBMK_cPLAT ] == "os2" ; cOpt_Res += " -bt=os2"
            ENDCASE
            cResPrefix := "OP res="
         ENDIF

      CASE hbmk[ _HBMK_cPLAT ] == "win" .AND. HBMK_ISCOMP( "bcc|bcc64" )
         hbmk[ _HBMK_nCmd_FNF ] := _FNF_BCKSLASH
         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #else
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif
         IF hbmk[ _HBMK_lDEBUG ]
            IF hbmk[ _HBMK_cCOMP ] == "bcc64"
               AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-v -y" )
            ENDIF
            AAdd( hbmk[ _HBMK_aOPTL ], "-v" )
         ELSE
            AAdd( l_aCLEAN, hb_DirSepToOS( hb_FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".tds" ) ) )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-tW" )
         ENDIF
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-P" )
         ENDIF
         cLibPrefix := NIL
         IF hbmk[ _HBMK_cCOMP ] == "bcc64"
            cLibExt := ".a"
            cObjExt := ".o"
            cBin_Lib := "tlib64.exe"
         ELSE
            cLibExt := ".lib"
            cObjExt := ".obj"
            cBin_Lib := "tlib.exe"
         ENDIF
         /* Only forward slash is accepted here as option prefix. */
         cOpt_Lib := "/P128 {FA} {OL} {LO}{SCRIPT}"
         cBin_LibHBX := cBin_Lib
         cOpt_LibHBX := "{LI}, {OT}"
         cLibLibExt := cLibExt
         cImpLibExt := cLibLibExt
         cLibObjPrefix := "-+ "
         cOptIncMask := "-I{DI}"
         IF hbmk[ _HBMK_cCOMP ] == "bcc64"
            cBin_CompC := "bcc64.exe"
         ELSE
            cBin_CompC := "bcc32.exe"
         ENDIF
         cBin_CompCPP := cBin_CompC
         cOpt_CompC := "-c -q -CP437"
         IF hbmk[ _HBMK_lOPTIM ]
            IF hbmk[ _HBMK_cCOMP ] == "bcc64"
               cOpt_CompC += " -d -O2 -OS -Ov -Oc"
            ELSE
               cOpt_CompC += " -d -O2 -OS -Ov -Oc -Oi -6"
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] == "bcc64"
            cLibBCC_CRTL := "cw64mt.lib"
         ELSE
            cLibBCC_CRTL := "cw32mt.lib"
         ENDIF
         IF hbmk[ _HBMK_lWINUNI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
         ENDIF
         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            /* Adding weird hack for xhb to make it possible to force ST C mode. */
            IF AScan( hbmk[ _HBMK_aOPTC ], {| tmp | tmp == "-tW" } ) == 0
               AAdd( hbmk[ _HBMK_aOPTC ], "-tWM" )
            ELSE
               IF hbmk[ _HBMK_cCOMP ] == "bcc64"
                  cLibBCC_CRTL := "cw64.lib"
               ELSE
                  cLibBCC_CRTL := "cw32.lib"
               ENDIF
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTC ], "-tWM" )
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-w -Q" )         ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-w -Q -w-sig-" ) ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-w-sig- -w-aus- -w-ccc- -w-csu- -w-par- -w-rch- -w-ucp- -w-use- -w-prc- -w-pia-" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w-" )           ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC} {LC}"
         cBin_Res := "brcc32.exe"
         cOpt_Res := "{FR} {IR} -fo{OS}"
         cResExt := ".res"
         IF hbmk[ _HBMK_cCOMP ] == "bcc64"
            cBin_Link := "ilink64.exe"
         ELSE
            cBin_Link := "ilink32.exe"
         ENDIF
         cBin_Dyn := cBin_Link
         IF hbmk[ _HBMK_cCOMP ] == "bcc64"
            cOpt_Link := "-Gn -Tpe -L{DL} {FL} " + iif( hbmk[ _HBMK_lGUI ], "c0w64.obj", "c0x64.obj" ) + " {LO}, {OE}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} {LF} " + cLibBCC_CRTL + " import64.lib, {IM}, {LS}{SCRIPT}"
            cOpt_Dyn  := "-Gn -Tpd -L{DL} {FD} " +                          "c0d64.obj"                + " {LO}, {OD}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} {LF} " + cLibBCC_CRTL + " import64.lib, {IM}, {LS}{SCRIPT}"
         ELSE
            cOpt_Link := "-Gn -Tpe -L{DL} {FL} " + iif( hbmk[ _HBMK_lGUI ], "c0w32.obj", "c0x32.obj" ) + " {LO}, {OE}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} {LF} " + cLibBCC_CRTL + " import32.lib, {IM}, {LS}{SCRIPT}"
            cOpt_Dyn  := "-Gn -Tpd -L{DL} {FD} " +                          "c0d32.obj"                + " {LO}, {OD}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} {LF} " + cLibBCC_CRTL + " import32.lib, {IM}, {LS}{SCRIPT}"
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] == "bcc"
            /* TODO: Add support for bcc64/mkexp */
            bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_bcc( hbmk, "implib.exe -c {FI} {OL} {ID}", cSourceDLL, cTargetLib, cFlags ) }
         ENDIF
         cLibPathPrefix := ""
         cLibPathSep := ";"
         IF hbmk[ _HBMK_lMAP ]
            /* Switch detailed map on */
            AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-aa" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-aa" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-ap" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-ap" )
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ]
            /* NOTE: Borland C does not support creating implibs with a specific name,
                     so it is done using post command. The resulting implib will not be
                     as optimal as the generated one, but it _should_ be the same.
                     [vszakats] */
            /* AAdd( hbmk[ _HBMK_aOPTL ], "-Gi" ) */
            /* AAdd( hbmk[ _HBMK_aOPTD ], "-Gi" ) */
            IF hbmk[ _HBMK_cCOMP ] == "bcc64"
               cBin_Post := "mkexp.exe"
               cOpt_Post := "{OI} {OB}"
            ELSE
               cBin_Post := "implib.exe"
               cOpt_Post := "-c {OI} {OB}"
            ENDIF
         ENDIF
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            AAdd( hbmk[ _HBMK_aOPTC ], "-n" + FNameEscape( hbmk[ _HBMK_cWorkDir ], hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )
         ELSE
            IF lStopAfterCComp .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ]
               IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-n{OD}" )
               ENDIF
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] == "bcc"
            l_aLIBSHARED := { cHarbourDyn + cDL_Version_Alter + "-bcc" + cLibExt }
         ENDIF
         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. HBMK_ISCOMP( "msvc|msvc64|msvcia64|icc|iccia64" ) ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "msvcarm" ) /* NOTE: Cross-platform: wce/ARM on win/x86 */

         hbmk[ _HBMK_nCmd_FNF ] := _FNF_BCKSLASH
         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #else
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif

         /* ; Not enabled yet, because it would cause a lot of 3rd party code to
              break due to sloppy type conversions and other trivial coding mistakes
              usually not noticed with C compilers. The other side-effect, is
              much slower compilation process. [vszakats] */
#if 0
         IF hbmk[ _HBMK_lCPP ] == NIL
            hbmk[ _HBMK_lCPP ] := .T.
         ENDIF
#endif
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-Zi" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-debug" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-debug" )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-subsystem:windows" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-subsystem:console" )
         ENDIF
         IF hbmk[ _HBMK_lCPP ] != NIL
            IF hbmk[ _HBMK_lCPP ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-TP" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-TC" )
            ENDIF
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         cImpLibExt := cLibLibExt
         IF HBMK_ISCOMP( "icc|iccia64" )
            cBin_Lib := "xilib.exe"
            cBin_CompC := "icl.exe"
            cBin_Link := "xilink.exe"
            cBin_Dyn := cBin_Link
         ELSE
            cBin_Lib := "lib.exe"
            IF hbmk[ _HBMK_cCOMP ] == "msvcarm" .AND. ( hbmk[ _HBMK_nCOMPVer ] != 0 .AND. hbmk[ _HBMK_nCOMPVer ] < 1400 )
               cBin_CompC := "clarm.exe"
            ELSE
               cBin_CompC := "cl.exe"
            ENDIF
            cBin_Link := "link.exe"
            cBin_Dyn := cBin_Link
         ENDIF
         cBin_CompCPP := cBin_CompC
         cOpt_Lib := "-nologo {FA} -out:{OL} {LO}{SCRIPT}"
         cOpt_Dyn := "-nologo {FD} {IM} -dll -out:{OD} {DL} {LO} {LL} {LB} {LF} {LS}{SCRIPT}"
         cOpt_CompC := "-nologo -c"
         cBin_LibHBX := "dumpbin.exe"
         cOpt_LibHBX := "-symbols {LI}"
         cLibHBX_Regex := R_( "SECT[0-9A-Z][0-9A-Z ].*[Ee]xternal.*_?HB_FUN_([A-Z0-9_]*)[\s]" )
         IF hbmk[ _HBMK_lOPTIM ]
            IF hbmk[ _HBMK_cPLAT ] == "wce"
               IF hbmk[ _HBMK_nCOMPVer ] != 0 .AND. hbmk[ _HBMK_nCOMPVer ] < 1400
                  cOpt_CompC += " -Oxsb1 -GF"
               ELSE
                  cOpt_CompC += " -Os -Gy"
               ENDIF
            ELSE
               IF hbmk[ _HBMK_nCOMPVer ] != 0 .AND. hbmk[ _HBMK_nCOMPVer ] < 1400
                  cOpt_CompC += " -Ogt2yb1p -GX- -G6"
               ELSE
                  cOpt_CompC += " -O2"
               ENDIF
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "win"
            IF hbmk[ _HBMK_nCOMPVer ] != 0 .AND. hbmk[ _HBMK_nCOMPVer ] < 1400
               IF hbmk[ _HBMK_lDEBUG ]
                  AAdd( hbmk[ _HBMK_aOPTC ], "-MTd" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-MT" )
               ENDIF
            ENDIF
         ENDIF
         IF HBMK_ISCOMP( "icc|iccia64" )
            SWITCH hbmk[ _HBMK_nWARN ]
            CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W4" ) ; EXIT
            CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W3" ) ; EXIT /* -W4 is deadly on icc */
            CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-W2" ) ; EXIT
            CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-W0" ) ; EXIT
            ENDSWITCH
         ELSE
            SWITCH hbmk[ _HBMK_nWARN ]
            CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W4" ) ; EXIT
            CASE _WARN_YES
               IF hbmk[ _HBMK_cCOMP ] == "msvcarm" .AND. hbmk[ _HBMK_nCOMPVer ] != 0 .AND. hbmk[ _HBMK_nCOMPVer ] < 1400
                  /* Lowered warning level to avoid large amount of warnings in system headers.
                     Maybe this is related to the msvc2003 kit I was using. [vszakats] */
                  AAdd( hbmk[ _HBMK_aOPTC ], "-W3" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-W4 -wd4127" )
               ENDIF
               EXIT
            CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-W2" ) ; EXIT
            CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-W0" ) ; EXIT
            ENDSWITCH
         ENDIF
         IF hbmk[ _HBMK_lSAFE ]
            IF hbmk[ _HBMK_cPLAT ] == "win"
               /* MSVS 2005 SP1 also supports it, but we only enable it
                  for 2008 and upper [vszakats] */
               IF hbmk[ _HBMK_nCOMPVer ] > 1400
                  AAdd( hbmk[ _HBMK_aOPTL ], "-nxcompat" )
                  AAdd( hbmk[ _HBMK_aOPTL ], "-dynamicbase" )
                  AAdd( hbmk[ _HBMK_aOPTL ], "-fixed:no" ) /* is this useful? */
                  AAdd( hbmk[ _HBMK_aOPTD ], "-nxcompat" )
                  AAdd( hbmk[ _HBMK_aOPTD ], "-dynamicbase" )
               ENDIF
            ENDIF
         ENDIF
         cOpt_CompC += " {FC} {LC}"
         cOptIncMask := "-I{DI}"
         cOpt_Link := "-nologo -out:{OE} {LO} {DL} {FL} {IM} {LL} {LB} {LF} {LS}{SCRIPT}"
         SWITCH hbmk[ _HBMK_cCOMP ]
         CASE "msvc"     ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:x86"  ) ; EXIT
         CASE "msvc64"   ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:x64"  ) ; EXIT
         CASE "msvcia64" ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:ia64" ) ; EXIT
         CASE "icc"      ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:x86"  ) ; EXIT
         CASE "iccia64"  ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:ia64" ) ; EXIT
         CASE "msvcarm"  ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:xarm" ) ; EXIT
         CASE "msvcmips" ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:mips" ) ; EXIT
         CASE "msvcsh"   ; AAdd( hbmk[ _HBMK_aOPTI ], "-machine:sh5"  ) ; EXIT
         ENDSWITCH
         bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_msvc( hbmk, cBin_Lib + " -nologo {FI} -def:{ID} -out:{OL}", cSourceDLL, cTargetLib, cFlags ) }
         cLibPathPrefix := "-libpath:"
         cLibPathSep := " "
         cDefPrefix := "-def:"
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-map" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-map" )
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-implib:{OI}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-implib:{OI}" )
         ENDIF
         IF hbmk[ _HBMK_lWINUNI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "wce"
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNDER_CE" )
            AAdd( hbmk[ _HBMK_aOPTRES ], "-DUNDER_CE" )
            DO CASE
            CASE hbmk[ _HBMK_cCOMP ] == "msvcarm"
               AAdd( hbmk[ _HBMK_aOPTC ], "-D_M_ARM -DARM -D_ARM_" )
            CASE hbmk[ _HBMK_cCOMP ] == "msvc"
               /* TODO */
            ENDCASE
            AAdd( hbmk[ _HBMK_aOPTL ], "-subsystem:windowsce" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-subsystem:windowsce" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-nodefaultlib:oldnames.lib" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-nodefaultlib:oldnames.lib" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-nodefaultlib:kernel32.lib" )
            IF hbmk[ _HBMK_nCOMPVer ] >= 1400
               AAdd( hbmk[ _HBMK_aOPTL ], "-manifest:no" )
            ENDIF
         ENDIF
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            AAdd( hbmk[ _HBMK_aOPTC ], "-Fo" + FNameEscape( hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ), hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) /* NOTE: Ending path sep is important. */
         ELSE
            IF lStopAfterCComp .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ]
               IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
                  AAdd( hbmk[ _HBMK_aOPTC ], "-Fo{OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-Fo{OD}" )
               ENDIF
            ENDIF
         ENDIF
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         l_aLIBSHARED := { cHarbourDyn + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) + cLibExt }
         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF ! HBMK_ISCOMP( "icc|iccia64" )
            cBin_Res := "rc.exe"
            cOpt_Res := "{FR} -fo {OS} {IR}"
            IF hbmk[ _HBMK_nCOMPVer ] >= 1600
               cOpt_Res := "-nologo " + cOpt_Res  /* NOTE: Only in MSVC 2010 and upper. [vszakats] */
            ENDIF
            cResExt := ".res"
         ENDIF

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "pocc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "pocc64" ) .OR. ; /* NOTE: Cross-platform: win/amd64 on win/x86 */
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "poccarm" ) .OR. ; /* NOTE: Cross-platform: wce/ARM on win/x86 */
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "xcc" )

         hbmk[ _HBMK_nCmd_FNF ] := _FNF_BCKSLASH
         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #else
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif

         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-subsystem:windows" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-subsystem:console" )
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-Zi" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         cImpLibExt := cLibLibExt
         IF hbmk[ _HBMK_cCOMP ] == "xcc"
            cBin_CompC := "xCC.exe"
            cBin_Lib := "xLib.exe"
            cBin_Link := "xLink.exe"
            cBin_Res := "xRC.exe"
         ELSE
            cBin_CompC := "pocc.exe"
            cBin_Lib := "polib.exe"
            cBin_Link := "polink.exe"
            cBin_Res := "porc.exe"
         ENDIF
         cBin_CompCPP := cBin_CompC
         cBin_Dyn := cBin_Link
         cOpt_CompC := "-c -Ze"
         IF !( hbmk[ _HBMK_cCOMP ] == "poccarm" ) .AND. ;
            !( hbmk[ _HBMK_cCOMP ] == "xcc" ) /* xcc does not have this enabled in default Harbour builds. */
            cOpt_CompC += " -MT"
         ENDIF
         IF !( hbmk[ _HBMK_cCOMP ] == "xcc" )
            cOpt_CompC += " -Go"
         ENDIF
         cOpt_CompC += " {FC} {IC} -Fo{OO}"
         IF Empty( hbmk[ _HBMK_cWorkDir ] )
            hbmk[ _HBMK_cWorkDir ] := "."
         ENDIF
         cOptIncMask := "-I{DI}"
         cOpt_Dyn := "{FD} {IM} -dll -out:{OD} {DL} {LO} {LL} {LB} {LF} {LS}"
         bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_pocc( hbmk, cBin_Lib + " {ID} -out:{OL}", cSourceDLL, cTargetLib, cFlags ) }
         cBin_LibHBX := "podump.exe"
         cOpt_LibHBX := "-symbols {LI}"
         cLibHBX_Regex := R_( "SECT[0-9A-Z][0-9A-Z ].*[Ee]xternal.*_?HB_FUN_([A-Z0-9_]*)[\s]" )
         IF hbmk[ _HBMK_lWINUNI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "wce"
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_WINCE" ) /* Required by pocc Windows headers */
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNDER_CE" )
            AAdd( hbmk[ _HBMK_aOPTRES ], "-DUNDER_CE" )
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] == "pocc"
            IF hbmk[ _HBMK_lOPTIM ]
               AAdd( hbmk[ _HBMK_aOPTC ], "-Ot" )
            ENDIF
            AAdd( hbmk[ _HBMK_aOPTC ], "-Tx86-coff" )
         CASE hbmk[ _HBMK_cCOMP ] == "pocc64"
            AAdd( hbmk[ _HBMK_aOPTC ], "-Tamd64-coff" )
         CASE hbmk[ _HBMK_cCOMP ] == "poccarm"
            AAdd( hbmk[ _HBMK_aOPTC ], "-Tarm-coff" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_M_ARM -DARM" )
         ENDCASE
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W2" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W1" ) ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-W1" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-W0" ) ; EXIT
         ENDSWITCH
         cOpt_Res := "{FR} -Fo{OS} {IR}"
         cResExt := ".res"
         cOpt_Lib := "{FA} -out:{OL} {LO}"
         IF hbmk[ _HBMK_lMT ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-MT" )
         ENDIF
         cOpt_Link := "-out:{OE} {LO} {DL} {FL} {IM} {LL} {LB} {LF} {LS}"
         cLibPathPrefix := "-libpath:"
         cLibPathSep := " "
         cDefPrefix := "-def:"
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-map" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-map" )
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-implib:{OI}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-implib:{OI}" )
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-debug" )
         ENDIF
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         l_aLIBSHARED := { cHarbourDyn + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) + cLibExt }
         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

      CASE ( hbmk[ _HBMK_cPLAT ] == "sunos" .AND. hbmk[ _HBMK_cCOMP ] == "sunpro" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux" .AND. hbmk[ _HBMK_cCOMP ] == "sunpro" )

         hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_Lib := "ar"
         IF hbmk[ _HBMK_cPLAT ] == "linux"
            cOpt_Lib := "{FA} rcs {OL} {LO}"
         ELSE
            cOpt_Lib := "{FA} rc {OL} {LO}"
         ENDIF
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "sunCC" + hbmk[ _HBMK_cCCSUFFIX ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "suncc" + hbmk[ _HBMK_cCCSUFFIX ] )
         cOpt_CompC := "-c {FC}"
         IF hbmk[ _HBMK_lOPTIM ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-fast" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-xnolibmopt" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-fast" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-xnolibmopt" )
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX
         CASE _WARN_YES
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-erroff=%none" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-erroff=%all" ) ; EXIT
         ENDSWITCH
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cC ] == "iso90" ; AAdd( hbmk[ _HBMK_aOPTCX ], "-xc99=none" )
         CASE hbmk[ _HBMK_cC ] == "iso99" ; AAdd( hbmk[ _HBMK_aOPTCX ], "-xc99=all" )
         ENDCASE
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-G {FD} -o {OD} {DL} {LO} {LL} {LB} {LF} {LS}"
         IF ! lStopAfterCComp
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB} {LF}" )
            l_aLIBHBBASE_2 := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_2_MT, aLIB_BASE_2 )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-M{OM}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-M{OM}" )
         ENDIF
         IF hbmk[ _HBMK_lSTATICFULL ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-B -static" )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_cPLAT ] == "sunos"
               cBin_Post := "strip"
               IF hbmk[ _HBMK_lCreateDyn ] .OR. hbmk[ _HBMK_lCreateLib ]
                  cOpt_Post := "-S {OB}"
               ELSE
                  cOpt_Post := "{OB}"
               ENDIF
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
            ENDIF
         ENDIF
         IF lStopAfterCComp
            IF ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o {OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o {OE}" )
         ENDIF

         IF hbmk[ _HBMK_lPIC ]
            IF hbmk[ _HBMK_cPLAT ] == "sunos" .AND. hbmk[ _HBMK_cCPU ] == "sparc"
               AAdd( hbmk[ _HBMK_aOPTC ], "-xcode=pic32" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-KPIC" )
            ENDIF
         ENDIF

         /* Add system libraries */
         IF ! hbmk[ _HBMK_lSHARED ]
            AAdd( l_aLIBSYS, "m" )
            IF hbmk[ _HBMK_lMT ]
               AAdd( l_aLIBSYS, "pthread" )
            ENDIF
            DO CASE
            CASE HBMK_ISPLAT( "linux|cygwin" )
               AAdd( l_aLIBSYS, "rt" )
               AAdd( l_aLIBSYS, "dl" )
            CASE hbmk[ _HBMK_cPLAT ] == "sunos"
               AAdd( l_aLIBSYS, "rt" )
               AAdd( l_aLIBSYS, "socket" )
               AAdd( l_aLIBSYS, "nsl" )
               AAdd( l_aLIBSYS, "resolv" )
            ENDCASE

            IF ! Empty( cLIB_BASE_PCRE ) .AND. ! hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_PCRE ) )
               AAdd( l_aLIBSYS, "pcre" )
               cLIB_BASE_PCRE := NIL
            ENDIF
            IF ! Empty( cLIB_BASE_ZLIB ) .AND. ! hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_ZLIB ) )
               AAdd( l_aLIBSYS, "z" )
               cLIB_BASE_ZLIB := NIL
            ENDIF
         ENDIF

         IF IsGTRequested( hbmk, "gtcrs" )
            /* TOFIX: Sometimes 'ncur194' is needed. */
            AAdd( l_aLIBSYS, iif( hbmk[ _HBMK_cPLAT ] == "sunos", "curses", "ncurses" ) )
         ENDIF
         IF IsGTRequested( hbmk, "gtsln" )
            AAdd( l_aLIBSYS, "slang" )
         ENDIF
         IF IsGTRequested( hbmk, "gtxwc" )
            IF hbmk[ _HBMK_cPLAT ] == "linux" .AND. hb_DirExists( "/usr/X11R6/lib64" )
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib64" )
            ENDIF
            AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib" )
            AAdd( l_aLIBSYS, "X11" )
         ENDIF

         IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
            cBin_CompCPP := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompCPP, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_CompC   := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_CompC, hbmk[ _HBMK_nCmd_Esc ] )
            cBin_Link    := FNameEscape( hbmk[ _HBMK_cCCPATH ] + hb_ps() + cBin_Link, hbmk[ _HBMK_nCmd_Esc ] )
         ENDIF

      CASE hbmk[ _HBMK_cPLAT ] == "vxworks" .AND. hbmk[ _HBMK_cCOMP ] == "diab"

         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #elif defined( __PLATFORM__WINDOWS )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         vxworks_env_init( hbmk )
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "dar"
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "dplus"
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "dcc" )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -XO level-3"
         ENDIF
         tmp := "-WDVSB_DIR=" + hb_DirSepToOS( GetEnv( "WIND_BASE" ) + "/target/lib" )
         AAdd( hbmk[ _HBMK_aOPTC ], tmp )
         AAdd( hbmk[ _HBMK_aOPTL ], tmp )
         AAdd( hbmk[ _HBMK_aOPTD ], tmp )
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Xlint" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Xlint" ) ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-W" )        ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "" )          ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cC ] == "iso90" ; AAdd( hbmk[ _HBMK_aOPTCX ], "-Xdialect-ansi" ) /* Conform to ANSI X3.159-1989 with some additions [as shown in the table below]. */
         ENDCASE
         /* lib path list ({DL}) must precede lib list */
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-Xpic -Wl, -Xshared -o {OD} {LO} {DL} {FD} {LS}"
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {DL} {FL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB} {LF}" )
         AAdd( hbmk[ _HBMK_aOPTD ], "{LL} {LB} {LF}" )
         l_aLIBHBBASE_2 := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_2_MT, aLIB_BASE_2 )
         IF hbmk[ _HBMK_lSTATICFULL ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl, -Xstatic" ) /* not tested */
         ELSE
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl, -Xdynamic" )
         ENDIF
         IF hbmk[ _HBMK_lSHARED ]
            /* TOFIX: .so is referred by its full link time search path,
                      there is even a backslash present in the dir formed by
                      the linker */
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl, -Xdynamic" )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            /* TODO: Map goes to stdout, we should ideally catch it to {OM} */
            AAdd( hbmk[ _HBMK_aOPTL ], "-m16" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-m16" )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_lCreateLib ]
               cBin_Post := "strip"
               IF hbmk[ _HBMK_lCreateDyn ] .OR. hbmk[ _HBMK_lCreateLib ]
                  cOpt_Post := "-S {OB}"
               ELSE
                  cOpt_Post := "{OB}"
               ENDIF
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
               AAdd( hbmk[ _HBMK_aOPTD ], "-s" )
            ENDIF
         ENDIF
         IF lStopAfterCComp
            IF ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
         ENDIF

         IF hbmk[ _HBMK_lPIC ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-Xpic" )
         ENDIF

         /* Add system libraries */
         IF ! hbmk[ _HBMK_lSHARED ]
            IF ! Empty( cLIB_BASE_PCRE ) .AND. ! hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_PCRE ) )
               AAdd( l_aLIBSYS, "pcre" )
               cLIB_BASE_PCRE := NIL
            ENDIF
            IF ! Empty( cLIB_BASE_ZLIB ) .AND. ! hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_ZLIB ) )
               AAdd( l_aLIBSYS, "z" )
               cLIB_BASE_ZLIB := NIL
            ENDIF
         ENDIF

      ENDCASE

      IF hbmk[ _HBMK_lCreateDyn ]
         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ) .OR. ;
            hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10
            AAdd( hbmk[ _HBMK_aOPTC ], "-D__EXPORT__" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTC ], "-DHB_DYNLIB" )
         ENDIF
      ENDIF
#if 0
      IF hbmk[ _HBMK_lCreateDyn ]
         IF !( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ) .OR. ;
               hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
               hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB20 )
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-DHB_DYNLIB" )
         ENDIF
      ENDIF
#endif
   ENDIF

   /* Call plugins */

   IF ! PlugIn_Execute_All( hbmk, "pre_all" )
      IF hbmk[ _HBMK_lBEEP ]
         DoBeep( .F. )
      ENDIF
      RETURN _EXIT_PLUGINPREALL
   ENDIF

   /* ; */

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. hbmk[ _HBMK_lCreateImpLib ] .AND. ! hbmk[ _HBMK_lDumpInfo ]
      /* OBSOLETE functionality */
      IF DoIMPLIB( hbmk, bBlk_ImpLib, cLibLibPrefix, cLibLibExt, hbmk[ _HBMK_aIMPLIBSRC ], hbmk[ _HBMK_cPROGNAME ], "" )
         DoInstCopy( hbmk )
      ENDIF
      hbmk[ _HBMK_lStopAfterInit ] := .T.
   ENDIF

   hb_default( @hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )

   IF ! hbmk[ _HBMK_lStopAfterInit ]
      IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
         /* NOTE: Ending path sep is important. */
         /* Different escaping for internal and external compiler. */
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-o" + hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-o" + FNameEscape( hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ), hbmk[ _HBMK_nCmd_Esc ] ) )
         ENDIF
      ENDIF
   ENDIF

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]
      hb_FNameSplit( hbmk[ _HBMK_cPROGNAME ], @cDir, @cName, @cExt )
      IF l_cIMPLIBDIR == NIL
         l_cIMPLIBDIR := cDir
      ENDIF
      DO CASE
      CASE ! lStopAfterCComp
         IF Empty( cExt ) .AND. ! Empty( cBinExt )
            hbmk[ _HBMK_cPROGNAME ] := hb_FNameMerge( cDir, cName, cBinExt )
         ENDIF
         IF l_cIMPLIBNAME == NIL
            l_cIMPLIBNAME := cName + _HBMK_IMPLIB_EXE_POST
         ENDIF
         l_cIMPLIBNAME := hb_FNameMerge( l_cIMPLIBDIR, cLibLibPrefix + l_cIMPLIBNAME, cImpLibExt )
      CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateDyn ]
         IF ! HBMK_ISPLAT( "win|os2|dos" )
            l_cLIBSELF := cName
         ENDIF
         cName := hbmk[ _HBMK_cDynLibPrefix ] + cName
         IF Empty( cExt ) .AND. ! Empty( hbmk[ _HBMK_cDynLibExt ] )
            cExt := hbmk[ _HBMK_cDynLibExt ]
         ENDIF
         hbmk[ _HBMK_cPROGNAME ] := hb_FNameMerge( cDir, cName, cExt )
         IF l_cIMPLIBNAME == NIL
            /* By default add default postfix to avoid collision with static lib
               with the same name. */
            l_cIMPLIBNAME := cName + _HBMK_IMPLIB_DLL_POST
         ENDIF
         IF hbmk[ _HBMK_lIMPLIB ] .AND. HBMK_ISPLAT( "win|os2|dos" )
            l_cLIBSELF := l_cIMPLIBNAME
         ENDIF
         l_cIMPLIBNAME := hb_FNameMerge( l_cIMPLIBDIR, cLibLibPrefix + l_cIMPLIBNAME, cImpLibExt )
      CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ]
         l_cLIBSELF := cName
         hbmk[ _HBMK_cPROGNAME ] := hb_FNameMerge( cDir, cLibLibPrefix + cName, iif( Empty( cLibLibExt ), cExt, cLibLibExt ) )
      ENDCASE
   ENDIF

   DoLinkCalc( hbmk )

   /* Generate header with repository ID information */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! hbmk[ _HBMK_lDumpInfo ]
      IF ! Empty( l_cVCSHEAD )
         tmp1 := VCSID( hbmk, l_cVCSDIR, l_cVCSHEAD, @tmp2 )

         /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
         tmp := ;
            "/* Automatically generated by " + _SELF_NAME_ + ". Do not edit. */" + _FIL_EOL + ;
            "#define _HBMK_VCS_TYPE_ " + '"' + tmp2 + '"' + _FIL_EOL + ;
            "#define _HBMK_VCS_ID_   " + '"' + tmp1 + '"' + _FIL_EOL

         /* Update only if something changed to trigger rebuild only if really needed.
            Do not update if the VCS header is already present, but currently extracted
            VCS information is empty (this is sign of extraction command failure). */
         tmp2 := hb_MemoRead( l_cVCSHEAD )
         IF ( Empty( tmp2 ) .OR. ! Empty( tmp1 ) ) .AND. ;
            ( hbmk[ _HBMK_lREBUILD ] .OR. !( tmp2 == tmp ) )
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating VCS header: %1$s" ), l_cVCSHEAD ) )
            ENDIF
            hb_MemoWrit( l_cVCSHEAD, tmp )
         ENDIF
      ENDIF
      IF ! Empty( l_cTSHEAD )

         /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
         tmp1 := hb_DateTime()
         tmp := ;
            "/* Automatically generated by " + _SELF_NAME_ + ". Do not edit. */" + _FIL_EOL + ;
            "#define _HBMK_BUILD_DATE_      " + '"' +            DToS( tmp1 )         + '"' + _FIL_EOL + ;
            "#define _HBMK_BUILD_TIME_      " + '"' + SubStr( hb_TToS( tmp1 ), 9, 6 ) + '"' + _FIL_EOL + ;
            "#define _HBMK_BUILD_TIMESTAMP_ " + '"' +         hb_TToS( tmp1 )         + '"' + _FIL_EOL

         IF hbmk[ _HBMK_lInfo ]
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating timestamp header: %1$s" ), l_cTSHEAD ) )
         ENDIF
         hb_MemoWrit( l_cTSHEAD, tmp )
      ENDIF
   ENDIF

   /* Header paths */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ]
      convert_incpaths_to_options( hbmk, cOptIncMask, lCHD_Comp )
   ENDIF

   /* Do header detection and create incremental file list for .c files */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! hbmk[ _HBMK_lDumpInfo ]

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ] .AND. ! hbmk[ _HBMK_lCLEAN ]
         l_aC_TO_DO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aC ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C %1$s %2$s", tmp, FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .T., cBin_CompC ) ) .OR. ;
               hb_FSize( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) == 0
               AAdd( l_aC_TO_DO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aC_TO_DO := AClone( hbmk[ _HBMK_aC ] )
      ENDIF
   ENDIF

   /* Do header detection and create incremental file list for .cpp files */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! hbmk[ _HBMK_lDumpInfo ]

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ] .AND. ! hbmk[ _HBMK_lCLEAN ]
         l_aCPP_TO_DO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aCPP ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C++ %1$s %2$s", tmp, FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .T., cBin_CompCPP ) ) .OR. ;
               hb_FSize( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) == 0
               AAdd( l_aCPP_TO_DO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aCPP_TO_DO := AClone( hbmk[ _HBMK_aCPP ] )
      ENDIF
   ENDIF

   /* Create incremental file list for .prg files */

   IF ( ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C ) .OR. ;
      ( hbmk[ _HBMK_lCreateHRB ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] ) .OR. ; /* or in HRB mode */
      ( hbmk[ _HBMK_lCreatePPO ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] )        /* or in preprocessor mode */

      IF ! hbmk[ _HBMK_lDumpInfo ]
         PlugIn_Execute_All( hbmk, "pre_prg" )
      ENDIF

      /* Incremental */

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ] .AND. ;
         ! hbmk[ _HBMK_lCLEAN ] .AND. ! hbmk[ _HBMK_lDumpInfo ]
         IF hbmk[ _HBMK_lCreateHRB ] .AND. hbmk[ _HBMK_lStopAfterHarbour ]
            cHarbourOutputExt := ".hrb"
            cHarbourOutputDir := hbmk[ _HBMK_cWorkDir ]
         ELSEIF hbmk[ _HBMK_lCreatePPO ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] /* .ppo files are the dependents in preprocessor mode */
            cHarbourOutputExt := ".ppo"
            cHarbourOutputDir := cHarbourPPODir
         ELSE
            cHarbourOutputExt := ".c"
            cHarbourOutputDir := hbmk[ _HBMK_cWorkDir ]
         ENDIF
         l_aPRG_TO_DO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
            IF LEFTEQUAL( tmp, "@" ) .AND. Lower( hb_FNameExt( tmp ) ) == ".clp"
               tmp3 := SubStr( tmp, 2 )
            ELSE
               tmp3 := tmp
            ENDIF
            tmp4 := FNameDirExtSet( tmp3, cHarbourOutputDir, cHarbourOutputExt )
            IF hbmk[ _HBMK_lDEBUGINC ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: PRG %1$s %2$s", ;
                  tmp3, tmp4 ) )
            ENDIF
            IF ! hb_FGetDateTime( tmp4, @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp3, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .F., cBin_CompC ) ) .OR. ;
               checkDepTime( hbmk, tmp4, tmp2 )
               AAdd( l_aPRG_TO_DO, tmp )
            ENDIF
         NEXT
      ELSE
         IF ! Empty( hbmk[ _HBMK_hAUTOHBC ] )
            FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
               FindNewerHeaders( hbmk, tmp, NIL, .F., cBin_CompC )
            NEXT
         ENDIF

         l_aPRG_TO_DO := hbmk[ _HBMK_aPRG ]
      ENDIF

      IF ! Empty( hbmk[ _HBMK_hAUTOHBCFOUND ] )
         FOR EACH cParam IN hbmk[ _HBMK_hAUTOHBCFOUND ]

            IF ! Empty( cParam )
               IF Left( cParam:__enumKey(), 1 ) == "."
                  _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Triggered by #require directive: %1$s" ), cParam ) )
               ELSE
                  _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Triggered by '%1$s' header: %2$s" ), cParam:__enumKey(), cParam ) )
               ENDIF
               IF Empty( HBC_Find( hbmk, cParam ) )
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot find %1$s" ), cParam ) )
               ENDIF
            ENDIF
         NEXT

         convert_incpaths_to_options( hbmk, cOptIncMask, lCHD_Comp )
      ENDIF
   ELSE
      l_aPRG_TO_DO := hbmk[ _HBMK_aPRG ]
   ENDIF

   /* Delete all lib paths which contain late-evaluation macros. */
   FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ] DESCEND
      IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ tmp
         hb_ADel( hbmk[ _HBMK_aLIBPATH ], tmp:__enumIndex(), .T. )
      ENDIF
   NEXT

   /* Dump build information */

   IF hbmk[ _HBMK_lDumpInfo ]

      IF ! lDumpInfoNested .AND. nLevel > 1
         RETURN _EXIT_OK
      ENDIF

      tmp := { ;
         "platform"   => hbmk[ _HBMK_cPLAT ], ;
         "compiler"   => hbmk[ _HBMK_cCOMP ], ;
         "cpu"        => hbmk[ _HBMK_cCPU ], ;
         "buildname"  => hbmk[ _HBMK_cBUILD ], ;
         "targetname" => hbmk_TARGETNAME( hbmk ), ;
         "targettype" => hbmk_TARGETTYPE( hbmk ), ;
         "dynprefix"  => iif( Empty( l_cDynLibDir ), "", l_cDynLibDir + hbmk[ _HBMK_cDynLibPrefix ] ), ;
         "dynsuffix"  => hbmk_DYNSUFFIX( hbmk ), ;
         "inc"        => iif( hbmk[ _HBMK_lINC ], "yes", "no" ) }

      IF ! Empty( hbmk[ _HBMK_cPROGNAME ] )
         tmp[ "outputname" ] := PathSepToForward( hbmk[ _HBMK_cPROGNAME ] )
      ENDIF

      tmp[ "hbctree" ] := ""
      FOR EACH tmp1 IN hbmk[ _HBMK_aDEPTHBC ]
         tmp[ "hbctree" ] += Replicate( Chr( 9 ), tmp1[ 2 ] ) + PathSepToForward( hb_PathNormalize( tmp1[ 1 ] ) ) + Chr( 10 )
      NEXT

      OutStd( hb_jsonEncode( tmp ) + Chr( 10 ) )

      RETURN _EXIT_OK
   ENDIF

   /* Check if we've found all dependencies */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]
      IF ! dep_evaluate( hbmk )
         IF hbmk[ _HBMK_lBEEP ]
            DoBeep( .F. )
         ENDIF
         RETURN _EXIT_MISSDEPT
      ENDIF
   ENDIF

   /* Creating implibs requested in dependency specification */

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. HB_ISBLOCK( bBlk_ImpLib )
      FOR EACH tmp IN hbmk[ _HBMK_hDEP ]
         IF tmp[ _HBMKDEP_lFound ] .AND. ! Empty( tmp[ _HBMKDEP_aIMPLIBSRC ] )
            DoIMPLIB( hbmk, bBlk_ImpLib, cLibLibPrefix, cLibLibExt, tmp[ _HBMKDEP_aIMPLIBSRC ], tmp[ _HBMKDEP_cIMPLIBDST ], "depimplib", ! hbmk[ _HBMK_lDEPIMPLIB ] )
         ENDIF
      NEXT
   ENDIF

   /* Harbour compilation */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. Empty( l_aPRG_TO_DO ) .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. hbmk[ _HBMK_lINC ] .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C .AND. ;
      hbmk[ _HBMK_lCreateHRB ] .AND. hbmk[ _HBMK_lStopAfterHarbour ]
      _hbmk_OutStd( hbmk, I_( "Target(s) up to date." ) )
   ELSEIF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. Len( l_aPRG_TO_DO ) > 0 .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, I_( "Compiling Harbour sources..." ) )
      ENDIF

      IF ! Empty( hbmk[ _HBMK_cPO ] )
         AAdd( hbmk[ _HBMK_aOPTPRG ], "-j" )
      ENDIF
      IF hbmk[ _HBMK_nHEAD ] == _HEAD_DEP
         AAdd( hbmk[ _HBMK_aOPTPRG ], "-gd" )
      ENDIF
      FOR EACH tmp IN hbmk[ _HBMK_aCH ]
         AAdd( hbmk[ _HBMK_aOPTPRG ], "-u+" + tmp )
      NEXT

      PlatformPRGFlags( hbmk, hbmk[ _HBMK_aOPTPRG ] )

      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE

         /* Use integrated compiler */

         aThreads := {}
         FOR EACH aTO_DO IN ArraySplit( l_aPRG_TO_DO, l_nJOBS )

            aCommand := ArrayAJoin( { ;
               { iif( hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ], "-n1", "-n2" ) }, ;
               aTO_DO, ;
               iif( hbmk[ _HBMK_lBLDFLGP ], { hb_Version( HB_VERSION_FLAG_PRG ) }, {} ), ;
               ListToArray( iif( Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), "", " " + GetEnv( "HB_USER_PRGFLAGS" ) ) ), ;
               hbmk[ _HBMK_aOPTPRG ] } )

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  IF Len( aTO_DO:__enumBase() ) > 1
                     _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Harbour compiler command (embedded) job #%1$d:" ), aTO_DO:__enumIndex() ) )
                  ELSE
                     _hbmk_OutStd( hbmk, I_( "Harbour compiler command (embedded):" ) )
                  ENDIF
               ENDIF
               OutStd( ;
                  "(" + FNameEscape( hb_DirSepAdd( hb_DirBase() ) + cBin_CompPRG + cBinExt, hbmk[ _HBMK_nCmd_Esc ] ) + ")" + ;
                  " " + ArrayToList( aCommand ) + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ]
               IF hb_mtvm() .AND. Len( aTO_DO:__enumBase() ) > 1
                  AAdd( aThreads, { hb_threadStart( @hbmk_hb_compile(), hbmk, "harbour", aCommand ), aCommand } )
               ELSE
                  IF ( tmp := hbmk_hb_compile( hbmk, "harbour", aCommand ) ) != 0
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler (embedded). %1$d" ), tmp ) )
                     IF ! hbmk[ _HBMK_lQuiet ]
                        OutErr( ;
                           "(" + FNameEscape( hb_DirSepAdd( hb_DirBase() ) + cBin_CompPRG + cBinExt, hbmk[ _HBMK_nCmd_Esc ] ) + ")" + ;
                           " " + ArrayToList( aCommand ) + _OUT_EOL )
                     ENDIF
                     IF ! hbmk[ _HBMK_lIGNOREERROR ]
                        IF lDeleteWorkDir
                           hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                        ENDIF
                        IF hbmk[ _HBMK_lBEEP ]
                           DoBeep( .F. )
                        ENDIF
                        RETURN _EXIT_COMPPRG
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         NEXT

         IF hb_mtvm() .AND. Len( aThreads ) > 1
            FOR EACH thread IN aThreads
               hb_threadJoin( thread[ 1 ], @tmp )
               IF tmp != 0
                  IF Len( aThreads ) > 1
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler job #%1$d. %2$d" ), thread:__enumIndex(), tmp ) )
                  ELSE
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler. %1$d" ), tmp ) )
                  ENDIF
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( ArrayToList( thread[ 2 ] ) + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     IF lDeleteWorkDir
                        hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                     ENDIF
                     IF hbmk[ _HBMK_lBEEP ]
                        DoBeep( .F. )
                     ENDIF
                     RETURN _EXIT_COMPPRG
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ELSE
         /* Use external compiler */

         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            tmp1 := l_aPRG_TO_DO /* Call compiler for each source file to avoid compiler bugs. */
         ELSE
            tmp1 := { ArrayToList( l_aPRG_TO_DO,, hbmk[ _HBMK_nCmd_Esc ] ) }
         ENDIF

         FOR EACH tmp IN tmp1

            cCommand := ;
               FNameEscape( hb_DirSepAdd( hb_DirSepToOS( hbmk[ _HBMK_cHB_INSTALL_BIN ] ) ) + cBin_CompPRG + cBinExt, hbmk[ _HBMK_nCmd_Esc ] ) + ;
               " " + iif( hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ], "-n1", iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_NATIVE, "-n", "-n2" ) ) + ;
               " " + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), FNameEscape( tmp, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ), tmp ) + ;
               iif( hbmk[ _HBMK_lBLDFLGP ], " " + hb_Version( HB_VERSION_FLAG_PRG ), "" ) + ;
               iif( Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), "", " " + GetEnv( "HB_USER_PRGFLAGS" ) ) + ;
               iif( Empty( hbmk[ _HBMK_aOPTPRG ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTPRG ] ) )

            cCommand := AllTrim( cCommand )

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  _hbmk_OutStd( hbmk, I_( "Harbour compiler command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hb_processRun( cCommand ) ) != 0
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler. %1$d" ), tmp1 ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + _OUT_EOL )
               ENDIF
               IF ! hbmk[ _HBMK_lIGNOREERROR ]
                  IF lDeleteWorkDir
                     hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                  ENDIF
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _EXIT_COMPPRG
               ENDIF
            ENDIF
         NEXT
      ENDIF
   ENDIF

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]

      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

         /* Do entry function detection on platform required and supported */
         IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ! lStopAfterCComp .AND. l_cMAIN == NIL
            tmp := iif( HBMK_IS_IN( Lower( hb_FNameExt( hbmk[ _HBMK_cFIRST ] ) ), ".prg|.hb|.clp" ) .OR. Empty( hb_FNameExt( hbmk[ _HBMK_cFIRST ] ) ), FNameDirExtSet( hbmk[ _HBMK_cFIRST ], hbmk[ _HBMK_cWorkDir ], ".c" ), hbmk[ _HBMK_cFIRST ] )
            IF ! Empty( tmp := getFirstFunc( hbmk, tmp ) )
               l_cMAIN := tmp
            ENDIF
         ENDIF

         lHBMAINDLLP := lStopAfterCComp .AND. hbmk[ _HBMK_lCreateDyn ] .AND. ;
                        AScan( hbmk[ _HBMK_aLIBUSER ], {| tmp | hb_FileMatch( tmp, "hbmaindllp" ) } ) > 0

         /* HACK: Override entry point requested by user or detected by us,
                  and override the GT if requested by user. */
         IF ( ( ! lStopAfterCComp .OR. hbmk[ _HBMK_lDynVM ] ) .AND. ;
              ( l_cMAIN != NIL .OR. ;
                ! Empty( hbmk[ _HBMK_aREQUEST ] ) .OR. ;
                ! Empty( hbmk[ _HBMK_aGT ] ) .OR. ;
                hbmk[ _HBMK_cGT ] != NIL .OR. ;
                l_cCMAIN != NIL ) ) .OR. lHBMAINDLLP

#if defined( __PLATFORM__DOS )
            l_cCSTUB := hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkaut.c"
#else
            l_cCSTUB := hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkaut_" + hb_FNameName( hbmk[ _HBMK_cFIRST ] ) + ".c"
#endif

            IF ! hbmk[ _HBMK_lCLEAN ]

               /* NOTE: This has to be kept synced with Harbour HB_IMPORT values. */
               DO CASE
               CASE ! hbmk[ _HBMK_lSHARED ] .OR. ;
                    ! HBMK_ISPLAT( "win|wce" ) .OR. ;
                    HBMK_ISCOMP( "msvc|msvc64|msvcia64|icc|iccia64" )

                  /* NOTE: MSVC gives the warning:
                           "LNK4217: locally defined symbol ... imported in function ..."
                           if using 'dllimport'. [vszakats] */
                  tmp := ""
               CASE HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|clang" )
                  tmp := "__attribute__ (( dllimport ))"
               CASE HBMK_ISCOMP( "bcc|bcc64|watcom" )
                  tmp := "__declspec( dllimport )"
               OTHERWISE
                  tmp := "_declspec( dllimport )"
               ENDCASE

               /* Create list of requested symbols */
               array := {}
               IF ! lHBMAINDLLP
                  IF l_cMAIN != NIL
                     /* NOTE: Request this function to generate link error, rather
                              than starting with the wrong (default) function. */
                     AAdd( array, Upper( iif( Left( l_cMAIN, 1 ) == "@", SubStr( l_cMAIN, 2 ), l_cMAIN ) ) )
                  ENDIF
                  IF hbmk[ _HBMK_cGT ] != NIL
                     /* Always request default GT first */
                     AAdd( array, "HB_GT_" + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) )
                  ENDIF
                  FOR EACH tmp IN hbmk[ _HBMK_aGT ]
                     IF hbmk[ _HBMK_cGT ] == NIL .OR. !( Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) == Upper( SubStr( tmp, 3 ) ) )
                        AAdd( array, "HB_GT_" + Upper( SubStr( tmp, 3 ) ) )
                     ENDIF
                  NEXT
               ENDIF
               AEval( hbmk[ _HBMK_aREQUEST ], {| tmp | AAdd( array, tmp ) } )

               /* Build C stub */
               /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
               cFile := ;
                  "/* This temp source file was generated by " + _SELF_NAME_ + " tool. */"  + _FIL_EOL + ;
                  "/* You can safely delete it. */"                                         + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  '#include "hbapi.h"'                                                      + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL

               IF ! Empty( array ) .OR. ( l_cCMAIN != NIL .AND. ! lHBMAINDLLP )

                  AEval( array, {| tmp, i | array[ i ] := FuncNameEncode( tmp ) } )

                  AEval( array, {| tmp | cFile += "HB_FUNC_EXTERN( " + tmp + " );"                + _FIL_EOL } )

                  IF l_cCMAIN != NIL .AND. ! lHBMAINDLLP
                     IF ! Empty( array )
                        cFile += ""                                                               + _FIL_EOL
                     ENDIF
                     cFile += "HB_EXTERN_BEGIN"                                                   + _FIL_EOL + ;
                              "void " + l_cCMAIN + "( void );"                                    + _FIL_EOL + ;
                              "HB_EXTERN_END"                                                     + _FIL_EOL
                  ENDIF
                  cFile += ""                                                                     + _FIL_EOL
                  cFile += "void _hb_lnk_ForceLink_hbmk( void )"                                  + _FIL_EOL
                  cFile += "{"                                                                    + _FIL_EOL
                  AEval( array, {| tmp | cFile += "   HB_FUNC_EXEC( " + tmp + " );"               + _FIL_EOL } )
                  IF l_cCMAIN != NIL .AND. ! lHBMAINDLLP
                     IF ! Empty( array )
                        cFile += ""                                                               + _FIL_EOL
                     ENDIF
                     cFile += "   " + l_cCMAIN + "();"                                            + _FIL_EOL
                  ENDIF
                  cFile += "}"                                                                    + _FIL_EOL
                  cFile += ""                                                                     + _FIL_EOL
               ENDIF

               IF lHBMAINDLLP .AND. .F.
                  cFile += ;
                     "HB_EXPORT_ATTR PHB_FUNC dll_hb_vmProcAddress( const char * szFuncName )" + _FIL_EOL + ;
                     "{"                                                                       + _FIL_EOL + ;
                     "   return hb_vmProcAddress( szFuncName );"                               + _FIL_EOL + ;
                     "}"                                                                       + _FIL_EOL + ;
                     ""                                                                        + _FIL_EOL
               ENDIF

               IF hbmk[ _HBMK_cGT ] != NIL .OR. ;
                  l_cMAIN != NIL
                  IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                     _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
                     cFile += ;
                        '#include "hbinit.h"'                                                  + _FIL_EOL + ;
                        ""                                                                     + _FIL_EOL + ;
                        "HB_EXTERN_BEGIN"                                                      + _FIL_EOL + ;
                        "extern " + tmp + " const char * s_defaultGT;"                         + _FIL_EOL + ;
                        "extern " + tmp + " const char * s_pszLinkedMain;"                     + _FIL_EOL + ;
                        "HB_EXTERN_END"                                                        + _FIL_EOL + ;
                        ""                                                                     + _FIL_EOL + ;
                        "HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )"                         + _FIL_EOL
                  ELSE
                     cFile += ;
                        '#include "hbinit.h"'                                                  + _FIL_EOL + ;
                        ""                                                                     + _FIL_EOL + ;
                        "HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )"                         + _FIL_EOL
                  ENDIF
                  IF hbmk[ _HBMK_cGT ] != NIL
                     IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                        _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
                        cFile += '   s_defaultGT = "' + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) + '";'           + _FIL_EOL
                     ELSE
                        cFile += '   hb_vmSetDefaultGT( "' + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) + '" );'    + _FIL_EOL
                     ENDIF
                  ENDIF
                  IF l_cMAIN != NIL
                     IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                        _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
                        cFile += '   s_pszLinkedMain = "' + Upper( l_cMAIN ) + '";'                  + _FIL_EOL
                     ELSE
                        cFile += '   hb_vmSetLinkedMain( "' + Upper( l_cMAIN ) + '" );'              + _FIL_EOL
                     ENDIF
                  ENDIF
                  cFile += ;
                     "HB_CALL_ON_STARTUP_END( _hb_hbmk_setdef_ )"                           + _FIL_EOL + ;
                     ""                                                                     + _FIL_EOL + ;
                     "#if defined( HB_PRAGMA_STARTUP )"                                     + _FIL_EOL + ;
                     "   #pragma startup _hb_hbmk_setdef_"                                  + _FIL_EOL + ;
                     "#elif defined( HB_DATASEG_STARTUP )"                                  + _FIL_EOL + ;
                     "   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_hbmk_setdef_ )"    + _FIL_EOL + ;
                     '   #include "hbiniseg.h"'                                             + _FIL_EOL + ;
                     "#endif"                                                               + _FIL_EOL
               ENDIF

               IF hbmk[ _HBMK_lINC ]
                  IF hbmk[ _HBMK_lREBUILD ] .OR. !( hb_MemoRead( l_cCSTUB ) == cFile )
                     fhnd := FCreate( l_cCSTUB )
                  ELSE
                     fhnd := NIL
                  ENDIF
               ELSE
                  fhnd := hb_FTempCreateEx( @l_cCSTUB, NIL, "hbmk_", ".c" )
               ENDIF
               IF fhnd == NIL
                  AAdd( hbmk[ _HBMK_aC ], l_cCSTUB )
               ELSEIF fhnd != F_ERROR
                  FWrite( fhnd, cFile )
                  FClose( fhnd )

                  IF hbmk[ _HBMK_lDEBUGSTUB ]
                     OutStd( "C stub dump:" + _OUT_EOL )
                     OutStd( cFile )
                  ENDIF
                  AAdd( hbmk[ _HBMK_aC ], l_cCSTUB )
                  AAdd( l_aC_TO_DO, l_cCSTUB )
               ELSE
                  _hbmk_OutErr( hbmk, I_( "Warning: Stub helper .c program could not be created." ) )
                  IF ! hbmk[ _HBMK_lINC ]
                     AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".c", .T. ), {| tmp | FErase( tmp ) } )
                  ENDIF
                  IF lDeleteWorkDir
                     hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                  ENDIF
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _EXIT_STUBCREATE
               ENDIF
               /* Do not delete stub in workdir in incremental mode. */
               IF hbmk[ _HBMK_lINC ]
                  l_cCSTUB := NIL
               ENDIF
            ENDIF
         ENDIF

         /* HACK: Override memory allocation functions for apps that request it. */
         IF ! lStopAfterCComp .AND. ;
            ! Empty( cBin_CompCPP ) .AND. ;
            hbmk[ _HBMK_lHBCPPMM ]

#if defined( __PLATFORM__DOS )
            l_cCPPSTUB := hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkcpp.cpp"
#else
            l_cCPPSTUB := hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkcpp_" + hb_FNameName( hbmk[ _HBMK_cFIRST ] ) + ".cpp"
#endif

            IF ! hbmk[ _HBMK_lCLEAN ]

               /* Build C++ stub */
               /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
               cFile := ;
                  "/* This temp source file was generated by " + _SELF_NAME_ + " tool. */"  + _FIL_EOL + ;
                  "/* You can safely delete it. */"                                         + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  '#include "hbapi.h"'                                                      + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "#if defined( __cplusplus )"                                              + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "const char * __hbmk_hbcppmm( void )"                                     + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  '   return "HBCPPMM";'                                                    + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "void * operator new[]( size_t nSize )"                                   + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  "   if( nSize == 0 )"                                                     + _FIL_EOL + ;
                  "      nSize = 1;"                                                        + _FIL_EOL + ;
                  "   return hb_xgrab( nSize );"                                            + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "void * operator new( size_t nSize )"                                     + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  "   if( nSize == 0 )"                                                     + _FIL_EOL + ;
                  "      nSize = 1;"                                                        + _FIL_EOL + ;
                  "   return hb_xgrab( nSize );"                                            + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "void operator delete[]( void * ptr )"                                    + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  "   if( ptr )"                                                            + _FIL_EOL + ;
                  "      hb_xfree( ptr );"                                                  + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "void operator delete[]( void * ptr, size_t )"                            + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  "   if( ptr )"                                                            + _FIL_EOL + ;
                  "      hb_xfree( ptr );"                                                  + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "void operator delete( void * ptr )"                                      + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  "   if( ptr )"                                                            + _FIL_EOL + ;
                  "      hb_xfree( ptr );"                                                  + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "void operator delete( void * ptr, size_t )"                              + _FIL_EOL + ;
                  "{"                                                                       + _FIL_EOL + ;
                  "   if( ptr )"                                                            + _FIL_EOL + ;
                  "      hb_xfree( ptr );"                                                  + _FIL_EOL + ;
                  "}"                                                                       + _FIL_EOL + ;
                  ""                                                                        + _FIL_EOL + ;
                  "#endif"                                                                  + _FIL_EOL

               IF hbmk[ _HBMK_lINC ]
                  IF hbmk[ _HBMK_lREBUILD ] .OR. !( hb_MemoRead( l_cCPPSTUB ) == cFile )
                     fhnd := FCreate( l_cCPPSTUB )
                  ELSE
                     fhnd := NIL
                  ENDIF
               ELSE
                  fhnd := hb_FTempCreateEx( @l_cCPPSTUB, NIL, "hbmk_", ".cpp" )
               ENDIF
               IF fhnd == NIL
                  AAdd( hbmk[ _HBMK_aCPP ], l_cCPPSTUB )
               ELSEIF fhnd != F_ERROR
                  FWrite( fhnd, cFile )
                  FClose( fhnd )

                  IF hbmk[ _HBMK_lDEBUGSTUB ]
                     OutStd( "C++ stub dump:" + _OUT_EOL )
                     OutStd( cFile )
                  ENDIF
                  AAdd( hbmk[ _HBMK_aCPP ], l_cCPPSTUB )
                  AAdd( l_aCPP_TO_DO, l_cCPPSTUB )
               ELSE
                  _hbmk_OutErr( hbmk, I_( "Warning: Stub helper .cpp program could not be created." ) )
                  IF ! hbmk[ _HBMK_lINC ]
                     AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".c", .T. ), {| tmp | FErase( tmp ) } )
                  ENDIF
                  IF lDeleteWorkDir
                     hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                  ENDIF
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _EXIT_STUBCREATE
               ENDIF
               /* Do not delete stub in workdir in incremental mode. */
               IF hbmk[ _HBMK_lINC ]
                  l_cCPPSTUB := NIL
               ENDIF
            ENDIF
         ENDIF

         IF ! hbmk[ _HBMK_lSHARED ]
            IF ! Empty( cLIB_BASE_PCRE ) .AND. hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_PCRE ) )
               AAdd( l_aLIBSYS, cLIB_BASE_PCRE )
            ENDIF
            IF ! Empty( cLIB_BASE_ZLIB ) .AND. hb_FileExists( _HBLIB_FULLPATH( cLIB_BASE_ZLIB ) )
               AAdd( l_aLIBSYS, cLIB_BASE_ZLIB )
            ENDIF
         ENDIF

         /* Library list assembly */
         IF hbmk[ _HBMK_lSHARED ] .AND. ! Empty( l_aLIBSHARED )
            /* Do not link Harbour dynamic/static libs when in '-hbdyn -shared' mode */
            IF !( hbmk[ _HBMK_lCreateDyn ] .AND. ! hbmk[ _HBMK_lDynVM ] )
               l_aLIBHB := AClone( l_aLIBSHAREDPOST )
               /* NOTE: Make sure to add these static libs only if they can be found.
                        This will ensure that we can build shared mode binaries
                        even when static libs are not installed (typically on *nix systems).
                        [vszakats] */
               FOR EACH tmp IN ArrayAJoin( { aLIB_BASE_CPLR, ;
                                             aLIB_BASE_DEBUG } )
                  IF hb_FileExists( _HBLIB_FULLPATH( tmp ) )
                     AAdd( l_aLIBHB, tmp )
                  ENDIF
               NEXT
            ELSE
               l_aLIBHB := {}
            ENDIF
         ELSE
            l_aLIBHB := ArrayAJoin( { ;
               aLIB_BASE_EXTERN, ;
               aLIB_BASE_DEBUG, ;
               iif( hbmk[ _HBMK_lMT ], aLIB_BASE_1_MT, aLIB_BASE_1 ), ;
               hbmk[ _HBMK_aLIBCOREGT ], ;
               iif( hbmk[ _HBMK_lNULRDD ], aLIB_BASE_NULRDD, iif( hbmk[ _HBMK_lMT ], aLIB_BASE_RDD_MT, aLIB_BASE_RDD ) ), ;
               l_aLIBHBBASE_2, ;
               iif( hbmk[ _HBMK_lMT ], aLIB_BASE_3_MT, aLIB_BASE_3 ), ;
               l_aLIBSTATICPOST } )
         ENDIF
      ELSE
         lHBMAINDLLP := .F.
         l_aLIBHB := {}
         l_aLIBSHARED := {}
         hbmk[ _HBMK_aPRG ] := {}
      ENDIF

      /* NOTE: Temporary trick to remove our own implib output name and
               lib output name from lib list.
               This is to avoid adding self-reference when building a
               -hbdyn or -hblib and including both project .hbp + .hbc.
               The downside is that one cannot have a lib dependency
               with the same name as the output.
               [vszakats] */
      IF l_cLIBSELF != NIL
         tmp1 := FNameNameGetNoExt( l_cLIBSELF )
         FOR EACH tmp IN hbmk[ _HBMK_aLIBUSER ] DESCEND
            IF hb_FileMatch( hb_FNameName( tmp ), tmp1 )
               hb_ADel( hbmk[ _HBMK_aLIBUSER ], tmp:__enumIndex(), .T. )
            ENDIF
         NEXT
      ENDIF

      /* Handle filter list of libs */
      FOR EACH tmp2 IN hbmk[ _HBMK_aLIBFILTEROUT ]
         FOR EACH tmp1 IN { hbmk[ _HBMK_aLIBUSER ], l_aLIB3RD, hbmk[ _HBMK_aLIBUSERSYSPRE ], l_aLIBSYS, hbmk[ _HBMK_aLIBUSERSYS ] }
            FOR EACH tmp IN tmp1 DESCEND
               IF hb_FileMatch( tmp, tmp2 )
                  hb_ADel( tmp1, tmp:__enumIndex(), .T. )
               ENDIF
            NEXT
         NEXT
      NEXT

      /* Process build-time configuration */

      /* TOFIX: This does not work well when doing cross-platform
                build f.e. on a 32-bit *nix system to 64-bit target
                where the 64-bit target does not happen to provide
                64-bit flavor of gpm lib. This is the case when
                building 64-bit target on a 32-bit Ubuntu 10.10
                system. Moral of the story: we should decide about
                gpm using dynamic information instead of using
                build-time default HB_HAS_GPM value.
                [vszakats] */
      #if defined( HB_HAS_GPM )
         IF hbmk[ _HBMK_cPLAT ] == "linux"
            FOR EACH tmp IN l_aLIBHB
               IF tmp == "gtcrs" .OR. ;
                  tmp == "gtsln" .OR. ;
                  tmp == "gttrm"
                  AAdd( hbmk[ _HBMK_aLIBUSERSYS ], "gpm" )
                  EXIT
               ENDIF
            NEXT
         ENDIF
      #endif

      /* Merge lib lists. */
      l_aLIBRAW := ArrayAJoin( { hbmk[ _HBMK_aLIBUSER ], l_aLIBHB, l_aLIB3RD, hbmk[ _HBMK_aLIBUSERSYSPRE ], l_aLIBSYS, hbmk[ _HBMK_aLIBUSERSYS ] } )
      /* Dress lib names. */
      l_aLIB := {}
      l_aLIBA := {}
      ListCookLib( hbmk, l_aLIB, l_aLIBA, l_aLIBRAW, NIL, cLibExt )
      IF hbmk[ _HBMK_lSHARED ] .AND. ! Empty( l_aLIBSHARED )
         /* Do not link Harbour dynamic/static libs when in '-hbdyn -shared' mode */
         IF !( hbmk[ _HBMK_lCreateDyn ] .AND. ! hbmk[ _HBMK_lDynVM ] ) .OR. lHBMAINDLLP
            l_aLIBRAW := ArrayJoin( l_aLIBSHARED, l_aLIBRAW )
            ListCookLib( hbmk, l_aLIB, l_aLIBA, l_aLIBSHARED, NIL )
         ENDIF
      ENDIF
      /* Dress obj names. */
      IF cObjExt == NIL
         /* NOTE: May only happen if the plat/comp combination is not supported.
                  Do not let the obj filelist be the exact same as the source list,
                  it would cause unwanted deletion of source at cleanup stage.
                  [vszakats] */
         l_aOBJ := {}
      ELSE
         l_aOBJ := ListDirExt( ArrayAJoin( { hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_aC ], hbmk[ _HBMK_aCPP ] } ), hbmk[ _HBMK_cWorkDir ], cObjExt, .T. )
      ENDIF
      hbmk[ _HBMK_aOBJUSER ] := ListCook( hbmk[ _HBMK_aOBJUSER ], cObjExt )

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ] .AND. ! hbmk[ _HBMK_lCLEAN ]
         l_aRESSRC_TO_DO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aRESSRC ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: RESSRC %1$s %2$s", tmp, FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .T., cBin_CompC ) ) .OR. ;
               hb_FSize( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ) ) == 0
               AAdd( l_aRESSRC_TO_DO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aRESSRC_TO_DO := AClone( hbmk[ _HBMK_aRESSRC ] )
      ENDIF

      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C .AND. ! hbmk[ _HBMK_lCLEAN ]
         IF hbmk[ _HBMK_lREBUILDPO ]
            IF ! Empty( hbmk[ _HBMK_cPO ] ) .AND. ! Empty( hbmk[ _HBMK_aPRG ] )
               RebuildPO( hbmk, ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".pot", .T. ) )
            ENDIF
         ELSE
            IF ! Empty( hbmk[ _HBMK_cPO ] ) .AND. Len( l_aPRG_TO_DO ) > 0
               UpdatePO( hbmk, ListDirExt( l_aPRG_TO_DO, hbmk[ _HBMK_cWorkDir ], ".pot", .T. ) )
            ENDIF
         ENDIF

         IF Len( hbmk[ _HBMK_aPO ] ) > 0 .AND. hbmk[ _HBMK_cHBL ] != NIL

            /* Combine target dir with .hbl output name. */

            hb_FNameSplit( hbmk[ _HBMK_cPROGNAME ], @tmp )
            IF Empty( tmp )
               hbmk[ _HBMK_cHBL ] := PathMakeAbsolute( hbmk[ _HBMK_cHBL ], hbmk[ _HBMK_cHBLDir ] )
            ELSE
               hbmk[ _HBMK_cHBL ] := PathMakeAbsolute( hbmk[ _HBMK_cHBL ], tmp )
            ENDIF

            MakeHBL( hbmk, hbmk[ _HBMK_cHBL ] )
         ENDIF
      ENDIF

      IF ( HBMK_ISPLAT( "win|wce|os2" ) .AND. ! Empty( hbmk[ _HBMK_aICON ] ) ) .OR. ;
         ( HBMK_ISPLAT( "win|wce" ) .AND. ! Empty( hbmk[ _HBMK_cMANIFEST ] ) )

#if defined( __PLATFORM__DOS )
         l_cRESSTUB := hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkaut.rc"
#else
         l_cRESSTUB := hb_DirSepAdd( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkaut_" + hb_FNameName( hbmk[ _HBMK_cFIRST ] ) + ".rc"
#endif

         IF ! hbmk[ _HBMK_lCLEAN ]
            /* Build .rc stub */
            /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
            cFile := ;
               "/* This temp source file was generated by " + _SELF_NAME_ + " tool. */"  + _FIL_EOL + ;
               "/* You can safely delete it. */"                                         + _FIL_EOL + ;
               ""                                                                        + _FIL_EOL
            IF ! Empty( hbmk[ _HBMK_cMANIFEST ] )
               cFile += ;
                  "#include <winuser.h>" + _FIL_EOL + ;
                  "#ifndef RT_MANIFEST" + _FIL_EOL + ;
                  "#define RT_MANIFEST 24" + _FIL_EOL + ;
                  "#endif" + _FIL_EOL + ;
                  "#ifndef CREATEPROCESS_MANIFEST_RESOURCE_ID" + _FIL_EOL + ;
                  "#define CREATEPROCESS_MANIFEST_RESOURCE_ID 1" + _FIL_EOL + ;
                  "#endif" + _FIL_EOL + ;
                  'CREATEPROCESS_MANIFEST_RESOURCE_ID RT_MANIFEST "' + PathSepToForward( hbmk[ _HBMK_cMANIFEST ] ) + '"' + _FIL_EOL
            ENDIF
            IF ! Empty( hbmk[ _HBMK_aICON ] ) .AND. ! HBMK_ISCOMP( "bcc|bcc64" ) /* BCC cannot handle certain new .ico files */
               IF hbmk[ _HBMK_cPLAT ] == "os2"
                  AEval( hbmk[ _HBMK_aICON ], {| tmp, tmp1 | cFile += "ICON " + hb_ntos( tmp1 ) + ' DISCARDABLE "' + PathSepToForward( tmp ) + '"' + _FIL_EOL } )
               ELSE
                  AEval( hbmk[ _HBMK_aICON ], {| tmp, tmp1 | cFile += hb_ntos( tmp1 ) + ' ICON DISCARDABLE "' + PathSepToForward( tmp ) + '"' + _FIL_EOL } )
               ENDIF
            ENDIF

            IF hbmk[ _HBMK_lINC ]
               IF hbmk[ _HBMK_lREBUILD ] .OR. !( hb_MemoRead( l_cRESSTUB ) == cFile )
                  fhnd := FCreate( l_cRESSTUB )
               ELSE
                  fhnd := NIL
               ENDIF
            ELSE
               fhnd := hb_FTempCreateEx( @l_cRESSTUB, NIL, "hbmk_", ".rc" )
            ENDIF
            IF fhnd == NIL
               hb_AIns( hbmk[ _HBMK_aRESSRC ], 1, l_cRESSTUB, .T. )
            ELSEIF fhnd != F_ERROR
               FWrite( fhnd, cFile )
               FClose( fhnd )

               IF hbmk[ _HBMK_lDEBUGSTUB ]
                  OutStd( ".rc stub dump:" + _OUT_EOL )
                  OutStd( cFile )
               ENDIF
               hb_AIns( hbmk[ _HBMK_aRESSRC ], 1, l_cRESSTUB, .T. )
               AAdd( l_aRESSRC_TO_DO, l_cRESSTUB )
            ELSE
               _hbmk_OutErr( hbmk, I_( "Warning: Stub helper .rc file could not be created." ) )
            ENDIF
            /* Do not delete stub in workdir in incremental mode. */
            IF hbmk[ _HBMK_lINC ]
               l_cRESSTUB := NIL
            ENDIF
         ENDIF
      ENDIF

      /* Avoid this list being added at link phase if resource compiling is not available
         in target compiler. */
      IF Empty( cBin_Res )
         ASize( hbmk[ _HBMK_aRESSRC ], 0 )
      ENDIF

      IF Len( l_aRESSRC_TO_DO ) > 0 .AND. ! Empty( cBin_Res ) .AND. ! hbmk[ _HBMK_lCLEAN ]

         PlugIn_Execute_All( hbmk, "pre_res" )

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, I_( "Compiling resources..." ) )
         ENDIF

         /* Compiling resource */

         nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Res, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
         nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Res, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

         hReplace := { ;
            "{FR}" => GetEnv( "HB_USER_RESFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTRES ] ), ;
            "{DI}" => FNameEscape( hbmk[ _HBMK_cHB_INSTALL_INC ], nOpt_Esc, nOpt_FNF ) }

         IF "{IR}" $ cOpt_Res

            FOR EACH tmp IN l_aRESSRC_TO_DO

               hReplace[ "{IR}" ] := FNameEscape( tmp, nOpt_Esc, nOpt_FNF )
               hReplace[ "{OS}" ] := FNameEscape( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ), nOpt_Esc, nOpt_FNF )

               cCommand := cBin_Res + " " + AllTrim( hb_StrReplace( cOpt_Res, hReplace ) )

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     _hbmk_OutStd( hbmk, I_( "Resource compiler command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hb_processRun( cCommand ) ) != 0
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running resource compiler. %1$d" ), tmp1 ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nExitCode ] := _EXIT_RUNRES
                     EXIT
                  ENDIF
               ENDIF
            NEXT
         ELSE
            hReplace[ "{LR}" ] := ArrayToList( l_aRESSRC_TO_DO,, nOpt_Esc, nOpt_FNF )

            cOpt_Res := AllTrim( hb_StrReplace( cOpt_Res, hReplace ) )

            /* Handle moving the whole command line to a script, if requested. */
            cScriptFile := NIL
            IF "{SCRIPT}" $ cOpt_Res
               fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
               IF fhnd != F_ERROR
                  FWrite( fhnd, StrTran( cOpt_Res, "{SCRIPT}" ) )
                  FClose( fhnd )
                  cOpt_Res := "@" + cScriptFile
               ELSE
                  _hbmk_OutErr( hbmk, I_( "Warning: Resource compiler script could not be created, continuing in command-line." ) )
               ENDIF
            ENDIF

            cCommand := cBin_Res + " " + cOpt_Res

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  _hbmk_OutStd( hbmk, I_( "Resource compiler command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
               IF ! Empty( cScriptFile )
                  _hbmk_OutStd( hbmk, I_( "Resource compiler script:" ) )
                  OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
               ENDIF
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running resource compiler. %1$d" ), tmp ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + _OUT_EOL )
               ENDIF
               IF ! hbmk[ _HBMK_lIGNOREERROR ]
                  hbmk[ _HBMK_nExitCode ] := _EXIT_RUNRES
               ENDIF
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF
         ENDIF
      ENDIF

      IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
            l_aPRG_TO_DO := {}

            FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
               IF LEFTEQUAL( tmp, "@" ) .AND. Lower( hb_FNameExt( tmp ) ) == ".clp"
                  tmp3 := SubStr( tmp, 2 )
               ELSE
                  tmp3 := tmp
               ENDIF
               tmp4 := FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], cObjExt )
               IF hbmk[ _HBMK_lDEBUGINC ]
                  _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CPRG %1$s %2$s", ;
                     FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], ".c" ), tmp4 ) )
               ENDIF
               IF ! hb_FGetDateTime( FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], ".c" ), @tmp1 ) .OR. ;
                  ! hb_FGetDateTime( tmp4, @tmp2 ) .OR. ;
                  tmp1 > tmp2 .OR. ;
                  hb_FSize( tmp4 ) == 0 .OR. ;
                  checkDepTime( hbmk, tmp4, tmp2 )
                  AAdd( l_aPRG_TO_DO, tmp )
               ENDIF
            NEXT
         ELSE
            l_aPRG_TO_DO := hbmk[ _HBMK_aPRG ]
         ENDIF
      ENDIF

      PlugIn_Execute_All( hbmk, "pre_c" )

      IF ! hbmk[ _HBMK_lCLEAN ]

         FOR EACH tmp3 IN { _CCOMP_PASS_C, _CCOMP_PASS_CPP }

            IF tmp3 == _CCOMP_PASS_C
               l_aCGEN_TO_DO := ArrayJoin( ListDirExt( l_aPRG_TO_DO, hbmk[ _HBMK_cWorkDir ], ".c", .T. ), l_aC_TO_DO )
               cBin_CompCGEN := cBin_CompC
            ELSE
               l_aCGEN_TO_DO := AClone( l_aCPP_TO_DO )
               cBin_CompCGEN := cBin_CompCPP
            ENDIF

            IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK .AND. Len( l_aCGEN_TO_DO ) > 0

               IF ! Empty( cBin_CompCGEN )

                  IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
                     IF tmp3 == _CCOMP_PASS_C
                        _hbmk_OutStd( hbmk, I_( "Compiling..." ) )
                     ELSE
                        _hbmk_OutStd( hbmk, I_( "Compiling C++..." ) )
                     ENDIF
                  ENDIF

                  /* Compiling */

                  nOpt_Esc := iif( "{SCRIPT}" $ cOpt_CompC, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
                  nOpt_FNF := iif( "{SCRIPT}" $ cOpt_CompC, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

                  /* Order is significant */
                  tmp4 := iif( tmp3 == _CCOMP_PASS_C .AND. ( hbmk[ _HBMK_lCPP ] == NIL .OR. ! hbmk[ _HBMK_lCPP ] ), hbmk[ _HBMK_aOPTCX ], hbmk[ _HBMK_aOPTCPPX ] )
                  cOpt_CompCPass := cOpt_CompC

                  /* TODO: eliminate recursive macros from hbmk[ _HBMK_aOPTC ] */
                  cOpt_CompCPass := StrTran( cOpt_CompCPass, "{FC}", ;
                     iif( hbmk[ _HBMK_lBLDFLGC ], hb_Version( HB_VERSION_FLAG_C ) + " ", "" ) + ;
                     GetEnv( "HB_USER_CFLAGS" ) + ;
                     iif( Empty( hbmk[ _HBMK_aOPTC ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTC ] ) ) + ;
                     iif( Empty( tmp4 ), "", " " + ArrayToList( tmp4 ) ) + ;
                     iif( Empty( hbmk[ _HBMK_aOPTCUSER ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTCUSER ] ) ) )

                  hReplace := { ;
                     "{OD}" => FNameEscape( hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ), nOpt_Esc, nOpt_FNF ), ;
                     "{DI}" => FNameEscape( hbmk[ _HBMK_cHB_INSTALL_INC ], nOpt_Esc, nOpt_FNF ) }

                  IF "{IC}" $ cOpt_CompCPass

                     aThreads := {}
                     FOR EACH aTO_DO IN ArraySplit( l_aCGEN_TO_DO, l_nJOBS )
                        IF hb_mtvm() .AND. Len( aTO_DO:__enumBase() ) > 1
                           AAdd( aThreads, hb_threadStart( @CompileCLoop(), hbmk, aTO_DO, cBin_CompCGEN, cOpt_CompCPass, hReplace, cObjExt, nOpt_Esc, nOpt_FNF, aTO_DO:__enumIndex(), Len( aTO_DO:__enumBase() ) ) )
                        ELSE
                           IF ! CompileCLoop( hbmk, aTO_DO, cBin_CompCGEN, cOpt_CompCPass, hReplace, cObjExt, nOpt_Esc, nOpt_FNF, 0, 0 )
                              IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                 hbmk[ _HBMK_nExitCode ] := _EXIT_COMPC
                                 EXIT
                              ENDIF
                           ENDIF
                        ENDIF
                     NEXT

                     IF hb_mtvm() .AND. Len( aThreads ) > 1
                        FOR EACH thread IN aThreads
                           hb_threadJoin( thread, @tmp )
                           IF ! tmp
                              IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                 hbmk[ _HBMK_nExitCode ] := _EXIT_COMPC
                              ENDIF
                           ENDIF
                        NEXT
                     ENDIF
                  ELSE
                     hReplace[ "{OO}" ] := FNameEscape( hb_FNameExtSet( hbmk[ _HBMK_cPROGNAME ], cObjExt ), nOpt_Esc, nOpt_FNF )
                     hReplace[ "{OW}" ] := FNameEscape( hbmk[ _HBMK_cWorkDir ], nOpt_Esc, nOpt_FNF )

                     IF lCHD_Comp
                        tmp2 := hb_DirSepAdd( hb_PathRelativize( hb_PathNormalize( PathMakeAbsolute( hbmk[ _HBMK_cWorkDir ], hb_cwd() ) ), hb_cwd(), .T. ) )
                        IF hbmk[ _HBMK_lDONTEXEC ]
                           cCHD_DirOld := NIL
                        ELSE
                           cCHD_DirOld := hb_cwd( hbmk[ _HBMK_cWorkDir ] )
                           IF hbmk[ _HBMK_lTRACE ] .AND. hbmk[ _HBMK_lInfo ]
                              _hbmk_OutStd( hbmk, hb_StrFormat( I_( "'cd' to: %1$s" ), hbmk[ _HBMK_cWorkDir ] ) )
                           ENDIF
                        ENDIF
                     ENDIF

                     aThreads := {}
                     FOR EACH aTO_DO IN ArraySplit( l_aCGEN_TO_DO, l_nJOBS )

                        IF lCHD_Comp
                           /* Convert source filenames relative to the target dir */
                           tmp := AClone( aTO_DO )
                           FOR EACH tmp1 IN tmp
                              tmp1 := hb_PathNormalize( PathMakeAbsolute( tmp1, tmp2 ) )
                           NEXT
                           hReplace[ "{LC}" ] := ArrayToList( tmp,, nOpt_Esc, nOpt_FNF )
                        ELSE
                           hReplace[ "{LC}" ] := ArrayToList( aTO_DO,, nOpt_Esc, nOpt_FNF )
                        ENDIF

                        cOpt_CompCLoop := AllTrim( hb_StrReplace( cOpt_CompCPass, hReplace ) )

                        /* Handle moving the whole command line to a script, if requested. */
                        cScriptFile := NIL
                        IF "{SCRIPT}" $ cOpt_CompCLoop
                           fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".cpl" )
                           IF fhnd != F_ERROR
                              FWrite( fhnd, StrTran( cOpt_CompCLoop, "{SCRIPT}" ) )
                              FClose( fhnd )
                              cOpt_CompCLoop := "@" + cScriptFile
                           ELSE
                              _hbmk_OutErr( hbmk, I_( "Warning: C/C++ compiler script could not be created, continuing in command-line." ) )
                           ENDIF
                        ENDIF

                        cCommand := cBin_CompCGEN + " " + cOpt_CompCLoop

                        IF hbmk[ _HBMK_lTRACE ]
                           IF ! hbmk[ _HBMK_lQuiet ]
                              IF Len( aTO_DO:__enumBase() ) > 1
                                 _hbmk_OutStd( hbmk, hb_StrFormat( I_( "C/C++ compiler command job #%1$d:" ), aTO_DO:__enumIndex() ) )
                              ELSE
                                 _hbmk_OutStd( hbmk, I_( "C/C++ compiler command:" ) )
                              ENDIF
                           ENDIF
                           OutStd( cCommand + _OUT_EOL )
                           IF ! Empty( cScriptFile )
                              _hbmk_OutStd( hbmk, I_( "C/C++ compiler script:" ) )
                              OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                           ENDIF
                        ENDIF

                        IF ! hbmk[ _HBMK_lDONTEXEC ]
                           IF hb_mtvm() .AND. Len( aTO_DO:__enumBase() ) > 1
                              AAdd( aThreads, { hb_threadStart( @hb_processRun(), cCommand ), cCommand } )
                           ELSE
                              IF ( tmp := hb_processRun( cCommand ) ) != 0
                                 _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler. %1$d" ), tmp ) )
                                 IF ! hbmk[ _HBMK_lQuiet ]
                                    OutErr( cCommand + _OUT_EOL )
                                 ENDIF
                                 IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                    hbmk[ _HBMK_nExitCode ] := _EXIT_COMPC
                                    EXIT
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF

                        IF ! Empty( cScriptFile )
                           FErase( cScriptFile )
                        ENDIF
                     NEXT

                     IF hb_mtvm() .AND. Len( aThreads ) > 1
                        FOR EACH thread IN aThreads
                           hb_threadJoin( thread[ 1 ], @tmp )
                           IF tmp != 0
                              IF Len( aThreads ) > 1
                                 _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler job #%1$d. %2$d" ), thread:__enumIndex(), tmp ) )
                              ELSE
                                 _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler. %1$d" ), tmp ) )
                              ENDIF
                              IF ! hbmk[ _HBMK_lQuiet ]
                                 OutErr( thread[ 2 ] + _OUT_EOL )
                              ENDIF
                              IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                 hbmk[ _HBMK_nExitCode ] := _EXIT_COMPC
                              ENDIF
                           ENDIF
                        NEXT
                     ENDIF

                     IF lCHD_Comp .AND. cCHD_DirOld != NIL
                        hb_cwd( cCHD_DirOld )
                        IF hbmk[ _HBMK_lTRACE ] .AND. hbmk[ _HBMK_lInfo ]
                           _hbmk_OutStd( hbmk, I_( "'cd' back." ) )
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  _hbmk_OutErr( hbmk, I_( "Error: C/C++ command is not implemented for this platform/compiler." ) )
                  hbmk[ _HBMK_nExitCode ] := _EXIT_UNSUPPORTED
               ENDIF
            ENDIF
         NEXT
      ENDIF

      IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK

         lTargetUpToDate := .F.

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]

            IF hbmk[ _HBMK_lDEBUGINC ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: target %1$s", hbmk[ _HBMK_cPROGNAME ] ) )
            ENDIF

            IF hb_FGetDateTime( hbmk[ _HBMK_cPROGNAME ], @tTarget )

               lTargetUpToDate := .T.
               IF lTargetUpToDate
                  FOR EACH tmp IN ArrayAJoin( { l_aOBJ, hbmk[ _HBMK_aOBJUSER ], l_aOBJA, ListDirExt( ArrayAJoin( { hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_aRESCMP ] } ), hbmk[ _HBMK_cWorkDir ], cResExt, .F. ) } )
                     IF hbmk[ _HBMK_lDEBUGINC ]
                        _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: EXEDEP %1$s", tmp ) )
                     ENDIF
                     IF ! hb_FGetDateTime( tmp, @tmp1 ) .OR. tmp1 > tTarget
                        lTargetUpToDate := .F.
                        EXIT
                     ENDIF
                  NEXT
               ENDIF
               /* We need a way to find and pick libraries according to linker rules. */
               IF lTargetUpToDate
                  FOR EACH tmp IN l_aLIBRAW
                     IF ! Empty( tmp2 := FindLib( hbmk, tmp, hbmk[ _HBMK_aLIBPATH ], cLibLibPrefix, cLibLibExt ) )
                        IF hbmk[ _HBMK_lDEBUGINC ]
                           _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: EXEDEPLIB %1$s", tmp2 ) )
                        ENDIF
                        IF ! hb_FGetDateTime( tmp2, @tmp1 ) .OR. tmp1 > tTarget
                           lTargetUpToDate := .F.
                           EXIT
                        ENDIF
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK .AND. ( Len( l_aOBJ ) + Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ) ) > 0 .AND. ! hbmk[ _HBMK_lCLEAN ]

         /* Must be called before target creation to avoid errors. */
         DoLinkDelete( hbmk )

         IF lTargetUpToDate
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Target up to date: %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )

            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )
               l_lIMPLIBToProcess := .T.
            CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateDyn ] .AND. ! Empty( cBin_Dyn )
               l_lIMPLIBToProcess := .T.
            ENDCASE
         ELSE
            IF ! hb_DirBuild( hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ) )
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create directory for target '%1$s'." ), hbmk[ _HBMK_cPROGNAME ] ) )
            ENDIF
            IF ! Empty( l_cIMPLIBNAME )
               IF ! hb_DirBuild( hb_FNameDir( l_cIMPLIBNAME ) )
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create directory for import library '%1$s'." ), l_cIMPLIBNAME ) )
               ENDIF
            ENDIF

            IF hbmk[ _HBMK_lREBUILD ] .OR. ;
               ( ! hbmk[ _HBMK_lINC ] .AND. lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ] .AND. ! Empty( cBin_Lib ) ) /* non-incremental + static lib */
               IF hb_FileExists( hbmk[ _HBMK_cPROGNAME ] ) .AND. ;
                  FErase( hbmk[ _HBMK_cPROGNAME ] ) == F_ERROR
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot delete existing target '%1$s'." ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF
            ENDIF

            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )

               PlugIn_Execute_All( hbmk, "pre_link" )

               IF ( hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ] ) .OR. hbmk[ _HBMK_lInfo ]
                  _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Linking... %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF

               /* Linking */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Link, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
               nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Link, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

               /* TODO: eliminate recursive macros from hbmk[ _HBMK_aOPTL ] */
               cOpt_Link := StrTran( cOpt_Link, "{FL}", ;
                  iif( hbmk[ _HBMK_lBLDFLGL ], hb_Version( HB_VERSION_FLAG_LINKER ) + " ", "" ) + ;
                  GetEnv( "HB_USER_LDFLAGS" ) + ;
                  iif( Empty( hbmk[ _HBMK_aOPTL ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTL ] ) ) )

               cOpt_Link := AllTrim( hb_StrReplace( cOpt_Link, { ;
                  "{LO}" => ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, nOpt_FNF, cObjPrefix ), ;
                  "{LS}" => ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_cWorkDir ], cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, nOpt_FNF, cResPrefix ), ;
                  "{LA}" => ArrayToList( l_aOBJA,, nOpt_Esc, nOpt_FNF ), ;
                  "{LL}" => ArrayToList( l_aLIB,, nOpt_Esc, nOpt_FNF, cLibPrefix ), ;
                  "{LB}" => ArrayToList( l_aLIBA,, nOpt_Esc, nOpt_FNF ), ;
                  "{LF}" => iif( Empty( hbmk[ _HBMK_aOPTLPOST ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTLPOST ] ) ), ;
                  "{IM}" => ArrayToList( hbmk[ _HBMK_aDEF ],, nOpt_Esc, nOpt_FNF, cDefPrefix ), ;
                  "{OE}" => FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ), ;
                  "{OM}" => FNameEscape( hb_FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".map" ), nOpt_Esc, nOpt_FNF ), ;
                  "{OI}" => FNameEscape( l_cIMPLIBNAME, nOpt_Esc, nOpt_FNF ), ;
                  "{DL}" => ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, nOpt_FNF, cLibPathPrefix ), ;
                  "{DB}" => hbmk[ _HBMK_cHB_INSTALL_BIN ] } ) )

               /* Handle moving the whole command line to a script, if requested. */
               cScriptFile := NIL
               IF "{SCRIPT}" $ cOpt_Link
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Link, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Link := "@" + cScriptFile
                  ELSE
                     _hbmk_OutErr( hbmk, I_( "Warning: Link script could not be created, continuing in command-line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Link + " " + cOpt_Link

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     _hbmk_OutStd( hbmk, I_( "Linker command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
                  IF ! Empty( cScriptFile )
                     _hbmk_OutStd( hbmk, I_( "Linker script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_hb_processRunCatch( cCommand, @cStdOutErr ) ) != 0
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running linker. %1$d" ), tmp ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nExitCode ] := _EXIT_RUNLINKER
                  ENDIF

                  /* Run failed linker command again to
                     analyze its output and present hints */
                  IF ! hbmk[ _HBMK_lQuiet ]
                     ShowFunctionProviders( hbmk, ExtractHarbourSymbols( cStdOutErr ), .F. )
                  ENDIF
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

               IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK
                  l_lIMPLIBToProcess := .T.
               ENDIF

               IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK .AND. hbmk[ _HBMK_lGUI ] .AND. hbmk[ _HBMK_cPLAT ] == "darwin"
                  /* Build app bundle for OS X GUI apps. (experimental) */
                  tmp := hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] )
                  IF ! Empty( tmp )
                     tmp += hb_ps()
                  ENDIF
                  tmp += hb_FNameName( hbmk[ _HBMK_cPROGNAME ] ) + ".app" + hb_ps() + "Contents"
                  IF hb_DirBuild( tmp + hb_ps() + "MacOS" )
                     hb_FCopy( hbmk[ _HBMK_cPROGNAME ], tmp + hb_ps() + "MacOS" + hb_ps() + hb_FNameName( hbmk[ _HBMK_cPROGNAME ] ) )
                     IF ! hb_FileExists( tmp + hb_ps() + "Info.plist" )
                        hb_MemoWrit( tmp + hb_ps() + "Info.plist", Apple_App_Template_Files( hbmk, "Info.plist", hb_FNameName( hbmk[ _HBMK_cPROGNAME ] ) ) )
                     ENDIF
                     IF ! hb_FileExists( tmp + hb_ps() + "PkgInfo" )
                        hb_MemoWrit( tmp + hb_ps() + "PkgInfo", Apple_App_Template_Files( hbmk, "PkgInfo", hb_FNameName( hbmk[ _HBMK_cPROGNAME ] ) ) )
                     ENDIF
                     IF ! Empty( hbmk[ _HBMK_aICON ] )
                        IF hb_DirBuild( tmp + hb_ps() + "Resources" )
                           FOR EACH tmp1 IN hbmk[ _HBMK_aICON ]
                              hb_FCopy( tmp1, tmp + hb_ps() + "Resources" + hb_ps() + hb_FNameNameExt( tmp1 ) )
                           NEXT
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF

            CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateDyn ] .AND. ! Empty( cBin_Dyn )

               PlugIn_Execute_All( hbmk, "pre_link" )

               IF ( hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ] ) .OR. hbmk[ _HBMK_lInfo ]
                  _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating dynamic library... %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF

               /* Lib creation (dynamic) */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Dyn, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
               nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Dyn, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

               aOBJLIST := ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] )
               tmp := ArrayToList( aOBJLIST,, nOpt_Esc, nOpt_FNF, cDynObjPrefix )

               IF "{SCRIPT_MINGW}" $ cOpt_Dyn
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, ArrayToList( aOBJLIST, hb_eol(), nOpt_Esc, nOpt_FNF, "INPUT(" + iif( cDynObjPrefix == NIL, "", cDynObjPrefix ), ")" ) )
                     FClose( fhnd )
                     cOpt_Dyn := StrTran( cOpt_Dyn, "{SCRIPT_MINGW}" )
                     tmp := FNameEscape( cScriptFile, nOpt_Esc, nOpt_FNF )
                  ELSE
                     _hbmk_OutErr( hbmk, I_( "Warning: Dynamic lib link script could not be created, continuing in command-line." ) )
                  ENDIF
               ENDIF

               /* TODO: eliminate recursive macros from hbmk[ _HBMK_aOPTD ] */
               cOpt_Dyn := StrTran( cOpt_Dyn, "{FD}", ;
                  GetEnv( "HB_USER_DFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTD ] ) )

               cOpt_Dyn := AllTrim( hb_StrReplace( cOpt_Dyn, { ;
                  "{LO}" => tmp, ;
                  "{LS}" => ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_cWorkDir ], cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, nOpt_FNF, cResPrefix ), ;
                  "{LL}" => ArrayToList( l_aLIB,, nOpt_Esc, nOpt_FNF, cLibPrefix ), ;
                  "{LB}" => ArrayToList( l_aLIBA,, nOpt_Esc, nOpt_FNF ), ;
                  "{LF}" => iif( Empty( hbmk[ _HBMK_aOPTDPOST ] ), "", " " + ArrayToList( hbmk[ _HBMK_aOPTDPOST ] ) ), ;
                  "{IM}" => ArrayToList( hbmk[ _HBMK_aDEF ],, nOpt_Esc, nOpt_FNF, cDefPrefix ), ;
                  "{OD}" => FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ), ;
                  "{OM}" => FNameEscape( hb_FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".map" ), nOpt_Esc, nOpt_FNF ), ;
                  "{OI}" => FNameEscape( l_cIMPLIBNAME, nOpt_Esc, nOpt_FNF ), ;
                  "{DL}" => ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, nOpt_FNF, cLibPathPrefix ), ;
                  "{DB}" => hbmk[ _HBMK_cHB_INSTALL_BIN ] } ) )

               /* Handle moving the whole command line to a script, if requested. */
               cScriptFile := NIL
               IF "{SCRIPT}" $ cOpt_Dyn
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Dyn, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Dyn := "@" + cScriptFile
                  ELSE
                     _hbmk_OutErr( hbmk, I_( "Warning: Dynamic lib link script could not be created, continuing in command-line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Dyn + " " + cOpt_Dyn

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     _hbmk_OutStd( hbmk, I_( "Dynamic lib link command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
                  IF ! Empty( cScriptFile )
                     _hbmk_OutStd( hbmk, I_( "Dynamic lib link script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_hb_processRunCatch( cCommand, @cStdOutErr ) ) != 0
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running dynamic lib link command. %1$d" ), tmp ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nExitCode ] := _EXIT_RUNLINKER
                  ENDIF

                  /* Run failed linker command again to
                     analyze its output and present hints */
                  IF ! hbmk[ _HBMK_lQuiet ]
                     ShowFunctionProviders( hbmk, ExtractHarbourSymbols( cStdOutErr ), .F. )
                  ENDIF
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

               IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK
                  l_lIMPLIBToProcess := .T.
               ENDIF

            CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ] .AND. ! Empty( cBin_Lib )

               PlugIn_Execute_All( hbmk, "pre_lib" )

               IF ( hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ] ) .OR. hbmk[ _HBMK_lInfo ]
                  _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating static library... %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF

               /* Lib creation (static) */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Lib, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
               nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Lib, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

               cOpt_Lib := AllTrim( hb_StrReplace( cOpt_Lib, { ;
                  "{FA}" => GetEnv( "HB_USER_AFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTA ] ), ;
                  "{LO}" => ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, nOpt_FNF, cLibObjPrefix ), ;
                  "{LL}" => ArrayToList( l_aLIB,, nOpt_Esc, nOpt_FNF, cLibPrefix ), ;
                  "{LB}" => ArrayToList( l_aLIBA,, nOpt_Esc, nOpt_FNF ), ;
                  "{OL}" => FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ), ;
                  "{DL}" => ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, nOpt_FNF, cLibPathPrefix ), ;
                  "{DB}" => hbmk[ _HBMK_cHB_INSTALL_BIN ] } ) )

               /* Handle moving the whole command line to a script, if requested. */
               cScriptFile := NIL
               IF "{SCRIPT}" $ cOpt_Lib
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Lib, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Lib := "@" + cScriptFile
                  ELSE
                     _hbmk_OutErr( hbmk, I_( "Warning: Lib script could not be created, continuing in command-line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Lib + " " + cOpt_Lib

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     _hbmk_OutStd( hbmk, I_( "Lib command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
                  IF ! Empty( cScriptFile )
                     _hbmk_OutStd( hbmk, I_( "Lib script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running lib command. %1$d" ), tmp ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nExitCode ] := _EXIT_RUNLIB
                  ENDIF
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            ENDCASE
         ENDIF

         IF lTargetUpToDate .OR. hbmk[ _HBMK_nExitCode ] == _EXIT_OK

            IF ! Empty( hbmk[ _HBMK_cHBX ] )
               /* Use the implib, if we created one.
                  It is the safer target to extract exports from.
                  On Windows, .dlls sometimes export stuff
                  that is really meant to be imported, despite all
                  the switches and hunt for finding any useful
                  root reason for this behavior.
                  Try '-hbdyn rddado.hbp' for an example. */
               mk_extern( hbmk, iif( Empty( l_cIMPLIBNAME ), hbmk[ _HBMK_cPROGNAME ], l_cIMPLIBNAME ), cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex, hbmk[ _HBMK_cHBX ] )
            ENDIF

            DoLink( hbmk )
            IF ! lTargetUpToDate .OR. hbmk[ _HBMK_lInstForce ]
               FOR EACH tmp IN hbmk[ _HBMK_aLINK ]
                  hb_AIns( hbmk[ _HBMK_aINSTFILE ], 1, { "", tmp }, .T. )
               NEXT
            ENDIF
         ENDIF

         IF ! lTargetUpToDate .OR. hbmk[ _HBMK_lInstForce ]
            /* For win/bcc and os2/gcc the implib is not created at this point yet,
               so there will be a copy failure in case the implib generation
               fails at the post-processing phase. */
            IF hbmk[ _HBMK_lIMPLIB ] .AND. HBMK_ISPLAT( "win|os2|dos" ) .AND. ;
               l_lIMPLIBToProcess
               hb_AIns( hbmk[ _HBMK_aINSTFILE ], 1, { "implib", l_cIMPLIBNAME }, .T. )
            ENDIF
            hb_AIns( hbmk[ _HBMK_aINSTFILE ], 1, { "", hbmk[ _HBMK_cPROGNAME ] }, .T. )
         ENDIF
      ENDIF
   ENDIF

   /* Cleanup */
   IF ( ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] ) .OR. ;
      ( hbmk[ _HBMK_lCreateHRB ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] ) .OR. ;
      ( hbmk[ _HBMK_lCreatePPO ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] )

      PlugIn_Execute_All( hbmk, "pre_cleanup" )

      IF hbmk[ _HBMK_lCLEAN ]
         FErase( hbmk[ _HBMK_cPROGNAME ] )
         IF hbmk[ _HBMK_lIMPLIB ] .AND. HBMK_ISPLAT( "win|os2|dos" ) .AND. l_cIMPLIBNAME != NIL
            FErase( l_cIMPLIBNAME )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            FErase( hb_FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".map" ) )
         ENDIF
         IF lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ]
            /* bcc is known to create it for static libs */
            FErase( hb_FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".bak" ) )
         ENDIF
         DoLinkDelete( hbmk )
      ENDIF
      IF ! Empty( l_cCSTUB )
         FErase( l_cCSTUB )
         FErase( FNameDirExtSet( l_cCSTUB, hbmk[ _HBMK_cWorkDir ], cObjExt ) )
      ENDIF
      IF ! Empty( l_cCPPSTUB )
         FErase( l_cCPPSTUB )
         FErase( FNameDirExtSet( l_cCPPSTUB, hbmk[ _HBMK_cWorkDir ], cObjExt ) )
      ENDIF
      IF ! Empty( l_cRESSTUB )
         FErase( l_cRESSTUB )
         FErase( FNameDirExtSet( l_cRESSTUB, hbmk[ _HBMK_cWorkDir ], cResExt ) )
      ENDIF
      IF ! hbmk[ _HBMK_lINC ] .OR. hbmk[ _HBMK_lCLEAN ]
         AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".c", .T. ), {| tmp | FErase( tmp ) } )
      ENDIF
      IF hbmk[ _HBMK_lCLEAN ]
         IF hbmk[ _HBMK_lINC ]
            AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".ppo", .T. ), {| tmp | FErase( tmp ) } )
            AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".ppt", .T. ), {| tmp | FErase( tmp ) } )
            AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".pot", .T. ), {| tmp | FErase( tmp ) } )
            AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".d", .T. ), {| tmp | FErase( tmp ) } )
         ENDIF
         IF hbmk[ _HBMK_lCreateHRB ] .AND. hbmk[ _HBMK_lStopAfterHarbour ]
            AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".hrb", .T. ), {| tmp | FErase( tmp ) } )
         ENDIF
      ENDIF
      IF ! lStopAfterCComp .OR. hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ]
         IF ! hbmk[ _HBMK_lINC ] .OR. hbmk[ _HBMK_lCLEAN ]
            IF ! Empty( cResExt )
               AEval( ListDirExt( hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_cWorkDir ], cResExt ), {| tmp | FErase( tmp ) } )
            ENDIF
            IF ! Empty( l_aOBJ )
               AEval( l_aOBJ, {| tmp | FErase( tmp ) } )
            ENDIF
         ENDIF
      ENDIF
      IF ! Empty( l_aCLEAN )
         AEval( l_aCLEAN, {| tmp | FErase( tmp ) } )
      ENDIF
      IF lDeleteWorkDir
         hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
      ENDIF
      IF hbmk[ _HBMK_lCLEAN ]
         hb_DirUnbuild( hbmk[ _HBMK_cWorkDir ] )
      ENDIF
   ENDIF

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]
      IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. ! lTargetUpToDate .AND. ;
         ( ! lStopAfterCComp .OR. hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ] )

         IF ! Empty( cBin_Post )

            cCommand := ;
               cBin_Post + " " + ;
               AllTrim( hb_StrReplace( cOpt_Post, { ;
                  "{OB}" => FNameEscape( hbmk[ _HBMK_cPROGNAME ], hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ), ;
                  "{OI}" => iif( l_cIMPLIBNAME == NIL, "", FNameEscape( l_cIMPLIBNAME, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) } ) ) /* for implib post-process command */

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  _hbmk_OutStd( hbmk, I_( "Post processor command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running post processor command. %1$d:" ), tmp ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + _OUT_EOL )
               ENDIF
            ENDIF
         ENDIF

         /* Setup compressor for host platform */

         #if defined( __PLATFORM__WINDOWS ) .OR. ;
             defined( __PLATFORM__DOS )

            cBin_Cprs := "upx.exe"
            cOpt_Cprs := "{OB}"
            cOpt_CprsMin := "-1"
            cOpt_CprsMax := "-9"
            IF hbmk[ _HBMK_cPLAT ] == "linux"
               /* To avoid error below when creating Linux targets on non-Linux hosts using watcom:
                  upx: t.: CantPackException: invalid Phdr p_offset; try '--force-execve'
                  [vszakats] */
               cOpt_Cprs += " --force-execve"
            ENDIF

         #elif defined( __PLATFORM__UNIX )

            cBin_Cprs := "upx"
            cOpt_Cprs := "{OB}"
            cOpt_CprsMin := "-1"
            cOpt_CprsMax := "-9"

         #else

            cBin_Cprs := NIL
            cOpt_Cprs := ""
            cOpt_CprsMin := ""
            cOpt_CprsMax := ""

         #endif

         IF hbmk[ _HBMK_nCOMPR ] != _COMPR_OFF .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! Empty( cBin_Cprs )

            /* Executable compression */

            #if defined( __PLATFORM__WINDOWS ) .OR. ;
                defined( __PLATFORM__DOS )

               /* Use embedded version if present, otherwise it should be in PATH. */
               IF hb_FileExists( hb_DirSepAdd( hb_DirBase() ) + cBin_Cprs )
                  cBin_Cprs := hb_DirSepAdd( hb_DirBase() ) + cBin_Cprs
               ENDIF
            #endif

            DO CASE
            CASE hbmk[ _HBMK_nCOMPR ] == _COMPR_MIN ; cOpt_Cprs += " " + cOpt_CprsMin
            CASE hbmk[ _HBMK_nCOMPR ] == _COMPR_MAX ; cOpt_Cprs += " " + cOpt_CprsMax
            ENDCASE

            cOpt_Cprs := StrTran( cOpt_Cprs, "{OB}", FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ) )
            cOpt_Cprs := AllTrim( cOpt_Cprs )

            cCommand := cBin_Cprs + " " + cOpt_Cprs

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  _hbmk_OutStd( hbmk, I_( "Compression command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running compression command. %1$d:" ), tmp ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + _OUT_EOL )
               ENDIF
            ENDIF
         ENDIF

         /* Setup code signing for host platform */

         IF HBMK_ISPLAT( "win|wce" )
            IF ( cBin_Sign := FindInPath( "signtool.exe" ) ) != NIL /* in MS Windows SDK */
               cOpt_Sign := "sign {FS} -f {ID} -p {PW} -t {UT} {OB}"
               IF AScan( hbmk[ _HBMK_aOPTS ], {| tmp | HBMK_IS_IN( Lower( tmp ), "-v|/v" ) } ) == 0
                  AAdd( hbmk[ _HBMK_aOPTS ], "-q" )
               ENDIF
            ELSEIF ( cBin_Sign := FindInPath( "posign.exe" ) ) != NIL /* in Pelles C 7.00.0 or newer */
               cBin_Sign := "posign.exe"
               cOpt_Sign := "{FS} -pfx:{ID} -pwd:{PW} -timeurl:{UT} {OB}"
            ELSE
               _hbmk_OutErr( hbmk, I_( "Warning: Code signing skipped, because no supported code signing tool could be found." ) )
            ENDIF
         ENDIF

         #if defined( __PLATFORM__DARWIN )
            IF HBMK_ISPLAT( "darwin" )
               cBin_Sign := "codesign"
               cOpt_Sign := "{FS} -s {ID} -f {OB}"
            ENDIF
         #endif

         IF ! Empty( cOpt_SignID ) .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! Empty( cBin_Sign )

            /* Code signing */

            hReplace := { ;
               "{FS}" => ArrayToList( hbmk[ _HBMK_aOPTS ] ), ;
               "{UT}" => _HBMK_SIGN_TIMEURL, ;
               "{ID}" => cOpt_SignID, ;
               "{OB}" => FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ), ;
               "{PW}" => cOpt_SignPass }

            cCommand := cBin_Sign + " " + AllTrim( hb_StrReplace( cOpt_Sign, hReplace ) )
            hReplace[ "{PW}" ] := iif( Empty( cOpt_SignPass ), "", "***" )
            tmp1     := cBin_Sign + " " + AllTrim( hb_StrReplace( cOpt_Sign, hReplace ) )

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  _hbmk_OutStd( hbmk, I_( "Code sign command:" ) )
               ENDIF
               OutStd( tmp1 + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running code sign command. %1$d:" ), tmp ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutStd( tmp1 + _OUT_EOL )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF hbmk[ _HBMK_nExitCode ] == _EXIT_OK .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. ;
         ( ! lStopAfterCComp .OR. hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ] )

         DoInstCopy( hbmk )
      ENDIF

      PlugIn_Execute_All( hbmk, "post_build" )
   ENDIF

   PlugIn_Execute_All( hbmk, "post_all" )

   IF hbmk[ _HBMK_lDEBUGTIME ]
      _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Running time: %1$ds" ), Round( ( hb_MilliSeconds() - nStart ) / 1000, 1 ) ) )
   ENDIF

   IF ! lSkipBuild .AND. hbmk[ _HBMK_lBEEP ]
      DoBeep( hbmk[ _HBMK_nExitCode ] == _EXIT_OK )
   ENDIF

   IF ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! lStopAfterCComp .AND. ;
      ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ;
      hbmk[ _HBMK_nExitCode ] == _EXIT_OK .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. hbmk[ _HBMK_lRUN ]
      cCommand := hbmk[ _HBMK_cPROGNAME ]
      IF Empty( hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ) )
         cCommand := "." + hb_ps() + hbmk[ _HBMK_cPROGNAME ]
      ENDIF
      #if defined( __PLATFORM__WINDOWS )
         IF hbmk[ _HBMK_lGUI ]
            IF hb_osIsWinNT()
               cCommand := 'start "" ' + FNameEscape( cCommand, _ESC_DBLQUOTE )
            ELSE
               cCommand := "start " + cCommand
            ENDIF
         ENDIF
      #elif defined( __PLATFORM__OS2 )
         IF hbmk[ _HBMK_lGUI ]
            cCommand := 'start "" ' + FNameEscape( cCommand, _ESC_DBLQUOTE )
         ENDIF
      #elif defined( __PLATFORM__DARWIN )
         IF hbmk[ _HBMK_lGUI ]
            /* TOFIX: Find a way to pass arbitrary options to an .app. */
            l_aOPTRUN := {}
            cCommand := "open " + FNameEscape( cCommand + ".app", _ESC_NIX )
         ENDIF
      #endif
      cCommand := AllTrim( cCommand + " " + ArrayToList( l_aOPTRUN ) )
      IF hbmk[ _HBMK_lTRACE ]
         IF ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, I_( "Running executable:" ) )
         ENDIF
         OutStd( cCommand + _OUT_EOL )
      ENDIF
      IF ! hbmk[ _HBMK_lDONTEXEC ]
         hbmk[ _HBMK_nExitCode ] := hb_run( cCommand )
      ENDIF
   ENDIF

   RETURN hbmk[ _HBMK_nExitCode ]

STATIC PROCEDURE ProcEnvOption( cValue )

   LOCAL tmp

   IF ! Empty( cValue )
      IF     ( tmp := At( "=", cValue ) ) > 1
         hb_SetEnv( Left( cValue, tmp - 1 ), SubStr( cValue, tmp + 1 ) )
      ELSEIF ( tmp := At( "+", cValue ) ) > 1
         hb_SetEnv( Left( cValue, tmp - 1 ), GetEnv( Left( cValue, tmp - 1 ) ) + SubStr( cValue, tmp + 1 ) )
      ELSEIF ( tmp := At( "#", cValue ) ) > 1
         hb_SetEnv( Left( cValue, tmp - 1 ), SubStr( cValue, tmp + 1 ) + GetEnv( Left( cValue, tmp - 1 ) ) )
      ELSEIF ( tmp := At( "-", cValue ) ) > 1
         hb_SetEnv( Left( cValue, tmp - 1 ) )
      ELSE
         hb_SetEnv( cValue, "" )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE IncPointlessPair( hbmk, /* @ */ aParam1, aParam2, cParam2L, cOption )

   IF aParam1 != NIL .AND. ;  /* there was a previous option */
      aParam1[ _PAR_cFileName ] == aParam2[ _PAR_cFileName ] .AND. ;  /* same location */
      Lower( aParam2[ _PAR_cParam ] ) == cOption .AND. ;  /* no condition/filter used */
      !( aParam1[ 4 ] == aParam2[ _PAR_cParam ] )  /* different effective option */

      _hbmk_OutErr( hbmk, hb_StrFormat( ;
         iif( Empty( aParam1[ _PAR_cFileName ] ), ;
            I_( "Warning: Pointless usage of %1$s and %2$s options together on command-line." ), ;
            I_( "Warning: Pointless usage of %1$s and %2$s options together in '%3$s' line %4$d and %5$d." ) ), ;
         aParam1[ _PAR_cParam ], ;
         aParam2[ _PAR_cParam ], ;
         aParam1[ _PAR_cFileName ], ;
         aParam1[ _PAR_nLine ], ;
         aParam2[ _PAR_nLine ] ) )
   ENDIF

   aParam1 := AClone( aParam2 )
   AAdd( aParam1, cParam2L )  /* [ 4 ] */

   RETURN

STATIC FUNCTION ParamToString( aParam )
   RETURN iif( Empty( aParam[ _PAR_cFileName ] ), ;
      hb_StrFormat( "'%1$s'", aParam[ _PAR_cParam ] ), ; /* on the command line */
      hb_StrFormat( "'%1$s' in %2$s:%3$d", aParam[ _PAR_cParam ], aParam[ _PAR_cFileName ], aParam[ _PAR_nLine ] ) )

STATIC FUNCTION InvalidOptionValue( hbmk, aParam )
   RETURN _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignored invalid option value in: %1$s" ), ParamToString( aParam ) ) )

#ifdef HB_LEGACY_LEVEL4
/* Do not delete this function when legacy level is reached,
   only convert above guard to a temporary '#if 0' one. */
STATIC FUNCTION LegacyWarning( hbmk, aParam, cSuggestion )
   RETURN _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Deprecated compatibility option: %1$s. Use '%2$s' instead." ), ParamToString( aParam ), cSuggestion ) )
#endif

STATIC PROCEDURE AAddWithWarning( hbmk, aArray, cOption, aParam, lNew )

   STATIC sc_aWarning := { ;
      "-Wl,--allow-multiple-definition", ; /* gcc */
      "muldefs", ; /* ld '-z muldefs' */
      "force:multiple", ; /* msvc, pocc, watcom, xcc */
      "w-dup", ; /* bcc */
      "w-dpl" } /* bcc (for libs) */

   IF AScan( sc_aWarning, {| tmp | Lower( tmp ) $ Lower( cOption ) } ) > 0
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Dangerous low-level option not recommended: %1$s" ), ParamToString( aParam ) ) )
   ENDIF

   IF lNew
      AAddNewNotEmpty( aArray, cOption )
   ELSE
      AAddNotEmpty( aArray, cOption )
   ENDIF

   RETURN

STATIC FUNCTION CheckParamInc( hbmk, cPath )

#if ! defined( __PLATFORM__UNIX )
   LOCAL cComp
#endif

   cPath := hb_DirSepDel( hb_PathNormalize( cPath ) )

   /* check against Harbour core header directory */
   IF hb_FileMatch( cPath, hbmk[ _HBMK_cHB_INSTALL_INC ] )
      RETURN .F.
   ENDIF

#if ! defined( __PLATFORM__UNIX )
   /* check against any directory under embedded compiler directory */
   cComp := hb_DirSepDel( hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP ) )
   IF hb_FileMatch( Left( cPath, Len( cComp ) ), cComp )
      RETURN .F.
   ENDIF
#endif

   RETURN .T.

STATIC FUNCTION CheckParamLibPath( hbmk, cPath )

#if ! defined( __PLATFORM__UNIX )
   LOCAL cComp
#endif

   cPath := hb_DirSepDel( hb_PathNormalize( cPath ) )

   /* check against Harbour core lib directory */
   IF hb_FileMatch( cPath, hbmk[ _HBMK_cHB_INSTALL_LIB ] )
      RETURN .F.
   ENDIF

   /* check against Harbour core lib directory in its legacy
      (non multi-compiler/platform) location. It is even more
      dangerous than above. */
   IF ! hbmk[ _HBMK_lSysLoc ] .AND. ;
      hb_FileMatch( cPath, hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + "lib" ) )
      RETURN .F.
   ENDIF

#if ! defined( __PLATFORM__UNIX )
   /* check against any directory under embedded compiler directory */
   cComp := hb_DirSepDel( hb_PathNormalize( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + _HBMK_SPECDIR_COMP ) )
   IF hb_FileMatch( Left( cPath, Len( cComp ) ), cComp )
      RETURN .F.
   ENDIF
#endif

   RETURN .T.

STATIC FUNCTION CheckParamLib( hbmk, cLibName, lHBC, aParam )

   LOCAL cExtL := Lower( hb_FNameExt( cLibName ) )

   LOCAL cSuggestion := ""
   LOCAL cOpt

   /* detect path in libname (non-portable) */
   IF ! Empty( hb_FNameDir( cLibName ) )
      cOpt := hb_DirSepDel( hb_FNameDir( cLibName ) )
      IF CheckParamLibPath( hbmk, cOpt )
         cSuggestion += iif( lHBC, "'" + "libpaths=", "-L" ) + cOpt + iif( lHBC, "'", "" )
      ENDIF
   ENDIF

   /* detect certain variations of non-portable libname */
   IF cExtL == ".lib" .OR. ;
      cExtL == ".a" .OR. ;
      cOpt != NIL /* always include lib name suggestion, if there was a path in the value */

      cOpt := hb_FNameName( cLibName )
      /* readd empty extension */
      IF Right( hb_FNameNameExt( cLibName ), 1 ) == "."
         cOpt += "."
      ENDIF
      IF cExtL == ".a" .AND. Lower( Left( cOpt, 3 ) ) == "lib"
         cOpt := SubStr( cOpt, 4 )
      ENDIF
      /* never suggest core libs */
      IF AScan( hbmk[ _HBMK_aLIB_BASE_WARN ], {| tmp | Lower( tmp ) == Lower( cOpt ) } ) > 0
         cOpt := ""
      ENDIF
      IF ! Empty( cOpt )
         IF ! Empty( cSuggestion )
            cSuggestion += iif( lHBC, ", ", " " )
         ENDIF
         cSuggestion += iif( lHBC, "'" + "libs=", "-l" ) + cOpt + iif( lHBC, "'", "" )
      ENDIF
   ENDIF

   /* offer suggestions */
   IF ! Empty( cSuggestion )
#ifdef HB_LEGACY_LEVEL4
      IF lHBC
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Non-portable parameter: %1$s. Use %2$s directives(s) instead." ), ParamToString( aParam ), cSuggestion ) )
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Non-portable parameter: %1$s. Use '%2$s' option(s) instead." ), ParamToString( aParam ), cSuggestion ) )
      ENDIF
#else
      IF lHBC
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring non-portable parameter: %1$s. Use %2$s directives(s) instead." ), ParamToString( aParam ), cSuggestion ) )
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring non-portable parameter: %1$s. Use '%2$s' option(s) instead." ), ParamToString( aParam ), cSuggestion ) )
      ENDIF
      RETURN .F.
#endif
   ENDIF

   cLibName := Lower( hb_FNameName( cLibName ) )

   IF AScan( hbmk[ _HBMK_aLIB_BASE_WARN ], {| tmp | Lower( tmp ) == cLibName } ) > 0
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring explicitly specified core library: %1$s (in option %2$s)" ), cLibName, ParamToString( aParam ) ) )
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC PROCEDURE convert_incpaths_to_options( hbmk, cOptIncMask, lCHD_Comp )

   LOCAL cBaseDir
   LOCAL cINCPATH

   IF lCHD_Comp
      cBaseDir := hb_DirSepAdd( hb_PathRelativize( hb_PathNormalize( PathMakeAbsolute( hbmk[ _HBMK_cWorkDir ], hb_cwd() ) ), hb_cwd(), .T. ) )
   ENDIF

   FOR EACH cINCPATH IN hbmk[ _HBMK_aINCPATH ]
      IF ! Empty( cINCPATH )
         /* Different escaping for internal and external compiler. */
         AAddNew( hbmk[ _HBMK_aOPTPRG ], "-i" + iif( hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE, cINCPATH, FNameEscape( cINCPATH, hbmk[ _HBMK_nCmd_Esc ] ) ) )
         IF ! hbmk[ _HBMK_lStopAfterHarbour ]
            IF lCHD_Comp
               /* Rebase source dirs relative to the target dir */
               AAddNew( hbmk[ _HBMK_aOPTC ], StrTran( cOptIncMask, "{DI}", FNameEscape( hb_PathNormalize( PathMakeAbsolute( cINCPATH, cBaseDir ) ), hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) )
            ELSE
               AAddNew( hbmk[ _HBMK_aOPTC ], StrTran( cOptIncMask, "{DI}", FNameEscape( cINCPATH, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) )
            ENDIF
            AAddNew( hbmk[ _HBMK_aOPTRES ], StrTran( cOptIncMask, "{DI}", FNameEscape( cINCPATH, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION hbmk_hb_FileExists( cFileName )

#if defined( __PLATFORM__DOS )
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )

   IF Len( cName ) > 8 .OR. ;
      Len( cExt ) > ( 1 + 3 )
      /* Return failure instead of loading wrong file or
         do other unpredictable operation */
      RETURN .F.
   ENDIF
#endif

   RETURN hb_FileExists( cFileName )

/* NOTE: We store -hbdyn objects in different dirs by default as - for Windows
         platforms - they're always built using different compilation options
         than normal targets. [vszakats] */
STATIC PROCEDURE Set_lCreateDyn( hbmk, lValue )

   hbmk[ _HBMK_lCreateDyn ] := lValue

   IF hbmk[ _HBMK_lCreateDyn ]
      hbmk[ _HBMK_cWorkDirDynSub ] := hb_ps() + "hbdyn"
   ELSE
      hbmk[ _HBMK_cWorkDirDynSub ] := ""
   ENDIF

   RETURN

STATIC FUNCTION gcc_opt_lngc_fill( hbmk )

   DO CASE
   CASE HBMK_ISCOMP( "gcc|gccarm|gccomf|mingw|mingw64|mingwarm|djgpp" )

      SWITCH hbmk[ _HBMK_cC ]
      CASE "iso90" ; RETURN "-std=c89"
      CASE "iso99" ; RETURN "-std=c9x"
      CASE "iso1x" ; RETURN "-std=c1x"
      CASE "gnu90" ; RETURN "-std=gnu89"
      CASE "gnu99" ; RETURN "-std=gnu9x"
      CASE "gnu1x" ; RETURN "-std=gnu1x"
      ENDSWITCH

   CASE HBMK_ISCOMP( "icc|clang" )

      SWITCH hbmk[ _HBMK_cC ]
      CASE "iso90" ; RETURN "-std=c89"
      CASE "iso99" ; RETURN "-std=c99"
      CASE "gnu90" ; RETURN "-std=gnu89"
      CASE "gnu99" ; RETURN "-std=gnu99"
      ENDSWITCH

   ENDCASE

   RETURN ""

STATIC FUNCTION gcc_opt_lngcpp_fill( hbmk )

   DO CASE
   CASE HBMK_ISCOMP( "gcc|gccarm|gccomf|mingw|mingw64|mingwarm|djgpp" )

      SWITCH hbmk[ _HBMK_cCPP ]
      CASE "iso98" ; RETURN "-std=c++98"
      CASE "iso0x" ; RETURN "-std=c++0x"
      CASE "gnu98" ; RETURN "-std=gnu++98"
      CASE "gnu0x" ; RETURN "-std=gnu++0x"
      ENDSWITCH

   CASE HBMK_ISCOMP( "icc" )

      SWITCH hbmk[ _HBMK_cCPP ]
      CASE "iso98"
      CASE "gnu98" ; RETURN "-std=gnu++98"
      CASE "iso0x" ; RETURN "-std=c++0x"
      ENDSWITCH

   ENDCASE

   RETURN ""

STATIC PROCEDURE vxworks_env_init( hbmk )

   /* Array positions for aTable */
   #define _VX_CCSUFFIX         1
   #define _VX_DIAB_CPU         2
   #define _VX_CPU              3
   #define _VX_LIB_SUBDIR       4

   #define _VX_DIAB_ENV         "rtp"

   /* Conversion table between ours and vxworks CPU values required to target that CPU */
   LOCAL aTable := { ;
      "x86"  => { "pentium", "X86LH"  , "_VX_SIMPENTIUM", "simpentium/SIMPENTIUM" }, ;
      "arm"  => { "arm"    , "ARMV7LS", "_VX_ARMARCH7"  , "arm/ARMARCH7"          }, ;
      "mips" => { "mips"   , ""       , ""              , ""                      }, ;
      "ppc"  => { "ppc"    , ""       , ""              , ""                      } }

   IF hbmk[ _HBMK_cCPU ] $ aTable
      IF Empty( hbmk[ _HBMK_cCCSUFFIX ] )
         /* Used by gcc, and it is also used for strip even with diab compiler */
         hbmk[ _HBMK_cCCSUFFIX ] := aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_CCSUFFIX ]
      ENDIF
      IF hbmk[ _HBMK_cCOMP ] == "diab"
         IF ! Empty( aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_DIAB_CPU ] )
            AAdd( hbmk[ _HBMK_aOPTC ], "-t" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_DIAB_CPU ] + ":" + _VX_DIAB_ENV )
            AAdd( hbmk[ _HBMK_aOPTL ], "-t" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_DIAB_CPU ] + ":" + _VX_DIAB_ENV )
            AAdd( hbmk[ _HBMK_aOPTD ], "-t" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_DIAB_CPU ] + ":" + _VX_DIAB_ENV )
         ENDIF
         IF ! Empty( aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_CPU ] )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_VX_CPU=" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_CPU ] )
         ENDIF
      ENDIF
      IF ! Empty( aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_LIB_SUBDIR ] )
         IF hbmk[ _HBMK_lCreateDyn ]
            AAdd( hbmk[ _HBMK_aLIBPATH ], hb_DirSepToOS( GetEnv( "WIND_BASE" ) + "/target/lib/usr/lib/" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_LIB_SUBDIR ] + "/common/PIC" ) )
         ELSE
            AAdd( hbmk[ _HBMK_aLIBPATH ], hb_DirSepToOS( GetEnv( "WIND_BASE" ) + "/target/lib/usr/lib/" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_LIB_SUBDIR ] + "/common" ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE DoLinkCalc( hbmk )

   LOCAL cDir, cName, cExt
   LOCAL tmp

   FOR EACH tmp IN hbmk[ _HBMK_aLINK ]

      cDir := hb_PathRelativize( hb_FNameDir( PathMakeAbsolute( tmp, hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ) ) ), hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ), .T. )
      /* Cheap hack */
      IF cDir == "." + hb_ps() .OR. ;
         cDir == hb_ps()
         cDir := ""
      ENDIF
      hb_FNameSplit( hbmk[ _HBMK_cPROGNAME ],, @cName, @cExt )

      tmp := { ;
         /* <cNewFileName>    */ hb_PathNormalize( PathMakeAbsolute( tmp, hb_FNameDir( hbmk[ _HBMK_cPROGNAME ] ) ) ), ;
         /* <cTargetFileName> */ hb_FNameMerge( cDir, cName, cExt ) }
   NEXT

   RETURN

STATIC FUNCTION DoLinkDelete( hbmk )

   LOCAL tmp

   FOR EACH tmp IN hbmk[ _HBMK_aLINK ]
      FErase( tmp[ 1 ] )
   NEXT

   RETURN .T.

STATIC FUNCTION DoLink( hbmk )

   LOCAL tmp

   FOR EACH tmp IN hbmk[ _HBMK_aLINK ]
      IF hb_FLinkSym( tmp[ 2 ], tmp[ 1 ] ) == F_ERROR
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Failed creating symbolic link %1$s to %2$s" ), tmp[ 1 ], tmp[ 2 ] ) )
      ELSE
         _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created symbolic link %1$s to %2$s" ), tmp[ 1 ], tmp[ 2 ] ) )
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION DoIMPLIB( hbmk, bBlk_ImpLib, cLibLibPrefix, cLibLibExt, aIMPLIBSRC, cPROGNAME, cInstCat, lDoSrc )

   LOCAL cMakeImpLibDLL
   LOCAL tmp, tmp1
   LOCAL nNotFound

   LOCAL aToDelete
   LOCAL lRetVal := .F.

   IF HB_ISBLOCK( bBlk_ImpLib )
      IF ! Empty( aIMPLIBSRC )
         hb_default( @lDoSrc, .F. )
         aToDelete := {}
         nNotFound := 0
         FOR EACH cMakeImpLibDLL IN aIMPLIBSRC

            cMakeImpLibDLL := hb_FNameExtSetDef( cMakeImpLibDLL, ".dll" )

            IF lDoSrc
               IF hb_FileExists( cMakeImpLibDLL )
                  /* Keep a list of found dynamic library sources, allowing this
                     list to be used to install those dynamic libs */
                  AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { "depimplibsrc", cMakeImpLibDLL }, .T. )
               ENDIF
            ELSE
               tmp1 := cPROGNAME
               hb_default( @tmp1, hb_FNameName( cMakeImpLibDLL ) )
               tmp := FN_CookLib( hb_FNameMerge( hbmk[ _HBMK_cPROGDIR ], tmp1 ), cLibLibPrefix, cLibLibExt )

               IF hbmk[ _HBMK_lCLEAN ]
                  AAddNew( aToDelete, tmp )
               ELSE
                  SWITCH Eval( bBlk_ImpLib, cMakeImpLibDLL, tmp, ArrayToList( hbmk[ _HBMK_aOPTI ] ) )
                  CASE _HBMK_IMPLIB_OK
                     _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created import library: %1$s <= %2$s" ), tmp, cMakeImpLibDLL ) )
                     AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { cInstCat, tmp }, .T. )
                     EXIT
                  CASE _HBMK_IMPLIB_FAILED
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Failed creating import library %1$s from %2$s." ), tmp, cMakeImpLibDLL ) )
                     EXIT
                  CASE _HBMK_IMPLIB_NOTFOUND
                     ++nNotFound
                     EXIT
                  ENDSWITCH
               ENDIF
            ENDIF
         NEXT

         IF ! lDoSrc
            IF nNotFound == Len( aIMPLIBSRC )
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: No import library sources were found." ) ) )
            ELSE
               IF hbmk[ _HBMK_lCLEAN ]
                  AEval( aToDelete, {| tmp | FErase( tmp ) } )
               ELSE
                  lRetVal := .T.
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lInfo ]
            _hbmk_OutErr( hbmk, I_( "Warning: No import library source was specified" ) )
         ENDIF
      ENDIF
   ELSE
      _hbmk_OutErr( hbmk, I_( "Error: Creating import libraries is not supported for this platform or compiler." ) )
   ENDIF

   RETURN lRetVal

#define _INST_cGroup            1
#define _INST_cData             2

STATIC PROCEDURE DoInstCopy( hbmk )

   LOCAL aInstPath
   LOCAL aInstFile
   LOCAL cInstPath
   LOCAL cInstFile

   LOCAL cDestFileName
   LOCAL nCopied

   LOCAL tSrc, tDst

   LOCAL cLink

   IF ! Empty( hbmk[ _HBMK_aINSTPATH ] )

      FOR EACH aInstPath IN hbmk[ _HBMK_aINSTPATH ]

         cInstPath := aInstPath[ _INST_cData ]

         nCopied := 0 /* files copied */
         FOR EACH aInstFile IN hbmk[ _HBMK_aINSTFILE ]

            IF HB_ISARRAY( aInstFile[ _INST_cData ] )
               cInstFile := aInstFile[ _INST_cData ][ 1 ]
               cLink := aInstFile[ _INST_cData ][ 2 ]
            ELSE
               cInstFile := aInstFile[ _INST_cData ]
               cLink := NIL
            ENDIF

            IF aInstPath[ _INST_cGroup ] == aInstFile[ _INST_cGroup ]
               IF Empty( hb_FNameNameExt( cInstPath ) )
                  cDestFileName := hb_DirSepAdd( cInstPath ) + hb_FNameNameExt( cInstFile )
               ELSE
                  /* If destination is a full name, do not copy the extra files, only the first one.
                     (for the empty group name, this will be the build target) */
                  IF nCopied > 0
                     IF hbmk[ _HBMK_lInfo ]
                        _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Install path not a directory (%1$s). Extra install files not copied." ), cInstPath ) )
                     ENDIF
                     EXIT
                  ELSE
                     cDestFileName := cInstPath
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lINC ] .OR. ;
                  ! hb_FGetDateTime( cDestFileName, @tDst ) .OR. ;
                  ! hb_FGetDateTime( cInstFile, @tSrc ) .OR. ;
                  tSrc > tDst

                  IF hb_DirBuild( hb_FNameDir( cDestFileName ) )
                     ++nCopied
                     IF cLink != NIL
                        FErase( cDestFileName )
                        IF hb_FLinkSym( cLink, cDestFileName ) == F_ERROR
                           _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Copying symbolic link %1$s to %2$s failed with %3$d." ), cInstFile, cDestFileName, FError() ) )
                        ELSEIF hbmk[ _HBMK_lInfo ]
                           _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Copied symbolic link %1$s to %2$s" ), cInstFile, cDestFileName ) )
                        ENDIF
                     ELSE
                        IF hb_FCopy( cInstFile, cDestFileName ) == F_ERROR
                           _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Copying %1$s to %2$s failed with %3$d." ), cInstFile, cDestFileName, FError() ) )
                        ELSEIF hbmk[ _HBMK_lInfo ]
                           _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Copied %1$s to %2$s" ), cInstFile, cDestFileName ) )
                        ENDIF
                     ENDIF
                  ELSE
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create install directory for install target %1$s." ), cDestFileName ) )
                  ENDIF
               ENDIF
            ENDIF
         NEXT
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE DoBeep( lSuccess )

   LOCAL nRepeat := iif( lSuccess, 1, 2 )
   LOCAL tmp

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      FOR tmp := 1 TO nRepeat
         Tone( 800, 3.5 )
      NEXT
   ELSE
      OutStd( Replicate( e"\a", nRepeat ) )
   ENDIF

   RETURN

STATIC FUNCTION hbmk_UTF8_BOM()
   RETURN ;
      hb_BChar( 0xEF ) + ;
      hb_BChar( 0xBB ) + ;
      hb_BChar( 0xBF )

STATIC FUNCTION hbmk_MemoRead( cFileName )

   LOCAL cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */

   IF Left( cFile, Len( hbmk_UTF8_BOM() ) ) == hbmk_UTF8_BOM()
      cFile := SubStr( cFile, Len( hbmk_UTF8_BOM() ) + 1 )
   ENDIF

   RETURN hb_UTF8ToStr( cFile )

STATIC FUNCTION hbmk_hb_compile( hbmk, ... )

   LOCAL cSaveCP
   LOCAL xRetVal

   IF Empty( hbmk[ _HBMK_cCPPRG ] )
      RETURN hb_compile( ... )
   ELSE
      cSaveCP := hb_cdpSelect( hbmk[ _HBMK_cCPPRG ] )
      /* We can use this function as this is a GPL licenced application */
      xRetVal := hb_compile( ... )
      hb_cdpSelect( cSaveCP )
   ENDIF

   RETURN xRetVal

STATIC FUNCTION hbmk_hb_compileBuf( hbmk, ... )

   LOCAL cSaveCP
   LOCAL xRetVal

   IF Empty( hbmk[ _HBMK_cCPPRG ] )
      RETURN hb_compileBuf( ... )
   ELSE
      cSaveCP := hb_cdpSelect( hbmk[ _HBMK_cCPPRG ] )
      /* We can use this function as this is a GPL licenced application */
      xRetVal := hb_compileBuf( ... )
      hb_cdpSelect( cSaveCP )
   ENDIF

   RETURN xRetVal

STATIC FUNCTION CompileCLoop( hbmk, aTO_DO, cBin_CompC, cOpt_CompC, hReplace, cObjExt, nOpt_Esc, nOpt_FNF, nJob, nJobs )

   LOCAL lResult := .T.
   LOCAL cCommand
   LOCAL tmp, tmp1

   LOCAL lOutputSpecified
   LOCAL cOutputFile

   FOR EACH tmp IN aTO_DO

      lOutputSpecified := "{OO}" $ cOpt_CompC
      cOutputFile := FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt )

      hReplace[ "{IC}" ] := FNameEscape( tmp, nOpt_Esc, nOpt_FNF )
      hReplace[ "{OO}" ] := FNameEscape( cOutputFile, nOpt_Esc, nOpt_FNF )

      cCommand := cBin_CompC + " " + AllTrim( hb_StrReplace( cOpt_CompC, hReplace ) )

      IF hbmk[ _HBMK_lTRACE ]
         IF ! hbmk[ _HBMK_lQuiet ]
            IF nJobs > 1
               _hbmk_OutStd( hbmk, hb_StrFormat( I_( "C/C++ compiler command job #%1$d:" ), nJob ) )
            ELSE
               _hbmk_OutStd( hbmk, I_( "C/C++ compiler command:" ) )
            ENDIF
         ENDIF
         OutStd( cCommand + _OUT_EOL )
      ENDIF

      IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hb_processRun( cCommand ) ) != 0
         IF nJobs > 1
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler job #%1$d. %2$d" ), nJob, tmp1 ) )
         ELSE
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler. %1$d" ), tmp1 ) )
         ENDIF
         IF ! hbmk[ _HBMK_lQuiet ]
            OutErr( cCommand + _OUT_EOL )
         ENDIF
         /* Delete output file in case of compile error.
            (only if we know for sure what is the output filename, that is when we
             speficied it on the command line)
            This is to protect against compiler bugs (f.e. gcc with -pipe option)
            when dummy or wrong object file is left on the disk, and misleading
            next incremental build pass. [vszakats] */
         IF lOutputSpecified
            FErase( cOutputFile )
         ENDIF
         lResult := .F.
         EXIT
      ENDIF
   NEXT

   RETURN lResult

STATIC FUNCTION SetupForGT( cGT_New, /* @ */ cGT, /* @ */ lGUI )

   IF IsValidHarbourID( cGT_New )

      cGT := cGT_New

      /* Setup default GUI mode for core GTs:
         (please do not add contrib/3rd parties here) */
      SWITCH Lower( cGT_New )
      CASE "gtcgi"
      CASE "gtcrs"
      CASE "gtdos"
      CASE "gtos2"
      CASE "gtpca"
      CASE "gtsln"
      CASE "gtstd"
      CASE "gttrm"
      CASE "gtwin"
         lGUI := .F.
         EXIT

      CASE "gtgui"
      CASE "gtwvt"
      CASE "gtxwc"
         lGUI := .T.
         EXIT

      ENDSWITCH

      RETURN .T.
   ENDIF

   RETURN .F.

/* This function will scan and detect header dependencies newer than
   root file. It will not attempt to parse all possible #include syntaxes
   and source code formats, will not try to interpret comments, line
   continuation, different keyword and filename cases, etc, etc. In
   order to work, it will need #include "filename" and #include
   <filename> format in source. If this is not enough for your needs,
   feel free to update the code.
   [vszakats] */

STATIC FUNCTION FindNewerHeaders( hbmk, cFileName, tTimeParent, lCMode, cBin_CompC )

   LOCAL tTimeSelf
   LOCAL tTimeDependency
   LOCAL tmp
   LOCAL cExt
   LOCAL cModule
   LOCAL cDependency
   LOCAL aCommand

   THREAD STATIC t_pRegexInclude := NIL
   THREAD STATIC t_hExclStd := NIL

   IF hbmk[ _HBMK_nHEAD ] == _HEAD_OFF
      RETURN .F.
   ENDIF

   IF tTimeParent != NIL
      IF hb_FGetDateTime( cFileName, @tTimeSelf ) .AND. tTimeSelf > tTimeParent
         RETURN .T.
      ENDIF
      IF checkDepTime( hbmk, cFileName, tTimeParent )
         RETURN .T.
      ENDIF
   ENDIF

   cExt := Lower( hb_FNameExt( cFileName ) )

   /* Filter out non-source format inputs for MinGW / windres */
   IF HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|clang" ) .AND. HBMK_ISPLAT( "win|wce" ) .AND. cExt == ".res"
      RETURN .F.
   ENDIF

   IF ! lCMode .AND. hbmk[ _HBMK_nHEAD ] == _HEAD_DEP
      cDependency := FNameDirExtSet( cFileName, hbmk[ _HBMK_cWorkDir ], ".d" )
      IF ! hb_FGetDateTime( cDependency, @tTimeDependency ) .OR. ;
         tTimeDependency > tTimeParent
         RETURN .T.
      ENDIF
      deplst_read( hbmk, hbmk[ _HBMK_hDEPTS ], cDependency )

   ELSEIF ! lCMode .AND. hbmk[ _HBMK_nHEAD ] == _HEAD_NATIVE .AND. hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE

      IF hbmk[ _HBMK_lDEBUGINC ]
         _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: Calling Harbour compiler to detect dependencies of %1$s", cFileName ) )
      ENDIF

      aCommand := ArrayAJoin( { { "-q0", "-sm" }, ;
                                { iif( hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ], "-n1", "-n2" ) }, ;
                                { cFileName }, ;
                                iif( hbmk[ _HBMK_lBLDFLGP ], { hb_Version( HB_VERSION_FLAG_PRG ) }, {} ), ;
                                ListToArray( iif( Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), "", " " + GetEnv( "HB_USER_PRGFLAGS" ) ) ), ;
                                hbmk[ _HBMK_aOPTPRG ] } )

      IF ! HB_ISSTRING( tmp := hbmk_hb_compileBuf( hbmk, "harbour", aCommand ) )
         RETURN .F.
      ENDIF

      /* TODO: Module handling. */
      FOR EACH cModule IN hb_ATokens( tmp, Chr( 9 ) )
         IF ! Empty( cModule )
            FOR EACH cDependency IN hb_ATokens( cModule, " " )
               IF ( cDependency:__enumIndex() > 1 .OR. ; /* Skip own (module) name */
                    ( LEFTEQUAL( cFileName, "@" ) .AND. cExt == ".clp" ) ) .AND. ;
                    ! Empty( cDependency )
                  IF hbmk[ _HBMK_lDEBUGINC ]
                     _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER (NATIVE) %1$s", cDependency ) )
                  ENDIF
                  IF hb_FGetDateTime( cDependency, @tTimeDependency ) .AND. tTimeDependency > tTimeParent
                     IF Empty( hbmk[ _HBMK_hAUTOHBC ] )
                        RETURN .T.
                     ENDIF
                  ENDIF
                  IF cDependency $ hbmk[ _HBMK_hAUTOHBC ]
                     hbmk[ _HBMK_hAUTOHBCFOUND ][ cDependency ] := hbmk[ _HBMK_hAUTOHBC ][ cDependency ]
                     hb_HDel( hbmk[ _HBMK_hAUTOHBC ], cDependency )
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      NEXT

   ELSEIF lCMode .AND. hbmk[ _HBMK_nHEAD ] == _HEAD_NATIVE .AND. HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|djgpp|gccomf|clang|open64" )

      IF hbmk[ _HBMK_lDEBUGINC ]
         _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: Calling C/C++ compiler to detect dependencies of %1$s", cFileName ) )
      ENDIF

      tmp := ""
      hb_processRun( ;
         cBin_CompC + " -MM" + ;
         " " + iif( hbmk[ _HBMK_lBLDFLGC ], hb_Version( HB_VERSION_FLAG_C ) + " ", "" ) + ;
         GetEnv( "HB_USER_CFLAGS" ) + ;
         " " + ArrayToList( hbmk[ _HBMK_aOPTC ] ) + ;
         " " + ArrayToList( hbmk[ _HBMK_aOPTCUSER ] ) + ;
         " " + cFileName,, @tmp )

      tmp := StrTran( tmp, Chr( 13 ) )
      tmp := StrTran( tmp, " \" + Chr( 10 ) )

      FOR EACH cModule IN hb_ATokens( tmp, Chr( 10 ) )
         IF ! Empty( cModule )
            FOR EACH cDependency IN hb_ATokens( cModule, " " )
               IF cDependency:__enumIndex() > 2 .AND. ; /* Skip own (module) name as object and source */
                  ! Empty( cDependency )
                  IF hbmk[ _HBMK_lDEBUGINC ]
                     _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C HEADER (NATIVE) %1$s", cDependency ) )
                  ENDIF
                  IF hb_FGetDateTime( cDependency, @tTimeDependency ) .AND. tTimeDependency > tTimeParent
                     RETURN .T.
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      NEXT
   ELSE
      IF ! lCMode .AND. LEFTEQUAL( cFileName, "@" ) .AND. cExt == ".clp"
         FOR EACH cDependency IN clpfile_read( SubStr( cFileName, 2 ) )
            IF ! Empty( cDependency )
               IF hbmk[ _HBMK_lDEBUGINC ]
                  _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER (CLP) %1$s", cDependency ) )
               ENDIF
               IF getNewestTime( hbmk, cDependency, @hbmk[ _HBMK_hFiles ], lCMode ) > tTimeParent
                  RETURN .T.
               ENDIF
            ENDIF
         NEXT
      ELSEIF tTimeParent == NIL /* Header detection for autohbc feature */
         getNewestTime( hbmk, cFileName, @hbmk[ _HBMK_hFiles ], lCMode )
         RETURN .T.
      ELSEIF getNewestTime( hbmk, cFileName, @hbmk[ _HBMK_hFiles ], lCMode ) > tTimeParent
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

#define _HBMK_HEADER_cHeader        1
#define _HBMK_HEADER_lSystemHeader  2
#define _HBMK_HEADER_LEN_           2

STATIC FUNCTION s_getIncludedFiles( hbmk, cFile, cParentDir, lCMode )

   THREAD STATIC t_pRegexInclude
   THREAD STATIC t_pRegexRequire
   THREAD STATIC t_hExclStd

   LOCAL aDeps
   LOCAL cFileBody
   LOCAL lSystemHeader
   LOCAL cHeader
   LOCAL aDep
   LOCAL tmp

   /* NOTE:
         http://en.wikipedia.org/wiki/PCRE
         http://www.pcre.org/pcre.txt */

   IF t_pRegexInclude == NIL
      /* Switch to non UTF8 CP - otherwise PCRE fails on user files
       * containing non UTF8 characters. For this expression we do
       * not need UTF8 or any other fixed encoding.
       */
      tmp := hb_cdpSelect( "EN" )
      t_pRegexInclude := hb_regexComp( _HBMK_REGEX_INCLUDE, .F. /* lCaseSensitive */, .T. /* lNewLine */ )
      t_pRegexRequire := hb_regexComp( _HBMK_REGEX_REQUIRE, .F. /* lCaseSensitive */, .T. /* lNewLine */ )
      hb_cdpSelect( tmp )
      IF Empty( t_pRegexInclude )
         _hbmk_OutErr( hbmk, I_( "Internal Error: Regular expression engine missing or unsupported. Check your Harbour build settings." ) )
         t_pRegexInclude := 0 /* To show the error only once by setting to non-NIL empty value */
      ENDIF
   ENDIF

   aDeps := {}
   IF ! Empty( t_pRegexInclude ) .AND. ;
      ! Empty( t_pRegexRequire )

      cFileBody := MemoRead( cFile )

      IF ! Empty( cFileBody )
         FOR EACH tmp IN hb_regexAll( t_pRegexInclude, cFileBody, ;
                                      NIL /* lCaseSensitive */, ;
                                      NIL /* lNewLine */, NIL, ;
                                      NIL /* nGetMatch */, ;
                                      .T. /* lOnlyMatch */ )
            cHeader := tmp[ 2 ]
            lSystemHeader := ( Left( cHeader, 1 ) == "<" )
            cHeader := SubStr( cHeader, 2, Len( cHeader ) - 2 )

            /* Do not spend time on known system headers */
            IF lSystemHeader

               IF t_hExclStd == NIL
                  t_hExclStd := { ;
                     "assert.h"       => NIL, ; /* Standard C */
                     "ctype.h"        => NIL, ;
                     "errno.h"        => NIL, ;
                     "float.h"        => NIL, ;
                     "limits.h"       => NIL, ;
                     "locale.h"       => NIL, ;
                     "math.h"         => NIL, ;
                     "setjmp.h"       => NIL, ;
                     "signal.h"       => NIL, ;
                     "stdarg.h"       => NIL, ;
                     "stddef.h"       => NIL, ;
                     "stdio.h"        => NIL, ;
                     "stdlib.h"       => NIL, ;
                     "string.h"       => NIL, ;
                     "time.h"         => NIL, ;
                     "iso646.h"       => NIL, ; /* ISO C NA1 */
                     "wchar.h"        => NIL, ;
                     "wctype.h"       => NIL, ;
                     "complex.h"      => NIL, ; /* ISO C C99 */
                     "fenv.h"         => NIL, ;
                     "inttypes.h"     => NIL, ;
                     "stdbool.h"      => NIL, ;
                     "stdint.h"       => NIL, ;
                     "tgmath.h"       => NIL, ;
                     "unistd.h"       => NIL, ; /* Standard C POSIX */
                     "aio.h"          => NIL, ;
                     "arpa/inet.h"    => NIL, ;
                     "cpio.h"         => NIL, ;
                     "dirent.h"       => NIL, ;
                     "dlfcn.h"        => NIL, ;
                     "fcntl.h"        => NIL, ;
                     "fmtmsg.h"       => NIL, ;
                     "fnmatch.h"      => NIL, ;
                     "ftw.h"          => NIL, ;
                     "glob.h"         => NIL, ;
                     "grp.h"          => NIL, ;
                     "iconv.h"        => NIL, ;
                     "langinfo.h"     => NIL, ;
                     "libgen.h"       => NIL, ;
                     "monetary.h"     => NIL, ;
                     "mqueue.h"       => NIL, ;
                     "ndbm.h"         => NIL, ;
                     "net/if.h"       => NIL, ;
                     "netdb.h"        => NIL, ;
                     "netinet/in.h"   => NIL, ;
                     "netinet/tcp.h"  => NIL, ;
                     "nl_types.h"     => NIL, ;
                     "poll.h"         => NIL, ;
                     "pthread.h"      => NIL, ;
                     "pwd.h"          => NIL, ;
                     "regex.h"        => NIL, ;
                     "sched.h"        => NIL, ;
                     "search.h"       => NIL, ;
                     "semaphore.h"    => NIL, ;
                     "spawn.h"        => NIL, ;
                     "strings.h"      => NIL, ;
                     "stropts.h"      => NIL, ;
                     "sys/ipc.h"      => NIL, ;
                     "sys/mman.h"     => NIL, ;
                     "sys/msg.h"      => NIL, ;
                     "sys/resource.h" => NIL, ;
                     "sys/select.h"   => NIL, ;
                     "sys/sem.h"      => NIL, ;
                     "sys/shm.h"      => NIL, ;
                     "sys/socket.h"   => NIL, ;
                     "sys/stat.h"     => NIL, ;
                     "sys/statvfs.h"  => NIL, ;
                     "sys/time.h"     => NIL, ;
                     "sys/times.h"    => NIL, ;
                     "sys/types.h"    => NIL, ;
                     "sys/uio.h"      => NIL, ;
                     "sys/un.h"       => NIL, ;
                     "sys/utsname.h"  => NIL, ;
                     "sys/wait.h"     => NIL, ;
                     "syslog.h"       => NIL, ;
                     "tar.h"          => NIL, ;
                     "termios.h"      => NIL, ;
                     "trace.h"        => NIL, ;
                     "ulimit.h"       => NIL, ;
                     "unistd.h"       => NIL, ;
                     "utime.h"        => NIL, ;
                     "utmpx.h"        => NIL, ;
                     "wordexp.h"      => NIL, ;
                     "windows.h"      => NIL, ; /* OS (win) */
                     "winspool.h"     => NIL, ;
                     "shellapi.h"     => NIL, ;
                     "ole2.h"         => NIL, ;
                     "dos.h"          => NIL, ; /* OS (dos) */
                     "os2.h"          => NIL }  /* OS (os2) */
               ENDIF

               IF StrTran( Lower( cHeader ), "\", "/" ) $ t_hExclStd
                  LOOP
               ENDIF
            ENDIF

            IF ! lCMode .AND. cHeader $ hbmk[ _HBMK_hAUTOHBC ]
               hbmk[ _HBMK_hAUTOHBCFOUND ][ cHeader ] := hbmk[ _HBMK_hAUTOHBC ][ cHeader ]
               hb_HDel( hbmk[ _HBMK_hAUTOHBC ], cHeader )
            ENDIF

            IF ( cHeader := FindHeader( hbmk, cHeader, cParentDir, lSystemHeader, lSystemHeader ) ) != NIL

               IF hbmk[ _HBMK_lDEBUGINC ]
                  _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER %1$s %2$s", cHeader, iif( lSystemHeader, "(system)", "" ) ) )
               ENDIF

               aDep := Array( _HBMK_HEADER_LEN_ )
               aDep[ _HBMK_HEADER_cHeader ]       := cHeader
               aDep[ _HBMK_HEADER_lSystemHeader ] := lSystemHeader
               AAdd( aDeps, aDep )
            ENDIF
         NEXT

         IF ! lCMode
            FOR EACH tmp IN hb_regexAll( t_pRegexRequire, cFileBody, ;
                                         NIL /* lCaseSensitive */, ;
                                         NIL /* lNewLine */, NIL, ;
                                         NIL /* nGetMatch */, ;
                                         .T. /* lOnlyMatch */ )
               cHeader := tmp[ 2 ]
               cHeader := SubStr( cHeader, 2, Len( cHeader ) - 2 )
               hbmk[ _HBMK_hAUTOHBCFOUND ][ "." + cHeader ] := hb_FNameExtSet( cHeader, ".hbc" )
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN aDeps

/* optimized time scan algorithm */

#define _HBMK_FILEDEF_aINCFILES     1
#define _HBMK_FILEDEF_tFILETIME     2
#define _HBMK_FILEDEF_tNEWESTTIME   3
#define _HBMK_FILEDEF_lCANSCAN      4
#define _HBMK_FILEDEF_LEN_          4

STATIC PROCEDURE s_getFilesDep( hbmk, cFile, hFiles, cParentDir, lSystemHeader, lCMode )

   LOCAL aDep, tTime, aDeps, aFile

   IF ! cFile $ hFiles

      IF lSystemHeader
         /* Do not scan into system headers */
         aDeps := {}
      ELSE
         aDeps := s_getIncludedFiles( hbmk, cFile, iif( lCMode, hb_FNameDir( cFile ), cParentDir ), lCMode )
      ENDIF

      IF ! hb_FGetDateTime( cFile, @tTime )
         tTime := t"00:00"
      ENDIF

      aFile := Array( _HBMK_FILEDEF_LEN_ )
      aFile[ _HBMK_FILEDEF_aINCFILES ]   := aDeps
      aFile[ _HBMK_FILEDEF_tFILETIME ]   := tTime
      aFile[ _HBMK_FILEDEF_tNEWESTTIME ] := NIL
      aFile[ _HBMK_FILEDEF_lCANSCAN ]    := .T.
      hFiles[ cFile ] := aFile

      FOR EACH aDep IN aDeps
         s_getFilesDep( hbmk, ;
            aDep[ _HBMK_HEADER_cHeader ], ;
            hFiles, ;
            cParentDir, ;
            aDep[ _HBMK_HEADER_lSystemHeader ], ;
            lCMode )
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION s_getNewestTime( cFile, hFiles, lFileReq )

   LOCAL aFile, tTime, aDep, tDep, lReq

   tTime := t"00:00"
   IF cFile $ hFiles
      aFile := hFiles[ cFile ]
      IF aFile[ _HBMK_FILEDEF_tNEWESTTIME ] != NIL
         /* this file does not have any cross references other then to self
          * and the time of the newest included file is already calculated
          * so we can simply use it
          */
         tTime := aFile[ _HBMK_FILEDEF_tNEWESTTIME ]
      ELSEIF aFile[ _HBMK_FILEDEF_lCANSCAN ]
         lReq := .F.
         aFile[ _HBMK_FILEDEF_lCANSCAN ] := .F.
         tTime := aFile[ _HBMK_FILEDEF_tFILETIME ]
         FOR EACH aDep IN aFile[ _HBMK_FILEDEF_aINCFILES ]
            tDep := s_getNewestTime( aDep[ _HBMK_HEADER_cHeader ], hFiles, @lReq )
            IF tDep > tTime
               tTime := tDep
            ENDIF
         NEXT
         IF lReq
            /* This file has references to some other files already
             * scanned. It is possible that these are circular references
             * and the time of files with such references is not fully
             * calculated yet (they are now process on higher recursion
             * levels) so we cannot store calculated time as the final
             * newest time of this file
             */
            lFileReq := .T.
         ELSE
            /* we do not have any circular references to files with
             * undefined yet time so we can safely set the time of the
             * newest included file to not repeat the scan when this
             * file is reused
             */
            aFile[ _HBMK_FILEDEF_lCANSCAN ] := .T.
            aFile[ _HBMK_FILEDEF_tNEWESTTIME ] := tTime
         ENDIF
      ELSE
         lFileReq := .T.
      ENDIF
   ENDIF

   RETURN tTime

STATIC FUNCTION getNewestTime( hbmk, cFile, hFiles, lCMode )

   LOCAL aFile, tTime

   IF hFiles == NIL
      hFiles := { => }
      /* for easier visualization the scan steps in debug mode */
      /* hb_HKeepOrder( hFiles, .T. ) */
   ENDIF
   s_getFilesDep( hbmk, cFile, hFiles, hb_FNameDir( cFile ), .F., lCMode )
   tTime := s_getNewestTime( cFile, hFiles )
   /* we calculated the newest time of this file and all included files
    * so we can set it for future reuse if this file included also by
    * some other ones.
    */
   hFiles[ cFile, _HBMK_FILEDEF_tNEWESTTIME ] := tTime
   /* mark all files with cross references as scanable so we can
    * repeat the scan process for other files
    */
   FOR EACH aFile IN hFiles
      aFile[ _HBMK_FILEDEF_lCANSCAN ] := .T.
   NEXT

   RETURN tTime

STATIC FUNCTION checkDepTime( hbmk, cFile, tTime )

   LOCAL cDepFile, tDepTime

   IF hbmk[ _HBMK_lDEBUGINC ]
      _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CHECK DepTime: %s (%s)", cFile, hb_TSToStr( tTime ) ) )
   ENDIF

   IF cFile $ hbmk[ _HBMK_hDEPTS ]
      IF hbmk[ _HBMK_lDEBUGINC ]
         _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CHECKING....", cFile) )
      ENDIF
      FOR EACH cDepFile IN hbmk[ _HBMK_hDEPTS ][ cFile ]
         IF ! hb_FGetDateTime( cDepFile, @tDepTime ) .OR. ;
            tDepTime > tTime
            IF hbmk[ _HBMK_lDEBUGINC ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CHECK DepTime=%s !!! (%s>%s)", cDepFile, hb_TSToStr( tDepTime ), hb_TSToStr( tTime ) ) )
            ENDIF
            RETURN .T.
         ENDIF
         IF hbmk[ _HBMK_lDEBUGINC ]
            _hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CHECK DepTime=%s (%s)", cDepFile, hb_TSToStr( tDepTime ) ) )
         ENDIF
      NEXT
   ENDIF

   RETURN .F.

STATIC FUNCTION clpfile_read( cFileName )

   LOCAL cFileBody := MemoRead( cFileName )
   LOCAL aFiles
   LOCAL cFile

   cFileBody := StrTran( cFileBody, Chr( 13 ) )
   cFileBody := StrTran( cFileBody, Chr( 10 ), " " )
   cFileBody := StrTran( cFileBody, Chr( 9 ), " " )

   aFiles := hb_ATokens( cFileBody,, .T. )
   FOR EACH cFile IN aFiles
      cFile := hb_FNameExtSetDef( StrTran( cFile, '"' ), ".prg" )
   NEXT

   RETURN aFiles

STATIC FUNCTION deplst_read( hbmk, hDeps, cFileName )

   LOCAL cFileBody := MemoRead( cFileName )
   LOCAL cList := ""
   LOCAL cLine
   LOCAL nLine := 0

   cFileBody := StrTran( cFileBody, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   cFileBody := StrTran( cFileBody, Chr( 9 ), " " )

   FOR EACH cLine IN hb_ATokens( cFileBody, Chr( 10 ) )
      ++nLine
      cLine := AllTrim( cLine )
      IF cLine == "\" .OR. Right( cLine, 2 ) == " \"
         cList += Left( cLine, Len( cLine ) - 1 )
      ELSE
         cList += cLine
         IF ! deplst_add( hDeps, cList )
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: In %1$s at line %2$d %3$s:" ), cFileName, nLine, cList ) )
            RETURN NIL
         ENDIF
         cList := ""
      ENDIF
   NEXT

   IF ! deplst_add( hDeps, cList )
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: In %1$s at line %2$d %3$s:" ), cFileName, nLine, cList ) )
      RETURN NIL
   ENDIF

   RETURN hDeps

STATIC FUNCTION deplst_add( hDeps, cList )

   LOCAL cFile
   LOCAL aList
   LOCAL n

   IF ! Empty( cList )
      n := At( ": ", cList )
      IF n != 0 .AND. ! Empty( cFile := AllTrim( Left( cList, n - 1 ) ) )
         aList := hb_ATokens( SubStr( cList, n + 1 ) )
         IF cFile $ hDeps
            AMerge( hDeps[ cFile ], aList )
         ELSE
            hDeps[ cFile ] := aList
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION inst_split_arg( cParam, /* @ */ cName, /* @ */ cData )

   LOCAL nPos

   /* separate install group from install file or path.
      install group must be at least 2 chars, otherwise it is considered as drive letter */

   IF ( nPos := At( ":", cParam ) ) > 2
      cName := Left( cParam, nPos - 1 )
      cData := SubStr( cParam, nPos + 1 )
   ELSE
      cName := ""
      cData := cParam
   ENDIF

   cData := hb_DirSepToOS( cData )

   RETURN ! Empty( cData )

STATIC FUNCTION autohbc_split_arg( cParam, /* @ */ cHeader, /* @ */ cHBC )

   LOCAL nPos

   IF ( nPos := At( ":", cParam ) ) > 0
      cHeader := Left( cParam, nPos - 1 )
      cHBC := SubStr( cParam, nPos + 1 )
   ELSE
      cHeader := cParam
      cHBC := ""
   ENDIF

   IF Empty( cHeader ) .AND. ! Empty( cHBC )
      cHeader := hb_FNameExtSet( cHBC )
   ELSEIF Empty( cHBC ) .AND. ! Empty( cHeader )
      cHBC := hb_FNameExtSet( cHeader )
   ENDIF

   RETURN ! Empty( cHeader ) .AND. ! Empty( cHBC )

STATIC FUNCTION dep_split_arg( hbmk, cParam, /* @ */ cName, /* @ */ cData )

   LOCAL nPos
   LOCAL dep

   IF ( nPos := At( ":", cParam ) ) > 1
      cName := Left( cParam, nPos - 1 )
      cData := SubStr( cParam, nPos + 1 )
   ELSE
      cName := NIL
      cData := NIL
   ENDIF

   IF ! Empty( cName ) .AND. ! Empty( cData )
      IF !( cName $ hbmk[ _HBMK_hDEP ] )
         dep := Array( _HBMKDEP_MAX_ )
         dep[ _HBMKDEP_cName ] := cName
         dep[ _HBMKDEP_aPKG ] := {}
         dep[ _HBMKDEP_aKeyHeader ] := {}
         dep[ _HBMKDEP_cControl ] := NIL
         dep[ _HBMKDEP_lOptional ] := .F.
         dep[ _HBMKDEP_cINCROOT ] := ""
         dep[ _HBMKDEP_aINCPATH ] := {}
         dep[ _HBMKDEP_aINCPATHLOCAL ] := {}
         dep[ _HBMKDEP_aIMPLIBSRC ] := {}
         dep[ _HBMKDEP_cIMPLIBDST ] := NIL
         dep[ _HBMKDEP_cFound ] := NIL
         dep[ _HBMKDEP_lFound ] := .F.
         dep[ _HBMKDEP_lFoundLOCAL ] := .F.
         dep[ _HBMKDEP_cVersion ] := ""
         dep[ _HBMKDEP_lForced ] := .F.
         dep[ _HBMKDEP_lDetected ] := .F.
         hbmk[ _HBMK_hDEP ][ cName ] := dep
      ENDIF
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC PROCEDURE dep_postprocess_one( hbmk, dep )

   LOCAL tmp
   LOCAL cControlL

   /* Add our given name of the dependency to the list of
      package names to check. Little convenience also
      encouraging usage of standard package names. [vszakats] */
   AAddNew( dep[ _HBMKDEP_aPKG ], Lower( dep[ _HBMKDEP_cName ] ) )

   /* Process "control" value. It can be a control keyword,
      or a custom header include path. */
   IF dep[ _HBMKDEP_cControl ] == NIL
      dep[ _HBMKDEP_cControl ] := GetEnv( hb_StrFormat( _HBMK_WITH_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) )
   ENDIF

   cControlL := Lower( dep[ _HBMKDEP_cControl ] )

   DO CASE
   CASE cControlL == "no"
      dep[ _HBMKDEP_cControl ] := cControlL
      dep[ _HBMKDEP_aKeyHeader ] := {}
      dep[ _HBMKDEP_aPKG ] := {}
      dep[ _HBMKDEP_cINCROOT ] := ""
      dep[ _HBMKDEP_aINCPATH ] := {}
      dep[ _HBMKDEP_aINCPATHLOCAL ] := {}
      dep[ _HBMKDEP_aIMPLIBSRC ] := {}
      dep[ _HBMKDEP_cIMPLIBDST ] := NIL
      dep[ _HBMKDEP_lForced ] := .T.
   CASE cControlL == "local"
      dep[ _HBMKDEP_cControl ] := cControlL
      dep[ _HBMKDEP_cINCROOT ] := ""
      dep[ _HBMKDEP_aINCPATH ] := {}
   CASE cControlL == "nolocal"
      dep[ _HBMKDEP_cControl ] := cControlL
      dep[ _HBMKDEP_aINCPATHLOCAL ] := {}
   CASE Left( cControlL, Len( "strict:" ) ) == "strict:"
      dep[ _HBMKDEP_cControl ] := cControlL
      dep[ _HBMKDEP_aINCPATH ] := { SubStr( dep[ _HBMKDEP_cControl ], Len( "strict:" ) + 1 ) }
      dep[ _HBMKDEP_aINCPATHLOCAL ] := {}
   CASE cControlL == "yes"
      /* do nothing */
   CASE cControlL == "force"
      dep[ _HBMKDEP_aKeyHeader ] := {}
      dep[ _HBMKDEP_aPKG ] := {}
      dep[ _HBMKDEP_cINCROOT ] := ""
      dep[ _HBMKDEP_aINCPATH ] := {}
      dep[ _HBMKDEP_aINCPATHLOCAL ] := {}
      dep[ _HBMKDEP_cFound ] := "."
      dep[ _HBMKDEP_lFound ] := .T.
      dep[ _HBMKDEP_lFoundLOCAL ] := .F.
      dep[ _HBMKDEP_lForced ] := .T.
      AAdd( hbmk[ _HBMK_aOPTC ], "-D" + hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) )
      hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) ] := NIL
   OTHERWISE
      /* If control is not a recognized control keyword, interpret it
         as a header search path and add it to the search path list
         by keeping the position where it was specified. [vszakats] */
      FOR EACH tmp IN dep[ _HBMKDEP_aINCPATH ]
         IF tmp == _HBMK_DEP_CTRL_MARKER
            tmp := dep[ _HBMKDEP_cControl ]
            EXIT
         ENDIF
      NEXT
   ENDCASE

   RETURN

STATIC FUNCTION dep_evaluate( hbmk )

   LOCAL dep

   LOCAL aREQ := {}
   LOCAL aOPT := {}
   LOCAL aWRN := {}
   LOCAL lAnyForcedOut := .F.

   FOR EACH dep IN hbmk[ _HBMK_hDEP ]
      IF dep[ _HBMKDEP_lFound ]
         IF ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Dependency '%1$s' found: %2$s%3$s%4$s%5$s" ), ;
               dep[ _HBMKDEP_cName ], ;
               dep[ _HBMKDEP_cFound ], ;
               iif( Empty( dep[ _HBMKDEP_cVersion ] ), "", " (" + dep[ _HBMKDEP_cVersion ] + ")" ), ;
               iif( dep[ _HBMKDEP_lFoundLOCAL ], " (local)", "" ), ;
               iif( dep[ _HBMKDEP_lForced ], " (forced)", "" ) ) )
         ENDIF
      ELSE
         IF dep[ _HBMKDEP_lForced ]
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Dependency '%1$s' forcibly disabled" ), dep[ _HBMKDEP_cName ] ) )
            ENDIF
            lAnyForcedOut := .T.
            LOOP
         ELSE
            IF hbmk[ _HBMK_lDEBUGDEPD ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "debugdepd: REQ %1$s: missing", dep[ _HBMKDEP_cName ] ) )
            ENDIF
         ENDIF
         IF dep[ _HBMKDEP_lOptional ]
            AAdd( aOPT, dep[ _HBMKDEP_cName ] )
         ELSE
            /* Do not issue a missing dependency error (just warning) for non-*nix
               platforms if no manual dependency location and no local dir were
               specified. This assumes that on these platforms dependencies can never
               be found on locations known in advance and specified in make
               files. [vszakats] */
            IF HBMK_ISPLAT( "win|wce|os2|dos" ) .AND. ;
               Empty( dep[ _HBMKDEP_cControl ] ) .AND. ;
               Empty( dep[ _HBMKDEP_aINCPATHLOCAL ] )
               AAdd( aWRN, dep[ _HBMKDEP_cName ] )
            ELSE
               AAdd( aREQ, dep[ _HBMKDEP_cName ] )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   IF ! Empty( aOPT ) .AND. hbmk[ _HBMK_lInfo ]
      IF Len( aOPT ) > 1
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing optional dependencies: %1$s" ), ArrayToList( aOPT, ", " ) ) )
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing optional dependency: %1$s" ), ArrayToList( aOPT, ", " ) ) )
      ENDIF
   ENDIF

   IF ! Empty( aREQ )
      IF Len( aREQ ) > 1
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Missing dependencies: %1$s" ), ArrayToList( aREQ, ", " ) ) )
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Missing dependency: %1$s" ), ArrayToList( aREQ, ", " ) ) )
      ENDIF
   ENDIF

   IF ! Empty( aWRN )
      IF Len( aWRN ) > 1
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing dependencies: %1$s" ), ArrayToList( aWRN, ", " ) ) )
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing dependency: %1$s" ), ArrayToList( aWRN, ", " ) ) )
      ENDIF
   ENDIF

   RETURN Empty( aREQ ) .AND. Empty( aWRN ) .AND. ! lAnyForcedOut

STATIC PROCEDURE dep_try_detection( hbmk, dep )

   IF ! dep[ _HBMKDEP_lDetected ]
      dep_postprocess_one( hbmk, dep )
      IF ! dep_try_pkg_detection( hbmk, dep )
         dep_try_header_detection( hbmk, dep )
      ENDIF
      dep[ _HBMKDEP_lDetected ] := .T.
   ENDIF

   RETURN

/* Try '*-config' and 'pkg-config *' detection */
STATIC FUNCTION dep_try_pkg_detection( hbmk, dep )

   LOCAL cStdOut
   LOCAL cStdErr
   LOCAL cItem

   LOCAL tmp

   LOCAL cName
   LOCAL cIncludeDir
   LOCAL cVersion

   IF ! Empty( dep[ _HBMKDEP_cINCROOT ] )
      RETURN .F.
   ENDIF

   FOR EACH cName IN dep[ _HBMKDEP_aPKG ]

      IF ! Empty( cName )
         IF ! dep[ _HBMKDEP_lFound ]
            cName := AllTrim( cName )

            cStdOut := ""
            hb_processRun( "pkg-config --modversion --libs --cflags " + cName,, @cStdOut, @cStdErr )
            IF Empty( cStdOut )
               hb_processRun( cName + "-config --version --libs --cflags",, @cStdOut, @cStdErr )
            ENDIF
#if defined( __PLATFORM__DARWIN )
            /* Homebrew */
            IF Empty( cStdOut )
               IF hb_FileExists( "/usr/local/bin/pkg-config" )
                  hb_processRun( "/usr/local/bin/pkg-config --modversion --libs --cflags " + cName,, @cStdOut, @cStdErr )
               ENDIF
            ENDIF
            IF Empty( cStdOut )
               IF hb_FileExists( "/usr/local/bin/" + cName + "-config" )
                  hb_processRun( "/usr/local/bin/" + cName + "-config --version --libs --cflags",, @cStdOut, @cStdErr )
               ENDIF
            ENDIF
            /* MacPorts/DarwinPorts */
            IF Empty( cStdOut )
               IF hb_FileExists( "/opt/local/bin/pkg-config" )
                  hb_processRun( "/opt/local/bin/pkg-config --modversion --libs --cflags " + cName,, @cStdOut, @cStdErr )
               ENDIF
            ENDIF
            IF Empty( cStdOut )
               IF hb_FileExists( "/opt/local/bin/" + cName + "-config" )
                  hb_processRun( "/opt/local/bin/" + cName + "-config --version --libs --cflags",, @cStdOut, @cStdErr )
               ENDIF
            ENDIF
#endif

            IF ! Empty( cStdOut )

               cStdOut := StrTran( cStdOut, Chr( 13 ) )
               IF ( tmp := At( Chr( 10 ), cStdOut ) ) > 0
                  cVersion := Left( cStdOut, tmp - 1 )
                  cStdOut := SubStr( cStdOut, tmp + 1 )
               ELSE
                  cVersion := "unknown version"
               ENDIF

               cStdOut := StrTran( cStdOut, Chr( 10 ), " " )

               FOR EACH cItem IN hb_ATokens( cStdOut,, .T. )
                  IF Left( cItem, Len( "-I" ) ) == "-I"
                     dep[ _HBMKDEP_lFound ] := .T.
                     EXIT
                  ENDIF
               NEXT

               IF dep[ _HBMKDEP_lFound ]

                  FOR EACH cItem IN hb_ATokens( cStdOut,, .T. )
                     DO CASE
                     CASE Left( cItem, Len( "-l" ) ) == "-l"
                        cItem := SubStr( cItem, Len( "-l" ) + 1 )
                        IF _IS_AUTOLIBSYSPRE( cItem )
                           AAdd( hbmk[ _HBMK_aLIBUSERSYSPRE ], cItem )
                        ELSE
                           AAdd( hbmk[ _HBMK_aLIBUSER ], cItem )
                        ENDIF
                     CASE Left( cItem, Len( "-L" ) ) == "-L"
                        cItem := SubStr( cItem, Len( "-L" ) + 1 )
                        AAdd( hbmk[ _HBMK_aLIBPATH ], hb_DirSepDel( hb_DirSepToOS( cItem ) ) )
                     CASE Left( cItem, Len( "-I" ) ) == "-I"
                        cItem := hb_DirSepDel( hb_DirSepToOS( SubStr( cItem, Len( "-I" ) + 1 ) ) )
                        IF Empty( cIncludeDir )
                           cIncludeDir := cItem
                        ENDIF
                        AAdd( hbmk[ _HBMK_aINCPATH ], cItem )
                     ENDCASE
                  NEXT

                  dep[ _HBMKDEP_cVersion ] := cVersion
                  dep[ _HBMKDEP_cFound ] := iif( Empty( cIncludeDir ), "(system)", cIncludeDir )
                  IF ! Empty( cIncludeDir )
                     hbmk[ _HBMK_hDEPTSDIR ][ cIncludeDir ] := NIL
                     hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_DIR_TPL, StrToDefine( cName ) ) ] := cIncludeDir
                     /* Adjust implib source names with component path */
                     FOR EACH tmp IN dep[ _HBMKDEP_aIMPLIBSRC ]
                        tmp := hb_PathNormalize( PathMakeAbsolute( tmp, hb_DirSepAdd( cIncludeDir ) ) )
                     NEXT
                  ENDIF
                  IF hbmk[ _HBMK_lDEBUGDEPD ]
                     _hbmk_OutStd( hbmk, hb_StrFormat( "debugdepd: REQ %1$s: found as pkg at %2$s (%3$s)", dep[ _HBMKDEP_cName ], dep[ _HBMKDEP_cFound ], dep[ _HBMKDEP_cVersion ] ) )
                  ENDIF
                  AAdd( hbmk[ _HBMK_aOPTC ], "-D" + hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( cName ) ) )
                  hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( cName ) ) ] := NIL
                  RETURN .T.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN .F.

/* Try detection by header */
STATIC FUNCTION dep_try_header_detection( hbmk, dep )

   LOCAL aINCPATH
   LOCAL cDirOri
   LOCAL cDir
   LOCAL cFileName

   LOCAL tmp

   /* Check dependency include path list */

   IF ! dep[ _HBMKDEP_lFound ]
      FOR EACH aINCPATH IN { dep[ _HBMKDEP_aINCPATH ], ;
                             dep[ _HBMKDEP_aINCPATHLOCAL ] }
         FOR EACH cDirOri IN aINCPATH
            cDir := iif( aINCPATH:__enumIndex() == 1, dep[ _HBMKDEP_cINCROOT ], "" ) + cDirOri
            FOR EACH cFileName IN dep[ _HBMKDEP_aKeyHeader ]
               IF HeaderExists( cDir, cFileName ) != NIL
                  dep[ _HBMKDEP_cFound ] := hb_DirSepDel( hb_DirSepToOS( cDir ) )
                  hbmk[ _HBMK_hDEPTSDIR ][ dep[ _HBMKDEP_cFound ] ] := NIL
                  /* Adjust implib source names with component path */
                  FOR EACH tmp IN dep[ _HBMKDEP_aIMPLIBSRC ]
                     tmp := hb_PathNormalize( PathMakeAbsolute( tmp, hb_DirSepAdd( dep[ _HBMKDEP_cFound ] ) ) )
                  NEXT
                  dep[ _HBMKDEP_lFound ] := .T.
                  dep[ _HBMKDEP_lFoundLOCAL ] := ( aINCPATH:__enumIndex() == 2 )
                  IF hbmk[ _HBMK_lDEBUGDEPD ]
                     _hbmk_OutStd( hbmk, hb_StrFormat( "debugdepd: REQ %1$s: found by %2$s header at %3$s %4$s", dep[ _HBMKDEP_cName ], hb_DirSepToOS( cFileName ), dep[ _HBMKDEP_cFound ], iif( dep[ _HBMKDEP_lFoundLOCAL ], "(local)", "" ) ) )
                  ENDIF
                  AAddNew( hbmk[ _HBMK_aINCPATH ], hb_DirSepDel( hb_DirSepToOS( cDir ) ) )
                  AAdd( hbmk[ _HBMK_aOPTC ], "-D" + hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) )
                  hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) ] := NIL
                  hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_DIR_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) ] := hb_DirSepDel( hb_DirSepToOS( cDir ) )
                  IF dep[ _HBMKDEP_lFoundLOCAL ]
                     hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_HAS_TPL_LOCAL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) ] := NIL
                  ENDIF
                  RETURN .T.
               ENDIF
            NEXT
         NEXT
      NEXT
   ENDIF

   RETURN .F.

STATIC FUNCTION StrToDefine( cString )

   LOCAL cDefine := ""
   LOCAL c

   FOR EACH c IN hb_asciiUpper( cString )
      IF c $ "- "
         cDefine += "_"
      ELSEIF hb_asciiIsDigit( c ) .OR. hb_asciiIsAlpha( c ) .OR. c == "_"
         cDefine += c
      ENDIF
   NEXT

   RETURN cDefine

STATIC FUNCTION AMerge( aDst, aSrc )

   LOCAL item

   FOR EACH item IN aSrc
      IF hb_AScan( aDst, item,,, .T. ) == 0
         AAdd( aDst, item )
      ENDIF
   NEXT

   RETURN aDst

STATIC FUNCTION FindHeader( hbmk, cFileName, cParentDir, lSystemHeader, lSkipDept )

   LOCAL cDir
   LOCAL tmp

   IF ! lSystemHeader
      IF Empty( cParentDir )
         /* Check in current dir */
         IF hb_FileExists( tmp := hb_DirSepToOS( cFileName ) )
            RETURN tmp
         ENDIF
      ELSE
         /* Check in parent dir */
         tmp := hb_DirSepAdd( hb_DirSepToOS( cParentDir ) ) + hb_DirSepToOS( cFileName )
         IF hb_FileExists( tmp )
            RETURN tmp
         ENDIF
      ENDIF
   ENDIF

   /* Check in include path list specified via -incpath options */
   IF lSkipDept
      FOR EACH cDir IN hbmk[ _HBMK_aINCPATH ]
         IF !( cDir $ hbmk[ _HBMK_hDEPTSDIR ] )
            tmp := hb_DirSepAdd( hb_DirSepToOS( cDir ) ) + hb_DirSepToOS( cFileName )
            IF hb_FileExists( tmp )
               RETURN tmp
            ENDIF
         ENDIF
      NEXT
   ELSE
      FOR EACH cDir IN hbmk[ _HBMK_aINCPATH ]
         tmp := hb_DirSepAdd( hb_DirSepToOS( cDir ) ) + hb_DirSepToOS( cFileName )
         IF hb_FileExists( tmp )
            RETURN tmp
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

STATIC FUNCTION HeaderExists( cDir, cFileName )

   LOCAL tmp
#if defined( __PLATFORM__DARWIN )
   LOCAL nPos

   IF ( nPos := At( "/", cFileName ) ) > 0
      tmp := hb_DirSepAdd( hb_DirSepToOS( cDir ) ) + Left( cFileName, nPos - 1 ) + ".framework" + hb_ps() + "Headers" + hb_ps() + SubStr( cFileName, nPos + 1 )
      IF hb_FileExists( tmp )
         RETURN tmp
      ENDIF
   ENDIF
#endif
   tmp := hb_DirSepAdd( hb_DirSepToOS( cDir ) ) + hb_DirSepToOS( cFileName )

   RETURN iif( hb_FileExists( tmp ), tmp, NIL )

/* Replicating logic used by compilers. */

STATIC FUNCTION FindLib( hbmk, cLib, aLIBPATH, cLibPrefix, cLibExt )

   LOCAL cDir
   LOCAL tmp

   /* Check libs in their full paths */
   IF HBMK_ISCOMP( "msvc|msvc64|msvcarm|bcc|bcc64|pocc|pocc64|poccarm|watcom" )
      IF ! Empty( hb_FNameDir( cLib ) )
         IF hb_FileExists( cLib := hb_FNameExtSet( cLib, cLibExt ) )
            RETURN cLib
         ENDIF
         IF HBMK_ISCOMP( "pocc|pocc64|poccarm" )
            IF hb_FileExists( cLib := hb_FNameExtSet( cLib, ".a" ) )
               RETURN cLib
            ENDIF
         ENDIF
         RETURN NIL
      ENDIF
   ENDIF

   /* Check in current dir */
   IF HBMK_ISCOMP( "msvc|msvc64|msvcarm|bcc|bcc64|pocc|pocc64|poccarm|watcom" )
      IF ! Empty( tmp := LibExists( hbmk, "", cLib, cLibPrefix, cLibExt ) )
         RETURN tmp
      ENDIF
   ENDIF

   /* Check in libpaths */
   FOR EACH cDir IN aLIBPATH
      IF ! Empty( cDir )
         IF ! Empty( tmp := LibExists( hbmk, cDir, cLib, cLibPrefix, cLibExt ) )
            RETURN tmp
         ENDIF
      ENDIF
   NEXT

#if 0
   /* Check in certain other compiler specific locations. */
   IF HBMK_ISCOMP( "msvc|msvc64|msvcarm" )
      FOR EACH cDir IN hb_ATokens( GetEnv( "LIB" ), hb_osPathListSeparator(), .T., .T. )
         IF ! Empty( cDir )
            IF ! Empty( tmp := LibExists( hbmk, cDir, cLib, cLibPrefix, cLibExt ) )
               RETURN tmp
            ENDIF
         ENDIF
      NEXT
   ENDIF
#endif

   RETURN NIL

STATIC FUNCTION LibExists( hbmk, cDir, cLib, cLibPrefix, cLibExt )

   LOCAL tmp

   cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )

   DO CASE
   CASE HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|clang" ) .AND. HBMK_ISPLAT( "win|wce|cygwin" )
      /* NOTE: ld/gcc option -dll-search-prefix is not taken into account here,
               So, '<prefix>xxx.dll' format libs will not be found here in any case. */
      DO CASE
      CASE                                       hb_FileExists( tmp := cDir + "lib" + hb_FNameExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir +         hb_FNameExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir + "lib" + hb_FNameExtSet( cLib, ".a" )     ) ; RETURN tmp
      CASE hbmk[ _HBMK_cPLAT ] == "cygwin" .AND. hb_FileExists( tmp := cDir + "cyg" + hb_FNameExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir + "lib" + hb_FNameExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir +         hb_FNameExtSet( cLib, ".dll" )   ) ; RETURN tmp
      ENDCASE
   CASE hbmk[ _HBMK_cCOMP ] == "gcc" .AND. HBMK_ISPLAT( "linux|sunos|android" )
      DO CASE
      CASE                                       hb_FileExists( tmp := cDir + "lib" + hb_FNameExtSet( cLib, ".so" )    ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir + "lib" + hb_FNameExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   CASE HBMK_ISCOMP( "pocc|pocc64|poccarm" )
      DO CASE
      CASE                                       hb_FileExists( tmp := cDir +         hb_FNameExtSet( cLib, cLibExt )  ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir +         hb_FNameExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   OTHERWISE
      DO CASE
      CASE                                   hb_FileExists( tmp := cDir + cLibPrefix + hb_FNameExtSet( cLib, cLibExt ) ) ; RETURN tmp
      ENDCASE
   ENDCASE

   RETURN NIL

#if ! defined( __PLATFORM__UNIX )
STATIC FUNCTION FindInSamePath( cFileName, cFileName2, cPath )

   LOCAL cDir, cName, cExt

   cFileName := FindInPath( cFileName, cPath )

   IF ! Empty( cFileName )

      /* Look for the second filename in the same dir the first one was found. */

      hb_FNameSplit( cFileName, @cDir )
      hb_FNameSplit( cFileName2,, @cName, @cExt )

      #if defined( __PLATFORM__WINDOWS ) .OR. ;
          defined( __PLATFORM__DOS ) .OR. ;
          defined( __PLATFORM__OS2 )
         IF Empty( cExt )
            cExt := ".exe"
         ENDIF
      #endif

      IF hb_FileExists( cFileName := hb_FNameMerge( cDir, cName, cExt ) )
         RETURN cFileName
      ENDIF
   ENDIF

   RETURN NIL
#endif

STATIC PROCEDURE PlugIn_Load( hbmk, cFileName )

   LOCAL cFile
   LOCAL cExt
   LOCAL lOK
   LOCAL cType
   LOCAL hrb

   cFileName := hb_PathNormalize( cFileName )

   IF !( cFileName $ hbmk[ _HBMK_hPLUGINHRB ] )

      hrb := NIL

      hb_FNameSplit( cFileName, NIL, NIL, @cExt )

      cFile := hb_MemoRead( cFileName )

      IF ! Empty( cFile )
         lOK := .F.
         /* Optimization: Do not try to load it as .hrb if the extension is .prg, .hb (Harbour script) */
         IF !( Lower( cExt ) == ".prg" ) .AND. ;
            !( Lower( cExt ) == ".hb" )
            BEGIN SEQUENCE WITH {| oError | Break( oError ) }
               hrb := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
               cType := I_( "(compiled)" )
               lOK := .T.
            END /* SEQUENCE */
         ENDIF
         IF ! lOK .AND. !( Lower( cExt ) == ".hrb" ) /* Optimization: Do not try to load it as .prg if the extension is .hrb */
            cType := I_( "(source)" )
            /* We can use this function as this is a GPL licenced application */
            cFile := hb_compileFromBuf( cFile, "-n2", "-w3", "-es2", "-q0", "-i" + hbmk[ _HBMK_cHB_INSTALL_INC ], "-D" + _HBMK_PLUGIN )
            IF ! Empty( cFile )
               hrb := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
            ENDIF
         ENDIF
      ENDIF

      IF ! Empty( hrb )
         hbmk[ _HBMK_hPLUGINVars ][ cFileName ] := { => }
         IF ! PlugIn_call_low( hbmk, cFileName, hrb, PlugIn_make_ctx( hbmk, "init", hbmk[ _HBMK_hPLUGINVars ][ cFileName ] ) )
            /* Do not call plugin any further if initialization returned error */
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Initializing plugin: %1$s" ), cFileName ) )
            ENDIF
         ELSE
            hbmk[ _HBMK_hPLUGINHRB ][ cFileName ] := hrb
            IF hbmk[ _HBMK_lTRACE ]
               _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loaded plugin: %1$s %2$s" ), cFileName, cType ) )
            ENDIF
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lInfo ]
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Loading plugin: %1$s" ), cFileName ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

/* Public functions accessible for plugins */

#ifdef HB_LEGACY_LEVEL4
FUNCTION hbmk_FNameToSymbol( ... )    ; RETURN hbmk_FuncNameEncode( ... )
#endif

FUNCTION hbmk_FindInPath( ... )       ; RETURN FindInPath( ... )
FUNCTION hbmk_PathSepToForward( ... ) ; RETURN PathSepToForward( ... )
FUNCTION hbmk_FNameDirExtSet( ... )   ; RETURN FNameDirExtSet( ... )
FUNCTION hbmk_FuncNameEncode( ... )   ; RETURN FuncNameEncode( ... )
FUNCTION hbmk_StrStripQuote( ... )    ; RETURN StrStripQuote( ... )

FUNCTION hbmk_ArrayToList( array, cSeparator )

   LOCAL cString := ""
   LOCAL tmp

   IF HB_ISARRAY( array )
      hb_default( @cSeparator, " " )

      FOR tmp := 1 TO Len( array )
         IF HB_ISSTRING( array[ tmp ] )
            cString += array[ tmp ]
         ENDIF
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
   ENDIF

   RETURN cString

STATIC FUNCTION ctx_to_hbmk( ctx )

   LOCAL hbmk

   LOCAL cSecToken := hbmk_SecToken()

   IF HB_ISHASH( ctx ) .AND. cSecToken $ ctx
      hbmk := ctx[ cSecToken ]
      IF HB_ISARRAY( hbmk ) .AND. Len( hbmk ) == _HBMK_MAX_
         RETURN hbmk
      ENDIF
   ENDIF

   RETURN NIL

FUNCTION hbmk_FNameEscape( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk == NIL
#ifdef HB_LEGACY_LEVEL4
      /* legacy calling mode */
      IF HB_ISSTRING( ctx )
         RETURN FNameEscape( ctx, cFileName, hb_PValue( 3 ) )
      ENDIF
#endif
      RETURN NIL
   ENDIF

   RETURN FNameEscape( cFileName, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] )

FUNCTION hbmk_OutStdRaw( ctx, ... )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk == NIL .OR. hbmk[ _HBMK_lDumpInfo ]
      RETURN NIL
   ENDIF

   RETURN ( OutStd( ... ), OutStd( _OUT_EOL ) )

FUNCTION hbmk_OutErrRaw( ctx, ... )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk == NIL .OR. hbmk[ _HBMK_lDumpInfo ]
      RETURN NIL
   ENDIF

   RETURN ( OutErr( ... ), OutErr( _OUT_EOL ) )

FUNCTION hbmk_OutStd( ctx, cText )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL
      RETURN _hbmk_OutStd( hbmk, hb_StrFormat( I_( "plugin: %1$s" ), cText ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_OutErr( ctx, cText )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL
      RETURN _hbmk_OutErr( hbmk, hb_StrFormat( I_( "plugin: %1$s" ), cText ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_PathFromWorkdirToCWD( ctx )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( hbmk[ _HBMK_cWorkDir ] )
      RETURN hb_DirSepAdd( hb_PathRelativize( hb_PathNormalize( PathMakeAbsolute( hbmk[ _HBMK_cWorkDir ], hb_cwd() ) ), hb_cwd(), .T. ) )
   ENDIF

   RETURN ""

FUNCTION hbmk_Macro( ctx, cString )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cString )
      RETURN MacroProc( hbmk, cString )
   ENDIF

   RETURN ""

FUNCTION hbmk_PathSepToTarget( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      RETURN PathSepToTarget( hbmk, cFileName )
   ENDIF

   RETURN ""

FUNCTION hbmk_AddInput_PRG( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      AAdd( hbmk[ _HBMK_aPRG ], hb_DirSepToOS( cFileName ) )
      hb_default( @hbmk[ _HBMK_cFIRST ], hb_DirSepToOS( cFileName ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_AddInput_C( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      AAdd( hbmk[ _HBMK_aC ], hb_DirSepToOS( cFileName ) )
      hb_default( @hbmk[ _HBMK_cFIRST ], hb_DirSepToOS( cFileName ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_AddInput_CPP( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      AAdd( hbmk[ _HBMK_aCPP ], hb_DirSepToOS( cFileName ) )
      hb_default( @hbmk[ _HBMK_cFIRST ], hb_DirSepToOS( cFileName ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_AddInput_RC( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      AAdd( hbmk[ _HBMK_aRESSRC ], hb_DirSepToOS( cFileName ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_AddInput_OBJ( ctx, cFileName )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      AAdd( hbmk[ _HBMK_aOBJUSER ], hb_DirSepToOS( cFileName ) )
   ENDIF

   RETURN NIL

FUNCTION hbmk_AddInput_INSTFILE( ctx, cFileName, cGroup )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cFileName )
      hb_default( @cGroup, "" )
      AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { cGroup, hb_DirSepToOS( cFileName ) } )
   ENDIF

   RETURN NIL

FUNCTION hbmk_Register_Input_File_Extension( ctx, cExt )

   LOCAL hbmk := ctx_to_hbmk( ctx )

   IF hbmk != NIL .AND. HB_ISSTRING( cExt )
      IF ! Empty( cExt )
         IF !( Left( cExt, 1 ) == "." )
            cExt := "." + cExt
         ENDIF
         hbmk[ _HBMK_hPLUGINExt ][ Lower( cExt ) ] := NIL
      ENDIF
   ENDIF

   RETURN NIL

STATIC FUNCTION hbmk_SecToken()

   /* NOTE: Security token to protect against plugins accessing our
            internal structures referenced from context variable */

   STATIC s_cToken := NIL
   STATIC s_mutexToken := hb_mutexCreate()

   hb_mutexLock( s_mutexToken )

   IF s_cToken == NIL
      s_cToken := StrZero( hb_rand32(), 10, 0 )
   ENDIF

   hb_mutexUnlock( s_mutexToken )

   RETURN s_cToken

/* ; */

STATIC FUNCTION PlugIn_make_ctx( hbmk, cState, hVars )
   RETURN { ;
      "apiver"        => _HBMK_PLUGIN_APIVER        , ;
      "cSTATE"        => cState                     , ;
      "params"        => hbmk[ _HBMK_aPLUGINPars ]  , ;
      "vars"          => hVars                      , ;
      "cPLAT"         => hbmk[ _HBMK_cPLAT ]        , ;
      "cCOMP"         => hbmk[ _HBMK_cCOMP ]        , ;
      "nCOMPVer"      => hbmk[ _HBMK_nCOMPVer ]     , ;
      "cCPU"          => hbmk[ _HBMK_cCPU ]         , ;
      "cBUILD"        => hbmk[ _HBMK_cBUILD ]       , ;
      "cOUTPUTNAME"   => hbmk[ _HBMK_cPROGNAME ]    , ;
      "cTARGETNAME"   => hbmk_TARGETNAME( hbmk )    , ;
      "cTARGETTYPE"   => hbmk_TARGETTYPE( hbmk )    , ;
      "lREBUILD"      => hbmk[ _HBMK_lREBUILD ]     , ;
      "lCLEAN"        => hbmk[ _HBMK_lCLEAN ]       , ;
      "lDEBUG"        => hbmk[ _HBMK_lDEBUG ]       , ;
      "lMAP"          => hbmk[ _HBMK_lMAP ]         , ;
      "lSTRIP"        => hbmk[ _HBMK_lSTRIP ]       , ;
      "lDONTEXEC"     => hbmk[ _HBMK_lDONTEXEC ]    , ;
      "lIGNOREERROR"  => hbmk[ _HBMK_lIGNOREERROR ] , ;
      "lTRACE"        => hbmk[ _HBMK_lTRACE ]       , ;
      "lQUIET"        => hbmk[ _HBMK_lQuiet ]       , ;
      "lINFO"         => hbmk[ _HBMK_lInfo ]        , ;
      "lBEEP"         => hbmk[ _HBMK_lBEEP ]        , ;
      "lRUN"          => hbmk[ _HBMK_lRUN ]         , ;
      "lINC"          => hbmk[ _HBMK_lINC ]         , ;
      "cCCPATH"       => hbmk[ _HBMK_cCCPATH ]      , ;
      "cCCPREFIX"     => hbmk[ _HBMK_cCCPREFIX ]    , ;
      "cCCSUFFIX"     => hbmk[ _HBMK_cCCSUFFIX ]    , ;
      "cCCEXT"        => hbmk[ _HBMK_cCCEXT ]       , ;
      "nCmd_Esc"      => hbmk[ _HBMK_nCmd_Esc ]     , ; /* deprecated with HB_LEGACY_LEVEL4 */
      "nScr_Esc"      => hbmk[ _HBMK_nScr_Esc ]     , ; /* deprecated with HB_LEGACY_LEVEL4 */
      "nCmd_FNF"      => hbmk[ _HBMK_nCmd_FNF ]     , ; /* deprecated with HB_LEGACY_LEVEL4 */
      "nScr_FNF"      => hbmk[ _HBMK_nScr_FNF ]     , ; /* deprecated with HB_LEGACY_LEVEL4 */
      "cWorkDir"      => hbmk[ _HBMK_cWorkDir ]     , ;
      "nExitCode"     => hbmk[ _HBMK_nExitCode ]    , ;
      hbmk_SecToken() => hbmk                       }

STATIC FUNCTION PlugIn_ctx_get_state( ctx )
   RETURN ctx[ "cSTATE" ]

STATIC FUNCTION PlugIn_call_low( hbmk, cName, hrb, ctx )

   LOCAL xResult
   LOCAL oError
   LOCAL lSuccess := .T.

   BEGIN SEQUENCE WITH {| oError | oError:cargo := { ProcName( 1 ), ProcLine( 1 ) }, Break( oError ) }
      xResult := hb_hrbDo( hrb, ctx )
      IF ! Empty( xResult )
         IF hbmk[ _HBMK_lInfo ]
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Plugin %1$s returned at '%2$s': '%3$s'" ), cName, PlugIn_ctx_get_state( ctx ), hb_CStr( xResult ) ) )
         ENDIF
         IF ! hbmk[ _HBMK_lIGNOREERROR ]
            lSuccess := .F.
         ENDIF
      ENDIF
   RECOVER USING oError
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( e"Error: Executing plugin: %1$s at %3$s(%4$d)\n'%2$s'" ), cName, hbmk_ErrorMessage( oError ), oError:cargo[ 1 ], oError:cargo[ 2 ] ) )
      ENDIF
   END /* SEQUENCE */

   RETURN lSuccess

STATIC FUNCTION PlugIn_Execute_All( hbmk, cState )

   LOCAL hrb
   LOCAL ctx
   LOCAL lSuccess := .T.

   FOR EACH hrb IN hbmk[ _HBMK_hPLUGINHRB ]
      ctx := PlugIn_make_ctx( hbmk, cState, hbmk[ _HBMK_hPLUGINVars ][ hrb:__enumKey() ] )
      IF ! PlugIn_call_low( hbmk, hrb:__enumKey(), hrb, ctx )
         lSuccess := .F.
      ENDIF
   NEXT

   RETURN lSuccess

STATIC FUNCTION hbmk_ErrorMessage( oError )

   /* start error message */
   LOCAL cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   /* add subsystem name if available */
   cMessage += iif( HB_ISSTRING( oError:subsystem ), oError:subsystem(), "???" )

   /* add subsystem's error code if available */
   cMessage += "/" + iif( HB_ISNUMERIC( oError:subCode ), hb_ntos( oError:subCode ), "???" )

   /* add error description if available */
   IF HB_ISSTRING( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   /* add either filename or operation */
   DO CASE
   CASE ! Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE ! Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   RETURN cMessage

STATIC FUNCTION FindInPathPlugIn( /* @ */ cFileName )

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

   IF ! Empty( cDir )
      RETURN iif( hb_FileExists( cFileName ), cFileName, NIL )
   ENDIF

   IF Empty( cExt )
      cExt := ".hb"
   ENDIF

   cFileName := hb_FNameMerge( cDir, cName, cExt )

   RETURN FindInPath( cFileName )

STATIC FUNCTION FindInPath( cFileName, xPath, aExtDef )

   LOCAL cDir
   LOCAL cName
   LOCAL cExt
   LOCAL aExt

   IF HB_ISSTRING( cFileName )

      hb_FNameSplit( cFileName, @cDir, @cName, @cExt )
      #if defined( __PLATFORM__WINDOWS ) .OR. ;
          defined( __PLATFORM__DOS ) .OR. ;
          defined( __PLATFORM__OS2 )
         hb_default( @aExtDef, { ".exe" } )
      #else
         hb_default( @aExtDef, { cExt } )
      #endif
      aExt := { cExt }
      IF Empty( cExt )
         FOR EACH cExt IN aExtDef
            IF AScan( aExt, {| tmp | hb_FileMatch( tmp, cExt ) } ) == 0
               AAdd( aExt, cExt )
            ENDIF
         NEXT
      ENDIF

      FOR EACH cExt IN aExt
         /* Check original filename (in supplied path or current dir) */
         IF hb_FileExists( cFileName := hb_FNameMerge( cDir, cName, cExt ) )
            RETURN cFileName
         ENDIF
      NEXT

      IF Empty( cDir )
         /* Check in the dir of this executable. */
         IF ! Empty( cDir := hb_DirBase() )
            FOR EACH cExt IN aExt
               IF hb_FileExists( cFileName := hb_FNameMerge( cDir, cName, cExt ) )
                  RETURN cFileName
               ENDIF
            NEXT
         ENDIF

         IF ! HB_ISSTRING( xPath ) .AND. ;
            ! HB_ISARRAY( xPath )
            xPath := GetEnv( "PATH" )
         ENDIF

         IF HB_ISSTRING( xPath )
            #if defined( __PLATFORM__WINDOWS ) .OR. ;
                defined( __PLATFORM__DOS ) .OR. ;
                defined( __PLATFORM__OS2 )
               xPath := hb_ATokens( xPath, hb_osPathListSeparator(), .T., .T. )
            #else
               xPath := hb_ATokens( xPath, hb_osPathListSeparator() )
            #endif
         ENDIF

         FOR EACH cExt IN aExt
            /* Check in the PATH. */
            FOR EACH cDir IN xPath
               IF ! Empty( cDir := StrStripQuote( cDir ) )
                  IF hb_FileExists( cFileName := hb_FNameMerge( hb_DirSepAdd( cDir ), cName, cExt ) )
                     RETURN cFileName
                  ENDIF
               ENDIF
            NEXT
         NEXT
      ENDIF
   ENDIF

   RETURN NIL

#if 0
STATIC FUNCTION ArrayJoinNoClone( arraySrc1, arraySrc2 )

   LOCAL nLen1 := Len( arraySrc1 )

   ASize( arraySrc1, nLen1 + Len( arraySrc2 ) )

   RETURN ACopy( arraySrc2, arraySrc1, , , nLen1 + 1 )
#endif

STATIC FUNCTION ArrayJoin( arraySrc1, arraySrc2 )

   LOCAL arrayNew := AClone( arraySrc1 )
   LOCAL nLen1 := Len( arrayNew )

   ASize( arrayNew, nLen1 + Len( arraySrc2 ) )

   RETURN ACopy( arraySrc2, arrayNew, , , nLen1 + 1 )

STATIC FUNCTION ArrayAJoin( arrayList )

   LOCAL array := AClone( arrayList[ 1 ] )
   LOCAL tmp
   LOCAL nLenArray := Len( arrayList )
   LOCAL nLen
   LOCAL nPos := Len( array ) + 1

   nLen := 0
   FOR tmp := 1 TO nLenArray
      nLen += Len( arrayList[ tmp ] )
   NEXT

   ASize( array, nLen )

   FOR tmp := 2 TO nLenArray
      ACopy( arrayList[ tmp ], array, , , nPos )
      nPos += Len( arrayList[ tmp ] )
   NEXT

   RETURN array

STATIC FUNCTION ArraySplit( arrayIn, nChunksReq )

   LOCAL arrayOut
   LOCAL nChunkSize
   LOCAL nChunkPos
   LOCAL item

   IF nChunksReq > 0

      arrayOut := {}
      nChunkSize := Max( Round( Len( arrayIn ) / nChunksReq, 0 ), 1 )
      nChunkPos := 0

      FOR EACH item IN arrayIn
         IF nChunkPos == 0
            AAdd( arrayOut, {} )
         ENDIF
         AAdd( ATail( arrayOut ), item )
         IF ++nChunkPos == nChunkSize .AND. Len( arrayOut ) < nChunksReq
            nChunkPos := 0
         ENDIF
      NEXT
   ELSE
      arrayOut := { arrayIn }
   ENDIF

   RETURN arrayOut

STATIC FUNCTION AAddNewNotEmpty( array, xItem )

   IF ! Empty( xItem ) .AND. AScan( array, {| tmp | tmp == xItem } ) == 0
      AAdd( array, xItem )
   ENDIF

   RETURN array

#if 0
STATIC FUNCTION AAddNewAtTop( array, xItem )

   IF AScan( array, {| tmp | tmp == xItem } ) == 0
      hb_AIns( array, 1, xItem, .T. )
   ENDIF

   RETURN array
#endif

STATIC FUNCTION AAddNew( array, xItem )

   IF AScan( array, {| tmp | tmp == xItem } ) == 0
      AAdd( array, xItem )
   ENDIF

   RETURN array

STATIC FUNCTION AAddNewINST( array, xItem, lToTop )

   IF AScan( array, {| tmp | tmp[ 1 ] == xItem[ 1 ] .AND. tmp[ 2 ] == xItem[ 2 ] } ) == 0
      IF lToTop != NIL .AND. lToTop
         hb_AIns( array, 1, xItem, .T. )
      ELSE
         AAdd( array, xItem )
      ENDIF
   ENDIF

   RETURN array

STATIC FUNCTION AAddNotEmpty( array, xItem )

   IF ! Empty( xItem )
      AAdd( array, xItem )
   ENDIF

   RETURN array

#if 0
STATIC FUNCTION DepTreeToList( aTree )

   LOCAL aList := {}

   DepTreeWorker( aList, aTree )

   RETURN aList
#endif

STATIC PROCEDURE DepTreeWorker( aList, aTree )

   LOCAL xItem

   FOR EACH xItem IN aTree DESCEND
      IF HB_ISARRAY( xItem ) .AND. Len( xItem ) == 2
         DepTreeWorker( aList, xItem[ 2 ] )
         AAddNew( aList, xItem[ 1 ] )
      ELSE
         AAddNew( aList, xItem )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION ListDirExt( arraySrc, cDirNew, cExtNew, lStripClpAt )

   LOCAL array := AClone( arraySrc )
   LOCAL cFileName

   hb_default( @lStripClpAt, .F. )

   IF lStripClpAt
      FOR EACH cFileName IN array
         IF Left( cFileName, 1 ) == "@" .AND. ;
            Lower( hb_FNameExt( cFileName ) ) == ".clp"
            cFileName := FNameDirExtSet( SubStr( cFileName, 2 ), cDirNew, cExtNew )
         ELSE
            cFileName := FNameDirExtSet( cFileName, cDirNew, cExtNew )
         ENDIF
      NEXT
   ELSE
      FOR EACH cFileName IN array
         cFileName := FNameDirExtSet( cFileName, cDirNew, cExtNew )
      NEXT
   ENDIF

   RETURN array

/* Forms the list of libs as to appear on the command line */
STATIC FUNCTION ListCookLib( hbmk, aLIB, aLIBA, array, cPrefix, cExtNew )

   LOCAL cDir
   LOCAL cLibName
   LOCAL cLibNameCooked
   LOCAL cName, cExt

   IF HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|djgpp|gccomf|clang|open64" )
      FOR EACH cLibName IN array
         hb_FNameSplit( cLibName, @cDir )
         IF Empty( cDir )
            cLibNameCooked := cLibName
#if 0
            /* Do not attempt to strip this as it can be valid for libs which have double lib prefixes (f.e. libpng) */
            IF Left( cLibNameCooked, 3 ) == "lib"
               cLibNameCooked := SubStr( cLibNameCooked, 4 )
            ENDIF
#endif
            IF cPrefix != NIL
               cLibNameCooked := cPrefix + cLibNameCooked
            ENDIF
            IF cExtNew != NIL
               hb_FNameSplit( cLibNameCooked,, @cName, @cExt )
               /* Do not strip version number postfixes */
               IF hb_asciiIsDigit( SubStr( cExt, 2, 1 ) )
                  cLibNameCooked += cExtNew
               ELSE
                  cLibNameCooked := hb_FNameMerge(, cName, cExtNew )
               ENDIF
            ENDIF
            AAdd( aLIB, cLibNameCooked )
         ELSE
            AAdd( aLIBA, cLibName )
         ENDIF
      NEXT
   ELSE
      FOR EACH cLibName IN array
         IF cExtNew != NIL
            AAdd( aLIB, hb_FNameExtSet( cLibName, cExtNew ) )
         ELSE
            AAdd( aLIB, cLibName )
         ENDIF
      NEXT
   ENDIF

   RETURN array

STATIC FUNCTION FN_CookLib( cLibName, cPrefix, cExtNew )

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cLibName, @cDir, @cName, @cExt )

   IF cPrefix != NIL
      cName := cPrefix + cName
   ENDIF
   IF cExtNew != NIL
      cExt := cExtNew
   ENDIF

   RETURN hb_FNameMerge( cDir, cName, cExt )

/* Append optional prefix and optional extension to all members */
STATIC FUNCTION ListCook( arraySrc, cExtNew )

   LOCAL array := AClone( arraySrc )
   LOCAL cItem

   IF cExtNew != NIL
      FOR EACH cItem IN array
         cItem := hb_FNameExtSet( cItem, cExtNew )
      NEXT
   ENDIF

   RETURN array

STATIC FUNCTION ArrayToList( array, cSeparator, nEscapeMode, nFNNotation, cPrefix, cSuffix )

   LOCAL cString := ""
   LOCAL tmp

   hb_default( @cSeparator, " " )
   hb_default( @cPrefix, "" )
   hb_default( @cSuffix, "" )

   IF nEscapeMode == NIL .AND. nFNNotation == NIL
      FOR tmp := 1 TO Len( array )
         cString += cPrefix + array[ tmp ] + cSuffix
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
   ELSE
      FOR tmp := 1 TO Len( array )
         cString += cPrefix + FNameEscape( array[ tmp ], nEscapeMode, nFNNotation ) + cSuffix
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
   ENDIF

   RETURN cString

STATIC FUNCTION ListToArray( cList, cSep )

   LOCAL array := {}
   LOCAL cItem

   IF ! Empty( cList )
      FOR EACH cItem IN hb_ATokens( cList, cSep )
         AAddNotEmpty( array, cItem )
      NEXT
   ENDIF

   RETURN array

STATIC FUNCTION PathSepCount( cPath )

   LOCAL nCount := 0
   LOCAL c

   FOR EACH c IN cPath
      IF c == hb_ps()
         ++nCount
      ENDIF
   NEXT

   RETURN nCount

STATIC FUNCTION PathSepToForward( cFileName )
   RETURN iif( HB_ISSTRING( cFileName ), StrTran( cFileName, "\", "/" ), "" )

STATIC FUNCTION hbmk_hb_DirSepToOS( cFileName, nStart )
   RETURN iif( nStart == NIL, StrTran( cFileName, iif( hb_ps() == "\", "/", "\" ), hb_ps() ), ;
                              Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), iif( hb_ps() == "\", "/", "\" ), hb_ps() ) )

STATIC FUNCTION PathSepToTarget( hbmk, cFileName, nStart )

   hb_default( @nStart, 1 )

   IF HBMK_ISPLAT( "win|wce|dos|os2" ) .AND. ! HBMK_ISCOMP( "mingw|mingw64|mingwarm|clang" )
      RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "/", "\" )
   ENDIF

   RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "\", "/" )

STATIC FUNCTION FNameEscape( cFileName, nEscapeMode, nFNNotation )

   LOCAL cDir, cName, cExt, cDrive

   hb_default( @nEscapeMode, _ESC_NONE )

#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
   hb_default( @nFNNotation, _FNF_BCKSLASH )
#else
   hb_default( @nFNNotation, _FNF_FWDSLASH )
#endif

   SWITCH nFNNotation
   CASE _FNF_BCKSLASH
      cFileName := StrTran( cFileName, "/", "\" )
      EXIT
   CASE _FNF_FWDSLASH
      cFileName := StrTran( cFileName, "\", "/" )
      EXIT
   CASE _FNF_FWDSLASHCYGWIN
      hb_FNameSplit( cFileName, @cDir, @cName, @cExt, @cDrive )
      IF ! Empty( cDrive )
         cDir := SubStr( cDir, Len( cDrive + hb_osDriveSeparator() ) + 1 )
         IF Left( cDir, Len( hb_ps() ) ) == hb_ps()
            cDir := SubStr( cDir, Len( hb_ps() ) + 1 )
         ENDIF
         cDir := "/cygdrive/" + Lower( Left( cDrive, 1 ) ) + "/" + cDir
         cFileName := hb_FNameMerge( cDir, cName, cExt )
      ENDIF
      cFileName := StrTran( cFileName, "\", "/" )
      EXIT
   CASE _FNF_FWDSLASHMSYS
      hb_FNameSplit( cFileName, @cDir, @cName, @cExt, @cDrive )
      IF ! Empty( cDrive )
         cDir := SubStr( cDir, Len( cDrive + hb_osDriveSeparator() ) + 1 )
         IF Left( cDir, Len( hb_ps() ) ) == hb_ps()
            cDir := SubStr( cDir, Len( hb_ps() ) + 1 )
         ENDIF
         cDir := "/" + Lower( Left( cDrive, 1 ) ) + "/" + cDir
         cFileName := hb_FNameMerge( cDir, cName, cExt )
      ENDIF
      cFileName := StrTran( cFileName, "\", "/" )
      EXIT
   ENDSWITCH

   SWITCH nEscapeMode
   CASE _ESC_DBLQUOTE
      IF " " $ cFileName .OR. "-" $ cFileName
         /* Sloppy */
         IF Right( cFileName, 1 ) == "\"
            cFileName += "\"
         ENDIF
         RETURN '"' + cFileName + '"'
      ENDIF
      EXIT
   CASE _ESC_SGLQUOTE_WATCOM
      IF " " $ cFileName
         /* Sloppy */
         IF Right( cFileName, 1 ) == "\"
            cFileName += "\"
         ENDIF
         RETURN "'" + cFileName + "'"
      ENDIF
      EXIT
   CASE _ESC_NIX
      IF StrHasSpecialChar( cFileName )
         cFileName := "'" + StrTran( cFileName, "'", "'\''" ) + "'"
      ENDIF
      EXIT
   CASE _ESC_BCKSLASH
      cFileName := StrTran( cFileName, "\", "\\" )
      EXIT
   ENDSWITCH

   RETURN cFileName

STATIC FUNCTION StrHasSpecialChar( cString )

   LOCAL c

   FOR EACH c IN cString
      IF !( hb_asciiIsAlpha( c ) .OR. hb_asciiIsDigit( c ) .OR. c $ "/." )
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

/* Remove all extensions from name */
STATIC FUNCTION FNameNameGetNoExt( cFileName )

   LOCAL cName := cFileName

   hb_FNameSplit( cFileName,, @cName )

   WHILE ! Empty( cName ) .AND. ! Empty( hb_FNameExt( cName ) )
      hb_FNameSplit( cName,, @cName )
   ENDDO

   RETURN cName

STATIC FUNCTION FNameDirExtSet( cFileName, cDirNew, cExtNew )

   LOCAL cDir, cName, cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

   IF HB_ISSTRING( cDirNew )
      cDir := cDirNew
   ENDIF
   IF HB_ISSTRING( cExtNew )
      cExt := cExtNew
   ENDIF

   RETURN hb_FNameMerge( cDir, cName, cExt )

STATIC FUNCTION FNameDirName( cFileName )

   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName )

STATIC FUNCTION FN_Expand( cFileName, lCommandLine )

   LOCAL aFileList
   LOCAL aFile

   IF Empty( cFileName )
      RETURN {}
   ENDIF

#if defined( __PLATFORM__UNIX )
   /* Disable expansion if this came from the command line */
   IF lCommandLine
      RETURN { cFileName }
   ENDIF
#else
   HB_SYMBOL_UNUSED( lCommandLine )
#endif

   IF ! FNameHasWildcard( cFileName )
      RETURN { cFileName }
   ENDIF

   aFileList := {}

   FOR EACH aFile IN Directory( cFileName )
      AAdd( aFilelist, hb_FNameMerge( hb_FNameDir( cFileName ), aFile[ F_NAME ] ) )
   NEXT

   RETURN aFileList

STATIC FUNCTION FNameHasWildcard( cFileName )
   RETURN ;
      "?" $ cFileName .OR. ;
      "*" $ cFileName

STATIC FUNCTION HBC_Find( hbmk, cFile, nNesting )

   LOCAL cLibPath
   LOCAL lFound

   LOCAL cDir
   LOCAL aFile
   LOCAL tmp

   hb_default( @nNesting, 1 )

   lFound := .F.
   IF hb_FileExists( cFile )
      lFound := .T.
   ELSE
      FOR EACH cLibPath IN hbmk[ _HBMK_aLIBPATH ]
         IF hb_FileExists( hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, cLibPath, cFile, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cFile ) )
            cFile := hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, cLibPath, cFile, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cFile )
            lFound := .T.
            EXIT
         ENDIF
      NEXT

      IF ! lFound

         FOR EACH cDir IN { ;
            hbmk[ _HBMK_cHB_INSTALL_CON ], ;
            hbmk[ _HBMK_cHB_INSTALL_ADD ] }

            FOR EACH aFile IN Directory( hb_DirSepAdd( cDir ), "D" )
               IF "D" $ aFile[ F_ATTR ] .AND. !( aFile[ F_NAME ] == "." ) .AND. !( aFile[ F_NAME ] == ".." ) .AND. ;
                  hb_FileExists( tmp := hb_DirSepAdd( cDir ) + aFile[ F_NAME ] + hb_ps() + hb_FNameNameExt( cFile ) )

                  cFile := tmp
                  lFound := .T.
                  EXIT
               ENDIF
            NEXT
            IF lFound
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   IF lFound
      RETURN HBC_ProcessOne( hbmk, hb_PathNormalize( cFile ), nNesting )
   ENDIF

   RETURN ""

/* shell dependent, but let's assume the platform default is used */
STATIC FUNCTION EnvNotation( cEnvName )
#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
   RETURN "%" + cEnvName + "%"
#else
   RETURN "$" + cEnvName
#endif

STATIC FUNCTION AutoConfPathList( lCWD, lForDocOutput )

   LOCAL aPath := {}

   /* It will form the output so that it does not contain
      configuration specific (potentially sensitive)
      information by using generic term. */
   hb_default( @lForDocOutput, .F. )

   IF lCWD
      AAdd( aPath, "." + hb_ps() )
   ENDIF

   IF ! Empty( GetEnv( _OSCONFDIR_ENV_ ) )
      IF lForDocOutput
         /* QUESTION: How to document home directory in a multiplatform fashion? */
         AAdd( aPath, hb_DirSepAdd( EnvNotation( _OSCONFDIR_ENV_ ) ) + _CONFDIR_BASE_ )
      ELSE
         AAdd( aPath, hb_DirSepAdd( GetEnv( _OSCONFDIR_ENV_ ) ) + _CONFDIR_BASE_ )
      ENDIF
   ENDIF

#if defined( __PLATFORM__UNIX )
   AAdd( aPath, "/etc/harbour" )
   IF lForDocOutput
      AAdd( aPath, hb_StrFormat( I_( "<%1$s directory>" ), _SELF_NAME_ ) + hb_ps() + "../etc/harbour" )
      AAdd( aPath, hb_StrFormat( I_( "<%1$s directory>" ), _SELF_NAME_ ) + hb_ps() + "../etc" )
   ELSE
      AAdd( aPath, hb_DirSepAdd( hb_DirBase() ) + "../etc/harbour" )
      AAdd( aPath, hb_DirSepAdd( hb_DirBase() ) + "../etc" )
   ENDIF
#endif

   IF lForDocOutput
      AAdd( aPath, hb_StrFormat( I_( "<%1$s directory>" ), _SELF_NAME_ ) )
   ELSE
      AAdd( aPath, hb_DirBase() )
   ENDIF

   RETURN aPath

STATIC PROCEDURE HBC_ProcessAuto( hbmk )

   LOCAL cDir
   LOCAL cFileName

   FOR EACH cDir IN AutoConfPathList( .F. )
      IF hb_FileExists( cFileName := ( hb_PathNormalize( hb_DirSepAdd( cDir ) ) + _HBMK_AUTOHBC_NAME ) )
         IF ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing configuration: %1$s" ), cFileName ) )
         ENDIF
         HBC_ProcessOne( hbmk, cFileName, 1 )
         EXIT
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION HBC_ProcessOne( hbmk, cFileName, nNestingLevel )

   LOCAL cFile
   LOCAL cLine
   LOCAL cLineOri
   LOCAL cItem
   LOCAL cItemL
   LOCAL cName
   LOCAL lFound
   LOCAL tmp, tmp1

   LOCAL nVersion
   LOCAL cVersion

   IF hbmk[ _HBMK_lInfo ]
      _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cFileName ) )
   ENDIF

   IF ! hbmk_hb_FileExists( cFileName )
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Opening: %1$s" ), cFileName ) )
      RETURN ""
   ENDIF

   nVersion := 1

   AAddNew( hbmk[ _HBMK_aDEPTHBC ], { cFileName, nNestingLevel - 1 } )

   cFile := hbmk_MemoRead( cFileName ) /* NOTE: Intentionally using hbmk_MemoRead() which handles EOF char. */

   IF !( hb_eol() == _CHR_EOL )
      cFile := StrTran( cFile, hb_eol(), _CHR_EOL )
   ENDIF
   IF !( hb_eol() == Chr( 13 ) + Chr( 10 ) )
      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _CHR_EOL )
   ENDIF

   FOR EACH cLine IN hb_ATokens( cFile, _CHR_EOL )

      cLineOri := cLine := AllTrim( ArchCompFilter( hbmk, AllTrim( cLine ), cFileName ) )

      #define _PAR_NEW_HBC()  _PAR_NEW( cLineOri, cFileName, cLine:__enumIndex() )

      DO CASE
      CASE Lower( Left( cLine, Len( "skip="         ) ) ) == "skip="         ; cLine := SubStr( cLine, Len( "skip="         ) + 1 )

         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + _OUT_EOL )
         ENDIF

         IF hbmk[ _HBMK_lInfo ]
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Skipping from: %1$s" ), cFileName ) )
         ENDIF
         EXIT

      CASE Lower( Left( cLine, Len( "sources="      ) ) ) == "sources="      ; cLine := SubStr( cLine, Len( "sources="      ) + 1 )

         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               cItem := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) ) )
               cItemL := Lower( cItem )
               DO CASE
               CASE hb_FNameExt( cItemL ) == ".o" .OR. ;
                    hb_FNameExt( cItemL ) == ".obj"
                  AAddNew( hbmk[ _HBMK_aOBJUSER ], cItem )
               CASE hb_FNameExt( cItemL ) == ".cpp" .OR. ;
                    hb_FNameExt( cItemL ) == ".cc" .OR. ;
                    hb_FNameExt( cItemL ) == ".cxx" .OR. ;
                    hb_FNameExt( cItemL ) == ".cx"
                  AAddNew( hbmk[ _HBMK_aCPP ], cItem )
               CASE hb_FNameExt( cItemL ) == ".c" .OR. ;
                    hb_FNameExt( cItemL ) == ".m"
                  AAddNew( hbmk[ _HBMK_aC ], cItem )
               CASE hb_FNameExt( cItemL ) == ".d"
                  deplst_read( hbmk, hbmk[ _HBMK_hDEPTS ], cItem )
               CASE hb_FNameExt( cItemL ) == ".po" .OR. ;
                    hb_FNameExt( cItemL ) == ".pot"
                  AAddNew( hbmk[ _HBMK_aPO ], cItem )
               CASE hb_FNameExt( cItemL ) == ".rc"
                  FOR EACH tmp IN FN_Expand( cItem, .F. )
                     AAddNew( hbmk[ _HBMK_aRESSRC ], tmp )
                  NEXT
               CASE hb_FNameExt( cItemL ) == ".def"
                  FOR EACH tmp IN FN_Expand( cItem, .F. )
                     AAddNew( hbmk[ _HBMK_aDEF ], tmp )
                  NEXT
               CASE hb_FNameExt( cItemL ) == ".res"
                  IF HBMK_ISCOMP( "mingw|mingw64|mingwarm" ) .OR. ;
                     ( hbmk[ _HBMK_cPLAT ] == "win" .AND. HBMK_ISCOMP( "clang" ) ) .OR. ;
                     ( hbmk[ _HBMK_cPLAT ] == "os2" .AND. HBMK_ISCOMP( "gcc|gccomf" ) )
                     /* For MinGW/EMX GCC family add .res files as source input, as they
                        will need to be converted to coff format with windres (just
                        like plain .rc files) before feeding them to gcc. */
                     FOR EACH tmp IN FN_Expand( cItem, .F. )
                        AAddNew( hbmk[ _HBMK_aRESSRC ], tmp )
                     NEXT
                  ELSE
                     FOR EACH tmp IN FN_Expand( cItem, .F. )
                        AAddNew( hbmk[ _HBMK_aRESCMP ], tmp )
                     NEXT
                  ENDIF
               CASE hb_FNameExt( cItemL ) $ hbmk[ _HBMK_hPLUGINExt ]
                  FOR EACH tmp IN FN_Expand( cItem, .F. )
                     AAddNew( hbmk[ _HBMK_aPLUGINPars ], tmp )
                  NEXT
               CASE hb_FNameExt( cItemL ) == ".ch"
                  FOR EACH tmp IN FN_Expand( cItem, .F. )
                     AAddNew( hbmk[ _HBMK_aCH ], tmp )
                  NEXT
               OTHERWISE /* .prg */
                  IF Empty( hb_FNameExt( cItem ) )
                     cItem := hb_FNameExtSet( cItem, ".prg" )
                  ENDIF
                  AAddNew( hbmk[ _HBMK_aPRG ], cItem )
               ENDCASE
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "headers="      ) ) ) == "headers="      ; cLine := SubStr( cLine, Len( "headers="      ) + 1 )

         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               FOR EACH tmp IN FN_Expand( hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) ) ), .F. )
                  cItemL := Lower( cItem )
                  DO CASE
                  CASE hb_FNameExt( cItemL ) == ".h" .OR. ;
                       hb_FNameExt( cItemL ) == ".hpp" .OR. ;
                       hb_FNameExt( cItemL ) == ".hh" .OR. ;
                       hb_FNameExt( cItemL ) == ".mm" .OR. ;
                       Empty( hb_FNameExt( cItemL ) )
                     /* ignore C/C++/Objective C headers */
                  OTHERWISE
                     AAddNew( hbmk[ _HBMK_aCH ], tmp )
                  ENDCASE
               NEXT
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libs="         ) ) ) == "libs="         ; cLine := SubStr( cLine, Len( "libs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := tmp1 := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF hb_FNameExt( cItem ) == ".hbc"
               cItem := PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) )
               IF nNestingLevel < _HBMK_NEST_MAX
                  IF Empty( HBC_Find( hbmk, cItem, nNestingLevel + 1 ) )
                     _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot find %1$s (referenced from %2$s)" ), tmp1, cFileName ) )
                  ENDIF
               ELSE
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
               ENDIF
            ELSE
               cItem := hb_DirSepToOS( cItem )
               IF Left( cItem, 1 ) == "-"
                  IF CheckParamLib( hbmk, SubStr( cItem, 2 ), .T., _PAR_NEW_HBC() )
                     AAddNewNotEmpty( hbmk[ _HBMK_aLIBFILTEROUT ], SubStr( cItem, 2 ) )
                  ENDIF
               ELSE
                  IF CheckParamLib( hbmk, cItem, .T., _PAR_NEW_HBC() )
                     IF _IS_AUTOLIBSYSPRE( cItem )
                        AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSERSYSPRE ], cItem )
                     ELSE
                        AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSER ], cItem )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "frameworks="   ) ) ) == "frameworks="   ; cLine := SubStr( cLine, Len( "frameworks="   ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSERFWK ], hb_FNameExtSet( hb_DirSepToOS( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) ) ) )
         NEXT

      CASE Lower( Left( cLine, Len( "requests="     ) ) ) == "requests="     ; cLine := SubStr( cLine, Len( "requests="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF IsValidHarbourID( cItem )
               AAddNew( hbmk[ _HBMK_aREQUEST ], cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "syslibs="      ) ) ) == "syslibs="      ; cLine := SubStr( cLine, Len( "syslibs="      ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSERSYS ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "hbcs="         ) ) ) == "hbcs="         ; cLine := SubStr( cLine, Len( "hbcs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            IF nNestingLevel < _HBMK_NEST_MAX

               cItem := tmp1 := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
               cItem := PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) )

               IF Empty( hb_FNameExt( cItem ) )
                  cItem := hb_FNameExtSet( cItem, ".hbc" )
               ENDIF

               IF Empty( HBC_Find( hbmk, cItem, nNestingLevel + 1 ) )
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot find %1$s (referenced from %2$s)" ), tmp1, cFileName ) )
               ENDIF
            ELSE
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "autohbcs="     ) ) ) == "autohbcs="     ; cLine := SubStr( cLine, Len( "autohbcs="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF autohbc_split_arg( cItem, @cName, @cItem )

               cItem := tmp1 := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
               cItem := PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) )

               IF Empty( hb_FNameExt( cName ) )
                  cName := hb_FNameExtSet( cName, ".ch" )
               ENDIF
               IF Empty( hb_FNameExt( cItem ) )
                  cItem := hb_FNameExtSet( cItem, ".hbc" )
               ENDIF

               lFound := .F.
               IF hb_FileExists( cItem )
                  lFound := .T.
               ELSE
                  FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                     IF hb_FileExists( hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cItem ) )
                        cItem := hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cItem )
                        lFound := .T.
                        EXIT
                     ENDIF
                  NEXT
               ENDIF

               IF lFound
                  hbmk[ _HBMK_hAUTOHBC ][ AllTrim( StrTran( cName, "\", "/" ) ) ] := cItem
               ELSE
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot find %1$s (referenced from %2$s)" ), tmp1, cFileName ) )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libpaths="     ) ) ) == "libpaths="     ; cLine := SubStr( cLine, Len( "libpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               cItem := hb_DirSepDel( hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) ) ) )
               IF CheckParamLibPath( hbmk, cItem )
                  IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ cItem .OR. hb_DirExists( cItem )
                     AAddNew( hbmk[ _HBMK_aLIBPATH ], cItem )
                  ENDIF
               ELSE
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring explicitly specified core library directory: %1$s (in directive %2$s)" ), cItem, ParamToString( _PAR_NEW_HBC() ) ) )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "incpaths="     ) ) ) == "incpaths="     ; cLine := SubStr( cLine, Len( "incpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               IF CheckParamInc( hbmk, cItem )
                  AAddNew( hbmk[ _HBMK_aINCPATH ], hb_DirSepDel( hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) ) ) ) )
               ELSE
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Ignoring explicitly specified core header directory: %1$s (in directive %2$s)" ), cItem, ParamToString( _PAR_NEW_HBC() ) ) )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "instfiles="    ) ) ) == "instfiles="    ; cLine := SubStr( cLine, Len( "instfiles="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF inst_split_arg( cItem, @cName, @cItem )
               cItem := hb_PathNormalize( PathMakeAbsolute( cItem, hb_FNameDir( cFileName ) ) )
               FOR EACH tmp IN FN_Expand( cItem, .F. )
                  AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { cName, tmp } )
               NEXT
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "instpaths="    ) ) ) == "instpaths="    ; cLine := SubStr( cLine, Len( "instpaths="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF inst_split_arg( cItem, @cName, @cItem )
               AAddNewINST( hbmk[ _HBMK_aINSTPATH ], { cName, hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) ) ) } )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "echo="         ) ) ) == "echo="         ; cLine := SubStr( cLine, Len( "echo="         ) + 1 )
         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + _OUT_EOL )
         ENDIF

      CASE Lower( Left( cLine, Len( "stop="         ) ) ) == "stop="         ; cLine := SubStr( cLine, Len( "stop="         ) + 1 )

         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + _OUT_EOL )
         ENDIF

         hbmk[ _HBMK_lStopAfterInit ] := .T.
         hbmk[ _HBMK_lRUN ] := .F.
         hbmk[ _HBMK_nExitCode ] := _EXIT_STOP
         EXIT

      CASE Lower( Left( cLine, Len( "prgflags="     ) ) ) == "prgflags="     ; cLine := SubStr( cLine, Len( "prgflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aOPTPRG ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "cflags="       ) ) ) == "cflags="       ; cLine := SubStr( cLine, Len( "cflags="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aOPTC ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "resflags="     ) ) ) == "resflags="     ; cLine := SubStr( cLine, Len( "resflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aOPTRES ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "ldflags="      ) ) ) == "ldflags="      ; cLine := SubStr( cLine, Len( "ldflags="      ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTL ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), _PAR_NEW_HBC(), .T. )
         NEXT

      CASE Lower( Left( cLine, Len( "ldflags+="     ) ) ) == "ldflags+="     ; cLine := SubStr( cLine, Len( "ldflags+="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTLPOST ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), _PAR_NEW_HBC(), .T. )
         NEXT

      CASE Lower( Left( cLine, Len( "dflags="       ) ) ) == "dflags="       ; cLine := SubStr( cLine, Len( "dflags="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTD ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), _PAR_NEW_HBC(), .T. )
         NEXT

      CASE Lower( Left( cLine, Len( "dflags+="      ) ) ) == "dflags+="      ; cLine := SubStr( cLine, Len( "dflags+="      ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddWithWarning( hbmk, hbmk[ _HBMK_aOPTDPOST ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), _PAR_NEW_HBC(), .T. )
         NEXT

      CASE Lower( Left( cLine, Len( "pflags="       ) ) ) == "pflags="       ; cLine := SubStr( cLine, Len( "pflags="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aPLUGINPars ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "psources="     ) ) ) == "psources="     ; cLine := SubStr( cLine, Len( "psources="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               AAddNew( hbmk[ _HBMK_aPLUGINPars ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( cItem ), hb_FNameDir( cFileName ) ) ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "gui="          ) ) ) == "gui="          ; cLine := SubStr( cLine, Len( "gui="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lGUI ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lGUI ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "mt="           ) ) ) == "mt="           ; cLine := SubStr( cLine, Len( "mt="           ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lMT ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lMT ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "pic="          ) ) ) == "pic="          ; cLine := SubStr( cLine, Len( "pic="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lPIC ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lPIC ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "shareddef="    ) ) ) == "shareddef="    ; cLine := SubStr( cLine, Len( "shareddef="    ) + 1 )
         IF hbmk[ _HBMK_lSHARED ] == NIL
            DO CASE
            CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSHARED ] := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
            CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
            OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
            ENDCASE
         ENDIF

      CASE Lower( Left( cLine, Len( "shared="       ) ) ) == "shared="       ; cLine := SubStr( cLine, Len( "shared="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSHARED ] := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "fullstatic="   ) ) ) == "fullstatic="   ; cLine := SubStr( cLine, Len( "fullstatic="   ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "debug="        ) ) ) == "debug="        ; cLine := SubStr( cLine, Len( "debug="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lDEBUG ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lDEBUG ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "optim="        ) ) ) == "optim="        ; cLine := SubStr( cLine, Len( "optim="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lOPTIM ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lOPTIM ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "nulrdd="       ) ) ) == "nulrdd="       ; cLine := SubStr( cLine, Len( "nulrdd="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lNULRDD ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lNULRDD ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "nodefgt="      ) ) ) == "nodefgt="      ; cLine := SubStr( cLine, Len( "nodefgt="      ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_aLIBCOREGT ] := {}
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_aLIBCOREGT ] := hbmk[ _HBMK_aLIBCOREGTDEF ]
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "map="          ) ) ) == "map="          ; cLine := SubStr( cLine, Len( "map="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lMAP ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lMAP ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "hbcppmm="      ) ) ) == "hbcppmm="      ; cLine := SubStr( cLine, Len( "hbcppmm="      ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lHBCPPMM ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lHBCPPMM ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "implib="       ) ) ) == "implib="       ; cLine := SubStr( cLine, Len( "implib="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lIMPLIB ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lIMPLIB ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "winuni="       ) ) ) == "winuni="       ; cLine := SubStr( cLine, Len( "winuni="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lWINUNI ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lWINUNI ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "strip="        ) ) ) == "strip="        ; cLine := SubStr( cLine, Len( "strip="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSTRIP ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSTRIP ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "cpp="          ) ) ) == "cpp="          ; cLine := SubStr( cLine, Len( "cpp="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )       ; hbmk[ _HBMK_lCPP ] := .T.
         CASE ValueIsF( cLine )       ; hbmk[ _HBMK_lCPP ] := .F.
         CASE Lower( cLine ) == "def" ; hbmk[ _HBMK_lCPP ] := NIL
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "warn="         ) ) ) == "warn="         ; cLine := SubStr( cLine, Len( "warn="         ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )        ; hbmk[ _HBMK_nWARN ] := _WARN_YES
         CASE ValueIsF( cLine )        ; hbmk[ _HBMK_nWARN ] := _WARN_NO
         CASE Lower( cLine ) == "low"  ; hbmk[ _HBMK_nWARN ] := _WARN_LOW
         CASE Lower( cLine ) == "max"  ; hbmk[ _HBMK_nWARN ] := _WARN_MAX
         CASE Lower( cLine ) == "def"  ; hbmk[ _HBMK_nWARN ] := _WARN_DEF
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "safe="         ) ) ) == "safe="         ; cLine := SubStr( cLine, Len( "safe="         ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )        ; hbmk[ _HBMK_lSAFE ] := .T.
         CASE ValueIsF( cLine )        ; hbmk[ _HBMK_lSAFE ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "compr="        ) ) ) == "compr="        ; cLine := SubStr( cLine, Len( "compr="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )       ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
         CASE ValueIsF( cLine )       ; hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF
#ifdef HB_LEGACY_LEVEL4
         CASE Lower( cLine ) == "def" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
#endif
         CASE Lower( cLine ) == "min" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MIN
         CASE Lower( cLine ) == "max" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MAX
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "head="         ) ) ) == "head="         ; cLine := SubStr( cLine, Len( "head="         ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "off"     ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF
         CASE Lower( cLine ) == "full"    ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
         CASE Lower( cLine ) == "native"  ; hbmk[ _HBMK_nHEAD ] := _HEAD_NATIVE
         CASE Lower( cLine ) == "dep"     ; hbmk[ _HBMK_nHEAD ] := _HEAD_DEP
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "run="          ) ) ) == "run="          ; cLine := SubStr( cLine, Len( "run="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lRUN ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lRUN ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "inc="          ) ) ) == "inc="          ; cLine := SubStr( cLine, Len( "inc="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lINC ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lINC ] := .F.
         OTHERWISE ; InvalidOptionValue( hbmk, _PAR_NEW_HBC() )
         ENDCASE

      CASE Lower( Left( cLine, Len( "plugins="      ) ) ) == "plugins="      ; cLine := SubStr( cLine, Len( "plugins="      ) + 1 )

         cLine := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cLine, cFileName ) ), hb_FNameDir( cFileName ) ) )
         IF ( tmp := FindInPathPlugIn( cLine ) ) != NIL
            PlugIn_Load( hbmk, tmp )
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Plugin not found: %1$s" ), cLine ) )
            ENDIF
         ENDIF

      /* NOTE: This keyword is used to signal the default GT used when
               building Harbour. It only needs to be filled if this default
               GT is different from the Harbour default one, IOW when it
               was overridden by user at Harbour build time. [vszakats] */
      CASE Lower( Left( cLine, Len( "gtdef="        ) ) ) == "gtdef="        ; cLine := SubStr( cLine, Len( "gtdef="        ) + 1 )
         IF ! Empty( cLine )
            IF ! SetupForGT( cLine, @hbmk[ _HBMK_cGTDEFAULT ], @hbmk[ _HBMK_lGUI ] )
               cLine := NIL
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], hb_DirSepToOS( cLine ) )
               ENDIF
               IF AScan( hbmk[ _HBMK_aGT ], {| tmp | Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aGT ], hb_DirSepToOS( cLine ) )
               ENDIF
            ENDIF
         ENDIF

      CASE Lower( Left( cLine, Len( "gt="           ) ) ) == "gt="           ; cLine := SubStr( cLine, Len( "gt="           ) + 1 )
         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            IF hbmk[ _HBMK_cGT ] == NIL
               IF ! SetupForGT( cLine, @hbmk[ _HBMK_cGT ], @hbmk[ _HBMK_lGUI ] )
                  cLine := NIL
               ENDIF
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], hb_DirSepToOS( cLine ) )
               ENDIF
               IF AScan( hbmk[ _HBMK_aGT ], {| tmp | Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aGT ], hb_DirSepToOS( cLine ) )
               ENDIF
            ENDIF
         ENDIF

      CASE Lower( Left( cLine, Len( "env="          ) ) ) == "env="          ; cLine := SubStr( cLine, Len( "env="          ) + 1 )

         ProcEnvOption( cLine )

      CASE Lower( Left( cLine, Len( "deppkgname="   ) ) ) == "deppkgname="   ; cLine := SubStr( cLine, Len( "deppkgname="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            cLine := MacroProc( hbmk, cLine, cFileName )
            AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aPKG ], StrStripQuote( AllTrim( cLine ) ) )
         ENDIF

      CASE Lower( Left( cLine, Len( "depkeyhead="   ) ) ) == "depkeyhead="   ; cLine := SubStr( cLine, Len( "depkeyhead="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aKeyHeader ], AllTrim( StrTran( MacroProc( hbmk, cItem, cFileName ), "\", "/" ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depoptional="  ) ) ) == "depoptional="  ; cLine := SubStr( cLine, Len( "depoptional="  ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            cLine := MacroProc( hbmk, cLine, cFileName )
            DO CASE
            CASE Lower( cLine ) == "yes" ; hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_lOptional ] := .T.
            CASE Lower( cLine ) == "no"  ; hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_lOptional ] := .F.
            ENDCASE
         ENDIF

      CASE Lower( Left( cLine, Len( "depcontrol="   ) ) ) == "depcontrol="   ; cLine := SubStr( cLine, Len( "depcontrol="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_cControl ] := AllTrim( MacroProc( hbmk, cLine, cFileName ) )
            AAddNew( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aINCPATH ], _HBMK_DEP_CTRL_MARKER )
         ENDIF

      CASE Lower( Left( cLine, Len( "depincroot="   ) ) ) == "depincroot="   ; cLine := SubStr( cLine, Len( "depincroot="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_cINCROOT ] := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cLine, cFileName ) ), hb_FNameDir( cFileName ) ) )
         ENDIF

      CASE Lower( Left( cLine, Len( "depincpath="   ) ) ) == "depincpath="   ; cLine := SubStr( cLine, Len( "depincpath="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aINCPATH ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cItem, cFileName ) ), hb_FNameDir( cFileName ) ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depincpathlocal=" ) ) ) == "depincpathlocal=" ; cLine := SubStr( cLine, Len( "depincpathlocal=" ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aINCPATHLOCAL ], hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cItem, cFileName ) ), hb_FNameDir( cFileName ) ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depimplibs="   ) ) ) == "depimplibs="   ; cLine := SubStr( cLine, Len( "depimplibs="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aIMPLIBSRC ], hb_DirSepToOS( MacroProc( hbmk, cItem, cFileName ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depimplibd="   ) ) ) == "depimplibd="   ; cLine := SubStr( cLine, Len( "depimplibd="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_cIMPLIBDST ] := hb_FNameNameExt( hb_DirSepToOS( cLine ) )
         ENDIF

      /* .hbc identification strings. Similar to pkgconfig ones. */
      CASE Lower( Left( cLine, Len( "name="         ) ) ) == "name="         ; cLine := SubStr( cLine, Len( "name="         ) + 1 )

         /* Silently ignore */

      CASE Lower( Left( cLine, Len( "description="  ) ) ) == "description="  ; cLine := SubStr( cLine, Len( "description="  ) + 1 )

         /* Silently ignore */

      CASE Lower( Left( cLine, Len( "version="      ) ) ) == "version="      ; cLine := SubStr( cLine, Len( "version="      ) + 1 )

         /* x.y.z where x,y,z >= 0 <= 255 */
         nVersion := 0
         FOR EACH tmp IN ASize( hb_ATokens( cLine, "." ), 3 )
            IF tmp != NIL
               nVersion += Val( tmp )
            ENDIF
            IF tmp:__enumIndex() > 2
               EXIT
            ENDIF
            nVersion *= 256
         NEXT

      CASE Lower( Left( cLine, Len( "keywords="     ) ) ) == "keywords="     ; cLine := SubStr( cLine, Len( "keywords="     ) + 1 )

         /* Silently ignore */

      CASE Lower( Left( cLine, Len( "licences="     ) ) ) == "licenses="     ; cLine := SubStr( cLine, Len( "licences="     ) + 1 )

         /* Silently ignore */

      CASE Lower( Left( cLine, Len( "repository="   ) ) ) == "repository="   ; cLine := SubStr( cLine, Len( "repository="   ) + 1 )

         /* Silently ignore */

      ENDCASE
   NEXT

   cVersion := iif( nVersion == 1, "1", "0x" + hb_NumToHex( nVersion, 6 ) )

   AAddNew( hbmk[ _HBMK_aOPTPRG ], "-D" + hb_StrFormat( _HBMK_HAS_TPL_HBC, StrToDefine( hb_FNameName( cFileName ) ) ) + "=" + cVersion )

   RETURN cVersion

STATIC FUNCTION IsGTRequested( hbmk, cWhichGT )

   /* Check if it is a core/user GT. */
   RETURN ;
      AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == Lower( cWhichGT ) } ) > 0 .OR. ;
      AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == Lower( cWhichGT ) } ) > 0

STATIC FUNCTION StrStripQuote( cString )

   hb_default( @cString, "" )

   RETURN iif( Left( cString, 1 ) == '"' .AND. Right( cString, 1 ) == '"', ;
               SubStr( cString, 2, Len( cString ) - 2 ), ;
               cString )

STATIC FUNCTION ValueIsT( cString )

   cString := Lower( cString )

#ifdef HB_LEGACY_LEVEL4 /* cleanup surrounding code after removing this */
   IF cString == "1" /* Compatibility */
      RETURN .T.
   ENDIF
#endif

   RETURN cString == "yes"

STATIC FUNCTION ValueIsF( cString )

   cString := Lower( cString )

#ifdef HB_LEGACY_LEVEL4 /* cleanup surrounding code after removing this */
   IF cString == "0" /* Compatibility */
      RETURN .T.
   ENDIF
#endif

   RETURN cString == "no"

/* built-in files */

STATIC FUNCTION hbmk_builtin_File_hb_pkg_dynlib()
#pragma __streaminclude "pkg_dynl.hbm" | RETURN %s

STATIC FUNCTION hbmk_builtin_File_hb_pkg_install()
#pragma __streaminclude "pkg_inst.hbm" | RETURN %s

/* interface for handling built-in files */

#define _HBMK_BUILTIN_FILENAME_MARKER_ "$"

STATIC FUNCTION hbmk_builtin_List()

   STATIC s_hHBM_BuiltIn := { ;
      _HBMK_BUILTIN_FILENAME_MARKER_ + "hb_pkg_dynlib.hbm" => {|| hbmk_builtin_File_hb_pkg_dynlib() }, ;
      _HBMK_BUILTIN_FILENAME_MARKER_ + "hb_pkg_install.hbm" => {|| hbmk_builtin_File_hb_pkg_install() } }

   RETURN s_hHBM_BuiltIn

STATIC FUNCTION hbmk_builtin_Is( cFileName )

   cFileName := hb_FNameNameExt( cFileName )

   RETURN LEFTEQUAL( cFileName, _HBMK_BUILTIN_FILENAME_MARKER_ ) .AND. ;
      Len( cFileName ) > Len( _HBMK_BUILTIN_FILENAME_MARKER_ )

STATIC FUNCTION hbmk_builtin_Exists( cFileName )

   cFileName := hb_FNameNameExt( cFileName )

   RETURN hbmk_builtin_Is( cFileName ) .AND. cFileName $ hbmk_builtin_List()

STATIC FUNCTION hbmk_builtin_Load( cFileName )
   RETURN Eval( hbmk_builtin_List()[ hb_FNameNameExt( cFileName ) ] )

/* ; */

STATIC FUNCTION HBM_Load( hbmk, aParams, cFileName, nNestingLevel, lProcHBP, cParentFileName )

   LOCAL cFile
   LOCAL cLine
   LOCAL cParam
   LOCAL aArgs
   LOCAL nResult
   LOCAL cHBP
   LOCAL lFound
   LOCAL tmp

   IF hbmk_builtin_Exists( cFileName ) .OR. hbmk_hb_FileExists( cFileName )

      IF hbmk_builtin_Is( cFileName )
         cFile := hbmk_builtin_Load( cFileName )
         /* Built-in files will act as if they were part of the parant file,
            since their name is fixed and have no useful meaning whatsoever. */
         IF ! Empty( cParentFileName )
            cFileName := cParentFileName
         ENDIF
      ELSE
         cFile := hbmk_MemoRead( cFileName ) /* NOTE: Intentionally using hbmk_MemoRead() which handles EOF char. */
      ENDIF

      IF !( hb_eol() == _CHR_EOL )
         cFile := StrTran( cFile, hb_eol(), _CHR_EOL )
      ENDIF
      IF !( hb_eol() == Chr( 13 ) + Chr( 10 ) )
         cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _CHR_EOL )
      ENDIF

      FOR EACH cLine IN hb_ATokens( cFile, _CHR_EOL )
         IF !( Left( cLine, 1 ) == "#" )
            FOR EACH cParam IN hb_ATokens( cLine,, .T. )
               cParam := StrStripQuote( cParam )
               IF ! Empty( cParam )
                  DO CASE
                  CASE Lower( cParam ) == "-skip"
                     RETURN 0
                  CASE !( Left( cParam, 1 ) == "-" ) .AND. Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" .AND. ;
                       !( Lower( hb_FNameExt( cParam ) ) == ".clp" )
                     IF nNestingLevel < _HBMK_NEST_MAX
                        cParam := SubStr( cParam, 2 )
                        IF Empty( hb_FNameExt( cParam ) )
                           cParam := hb_FNameExtSet( cParam, ".hbm" )
                        ENDIF
                        /* TODO: Modify '@script.ext' (@ prefixes) inclusion to not inherit path from parent */
                        nResult := HBM_Load( hbmk, aParams, PathMakeAbsolute( hb_DirSepToOS( cParam ), cFileName ), nNestingLevel + 1, .T., cFileName ) /* Load parameters from script file */
                        IF nResult != _EXIT_OK .AND. ;
                           nResult != _EXIT_STOP
                           RETURN nResult
                        ENDIF
                     ELSE
                        _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
                     ENDIF
                  CASE !( Left( cParam, 1 ) == "-" ) .AND. ;
                       Lower( hb_FNameExt( cParam ) ) == ".hbm"
                     IF nNestingLevel < _HBMK_NEST_MAX
                        nResult := HBM_Load( hbmk, aParams, PathMakeAbsolute( hb_DirSepToOS( cParam ), cFileName ), nNestingLevel + 1, .T., cFileName ) /* Load parameters from script file */
                        IF nResult != _EXIT_OK .AND. ;
                           nResult != _EXIT_STOP
                           RETURN nResult
                        ENDIF
                     ELSE
                        _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
                     ENDIF
                  CASE !( Left( cParam, 1 ) == "-" ) .AND. ;
                       Lower( hb_FNameExt( cParam ) ) == ".hbp"
                     cHBP := PathMakeAbsolute( hb_DirSepToOS( cParam ), cFileName )
                     IF lProcHBP
                        IF hbmk[ _HBMK_nArgTarget ] > 0

                           /* search for .hbp files in macro libpaths */
                           IF hbmk_hb_FileExists( cHBP )
                              lFound := .T.
                           ELSE
                              lFound := .F.
                              FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                                 IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ tmp .AND. hbmk_hb_FileExists( hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cHBP, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cHBP ) )
                                    cHBP := hb_DirSepAdd( hb_DirSepToOS( MacroProc( hbmk, tmp, cHBP, _MACRO_LATE_PREFIX ) ) ) + hb_FNameNameExt( cHBP )
                                    IF hbmk[ _HBMK_lInfo ]
                                       _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Found project reference on library search path: %1$s" ), cHBP ) )
                                    ENDIF
                                    lFound := .T.
                                    EXIT
                                 ENDIF
                              NEXT
                           ENDIF

                           IF lFound
                              aArgs := AClone( hbmk[ _HBMK_aArgs ] )
                              aArgs[ hbmk[ _HBMK_nArgTarget ] ] := cHBP
                              nResult := __hbmk( aArgs, hbmk[ _HBMK_nArgTarget ], hbmk[ _HBMK_nLevel ] + 1, @hbmk[ _HBMK_lPause ] )
                              IF nResult != 0
                                 RETURN nResult
                              ENDIF
                           ELSE
                              _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Project reference (%1$s) ignored. File not found." ), cHBP ) )
                           ENDIF
                        ELSE
                           _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Project reference (%1$s) ignored. Project references require %2$s to be invoked with a main project." ), cHBP, _SELF_NAME_ ) )
                        ENDIF
                     ELSE
                        _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Project reference (%1$s) ignored in automatic make file: %2$s" ), cHBP, cFileName ) )
                     ENDIF
                  OTHERWISE
                     AAdd( aParams, _PAR_NEW( cParam, cFileName, cLine:__enumIndex() ) )
                  ENDCASE
               ENDIF
            NEXT
         ENDIF
      NEXT
   ELSE
      _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: File cannot be found: %1$s" ), cFileName ) )
   ENDIF

   RETURN 0

/* Filter microformat:
   {[!][<plat|comp>]['&'|'|'][...]}
*/

STATIC FUNCTION ArchCompFilter( hbmk, cItem, cFileName )

   LOCAL nStart, nEnd
   LOCAL cFilterSrc
   LOCAL cFilterHarb
   LOCAL bFilter
   LOCAL xResult
   LOCAL cKeyword
   LOCAL cValue
   LOCAL cOperator
   LOCAL cChar
   LOCAL lSkipQuote
   LOCAL cRetVal
   LOCAL nPos

   LOCAL cExpr := "hbmk_KEYW( hbmk, cFileName, '%1' )"
   LOCAL cExprWithValue := "hbmk_KEYW( hbmk, cFileName, '%1', '%2', '%3' )"
   LOCAL tmp

   nEnd := 1
   WHILE .T.
      IF ( nStart := hb_At( _MACRO_OPEN, cItem, nEnd ) ) == 0
         EXIT
      ENDIF
      IF ( nEnd := hb_At( _MACRO_CLOSE, cItem, nStart + Len( _MACRO_OPEN ) ) ) == 0
         EXIT
      ENDIF
      IF !( SubStr( cItem, nStart - 1, 1 ) $ _MACRO_PREFIX_ALL )
         EXIT
      ENDIF
      nEnd += Len( _MACRO_CLOSE )
   ENDDO

   IF nStart > 0 .AND. nEnd > 0

      /* Separate filter from the rest of the item */
      cFilterSrc := SubStr( cItem, nStart + Len( _MACRO_OPEN ), nEnd - nStart - Len( _MACRO_OPEN ) )
      cItem := Left( cItem, nStart - 1 ) + SubStr( cItem, nEnd + Len( _MACRO_CLOSE ) )

      IF ! Empty( cFilterSrc )

         /* Parse filter and convert it to Harbour expression */
         cFilterHarb := ""
         cKeyword := ""
         cValue := NIL
         cOperator := ""
         lSkipQuote := .F.

         FOR nPos := 1 TO Len( cFilterSrc ) /* USE_FOREACH_ON_STRINGS */
            cChar := SubStr( cFilterSrc, nPos, 1 )

            IF cValue == NIL
               IF iif( Empty( cKeyword ), ;
                     HB_ISFIRSTIDCHAR( cChar ), ;
                     HB_ISNEXTIDCHAR( cChar ) )
                  cKeyword += cChar
               ELSEIF cChar $ "=<>" .AND. SubStr( cFilterSrc, nPos + 1, 1 ) == "'"
                  cOperator := cChar
                  cValue := ""
                  lSkipQuote := .T.
               ELSE
                  IF ! Empty( cKeyword )
                     cFilterHarb += StrTran( cExpr, "%1", cKeyword ) + cChar
                     cKeyword := ""
                  ELSE
                     cFilterHarb += cChar
                  ENDIF
               ENDIF
            ELSE
               IF !( cChar == "'" ) .OR. lSkipQuote
                  IF lSkipQuote
                     lSkipQuote := .F.
                  ELSE
                     cValue += cChar
                  ENDIF
               ELSE
                  IF ! Empty( cKeyword ) .AND. ! Empty( cValue )
                     tmp := cExprWithValue
                     tmp := hb_StrReplace( tmp, ;
                        { "%1", "%2", "%3" }, ;
                        { cKeyword, cValue, cOperator } )
                     cFilterHarb += tmp
                     cKeyword := ""
                     cValue := NIL
                     cOperator := ""
                  ENDIF
               ENDIF
            ENDIF
         NEXT
         IF ! Empty( cKeyword )
            IF ! Empty( cValue )
               tmp := cExprWithValue
               tmp := hb_StrReplace( tmp, ;
                  { "%1", "%2", "%3" }, ;
                  { cKeyword, cValue, cOperator } )
               cFilterHarb += tmp
            ELSE
               cFilterHarb += StrTran( cExpr, "%1", cKeyword )
            ENDIF
         ENDIF

         cFilterHarb := StrTran( cFilterHarb, "&&", "&" )
         cFilterHarb := StrTran( cFilterHarb, "||", "|" )

         cFilterHarb := StrTran( cFilterHarb, "&", ".AND." )
         cFilterHarb := StrTran( cFilterHarb, "|", ".OR." )

         cRetVal := ""

         /* Evaluate filter */
         BEGIN SEQUENCE WITH {| oError | Break( oError ) }
            bFilter := &( "{| hbmk, cFileName |" + cFilterHarb + "}" )
            IF HB_ISLOGICAL( xResult := Eval( bFilter, hbmk, cFileName ) ) .AND. xResult
               cRetVal := cItem
            ENDIF
         RECOVER
            IF ! hbmk[ _HBMK_lQuiet ]
               _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Error in filter expression: '%1$s'" ), cFilterSrc ) )
            ENDIF
         END /* SEQUENCE */

         RETURN cRetVal
      ENDIF
   ENDIF

   RETURN cItem

STATIC FUNCTION MacroProc( hbmk, cString, cFileName, cMacroPrefix )

   LOCAL nStart
   LOCAL nEnd
   LOCAL cMacro

   LOCAL cStart := iif( HB_ISSTRING( cMacroPrefix ), cMacroPrefix, _MACRO_NORM_PREFIX ) + _MACRO_OPEN

   LOCAL cStdOut

   WHILE ( nStart := At( cStart, cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( _MACRO_CLOSE, cString, nStart + Len( cStart ) ) ) > 0

      cMacro := MacroGet( hbmk, SubStr( cString, nStart + Len( cStart ), nEnd - nStart - Len( cStart ) ), cFileName )

      cString := Left( cString, nStart - 1 ) + cMacro + SubStr( cString, nEnd + Len( _MACRO_CLOSE ) )
   ENDDO

   WHILE ( nStart := At( _CMDSUBST_OPEN, cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( _CMDSUBST_CLOSE, cString, nStart + Len( _CMDSUBST_OPEN ) ) ) > 0
      cMacro := SubStr( cString, nStart + Len( _CMDSUBST_OPEN ), nEnd - nStart - Len( _CMDSUBST_OPEN ) )
      cStdOut := ""
      IF ! Empty( cMacro )
         hb_processRun( cMacro,, @cStdOut )
         cStdOut := StrTran( StrTran( cStdOut, Chr( 13 ) ), Chr( 10 ), " " )
      ENDIF
      cString := Left( cString, nStart - 1 ) + cStdOut + SubStr( cString, nEnd + Len( _CMDSUBST_CLOSE ) )
   ENDDO

   RETURN cString

STATIC FUNCTION MacroGet( hbmk, cMacro, cFileName )

#if 0
   /* Support for: ${@<filename>} to include on-disk file content of <filename>
                   ${@@<envvar>} to include on-disk file content referenced
                                 from filename contained in <envvar> */
   IF Left( cMacro, 1 ) == "@"
      cMacro := SubStr( cMacro, 2 )
      IF Left( cMacro, 1 ) == "@"
         cMacro := GetEnv( SubStr( cMacro, 2 ) )
      ENDIF
      RETURN StrTran( StrTran( hb_MemoRead( cMacro ), Chr( 13 ) ), Chr( 10 ), " " )
   ENDIF
#endif

   SWITCH Lower( cMacro )
   CASE "hb_root"
      cMacro := hb_DirSepAdd( hb_DirBase() ) ; EXIT
   CASE "hb_dir"
      cMacro := hb_DirSepToOS( hb_FNameDir( cFileName ) ) ; EXIT
   CASE "hb_dirname"
      cMacro := hb_FNameName( hb_DirSepDel( hb_DirSepToOS( hb_FNameDir( cFileName ) ) ) ) ; EXIT
   CASE "hb_name"
      cMacro := hb_DirSepToOS( hb_FNameName( cFileName ) ) ; EXIT
   CASE "hb_self"
      cMacro := hb_DirSepToOS( cFileName ) ; EXIT
   CASE "hb_curdir"
      cMacro := hb_cwd() ; EXIT
   CASE "hb_tempdir"
      cMacro := hb_DirTemp() ; EXIT
   CASE "hb_targetname"
      cMacro := hb_FNameName( hb_DirSepToOS( hbmk_TARGETNAME( hbmk ) ) ) ; EXIT
   CASE "hb_targettype"
      cMacro := hbmk_TARGETTYPE( hbmk ) ; EXIT
   CASE "hb_plat"
#ifdef HB_LEGACY_LEVEL4
   CASE "hb_platform" /* Compatibility */
#endif
      cMacro := hbmk[ _HBMK_cPLAT ] ; EXIT
   CASE "hb_comp"
#ifdef HB_LEGACY_LEVEL4
   CASE "hb_compiler" /* Compatibility */
#endif
      cMacro := hbmk[ _HBMK_cCOMP ] ; EXIT
   CASE "hb_comp_ver"
      cMacro := hb_ntos( hbmk[ _HBMK_nCOMPVer ] ) ; EXIT
   CASE "hb_build"
      cMacro := hbmk[ _HBMK_cBUILD ] ; EXIT
   CASE "hb_cpu"
      cMacro := hbmk[ _HBMK_cCPU ] ; EXIT
   CASE "hb_work"
      cMacro := _WORKDIR_BASE_ ; EXIT
   CASE "hb_workdynsub"
      cMacro := hbmk[ _HBMK_cWorkDirDynSub ] ; EXIT
   CASE "hb_dynprefix"
      cMacro := hbmk[ _HBMK_cDynLibPrefix ] ; EXIT
   CASE "hb_dynsuffix"
      cMacro := hbmk_DYNSUFFIX( hbmk ) ; EXIT
   CASE "hb_dynext"
      cMacro := hbmk[ _HBMK_cDynLibExt ] ; EXIT
   CASE "hb_ver"
      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
         cMacro := ;
            hb_NumToHex( hb_Version( HB_VERSION_MAJOR ), 2 ) + ;
            hb_NumToHex( hb_Version( HB_VERSION_MINOR ), 2 ) + ;
            hb_NumToHex( hb_Version( HB_VERSION_RELEASE ), 2 )
      ELSE
         cMacro := hb_NumToHex( Abs( hbmk[ _HBMK_nHBMODE ] ), 6 )
      ENDIF
      EXIT
   CASE "hb_verstr"
      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
         cMacro := ;
            hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) + "." + ;
            hb_ntos( hb_Version( HB_VERSION_MINOR ) ) + "." + ;
            hb_ntos( hb_Version( HB_VERSION_RELEASE ) ) + ;
            hb_Version( HB_VERSION_STATUS )
      ELSE
         cMacro := ;
            hb_ntos( hb_bitAnd( hb_bitShift( Abs( hbmk[ _HBMK_nHBMODE ] ), -16 ), 0xFF ) ) + "." + ;
            hb_ntos( hb_bitAnd( hb_bitShift( Abs( hbmk[ _HBMK_nHBMODE ] ),  -8 ), 0xFF ) ) + "." + ;
            hb_ntos( hb_bitAnd( hb_bitShift( Abs( hbmk[ _HBMK_nHBMODE ] ),   0 ), 0xFF ) )
      ENDIF
      EXIT
   CASE "hb_major"
      cMacro := hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) ; EXIT
   CASE "hb_minor"
      cMacro := hb_ntos( hb_Version( HB_VERSION_MINOR ) ) ; EXIT
   CASE "hb_release"
      cMacro := hb_ntos( hb_Version( HB_VERSION_RELEASE ) ) ; EXIT
   CASE "hb_status"
      cMacro := hb_Version( HB_VERSION_STATUS ) ; EXIT
   CASE "hb_revision"
      cMacro := hb_ntos( hb_Version( HB_VERSION_REVISION ) ) ; EXIT
   CASE "hb_host_plat"
      cMacro := hb_Version( HB_VERSION_PLATFORM ) ; EXIT
   CASE "hb_host_plat_unix"
      cMacro := iif( hb_Version( HB_VERSION_UNIX_COMPAT ), "1", "" ) ; EXIT
   CASE "hb_bin"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_BIN ] ; EXIT
   CASE "hb_lib"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_LIB ] ; EXIT
   CASE "hb_lib3rd"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_LI3 ] ; EXIT
   CASE "hb_dyn"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_DYN ] ; EXIT
   CASE "hb_inc"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_INC ] ; EXIT
   CASE "hb_addons"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_ADD ] ; EXIT
   CASE "hb_first"
      cMacro := hb_FNameName( hbmk[ _HBMK_cFIRST ] ) ; EXIT
   CASE "hb_outputdir"
      cMacro := iif( HB_ISSTRING( hbmk[ _HBMK_cPROGDIR ] ), hb_FNameDir( hbmk[ _HBMK_cPROGDIR ] ), "" ) ; EXIT
   CASE "hb_outputname"
      cMacro := iif( HB_ISSTRING( hbmk[ _HBMK_cPROGNAME ] ), hb_FNameName( hbmk[ _HBMK_cPROGNAME ] ), "" ) ; EXIT
   CASE "hb_level"
      cMacro := hb_ntos( hbmk[ _HBMK_nLevel ] ) ; EXIT
   OTHERWISE
      IF cMacro $ hbmk[ _HBMK_hDEPTMACRO ] /* Check for dependency detection macros */
         IF Empty( hbmk[ _HBMK_hDEPTMACRO ][ cMacro ] )
            cMacro := "1"
         ELSE
            cMacro := hbmk[ _HBMK_hDEPTMACRO ][ cMacro ]
         ENDIF
      ELSE
         /* NOTE: If macro not found, try to interpret as
                  envvar. If it does not exist, empty string
                  will be returned (without warning) [vszakats] */
         cMacro := GetEnv( cMacro )
      ENDIF
   ENDSWITCH

   hb_default( @cMacro, "" )

   RETURN cMacro

STATIC FUNCTION IsValidHarbourID( cName )

   LOCAL c

   IF HB_ISFIRSTIDCHAR( Left( cName, 1 ) )
      FOR EACH c IN SubStr( cName, 2 )
         IF ! HB_ISNEXTIDCHAR( c )
            RETURN .F.
         ENDIF
      NEXT
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION FuncNameEncode( cName )

   LOCAL cResult := ""

   LOCAL c
   LOCAL nPos

   IF HB_ISSTRING( cName )
      FOR nPos := 1 TO hb_BLen( cName )
         c := hb_BSubStr( cName, nPos, 1 )
         /* synced to how Harbour compiler actually works (see hb_compGenCFunc()).
            Theoretically, it should work like this:
               iif( cResult == "", HB_ISFIRSTIDCHAR( c ), HB_ISNEXTIDCHAR( c ) ) */
         IF HB_ISNEXTIDCHAR( c )
            cResult += c
         ELSE
            cResult += "x" + Lower( hb_NumToHex( Asc( c ), 2 ) )
         ENDIF
      NEXT
   ENDIF

   RETURN cResult

STATIC FUNCTION IsHexDigit( c )
   RETURN c $ "0123456789ABCDEFabcdef"

/* in GCC LD (except DJGPP) the order of registering init function
 * does not depend directly on the order of linked files. If we want
 * to inform HVM about valid startup function then we should try to
 * locate it ourselves and pass it to HVM using our startup function
 * [druzus]
 */
STATIC FUNCTION getFirstFunc( hbmk, cFile )

   LOCAL cFuncList, cExecNM, cFuncName, cExt, cLine, n, c

   cFuncName := ""
   IF HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|gccomf|clang" )
      hb_FNameSplit( cFile,,, @cExt )
      IF cExt == ".c"
         FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFile ), Chr( 13 ), Chr( 10 ) ), Chr( 10 ) )
            cLine := AllTrim( cLine )
            IF LEFTEQUAL( cLine, '{ "' ) .AND. "HB_FS_FIRST" $ cLine .AND. !( "HB_FS_STATIC" $ cLine )
               n := 4
               WHILE ( c := SubStr( cLine, n++, 1 ) ) != '"'
                  cFuncName += c
               ENDDO
               EXIT
            ENDIF
         NEXT
      ELSEIF Lower( cExt ) == ".cpp" .OR. ;
             Lower( cExt ) == ".cc" .OR. ;
             Lower( cExt ) == ".cxx" .OR. ;
             Lower( cExt ) == ".cx"
         /* do nothing */
      ELSEIF ! Empty( cExecNM := FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "nm" ) )
         cFuncList := ""
         hb_processRun( cExecNM + " " + FNameEscape( cFile, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) + ;
            " -g -n" + iif( hbmk[ _HBMK_cCOMP ] == "darwin", "", " --defined-only -C" ),, @cFuncList )
         IF ( n := At( " T HB_FUN_", cFuncList ) ) != 0
            n += 10
         ELSEIF ( n := At( " T _HB_FUN_", cFuncList ) ) != 0
            n += 11
         ENDIF
         IF n != 0
            WHILE ( c := SubStr( cFuncList, n++, 1 ) ) == "_" .OR. ;
                     hb_asciiIsDigit( c ) .OR. hb_asciiIsAlpha( c )
               IF c == "x" .AND. IsHexDigit( SubStr( cFuncList, n, 1 ) ) .AND. ;
                                 IsHexDigit( SubStr( cFuncList, n + 1, 1 ) )
                  c := hb_HexToNum( SubStr( cFuncList, n, 2 ) )
                  n += 2
               ENDIF
               cFuncName += c
            ENDDO
         ENDIF
      ENDIF
   ENDIF

   RETURN cFuncName

STATIC PROCEDURE PlatformPRGFlags( hbmk, aOPTPRG )

   LOCAL aUn
   LOCAL aDf
   LOCAL cMacro
   LOCAL nPos

   IF !( hbmk[ _HBMK_cPLAT ] == hb_Version( HB_VERSION_BUILD_PLAT ) ) .OR. ;
      !( hbmk[ _HBMK_cCOMP ] == hb_Version( HB_VERSION_BUILD_COMP ) )

      aUn := {}
      aDf := {}

      AAdd( aUn, ".ARCH." )

      IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
      #if   defined( __PLATFORM__WINDOWS )
         AAdd( aUn, "__PLATFORM__Windows" )
         #if defined( __PLATFORM__WINCE )
            AAdd( aUn, "__PLATFORM__WINCE" )
         #endif
         /* This is defined with Cygwin */
         #if defined( __PLATFORM__UNIX )
            AAdd( aUn, "__PLATFORM__UNIX" )
         #endif
      #elif defined( __PLATFORM__DOS )
         AAdd( aUn, "__PLATFORM__DOS" )
      #elif defined( __PLATFORM__OS2 )
         AAdd( aUn, "__PLATFORM__OS2" )
      #elif defined( __PLATFORM__LINUX )
         AAdd( aUn, "__PLATFORM__Linux" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__DARWIN )
         AAdd( aUn, "__PLATFORM__DARWIN" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__BSD )
         AAdd( aUn, "__PLATFORM__BSD" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__SUNOS )
         AAdd( aUn, "__PLATFORM__SUNOS" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__HPUX )
         AAdd( aUn, "__PLATFORM__HPUX" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__CYGWIN )
         AAdd( aUn, "__PLATFORM__CYGWIN" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__AIX )
         AAdd( aUn, "__PLATFORM__AIX" )
         AAdd( aUn, "__PLATFORM__UNIX" )
      #endif

      #if   defined( __ARCH16BIT__ )
         AAdd( aUn, "__ARCH16BIT__" )
      #elif defined( __ARCH32BIT__ )
         AAdd( aUn, "__ARCH32BIT__" )
      #elif defined( __ARCH64BIT__ )
         AAdd( aUn, "__ARCH64BIT__" )
      #endif

      #if   defined( __LITTLE_ENDIAN__ )
         AAdd( aUn, "__LITTLE_ENDIAN__" )
      #elif defined( __BIG_ENDIAN__ )
         AAdd( aUn, "__BIG_ENDIAN__" )
      #elif defined( __PDP_ENDIAN__ )
         AAdd( aUn, "__PDP_ENDIAN__" )
      #endif
      ENDIF

      DO CASE
      CASE hbmk[ _HBMK_cPLAT ] == "wce"
         AAdd( aDf, "__PLATFORM__WINDOWS" )
         AAdd( aDf, "__PLATFORM__WINCE" )
         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            AAdd( aDf, "__PLATFORM__Windows" )
         ENDIF
      CASE hbmk[ _HBMK_cPLAT ] == "win"
         AAdd( aDf, "__PLATFORM__WINDOWS" )
         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            AAdd( aDf, "__PLATFORM__Windows" )
         ENDIF
      CASE hbmk[ _HBMK_cPLAT ] == "dos"
         AAdd( aDf, "__PLATFORM__DOS" )
      CASE hbmk[ _HBMK_cPLAT ] == "os2"
         AAdd( aDf, "__PLATFORM__OS2" )
      CASE hbmk[ _HBMK_cPLAT ] == "linux"
         AAdd( aDf, "__PLATFORM__LINUX" )
         AAdd( aDf, "__PLATFORM__UNIX" )
         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            AAdd( aDf, "__PLATFORM__Linux" )
         ENDIF
      CASE hbmk[ _HBMK_cPLAT ] == "darwin"
         AAdd( aDf, "__PLATFORM__DARWIN" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "bsd"
         AAdd( aDf, "__PLATFORM__BSD" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "sunos"
         AAdd( aDf, "__PLATFORM__SUNOS" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "hpux"
         AAdd( aDf, "__PLATFORM__HPUX" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "beos"
         AAdd( aDf, "__PLATFORM__BEOS" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "qnx"
         AAdd( aDf, "__PLATFORM__QNX" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "android"
         AAdd( aDf, "__PLATFORM__ANDROID" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
         AAdd( aDf, "__PLATFORM__VXWORKS" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "symbian"
         AAdd( aDf, "__PLATFORM__SYMBIAN" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "cygwin"
         AAdd( aDf, "__PLATFORM__CYGWIN" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "minix"
         AAdd( aDf, "__PLATFORM__MINIX" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "aix"
         AAdd( aDf, "__PLATFORM__AIX" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      ENDCASE

      /* Setup those CPU flags which we can be sure about.
         This is not fully generic solution, cross builds
         to *nix systems are not covered. Anyway, it is not
         recommended to use these macros in .prg code.
         [vszakats] */
      DO CASE
      CASE HBMK_ISPLAT( "dos|os2" )
         AAdd( aDf, "__LITTLE_ENDIAN__" )
         AAdd( aDf, "__ARCH32BIT__" )
      CASE HBMK_ISPLAT( "wce|win" )
         AAdd( aDf, "__LITTLE_ENDIAN__" ) /* Windows is currently little-endian on all supported CPUs. */
         IF hbmk[ _HBMK_cCOMP ] == "mingw64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "msvc64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "pocc64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "msvcia64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "iccia64"
            AAdd( aDf, "__ARCH64BIT__" )
         ELSE
            AAdd( aDf, "__ARCH32BIT__" )
         ENDIF
      OTHERWISE
         /* NOTE: Users will have to manually #define fitting macros for
                  given platform + compiler settings. We could only guess.
                  Let us assume the most probable CPU platform (as of 2009). */
         AAdd( aDf, "__LITTLE_ENDIAN__" )
         AAdd( aDf, "__ARCH32BIT__" )
      ENDCASE

      /* Delete macros present in both lists */
      FOR EACH cMacro IN aUn DESCEND
         IF ( nPos := AScan( aDf, {| tmp | tmp == cMacro } ) ) > 0
            hb_ADel( aUn, cMacro:__enumIndex(), .T. )
            hb_ADel( aDf, nPos, .T. )
         ENDIF
      NEXT

      FOR EACH cMacro IN aUn
         AAdd( aOPTPRG, "-undef:" + cMacro )
      NEXT
      FOR EACH cMacro IN aDf
         AAdd( aOPTPRG, "-D" + cMacro )
      NEXT
   ENDIF

   RETURN

#define RTLNK_MODE_NONE         0
#define RTLNK_MODE_OUT          1
#define RTLNK_MODE_FILE         2
#define RTLNK_MODE_FILENEXT     3
#define RTLNK_MODE_LIB          4
#define RTLNK_MODE_LIBNEXT      5
#define RTLNK_MODE_SKIP         6
#define RTLNK_MODE_SKIPNEXT     7

#if 0
STATIC PROCEDURE rtlnk_libtrans( aLibList )

   STATIC s_hTrans := { ;
      "CT"        => "hbct", ;
      "CTP"       => "hbct", ;
      "CLASSY"    => NIL, ;
      "CSYINSP"   => NIL, ;
      "SIX3"      => NIL, ;
      "NOMACH6"   => NIL, ;
      "BLXRATEX"  => NIL, ;
      "BLXCLP50"  => NIL, ;
      "BLXCLP52"  => NIL, ;
      "BLXCLP53"  => NIL, ;
      "EXOSPACE"  => NIL, ;
      "CLIPPER"   => NIL, ;
      "EXTEND"    => NIL, ;
      "TERMINAL"  => NIL, ;
      "PCBIOS"    => NIL, ;
      "ANSITERM"  => NIL, ;
      "DBFBLOB"   => NIL, ;
      "DBFMEMO"   => NIL, ;
      "DBFNTX"    => NIL, ;
      "DBFCDX"    => NIL, ;
      "_DBFCDX"   => NIL, ;
      "CLD"       => NIL, ;
      "CLDR"      => NIL, ;
      "LLIBCE"    => NIL, ;
      "LLIBCA"    => NIL }
   LOCAL cLib

   FOR EACH cLib IN aLibList DESCEND
      IF Lower( Right( cLib, 4 ) ) == ".lib"
         cLib := Left( cLib, Len( cLib ) - 4 )
      ENDIF
      IF Upper( cLib ) $ s_hTrans
         cLib := s_hTrans[ Upper( cLib ) ]
         IF cLib == NIL
            hb_ADel( aLibList, cLib:__enumIndex(), .T. )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE rtlnk_filetrans( aFileList )

   STATIC s_hTrans := { ;
      "CTUS"      => NIL, ;
      "CTUSP"     => NIL, ;
      "CTINT"     => NIL, ;
      "CTINTP"    => NIL, ;
      "__WAIT"    => NIL, ;
      "__WAIT_4"  => NIL, ;
      "__WAIT_B"  => NIL, ;
      "BLXCLP50"  => NIL, ;
      "BLXCLP52"  => NIL, ;
      "BLXCLP53"  => NIL, ;
      "BLDCLP50"  => NIL, ;
      "BLDCLP52"  => NIL, ;
      "BLDCLP53"  => NIL, ;
      "SIXCDX"    => NIL, ;
      "SIXNSX"    => NIL, ;
      "SIXNTX"    => NIL, ;
      "DBT"       => NIL, ;
      "FPT"       => NIL, ;
      "SMT"       => NIL, ;
      "NOMEMO"    => NIL, ;
      "CLD.LIB"   => NIL }
   LOCAL cFile

   FOR EACH cFile IN aFileList DESCEND
      IF Lower( Right( cFile, 4 ) ) == ".obj"
         cFile := Left( cFile, Len( cFile ) - 4 )
      ENDIF
      IF Upper( cFile ) $ s_hTrans
         cFile := s_hTrans[ Upper( cFile ) ]
         IF cFile == NIL
            hb_ADel( aFileList, cFile:__enumIndex(), .T. )
         ENDIF
      ENDIF
   NEXT

   RETURN
#endif

STATIC FUNCTION rtlnk_read( cFileName, aPrevFiles )

   LOCAL cFileBody
   LOCAL cPath, cFile, cExt
   LOCAL hFile

   hb_FNameSplit( cFileName, @cPath, @cFile, @cExt )
   IF Empty( cExt )
      cExt := ".lnk"
   ENDIF

   cFileName := hb_FNameMerge( cPath, cFile, ".lnk" )
   /* it is Blinker extension, look for .lnk file in paths
    * specified by LIB envvar
    */
   IF ! hb_FileExists( cFileName ) .AND. ;
      !( Left( cFileName, 1 ) $ hb_osPathDelimiters() ) .AND. ;
      !( SubStr( cFileName, 2, 1 ) == hb_osDriveSeparator() )
      FOR EACH cPath IN hb_ATokens( GetEnv( "LIB" ), hb_osPathListSeparator() )
         cFile := hb_FNameMerge( cPath, cFileName )
         IF hb_FileExists( cFile )
            cFileName := cFile
            EXIT
         ENDIF
      NEXT
   ENDIF

   /* protection against recursive calls */
   IF AScan( aPrevFiles, {| x | x == cFileName } ) == 0
      IF ( hFile := FOpen( cFileName ) ) != F_ERROR
         cFileBody := Space( FSeek( hFile, 0, FS_END ) )
         FSeek( hFile, 0, FS_SET )
         IF FRead( hFile, @cFileBody, hb_BLen( cFileBody ) ) != hb_BLen( cFileBody )
            cFileBody := NIL
         ENDIF
         FClose( hFile )
      ENDIF
      AAdd( aPrevFiles, cFileName )
   ELSE
      cFileBody := ""
   ENDIF

   RETURN cFileBody

STATIC FUNCTION rtlnk_tokens( cLine )

   LOCAL cCh

   FOR EACH cCh IN @cLine
      IF cCh $ Chr( 9 ) + Chr( 11 ) + Chr( 12 ) + ";"
         cCh := " "
      ENDIF
   NEXT

   RETURN hb_ATokens( StrTran( cLine, ",", " , " ) )

STATIC FUNCTION rtlnk_process( hbmk, cCommands, cFileOut, aFileList, ;
                               aLibList, aLibPath, aPrevFiles )

   LOCAL cLine, cWord
   LOCAL nMode

   cCommands := StrTran( StrTran( cCommands, Chr( 13 ) ), "//", "# " )

   nMode := RTLNK_MODE_NONE

   hb_default( @aPrevFiles, {} )

   FOR EACH cLine IN hb_ATokens( cCommands, Chr( 10 ) )
      cLine := AllTrim( cLine )
      IF ! Empty( cLine )
         FOR EACH cWord IN rtlnk_tokens( cLine )
            IF LEFTEQUAL( cWord, "#" )
               EXIT
            ELSEIF nMode == RTLNK_MODE_OUT
               cFileOut := hb_DirSepToOS( cWord )
               IF Lower( hb_FNameExt( cFileOut ) ) == ".exe"
                  cFileOut := FNameDirName( cFileOut )
               ENDIF
               nMode := RTLNK_MODE_FILENEXT
            ELSEIF nMode == RTLNK_MODE_FILE
               IF !( cWord == "," )
                  IF AScan( aFileList, {| x | x == cWord } ) == 0
                     AAdd( aFileList, hb_DirSepToOS( cWord ) )
                  ENDIF
                  nMode := RTLNK_MODE_FILENEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_LIB
               IF !( cWord == "," )
                  AAdd( aLibList, hb_FNameName( hb_DirSepToOS( cWord ) ) )
                  IF ! Empty( hb_FNameDir( hb_DirSepToOS( cWord ) ) )
                     AAddNew( aLibPath, hb_FNameDir( hb_DirSepToOS( cWord ) ) )
                  ENDIF
                  nMode := RTLNK_MODE_LIBNEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_SKIP
               IF !( cWord == "," )
                  nMode := RTLNK_MODE_SKIPNEXT
               ENDIF
            ELSEIF cWord == ","
               IF nMode == RTLNK_MODE_FILENEXT
                  nMode := RTLNK_MODE_FILE
               ELSEIF nMode == RTLNK_MODE_LIBNEXT
                  nMode := RTLNK_MODE_LIB
               ELSEIF nMode == RTLNK_MODE_SKIPNEXT
                  nMode := RTLNK_MODE_SKIP
               ENDIF
            ELSEIF LEFTEQUAL( cWord, "@" )
               cWord := SubStr( cWord, 2 )
               cCommands := rtlnk_read( @cWord, aPrevFiles )
               IF cCommands == NIL
                  _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Cannot open file: %1$s" ), cWord ) )
                  RETURN .F.
               ENDIF
               IF ! rtlnk_process( hbmk, cCommands, @cFileOut, @aFileList, @aLibList, @aLibPath, aPrevFiles )
                  RETURN .F.
               ENDIF
            ELSE
               cWord := Upper( cWord )
               IF Len( cWord ) >= 2
                  IF LEFTEQUAL( "OUTPUT", cWord )
                     nMode := RTLNK_MODE_OUT
                  ELSEIF LEFTEQUAL( "FILE", cWord )
                     nMode := RTLNK_MODE_FILE
                  ELSEIF LEFTEQUAL( "LIBRARY", cWord )
                     nMode := RTLNK_MODE_LIB
                  ELSEIF LEFTEQUAL( "MODULE", cWord ) .OR. ;
                         LEFTEQUAL( "EXCLUDE", cWord ) .OR. ;
                         LEFTEQUAL( "REFER", cWord ) .OR. ;
                         LEFTEQUAL( "INTO", cWord )
                     nMode := RTLNK_MODE_SKIP
                  /* Blinker extension */
                  ELSEIF LEFTEQUAL( "BLINKER", cWord )
                     /* skip Blinker commands */
                     EXIT
                  ELSEIF LEFTEQUAL( "ECHO", cWord )
                     _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Blinker ECHO: %1$s" ), SubStr( cLine, 6 ) ) )
                     EXIT
                  ELSEIF LEFTEQUAL( "MAP", cWord )
                     hbmk[ _HBMK_lMAP ] := .T.
                     EXIT
                  ELSEIF LEFTEQUAL( "NOBELL", cWord )
                     hbmk[ _HBMK_lBEEP ] := .F.
                     EXIT
                  ELSE /* TODO: add other Blinker commands */
                  ENDIF
               ENDIF
            ENDIF
         NEXT
      ENDIF
   NEXT

   RETURN .T.

/* .po generation */

STATIC PROCEDURE RebuildPO( hbmk, aPOTIN )

   LOCAL cLNG
   LOCAL fhnd
   LOCAL cPOTemp
   LOCAL cPOCooked

   LOCAL aNew := {}
   LOCAL aUpd := {}

   FOR EACH cLNG IN iif( Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] ), { _LNG_MARKER }, hbmk[ _HBMK_aLNG ] )
      IF cLNG:__enumIndex() == 1
         fhnd := hb_FTempCreateEx( @cPOTemp, NIL, NIL, ".po" )
         IF fhnd != F_ERROR
            FClose( fhnd )
            IF hbmk[ _HBMK_lDEBUGI18N ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: file .pot list: %1$s", ArrayToList( aPOTIN, ", " ) ) )
               _hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: temp unified .po: %1$s", cPOTemp ) )
            ENDIF
            POTMerge( hbmk, aPOTIN, NIL, cPOTemp )
         ELSE
            _hbmk_OutStd( hbmk, I_( "Error: Cannot create temporary unified .po file." ) )
         ENDIF
      ENDIF
      cPOCooked := StrTran( hbmk[ _HBMK_cPO ], _LNG_MARKER, cLNG )
      IF hb_FileExists( cPOCooked )
         IF hbmk[ _HBMK_lDEBUGI18N ]
            _hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: updating unified .po: %1$s", cPOCooked ) )
         ENDIF
         AutoTrans( hbmk, cPOTemp, { cPOCooked }, cPOCooked )
         AAdd( aUpd, cLNG )
      ELSE
         IF hbmk[ _HBMK_lDEBUGI18N ]
            _hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: creating unified .po: %1$s", cPOCooked ) )
         ENDIF
         hb_FCopy( cPOTemp, cPOCooked )
         AAdd( aNew, cLNG )
      ENDIF
   NEXT

   IF ! Empty( cPOTemp )
      FErase( cPOTemp )
   ENDIF

   IF ! hbmk[ _HBMK_lQuiet ]
      IF ! Empty( aNew )
         IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] )
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .po file '%1$s'" ), hbmk[ _HBMK_cPO ] ) )
         ELSE
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .po file '%1$s' for language(s): %2$s" ), hbmk[ _HBMK_cPO ], ArrayToList( aNew, "," ) ) )
         ENDIF
      ENDIF
      IF ! Empty( aUpd )
         IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] )
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Rebuilt .po file '%1$s'" ), hbmk[ _HBMK_cPO ] ) )
         ELSE
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Rebuilt .po file '%1$s' for language(s): %2$s" ), hbmk[ _HBMK_cPO ], ArrayToList( aUpd, "," ) ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

/* .po update */

STATIC PROCEDURE UpdatePO( hbmk, aPOTIN )

   LOCAL cLNG

   LOCAL aUpd := {}

   FOR EACH cLNG IN iif( Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] ), { _LNG_MARKER }, hbmk[ _HBMK_aLNG ] )
      POTMerge( hbmk, aPOTIN, StrTran( hbmk[ _HBMK_cPO ], _LNG_MARKER, cLNG ), StrTran( hbmk[ _HBMK_cPO ], _LNG_MARKER, cLNG ) )
      AAdd( aUpd, cLNG )
   NEXT

   IF hbmk[ _HBMK_lDEBUGI18N ]
      _hbmk_OutStd( hbmk, hb_StrFormat( "UpdatePO: file .pot list: %1$s", ArrayToList( aPOTIN, ", " ) ) )
      _hbmk_OutStd( hbmk, hb_StrFormat( "UpdatePO: for .po: %1$s", hbmk[ _HBMK_cPO ] ) )
      _hbmk_OutStd( hbmk, hb_StrFormat( "UpdatePO: for languages: %1$s", ArrayToList( hbmk[ _HBMK_aLNG ], ", " ) ) )
   ENDIF

   IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] )
      _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Updated .po file '%1$s'" ), hbmk[ _HBMK_cPO ] ) )
   ELSE
      _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Updated .po file '%1$s' for language(s): %2$s" ), hbmk[ _HBMK_cPO ], ArrayToList( aUpd, "," ) ) )
   ENDIF

   RETURN

/* .hbl generation */

STATIC PROCEDURE MakeHBL( hbmk, cHBL )

   LOCAL cPO
   LOCAL tPO
   LOCAL cLNG
   LOCAL tLNG
   LOCAL aPO_TO_DO
   LOCAL lUpdateNeeded

   LOCAL aNew := {}

   IF ! Empty( hbmk[ _HBMK_aPO ] )
      IF hbmk[ _HBMK_lDEBUGI18N ]
         _hbmk_OutStd( hbmk, hb_StrFormat( "po: in: %1$s", ArrayToList( hbmk[ _HBMK_aPO ] ) ) )
      ENDIF
      IF Empty( cHBL )
         cHBL := hb_FNameName( hbmk[ _HBMK_aPO ][ 1 ] )
      ENDIF
      IF Empty( hb_FNameExt( cHBL ) )
         cHBL := hb_FNameExtSet( cHBL, ".hbl" )
      ENDIF

      FOR EACH cLNG IN iif( Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ cHBL ), { _LNG_MARKER }, hbmk[ _HBMK_aLNG ] )
         tLNG := NIL
         hb_FGetDateTime( StrTran( cHBL, _LNG_MARKER, cLNG ), @tLNG )
         lUpdateNeeded := .F.
         aPO_TO_DO := {}
         FOR EACH cPO IN hbmk[ _HBMK_aPO ]
            IF tLNG == NIL .OR. ( hb_FGetDateTime( StrTran( cPO, _LNG_MARKER, cLNG ), @tPO ) .AND. tPO > tLNG )
               lUpdateNeeded := .T.
            ENDIF
            AAdd( aPO_TO_DO, StrTran( cPO, _LNG_MARKER, cLNG ) )
         NEXT
         IF lUpdateNeeded
            IF hbmk[ _HBMK_lDEBUGI18N ]
               _hbmk_OutStd( hbmk, hb_StrFormat( "po: %1$s -> %2$s", ArrayToList( aPO_TO_DO ), StrTran( cHBL, _LNG_MARKER, cLNG ) ) )
            ENDIF
            GenHBL( hbmk, aPO_TO_DO, StrTran( cHBL, _LNG_MARKER, cLNG ) )
            AAdd( aNew, cLNG )
         ENDIF
      NEXT
   ENDIF

   IF ! hbmk[ _HBMK_lQuiet ]
      IF ! Empty( aNew )
         IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ cHBL )
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .hbl file '%1$s'" ), cHBL ) )
         ELSE
            _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .hbl file '%1$s' for language(s): %2$s" ), cHBL, ArrayToList( aNew, "," ) ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION LoadPOTFiles( hbmk, aFiles, cFileBase, lIgnoreError )

   LOCAL aTrans, aTrans2
   LOCAL hIndex
   LOCAL cErrorMsg
   LOCAL cFileName

   IF ! Empty( cFileBase )
      aTrans := __i18n_potArrayLoad( cFileBase, @cErrorMsg )
   ENDIF

   IF aTrans == NIL
      aTrans := {}
   ENDIF

   FOR EACH cFileName IN aFiles
      aTrans2 := __i18n_potArrayLoad( cFileName, @cErrorMsg )
      IF aTrans2 != NIL
         __i18n_potArrayJoin( aTrans, aTrans2, @hIndex )
      ELSE
         IF ! lIgnoreError
            _hbmk_OutErr( hbmk, hb_StrFormat( I_( ".pot error: %1$s" ), cErrorMsg ) )
         ENDIF
         cErrorMsg := NIL
      ENDIF
   NEXT

   IF hbmk[ _HBMK_lDEBUGI18N ] .AND. aTrans == NIL
      _hbmk_OutErr( hbmk, "LoadPOTFiles() did not load anything" )
   ENDIF

   RETURN aTrans

STATIC FUNCTION LoadPOTFilesAsHash( hbmk, aFiles )

   LOCAL cErrorMsg
   LOCAL hTrans
   LOCAL aTrans
   LOCAL cFileName

   FOR EACH cFileName IN aFiles
      cErrorMsg := NIL
      aTrans := __i18n_potArrayLoad( cFileName, @cErrorMsg )
      IF aTrans != NIL
         IF hbmk[ _HBMK_lDEBUGI18N ]
            _hbmk_OutStd( hbmk, hb_StrFormat( "LoadPOTFilesAsHash(): %1$s", cFileName ) )
         ENDIF
         hTrans := __i18n_potArrayToHash( aTrans,, hTrans )
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: %1$s" ), cErrorMsg ) )
      ENDIF
   NEXT

   RETURN hTrans

STATIC PROCEDURE POTMerge( hbmk, aFiles, cFileBase, cFileOut )

   LOCAL cErrorMsg
   LOCAL aTrans := LoadPOTFiles( hbmk, aFiles, cFileBase, .T. )

   IF aTrans != NIL
      IF ! __i18n_potArraySave( cFileOut, aTrans, @cErrorMsg, ! hbmk[ _HBMK_lMINIPO ], ! hbmk[ _HBMK_lMINIPO ] )
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( ".pot merge error: %1$s" ), cErrorMsg ) )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE AutoTrans( hbmk, cFileIn, aFiles, cFileOut )

   LOCAL cErrorMsg
   LOCAL hTrans := LoadPOTFilesAsHash( hbmk, aFiles )

   IF hTrans != NIL
      IF ! __i18n_potArraySave( cFileOut, ;
             __i18n_potArrayTrans( LoadPOTFiles( hbmk, {}, cFileIn, .F. ), ;
                                   hTrans ), @cErrorMsg, ! hbmk[ _HBMK_lMINIPO ], ! hbmk[ _HBMK_lMINIPO ] )
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: %1$s" ), cErrorMsg ) )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION GenHBL( hbmk, aFiles, cFileOut, lEmpty )

   LOCAL cHBLBody
   LOCAL pI18N
   LOCAL aTrans := LoadPOTFiles( hbmk, aFiles, NIL, .F. )
   LOCAL lRetVal := .F.

   IF HB_ISARRAY( aTrans )
      pI18N := __i18n_hashTable( __i18n_potArrayToHash( aTrans, lEmpty ) )
      cHBLBody := hb_i18n_SaveTable( pI18N )
      IF hb_MemoWrit( cFileOut, cHBLBody )
         lRetVal := .T.
      ELSE
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Cannot create file: %1$s" ), cFileOut ) )
      ENDIF
   ENDIF

   RETURN lRetVal

STATIC FUNCTION win_implib_command( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   IF ! hb_FileExists( cSourceDLL )
      IF hbmk[ _HBMK_lInfo ]
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Source dynamic library not found: %1$s" ), cSourceDLL ) )
      ENDIF
      RETURN _HBMK_IMPLIB_NOTFOUND
   ENDIF

   hb_default( @cFlags, "" )

   cCommand := AllTrim( hb_StrReplace( cCommand, { ;
      "{FI}" => cFlags, ;
      "{ID}" => FNameEscape( cSourceDLL, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ), ;
      "{OL}" => FNameEscape( cTargetLib, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) } ) )

   IF hbmk[ _HBMK_lTRACE ]
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, I_( "Import library creation command:" ) )
      ENDIF
      OutStd( cCommand + _OUT_EOL )
   ENDIF

   RETURN iif( hb_processRun( cCommand ) == 0, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )

#define _COFF_LIB_SIGNATURE "!<arch>"

STATIC FUNCTION IsCOFFLib( cFileName )

   LOCAL fhnd := FOpen( cFileName, FO_READ )
   LOCAL cBuffer

   IF fhnd != F_ERROR
      cBuffer := Space( hb_BLen( _COFF_LIB_SIGNATURE ) )
      FRead( fhnd, @cBuffer, hb_BLen( cBuffer ) )
      FClose( fhnd )
      IF cBuffer == _COFF_LIB_SIGNATURE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

#define _OMF_LIB_SIGNATURE hb_BChar( 0xF0 )

STATIC FUNCTION IsOMFLib( cFileName )

   LOCAL fhnd := FOpen( cFileName, FO_READ )
   LOCAL cBuffer

   IF fhnd != F_ERROR
      cBuffer := Space( hb_BLen( _OMF_LIB_SIGNATURE ) )
      FRead( fhnd, @cBuffer, hb_BLen( cBuffer ) )
      FClose( fhnd )
      IF cBuffer == _OMF_LIB_SIGNATURE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION win_implib_coff( hbmk, cSourceDLL, cTargetLib )

   LOCAL cSourceLib

   /* Try to find COFF .lib with the same name */
   IF hb_FileExists( cSourceLib := hb_FNameExtSet( cSourceDLL, ".lib" ) )
      IF IsCOFFLib( cSourceLib )
         IF ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, I_( "Found COFF .lib with the same name, falling back to using it instead of the .dll." ) )
         ENDIF
         RETURN iif( hb_FCopy( cSourceLib, cTargetLib ) != F_ERROR, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )
      ENDIF
   ENDIF

   RETURN _HBMK_IMPLIB_NOTFOUND

STATIC FUNCTION win_implib_omf( hbmk, cSourceDLL, cTargetLib )

   LOCAL cSourceLib

   /* Try to find COFF .lib with the same name */
   IF hb_FileExists( cSourceLib := hb_FNameExtSet( cSourceDLL, ".lib" ) )
      IF IsOMFLib( cSourceLib )
         IF ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, I_( "Found OMF .lib with the same name, falling back to using it instead of the .dll." ) )
         ENDIF
         RETURN iif( hb_FCopy( cSourceLib, cTargetLib ) != F_ERROR, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )
      ENDIF
   ENDIF

   RETURN _HBMK_IMPLIB_NOTFOUND

STATIC FUNCTION win_implib_def( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   LOCAL cSourceDef

   /* Try to find .def file with the same name */
   IF hb_FileExists( cSourceDef := hb_FNameExtSet( cSourceDLL, ".def" ) )
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, I_( "Found .def file with the same name, falling back to using it instead of the .dll." ) )
      ENDIF
      RETURN win_implib_command( hbmk, cCommand, cSourceDef, cTargetLib, cFlags )
   ENDIF

   RETURN _HBMK_IMPLIB_NOTFOUND

STATIC FUNCTION win_implib_copy( hbmk, cSourceDLL, cTargetLib )

   HB_SYMBOL_UNUSED( hbmk )

   IF hb_FileExists( cSourceDLL )
      /* Use .dll directly if all other attempts failed */
      RETURN iif( hb_FCopy( cSourceDLL, cTargetLib ) != F_ERROR, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )
   ENDIF

   RETURN _HBMK_IMPLIB_NOTFOUND

/* NOTE: There is a big problem with mingw/cygwin 'ld' linker:
         It cannot properly link stdcall decorated (_sym@nn) function names
         directly with .dlls, since in .dlls the decoration is stripped from
         the exported symbols. So, it _requires_ a .def file or a COFF import .lib
         which have the the decorated version of the symbols. Such .def/.lib
         file cannot be automatically generated from the .dll, as the
         decoration needs to be rebuilt based on function parameters.
         Not even 'ld' option '--enable-stdcall-fixup' ("Link _sym to _sym@nn without warnings")
         option will help the case, since we'd need a "Link _sym@nn to _sym"
         option. For some reason and despite the frequent complaints, gcc
         developers failed to add such option since year ~2000.
         To circumvent that and make it possible for Harbour users to
         effortlessly generate implibs from .dlls, we cannot do more than
         rely on .dll distributors to provide .def or COFF import .libs
         and make use of these automatically if they are available.
         Hopefully one day gcc will introduce a feature to make such tricks
         unnecessary and make it possible to create proper implibs out of
         ordinary .dlls, like with every other compiler.
         [vszakats] */

STATIC FUNCTION win_implib_command_gcc( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   LOCAL nResult

   IF ( nResult := win_implib_coff( hbmk, cSourceDLL, cTargetLib ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   IF ( nResult := win_implib_def( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   RETURN win_implib_copy( hbmk, cSourceDLL, cTargetLib )

STATIC FUNCTION win_implib_command_bcc( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   LOCAL nResult

   IF ( nResult := win_implib_omf( hbmk, cSourceDLL, cTargetLib ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   IF ( nResult := win_implib_def( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   RETURN win_implib_command( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

STATIC FUNCTION win_implib_command_watcom( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )
   RETURN win_implib_command( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

STATIC FUNCTION win_implib_command_pocc( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   LOCAL nResult

   IF ( nResult := win_implib_coff( hbmk, cSourceDLL, cTargetLib ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   RETURN win_implib_command( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

STATIC FUNCTION win_implib_command_msvc( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   LOCAL nResult

   LOCAL cExports
   LOCAL fhnd
   LOCAL cSourceDef
   LOCAL cLine
   LOCAL tmp
   LOCAL aCols

   LOCAL cFuncList

   LOCAL cCommandDump

   IF ( nResult := win_implib_coff( hbmk, cSourceDLL, cTargetLib ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   IF ( nResult := win_implib_def( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags ) ) != _HBMK_IMPLIB_NOTFOUND
      RETURN nResult
   ENDIF

   IF ! hb_FileExists( cSourceDLL )
      IF hbmk[ _HBMK_lInfo ]
         _hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Source dynamic library not found: %1$s" ), cSourceDLL ) )
      ENDIF
      RETURN _HBMK_IMPLIB_NOTFOUND
   ENDIF

   cCommandDump := "dumpbin.exe -exports {ID}"
   cCommandDump := hb_StrReplace( cCommandDump, { ;
      "{ID}" => FNameEscape( cSourceDLL, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) } )

   IF hbmk[ _HBMK_lTRACE ]
      IF ! hbmk[ _HBMK_lQuiet ]
         _hbmk_OutStd( hbmk, I_( "Import library creation command:" ) )
      ENDIF
      OutStd( cCommandDump + _OUT_EOL )
   ENDIF

   nResult := _HBMK_IMPLIB_FAILED

   IF hb_processRun( cCommandDump,, @cExports ) == 0

      cFuncList := ;
         "LIBRARY " + '"' + hb_FNameNameExt( cSourceDLL ) + '"' + hb_eol() + ;
         "EXPORTS" + hb_eol()

      cExports := StrTran( cExports, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )

      tmp := At( "ordinal hint", cExports )
      IF tmp > 0
         cExports := SubStr( cExports, tmp + Len( "ordinal hint" ) )
      ENDIF

      FOR EACH cLine IN hb_ATokens( cExports, Chr( 10 ) )
         IF ! Empty( cLine )
            aCols := hb_ATokens( cLine )
            IF Len( aCols ) >= 4
               cFuncList += aCols[ 4 ] + hb_eol()
            ENDIF
         ENDIF
      NEXT

      fhnd := hb_FTempCreateEx( @cSourceDef )
      IF fhnd != F_ERROR
         FWrite( fhnd, cFuncList )
         FClose( fhnd )

         nResult := win_implib_command( hbmk, cCommand, cSourceDef, cTargetLib, cFlags )

         FErase( cSourceDef )
      ENDIF
   ENDIF

   RETURN nResult

#define _VCS_UNKNOWN            0
#define _VCS_SVN                1
#define _VCS_GIT                2
#define _VCS_MERCURIAL          3
#define _VCS_CVS                4
#define _VCS_BAZAAR             5
#define _VCS_FOSSIL             6
#define _VCS_MONOTONE           7

STATIC FUNCTION VCSDetect( cDir )

   hb_default( @cDir, "" )

   IF ! Empty( cDir )
      cDir := hb_DirSepAdd( cDir )
   ENDIF

   DO CASE
   CASE hb_DirExists( cDir + ".svn" )      ; RETURN _VCS_SVN
   CASE hb_DirExists( cDir + ".git" )      ; RETURN _VCS_GIT
   CASE hb_DirExists( cDir + ".hg" )       ; RETURN _VCS_MERCURIAL
   CASE hb_DirExists( cDir + ".bzr" )      ; RETURN _VCS_BAZAAR
   CASE hb_FileExists( cDir + "_FOSSIL_" ) ; RETURN _VCS_FOSSIL
   CASE hb_DirExists( cDir + "_MTN" )      ; RETURN _VCS_MONOTONE
   CASE hb_DirExists( cDir + "CVS" )       ; RETURN _VCS_CVS
   CASE hb_DirExists( cDir + "_svn" )      ; RETURN _VCS_SVN /* NOTE: When SVN_ASP_DOT_NET_HACK envvar is set. [vszakats] */
   ENDCASE

   RETURN _VCS_UNKNOWN

STATIC FUNCTION VCSID( hbmk, cDir, cVCSHEAD, /* @ */ cType )

   LOCAL cStdOut
   LOCAL nType := VCSDetect( cDir )
   LOCAL cCommand
   LOCAL cResult := ""
   LOCAL tmp, tmp1

   SWITCH nType
   CASE _VCS_SVN
      cType := "svn"
      cCommand := "svnversion " + iif( Empty( cDir ), ".", cDir )
      EXIT
   CASE _VCS_GIT
      cType := "git"
      cCommand := "git" + iif( Empty( cDir ), "", " --work-tree=" + cDir + ".git" ) + " rev-parse --short HEAD"
      EXIT
   CASE _VCS_MERCURIAL
      cType := "mercurial"
      cCommand := "hg head" + iif( Empty( cDir ), "", " -R " + cDir )
      EXIT
   CASE _VCS_CVS
      cType := "cvs"
      EXIT
   CASE _VCS_BAZAAR
      cType := "bazaar"
      cCommand := "bzr version-info" + iif( Empty( cDir ), "", " " + cDir )
      EXIT
   CASE _VCS_FOSSIL
      cType := "fossil"
      cCommand := "fossil info"
      EXIT
   CASE _VCS_MONOTONE
      cType := "monotone"
      cCommand := "mtn status"
      EXIT
   OTHERWISE
      /* No version control system detected, roll our own. */
      cType := "hbmk"
      cCommand := NIL
      cResult := "1"
      cStdOut := hb_MemoRead( cVCSHEAD )
      tmp1 := "#define _HBMK_VCS_ID_"
      tmp := At( tmp1, cStdOut )
      IF tmp > 0
         cStdOut := SubStr( cStdOut, tmp + Len( tmp1 ) + 1 )
         tmp := At( Chr( 10 ), cStdOut )
         IF tmp > 0
            cStdOut := Left( cStdOut, tmp - 1 )
            cResult := hb_ntos( Val( AllTrim( StrTran( cStdOut, '"' ) ) ) + 1 )
         ENDIF
      ENDIF
   ENDSWITCH

   IF ! Empty( cCommand )

      IF hbmk[ _HBMK_lTRACE ]
         IF ! hbmk[ _HBMK_lQuiet ]
            _hbmk_OutStd( hbmk, I_( "VCS version command:" ) )
         ENDIF
         OutStd( cCommand + _OUT_EOL )
      ENDIF

      hb_processRun( cCommand,, @cStdOut )

      SWITCH nType
      CASE _VCS_SVN
         /* 10959<n> */
      CASE _VCS_GIT
         /* fe3bb56<n> */
         cStdOut := StrTran( cStdOut, Chr( 13 ) )
         cResult := StrTran( cStdOut, Chr( 10 ) )
         EXIT
      CASE _VCS_MERCURIAL
         /* changeset:   696:9e33729cafae<n>... */
         tmp := At( Chr( 10 ), cStdOut )
         IF tmp > 0
            cStdOut := Left( cStdOut, tmp - 1 )
            cResult := AllTrim( StrTran( cStdOut, "changeset:" ) )
         ENDIF
         EXIT
      CASE _VCS_BAZAAR
         /* revision-id: pqm@pqm.ubuntu.com-20090813025005-k2k8pa2o38b8m0l8
            date: 2009-08-13 03:50:05 +0100
            build-date: 2009-08-13 16:53:32 +0200
            revno: 4602
            branch-nick: bzr */
         tmp := At( "revno: ", cStdOut )
         IF tmp > 0
            cStdOut := SubStr( cStdOut, tmp + Len( "revno: " ) )
            tmp := At( Chr( 10 ), cStdOut )
            IF tmp > 0
               cResult := Left( cStdOut, tmp - 1 )
            ENDIF
         ENDIF
         EXIT
      CASE _VCS_FOSSIL
         /* project-name: Fossil
            repository:   C:/fossil/fossil.fossil
            local-root:   C:/fossil/src/
            project-code: CE59BB9F186226D80E49D1FA2DB29F935CCA0333
            server-code:  9fbdba27d27885515cbf885579dbffa7cebd8d95
            checkout:     c774e298c3f213f7487fb0ba638edfa3f1b89edf 2009-09-18 20:58:06 UTC
            parent:       0eb08b860c5b851c074113fd459d1cd0671f4805 2009-09-16 21:29:18 UTC
            tags:         trunk
          */
         tmp := At( "checkout:", cStdOut )
         IF tmp > 0
            cStdOut := LTrim( SubStr( cStdOut, tmp + Len( "checkout:" ) ) )
            tmp := At( " ", cStdOut )
            IF tmp > 0
               cResult := Left( cStdOut, tmp - 1 )
            ENDIF
         ENDIF
         EXIT
      CASE _VCS_MONOTONE
         /* ----------------------------------------------------------------------
            Revision: c79f2332a1e9036bb52ac1f412b92e6a69fc9071
            Parent:   bf8b93290ea4e8e946961f51c47f8f4638f65372
            Author:   ???
            Date:     2010.07.14. 1:11:47
            Branch:   free.lp.se:LPlib

            Changes against parent bf8b93290ea4e8e946961f51c47f8f4638f65372

            no changes
          */
         tmp := At( "Revision:", cStdOut )
         IF tmp > 0
            cStdOut := StrTran( LTrim( SubStr( cStdOut, tmp + Len( "Revision:" ) ) ), Chr( 13 ) )
            tmp := At( Chr( 10 ), cStdOut )
            IF tmp > 0
               cResult := Left( cStdOut, tmp - 1 )
            ENDIF
         ENDIF
         EXIT
      ENDSWITCH
   ENDIF

   RETURN cResult

STATIC FUNCTION hbmk_TARGETNAME( hbmk )
   RETURN iif( hbmk[ _HBMK_nArgTarget ] == 0, _HBMK_TARGENAME_ADHOC, PathSepToForward( hbmk[ _HBMK_aArgs ][ hbmk[ _HBMK_nArgTarget ] ] ) )

STATIC FUNCTION hbmk_TARGETTYPE( hbmk )

   IF hbmk[ _HBMK_lContainer ]                                           ; RETURN "hbcontainer"
   ELSEIF hbmk[ _HBMK_lCreateLib ]                                       ; RETURN "hblib"
   ELSEIF hbmk[ _HBMK_lCreateDyn ] .AND. ! hbmk[ _HBMK_lDynVM ]          ; RETURN "hbdyn"
   ELSEIF hbmk[ _HBMK_lCreateDyn ] .AND. hbmk[ _HBMK_lDynVM ]            ; RETURN "hbdynvm"
   ELSEIF hbmk[ _HBMK_lCreateImpLib ]                                    ; RETURN "hbimplib"
   ELSEIF hbmk[ _HBMK_lStopAfterHarbour ] .AND. hbmk[ _HBMK_lCreatePPO ] ; RETURN "hbppo"
   ELSEIF hbmk[ _HBMK_lStopAfterHarbour ] .AND. hbmk[ _HBMK_lCreateHRB ] ; RETURN "hbhrb"
   ENDIF

   RETURN "hbexe"

STATIC FUNCTION hbmk_CPU( hbmk )

   DO CASE
   CASE HBMK_ISPLAT( "dos|os2|cygwin" ) .OR. ;
        HBMK_ISCOMP( "mingw|msvc|pocc|watcom|bcc|xcc" ) .OR. ;
        ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "icc" )
      RETURN "x86"
   CASE HBMK_ISCOMP( "gcc|icc|clang|sunpro|diab|pcc|tcc" )
      /* TOFIX: This is not necessarily correct, since these inherit the
                default CPU architecture from OS default, by and large,
                and targets can be overridden using user options. */
      RETURN "x86"
   CASE hbmk[ _HBMK_cCOMP ] == "mingw64" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "msvc64" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "bcc64" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "pocc64"
      RETURN "x86_64"
   CASE hbmk[ _HBMK_cCOMP ] == "msvcia64" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "iccia64"
      RETURN "ia64"
   CASE hbmk[ _HBMK_cCOMP ] == "mingwarm" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "msvcarm" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "poccarm"
      RETURN "arm"
   CASE hbmk[ _HBMK_cCOMP ] == "msvcmips"
      RETURN "mips"
   CASE hbmk[ _HBMK_cCOMP ] == "msvcsh"
      RETURN "sh"
   ENDCASE

   RETURN ""

/* Return standard dynamic lib name suffix used by Harbour */
STATIC FUNCTION hbmk_DYNSUFFIX( hbmk )

   SWITCH hbmk[ _HBMK_cPLAT ]
   CASE "wce"
      RETURN "-wce-" + hbmk[ _HBMK_cCPU ]
   CASE "win"
      DO CASE
      CASE hbmk[ _HBMK_cCOMP ] == "bcc"
         RETURN "-bcc"
      CASE hbmk[ _HBMK_cCPU ] == "x86_64"
         RETURN "-x64"
      CASE !( hbmk[ _HBMK_cCPU ] == "x86" )
         RETURN "-" + hbmk[ _HBMK_cCPU ]
      ENDCASE
      EXIT
   ENDSWITCH

   RETURN ""

/* Keep this public, it is used from macro. */
FUNCTION hbmk_KEYW( hbmk, cFileName, cKeyword, cValue, cOperator )

   LOCAL tmp

   IF cKeyword == hbmk[ _HBMK_cPLAT ] .OR. ;
      cKeyword == hbmk[ _HBMK_cCOMP ]
      RETURN .T.
   ENDIF

   SWITCH cKeyword
   CASE "mt"       ; RETURN hbmk[ _HBMK_lMT ]
   CASE "st"       ; RETURN ! hbmk[ _HBMK_lMT ]
   CASE "gui"      ; RETURN hbmk[ _HBMK_lGUI ]
   CASE "std"      ; RETURN ! hbmk[ _HBMK_lGUI ]
   CASE "debug"    ; RETURN hbmk[ _HBMK_lDEBUG ]
   CASE "nodebug"  ; RETURN ! hbmk[ _HBMK_lDEBUG ]
   CASE "shared"   ; RETURN hbmk[ _HBMK_lSHARED ]
   CASE "static"   ; RETURN ! hbmk[ _HBMK_lSHARED ]
   CASE "lngcpp"   ; RETURN hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
   CASE "lngc"     ; RETURN hbmk[ _HBMK_lCPP ] != NIL .AND. ! hbmk[ _HBMK_lCPP ]
   CASE "winuni"   ; RETURN hbmk[ _HBMK_lWINUNI ]
   CASE "winansi"  ; RETURN ! hbmk[ _HBMK_lWINUNI ]
   CASE "unix"     ; RETURN HBMK_ISPLAT( "bsd|hpux|sunos|beos|qnx|android|vxworks|symbian|linux|darwin|cygwin|minix|aix" )
   CASE "allwin"   ; RETURN HBMK_ISPLAT( "win|wce" )
   CASE "allgcc"   ; RETURN HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|djgpp|gccomf|clang|open64|pcc" )
   CASE "allmingw" ; RETURN HBMK_ISCOMP( "mingw|mingw64|mingwarm" )
   CASE "allmsvc"  ; RETURN HBMK_ISCOMP( "msvc|msvc64|msvcia64|msvcarm" )
   CASE "allbcc"   ; RETURN HBMK_ISCOMP( "bcc|bcc64" )
   CASE "allpocc"  ; RETURN HBMK_ISCOMP( "pocc|pocc64|poccarm" )
   CASE "allicc"   ; RETURN HBMK_ISCOMP( "icc|iccia64" )
   CASE "xhb"      ; RETURN _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
   CASE "hb10"     ; RETURN hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10
   CASE "hb20"     ; RETURN hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB20
   CASE "hb30"     ; RETURN hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB30
   ENDSWITCH

   IF cKeyword == hbmk_CPU( hbmk )
      RETURN .T.
   ENDIF

   IF cKeyword == hbmk_TARGETTYPE( hbmk )
      RETURN .T.
   ENDIF

   IF ! HBMK_IS_IN( cKeyword, ;
      "|win|wce|dos|os2" + ;
      "|bsd|hpux|sunos|beos|qnx|android|vxworks|symbian|linux|darwin|cygwin|minix|aix" + ;
      "|msvc|msvc64|msvcia64|msvcarm" + ;
      "|pocc|pocc64|poccarm|xcc|tcc" + ;
      "|mingw|mingw64|mingwarm|bcc|bcc64|watcom" + ;
      "|gcc|gccomf|djgpp" + ;
      "|hblib|hbdyn|hbdynvm|hbimplib|hbexe" + ;
      "|icc|iccia64|clang|open64|sunpro|diab|pcc" + ;
      "|x86|x86_64|ia64|arm|mips|sh" )

      /* handle pseudo-functions */
      IF cOperator == "=" .AND. cValue != NIL
         SWITCH cKeyword
         CASE "hb_ispath"
            cValue := hb_PathNormalize( PathMakeAbsolute( hb_DirSepToOS( MacroProc( hbmk, cValue, cFileName ) ), cFileName ) )
            RETURN iif( Empty( hb_FNameNameExt( cValue ) ), hb_DirExists( cValue ), hb_FileExists( cValue ) )
         ENDSWITCH
      ENDIF

      tmp := MacroGet( hbmk, cKeyWord, "" )
      IF cValue != NIL
         SWITCH cOperator
         CASE "="
            RETURN Lower( tmp ) == Lower( cValue )
         CASE ">"
            RETURN Lower( tmp ) > Lower( cValue )
         CASE "<"
            RETURN Lower( tmp ) < Lower( cValue )
         ENDSWITCH
      ELSE
         IF ! Empty( tmp ) .AND. !( tmp == "0" ) .AND. !( Lower( tmp ) == "no" )
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

STATIC PROCEDURE ParseCOMPPLATCPU( hbmk, cString, nMainTarget )

   LOCAL aToken := hb_ATokens( Lower( cString ), "/", .T., .T. )
   LOCAL cToken

   IF Len( aToken ) == 1
      IF ! Empty( cString )
         SWITCH nMainTarget
         CASE _TARG_PLAT ; hbmk[ _HBMK_cPLAT ] := AllTrim( cString ) ; EXIT
         CASE _TARG_COMP ; hbmk[ _HBMK_cCOMP ] := AllTrim( cString ) ; EXIT
         CASE _TARG_CPU  ; hbmk[ _HBMK_cCPU ]  := AllTrim( cString ) ; EXIT
         ENDSWITCH
      ENDIF
   ELSE
      FOR EACH cToken IN aToken
         IF ! Empty( cToken )
            SWITCH cToken:__enumIndex()
            CASE 1 ; hbmk[ _HBMK_cPLAT ] := AllTrim( cToken ) ; EXIT
            CASE 2 ; hbmk[ _HBMK_cCOMP ] := AllTrim( cToken ) ; EXIT
            CASE 3 ; hbmk[ _HBMK_cCPU ]  := AllTrim( cToken ) ; EXIT
            ENDSWITCH
            IF cToken:__enumIndex() > 3
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION Apple_App_Template_Files( hbmk, cFile, cPROGNAME )

   LOCAL cString

   SWITCH cFile
   CASE "Info.plist"
      cString := Apple_App_Template_Info_plist()
      EXIT
   CASE "PkgInfo"
      cString := "%__APPTYPE__%%__APPSIGN__%"
      EXIT
   OTHERWISE
      cString := ""
   ENDSWITCH

   RETURN hb_StrReplace( cString, { ;
      "%__APPNAME__%"      => cPROGNAME, ;
      "%__APPTYPE__%"      => "APPL", ;
      "%__APPSIGN__%"      => PadR( cPROGNAME, 4, "?" ), ;
      "%__APPID__%"        => /* TODO */ "%__APPID__%", ;
      "%__APPVERSION__%"   => /* TODO */ "%__APPVERSION__%", ;
      "%__APPCOPYRIGHT__%" => /* TODO */ "%__APPCOPYRIGHT__%", ;
      "%__APPICON__%"      => iif( Empty( hbmk[ _HBMK_aICON ] ), "", hb_FNameNameExt( hbmk[ _HBMK_aICON ][ 1 ] ) ) } )

STATIC FUNCTION Apple_App_Template_Info_plist()
#pragma __cstream | RETURN %s
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
<plist version="0.9">
<dict>
\t<key>CFBundleInfoDictionaryVersion</key>
\t<string>6.0</string>
\t<key>CFBundleIdentifier</key>
\t<string>%__APPID__%</string>
\t<key>CFBundleDevelopmentRegion</key>
\t<string>English</string>
\t<key>CFBundleExecutable</key>
\t<string>%__APPNAME__%</string>
\t<key>CFBundleIconFile</key>
\t<string>%__APPICON__%</string>
\t<key>CFBundleName</key>
\t<string>%__APPNAME__%</string>
\t<key>CFBundlePackageType</key>
\t<string>%__APPTYPE__%</string>
\t<key>CFBundleSignature</key>
\t<string>%__APPSIGN__%</string>
\t<key>CFBundleGetInfoString</key>
\t<string>%__APPNAME__% version %__APPVERSION__%, %__APPCOPYRIGHT__%</string>
\t<key>CFBundleLongVersionString</key>
\t<string>%__APPVERSION__%, %__APPCOPYRIGHT__%</string>
\t<key>NSHumanReadableCopyright</key>
\t<string>%__APPCOPYRIGHT__%</string>
\t<key>LSRequiresCarbon</key>
\t<true/>
\t<key>CSResourcesFileMapped</key>
\t<true/>
</dict>
</plist>
#pragma __endtext

STATIC FUNCTION hbmk_hb_processRunCatch( cCommand, /* @ */ cStdOutErr )

   LOCAL hProc
   LOCAL hOutErr

   LOCAL cData
   LOCAL nLen

   LOCAL nExitCode

   cStdOutErr := ""

   IF ( hProc := hb_processOpen( cCommand,, @hOutErr, @hOutErr ) ) != F_ERROR

      cData := Space( 1024 )
      DO WHILE ( nLen := FRead( hOutErr, @cData, hb_BLen( cData ) ) ) > 0
         cStdOutErr += hb_BLeft( cData, nLen )
      ENDDO

      nExitCode := hb_processValue( hProc )

      FClose( hOutErr )

      OutStd( cStdOutErr )
   ELSE
      nExitCode := -999
   ENDIF

   RETURN nExitCode

/* lGenericFind == .F.: advise missing libs
   lGenericFind == .T.: generic function finder (supports core functions and partial matches, different output) */
STATIC PROCEDURE ShowFunctionProviders( hbmk, aFunction, lGenericFind )

   LOCAL hAll := GetListOfFunctionsKnown( hbmk, lGenericFind )
   LOCAL cFunction
   LOCAL lFound
   LOCAL aLib
   LOCAL tmp, tmp1

   LOCAL hNeeded := { => }

   LOCAL bAdd := ;
      {| cFunction |
         LOCAL cLib
         FOR EACH cLib IN hb_ATokens( hAll[ cFunction ], "," )
            IF !( cLib $ hNeeded )
               hNeeded[ cLib ] := {}
            ENDIF
            AAddNew( hNeeded[ cLib ], cFunction )
         NEXT
         RETURN NIL
      }

   FOR EACH cFunction IN aFunction DESCEND
      lFound := .F.
      IF ( tmp := hb_HPos( hAll, cFunction ) ) > 0
         Eval( bAdd, hb_HKeyAt( hAll, tmp ) /* Get the function name in original .hbx casing */ )
         lFound := .T.
      ENDIF
      IF lGenericFind
         FOR EACH tmp1 IN hAll
            IF hb_WildMatchI( "*" + cFunction + "*", tmp1:__enumKey() )
               Eval( bAdd, tmp1:__enumKey() )
               lFound := .T.
            ENDIF
         NEXT
      ENDIF
      IF lFound
         hb_ADel( aFunction, cFunction:__enumIndex(), .T. )
      ENDIF
   NEXT

   IF lGenericFind
      IF Empty( hNeeded )
         OutStd( I_( "No matches" ) + _OUT_EOL )
      ELSE
         FOR EACH tmp IN hNeeded
            aLib := LibReferenceToOption( hbmk, tmp:__enumKey() )
            OutStd( hb_StrFormat( I_( "%1$s %2$s:" ), aLib[ 1 ], iif( aLib[ 2 ], I_( "(installed)" ), I_( "(not installed)" ) ) ) + _OUT_EOL )
            FOR EACH tmp1 IN tmp
               OutStd( "   " + tmp1 + "()" + _OUT_EOL )
            NEXT
         NEXT
      ENDIF
   ELSE
      IF ! Empty( hNeeded ) .OR. ;
         ! Empty( aFunction )
         /* Just an empty separator for better visibility
            of what follows */
         OutStd( _OUT_EOL )
      ENDIF

      FOR EACH tmp IN hNeeded
         aLib := LibReferenceToOption( hbmk, tmp:__enumKey() )
         _hbmk_OutStd( hbmk, hb_StrFormat( ;
            iif( aLib[ 2 ], ;
               I_( "Hint: Add option '%1$s' for missing function(s): %2$s" ), ;
               I_( "Hint: Install package %3$s and add option '%1$s' for missing function(s): %2$s" ) ), ;
            aLib[ 1 ], ;
            ArrayToList( tmp, ", ",,,, "()" ), ;
            hb_FNameName( aLib[ 1 ] ) ) )
      NEXT

      IF ! Empty( aFunction )
         _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Error: Referenced, missing, but unknown function(s): %1$s" ), ArrayToList( aFunction, ", ",,,, "()" ) ) )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION LibReferenceToOption( hbmk, cLib )

   LOCAL lInstalled

   DO CASE
   CASE cLib == "(hbcore)"
      cLib := I_( "Harbour core" )
      lInstalled := .T.
   CASE Left( cLib, 1 ) == "("
      cLib := "-l" + SubStr( cLib, 2, Len( cLib ) - 2 )
      lInstalled := .T.
   OTHERWISE
      lInstalled := hb_FileExists( hb_DirSepAdd( hbmk[ _HBMK_cHB_INSTALL_PFX ] ) + hb_FNameExtSet( hb_DirSepToOS( cLib ), ".hbc" ) )
      /* detect autofind libs */
      IF ( Left( cLib, Len( _HBMK_SPECDIR_CONTRIB + "/" ) ) == _HBMK_SPECDIR_CONTRIB + "/" .AND. _HBMK_SPECDIR_CONTRIB + "/" + hb_FNameName( cLib ) + "/" + hb_FNameName( cLib ) + ".hbx" == cLib ) .OR. ;
         ( Left( cLib, Len( _HBMK_SPECDIR_ADDONS  + "/" ) ) == _HBMK_SPECDIR_ADDONS  + "/" .AND. _HBMK_SPECDIR_ADDONS  + "/" + hb_FNameName( cLib ) + "/" + hb_FNameName( cLib ) + ".hbx" == cLib )
         cLib := hb_FNameName( cLib ) + ".hbc"
      ELSE
         cLib := hb_FNameExtSet( hb_DirSepToOS( cLib ), ".hbc" )
      ENDIF
   ENDCASE

   RETURN { cLib, lInstalled }

STATIC FUNCTION ExtractHarbourSymbols( cString )

   STATIC sc_aException := { ;
      "multiple definition", ; /* gcc */
      "duplicate symbol", ; /* clang */
      "already defined", ; /* msvc */
      "defined in both", ; /* bcc */
      "previous definition different" } /* dmc */

   LOCAL aList := {}

   LOCAL pRegex
   LOCAL tmp
   LOCAL cLine

   LOCAL cOldCP := hb_cdpSelect( "EN" )

   IF ! Empty( pRegex := hb_regexComp( R_( "HB_FUN_([A-Z_][A-Z_0-9]*)" ), .T., .T. ) )
      FOR EACH cLine IN hb_ATokens( StrTran( cString, Chr( 13 ), Chr( 10 ) ), Chr( 10 ) )
         IF AScan( sc_aException, {| tmp | tmp $ Lower( cLine ) } ) == 0
            FOR EACH tmp IN hb_regexAll( pRegex, cLine,,,,, .T. )
               AAddNew( aList, tmp[ 2 ] )
            NEXT
         ENDIF
      NEXT
   ENDIF

   hb_cdpSelect( cOldCP )

   RETURN aList

STATIC FUNCTION GetListOfFunctionsKnown( hbmk, lIncludeCore )

   LOCAL hAll := { => }
   LOCAL aFile

   hb_HCaseMatch( hAll, .F. )

   FOR EACH aFile IN Directory( hb_DirBase() + "*.hbr" )
      /* TOFIX: To handle function names present in multiple containers */
      hb_HMerge( hAll, hb_Deserialize( hb_ZUncompress( hb_MemoRead( hb_DirBase() + aFile[ F_NAME ] ) ) ) )
   NEXT

   IF lIncludeCore
      GetListOfFunctionsKnownWalkDir( hbmk[ _HBMK_cHB_INSTALL_INC ], "", hAll, "(hbcore)" )
   ENDIF

   hAll[ "hb_compile" ] := ;
   hAll[ "hb_compileBuf" ] := ;
   hAll[ "hb_compileFromBuff" ] := "(hbcplr)"

   GetListOfFunctionsKnownWalkDir( hbmk[ _HBMK_cHB_INSTALL_CON ], hb_FNameDir( hbmk[ _HBMK_cHB_INSTALL_CON ] ), hAll )
   GetListOfFunctionsKnownWalkDir( hbmk[ _HBMK_cHB_INSTALL_ADD ], hb_FNameDir( hbmk[ _HBMK_cHB_INSTALL_ADD ] ), hAll )

   RETURN hAll

STATIC PROCEDURE GetListOfFunctionsKnownWalkDir( cDir, cRoot, hAll, cName )

   LOCAL aFile

   cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )

   FOR EACH aFile IN hb_DirScan( cDir, "*.hbx" )
      GetListOfFunctionsKnownLoadHBX( cDir + aFile[ F_NAME ], cRoot, hAll, cName )
   NEXT

   RETURN

STATIC PROCEDURE GetListOfFunctionsKnownLoadHBX( cFileName, cRoot, hAll, cName )

   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp
   LOCAL cFilter

   IF ! HB_ISSTRING( cName )
      cName := StrTran( SubStr( cFileName, Len( cRoot ) + 1 ), "\", "/" )
   ENDIF

   IF ! Empty( cFile := hb_MemoRead( cFileName ) )

      FOR EACH cFilter IN { ;
         R_( "^DYNAMIC ([a-zA-Z0-9_]*)$" ), ;
         R_( "ANNOUNCE ([a-zA-Z0-9_]*)$" ) }

         IF ! Empty( pRegex := hb_regexComp( cFilter, .T., .T. ) )
            FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
               IF tmp[ 2 ] $ hAll
                  hAll[ tmp[ 2 ] ] += "," + cName
               ELSE
                  hAll[ tmp[ 2 ] ] := cName
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION mk_extern( hbmk, cInputName, cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex, cOutputName )

   LOCAL aExtern

   IF ( aExtern := __hb_extern_get_list( hbmk, cInputName, cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex ) ) != NIL
      __hb_extern_gen( hbmk, aExtern, cOutputName )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION __hb_extern_get_list( hbmk, cInputName, cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex )

   LOCAL aExtern := NIL
   LOCAL hExtern

   LOCAL cStdOut, cStdErr
   LOCAL cTempFile
   LOCAL pRegex
   LOCAL aResult
   LOCAL tmp

   IF ! Empty( cBin_LibHBX ) .AND. ;
      ! Empty( cLibHBX_Regex )

      IF hb_FileExists( cInputName )

         IF "{OT}" $ cOpt_LibHBX
            FClose( hb_FTempCreateEx( @cTempFile,,, ".tmp" ) )
         ELSE
            cTempFile := ""
         ENDIF

         cOpt_LibHBX := hb_StrReplace( cOpt_LibHBX, { ;
            "{LI}" => FNameEscape( cInputName, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ), ;
            "{OT}" => FNameEscape( cTempFile, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) } )

         IF hb_processRun( cBin_LibHBX + " " + cOpt_LibHBX,, @cStdOut, @cStdErr ) == 0
            IF ! Empty( cTempFile )
               cStdOut := MemoRead( cTempFile )
            ENDIF
            IF ! Empty( pRegex := hb_regexComp( cLibHBX_Regex, .T., .T. ) )
               aResult := hb_regexAll( pRegex, StrTran( cStdOut, Chr( 13 ) ),,,,, .T. )
               aExtern := {}
               hExtern := { => }
               FOR EACH tmp IN aResult
                  tmp[ 2 ] := hb_asciiUpper( tmp[ 2 ] )
                  IF !( tmp[ 2 ] $ hExtern )
                     AAdd( aExtern, tmp[ 2 ] )
                     hExtern[ tmp[ 2 ] ] := NIL
                  ENDIF
               NEXT
               tmp := hb_cdpSelect( "EN" )
               ASort( aExtern,,, {| tmp, tmp1 | tmp < tmp1 } )
               hb_cdpSelect( tmp )
            ENDIF
         ENDIF

         IF ! Empty( cTempFile )
            FErase( cTempFile )
         ENDIF
      ENDIF
   ENDIF

   RETURN aExtern

#define _HB_FUNC_INCLUDE_ "HB_FUNC_INCLUDE"
#define _HB_FUNC_EXCLUDE_ "HB_FUNC_EXCLUDE"

#define _HB_SELF_PREFIX   "__HBEXTERN__"
#define _HB_SELF_SUFFIX   "__"

STATIC PROCEDURE __hb_extern_get_exception_list( cInputName, /* @ */ aInclude, /* @ */ aExclude, /* @ */ hDynamic )

   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp

   aInclude := {}
   aExclude := {}
   hDynamic := { => }

   IF ! Empty( cFile := MemoRead( cInputName ) )
      IF ! Empty( pRegex := hb_regexComp( R_( "[\s]" + _HB_FUNC_INCLUDE_ + "[\s]([a-zA-Z0-9_].[^ \t\n\r]*)" ), .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            AAdd( aInclude, tmp[ 2 ] )
         NEXT
      ENDIF
      IF ! Empty( pRegex := hb_regexComp( R_( "[\s]" + _HB_FUNC_EXCLUDE_ + "[\s]([a-zA-Z0-9_].[^ \t\n\r]*)" ), .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            AAdd( aExclude, tmp[ 2 ] )
         NEXT
      ENDIF
      IF ! Empty( pRegex := hb_regexComp( R_( "^DYNAMIC ([a-zA-Z0-9_]*)$" ), .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            hDynamic[ Upper( tmp[ 2 ] ) ] := tmp[ 2 ]
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION __hb_extern_gen( hbmk, aFuncList, cOutputName )

   LOCAL aExtern
   LOCAL cExtern
   LOCAL tmp

   LOCAL aInclude
   LOCAL aExclude
   LOCAL hDynamic

   LOCAL cSelfName := _HB_SELF_PREFIX + StrToDefine( hb_FNameName( cOutputName ) ) + _HB_SELF_SUFFIX

   LOCAL cLine := "/* " + Replicate( "-", 68 ) + hb_eol()
   LOCAL cHelp := ;
      " *          Syntax: // HB_FUNC_INCLUDE <func>" + hb_eol() + ;
      " *                  // HB_FUNC_EXCLUDE <func>" + hb_eol() + ;
      " */" + hb_eol()

   __hb_extern_get_exception_list( cOutputName, @aInclude, @aExclude, @hDynamic )

   cExtern := ""
   IF Empty( aInclude ) .AND. ;
      Empty( aExclude )
      cExtern += cLine
      cExtern += " * NOTE: You can add manual override which functions to include or" + hb_eol()
      cExtern += " *       exclude from automatically generated EXTERNAL/DYNAMIC list." + hb_eol()
      cExtern += cHelp
   ELSE
      cExtern += cLine
      cExtern += " * NOTE: Following comments are control commands for the generator." + hb_eol()
      cExtern += " *       Do not edit them unless you know what you are doing." + hb_eol()
      cExtern += cHelp
      IF ! Empty( aInclude )
         cExtern += hb_eol()
         FOR EACH tmp IN aInclude
            cExtern += "// " + _HB_FUNC_INCLUDE_ + " " + tmp + hb_eol()
         NEXT
      ENDIF
      IF ! Empty( aExclude )
         cExtern += hb_eol()
         FOR EACH tmp IN aExclude
            cExtern += "// " + _HB_FUNC_EXCLUDE_ + " " + tmp + hb_eol()
         NEXT
      ENDIF
   ENDIF
   cExtern += hb_eol()
   cExtern += cLine
   cExtern += " * WARNING: Automatically generated code below. DO NOT EDIT! (except casing)" + hb_eol()
   cExtern += " *          Regenerate using " + _SELF_NAME_ + " '-hbx=' option." + hb_eol()
   cExtern += " */" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#ifndef " + "__HBEXTERN_CH__" + StrToDefine( hb_FNameName( cOutputName ) ) + "__" + hb_eol()
   cExtern += "#define " + "__HBEXTERN_CH__" + StrToDefine( hb_FNameName( cOutputName ) ) + "__" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#if defined( " + _HBMK_HBEXTREQ + " ) .OR. defined( " + cSelfName + "ANNOUNCE" + " )" + hb_eol()
   cExtern += "   ANNOUNCE " + cSelfName + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#if defined( " + _HBMK_HBEXTREQ + " ) .OR. defined( " + cSelfName + "REQUEST" + " )" + hb_eol()
   cExtern += "   #command DYNAMIC <fncs,...> => EXTERNAL <fncs>" + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   IF Empty( aInclude )
      aExtern := aFuncList
   ELSE
      aExtern := {}
      FOR EACH tmp IN aFuncList
         IF AScan( aInclude, {| flt | hb_WildMatchI( flt, tmp, .T. ) } ) > 0
            AAdd( aExtern, tmp )
         ENDIF
      NEXT
   ENDIF
   FOR EACH tmp IN aExtern
      IF ! hb_WildMatch( "HB_GT_*_DEFAULT", tmp, .T. ) .AND. ;
         ! hb_WildMatch( _HB_SELF_PREFIX + "*" + _HB_SELF_SUFFIX, tmp, .T. ) .AND. ;
         AScan( aExclude, {| flt | hb_WildMatchI( flt, tmp, .T. ) } ) == 0
         cExtern += "DYNAMIC " + hb_HGetDef( hDynamic, tmp, tmp ) + hb_eol()
      ENDIF
   NEXT
   cExtern += hb_eol()
   cExtern += "#if defined( " + _HBMK_HBEXTREQ + " ) .OR. defined( " + cSelfName + "REQUEST" + " )" + hb_eol()
   cExtern += "   #uncommand DYNAMIC <fncs,...> => EXTERNAL <fncs>" + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#endif" + hb_eol()

   /* Do not touch the file if the content is unchanged */
   IF hb_MemoRead( cOutputName ) == cExtern
      RETURN .T.
   ENDIF

   IF hbmk[ _HBMK_lInfo ]
      _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Updating extern header: %1$s" ), cOutputName ) )
   ENDIF

   IF hb_MemoWrit( cOutputName, cExtern )
      RETURN .T.
   ENDIF

   _hbmk_OutErr( hbmk, I_( "Error: Updating extern header." ) )

   RETURN .F.

/* TODO: Consider dropping all embedded headers, now that the runner
         "knows" where Harbour headers reside on disk. */

STATIC FUNCTION hbmk_CoreHeaderFiles()
   THREAD STATIC t_hHeaders := NIL

#if defined( HBMK_WITH_EMBEDDED_HEADERS ) .OR. ;
    defined( HBMK_WITH_ALL_EMBEDDED_HEADERS )

   IF t_hHeaders == NIL
      t_hHeaders := { => }

      /* command to store header files in hash array */
      #command ADD HEADER TO <hash> FILE <(cFile)> => ;
               #pragma __streaminclude <(cFile)> | <hash>\[ <(cFile)> \] := %s

#if defined( HBMK_WITH_ALL_EMBEDDED_HEADERS )
      ADD HEADER TO t_hHeaders FILE "achoice.ch"
      ADD HEADER TO t_hHeaders FILE "assert.ch"
      ADD HEADER TO t_hHeaders FILE "blob.ch"
      ADD HEADER TO t_hHeaders FILE "box.ch"
      ADD HEADER TO t_hHeaders FILE "button.ch"
      ADD HEADER TO t_hHeaders FILE "color.ch"
      ADD HEADER TO t_hHeaders FILE "common.ch"
      ADD HEADER TO t_hHeaders FILE "dbedit.ch"
      ADD HEADER TO t_hHeaders FILE "dbinfo.ch"
      ADD HEADER TO t_hHeaders FILE "dbstruct.ch"
      ADD HEADER TO t_hHeaders FILE "directry.ch"
      ADD HEADER TO t_hHeaders FILE "error.ch"
      ADD HEADER TO t_hHeaders FILE "fileio.ch"
      ADD HEADER TO t_hHeaders FILE "getexit.ch"
      ADD HEADER TO t_hHeaders FILE "hb.ch"
      ADD HEADER TO t_hHeaders FILE "hbclass.ch"
      ADD HEADER TO t_hHeaders FILE "hbcom.ch"
      ADD HEADER TO t_hHeaders FILE "hbdebug.ch"
      ADD HEADER TO t_hHeaders FILE "hbdyn.ch"
      ADD HEADER TO t_hHeaders FILE "hbextcdp.ch"
      ADD HEADER TO t_hHeaders FILE "hbextern.ch"
      ADD HEADER TO t_hHeaders FILE "hbextlng.ch"
      ADD HEADER TO t_hHeaders FILE "hbgfx.ch"
      ADD HEADER TO t_hHeaders FILE "hbgfxdef.ch"
      ADD HEADER TO t_hHeaders FILE "hbgtinfo.ch"
      ADD HEADER TO t_hHeaders FILE "hbhash.ch"
      ADD HEADER TO t_hHeaders FILE "hbhrb.ch"
      ADD HEADER TO t_hHeaders FILE "hbinkey.ch"
      ADD HEADER TO t_hHeaders FILE "hblang.ch"
      ADD HEADER TO t_hHeaders FILE "hblpp.ch"
      ADD HEADER TO t_hHeaders FILE "hbmacro.ch"
      ADD HEADER TO t_hHeaders FILE "hbmath.ch"
      ADD HEADER TO t_hHeaders FILE "hbmemory.ch"
      ADD HEADER TO t_hHeaders FILE "hbmemvar.ch"
      ADD HEADER TO t_hHeaders FILE "hboo.ch"
      ADD HEADER TO t_hHeaders FILE "hbpers.ch"
      ADD HEADER TO t_hHeaders FILE "hbsetup.ch"
      ADD HEADER TO t_hHeaders FILE "hbsix.ch"
      ADD HEADER TO t_hHeaders FILE "hbsocket.ch"
      ADD HEADER TO t_hHeaders FILE "hbstdgen.ch"
      ADD HEADER TO t_hHeaders FILE "hbstrict.ch"
      ADD HEADER TO t_hHeaders FILE "hbsxdef.ch"
      ADD HEADER TO t_hHeaders FILE "hbthread.ch"
      ADD HEADER TO t_hHeaders FILE "hbtrace.ch"
      ADD HEADER TO t_hHeaders FILE "hbusrrdd.ch"
      ADD HEADER TO t_hHeaders FILE "hbver.ch"
      ADD HEADER TO t_hHeaders FILE "hbzlib.ch"
      ADD HEADER TO t_hHeaders FILE "inkey.ch"
      ADD HEADER TO t_hHeaders FILE "memoedit.ch"
      ADD HEADER TO t_hHeaders FILE "ord.ch"
      ADD HEADER TO t_hHeaders FILE "rddsys.ch"
      ADD HEADER TO t_hHeaders FILE "reserved.ch"
      ADD HEADER TO t_hHeaders FILE "set.ch"
      ADD HEADER TO t_hHeaders FILE "setcurs.ch"
      ADD HEADER TO t_hHeaders FILE "simpleio.ch"
      ADD HEADER TO t_hHeaders FILE "std.ch"
      ADD HEADER TO t_hHeaders FILE "tbrowse.ch"
      ADD HEADER TO t_hHeaders FILE "harbour.hbx"
      ADD HEADER TO t_hHeaders FILE "hbcpage.hbx"
      ADD HEADER TO t_hHeaders FILE "hblang.hbx"
      ADD HEADER TO t_hHeaders FILE "hbscalar.hbx"
      ADD HEADER TO t_hHeaders FILE "hbusrrdd.hbx"
#else
      ADD HEADER TO t_hHeaders FILE "color.ch"
      ADD HEADER TO t_hHeaders FILE "common.ch"
      ADD HEADER TO t_hHeaders FILE "dbstruct.ch"
      ADD HEADER TO t_hHeaders FILE "directry.ch"
      ADD HEADER TO t_hHeaders FILE "error.ch"
      ADD HEADER TO t_hHeaders FILE "fileio.ch"
      ADD HEADER TO t_hHeaders FILE "hbgtinfo.ch"
      ADD HEADER TO t_hHeaders FILE "hbhash.ch"
      ADD HEADER TO t_hHeaders FILE "hbmemory.ch"
      ADD HEADER TO t_hHeaders FILE "hbver.ch"
      ADD HEADER TO t_hHeaders FILE "inkey.ch"
      ADD HEADER TO t_hHeaders FILE "setcurs.ch"
      ADD HEADER TO t_hHeaders FILE "simpleio.ch"
#endif

      hb_HCaseMatch( t_hHeaders, .T. )
   ENDIF

#endif

   RETURN t_hHeaders

/* Implement hbshell (formerly known as hbrun) */

#ifndef _HBMK_EMBEDDED_

#define _HBSH_ENV_DEBUG  "HBSHELL_DEBUG"

#define _EXT_FILE_NORMAL "hb_extension"
#define _EXT_FILE_MSDOS  "hb_ext.ini"

#if defined( __PLATFORM__DOS )
   #define _EXT_FILE_       _EXT_FILE_MSDOS
   #define _EXT_FILE_ALT    _EXT_FILE_NORMAL
   #define _EXT_FILE_ALT_OS I_( "non-MS-DOS" )
#else
   #define _EXT_FILE_       _EXT_FILE_NORMAL
   #define _EXT_FILE_ALT    _EXT_FILE_MSDOS
   #define _EXT_FILE_ALT_OS I_( "MS-DOS" )
#endif
#define _EXT_ENV_  "HB_EXTENSION"

STATIC FUNCTION hbsh()

   THREAD STATIC t_hbsh := NIL

   IF t_hbsh == NIL
      t_hbsh := Array( _HBSH_MAX_ )
      t_hbsh[ _HBSH_hLibExt ]          := { => }
      t_hbsh[ _HBSH_hCH ]              := { => }
      t_hbsh[ _HBSH_hOPTPRG ]          := { => }
      t_hbsh[ _HBSH_hINCPATH ]         := { => }
      t_hbsh[ _HBSH_hCHCORE ]          := { => }
      t_hbsh[ _HBSH_nCol ]             := 0
      t_hbsh[ _HBSH_aHistory ]         := {}
      t_hbsh[ _HBSH_lPreserveHistory ] := .T.
      t_hbsh[ _HBSH_lWasLoad ]         := .F.
      t_hbsh[ _HBSH_lInteractive ]     := .T.
      t_hbsh[ _HBSH_lClipperComp ]     := .F.
   ENDIF

   RETURN t_hbsh

STATIC PROCEDURE __hbshell( cFile, ... )

   LOCAL hbsh := hbsh()

   LOCAL aExtension := {}
   LOCAL hbmk
   LOCAL cHBC
   LOCAL cExt
   LOCAL tmp
   LOCAL aOPTPRG
   LOCAL hHRB
   LOCAL cVersion
   LOCAL cParamL

   /* get this before doing anything else */
   LOCAL lDebug := ;
      ! Empty( GetEnv( _HBSH_ENV_DEBUG ) ) .OR. ;
      Right( Lower( hb_FNameName( hb_argv( 0 ) ) ), 1 ) == "d"

   #if ! __pragma( b )
      IF ! lDebug
         __vmNoInternals() /* disable access to VM internals */
      ENDIF
   #endif

   /* Set CP and language */

   SetUILang( GetUILang() )

   /* Save originals */

   hbsh[ _HBSH_cDirBase ] := hb_DirBase()
   hbsh[ _HBSH_cProgName ] := hb_ProgName()

   /* Init */

   hbmk := hbsh[ _HBSH_hbmk ] := hbmk_new( .T. )

   /* Help */

   IF HB_ISSTRING( cFile )

      cParamL := Lower( cFile )

      DO CASE
      CASE cParamL == "-help" .OR. cParamL == "--help" .OR. ;
           cParamL == "-h" .OR. cParamL == "-?"

         SetUILang( hbmk[ _HBMK_cUILNG ] := GetUILang() )
         ShowHeader( hbmk )
         ShowHelp( hbmk, .T. )
         RETURN

      CASE cParamL == "-longhelp" .OR. cParamL == "--longhelp" .OR. ;
           cParamL == "-hh" .OR. cParamL == "-??"

         SetUILang( hbmk[ _HBMK_cUILNG ] := GetUILang() )
         ShowHeader( hbmk )
         ShowHelp( hbmk, .T., .T. )
         RETURN

      CASE cParamL == "-longhelpmd" .OR. cParamL == "--longhelpmd"

         hbmk[ _HBMK_lMarkdown ] := .T.

         SetUILang( hbmk[ _HBMK_cUILNG ] := GetUILang() )
         ShowHeader( hbmk )
         ShowHelp( hbmk, .T., .T. )
         RETURN

      ENDCASE
   ENDIF

   /* Detect Harbour dir layout */

   hbmk_init_stage2( hbmk )
   IF ! hbmk_harbour_dirlayout_detect( hbmk, .T. )
      IF __hbshell_CanLoadDyn()
         OutErr( StrTran( I_( e"hbshell: Warning: Failed to detect Harbour.\nRun this tool from its original location inside the Harbour installation." ), e"\n", hb_eol() ) + _OUT_EOL )
      ENDIF
   ENDIF
   hbmk[ _HBMK_cCOMP ] := hb_Version( HB_VERSION_BUILD_COMP )
   hbmk[ _HBMK_cPLAT ] := hb_Version( HB_VERSION_BUILD_PLAT )
   hbmk[ _HBMK_cCPU ] := hb_Version( HB_VERSION_CPU )
   hbmk_harbour_dirlayout_init( hbmk )

   __hbshell_ext_static_init()

   /* Load permanent extensions */

   /* NOTE: The drawback of this feature is that users might omit using
            '#require' keyword in script source, rendering these scripts
             non-portable. */

#if defined( HBMK_WITH_EXTS )
   #translate _HBMK_STRINGIFY( <x> ) => <"x">
   FOR EACH tmp IN hb_ATokens( _HBMK_STRINGIFY( HBMK_WITH_EXTS ), "|" )
      AAdd( aExtension, tmp )
   NEXT
#endif
   __hbshell_LoadExtFromFile( aExtension, __hbshell_ConfigDir() + _EXT_FILE_ )
   __hbshell_LoadExtFromString( aExtension, GetEnv( _EXT_ENV_ ) )

   /* Load default core headers for scripts and dot prompt */

   hbshell_include( "hb.ch" )

   /* Do the thing */

   IF ! Empty( cFile )
      cFile := hb_DirSepToOS( cFile )
   ENDIF

   IF !( cFile == "." ) .AND. ;
      ! Empty( hb_FNameName( cFile ) ) .AND. ;
      ! Empty( cFile := FindInPath( cFile,, { ".hb", ".hrb" } ) )

      cExt := Lower( hb_FNameExt( cFile ) )

      IF !( cExt == ".hb" .OR. ;
            cExt == ".hrb" .OR. ;
            cExt == ".dbf" )
         cExt := __hbshell_FileSig( cFile )
      ENDIF

      SWITCH cExt
      CASE ".hb"

         hbsh[ _HBSH_lInteractive ] := .F.

         /* NOTE: Assumptions:
                  - one dynamic lib belongs to one .hbc file (true for dynamic builds in contrib)
                  - dynamic libs will reference and automatically load all their dependencies
                    (true on all systems so far)
                  - this tool is located in well known place inside the Harbour dir tree.
                  - contribs/add-ons are located in well-known place inside the Harbour dir tree
                  - 3rd party add-ons can be loaded, too if they are installed into the Harbour dir tree
                    and built the same way as contribs.
                  - dynamic libs are installed into bin dir.
                    (this is not true on *nix, there they are in lib dir, and it is a problem
                    in configurations where lib dir contains <comp> component, so to solve it
                    we use the same <comp> values as was used to build itself.)
          */

         __hbshell_LoadExtFromSource( aExtension, cFile )

         /* NOTE: Find .hbc file. Load .hbc file. Process .hbc references.
                  Pick include paths. Load libs. Add include paths to include
                  path list. For this we need to know where Harbour tree
                  is located. */

         /* NOTE: Most filters and macros in .hbc files will not work in this mode */

         aOPTPRG := {}

         IF lDebug
            AAdd( aOPTPRG, "-b" )
         ENDIF

         FOR EACH tmp IN aExtension
            IF Empty( cVersion := HBC_Find( hbmk, cHBC := hb_FNameExtSet( tmp, ".hbc" ) ) )
               OutErr( hb_StrFormat( I_( "hbshell: Warning: Cannot find %1$s" ), cHBC ) + _OUT_EOL )
            ELSE
               AAddNew( aOPTPRG, "-D" + hb_StrFormat( _HBMK_HAS_TPL_HBC, StrToDefine( tmp ) ) + "=" + cVersion )
            ENDIF
         NEXT

         FOR EACH tmp IN hbmk[ _HBMK_aINCPATH ]
            AAdd( aOPTPRG, "-i" + tmp )
         NEXT

         FOR EACH tmp IN hbmk[ _HBMK_aCH ]
            AAdd( aOPTPRG, "-u+" + tmp )
         NEXT

         /* We can use this function as this is a GPL licenced application */
         cFile := hb_compileBuf( hbmk_CoreHeaderFiles(), hb_ProgName(), "-n2", "-w", "-es2", "-q0", ;
                                 hb_ArrayToParams( aOPTPRG ), "-D" + _HBMK_SHELL, cFile )
         IF cFile == NIL
            ErrorLevel( _EXIT_COMPPRG )
            EXIT
         ENDIF

      CASE ".hrb"
         hbsh[ _HBSH_lInteractive ] := .F.
         __hbshell_ext_init( aExtension )
         hHRB := hb_hrbLoad( cFile )
         hbshell_gtSelect( __hbshell_detect_GT( hHRB ) )
         hb_argShift( .T. )
         hb_hrbDo( hHRB, ... )
         EXIT
      CASE ".dbf"
         __hbshell_ext_init( aExtension )
         __hbshell_prompt( hb_AParams(), { "USE " + cFile + " SHARED" } )
         EXIT
      ENDSWITCH
   ELSE
      __hbshell_ext_init( aExtension )
      __hbshell_prompt( hb_AParams() )
   ENDIF

   RETURN

STATIC FUNCTION __hbshell_FileSig( cFile )

   LOCAL hFile
   LOCAL cBuff, cSig, cExt

   cExt := ".hb"
   hFile := FOpen( cFile, FO_READ )
   IF hFile != F_ERROR
      cSig := hb_hrbSignature()
      cBuff := Space( hb_BLen( cSig ) )
      FRead( hFile, @cBuff, hb_BLen( cBuff ) )
      FClose( hFile )
      IF cBuff == cSig
         cExt := ".hrb"
      ENDIF
   ENDIF

   RETURN cExt

STATIC FUNCTION __hbshell_ConfigDir( lForDocOutput )

   LOCAL cDir

   /* It will form the output so that it does not contain
      configuration specific (potentially sensitive)
      information by using generic term. */
   hb_default( @lForDocOutput, .F. )

   IF Empty( GetEnv( _OSCONFDIR_ENV_ ) )
      IF lForDocOutput
         cDir := hb_StrFormat( I_( "<%1$s directory>" ), _SELF_NAME_ )
      ELSE
         cDir := hb_DirBase()
      ENDIF
   ELSE
      IF lForDocOutput
         /* QUESTION: How to document home directory in a multiplatform fashion? */
         cDir := hb_DirSepAdd( EnvNotation( _OSCONFDIR_ENV_ ) ) + _CONFDIR_BASE_
      ELSE
         cDir := hb_DirSepAdd( GetEnv( _OSCONFDIR_ENV_ ) ) + _CONFDIR_BASE_
      ENDIF
   ENDIF

   RETURN hb_DirSepAdd( cDir )

STATIC PROCEDURE __hbshell_LoadExtFromFile( aExtension, cFileName )

   LOCAL cItem

   FOR EACH cItem IN hb_ATokens( StrTran( MemoRead( cFileName ), Chr( 13 ) ), Chr( 10 ) )
      IF "#" $ cItem
         cItem := Left( cItem, At( "#", cItem ) - 1 )
      ENDIF
      IF ! Empty( cItem )
         AAdd( aExtension, cItem )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE __hbshell_LoadExtFromString( aExtension, cString )

   LOCAL cItem

   FOR EACH cItem IN hb_ATokens( cString,, .T. )
      IF ! Empty( cItem )
         AAdd( aExtension, cItem )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE __hbshell_LoadExtFromSource( aExtension, cFileName )

   LOCAL cFile := MemoRead( cFileName )
   LOCAL pRegex
   LOCAL tmp

   tmp := hb_cdpSelect( "EN" )
   pRegex := hb_regexComp( _HBMK_REGEX_REQUIRE, .F. /* lCaseSensitive */, .T. /* lNewLine */ )
   hb_cdpSelect( tmp )

   IF ! Empty( pRegex )
      FOR EACH tmp IN hb_regexAll( pRegex, cFile, ;
                                   NIL /* lCaseSensitive */, ;
                                   NIL /* lNewLine */, NIL, ;
                                   NIL /* nGetMatch */, ;
                                   .T. /* lOnlyMatch */ )
         AAdd( aExtension, SubStr( tmp[ 2 ], 2, Len( tmp[ 2 ] ) - 2 ) )
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE __hbshell_ext_static_init()

   LOCAL hbsh := hbsh()

   LOCAL tmp
   LOCAL nCount
   LOCAL cName

   nCount := __dynsCount()
   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF LEFTEQUAL( cName, "__HBEXTERN__" ) .AND. ;
         ! HBMK_IS_IN( cName, "__HBEXTERN__HBCPAGE__" )
         hbsh[ _HBSH_hLibExt ][ Lower( SubStr( cName, Len( "__HBEXTERN__" ) + 1, Len( cName ) - Len( "__HBEXTERN__" ) - Len( "__" ) ) ) ] := NIL
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE __hbshell_ext_init( aExtension )

   LOCAL cName

   IF ! Empty( aExtension )
      FOR EACH cName IN aExtension
         hbshell_ext_load( cName )
      NEXT
   ENDIF

   RETURN

/* NOTE: Requires -shared mode build */
/* TOFIX: Load components from detected Harbour dir layout */
/* TODO: Load .hbc file (handle -stop command in it) and
         extend header search path accordingly */
FUNCTION hbshell_ext_load( cName )

   LOCAL hbsh := hbsh()

   LOCAL cFileName
   LOCAL hLib
   LOCAL tmp

   LOCAL cHBC
   LOCAL cVersion

   IF HB_ISSTRING( cName ) .AND. ! Empty( cName )
      IF __hbshell_CanLoadDyn()
         IF !( cName $ hbsh[ _HBSH_hLibExt ] )

            hbsh[ _HBSH_hbmk ][ _HBMK_aINCPATH ] := {}
            hbsh[ _HBSH_hbmk ][ _HBMK_aCH ] := {}
            hbsh[ _HBSH_hbmk ][ _HBMK_aLIBUSER ] := {}

            hbsh[ _HBSH_hINCPATH ][ cName ] := {}
            hbsh[ _HBSH_hCH ][ cName ] := {}
            hbsh[ _HBSH_hOPTPRG ][ cName ] := {}

            IF Empty( cVersion := HBC_Find( hbsh[ _HBSH_hbmk ], cHBC := hb_FNameExtSet( cName, ".hbc" ) ) )
               OutErr( hb_StrFormat( I_( "hbshell: Warning: Cannot find %1$s" ), cHBC ) + _OUT_EOL )
            ELSE
               AEval( hbsh[ _HBSH_hbmk ][ _HBMK_aINCPATH ], {| tmp | AAdd( hbsh[ _HBSH_hINCPATH ][ cName ], tmp ) } )
               AEval( hbsh[ _HBSH_hbmk ][ _HBMK_aCH ], {| tmp | AAdd( hbsh[ _HBSH_hCH ][ cName ], tmp ) } )
               AAddNew( hbsh[ _HBSH_hOPTPRG ][ cName ], "-D" + hb_StrFormat( _HBMK_HAS_TPL_HBC, StrToDefine( cName ) ) + "=" + cVersion )

               /* NOTE: Hack. We detect if the .hbc had defined any libs to load.
                        (f.e. there will not be any libs if the .hbc was skipped due
                        to filters)
                  TODO: In the future the .hbc should specify a list of dynamic libs
                        to load, and we should load those, if any. */
               IF ! Empty( hbsh[ _HBSH_hbmk ][ _HBMK_aLIBUSER ] )
                  cFileName := FindInPath( tmp := hb_libName( cName + hb_libPostfix() ), ;
                                           iif( hb_Version( HB_VERSION_UNIX_COMPAT ), GetEnv( "LD_LIBRARY_PATH" ), GetEnv( "PATH" ) ) )
                  IF Empty( cFileName )
                     OutErr( hb_StrFormat( I_( "hbshell: '%1$s' (%2$s) not found." ), cName, tmp ) + _OUT_EOL )
                  ELSE
                     hLib := hb_libLoad( cFileName )
                     IF Empty( hLib )
                        OutErr( hb_StrFormat( I_( "hbshell: Error loading '%1$s' (%2$s)." ), cName, cFileName ) + _OUT_EOL )
                     ELSE
                        hbsh[ _HBSH_hLibExt ][ cName ] := hLib
                        RETURN .T.
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE
         OutErr( hb_StrFormat( I_( "hbshell: Cannot load '%1$s'. Requires -shared %2$s build." ), cName, hb_FNameName( hbshell_ProgName() ) ) + _OUT_EOL )
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION hbshell_ext_unload( cName )

   LOCAL hbsh := hbsh()

   IF HB_ISSTRING( cName ) .AND. cName $ hbsh[ _HBSH_hLibExt ] .AND. hbsh[ _HBSH_hLibExt ][ cName ] != NIL
      hb_HDel( hbsh[ _HBSH_hINCPATH ], cName )
      hb_HDel( hbsh[ _HBSH_hCH ], cName )
      hb_HDel( hbsh[ _HBSH_hOPTPRG ], cName )
      hb_HDel( hbsh[ _HBSH_hLibExt ], cName )
      RETURN .T.
   ENDIF

   RETURN .F.

FUNCTION hbshell_ext_get_list()

   LOCAL hbsh := hbsh()

   LOCAL aName := Array( Len( hbsh[ _HBSH_hLibExt ] ) )
   LOCAL hLib

   FOR EACH hLib IN hbsh[ _HBSH_hLibExt ]
      aName[ hLib:__enumIndex() ] := iif( Empty( hLib ), Upper( hLib:__enumKey() ), hLib:__enumKey() )
   NEXT

   ASort( aName )

   RETURN aName

STATIC FUNCTION __plugin_ext()
#pragma __cstream | RETURN %s
/*
 * Harbour Project source code:
 * extension manager plugin
 *
 * Copyright 2012-2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 */

FUNCTION __hbshell_plugin()
   RETURN { ;
      "id"   => "ext", ;
      "init" => {| hConIO | __init( hConIO ) }, ;
      "exit" => {| context | HB_SYMBOL_UNUSED( context ) }, ;
      "cmd"  => {| context, cCommand | __command( context, cCommand ) } }

STATIC FUNCTION __init( hConIO )
   RETURN { hConIO, { ;
      "load"   => { "<name>" , "Load."   , {| context, cCommand | load( context, cCommand ) } }, ;
      "unload" => { "<name>" , "Unload." , {| context, cCommand | unload( context, cCommand ) } }, ;
      "list"   => { ""       , "List."   , {| context, cCommand | list( context ) } } } }

STATIC PROCEDURE __disp( context, cText )

   Eval( context[ 1 ][ "displine" ], cText )

   RETURN

STATIC FUNCTION __command( context, cCommand )

   LOCAL aCommand
   LOCAL nPos

   IF ! Empty( context )
      aCommand := hb_ATokens( cCommand, " " )
      IF ! Empty( aCommand ) .AND. ( nPos := hb_HPos( context[ 2 ], Lower( aCommand[ 1 ] ) ) ) > 0
         Eval( hb_HValueAt( context[ 2 ], nPos )[ 3 ], context, cCommand )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

/* Commands */

STATIC PROCEDURE load( context, cCommand )

   LOCAL aToken := hb_ATokens( cCommand, " " )
   LOCAL tmp

   FOR tmp := 2 TO Len( aToken )
      hbshell_ext_load( aToken[ tmp ] )
   NEXT

   RETURN

STATIC PROCEDURE unload( context, cCommand )

   LOCAL aToken := hb_ATokens( cCommand, " " )
   LOCAL tmp

   FOR tmp := 2 TO Len( aToken )
      hbshell_ext_unload( aToken[ tmp ] )
   NEXT

   RETURN

STATIC PROCEDURE list( context )

   LOCAL cName

   FOR EACH cName IN hbshell_ext_get_list()
      __disp( context, cName )
   NEXT

   RETURN
#pragma __endtext

#include "directry.ch"

#command ADD PLUGIN TO <hash> FILE <(cFile)> => ;
         #pragma __streaminclude <(cFile)> | <hash>\[ <(cFile)> \] := %s
#command ADD PLUGIN TO <hash> STRING <cExpr> => ;
         <hash>\[ <"cExpr"> \] := <cExpr>

STATIC FUNCTION __hbshell_plugins()

   LOCAL hPlugins := { => }
   LOCAL cDir
   LOCAL cExt
   LOCAL file

   ADD PLUGIN TO hPlugins STRING __plugin_ext()

   cDir := __hbshell_ConfigDir()

   FOR EACH cExt IN { "*.hb", "*.hrb" }
      FOR EACH file IN Directory( cDir + cExt )
         hPlugins[ cDir + file[ F_NAME ] ] := MemoRead( cDir + file[ F_NAME ] )
      NEXT
   NEXT

   RETURN hPlugins

#define _PLUGIN_hHRB                1
#define _PLUGIN_hMethods            2
#define _PLUGIN_ctx                 3
#define _PLUGIN_cID                 4
#define _PLUGIN_MAX_                4

STATIC FUNCTION __hbshell_plugins_load( hPlugins, aParams )

   LOCAL hConIO := { ;
      "displine"  => {| c | __hbshell_ToConsole( c ) }, ;
      "gethidden" => {|| __hbshell_GetHidden() } }

   LOCAL plugin
   LOCAL plugins := {}
   LOCAL hHRBEntry
   LOCAL cFile
   LOCAL oError

   FOR EACH cFile IN hPlugins

      plugin := Array( _PLUGIN_MAX_ )
      plugin[ _PLUGIN_hHRB ] := NIL

      IF !( Lower( hb_FNameExt( cFile:__enumKey() ) ) == ".hrb" )
         /* We can use this function as this is a GPL licenced application */
         cFile := hb_compileFromBuf( cFile, hbmk_CoreHeaderFiles(), hb_ProgName(), "-n2", "-w", "-es2", "-q0" )
      ENDIF

      IF ! Empty( cFile )
         BEGIN SEQUENCE WITH {| oError | Break( oError ) }
            plugin[ _PLUGIN_hHRB ] := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
            IF Empty( hHRBEntry := hb_hrbGetFunSym( plugin[ _PLUGIN_hHRB ], "__hbshell_plugin" ) )
               plugin[ _PLUGIN_hHRB ] := NIL
            ENDIF
         RECOVER USING oError
            plugin[ _PLUGIN_hHRB ] := NIL
            OutErr( StrTran( hb_StrFormat( I_( e"hbshell: Error: Loading shell plugin: %1$s\n'%2$s'" ), cFile:__enumKey(), hbmk_ErrorMessage( oError ) ), e"\n", hb_eol() ) + _OUT_EOL )
         END /* SEQUENCE */
      ENDIF

      IF ! Empty( plugin[ _PLUGIN_hHRB ] )
         plugin[ _PLUGIN_hMethods ] := Do( hHRBEntry )
         IF ! Empty( plugin[ _PLUGIN_hMethods ] )
            plugin[ _PLUGIN_ctx ] := Eval( plugin[ _PLUGIN_hMethods ][ "init" ], hConIO, aParams )
            IF ! Empty( plugin[ _PLUGIN_ctx ] )
               plugin[ _PLUGIN_cID ] := plugin[ _PLUGIN_hMethods ][ "id" ]
               IF ! Empty( plugin[ _PLUGIN_cID ] )
                  AAdd( plugins, plugin )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN plugins

STATIC FUNCTION __hbshell_plugins_command( plugins, cCommand, cDomain )

   LOCAL plugin

   FOR EACH plugin IN plugins
      IF Left( cCommand, Len( plugin[ _PLUGIN_cID ] ) + 1 ) == plugin[ _PLUGIN_cID ] + "."
         IF Eval( plugin[ _PLUGIN_hMethods ][ "cmd" ], plugin[ _PLUGIN_ctx ], SubStr( cCommand, Len( plugin[ _PLUGIN_cID ] ) + 2 ) )
            RETURN .T.
         ENDIF
      ELSEIF cDomain == plugin[ _PLUGIN_cID ]
         IF Eval( plugin[ _PLUGIN_hMethods ][ "cmd" ], plugin[ _PLUGIN_ctx ], cCommand )
            RETURN .T.
         ENDIF
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION __hbshell_plugins_valid_id( plugins, cID )

   LOCAL plugin

   FOR EACH plugin IN plugins
      IF plugin[ _PLUGIN_cID ] == cID
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION __hbshell_plugins_valid_id_list( plugins )

   LOCAL plugin
   LOCAL aList := {}

   FOR EACH plugin IN plugins
      AAdd( aList, plugin[ _PLUGIN_cID ] )
   NEXT

   RETURN aList

STATIC PROCEDURE __hbshell_plugins_unload( plugins )

   LOCAL plugin

   FOR EACH plugin IN plugins
      Eval( plugin[ _PLUGIN_hMethods ][ "exit" ], plugin[ _PLUGIN_ctx ] )
   NEXT

   RETURN

#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define _HBMK_AUTOSHELL_NAME "hbstart.hb"

STATIC PROCEDURE __hbshell_ProcessStart()

   LOCAL cDir
   LOCAL cFileName

   FOR EACH cDir IN AutoConfPathList( .T. )
      IF hb_FileExists( cFileName := ( hb_PathNormalize( hb_DirSepAdd( cDir ) ) + _HBMK_AUTOSHELL_NAME ) )
         __hbshell_Exec( hb_MemoRead( cFileName ) )
         EXIT
      ENDIF
   NEXT

   RETURN

/* TODO: rewrite the full-screen shell to be a simple stdout/stdin shell */
STATIC PROCEDURE __hbshell_prompt( aParams, aCommand )

   LOCAL hbsh := hbsh()

   LOCAL GetList
   LOCAL cLine
   LOCAL nMaxRow, nMaxCol
   LOCAL nHistIndex
   LOCAL bKeyUP, bKeyDown, bKeyIns, bKeyResize
   LOCAL lResize := .F.
   LOCAL plugins
   LOCAL cCommand

   LOCAL cDomain := ""
   LOCAL cPrompt
   LOCAL tmp

   hbshell_gtSelect()

   IF ! hb_gtInfo( HB_GTI_ISSCREENPOS )
      OutErr( hb_StrFormat( I_( "hbshell: Error: Interactive session not possible with %1$s terminal driver" ), hb_gtVersion() ) + _OUT_EOL )
      RETURN
   ENDIF

   hb_gtInfo( HB_GTI_ICONRES, 1 )

   hb_Scroll()
   Set( _SET_SCOREBOARD, .F. )

   __hbshell_HistoryLoad()

   AAdd( hbsh[ _HBSH_aHistory ], PadR( "quit", HB_LINE_LEN ) )
   nHistIndex := Len( hbsh[ _HBSH_aHistory ] ) + 1

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )

   hbsh[ _HBSH_nRow ] := 3

   __hbshell_Exec( "?? hb_Version()" )

   __hbshell_ProcessStart()

   IF HB_ISARRAY( aCommand )
      FOR EACH cCommand IN aCommand
         IF HB_ISSTRING( cCommand )
            AAdd( hbsh[ _HBSH_aHistory ], PadR( cCommand, HB_LINE_LEN ) )
            __hbshell_Info( cCommand )
            __hbshell_Exec( cCommand )
         ENDIF
      NEXT
   ELSE
      cCommand := ""
   ENDIF

   plugins := __hbshell_plugins_load( __hbshell_plugins(), aParams )

   WHILE .T.

      IF cLine == NIL
         cLine := Space( HB_LINE_LEN )
      ENDIF

      __hbshell_Info( cCommand )

      nMaxRow := MaxRow()
      nMaxCol := MaxCol()

      hb_DispOutAt( nMaxRow, 0, cPrompt := cDomain + "." )

      GetList := { Get():New( nMaxRow, Len( cPrompt ), {| v | iif( PCount() == 0, cLine, cLine := v ) }, "cLine", "@KS" + hb_ntos( nMaxCol - Len( cPrompt ) + 1 ) ) }
      ATail( GetList ):display()

      SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

      bKeyIns  := SetKey( K_INS, ;
         {|| SetCursor( iif( ReadInsert( ! ReadInsert() ), ;
                          SC_NORMAL, SC_INSERT ) ) } )
      bKeyUp   := SetKey( K_UP, ;
         {|| iif( nHistIndex > 1, ;
                  cLine := hbsh[ _HBSH_aHistory ][ --nHistIndex ], ) } )
      bKeyDown := SetKey( K_DOWN, ;
         {|| cLine := iif( nHistIndex < Len( hbsh[ _HBSH_aHistory ] ), ;
             hbsh[ _HBSH_aHistory ][ ++nHistIndex ], ;
             ( nHistIndex := Len( hbsh[ _HBSH_aHistory ] ) + 1, Space( HB_LINE_LEN ) ) ) } )
      bKeyResize := SetKey( HB_K_RESIZE, ;
         {|| lResize := .T., hb_keyPut( K_ENTER ) } )

      ReadModal( GetList )

      SetKey( K_DOWN, bKeyDown )
      SetKey( K_UP, bKeyUp )
      SetKey( K_INS, bKeyIns )
      SetKey( HB_K_RESIZE, bKeyResize )

      IF LastKey() == K_ESC .OR. Empty( cLine ) .OR. ;
         ( lResize .AND. LastKey() == K_ENTER )
         IF lResize
            lResize := .F.
         ELSE
            cLine := NIL
         ENDIF
         IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
            hb_Scroll( nMaxRow, 0 )
         ENDIF
         LOOP
      ENDIF

      IF Empty( hbsh[ _HBSH_aHistory ] ) .OR. !( ATail( hbsh[ _HBSH_aHistory ] ) == cLine )
         IF Len( hbsh[ _HBSH_aHistory ] ) < HB_HISTORY_LEN
            AAdd( hbsh[ _HBSH_aHistory ], cLine )
         ELSE
            ADel( hbsh[ _HBSH_aHistory ], 1 )
            hbsh[ _HBSH_aHistory ][ Len( hbsh[ _HBSH_aHistory ] ) ] := cLine
         ENDIF
      ENDIF
      nHistIndex := Len( hbsh[ _HBSH_aHistory ] ) + 1

      cCommand := AllTrim( cLine, " " )
      cLine := NIL
      hb_Scroll( nMaxRow, 0 )
      __hbshell_Info( cCommand )

      IF ! Empty( cCommand )

         IF Left( cCommand, 1 ) == "."
            IF cCommand == "."
               cDomain := ""
            ELSEIF __hbshell_plugins_valid_id( plugins, SubStr( cCommand, 2 ) )
               cDomain := SubStr( cCommand, 2 )
            ELSE
               FOR EACH tmp IN __hbshell_plugins_valid_id_list( plugins )
                  __hbshell_ToConsole( "." + tmp )
               NEXT
            ENDIF
         ELSE
            IF ! __hbshell_plugins_command( plugins, cCommand, cDomain )
               __hbshell_Exec( cCommand )
            ENDIF

            IF hbsh[ _HBSH_nRow ] >= MaxRow()
               hb_Scroll( 3, 0, MaxRow(), MaxCol(), 1 )
               hbsh[ _HBSH_nRow ] := MaxRow() - 1
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   __hbshell_plugins_unload( plugins )

   RETURN

STATIC PROCEDURE __hbshell_ToConsole( cText )

   QQOut( cText + hb_eol() )

   RETURN

STATIC FUNCTION __hbshell_GetHidden()

   LOCAL GetList := {}
   LOCAL cPassword := Space( 128 )
   LOCAL nSavedRow
   LOCAL bKeyPaste

   QQOut( I_( "Enter password:" ) + " " )

   nSavedRow := Row()

   AAdd( GetList, hb_Get():New( Row(), Col(), {| v | iif( PCount() == 0, cPassword, cPassword := v ) }, "cPassword", "@S" + hb_ntos( MaxCol() - Col() + 1 ), hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD ) ) )
   ATail( GetList ):hideInput( .T. )
   ATail( GetList ):postBlock := {|| ! Empty( cPassword ) }
   ATail( GetList ):display()

   SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )
   bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

   ReadModal( GetList )

   /* Positions the cursor on the line previously saved */
   SetPos( nSavedRow, MaxCol() - 1 )
   SetKey( K_ALT_V, bKeyPaste )

   QQOut( hb_eol() )

   RETURN AllTrim( cPassword )

STATIC PROCEDURE __hbshell_Info( cCommand )

   LOCAL hbsh := hbsh()

   IF cCommand != NIL
      hb_DispOutAt( 0, 0, "PP: ", "W/N" )
      hb_DispOutAt( 0, 4, PadR( cCommand, MaxCol() - 3 ), "N/R" )
   ENDIF
   IF Used()
      hb_DispOutAt( 1, 0, PadR( ;
         "RDD: " + PadR( rddName(), 6 ) +;
         " | Area:" + Str( Select(), 3 ) +;
         " | Dbf: " + PadR( Alias(), 10 ) +;
         " | Index: " + PadR( ordName( IndexOrd() ), 8 ) +;
         " | # " + Str( RecNo(), 7 ) + "/" + Str( RecCount(), 7 ), MaxCol() + 1 ), "N/BG" )
   ELSE
      hb_DispOutAt( 1, 0, PadR( ;
         "RDD: " + Space( 6 ) +;
         " | Area:" + Space( 3 ) +;
         " | Dbf: " + Space( 10 ) +;
         " | Index: " + Space( 8 ) +;
         " | # " + Space( 7 ) + "/" + Space( 7 ), MaxCol() + 1 ), "N/BG" )
   ENDIF
   IF hbsh[ _HBSH_lPreserveHistory ]
      hb_DispOutAt( 1, MaxCol(), "o", "R/BG" )
   ENDIF

   hb_DispOutAt( 2, 0, PadR( "Ext:" + " " + ArrayToList( hbshell_ext_get_list(), ", " ), MaxCol() + 1 ), iif( __hbshell_CanLoadDyn(), "W/B", "N/N*" ) )

   RETURN

STATIC FUNCTION __hbshell_CanLoadDyn()
   /* Can load them only in -shared builds on Windows,
      because dynlibs are built against harbour.dll. */
   RETURN hb_Version( HB_VERSION_UNIX_COMPAT ) .OR. hb_Version( HB_VERSION_SHARED )

STATIC PROCEDURE __hbshell_Err( oError, cCommand )

   LOCAL xArg, cMessage

   cMessage := I_( "Could not execute:" ) + ";;" + cCommand + ";;"
   IF oError:ClassName() == "ERROR"
      cMessage += oError:Description
      IF ! Empty( oError:Operation )
         cMessage += " " + oError:Operation
      ENDIF
      IF HB_ISARRAY( oError:Args ) .AND. Len( oError:Args ) > 0
         cMessage += ";" + I_( "Arguments:" )
         FOR EACH xArg IN oError:Args
            cMessage += ";" + hb_CStr( xArg )
         NEXT
      ENDIF
   ELSEIF HB_ISSTRING( oError )
      cMessage += oError
   ENDIF
   cMessage += ";;" + ProcName( 2 ) + "(" + hb_ntos( ProcLine( 2 ) ) + ")"

   Alert( cMessage )

   Break( oError )

STATIC PROCEDURE __hbshell_Exec( cCommand )

   LOCAL hbsh := hbsh()

   LOCAL pHRB, cHRB, cFunc, bBlock, nRowMin
   LOCAL aOPTPRG := {}

   IF ! Empty( hbsh[ _HBSH_hCHCORE ] )
      AAdd( aOPTPRG, "-i" + hbsh[ _HBSH_hbmk ][ _HBMK_cHB_INSTALL_INC ] )
      hb_HEval( hbsh[ _HBSH_hCHCORE ], {| tmp | AAdd( aOPTPRG, "-u+" + tmp ) } )
   ENDIF

   hb_HEval( hbsh[ _HBSH_hINCPATH ], ;
      {| cExt |
         AEval( hbsh[ _HBSH_hINCPATH ][ cExt ], {| tmp | AAddNew( aOPTPRG, "-i" + tmp ) } )
         AEval( hbsh[ _HBSH_hCH ][ cExt ]     , {| tmp | AAddNew( aOPTPRG, "-u+" + tmp ) } )
         AEval( hbsh[ _HBSH_hOPTPRG ][ cExt ] , {| tmp | AAddNew( aOPTPRG, tmp ) } )
         RETURN NIL
      } )

   cFunc := ;
      "STATIC FUNCTION __HBDOT()" + hb_eol() + ;
      "RETURN {||" + hb_eol() + ;
      "   " + cCommand + hb_eol() + ;
      "   RETURN __mvSetBase()" + hb_eol() + ;
      "}" + hb_eol()

   DevPos( hbsh[ _HBSH_nRow ], hbsh[ _HBSH_nCol ] )

   BEGIN SEQUENCE WITH {| oError | __hbshell_Err( oError, cCommand ) }

      /* We can use this function as this is a GPL licenced application */
      cHRB := hb_compileFromBuf( cFunc, hbmk_CoreHeaderFiles(), hb_ProgName(), "-n2", "-q2", hb_ArrayToParams( aOPTPRG ) )
      IF Empty( cHRB )
         Eval( ErrorBlock(), I_( "Syntax error." ) )
      ELSE
         pHRB := hb_hrbLoad( cHRB )
         IF ! Empty( pHrb )
            bBlock := hb_hrbDo( pHRB )
            Eval( bBlock )
         ENDIF
      ENDIF

   END /* SEQUENCE */

   hbsh[ _HBSH_nRow ] := Row()
   hbsh[ _HBSH_nCol ] := Col()
   nRowMin := 3
   IF hbsh[ _HBSH_nRow ] < nRowMin
      hbsh[ _HBSH_nRow ] := nRowMin
   ENDIF

   __mvSetBase()

   RETURN

#define _HISTORY_DISABLE_LINE "no"

#if defined( __PLATFORM__DOS )
   #define _FNAME_HISTORY_ "hbhist.ini"
#else
   #define _FNAME_HISTORY_ ".hb_history"
#endif

EXIT PROCEDURE __hbshell_exit()

   __hbshell_HistorySave()

   RETURN

STATIC PROCEDURE __hbshell_HistoryLoad()

   LOCAL hbsh := hbsh()

   LOCAL cHistory
   LOCAL cLine

   hbsh[ _HBSH_lWasLoad ] := .T.

   IF hbsh[ _HBSH_lPreserveHistory ]
      cHistory := StrTran( MemoRead( __hbshell_ConfigDir() + _FNAME_HISTORY_ ), Chr( 13 ) )
      IF Left( cHistory, Len( _HISTORY_DISABLE_LINE + Chr( 10 ) ) ) == _HISTORY_DISABLE_LINE + Chr( 10 )
         hbsh[ _HBSH_lPreserveHistory ] := .F.
      ELSE
         FOR EACH cLine IN hb_ATokens( StrTran( cHistory, Chr( 13 ) ), Chr( 10 ) )
            IF ! Empty( cLine )
               AAdd( hbsh[ _HBSH_aHistory ], PadR( cLine, HB_LINE_LEN ) )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE __hbshell_HistorySave()

   LOCAL hbsh := hbsh()

   LOCAL cHistory
   LOCAL cLine
   LOCAL cDir

   IF hbsh[ _HBSH_lWasLoad ] .AND. hbsh[ _HBSH_lPreserveHistory ]
      cHistory := ""
      FOR EACH cLine IN hbsh[ _HBSH_aHistory ]
         IF !( Lower( AllTrim( cLine ) ) == "quit" )
            cHistory += AllTrim( cLine ) + hb_eol()
         ENDIF
      NEXT
      IF ! hb_DirExists( cDir := __hbshell_ConfigDir() )
         hb_DirCreate( cDir )
      ENDIF
      hb_MemoWrit( cDir + _FNAME_HISTORY_, cHistory )
   ENDIF

   RETURN

#if defined( __PLATFORM__WINDOWS )

/* NOTE: Using an optional contrib component dynamically.
         It is a little trick to work around the limitation
         that core components cannot depend on contribs.
         This way we depend on it, but only dynamically.
         Probably in the future, this functionality should
         be moved to core. [vszakats] */

DYNAMIC win_regWrite
DYNAMIC win_regDelete

STATIC FUNCTION __hbshell_win_reg_self( lRegister, lAllUser )

   IF ! hb_IsFunction( "__HBEXTERN__HBWIN__" ) .AND. ;
      ! hbshell_ext_load( "hbwin" )
      RETURN .F.
   ENDIF
   IF ! hb_IsFunction( "win_regWrite" ) .OR. ;
      ! hb_IsFunction( "win_regDelete" )
      RETURN .F.
   ENDIF

   RETURN __hbshell_win_reg_app( lRegister, lAllUser, hb_ProgName() )

STATIC FUNCTION __hbshell_win_reg_app( lRegister, lAllUser, cAppPath )

   LOCAL cHive := iif( HB_ISLOGICAL( lAllUser ) .AND. lAllUser, "HKEY_CLASSES_ROOT", "HKEY_CURRENT_USER\Software\Classes" )
   LOCAL lSuccess := .T.
   LOCAL tmp

   LOCAL aEntries := { ;
      cHive + "\"                                , ""                    , ;
      cHive + "\.hb\"                            , "HarbourScript"       , ;
      cHive + "\HarbourScript\"                  , "Harbour Script file" , ;
      cHive + "\HarbourScript\DefaultIcon\"      , cAppPath + ",-1"      , ;
      cHive + "\HarbourScript\Shell\"            , "Run"                 , ;
      cHive + "\HarbourScript\Shell\Run\"        , ""                    , ;
      cHive + "\HarbourScript\Shell\Run\Command\", cAppPath + ' "%1"'    }

   IF lRegister
      FOR tmp := 1 TO Len( aEntries ) STEP 2
         lSuccess := lSuccess .AND. win_regWrite( aEntries[ tmp ], aEntries[ tmp + 1 ] )
      NEXT
   ELSE
      FOR tmp := Len( aEntries ) - 1 TO 3 STEP -2
         lSuccess := win_regDelete( aEntries[ tmp ] )
      NEXT
   ENDIF

   RETURN lSuccess

#endif

/* List of Harbour RTL function typically used in
   a full-screen CUI ("interactive") app */
STATIC FUNCTION __hbshell_detect_CUI_extern_positive()
   RETURN { ;
      "COL"              => NIL, ;
      "DISPBEGIN"        => NIL, ;
      "DISPBOX"          => NIL, ;
      "DISPCOUNT"        => NIL, ;
      "DISPEND"          => NIL, ;
      "DISPOUT"          => NIL, ;
      "DISPOUTAT"        => NIL, ;
      "HB_CLRAREA"       => NIL, ;
      "HB_DISPBOX"       => NIL, ;
      "HB_DISPOUTAT"     => NIL, ;
      "HB_DISPOUTATBOX"  => NIL, ;
      "HB_KEYCLEAR"      => NIL, ;
      "HB_KEYINS"        => NIL, ;
      "HB_KEYLAST"       => NIL, ;
      "HB_KEYNEXT"       => NIL, ;
      "HB_KEYPUT"        => NIL, ;
      "HB_KEYSETLAST"    => NIL, ;
      "HB_KEYSTD"        => NIL, ;
      "HB_MGETBOUNDS"    => NIL, ;
      "HB_MMIDDLEDOWN"   => NIL, ;
      "HB_SCRMAXCOL"     => NIL, ;
      "HB_SCRMAXROW"     => NIL, ;
      "HB_SCROLL"        => NIL, ;
      "HB_SHADOW"        => NIL, ;
      "INKEY"            => NIL, ;
      "LASTKEY"          => NIL, ;
      "MAXCOL"           => NIL, ;
      "MAXROW"           => NIL, ;
      "MCOL"             => NIL, ;
      "MDBLCLK"          => NIL, ;
      "MHIDE"            => NIL, ;
      "MLEFTDOWN"        => NIL, ;
      "MPRESENT"         => NIL, ;
      "MRESTSTATE"       => NIL, ;
      "MRIGHTDOWN"       => NIL, ;
      "MROW"             => NIL, ;
      "MSAVESTATE"       => NIL, ;
      "MSETBOUNDS"       => NIL, ;
      "MSETCURSOR"       => NIL, ;
      "MSETPOS"          => NIL, ;
      "MSHOW"            => NIL, ;
      "NEXTKEY"          => NIL, ;
      "RESTSCREEN"       => NIL, ;
      "ROW"              => NIL, ;
      "SAVESCREEN"       => NIL, ;
      "SCROLL"           => NIL, ;
      "SETCOLOR"         => NIL, ;
      "SETCURSOR"        => NIL, ;
      "SETMODE"          => NIL, ;
      "SETPOS"           => NIL, ;
      "SETPOSBS"         => NIL, ;
      "__ACCEPT"         => NIL, ;
      "__WAIT"           => NIL }

STATIC FUNCTION __hbshell_detect_CUI_extern_negative()
   RETURN { ;
      "HB_GT_CGI_DEFAULT" => NIL }

STATIC FUNCTION __hbshell_detect_GT( hHRB )

   LOCAL aFunction
   LOCAL cFunction
   LOCAL hFilter

   /* Detect based on function usage */

   aFunction := hb_hrbGetFunList( hHRB, HB_HRB_FUNC_EXTERN )

   hFilter := __hbshell_detect_CUI_extern_negative()

   FOR EACH cFunction IN aFunction
      IF cFunction $ hFilter
         RETURN _HBMK_GT_DEF_
      ENDIF
   NEXT

   hFilter := __hbshell_detect_CUI_extern_positive()

   FOR EACH cFunction IN aFunction
      IF cFunction $ hFilter
         RETURN __hbshell_gtDefault()
      ENDIF
   NEXT

   RETURN _HBMK_GT_DEF_

/* ------------------------------------------------------------- */

/* for interactive shell and running .hrb. Though it'd be
   even better if .hrb would natively support list of #require-ed
   modules, which could be queried and loaded. Shell prompt could
   support #require as well. */

/* Check if a header is a valid core one */
STATIC FUNCTION __hbshell_TryHeader( cName )

   LOCAL hbsh := hbsh()

   LOCAL lRetVal := .F.

   BEGIN SEQUENCE WITH {| oError | Break( oError ) }

      IF ! Empty( hb_compileFromBuf( "", hbmk_CoreHeaderFiles(), hb_ProgName(), "-q2", ;
                                     "-i" + hbsh[ _HBSH_hbmk ][ _HBMK_cHB_INSTALL_INC ], ;
                                     "-u+" + cName ) )
         lRetVal := .T.
      ENDIF

   END /* SEQUENCE */

   RETURN lRetVal

/* Public hbshell API usable in dot prompt and startup script */
FUNCTION hbshell_include( cName )

   LOCAL hbsh := hbsh()

   IF HB_ISSTRING( cName )

      cName := Lower( cName )

      IF !( cName $ hbsh[ _HBSH_hCHCORE ] ) .AND. __hbshell_TryHeader( cName )
         hbsh[ _HBSH_hCHCORE ][ cName ] := NIL
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION hbshell_uninclude( cName )

   LOCAL hbsh := hbsh()

   IF HB_ISSTRING( cName )

      cName := Lower( cName )

      IF cName $ hbsh[ _HBSH_hCHCORE ]
         hb_HDel( hbsh[ _HBSH_hCHCORE ], cName )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

PROCEDURE hbshell_include_list()

   LOCAL hbsh := hbsh()

   hb_HEval( hbsh[ _HBSH_hCHCORE ], {| tmp | __hbshell_ToConsole( tmp ) } )

   RETURN

/* Public hbshell API */
FUNCTION hbshell_DirBase()

   LOCAL hbsh := hbsh()

   RETURN hb_UTF8ToStr( hbsh[ _HBSH_cDirBase ] )

FUNCTION hbshell_ProgName()

   LOCAL hbsh := hbsh()

   RETURN hb_UTF8ToStr( hbsh[ _HBSH_cProgName ] )

FUNCTION hbshell_Clipper()

   LOCAL hbsh := hbsh()

   hbsh[ _HBSH_lClipperComp ] := .T.

   hb_cdpSelect( "EN" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .T. )
   hb_gtInfo( HB_GTI_BOXCP, "EN" )
   hb_langSelect( "en" )
   Set( _SET_DATEFORMAT, "mm/dd/yy" )

   RETURN NIL

FUNCTION hbshell_gtSelect( cGT )

   LOCAL hbsh := hbsh()

   hb_default( @cGT, __hbshell_gtDefault() )

   IF !( "GT" + hb_gtVersion() == Upper( cGT ) )
      hb_gtSelect( hb_gtCreate( cGT ) )
      hb_SetTermCP( hb_cdpTerm() )
      IF ! hbsh[ _HBSH_lClipperComp ]
         hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
         hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )
      ENDIF
   ENDIF

   RETURN NIL

STATIC FUNCTION __hbshell_gtDefault()
#if defined( __PLATFORM__WINCE )
   RETURN "GTWVT"
#elif defined( __PLATFORM__WINDOWS )
   RETURN "GTWIN"
#elif defined( __PLATFORM__DOS )
   RETURN "GTDOS"
#elif defined( __PLATFORM__OS2 )
   RETURN "GTOS2"
#elif defined( __PLATFORM__UNIX ) .AND. ! defined( __PLATFORM__VXWORKS ) .AND. ! defined( __PLATFORM__SYMBIAN )
   RETURN "GTTRM"
#else
   RETURN _HBMK_GT_DEF_
#endif

#endif /* ! _HBMK_EMBEDDED_ */

/* ------------------------------------------------------------- */

STATIC PROCEDURE convert_hbmake_to_hbp( hbmk, cSrcName, cDstName )

   LOCAL cSrc := MemoRead( cSrcName )
   LOCAL cDst
   LOCAL aDst := {}
   LOCAL tmp
   LOCAL cLine
   LOCAL cSetting
   LOCAL cValue
   LOCAL aValue

   LOCAL cMAIN := NIL

   _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loading hbmake project file: %1$s" ), cSrcName ) )

   IF Empty( cDstName )
      cDstName := hb_FNameExtSet( cSrcName, ".hbp" )
   ENDIF

   AAdd( aDst, hb_StrFormat( "# Automatically converted by %1$s from hbmake project:", _SELF_NAME_ ) )
   AAdd( aDst, hb_StrFormat( "# %1$s", cSrcName ) )
   AAdd( aDst, "" )

   cSrc := StrTran( cSrc, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   cSrc := StrTran( cSrc, Chr( 9 ), " " )

   FOR EACH cLine IN hb_ATokens( cSrc, Chr( 10 ) )
      tmp := At( " =", cLine )
      IF tmp > 0
         cSetting := AllTrim( Left( cLine, tmp - 1 ) )
         cValue := AllTrim( SubStr( cLine, tmp + Len( " =" ) ) )
         aValue := hb_ATokens( cValue )
         IF ! Empty( cValue )
            SWITCH cSetting
            CASE "COMPRESS"
               IF cValue == "YES"
                  AAdd( aDst, "-compr=yes" )
               ENDIF
               EXIT
            CASE "GUI"
               IF cValue == "YES"
                  AAdd( aDst, "-gui" )
               ENDIF
               EXIT
            CASE "MT"
               IF cValue == "YES"
                  AAdd( aDst, "-mt" )
               ENDIF
               EXIT
            CASE "PROJECT"
               IF Len( aValue ) >= 1
                  AAdd( aDst, "-o" + hb_FNameName( aValue[ 1 ] ) )
               ENDIF
               EXIT
            CASE "USERLIBS"
               FOR EACH tmp IN aValue
                  AAdd( aDst, "-l" + hb_FNameName( tmp ) )
               NEXT
               EXIT
            CASE "PRGFILES"
               FOR EACH tmp IN aValue
                  IF !( tmp == "$(PS)" )
                     IF cMAIN == NIL
                        cMAIN := tmp
                     ENDIF
                     AAdd( aDst, tmp )
                  ENDIF
               NEXT
               EXIT
            CASE "CFILES"
               FOR EACH tmp IN aValue
                  IF !( tmp == "$(CF)" )
                     AAdd( aDst, tmp )
                  ENDIF
               NEXT
               EXIT
            CASE "OBJFILES"
               FOR EACH tmp IN aValue
                  IF !( tmp == "$(OB)" )
                     AAdd( aDst, tmp )
                  ENDIF
               NEXT
               EXIT
            CASE "OBJCFILES"
               FOR EACH tmp IN aValue
                  IF !( tmp == "$(OBC)" )
                     AAdd( aDst, tmp )
                  ENDIF
               NEXT
               EXIT
            CASE "RESFILES"
               FOR EACH tmp IN aValue
                  AAdd( aDst, tmp )
               NEXT
               EXIT
            CASE "TOPMODULE"
               IF !( cValue == cMAIN )
                  tmp := AScan( aDst, {| tmp | tmp == cValue } )
                  IF tmp > 0
                     hb_ADel( aDst, tmp, .T. )
                     hb_AIns( aDst, 1, cValue, .T. )
                  ENDIF
               ENDIF
               EXIT
            CASE "CONTRIBLIBS"
               FOR EACH tmp IN aValue
                  AAdd( aDst, "-l" + hb_FNameName( tmp ) )
               NEXT
               EXIT
            CASE "HARBOURFLAGS"
               FOR EACH tmp IN aValue
                  AAdd( aDst, tmp )
               NEXT
               EXIT
            ENDSWITCH
         ENDIF
      ENDIF
   NEXT

   cDst := ""
   FOR EACH tmp IN aDst
      cDst += tmp + hb_eol()
   NEXT

   _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Saving as .hbp file: %1$s" ), cDstName ) )

   hb_MemoWrit( cDstName, cDst )

   RETURN

STATIC PROCEDURE convert_xbp_to_hbp( hbmk, cSrcName, cDstName )

   LOCAL cSrc := MemoRead( cSrcName )
   LOCAL cDst
   LOCAL aDst := {}
   LOCAL tmp
   LOCAL cLine
   LOCAL cSetting
   LOCAL cValue
   LOCAL aValue

   LOCAL cMAIN := NIL

   LOCAL lGlobalSection := .T.

   _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loading xbp (xbuild) project file: %1$s" ), cSrcName ) )

   IF Empty( cDstName )
      cDstName := hb_FNameExtSet( cSrcName, ".hbp" )
   ENDIF

   AAdd( aDst, hb_StrFormat( "# Automatically converted by %1$s from xbuild project:", _SELF_NAME_ ) )
   AAdd( aDst, hb_StrFormat( "# %1$s", cSrcName ) )
   AAdd( aDst, "" )
   AAdd( aDst, "-inc" )

   IF ".lib" $ cSrcName
      AAdd( aDst, "-hblib" )
   ELSEIF ".dll" $ cSrcName
      AAdd( aDst, "-hbdyn" )
   ENDIF

   cSrc := StrTran( cSrc, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   cSrc := StrTran( cSrc, Chr( 9 ), " " )

   FOR EACH cLine IN hb_ATokens( cSrc, Chr( 10 ) )
      IF Left( cLine, 1 ) == "[" .AND. Right( cLine, 1 ) == "]"
         lGlobalSection := .F.
         AAdd( aDst, SubStr( cLine, 2, Len( cLine ) - 2 ) )
      ELSEIF lGlobalSection
         tmp := At( " =", cLine )
         IF tmp > 0
            cSetting := AllTrim( Left( cLine, tmp - 1 ) )
            cValue := AllTrim( SubStr( cLine, tmp + Len( " =" ) ) )
            aValue := hb_ATokens( cValue )
            IF ! Empty( cValue )
               SWITCH cSetting
               CASE "LDEBUG"
                  IF cValue == ".T."
                     AAdd( aDst, "-debug" )
                  ENDIF
                  EXIT
               CASE "LGUI"
                  IF cValue == ".T."
                     AAdd( aDst, "-gui" )
                  ENDIF
                  EXIT
               CASE "LMT"
                  IF cValue == ".T."
                     AAdd( aDst, "-mt" )
                  ENDIF
                  EXIT
               CASE "LUSEDLL"
                  IF cValue == ".T."
                     AAdd( aDst, "-shared" )
                  ENDIF
                  EXIT
               CASE "MAPFILE"
                  IF ! Empty( cValue )
                     AAdd( aDst, "-map" )
                  ENDIF
                  EXIT
               CASE "TARGETFOLDER"
                  IF ! Empty( cValue )
                     AAdd( aDst, "-o" + hb_DirSepAdd( cValue ) )
                  ENDIF
                  EXIT
               CASE "LIBFOLDERS"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-L" + tmp )
                  NEXT
                  EXIT
               CASE "INCLUDEFOLDERS"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-incpath=" + tmp )
                  NEXT
                  EXIT
               CASE "MYC_FLAGS"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-cflag=" + tmp )
                  NEXT
                  EXIT
               CASE "MYDEFINES"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-D" + tmp )
                  NEXT
                  EXIT
               CASE "MYLINK_FLAGS"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-ldflag=" + tmp )
                  NEXT
                  EXIT
               CASE "MYRC_FLAGS"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-resflag=" + tmp )
                  NEXT
                  EXIT
               CASE "MYPRG_FLAGS"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, tmp )
                  NEXT
                  EXIT
               ENDSWITCH
            ENDIF
         ENDIF
      ENDIF
   NEXT

   cDst := ""
   FOR EACH tmp IN aDst
      cDst += tmp + hb_eol()
   NEXT

   _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Saving as .hbp file: %1$s" ), cDstName ) )

   hb_MemoWrit( cDstName, cDst )

   RETURN

STATIC PROCEDURE convert_xhp_to_hbp( hbmk, cSrcName, cDstName )

   LOCAL cSrc := MemoRead( cSrcName )
   LOCAL cDst
   LOCAL aDst := {}
   LOCAL tmp, tmp1
   LOCAL cLine
   LOCAL cSetting
   LOCAL cValue
   LOCAL aValue
   LOCAL cFile

   LOCAL hLIBPATH := { => }

   LOCAL cMAIN := NIL

   LOCAL lFileSection := .F.

   _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loading xhp (xMate) project file: %1$s" ), cSrcName ) )

   IF Empty( cDstName )
      cDstName := hb_FNameExtSet( cSrcName, ".hbp" )
   ENDIF

   AAdd( aDst, hb_StrFormat( "# Automatically converted by %1$s from xMate project:", _SELF_NAME_ ) )
   AAdd( aDst, hb_StrFormat( "# %1$s", cSrcName ) )
   AAdd( aDst, "" )
   AAdd( aDst, "-inc" )

   cSrc := StrTran( cSrc, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   cSrc := StrTran( cSrc, Chr( 9 ), " " )

   FOR EACH cLine IN hb_ATokens( cSrc, Chr( 10 ) )
      IF cLine == "[Files]"
         lFileSection := .T.
      ELSEIF lFileSection
         tmp := At( "=", cLine )
         IF tmp > 0
            cFile := AllTrim( Left( cLine, tmp - 1 ) )
            SWITCH Lower( hb_FNameExt( cFile ) )
            CASE ".c"
            CASE ".prg"
               IF !( "%HB_INSTALL%\" $ cFile )
                  tmp := StrTran( cFile, "%HOME%\" )
                  IF " " $ tmp
                     tmp := '"' + tmp + '"'
                  ENDIF
                  AAdd( aDst, tmp )
               ENDIF
               EXIT
            CASE ".lib"
            CASE ".a"
               IF !( "%C_LIB_INSTALL%\" $ cFile ) .AND. ;
                  !( "%HB_LIB_INSTALL%\" $ cFile )
                  cFile := StrTran( cFile, "%HOME%\" )
                  IF !( hb_FNameDir( cFile ) $ hLIBPATH ) .AND. ! Empty( hb_FNameDir( cFile ) )
                     hLIBPATH[ hb_FNameDir( cFile ) ] := NIL
                  ENDIF
                  tmp := hb_FNameName( cFile )
                  IF hb_FNameExt( cFile ) == ".a" .AND. Left( tmp, 3 ) == "lib"
                     tmp := SubStr( tmp, 4 )
                  ENDIF
                  tmp := "-l" + tmp
                  IF " " $ tmp
                     tmp := '"' + tmp + '"'
                  ENDIF
                  AAdd( aDst, tmp )
               ENDIF
               EXIT
            CASE ".obj"
            CASE ".o"
               IF !( "%C_LIB_INSTALL%\" $ cFile ) .AND. ;
                  !( "%HB_LIB_INSTALL%\" $ cFile )
                  tmp := StrTran( cFile, "%HOME%\" )
                  IF " " $ tmp
                     tmp := '"' + tmp + '"'
                  ENDIF
                  AAdd( aDst, tmp )
               ENDIF
               EXIT
            ENDSWITCH
         ENDIF
      ELSE
         tmp := At( "=", cLine )
         IF tmp > 0
            cSetting := AllTrim( Left( cLine, tmp - 1 ) )
            cValue := AllTrim( SubStr( cLine, tmp + Len( "=" ) ) )
            aValue := hb_ATokens( cValue )
            IF ! Empty( cValue )
               SWITCH cSetting
               CASE "Create Map/List File"
                  IF cValue == "Yes"
                     AAdd( aDst, "-map" )
                  ENDIF
                  EXIT
               CASE "Final Path"
                  IF ! Empty( cValue )
                     AAdd( aDst, "-o" + hb_DirSepAdd( StrTran( cValue, "%HOME%\" ) ) )
                  ENDIF
                  EXIT
               CASE "Include"
                  FOR EACH tmp IN aValue
                     IF !( "%HB_INSTALL%\" $ tmp )
                        IF Left( tmp, 2 ) == "-I"
                           tmp := SubStr( tmp, 3 )
                        ENDIF
                        tmp := StrTran( StrTran( tmp, '"' ), "%HOME%\" )
                        FOR EACH tmp1 IN hb_ATokens( tmp, ";" )
                           IF ! Empty( tmp1 )
                              AAdd( aDst, "-incpath=" + tmp1 )
                           ENDIF
                        NEXT
                     ENDIF
                  NEXT
                  EXIT
               CASE "Define"
                  FOR EACH tmp IN aValue
                     IF Left( tmp, 2 ) == "-D"
                        tmp := SubStr( tmp, 3 )
                     ENDIF
                     AAdd( aDst, "-D" + tmp )
                  NEXT
                  EXIT
               CASE "Params"
                  FOR EACH tmp IN aValue
                     AAdd( aDst, "-runflag=" + tmp )
                  NEXT
                  EXIT
               ENDSWITCH
            ENDIF
         ENDIF
      ENDIF
   NEXT

   FOR EACH tmp IN hLIBPATH
      AAdd( aDst, "-L" + tmp:__enumKey() )
   NEXT

   cDst := ""
   FOR EACH tmp IN aDst
      cDst += tmp + hb_eol()
   NEXT

   _hbmk_OutStd( hbmk, hb_StrFormat( I_( "Saving as .hbp file: %1$s" ), cDstName ) )

   hb_MemoWrit( cDstName, cDst )

   RETURN

STATIC PROCEDURE find_harbour_function( hbmk, aArgs, cSelf )

   LOCAL cArg

   hbmk_init_stage2( hbmk )
   hbmk_harbour_dirlayout_detect( hbmk, .T. )
   hbmk[ _HBMK_cCOMP ] := hb_Version( HB_VERSION_BUILD_COMP )
   hbmk[ _HBMK_cPLAT ] := hb_Version( HB_VERSION_BUILD_PLAT )
   hbmk[ _HBMK_cCPU ] := hb_Version( HB_VERSION_CPU )
   hbmk_harbour_dirlayout_init( hbmk )

   FOR EACH cArg IN aArgs DESCEND
      IF cArg == cSelf
         hb_ADel( aArgs, cArg:__enumIndex(), .T. )
      ENDIF
   NEXT

   ShowFunctionProviders( hbmk, aArgs, .T. )

   RETURN

STATIC FUNCTION GetUILang()

   LOCAL cLNG

   IF Empty( cLNG := GetEnv( "HB_LANG" ) )
      IF Empty( cLNG := hb_UserLang() )
         cLNG := "en"
      ENDIF
   ENDIF

   RETURN StrTran( cLNG, "_", "-" )

STATIC PROCEDURE SetUILang( cUILNG )

   LOCAL tmp

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   /* Configure language */
   IF cUILNG == "en"
      hb_i18n_Set( NIL )
      hb_langSelect( cUILNG )
   ELSE
      tmp := ;
         hb_DirSepAdd( hb_DirBase() ) + ;
         _SELF_NAME_ + ;
         "." + ;
         StrTran( cUILNG, "-", "_" ) + ;
         ".hbl"
      IF hb_i18n_Check( tmp := hb_MemoRead( tmp ) )
         hb_i18n_Set( hb_i18n_RestoreTable( tmp ) )
         hb_langSelect( cUILNG )
      ELSE
         hb_i18n_Set( NIL )
         hb_langSelect( "en" )
      ENDIF
   ENDIF

   RETURN

INIT PROCEDURE ClipInit()

   hb_cdpSelect( "UTF8EX" )

   RETURN

STATIC FUNCTION ToMarkdown( cText, cStyle )

   STATIC sc_hMarkdown := { ;
      "&"        => "&amp;", ;
      "<"        => "&lt;", ;
      ">"        => "&gt;", ;
      "(c)"      => "&copy;", ;
      e"\n"      => "  " + _OUT_EOL, ;
      "\"        => "\\", ;
      "`"        => "\`", ;
      "*"        => "\*", ;
      "_"        => "\_", ;
      "{"        => "\{", ;
      "}"        => "\}", ;
      "["        => "\[", ;
      "]"        => "\]", ;
      "("        => "\(", ;
      ")"        => "\)", ;
      "#"        => "\#", ;
      "+"        => "\+", ;
      "-"        => "\-", ;
      "."        => "\.", ;
      "!"        => "\!", ;
      "Markdown" => "[Markdown](http://daringfireball.net/projects/markdown/)" }

   cText := hb_StrReplace( cText, sc_hMarkdown )

#if 0
   /* experiments with Markdown formatting */
   cText := hb_StrReplace( cText, { ;
      "&lt;" => "*&lt;", ;
      "&gt;" => "&gt;*" } )
#endif

   IF HB_ISSTRING( cStyle )
      SWITCH cStyle
      CASE "strong"
         cText := "**" + cText + "**"
         EXIT
      CASE "url"
         cText := "<" + cText + ">"
         EXIT
      ENDSWITCH
   ENDIF

   RETURN cText

STATIC PROCEDURE ShowHeader( hbmk )

   LOCAL cText
   LOCAL cTrsText
   LOCAL cTrsTextI

   IF hbmk[ _HBMK_lShellMode ]
      cText := ;
         "Harbour Shell / Script Runner " + HBRawVersion() + e"\n" + ;
         "Copyright (c) 2007-2013, Viktor Szakáts" + e"\n" + ;
         "Copyright (c) 2003-2007, Przemysław Czerpak" + e"\n"
   ELSE
      cText := ;
         "Harbour Make (" + _SELF_NAME_ + ") " + HBRawVersion() + e"\n" + ;
         "Copyright (c) 1999-2013, Viktor Szakáts" + e"\n"
   ENDIF

   IF hbmk[ _HBMK_lMarkdown ]
      hb_SetTermCP( "UTF8EX" ) /* UTF-8 output for Markdown */
      cText := ToMarkdown( cText )
   ELSE
      cText := StrTran( cText, e"\n", _OUT_EOL )
   ENDIF
   OutStd( cText )

   cText := "http://harbour-project.org/"
   IF hbmk[ _HBMK_lMarkdown ]
      cText := ToMarkdown( cText, "url" ) + ToMarkdown( e"\n" )
   ELSE
      cText += _OUT_EOL
   ENDIF
   OutStd( cText )

   IF !( hbmk[ _HBMK_cUILNG ] == "en" ) .AND. ;
      !( hbmk[ _HBMK_cUILNG ] == "en-GB" ) .AND. ;
      !( hbmk[ _HBMK_cUILNG ] == "en-US" )
      cTrsText := hb_i18n_gettext_noop( "Translation (%1$s): (add your name here)" )
      cTrsTextI := I_( cTrsText )
      IF !( cTrsText == cTrsTextI ) .AND. ! Empty( cTrsTextI )
         cText := hb_StrFormat( cTrsTextI, hbmk[ _HBMK_cUILNG ] ) + e"\n"
         IF hbmk[ _HBMK_lMarkdown ]
            cText := ToMarkdown( cText )
         ELSE
            cText := StrTran( cText, e"\n", _OUT_EOL )
         ENDIF
         OutStd( cText )
      ENDIF
   ENDIF

   OutStd( _OUT_EOL )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

STATIC FUNCTION ExitCodeStr( nResult )

   SWITCH nResult
   CASE _EXIT_OK              ; RETURN I_( "no error" )
   CASE _EXIT_UNKNPLAT        ; RETURN I_( "unknown platform" )
   CASE _EXIT_UNKNCOMP        ; RETURN I_( "unknown compiler" )
   CASE _EXIT_FAILHBDETECT    ; RETURN I_( "failed Harbour detection" )
   CASE _EXIT_STUBCREATE      ; RETURN I_( "failed stub creation" )
   CASE _EXIT_PHASE_COMP      ; RETURN I_( "failed in compilation (Harbour, C compiler, Resource compiler)" )
   CASE _EXIT_PHASE_ASSEMBLY  ; RETURN I_( "failed in final assembly (linker or library manager)" )
   CASE _EXIT_UNSUPPORTED     ; RETURN I_( "unsupported" )
   CASE _EXIT_WORKDIRCREATE   ; RETURN I_( "failed to create working directory" )
   CASE _EXIT_HELP            ; RETURN I_( "help" )
   CASE _EXIT_MISSDEPT        ; RETURN I_( "dependency missing or disabled" )
   CASE _EXIT_PLUGINPREALL    ; RETURN I_( "plugin initialization" )
   CASE _EXIT_DEEPPROJNESTING ; RETURN I_( "too deep nesting" )
   CASE _EXIT_STOP            ; RETURN I_( "stop requested" )
   ENDSWITCH

   RETURN hb_StrFormat( I_( "unknown: %1$d" ), nResult )

STATIC PROCEDURE ShowHelp( hbmk, lMore, lLong )

   LOCAL aHdr_Syntax := { ;
      I_( "Syntax:" ), ;
      "", ;
      hb_StrFormat( I_( "  %1$s [options] [<script[s]>] <src[s][.prg|.c|.obj|.o|.rc|.res|.def|.po|.pot|.hbl|@.clp|.d|.ch]>" ), _SELF_NAME_ ) }

   LOCAL aHdr_Syntax_Shell := { ;
      I_( "Syntax:" ), ;
      "", ;
      hb_StrFormat( I_( "  %1$s <file[.hb|.prg|.hrb|.dbf]>|<option> [parameters[s]]" ), hb_FNameName( hb_ProgName() ) ) }

   LOCAL aHdr_Supp := { ;
      NIL, ;
      { "", I_( "Supported <compiler> values for each supported <platform> value:" ) } }

   LOCAL aLst_Supp := { ;
      NIL, ;
      { "linux"   , "gcc, clang, icc, watcom, sunpro, open64" }, ;
      { "darwin"  , "gcc, clang, icc" }, ;
      { "win"     , "mingw, msvc, clang, bcc, bcc64, watcom, icc, pocc, xcc, mingw64, msvc64, msvcia64, iccia64, pocc64" }, ;
      { "wce"     , "mingwarm, mingw, msvcarm, poccarm" }, ;
      { "os2"     , "gcc, gccomf, watcom" }, ;
      { "dos"     , "djgpp, watcom" }, ;
      { "bsd"     , "gcc, clang, pcc" }, ;
      { "hpux"    , "gcc" }, ;
      { "beos"    , "gcc" }, ;
      { "qnx"     , "gcc" }, ;
      { "android" , "gcc, gccarm" }, ;
      { "vxworks" , "gcc, diab" }, ;
      { "symbian" , "gcc" }, ;
      { "cygwin"  , "gcc" }, ;
      { "minix"   , "clang, gcc" }, ;
      { "aix"     , "gcc" }, ;
      { "sunos"   , "gcc, sunpro" } }

   LOCAL aHdr_Opt := { ;
      "", ;
      I_( "Options:" ) }

   LOCAL aLst_Opt_Basic := { ;
      NIL, ;
      { "-o<outname>"        , I_( "output file name" ) }, ;
      { "-l<libname>"        , I_( "link with <libname> library. <libname> should be without path, extension and 'lib' prefix (unless part of the name). Do not add core Harbour libraries, they are automatically added as needed. If <libname> starts with a '-' character, the library will be removed from the list of libraries at link time." ) }, ;
      { "-L<libpath>"        , I_( "additional path to search for libraries" ) }, ;
      { "-i<p>|-incpath=<p>" , I_( "additional path to search for headers" ) }, ;
      { "-static|-shared"    , I_( "link with static/shared libs" ) }, ;
      { "-gt<name>"          , I_( "link with GT<name> GT driver, can be repeated to link with more GTs. First one will be the default at runtime" ) }, ;
      { "-inc[-]"            , I_( "enable/disable incremental build mode (default: disabled)" ) }, ;
      { "-hbexe"             , I_( "create executable (default)" ) }, ;
      { "-hblib"             , I_( "create static library" ) }, ;
      { "-hbdyn"             , I_( "create dynamic library (without linked Harbour VM)" ) }, ;
      { "-hbdynvm"           , I_( "create dynamic library (with linked Harbour VM)" ) } }

   LOCAL aLst_Opt_Help := { ;
      { "-help"              , I_( "more help" ) } }

   LOCAL aLst_Opt_Long := { ;
      NIL, ;
      { "-mt|-st"            , I_( "link with multi/single-thread Harbour VM" ) }, ;
      { "-gui|-std"          , I_( "create GUI/console executable" ) }, ;
      { "-main=<mainfunc>"   , I_( "override the name of starting function/procedure" ) }, ;
      { "-request=<func>"    , I_( "force function/procedure to be linked" ) }, ;
      { "-fullstatic"        , I_( "link with all static libs" ) }, ;
      { "-pic[-]"            , I_( "create position independent object code (always enabled in -hbdyn/-hbdynvm modes)" ) }, ;
      { "-[full|fix]shared"  , I_( "create shared Harbour binaries without/with absolute dir reference to Harbour library (default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise) (fix/full option in *nix only)" ) }, ;
      { "-nulrdd[-]"         , I_( "link with nulrdd" ) }, ;
      { "-debug[-]"          , I_( "add/exclude C compiler debug info. For Harbour level debug, use Harbour option -b as usual" ) }, ;
      { "-optim[-]"          , I_( "toggle C compiler optimizations (default: on)" ) }, ;
      { "-cpp[-]"            , I_( "force C++/C mode" ) }, ;
      { "-cpp=<value>"       , I_( "select C++ mode. Allowed values are: def, yes, no" ) }, ;
/*    { "-c=<value>"         , I_( "select C standard. Allowed values are: iso90, iso99, iso1x, gnu90, gnu99, gnu1x" ) }, */;
/*    { "-cpp=<value>"       , I_( "select C++ mode or standard. Allowed values are: def, yes, no, iso98, iso03, iso0x, gnu98, gnu0x" ) }, */;
      { "-map[-]"            , I_( "create (or not) a map file" ) }, ;
      { "-implib[-]"         , I_( "create (or not) an import library (in -hbdyn/-hbexe mode). The name will have a postfix added." ) }, ;
      { "-implib=<output>"   , I_( "create import library (in -hbdyn/-hbexe mode) name to <output> (default: same as output)" ) }, ;
      { "-ln=<link>"         , I_( "create symbolic link pointing to <output> (<link> is considered relative to <output>)" ) }, ;
      { "-strip[-]"          , I_( "strip (no strip) binaries" ) }, ;
      { "-trace[-]"          , I_( "show commands executed" ) }, ;
      { "-beep[-]"           , I_( "enable (or disable) single beep on successful exit, double beep on failure" ) }, ;
      { "-ignore[-]"         , I_( "ignore errors when running compiler tools (default: off)" ) }, ;
      { "-hbcppmm[-]"        , I_( "override standard C++ memory management functions with Harbour ones" ) }, ;
      { "-winuni[-]"         , I_( "select between UNICODE (WIDE) and ANSI compilation modes (default: ANSI) (Windows only. For WinCE it is always set to UNICODE)" ) }, ;
      { "-nohblib[-]"        , I_( "do not use static core Harbour libraries when linking" ) }, ;
      { "-nodefgt[-]"        , I_( "do not link default GTs (effective in -static mode)" ) }, ;
      { "-nolibgrouping[-]"  , I_( "disable library grouping on gcc based compilers" ) }, ;
      { "-nomiscsyslib[-]"   , I_( "do not add extra list of system libraries to default library list" ) }, ;
      { "-traceonly"         , I_( "show commands to be executed, but do not execute them" ) }, ;
      { "-warn=<lev>"        , I_( e"set C compiler warning level\n<lev> can be: max, yes, low, no, def (default: yes)" ) }, ;
      { "-safe[-]"           , I_( e"enable safety options in C compiler/linker (default: enabled on Windows, disabled on other systems)" ) }, ;
      { "-compr=<lev>"       , I_( e"compress executable/dynamic lib (needs UPX tool)\n<lev> can be: yes, no, min, max" ) }, ;
      { "-run[-]"            , I_( "run/do not run output executable" ) }, ;
      { "-vcshead=<file>"    , I_( "generate .ch header file with local repository information. SVN, CVS, Git, Mercurial, Bazaar, Fossil and Monotone are currently supported. Generated header will define preprocessor constant _HBMK_VCS_TYPE_ with the name of detected VCS and _HBMK_VCS_ID_ with the unique ID of local repository. If no VCS system is detected, a sequential number will be rolled automatically on each build." ) }, ;
      { "-tshead=<file>"     , I_( "generate .ch header file with timestamp information. Generated header will define preprocessor constants _HBMK_BUILD_DATE_, _HBMK_BUILD_TIME_, _HBMK_BUILD_TIMESTAMP_ with the date/time of build" ) }, ;
      { "-icon=<file>"       , I_( "set <file> as application icon. <file> should be a supported format on the target platform (not supported by some platforms/compilers). On Windows, it is implemented by generating and linking a resource file." ) }, ;
      { "-manifest=<file>"   , I_( "embed manifest <file> in executable/dynamic lib (Windows only)" ) }, ;
      { "-sign=<key>"        , I_( "sign executable with <key> (Windows and Darwin only). On Windows signtool.exe is used (part of MS Windows SDK) or posign.exe (part of Pelles C 7), in that order, both autodetected." ) }, ;
      { "-signpw=<pw>"       , I_( "use <pw> as password when signing executable (Windows and Darwin only)" ) }, ;
      { "-instfile=<g:file>" , I_( "add <file> in to the list of files to be copied to path specified by -instpath option. <g> is an optional copy group (case sensitive), it must be at least two characters long. In case you do not specify <file>, the list of files in that group will be emptied." ) }, ;
      { "-instpath=<g:path>" , I_( "copy target to <path>. if <path> is a directory, it should end with path separator, in this case files specified by -instfile option will also be copied. can be specified multiple times. <g> is an optional copy group, it must be at least two characters long. Build target will be automatically copied to default (empty) copy group. There exist following built-in <g> groups: 'depimplib' for import libraries and 'depimplibsrc' for import library source (.dll) files, both belonging to dependencies." ) }, ;
      { "-instforce[-]"      , I_( "copy target to install path even if it is up to date" ) }, ;
      { "-depimplib[-]"      , I_( "enable (or disable) import library generation for import library sources specified in -depimplibs= options (default: yes)" ) }, ;
      { "-stop[=<text>]"     , I_( "stop without doing anything and display <text> if specified" ) }, ;
      { "-echo=<text>"       , I_( "echo text on screen" ) }, ;
      { "-pause"             , I_( "force waiting for a key on exit in case of failure (with alternate GTs only)" ) }, ;
      { "-exitstr"           , I_( "show error result as human readable text on exit" ) }, ;
      { "-info"              , I_( "turn on informational messages" ) }, ;
      { "-quiet[-]"          , I_( "suppress all screen messages" ) }, ;
      NIL, ;
      { "-bldf[-]"           , I_( "inherit all/no (default) flags from Harbour build" ) }, ;
      { "-bldf=[p][c][l]"    , I_( "inherit .prg/.c/linker flags (or none) from Harbour build" ) }, ;
      { "-F<framework>"      , I_( "link with <framework> framework (Darwin only)" ) }, ;
      { "-prgflag=<f>"       , I_( "pass single flag to Harbour compiler" ) }, ;
      { "-cflag=<f>"         , I_( "pass single flag to C compiler" ) }, ;
      { "-resflag=<f>"       , I_( "pass single flag to resource compiler (Windows only)" ) }, ;
      { "-ldflag=<f>"        , I_( "pass single flag to linker (executable)" ) }, ;
      { "-dflag=<f>"         , I_( "pass single flag to linker (dynamic library)" ) }, ;
      { "-aflag=<f>"         , I_( "pass single flag to linker (static library)" ) }, ;
      { "-iflag=<f>"         , I_( "pass single flag to import library creation command" ) }, ;
      { "-signflag=<f>"      , I_( "pass single flag to code sign command" ) }, ;
      { "-runflag=<f>"       , I_( "pass single flag to output executable when -run option is used" ) }, ;
      { "-cflag+=<f>"        , hb_StrFormat( I_( "pass single flag to C compiler overriding C compiler flags added by %1$s itself. Use with caution." ), _SELF_NAME_ ) }, ;
      { "-ldflag+=<f>"       , I_( "pass single raw option to linker (executable) after the library list. Use with caution." ) }, ;
      { "-dflag+=<f>"        , I_( "pass single raw option to linker (dynamic library) after the library list. Use with caution." ) }, ;
      { "-3rd=<f>"           , hb_StrFormat( I_( "options/flags reserved for 3rd party tools, always ignored by %1$s itself" ), _SELF_NAME_ ) }, ;
      { "-env:<e>[<o>[<v>]]" , I_( "alter local environment. <e> is the name of the environment variable to alter. <o> can be '=' to set/override, '-' to delete, '+' to append to the end of existing value, '#' to insert to the beginning of existing value. <v> is the value to set/append/insert." ) }, ;
      { "-jobs=<n>"          , I_( "start n compilation threads (multiprocess platforms only)" ) }, ;
      { "-head=<m>"          , I_( e"control source header parsing (in incremental build mode)\n<m> can be: native (uses compiler to extract dependencies), full (default, uses simple text parser on the whole file), dep, off" ) }, ;
      { "-rebuild"           , I_( "rebuild (in incremental build mode)" ) }, ;
      { "-rebuildall"        , I_( "rebuild with sub-projects (in incremental build mode)" ) }, ;
      { "-clean"             , I_( "clean (in incremental build mode)" ) }, ;
      { "-workdir=<dir>"     , hb_StrFormat( I_( e"working directory\n(default: %1$s/<platform>/<compiler> [*] in incremental mode, OS temp directory otherwise)" ), _WORKDIR_BASE_ ) }, ;
      NIL, ;
      { "-hbcontainer"       , I_( "virtual target, it does not create anything. Useful for creating an .hbp with the sole purpose of referencing sub-projects" ) }, ;
      { "-hbimplib"          , I_( "create import library (Windows only)" ) }, ;
      NIL, ;
      { "-hbl[=<output>]"    , hb_StrFormat( I_( "output .hbl filename. %1$s macro is accepted in filename" ), _LNG_MARKER ) }, ;
      { "-lng=<languages>"   , hb_StrFormat( I_( e"list of languages to be replaced in %1$s macros in .pot/.po filenames and output .hbl/.po filenames. Comma separated list:\n-lng=en,hu-HU,de" ), _LNG_MARKER ) }, ;
      { "-po=<output>"       , I_( "create/update .po file from source. Merge it with previous .po file of the same name" ) }, ;
      { "-minipo[-]"         , I_( "do (not) add Harbour version number and source file reference to .po (default: add them)" ) }, ;
      { "-rebuildpo"         , I_( "recreate .po file, thus removing all obsolete entries in it" ) }, ;
      NIL, ;
      { "-hbx=[<.ch>]"       , I_( "Create Harbour header (in .hbx format) with all external symbols. Empty parameter will disable it." ) }, ;
      { "-autohbc=<.ch:.hbc>", I_( "<.ch> is a header file name. <.hbc> is a .hbc filename to be automatically included in case the header is found in any of the compiled sources. (EXPERIMENTAL)" ) }, ;
      NIL, ;
      { "-deppkgname=<d:n>"      , I_( "<d> is the name of the dependency. <n> name of the package dependency. Can be specified multiple times." ) }, ;
      { "-depkeyhead=<d:h>"      , I_( "<d> is the name of the dependency. <h> is the key header (.h) of the package dependency. Multiple alternative headers can be specified." ) }, ;
      { "-depoptional=<d:f>"     , I_( "<d> is the name of the dependency. <f> can be 'yes' or 'no', specifies whether the dependency is optional. Default: no" ) }, ;
      { "-depcontrol=<d:v>"      , I_( "<d> is the name of the dependency. <v> is a value that controls how detection is done. Accepted values: no, yes, force, nolocal, local. Default: content of environment variable HBMK_WITH_<d>" ) }, ;
      { "-depincroot=<d:r>"      , I_( "<d> is the name of the dependency. Set <r> as root directory for paths specified in -depincpath options." ) }, ;
      { "-depincpath=<d:i>"      , I_( "<d> is the name of the dependency. Add <i> to the header detection path list." ) }, ;
      { "-depincpathlocal=<d:i>" , I_( "<d> is the name of the dependency. Add <i> to the header detection path list, where <i> is pointing to a directory local to the project and containing an embedded (aka. 'locally hosted') dependency." ) }, ;
      { "-depimplibs=<d:dll>"    , I_( "<d> is the name of the dependency. Add <dll> to the import library source list." ) }, ;
      { "-depimplibd=<d:lib>"    , I_( "<d> is the name of the dependency. Set generated import library name to <lib>" ) }, ;
      { "-depfinish=<d>"         , I_( "<d> is the name of the dependency. Closes the dependency definition and does the actual dependency detection, setting all predefined filter macro variables and build options accordingly. Optional, if omitted, detection will take place after processing all options." ) }, ;
      NIL, ;
      { "-plugin=<filename>" , I_( "add plugin. <filename> can be: .hb, .prg, .hrb" ) }, ;
      { "-pi=<filename>"     , I_( "pass input file to plugins" ) }, ;
      { "-pflag=<f>"         , I_( "pass single flag to plugins" ) } }

   LOCAL aHdr_Opt_LongCmd := { ;
      "", ;
      I_( "Options below are available on command-line:" ) }

   LOCAL aLst_Opt_LongCmd := { ;
      NIL, ;
      { "-target=<script>"   , I_( "specify a new build target. <script> can be .prg (or no extension) or .hbp file. Note that .hbp files are automatically considered as separate targets." ) }, ;
      NIL, ;
      { "-hbrun"             , I_( "run target" ) }, ;
      { "-hbraw"             , I_( "stop after running Harbour compiler" ) }, ;
      { "-hbcmp|-clipper"    , hb_StrFormat( I_( e"stop after creating the object files\ncreate link/copy %1$s to hbcmp/clipper for the same effect" ), _SELF_NAME_ ) }, ;
      { "-hbcc"              , hb_StrFormat( I_( e"accept raw C flags\ncreate link/copy %1$s to hbcc for the same effect" ), _SELF_NAME_ ) }, ;
      { "-hblnk"             , I_( "accept raw linker flags" ) }, ;
      { "-autohbm[-]"        , hb_StrFormat( I_( "enable (or disable) processing of %1$s in current directory (default: yes)" ), _HBMK_AUTOHBM_NAME ) }, ;
      { "-hb10"              , I_( "enable Harbour 1.0.x compatibility mode" ) }, ;
      { "-hb20"              , I_( "enable Harbour 2.0.x compatibility mode" ) }, ;
      { "-hb30"              , I_( "enable Harbour 3.0.x compatibility mode" ) }, ;
      { "-xhb"               , I_( "enable xhb mode" ) }, ;
      { "-hbc"               , I_( "enable pure C mode" ) }, ;
      { "-rtlink"            , "" }, ;
      { "-blinker"           , "" }, ;
      { "-exospace"          , hb_StrFormat( I_( e"emulate Clipper compatible linker behavior\ncreate link/copy %1$s to rtlink/blinker/exospace for the same effect" ), _SELF_NAME_ ) }, ;
      NIL, ;
      { "-hbreg[=global]"    , hb_StrFormat( I_( "register Harbour Script (.hb) with %1$s (Windows only)" ), _SELF_NAME_ ) }, ;
      { "-hbunreg[=global]"  , hb_StrFormat( I_( "unregister Harbour Script (.hb) from %1$s (Windows only)" ), _SELF_NAME_ ) }, ;
      NIL, ;
      { "-find <text>"       , I_( "lists all known Harbour functions that contain <text> in their name, along with their package (case insensitive, accepts multiple values, can contain wildcard characters)" ) }, ;
      NIL, ;
      { "-hbmake=<file>"     , I_( "convert hbmake project <file> to .hbp file" ) }, ;
      { "-xbp=<file>"        , I_( "convert .xbp (xbuild) project <file> to .hbp file" ) }, ;
      { "-xhp=<file>"        , I_( "convert .xhp (xMate) project <file> to .hbp file" ) }, ;
      NIL, ;
      { "--hbdirbin"         , I_( "output Harbour binary directory" ) }, ;
      { "--hbdirdyn"         , I_( "output Harbour dynamic library directory" ) }, ;
      { "--hbdirlib"         , I_( "output Harbour static library directory" ) }, ;
      { "--hbdirinc"         , I_( "output Harbour header directory" ) }, ;
      { "--hbinfo[=nested]"  , I_( "output Harbour build information. Output is in JSON format. The included paths always contain forward slashes. Each JSON block is followed by an 0x0A byte." ) }, ;
      NIL, ;
      { "-plat=<platform>"   , I_( "override default target platform (default: automatic)" ) }, ;
      { "-cpu=<cpu>"         , I_( "override default target CPU (default: automatic) (EXPERIMENTAL)" ) }, ;
      { "-comp=<compiler>"   , I_( e"override C compiler autodetection\nSpecial value:\n - bld: use original build settings (default on *nix)" ) }, ;
      { "-build=<name>"      , I_( "specify a build name" ) }, ;
      { "-lang=<lang>"       , I_( "override default language. <lang> is an ISO language code." ) }, ;
      { "-width=<n>"         , I_( "set output width to <n> characters (0=unlimited)." ) }, ;
      { "-shl"               , I_( "show sub-project level in output lines" ) }, ;
      { "-longhelp"          , I_( "long help" ) }, ;
      { "-longhelpmd"        , I_( "long help in Markdown format" ) }, ;
      { "-harbourhelp"       , hb_StrFormat( I_( "Harbour compiler help (all Harbour compiler options are accepted as is by %1$s)" ), _SELF_NAME_ ) }, ;
      { "-credits"           , I_( "Harbour compiler credits" ) }, ;
      { "-build"             , I_( "Harbour compiler build information" ) }, ;
      { "-version"           , I_( "display version header only" ) } }

   LOCAL aLst_Opt_LongCmd_Shell := { ;
      NIL, ;
      { "-help"              , I_( "this help" ) }, ;
      { "-longhelp"          , I_( "long help" ) }, ;
      { "-longhelpmd"        , I_( "long help in Markdown format" ) } }

   /* Internal option descriptions intentionally not marked as translatable */
   LOCAL aHdr_Opt_Internal := { ;
      "", ;
      "Options below are internal/developer ones (compatibility not guaranteed):" }

   /* Internal option descriptions intentionally not marked as translatable */
   LOCAL aLst_Opt_Internal := { ;
      NIL, ;
      { "-debugtime"         , "measure time spent on the build" }, ;
      { "-debuginc"          , "display internals of incremental build" }, ;
      { "-debugstub"         , "display content of all internally generated source files" }, ;
      { "-debugi18n"         , "display internals on translation file generation" }, ;
      { "-debugdepd"         , "display internals of dependency detection" }, ;
      { "-debugpars"         , "display all input parameters in processing order" }, ;
      { "-debugrte"          , "generate a runtime error" } }

   LOCAL aHdr_Opt_Self := { ;
      "", ;
      { "", hb_StrFormat( I_( "You can sym-link/copy/rename %1$s to the following names to alter default mode of operation:" ), _SELF_NAME_ ) } }

   LOCAL aLst_Opt_Self := { ;
      NIL, ;
      { "hbrun*|*hbrun" , I_( "mode script runner / interactive shell" ) }, ;
      { "hbrund|hbrun*d", I_( "mode script runner in debug mode / interactive shell" ) }, ;
      { "harbour"       , I_( "mode -hbraw (emulate -raw- Harbour compiler)" ) }, ;
      { "clipper"       , I_( "mode -hbcmp (emulate Clipper compiler)" ) }, ;
      { "rtlink"        , I_( "mode -rtlink (emulate Clipper linker)" ) }, ;
      { "exospace"      , I_( "mode -rtlink (emulate Clipper linker)" ) }, ;
      { "blinker"       , I_( "mode -rtlink (emulate Clipper linker)" ) }, ;
      { "*10"           , I_( "option -hb10" ) }, ;
      { "*20"           , I_( "option -hb20" ) }, ;
      { "*30"           , I_( "option -hb30" ) }, ;
      { "x*"            , I_( "option -xhb" ) }, ;
      { "hbcmp*|*hbcmp" , I_( "mode -hbcmp (emulate Harbour compiler producing a binary object)" ) }, ;
      { "hbcc*|*hbcc"   , I_( "mode -hbcc (emulate C compiler)" ) }, ;
      { "hblnk*|*hblnk" , I_( "mode -hblnk (emulate C linker)" ) }, ;
      { "hbexe*|*hbexe" , I_( "mode -hbexe" ) }, ;
      { "hblib*|*hblib" , I_( "mode -hblib" ) }, ;
      { "hbdyn*|*hbdyn" , I_( "mode -hbdyn" ) } }

   LOCAL aHdr_Exit := { ;
      "", ;
      I_( e"Exit codes (\"errorlevels\"):" ) }

   LOCAL aLst_Exit := { ;
      NIL, ;
      { hb_ntos( _EXIT_OK )               , ExitCodeStr( _EXIT_OK ) }, ;
      { hb_ntos( _EXIT_UNKNPLAT )         , ExitCodeStr( _EXIT_UNKNPLAT ) }, ;
      { hb_ntos( _EXIT_UNKNCOMP )         , ExitCodeStr( _EXIT_UNKNCOMP ) }, ;
      { hb_ntos( _EXIT_FAILHBDETECT )     , ExitCodeStr( _EXIT_FAILHBDETECT ) }, ;
      { hb_ntos( _EXIT_STUBCREATE )       , ExitCodeStr( _EXIT_STUBCREATE ) }, ;
      { hb_ntos( _EXIT_PHASE_COMP )       , ExitCodeStr( _EXIT_PHASE_COMP ) }, ;
      { hb_ntos( _EXIT_PHASE_ASSEMBLY )   , ExitCodeStr( _EXIT_PHASE_ASSEMBLY ) }, ;
      { hb_ntos( _EXIT_UNSUPPORTED )      , ExitCodeStr( _EXIT_UNSUPPORTED ) }, ;
      { hb_ntos( _EXIT_WORKDIRCREATE )    , ExitCodeStr( _EXIT_WORKDIRCREATE ) }, ;
      { hb_ntos( _EXIT_HELP )             , ExitCodeStr( _EXIT_HELP ) }, ;
      { hb_ntos( _EXIT_MISSDEPT )         , ExitCodeStr( _EXIT_MISSDEPT ) }, ;
      { hb_ntos( _EXIT_PLUGINPREALL )     , ExitCodeStr( _EXIT_PLUGINPREALL ) }, ;
      { hb_ntos( _EXIT_DEEPPROJNESTING )  , ExitCodeStr( _EXIT_DEEPPROJNESTING ) }, ;
      { hb_ntos( _EXIT_STOP )             , ExitCodeStr( _EXIT_STOP ) }, ;
      { I_( "<other>" )                   , I_( "when -run option is used, the exit code will be the one returned by the target executable" ) } }

   LOCAL aHdr_EnvVar := { ;
      "", ;
      I_( "Environment variables:" ) }

   LOCAL aLst_EnvVar := { ;
      NIL, ;
      { _HBMK_ENV_NAME       , I_( "accepts any options as if they were passed in the beginning of the command-line" ) }, ;
      { "HB_PLATFORM"        , I_( "accepts same values as -plat= option" ) }, ;
      { "HB_COMPILER"        , I_( "accepts same values as -comp= option" ) }, ;
      { "HB_CPU"             , I_( "accepts same values as -cpu= option" ) }, ;
      { "HB_BUILD_NAME"      , I_( "accepts same values as -build= option" ) }, ;
      { "HB_LANG"            , I_( "accepts same values as -lang= option" ) }, ;
      { "HB_USER_LIBS"       , I_( "accepts same values (space separated) as -l option" ) }, ;
      { "HB_USER_LIBPATHS"   , I_( "accepts same values (space separated) as -L option" ) }, ;
      { "HB_USER_PRGFLAGS"   , I_( "options to be passed to Harbour compiler (before command-line options)" ) }, ;
      { "HB_USER_CFLAGS"     , I_( "options to be passed to C compiler (before command-line options)" ) }, ;
      { "HB_USER_RESFLAGS"   , I_( "options to be passed to resource compiler (before command-line options) (Windows only)" ) }, ;
      { "HB_USER_LDFLAGS"    , I_( "options to be passed to linker (executable) (before command-line options)" ) }, ;
      { "HB_USER_DFLAGS"     , I_( "options to be passed to linker (dynamic library) (before command-line options)" ) }, ;
      { "HB_USER_AFLAGS"     , I_( "options to be passed to linker (static library) (before command-line options)" ) }, ;
      { "HB_COMPILER_VER"    , I_( "override C compiler version autodetection (gcc and msvc compiler families only). Format: <15><00>[.<00>] = <major><minor>[.<revision>]" ) }, ;
      { "HB_CCPATH"          , I_( "override C compiler executable directory (gcc compiler families only)" ) }, ;
      { "HB_CCPREFIX"        , I_( "override C compiler executable prefix (gcc compiler families only)" ) }, ;
      { "HB_CCSUFFIX"        , I_( "override C compiler executable suffix (gcc compiler families only)" ) }, ;
      { _HBMK_ENV_INSTALL_PFX, I_( "override Harbour base installation directory" ) }, ;
      { "HB_INSTALL_ADDONS"  , I_( "override Harbour base add-ons directory" ) } }

   LOCAL aLst_EnvVar_Shell := { ;
      NIL }

   LOCAL aHdr_File := { ;
      "", ;
      I_( "Files:" ) }

   LOCAL aLst_File := { ;
      NIL, ;
      { "*.hbp"              , I_( "project file. Can contain any number of command-line options, which are expected " + ;
                                   "to create an output. Lines beginning with '#' character are ignored, otherwise " + ;
                                   "newline is optional and options are space separated, just like on the command-line. " + ;
                                   "You must enclose option containing space in double quotes. Each .hbp file " + ;
                                   "reference will be executed as a sub-project." ) }, ;
      { "*.hbm"              , I_( "collection of options. Can be used to collect common ones into a file and " + ;
                                   "include that into project files. Uses same format as .hbp files." ) }, ;
      { "*.hbc"              , I_( "collection of options that accompany components (aka 'libs', aka packages). " + ;
                                   "Use different syntax than command-line and .hbp/.hbm files. Lines beginning " + ;
                                   "with '#' character are ignored, each directive must be placed in separate lines." ) }, ;
      { "*.ch"               , I_( "if passed directly as a source file, it will be used as additional standard header" ) }, ;
      { _HBMK_AUTOHBC_NAME   , hb_StrFormat( I_( "standard .hbc file that gets automatically processed, if present. Possible location(s) (in order of precedence) [*]: %1$s" ), ArrayToList( AutoConfPathList( .F., hbmk[ _HBMK_lMarkdown ] ), ", " ) ) }, ;
      { _HBMK_AUTOHBM_NAME   , I_( "optional .hbm file residing in current working directory, which gets automatically processed before other options" ) }, ;
      { _HBMK_BUILTIN_FILENAME_MARKER_ + "hb_pkg_dynlib.hbm" , hb_StrFormat( I_( "special .hbm file embedded inside %1$s. It manages the details of creating a dynamic library (in the style of Harbour contribs)." ), _SELF_NAME_ ) }, ;
      { _HBMK_BUILTIN_FILENAME_MARKER_ + "hb_pkg_install.hbm", hb_StrFormat( I_( "special .hbm file embedded inside %1$s. It manages the details of installing targets and related package files to standard locations (in the style of Harbour contribs)." ), _SELF_NAME_ ) } }

   LOCAL aLst_File_Shell := { ;
      NIL, ;
      { "*.hb"               , I_( "Harbour script" ) }, ;
      { "*.hrb"              , I_( "Harbour portable binary (aka precompiled Harbour script)" ) } }

   LOCAL aHdr_Macro := { ;
      "", ;
      I_( "Macro variables:" ) }

   LOCAL aLst_Macro := { ;
      NIL, ;
      { "${hb_root}"           , hb_StrFormat( I_( "directory of %1$s" ), _SELF_NAME_ ) }, ;
      { "${hb_dir}"            , I_( "directory of the filename it is used in" ) }, ;
      { "${hb_dirname}"        , I_( "top directory of the filename it is used in" ) }, ;
      { "${hb_name}"           , I_( "name of the filename it is used in (without directory and extension)" ) }, ;
      { "${hb_self}"           , I_( "full filename it is used in" ) }, ;
      { "${hb_curdir}"         , I_( "current working directory" ) }, ;
      { "${hb_tempdir}"        , I_( "OS directory for temporary files" ) }, ;
      { "${hb_targetname}"     , hb_StrFormat( I_( "name of the project (without directory and extension). Returns %1$s if there is not project file." ), _HBMK_TARGENAME_ADHOC ) }, ;
      { "${hb_targettype}"     , I_( "type of the project (hbexe, hblib, hbdyn, hbdynvm, hbimplib, hbppo, hbhrb, hbcontainer)" ) }, ;
      { "${hb_plat}"           , I_( "selected platform" ) }, ;
      { "${hb_comp}"           , I_( "selected C compiler" ) }, ;
      { "${hb_comp_ver}"       , I_( "C compiler version" ) }, ;
      { "${hb_build}"          , I_( "build name" ) }, ;
      { "${hb_cpu}"            , I_( "selected CPU" ) }, ;
      { "${hb_work}"           , I_( "default base workdir name" ) }, ;
      { "${hb_workdynsub}"     , I_( "default workdir subdirectory for dynamic library targets" ) }, ;
      { "${hb_dynprefix}"      , I_( "dynamic library prefix" ) }, ;
      { "${hb_dynsuffix}"      , I_( "dynamic library suffix" ) }, ;
      { "${hb_dynext}"         , I_( "dynamic library extension" ) }, ;
      { "${hb_ver}"            , hb_StrFormat( I_( "Harbour version in hexadecimal triple byte format. F.e.: %1$s" ), MacroGet( hbmk, "hb_ver" ) ) }, ;
      { "${hb_verstr}"         , hb_StrFormat( I_( "Harbour version in human readable format <major>.<minor>.<release><status>. F.e.: %1$s" ), MacroGet( hbmk, "hb_verstr" ) ) }, ;
      { "${hb_major}"          , I_( "Harbour major version number" ) }, ;
      { "${hb_minor}"          , I_( "Harbour minor version number" ) }, ;
      { "${hb_release}"        , I_( "Harbour release version number" ) }, ;
      { "${hb_status}"         , I_( "Harbour version status" ) }, ;
      { "${hb_revision}"       , I_( "Harbour revision" ) }, ;
      { "${hb_host_plat}"      , I_( "Harbour host platform" ) }, ;
      { "${hb_host_plat_unix}" , I_( "Returns '1' if Harbour host platform is *nix compatible" ) }, ;
      { "${hb_bin}"            , I_( "Harbour binary directory" ) }, ;
      { "${hb_lib}"            , I_( "Harbour static library directory" ) }, ;
      { "${hb_lib3rd}"         , I_( "Harbour 3rd party static library directory" ) }, ;
      { "${hb_dyn}"            , I_( "Harbour dynamic library directory" ) }, ;
      { "${hb_inc}"            , I_( "Harbour header directory" ) }, ;
      { "${hb_addons}"         , I_( "Harbour add-ons base directory" ) }, ;
      { "${hb_first}"          , I_( "Name of source file that holds the entry function (without directory and extension)" ) }, ;
      { "${hb_outputdir}"      , I_( "Directory of the output" ) }, ;
      { "${hb_outputname}"     , I_( "Name of the output (without extension)" ) }, ;
      { "${hb_level}"          , I_( "Sub-project recursion level" ) }, ;
      { "${<depname>}"         , I_( "Returns the header directory of dependency <depname>, or '1' if it is not detected" ) }, ;
      { "${<envvar>}"          , I_( "Returns the value of the environment variable <envvar>" ) } }

   LOCAL aHdr_Filter := { ;
      "", ;
      I_( "Filters (you can combine and/or negate them):" ) }

   LOCAL aLst_Filter := { ;
      NIL, ;
      { "{<platform>}"            , I_( "target platform. Where <platform> can be any value accepted by -plat= option." ) }, ;
      { "{<compiler>}"            , I_( "target C compiler. Where <compiler> can be any value accepted by -comp= option." ) }, ;
      { "{<cpu>}"                 , I_( "target CPU. Where <cpu> can be any of: x86, x86_64, ia64, arm, mips, sh" ) }, ;
      { "{<targettype>}"          , I_( "target type. Where <targettype> is any of the values returned by macro variable ${hb_targettype}." ) }, ;
      { "{mt}"                    , I_( "target is multi-threaded (see -mt option)" ) }, ;
      { "{st}"                    , I_( "target is single-threaded (see -st option)" ) }, ;
      { "{gui}"                   , I_( "GUI target (see -gui option)" ) }, ;
      { "{std}"                   , I_( "console target (see -console option)" ) }, ;
      { "{debug}"                 , I_( "C level debugging is enabled (see -debug option)" ) }, ;
      { "{nodebug}"               , I_( "C level debugging is disabled (see -debug- option)" ) }, ;
      { "{shared}"                , I_( "shared build (see -shared and related options)" ) }, ;
      { "{static}"                , I_( "static build (see -static and related options)" ) }, ;
      { "{lngcpp}"                , I_( "forced C++ mode (see -cpp option)" ) }, ;
      { "{lngc}"                  , I_( "forced C mode (see -cpp- option)" ) }, ;
      { "{winuni}"                , I_( "Windows UNICODE (WIDE) mode (see -winuni option)" ) }, ;
      { "{winansi}"               , I_( "Windows ANSI mode (see -winuni- option)" ) }, ;
      { "{unix}"                  , I_( "target platform is *nix compatible (bsd, hpux, sunos, beos, qnx, android, vxworks, symbian, linux, darwin, cygwin, minix, aix)" ) }, ;
      { "{allwin}"                , I_( "target platform is Windows compatible (win, wce)" ) }, ;
      { "{allgcc}"                , I_( "target C compiler belongs to gcc family (gcc, mingw, mingw64, mingwarm, djgpp, gccomf, clang, open64, pcc)" ) }, ;
      { "{allmingw}"              , I_( "target C compiler is mingw* (mingw, mingw64, mingwarm)" ) }, ;
      { "{allmsvc}"               , I_( "target C compiler is msvc* (msvc, msvc64, msvcia64, msvcarm)" ) }, ;
      { "{allbcc}"                , I_( "target C compiler is bcc* (bcc, bcc64)" ) }, ;
      { "{allpocc}"               , I_( "target C compiler is pocc* (pocc, pocc64, poccarm)" ) }, ;
      { "{allicc}"                , I_( "target C compiler is icc* (icc, iccia64)" ) }, ;
      { "{hb10}"                  , I_( "Harbour 1.0.x compatibility mode (see -hb10 option)" ) }, ;
      { "{hb20}"                  , I_( "Harbour 2.0.x compatibility mode (see -hb20 option)" ) }, ;
      { "{hb30}"                  , I_( "Harbour 3.0.x compatibility mode (see -hb30 option)" ) }, ;
      { "{xhb}"                   , I_( "xhb mode (see -xhb option)" ) }, ;
      { "{hb_ispath='<file|dir>'}", I_( "filter will pass if <file> or <dir> name exists on disk." ) }, ;
      { "{MACRO}"                 , I_( "filter will pass if ${MACRO} value is not empty and not equal to '0' or 'no' (case insensitive)" ) }, ;
      { "{MACRO='<value>'}"       , I_( "filter will pass if ${MACRO} value equals to <value> (case insensitive)." ) }, ;
      { "{MACRO>'<value>'}"       , I_( "filter will pass if ${MACRO} value is larger than <value> (case insensitive)." ) }, ;
      { "{MACRO<'<value>'}"       , I_( "filter will pass if ${MACRO} value is smaller than <value> (case insensitive)." ) } }

   LOCAL aHdr_HBC := { ;
      "", ;
      I_( ".hbc directives (they should be written in separate lines):" ) }

   LOCAL aLst_HBC := { ;
      NIL, ;
      { "echo=<msg>"        , I_( "display <msg>" ) }, ;
      { "skip=[<msg>]"      , I_( "skip processing the rest of the .hbc file. Display <msg>, if specified." ) }, ;
      { "stop=[<msg>]"      , I_( "stop the build. Display <msg>, if specified." ) }, ;
      { "sources="          , I_( "add space separated list of files as input files" ) }, ;
      { "headers="          , I_( "add space separated list of .ch format headers as standard header" ) }, ;
      { "libs="             , I_( "add space separated list of libraries (see more at -l option)" ) }, ;
      { "frameworks="       , I_( "add space separated list of frameworks (Darwin only)" ) }, ;
      { "requests="         , I_( "add space separated list of symbols to force link to the target" ) }, ;
      { "syslibs="          , I_( "add space separated list of libraries as system libraries (before regular libraries)" ) }, ;
      { "hbcs="             , I_( "embed space separated list of .hbc files. Names without the extension is accepted. These references are processed in place." ) }, ;
      { "autohbcs="         , I_( "space separated list of values as in -autohbc= option" ) }, ;
      { "libpaths="         , I_( "space separated list of additional library paths" ) }, ;
      { "incpaths="         , I_( "add space separated list of additional header paths (for both Harbour and C)" ) }, ;
      { "instfiles="        , I_( "space separated list of values as in -instfile= option" ) }, ;
      { "instpaths="        , I_( "space separated list of values as in -instpath= option" ) }, ;
      { "prgflags="         , I_( "space separated list of values as in -prgflag= option" ) }, ;
      { "cflags="           , I_( "space separated list of values as in -cflag= option" ) }, ;
      { "resflags="         , I_( "space separated list of values as in -resflag= option" ) }, ;
      { "ldflags="          , I_( "space separated list of values as in -ldflag= option" ) }, ;
      { "ldflags+="         , I_( "space separated list of values as in -ldflag+= option" ) }, ;
      { "dflags="           , I_( "space separated list of values as in -dflag= option" ) }, ;
      { "dflags+="          , I_( "space separated list of values as in -dflag+= option" ) }, ;
      { "pflags="           , I_( "space separated list of values as in -pflag= option" ) }, ;
      { "psources="         , I_( "space separated list of values as in -pi= option" ) }, ;
      { "gui=<bool>"        , I_( "'yes' = -gui, 'no' = -std option" ) }, ;
      { "mt=<bool>"         , I_( "'yes' = -mt, 'no' = -st option" ) }, ;
      { "pic=<bool>"        , I_( "'yes' = -pic, 'no' = -pic- option" ) }, ;
      { "shared=<bool>"     , I_( "'yes' = -shared, 'no' = -static option" ) }, ;
      { "shareddef=<bool>"  , I_( "similar to shared=, but works only if shared/static mode was not set before" ) }, ;
      { "fullstatic=<bool>" , I_( "'yes' = -fullstatic, 'no' = -static option" ) }, ;
      { "debug=<bool>"      , I_( "'yes' = -debug, 'no' = -debug- option" ) }, ;
      { "optim="            , I_( "'yes' = -optim, 'no' = -optim- option" ) }, ;
      { "nulrdd=<bool>"     , I_( "'yes' = -nulrdd, 'no' = -nulrdd- option" ) }, ;
      { "nodefgt=<bool>"    , I_( "'yes' = -nodefgt, 'no' = -nodefgt- option" ) }, ;
      { "map=<bool>"        , I_( "'yes' = -map, 'no' = -map- option" ) }, ;
      { "hbcppmm=<bool>"    , I_( "'yes' = -hbcpmm, 'no' = -hbcpmm- option" ) }, ;
      { "implib=<bool>"     , I_( "'yes' = -implib, 'no' = -implib- option" ) }, ;
      { "winuni=<bool>"     , I_( "'yes' = -winuni, 'no' = -winuni- option" ) }, ;
      { "strip=<bool>"      , I_( "'yes' = -strip, 'no' = -strip- option" ) }, ;
      { "run=<bool>"        , I_( "'yes' = -run, 'no' = -run- option" ) }, ;
      { "inc=<bool>"        , I_( "'yes' = -inc, 'no' = -inc- option" ) }, ;
      { "safe=<bool>"       , I_( "'yes' = -safe, 'no' = -safe- option" ) }, ;
      { "cpp="              , I_( "same as -cpp= option" ) }, ;
      { "warn="             , I_( "same as -warn= option" ) }, ;
      { "compr="            , I_( "same as -compr= option" ) }, ;
      { "head="             , I_( "same as -head= option" ) }, ;
      { "plugins="          , hb_StrFormat( I_( "space separated list of %1$s plugins to load" ), _SELF_NAME_ ) }, ;
      { "gt=<name>"         , I_( "same as -gt<name> option" ) }, ;
      { "gtdef=<name>"      , I_( "set the default GT to be used" ) }, ;
      { "env="              , I_( "same as -env: option" ) }, ;
      { "deppkgname="       , I_( "same as -deppkgname= option" ) }, ;
      { "depkeyhead="       , I_( "same as -depkeyhead= option" ) }, ;
      { "depoptional="      , I_( "same as -depoptional= option" ) }, ;
      { "depcontrol="       , I_( "same as -depcontrol= option" ) }, ;
      { "depincroot="       , I_( "same as -depincroot= option" ) }, ;
      { "depincpath="       , I_( "same as -depincpath= option" ) }, ;
      { "depincpathlocal="  , I_( "same as -depincpathlocal= option" ) }, ;
      { "depimplibs="       , I_( "same as -depimplibs= option" ) }, ;
      { "depimplibd="       , I_( "same as -depimplibd= option" ) }, ;
      { "name="             , I_( "package name" ) }, ;
      { "description="      , I_( "package description" ) }, ;
      { "version=<x.y.z>"   , I_( "package version number, where x,y,z >= 0 <= 255. Defaults to 0.0.1, if not specified." ) }, ;
      { "keywords="         , I_( "space separated list of keywords" ) }, ;
      { "licences="         , I_( "space separated list of licenses" ) }, ;
      { "repository="       , I_( "space separated list of source repository references" ) } }

   LOCAL aHdr_PredSource := { ;
      NIL, ;
      { "", I_( "Predefined constants in sources:" ) } }

   LOCAL aLst_PredSource := { ;
      NIL, ;
      { _HBMK_PLUGIN                                         , hb_StrFormat( I_( "when an .hb script is compiled as %1$s plugin" ), _SELF_NAME_ ) }, ;
      { _HBMK_HBEXTREQ                                       , I_( "when an .hbx source file is present in a project (available in Harbour sources)" ) }, ;
      { hb_StrFormat( _HBMK_HAS_TPL_HBC, I_( "<hbcname>" ) ) , I_( "when <hbcname>.hbc package is linked to the target. The value is the version= value from the .hbc file, converted to a decimal number, which is '1', if not specified. (available in Harbour sources)" ) }, ;
      { hb_StrFormat( _HBMK_HAS_TPL, I_( "<depname>" ) )     , I_( "when <depname> dependency was detected (available in C sources)" ) } }

   LOCAL aLst_PredSource_Shell := { ;
      NIL, ;
      { _HBMK_SHELL                                          , I_( "when a Harbour source file is run as a shell script" ) }, ;
      { "<standard Harbour>"                                 , I_( "__PLATFORM__*, __ARCH*BIT__, __*_ENDIAN__, etc..." ) } }

   LOCAL aHdr_PredBuild := { ;
      NIL, ;
      { "", I_( "Predefined constants in build files (they are available after '-depfinish=<depname>' / 'depfinish=<depname>'):" ) } }

   LOCAL aLst_PredBuild := { ;
      NIL, ;
      { hb_StrFormat( _HBMK_HAS_TPL, I_( "<depname>" ) )       , I_( "when <depname> dependency was detected" ) }, ;
      { hb_StrFormat( _HBMK_DIR_TPL, I_( "<depname>" ) )       , I_( "return the header directory where <depname> was detected, or empty if it was not." ) }, ;
      { hb_StrFormat( _HBMK_HAS_TPL_LOCAL, I_( "<depname>" ) ) , I_( "when <depname> dependency was detected in a location configured by -depincpathlocal= option" ) } }

#ifndef _HBMK_EMBEDDED_
   LOCAL aHdr_APIShell := { ;
      "", ;
      I_( "Shell API available in Harbour scripts:" ) }

   LOCAL aLst_APIShell := { ;
      NIL, ;
      { "hbshell_gtSelect( [<cGT>] ) -> NIL"                , hb_StrFormat( I_( "Switch GT. Default [*]: '%1$s'" ), Lower( __hbshell_gtDefault() ) ) }, ;
      { "hbshell_Clipper() -> NIL"                          , I_( "Enable Clipper compatibility (non-Unicode) mode." ) }, ;
      { "hbshell_include( <cHeader> ) -> <lSuccess>"        , I_( "Load Harbour header." ) }, ;
      { "hbshell_uninclude( <cHeader> ) -> <lSuccess>"      , I_( "Unload Harbour header." ) }, ;
      { "hbshell_include_list() -> NIL"                     , I_( "Display list of loaded Harbour header." ) }, ;
      { "hbshell_ext_load( <cPackageName> ) -> <lSuccess>"  , I_( "Load package. Similar to #request PP directive." ) }, ;
      { "hbshell_ext_unload( <cPackageName> ) -> <lSuccess>", I_( "Unload package." ) }, ;
      { "hbshell_ext_get_list() -> <aPackages>"             , I_( "List of loaded packages." ) }, ;
      { "hbshell_DirBase() -> <cBaseDir>"                   , I_( "DirBase() not mapped to script." ) }, ;
      { "hbshell_ProgName() -> <cPath>"                     , I_( "ProgName() not mapped to script." ) } }
#endif

   LOCAL aHdr_APIPlugin := { ;
      "", ;
      { "", I_( e"Plugin API:\n('hbmk' is the context variable received by the plugin entry function)" ) } }

   LOCAL aLst_APIPlugin := { ;
      NIL, ;
      { "hbmk_Register_Input_File_Extension( hbmk, cExt ) -> NIL"                     , I_( "Register input file extension to be passed to plugin (by default all unknown file extensions are passed to Harbour compiler)." ) }, ;
      { "hbmk_AddInput_PRG( hbmk, cFileName ) -> NIL"                                 , I_( "Add a Harbour input file to the project." ) }, ;
      { "hbmk_AddInput_C( hbmk, cFileName ) -> NIL"                                   , I_( "Add a C input file to the project." ) }, ;
      { "hbmk_AddInput_CPP( hbmk, cFileName ) -> NIL"                                 , I_( "Add a C++ input file to the project." ) }, ;
      { "hbmk_AddInput_RC( hbmk, cFileName ) -> NIL"                                  , I_( "Add a Windows resource input file to the project." ) }, ;
      { "hbmk_AddInput_OBJ( hbmk, cFileName ) -> NIL"                                 , I_( "Add a binary object file to the project." ) }, ;
      { "hbmk_AddInput_INSTFILE( hbmk, cFileName, [<cGroup>] ) -> NIL"                , I_( "Add a file to be installed, with an optional -instpath= group name." ) }, ;
      { "hbmk_OutStd( hbmk, cText ) -> NIL"                                           , I_( "Output text to stdout." ) }, ;
      { "hbmk_OutErr( hbmk, cText ) -> NIL"                                           , I_( "Output text to stderr." ) }, ;
      { "hbmk_OutStdRaw( hbmk, ... ) -> NIL"                                          , I_( "Output text to stdout without any formatting." ) }, ;
      { "hbmk_OutErrRaw( hbmk, ... ) -> NIL"                                          , I_( "Output text to stderr without any formatting." ) }, ;
      { "hbmk_Macro( hbmk, <cMacro> ) -> <cResult>"                                   , hb_StrFormat( I_( "Evaluate %1$s macro expression." ), _SELF_NAME_ ) }, ;
      { "hbmk_FNameEscape( hbmk, cFileName ) -> <cFileName>"                          , I_( "Escape/quote filename for using it as external command parameter." ) }, ;
      { "hbmk_PathSepToTarget( hbmk, cFileName ) -> <cFileName>"                      , I_( "Convert filename to the format required for the target toolchain." ) }, ;
      { "hbmk_PathSepToForward( <cPath> ) -> <cPath>"                                 , I_( "Convert filename to have forward slash directory separators." ) }, ;
      { "hbmk_PathFromWorkdirToCWD( hbmk ) -> <cRelativePath>"                        , I_( "Return relative path of -workdir= value from current working directory." ) }, ;
      { "hbmk_FindInPath( <cFileName>, [<xPath>], [<aExtDef>] ) -> <cFNFound> | NIL"  , I_( "Find file in <xPath> (array or pathsep delimited string are accepted) with list of <aExtDef> alternate extensions (defaults to executable binaries). Returns filename if found and NIL if not." ) }, ;
      { "hbmk_FNameDirExtSet( <cFileName>, [<cDirNew>], [<cExtNew>] ) -> <cFileName>" , I_( "Change directory and/or extension in filename." ) }, ;
      { "hbmk_FuncNameEncode( <cFuncName> ) -> <cFuncNameEncoded>"                    , I_( "Encode function name according to Harbour compiler rules for forming HB_FUNC() function names in C code." ) }, ;
      { "hbmk_StrStripQuote( cString ) -> <cString>"                                  , I_( "Strip double quote enclosure from a string." ) }, ;
      { "hbmk_ArrayToList( <aList>, [<cSeparator>] ) -> <cList>"                      , I_( "Convert array of strings to a string. Default separator is a single space." ) } }

   LOCAL aHdr_PluginVars := { ;
      "", ;
      { "", I_( e"Plugin variables:\n('hbmk' context hash items, case-sensitive, read-only unless marked otherwise)" ) } }

   LOCAL aLst_PluginVars := { ;
      NIL, ;
      { '"apiver"'       , I_( "API version as an integer" ) }, ;
      { '"cSTATE"'       , I_( "callback state. Can be: 'init', 'pre_all', 'pre_prg', 'pre_res', 'pre_c', 'pre_link', 'pre_lib', 'pre_cleanup', 'post_build', 'post_all'" ) }, ;
      { '"params"'       , I_( "array of parameters passed to plugins via -pflag=/pi= options or having an extension registered via hbmk_Register_Input_File_Extension()" ) }, ;
      { '"vars"'         , I_( "hash of plugin custom variables. Writable, local to each plugin" ) }, ;
      { '"cPLAT"'        , I_( "-plat value" ) }, ;
      { '"cCOMP"'        , I_( "-comp value" ) }, ;
      { '"nCOMPVer"'     , I_( "see HB_COMPILER_VER envvar" ) }, ;
      { '"cCPU"'         , I_( "-cpu value" ) }, ;
      { '"cBUILD"'       , I_( "-build= value" ) }, ;
      { '"cOUTPUTNAME"'  , I_( "-o value" ) }, ;
      { '"cTARGETNAME"'  , I_( "see ${hb_targetname} macro" ) }, ;
      { '"cTARGETTYPE"'  , I_( "see ${hb_targettype} macro" ) }, ;
      { '"lREBUILD"'     , I_( "-rebuild option status" ) }, ;
      { '"lCLEAN"'       , I_( "-clean option status" ) }, ;
      { '"lDEBUG"'       , I_( "-debug option status" ) }, ;
      { '"lMAP"'         , I_( "-map option status" ) }, ;
      { '"lSTRIP"'       , I_( "-strip option status" ) }, ;
      { '"lDONTEXEC"'    , I_( "-traceonly option status" ) }, ;
      { '"lIGNOREERROR"' , I_( "-ignore option status" ) }, ;
      { '"lTRACE"'       , I_( "-trace options status" ) }, ;
      { '"lQUIET"'       , I_( "-q option status" ) }, ;
      { '"lINFO"'        , I_( "-info option status" ) }, ;
      { '"lBEEP"'        , I_( "-beep option status" ) }, ;
      { '"lRUN"'         , I_( "-run option status" ) }, ;
      { '"lINC"'         , I_( "-inc option status" ) }, ;
      { '"cCCPATH"'      , I_( "see HB_CCPATH envvar" ) }, ;
      { '"cCCPREFIX"'    , I_( "see HB_CCPREFIX envvar" ) }, ;
      { '"cCCSUFFIX"'    , I_( "see HB_CCSUFFIX envvar" ) }, ;
      { '"cCCEXT"'       , I_( "see HB_CCEXT envvar" ) }, ;
      { '"cWorkDir"'     , I_( "-workdir= value" ) }, ;
      { '"nExitCode"'    , I_( "Current exit code" ) } }

   LOCAL aHdr_Notes := { ;
      "", ;
      I_( "Notes:" ) }

   LOCAL aLst_Notes := { ;
      NIL, ;
      I_( e"<script> can be:\n  <@script> or <script.hbm>: command-line options in file\n" + ;
         e"  <script.hbp>: command-line options in file, it also marks a new target " + ;
         e"if specified on the command-line\n" + ;
         e"  <script.hbc>: package configuration file" ), ;
      I_( "Source filename without extension will load the .hbp file, if such .hbp " + ;
         e"file exists in current directory. If not, .prg extension will be used." ), ;
      I_( "Multiple -l, -L, -i and <script> parameters are accepted." ), ;
      I_( e"Regular Harbour compiler options are also accepted as is.\n" + ;
         e"(see them with -harbourhelp option)" ), ;
      hb_StrFormat( I_( "%1$s option file in %2$s directory is always processed if it exists. " + ;
         e"On *nix platforms ~/.harbour, /etc/harbour, <base>/etc/harbour, <base>/etc are " + ;
         e"checked (in that order) before the %2$s directory." ), ;
         _HBMK_AUTOHBC_NAME, _SELF_NAME_ ), ;
      hb_StrFormat( I_( "%1$s make script in current directory is always processed " + ;
         e"if it exists." ), _HBMK_AUTOHBM_NAME ), ;
      I_( e"Filters are accepted in each .hbc line and most options.\nFilters can " + ;
         e"be combined using '&' (and), '|' (or) operators, negated by '!' operator " + ;
         e"and grouped by parentheses. Ex.: {win}, {gcc}, {linux|darwin}, {win&!pocc}, " + ;
         e"{(win|linux)&!watcom}, {unix&mt&gui}, -cflag={win}-DMYDEF, -stop{dos}, " + ;
         e"-stop{!allwin}" ), ;
      I_( "Most .hbc lines (libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, " + ;
         "instfiles=, instpaths=, echo=) and corresponding command-line parameters " + ;
         "will accept macro variables. libpaths= also accepts %{hb_name} which " + ;
         "translates to the name of the .hbc file under search." ), ;
      I_( e"Options accepting macro variables also support command substitution. " + ;
         e"Enclose command inside ``, and, if the command contains space, also " + ;
         e"enclose in double quotes. Standard output of the command will be used " + ;
         e"as the value. F.e. \"-cflag=`wx-config --cflags`\", or " + ;
         e"ldflags={unix&gcc}\"`wx-config --libs`\"." ), ;
      I_( "Libraries and object files built with/for CA-Cl*pper will not work with " + ;
         "any supported platform/compiler." ), ;
      I_( "Defaults and feature support may vary by platform/compiler." ), ;
      hb_StrFormat( I_( "GNU Make or any C compiler specific make tool and MSYS " + ;
         "(on Windows) are not needed to run %1$s." ), _SELF_NAME_ ), ;
      I_( ". (dot) passed as first parameter will enter the interactive Harbour shell." ) }

   LOCAL aLst_Notes_Shell := { ;
      NIL, ;
      hb_StrFormat( I_( "%1$s file passed as first parameter will be run as Harbour script. " + ;
         "If the filename contains no path components, it will be searched in current working " + ;
         "directory and in PATH. If not extension is given, .hb and .hrb extensions are " + ;
         "searched, in that order. .dbf file will be opened automatically in shared mode and " + ;
         "interactive Harbour shell launched. " + ;
         "Non-standard extensions will be autodetected for source and precompiled script types. " + ;
         "Note, for Harbour scripts, the codepage is set to UTF-8 by default. The default " + ;
         "core header 'hb.ch' is automatically #included. The default date format is the " + ;
         "ISO standard: yyyy-mm-dd. The default GT is '%2$s', unless full-screen CUI calls " + ;
         "are detected, when '%3$s' [*] is automatically selected (except for INIT PROCEDUREs)." ), ;
         iif( hbmk[ _HBMK_lShellMode ], I_( ".hb, .prg, .hrb or .dbf" ), I_( ".hb, .hrb or .dbf" ) ), ;
         Lower( _HBMK_GT_DEF_ ), Lower( __hbshell_gtDefault() ) ), ;
      I_( "You can use key <Alt+V> in interactive Harbour shell to paste text from the clipboard." ), ;
      hb_StrFormat( I_( "Values marked with [*] may be host platform and/or configuration " + ;
         "dependent. This help was generated on '%1$s' host platform." ), ;
         Lower( hb_Version( HB_VERSION_PLATFORM ) ) ) }

   LOCAL aHdr_Desc := { ;
      "", ;
      I_( "Description:" ) }

   LOCAL cDesc := hb_StrFormat( I_( ;
      e"%1$s is an integrated and portable build tool, making it possible to " + ;
      e"create various types of executable binaries (executable, dynamic library, " + ;
      e"static library, Harbour portable binary) out of multiple types of source " + ;
      e"files (C, C++, Objective C, Harbour, gettext translations, Windows " + ;
      e"resources). 'Integrated' means that a single %1$s project file can " + ;
      e"control all or most aspects of the build process. 'Portable' means that " + ;
      e"a single %1$s project file can control the build on all supported OS " + ;
      e"platforms and across all supported C compilers. It also aims to cover " + ;
      e"the majority of build tasks via short and simple project files (options). " + ;
      e"%1$s supports pure -non-Harbour- C/C++/Objective C projects as well. " + ;
      e"In order to achieve above goals, %1$s will autodetect Harbour, C compiler " + ;
      e"and other required tools, then configure and call them appropriately. " + ;
      e"%1$s allows to extend the types of supported source files via plugins.\n" + ;
      e"Besides building executables, %1$s is able to run Harbour scripts (both " + ;
      e"source and precompiled) directly, and it also features an interactive " + ;
      e"shell prompt." ), _SELF_NAME_ )

   LOCAL aLst_Desc := { ;
      NIL, ;
      cDesc }

   LOCAL cDesc_Shell := hb_StrFormat( I_( ;
      e"%1$s is able to run Harbour scripts (both source and precompiled), " + ;
      e"and it also features an interactive shell prompt." ), hb_FNameName( hb_ProgName() ) )

   LOCAL aLst_Desc_Shell := { ;
      NIL, ;
      cDesc_Shell }

   LOCAL aHdr_License := { ;
      "", ;
      I_( "License:" ) }

   LOCAL aLst_License := { ;
      NIL, ;
      LicenseString() }

   LOCAL aHdr_Auth := { ;
      "", ;
      I_( "Author:" ) }

   LOCAL aLst_Auth := { ;
      NIL, ;
      { "Viktor Szakáts (harbour syenar.net)", "" } }

   // ; Examples

   LOCAL aHdr_ExampleBasic := { ;
      "", ;
      { "", hb_StrFormat( I_( "Examples to start with %1$s:" ), _SELF_NAME_ ) } }

   LOCAL aLst_ExampleBasic := { ;
      NIL, ;
      { I_( "To run the interactive shell ('dot' prompt)" ) , hb_StrFormat( I_( "%1$s ." ), _SELF_NAME_ ) }, ;
      { I_( "To run a Harbour script" )                     , hb_StrFormat( I_( "%1$s myscript.hb [<parameters>]" ), _SELF_NAME_ ) } }

   LOCAL aHdr_ExampleHRB := { ;
      "", ;
      { "", I_( "Examples to build and run Harbour portable binary (aka precompiled Harbour script):" ) } }

   LOCAL aLst_ExampleHRB := { ;
      NIL, ;
      { I_( "To build" )                 , hb_StrFormat( I_( "%1$s -gh myscript.hb" ), _SELF_NAME_ ) }, ;
      { I_( "To run result of above" )   , hb_StrFormat( I_( "%1$s myscript.hrb [<parameters>]" ), _SELF_NAME_ ) } }

   LOCAL aHdr_ExampleApp := { ;
      "", ;
      { "", I_( "Examples to build a Harbour application:" ) } }

   LOCAL aLst_ExampleApp := { ;
      NIL, ;
      { I_( "To build one simple .prg" )                                                          , hb_StrFormat( I_( "%1$s hello.prg" ), _SELF_NAME_ ) }, ;
      { I_( "To build multiple .prg sources into one application in incremental mode" )           , hb_StrFormat( I_( "%1$s mymain.prg myfuncs.prg -inc" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application using a project file" )                                      , hb_StrFormat( I_( "%1$s myapp.hbp" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application using incremental mode" )                                    , hb_StrFormat( I_( "%1$s myapp.hbp -inc" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application which uses a contrib package or 3rd party (add-on) package that ships with an .hbc file" ), hb_StrFormat( I_( "%1$s myapp.prg hbct.hbc" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application which uses a raw library" )                                  , hb_StrFormat( I_( "%1$s myapp.prg -lmylib -L<path_to_mylib>" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application which uses a Windows resource" )                             , hb_StrFormat( I_( "%1$s mymain.prg myres.rc" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application which links against Harbour dynamic libraries" )             , hb_StrFormat( I_( "%1$s -shared myapp.prg" ), _SELF_NAME_ ) }, ;
      { I_( "To build an application out of all .prg and .c sources residing in 'source' subdir" ), hb_StrFormat( I_( "%1$s -omyapp src/*.prg src/*.c" ), _SELF_NAME_ ) } }

   LOCAL aHdr_ExampleLib := { ;
      "", ;
      { "", I_( "Examples to build a Harbour static library:" ) } }

   LOCAL aLst_ExampleLib := { ;
      NIL, ;
      { I_( "To build library 'mylib' from sources" )                        , hb_StrFormat( I_( "%1$s -hblib mylibsrc.prg -omylib" ), _SELF_NAME_ ) }, ;
      { I_( "To build library 'mylib' from sources using incremental mode" ) , hb_StrFormat( I_( "%1$s -hblib mylibsrc.prg -omylib -inc" ), _SELF_NAME_ ) } }

   LOCAL aHdr_Config := { ;
      "", ;
      hb_StrFormat( I_( "%1$s build-time configuration:" ), _SELF_NAME_ ) }

   LOCAL aLst_Config := { ;
      NIL }

   // ;

   hb_default( @lMore, .F. )
   hb_default( @lLong, .F. )

#ifndef _HBMK_EMBEDDED_
   AAdd( aLst_EnvVar_Shell, { _EXT_ENV_       , I_( "space separated list of extensions to load in interactive Harbour shell" ) } )
   AAdd( aLst_EnvVar_Shell, { _HBSH_ENV_DEBUG , I_( "enable script debugging if set to any non-empty value" ) } )
   AAdd( aLst_File_Shell, { _HBMK_AUTOSHELL_NAME, hb_StrFormat( I_( "startup Harbour script for interactive Harbour shell. It gets executed automatically on shell startup, if present. Possible locations (in order of precedence) [*]: %1$s" ), ArrayToList( AutoConfPathList( .T., hbmk[ _HBMK_lMarkdown ] ), ", " ) ) } )
   AAdd( aLst_File_Shell, { "shell plugins"     , hb_StrFormat( I_( ".hb and .hrb plugins for interactive Harbour shell. They may reside in [*]: %1$s" ), __hbshell_ConfigDir( hbmk[ _HBMK_lMarkdown ] ) ) } )
   AAdd( aLst_File_Shell, { _FNAME_HISTORY_     , hb_StrFormat( I_( "stores command history for interactive Harbour shell. You can disable history by making the first line '%1$s' (without quotes and with newline). Resides in [*]: %2$s" ), _HISTORY_DISABLE_LINE, __hbshell_ConfigDir( hbmk[ _HBMK_lMarkdown ] ) ) } )
   AAdd( aLst_File_Shell, { _EXT_FILE_          , hb_StrFormat( I_( "list of extensions to load in interactive Harbour shell. One extension per line, part of line beyond a '#' character is ignored. Alternate filename on %2$s: %1$s. Resides in [*]: %3$s" ), _EXT_FILE_ALT, _EXT_FILE_ALT_OS, __hbshell_ConfigDir( hbmk[ _HBMK_lMarkdown ] ) ) } )
#endif

#if defined( HBMK_WITH_ALL_EMBEDDED_HEADERS )
   AAdd( aLst_Config, I_( "Embed all core Harbour headers." ) )
#elif defined( HBMK_WITH_EMBEDDED_HEADERS )
   AAdd( aLst_Config, I_( "Embed frequently used core Harbour headers." ) )
#endif
#ifndef _HBMK_EMBEDDED_
   #if defined( HBMK_WITH_EXTS )
      AAdd( aLst_Config, hb_StrFormat( I_( "Automatically load these packages for scripts: %1$s" ), ArrayToList( hb_ATokens( _HBMK_STRINGIFY( HBMK_WITH_EXTS ), "|" ), ", " ) ) )
   #endif
#endif

   IF hbmk[ _HBMK_lShellMode ]
      AEval( aHdr_Syntax_Shell, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
   ELSE
      AEval( aHdr_Syntax, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
   ENDIF
   IF lMore
      AEval( aHdr_Desc, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aLst_Desc, {| tmp | OutNote( hbmk, tmp, "  " ) } )
      ELSE
         AEval( aLst_Desc_Shell, {| tmp | OutNote( hbmk, tmp, "  " ) } )
      ENDIF
   ENDIF
   IF ! hbmk[ _HBMK_lShellMode ]
      AEval( aHdr_Opt, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
      AEval( aLst_Opt_Basic, {| tmp | OutOpt( hbmk, tmp ) } )
   ENDIF
   IF lMore
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aLst_Opt_Long, {| tmp | OutOpt( hbmk, tmp ) } )
      ENDIF
      AEval( aHdr_Opt_LongCmd, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aLst_Opt_LongCmd, {| tmp | OutOpt( hbmk, tmp ) } )
      ELSE
         AEval( aLst_Opt_LongCmd_Shell, {| tmp | OutOpt( hbmk, tmp ) } )
      ENDIF
      IF ! hbmk[ _HBMK_lShellMode ]
         IF lLong
            AEval( aHdr_Opt_Internal, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
            AEval( aLst_Opt_Internal, {| tmp | OutOpt( hbmk, tmp ) } )
         ENDIF
         AEval( aHdr_Opt_Self, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_Opt_Self, {| tmp | OutOpt( hbmk, tmp ) } )
      ENDIF
      AEval( aHdr_File, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aLst_File, {| tmp | OutOpt( hbmk, tmp ) } )
      ENDIF
      AEval( aLst_File_Shell, {| tmp | OutOpt( hbmk, tmp ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aHdr_Macro, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
         AEval( aLst_Macro, {| tmp | OutOpt( hbmk, tmp ) } )
         AEval( aHdr_Filter, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
         AEval( aLst_Filter, {| tmp | OutOpt( hbmk, tmp, 28 ) } )
      ENDIF
      AEval( aHdr_PredSource, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aLst_PredSource, {| tmp | OutOpt( hbmk, tmp, 28 ) } )
         AEval( aLst_PredSource_Shell, {| tmp | OutOpt( hbmk, tmp, 28 ) } )
      ELSE
         AEval( aLst_PredSource_Shell, {| tmp | OutOpt( hbmk, tmp ) } )
      ENDIF
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aHdr_PredBuild, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_PredBuild, {| tmp | OutOpt( hbmk, tmp, 28 ) } )
      ENDIF
      IF lLong
         AEval( aHdr_EnvVar, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
         IF ! hbmk[ _HBMK_lShellMode ]
            AEval( aLst_EnvVar, {| tmp | OutOpt( hbmk, tmp ) } )
         ENDIF
         IF Len( aLst_EnvVar_Shell ) > 1
            AEval( aLst_EnvVar_Shell, {| tmp | OutOpt( hbmk, tmp ) } )
         ENDIF
         IF ! hbmk[ _HBMK_lShellMode ]
            AEval( aHdr_HBC, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
            AEval( aLst_HBC, {| tmp | OutOpt( hbmk, tmp ) } )
            AEval( aHdr_APIPlugin, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
            AEval( aLst_APIPlugin, {| tmp | OutOpt( hbmk, tmp, -1 ) } )
            AEval( aHdr_PluginVars, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
            AEval( aLst_PluginVars, {| tmp | OutOpt( hbmk, tmp ) } )
         ENDIF
#ifndef _HBMK_EMBEDDED_
         AEval( aHdr_APIShell, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
         AEval( aLst_APIShell, {| tmp | OutOpt( hbmk, tmp, -1 ) } )
#endif
         /* TODO: - %{}
                  - shell plugins
          */
      ENDIF
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aHdr_ExampleBasic, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_ExampleBasic, {| tmp | OutOpt( hbmk, tmp, -1 ) } )
         AEval( aHdr_ExampleHRB, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_ExampleHRB, {| tmp | OutOpt( hbmk, tmp, -1 ) } )
         AEval( aHdr_ExampleApp, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_ExampleApp, {| tmp | OutOpt( hbmk, tmp, -1 ) } )
         AEval( aHdr_ExampleLib, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_ExampleLib, {| tmp | OutOpt( hbmk, tmp, -1 ) } )
         AEval( aHdr_Exit, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
         AEval( aLst_Exit, {| tmp | OutOpt( hbmk, tmp, 10 ) } )
      ENDIF
      AEval( aHdr_Notes, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aLst_Notes, {| tmp | OutNote( hbmk, tmp ) } )
      ENDIF
      AEval( aLst_Notes_Shell, {| tmp | OutNote( hbmk, tmp ) } )
      IF ! hbmk[ _HBMK_lShellMode ]
         AEval( aHdr_Supp, {| tmp | OutOpt( hbmk, tmp, 0 ) } )
         AEval( aLst_Supp, {| tmp | OutOpt( hbmk, tmp, 11 ) } )
      ENDIF
      IF lLong
         IF Len( aLst_Config ) > 1
            AEval( aHdr_Config, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
            AEval( aLst_Config, {| tmp | OutNote( hbmk, tmp ) } )
         ENDIF
         AEval( aHdr_License, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
         AEval( aLst_License, {| tmp | OutNote( hbmk, tmp, "  " ) } )
      ENDIF
      AEval( aHdr_Auth, {| tmp | OutHdr( hbmk, tmp + _OUT_EOL ) } )
      AEval( aLst_Auth, {| tmp | OutOpt( hbmk, tmp, 50 ) } )
   ELSE
      AEval( aLst_Opt_Help, {| tmp | OutOpt( hbmk, tmp ) } )
   ENDIF

   RETURN

STATIC PROCEDURE OutHdr( hbmk, cText )

   OutStd( iif( hbmk[ _HBMK_lMarkdown ], ToMarkdown( cText ), cText ) )

   RETURN

STATIC PROCEDURE OutOpt( hbmk, aOpt, nWidth )

   LOCAL nLine
   LOCAL nLines
   LOCAL cOpt

   hb_default( @nWidth, 23 )

   IF Empty( aOpt )
      IF hbmk[ _HBMK_lMarkdown ]
         OutStd( _OUT_EOL )
         OutStd( _OUT_EOL )
      ELSE
         IF nWidth >= 0
            OutStd( _OUT_EOL )
         ENDIF
      ENDIF
   ELSE
      IF Len( aOpt ) > 1
         IF hbmk[ _HBMK_lMarkdown ]
            IF nWidth == 0
               OutStd( ToMarkdown( aOpt[ 2 ] ) + _OUT_EOL )
            ELSE
               OutStd( ;
                  " - " + ;
                  ToMarkdown( aOpt[ 1 ], iif( Empty( aOpt[ 2 ] ), NIL, "strong" ) ) + ;
                  iif( nWidth < 0, ToMarkdown( e"\n" ), " " ) + ;
                  ToMarkdown( aOpt[ 2 ] ) + _OUT_EOL )
            ENDIF
         ELSE
            IF nWidth >= 0
               aOpt[ 2 ] := StrTran( aOpt[ 2 ], e"\n", hb_eol() )
               nLines := Max( MLCount( aOpt[ 2 ], hbmk[ _HBMK_nMaxCol ] - nWidth ), ;
                              MLCount( aOpt[ 1 ], nWidth ) )
               FOR nLine := 1 TO nLines
                  OutStd( PadR( Space( 2 ) + MemoLine( aOpt[ 1 ], nWidth, nLine ), nWidth ) )
                  OutStd( RTrim( MemoLine( aOpt[ 2 ], hbmk[ _HBMK_nMaxCol ] - nWidth, nLine ) ) + _OUT_EOL )
               NEXT
            ELSE
               OutStd( _OUT_EOL )
               FOR EACH nWidth, cOpt IN { 2, 8 }, aOpt
                  cOpt := StrTran( cOpt, e"\n", hb_eol() )
                  nLines := MLCount( cOpt, hbmk[ _HBMK_nMaxCol ] - nWidth )
                  FOR nLine := 1 TO nLines
                     OutStd( Space( nWidth ) + RTrim( MemoLine( cOpt, hbmk[ _HBMK_nMaxCol ] - nWidth, nLine ) ) + _OUT_EOL )
                  NEXT
               NEXT
            ENDIF
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lMarkdown ]
            OutStd( " - " + ToMarkdown( aOpt[ 1 ], "strong" ) + _OUT_EOL )
         ELSE
            OutStd( Space( 2 ) + aOpt[ 1 ] + _OUT_EOL )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE OutNote( hbmk, cText, cPrefix )

   LOCAL nLine
   LOCAL nLines

   IF Empty( cText )
      IF hbmk[ _HBMK_lMarkdown ]
         OutStd( _OUT_EOL )
      ENDIF
      OutStd( _OUT_EOL )
   ELSE
      hb_default( @cPrefix, "  - " )
      IF hbmk[ _HBMK_lMarkdown ]
         OutStd( cPrefix + ToMarkdown( cText ) + _OUT_EOL )
      ELSE
         cText := StrTran( cText, e"\n", hb_eol() )
         nLines := MLCount( cText, hbmk[ _HBMK_nMaxCol ] - Len( cPrefix ) )
         FOR nLine := 1 TO nLines
            IF nLine == 1
               OutStd( cPrefix )
            ELSE
               OutStd( Space( Len( cPrefix ) ) )
            ENDIF
            OutStd( RTrim( MemoLine( cText, hbmk[ _HBMK_nMaxCol ] - Len( cPrefix ), nLine ) ) + _OUT_EOL )
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE _hbmk_OutStd( hbmk, cText )

   LOCAL nLine
   LOCAL nLines
   LOCAL nWidth
   LOCAL cPrefix
   LOCAL tmp

   IF hbmk[ _HBMK_lDumpInfo ]
      RETURN
   ENDIF

   IF hbmk[ _HBMK_lShowLevel ]
      nWidth := Len( _SELF_NAME_ ) + 5
      cPrefix := hb_StrFormat( _SELF_NAME_ + " #%1$d:", hbmk[ _HBMK_nLevel ] )
   ELSE
      nWidth := Len( _SELF_NAME_ ) + 2
      cPrefix := _SELF_NAME_ + ":"
   ENDIF

   cText := StrTran( cText, e"\n", hb_eol() )
   nLines := MLCount( cText, hbmk[ _HBMK_nMaxCol ] - nWidth )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := RTrim( MemoLine( cText, hbmk[ _HBMK_nMaxCol ] - nWidth, nLine ) ) )
         OutStd( iif( nLine == 1, PadR( cPrefix, nWidth ), Space( nWidth ) ) + tmp + _OUT_EOL )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE _hbmk_OutErr( hbmk, cText )

   LOCAL nLine
   LOCAL nLines
   LOCAL nWidth
   LOCAL cPrefix
   LOCAL tmp

   IF hbmk[ _HBMK_lDumpInfo ]
      RETURN
   ENDIF

   IF hbmk[ _HBMK_lShowLevel ]
      nWidth := Len( _SELF_NAME_ ) + 5
      cPrefix := hb_StrFormat( _SELF_NAME_ + " #%1$d:", hbmk[ _HBMK_nLevel ] )
   ELSE
      nWidth := Len( _SELF_NAME_ ) + 2
      cPrefix := _SELF_NAME_ + ":"
   ENDIF

   cText := StrTran( cText, e"\n", hb_eol() )
   nLines := MLCount( cText, hbmk[ _HBMK_nMaxCol ] - nWidth )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := RTrim( MemoLine( cText, hbmk[ _HBMK_nMaxCol ] - nWidth, nLine ) ) )
         OutErr( iif( nLine == 1, PadR( cPrefix, nWidth ), Space( nWidth ) ) + tmp + _OUT_EOL )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION LicenseString()
#pragma __cstream | RETURN %s
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
their web site at http://www.gnu.org/).

License extensions:
  - This source code must be kept and distributed as part
    of the Harbour package and/or the placement of the tool sources
    and files must reflect that it is part of Harbour Project.
  - Copyright information must always be presented by
    projects including this tool or help text.
  - Modified versions of the tool must clearly state this
    fact on the copyright screen.
  - Source code modifications shall always be made available
    along with binaries.
  - Help text and documentation is licensed under
    Creative Commons Attribution-ShareAlike 3.0:
    http://creativecommons.org/licenses/by-sa/3.0/
#pragma __endtext
