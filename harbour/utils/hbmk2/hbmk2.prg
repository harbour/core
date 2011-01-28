/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Make (alias hbmk, alias hbmk2)
 *
 * Copyright 1999-2011 Viktor Szakats (harbour.01 syenar.hu)
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
 *   - hbmk2 copyright information must always be presented by
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
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
 *    gcc and *nix configuration elements.
 *    bash script with similar purpose for gcc family.
 *    entry point override method and detection code for gcc.
 *    rtlink/blinker link script parsers.
 *    POTMerge(), LoadPOTFilesAsHash(), GenHBL() and AutoTrans().
 *       (with local modifications by hbmk2 author)
 *    optimized header time scan algorithm
 *
 * See COPYING for licensing terms.
 *
 */

/* Optimizations */
#pragma -km+
#pragma -ko+

/*
   Program Library HOWTO:
      http://www.linux.org/docs/ldp/howto/Program-Library-HOWTO/index.html

   Man page HOWTO:
      http://www.schweikhardt.net/man_page_howto.html
   Groff manual:
      http://www.gnu.org/software/groff/manual/html_node/index.html
      http://www.gnu.org/software/groff/manual/groff.pdf
   Troff manual:
      http://cm.bell-labs.com/sys/doc/troff.pdf
 */

#include "common.ch"
#include "directry.ch"
#include "error.ch"
#include "fileio.ch"
#include "simpleio.ch" /* Don't delete this, it's useful for development. */

#include "hbgtinfo.ch"
#include "hbhrb.ch"
#include "hbver.ch"

/* NOTE: Keep this code clean from any kind of contribs and Harbour level
         3rd party library/tool information. This (the hbmk2) component
         shall only contain hard-wired knowledge on Harbour _core_
         (official interfaces preferred), C compilers and OS details on
         the smallest possible level.
         Instead, 3rd party Harbour packages are recommended to maintain
         and provide .hbc files themselves, as part of their standard
         distribution packages. You can find a few such .hbc examples in
         the 'examples' directory. For Harbour contribs, the recommended
         method is to supply and maintain .hbc files in their respective
         directories, usually under tests (or utils, examples). As of this
         writing, most of them has one created.
         Thank you. [vszakats] */

/* TODO: Support debug/release modes. Some default setting can be set
         accordingly, and user can use it to further tweak settings. */
/* TODO: Support unicode/non-unicode build modes. */
/* TODO: Further clean hbmk context var usage (hbmk2 scope, project scope,
         adding rest of variables). */
/* TODO: Add a way to fallback to stop if required headers couldn't be found.
         This needs a way to spec what key headers to look for. */

/* TODO: Clean up compiler autodetection and add those few feature only
         found in GNU Make / global.mk, like *nix native autodetection,
         autodetection of watcom cross-build setups, poccarm/pocc64 setups,
         clang, etc. */

/* TODO: Use hashes instead of arrays for input files, options */
/* TODO: Avoid adding certain options and input files twice */

/* TODO: Next gen compiler autodetection:
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
                  On IA64 Windows: msvcia64, msvc, msvc64, mingw, mingw64, ... */

#ifndef _HBMK_EMBEDDED_

#include "hbextcdp.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

/* Include these for -pause support. */
#if   defined( __PLATFORM__WINCE )
   REQUEST HB_GT_WVT
#elif defined( __PLATFORM__WINDOWS )
   REQUEST HB_GT_WIN
#elif defined( __PLATFORM__DOS )
   REQUEST HB_GT_DOS
#elif defined( __PLATFORM__OS2 )
   REQUEST HB_GT_OS2
#elif defined( __PLATFORM__UNIX ) .AND. ! defined( __PLATFORM__VXWORKS ) .AND. ! defined( __PLATFORM__SYMBIAN )
   REQUEST HB_GT_TRM
#endif

#endif

REQUEST hbmk_KEYW

#define I_( x )                 hb_UTF8ToStr( hb_i18n_gettext( x ) )

#define _TARG_PLAT              1
#define _TARG_COMP              2
#define _TARG_CPU               3

#define _PAR_cParam             1
#define _PAR_cFileName          2
#define _PAR_nLine              3

#define _WARN_DEF               0 /* Don't set any explicit warning level */
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

#define _COMPDET_bBlock         1
#define _COMPDET_cCOMP          2
#define _COMPDET_cCCPREFIX      3 /* optional */
#define _COMPDET_cCCPOSTFIX     4 /* optional */
#define _COMPDET_cPLAT          5 /* optional */

#define _COMPDETE_bBlock        1
#define _COMPDETE_cPLAT         2
#define _COMPDETE_cCOMP         3
#define _COMPDETE_cCCPREFIX     4
#define _COMPDETE_cCCPATH       5
#define _COMPDETE_bSetup        6

#define _HBMODE_NATIVE          0
#define _HBMODE_HB10            0x010000
#define _HBMODE_HB20            0x020000
#define _HBMODE_XHB            -0x010200
#define _HBMODE_RAW_C          -1

#define _HBMODE_IS_HB( n )      ( n == 0 .OR. n >= _HBMODE_HB10 )
#define _HBMODE_IS_OLDHB( n )   ( n >= _HBMODE_HB10 )
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

#define _MACRO_NO_PREFIX        ""
#define _MACRO_NORM_PREFIX      "$"
#define _MACRO_LATE_PREFIX      "%"
#define _MACRO_PREFIX_ALL       ( _MACRO_NORM_PREFIX + _MACRO_LATE_PREFIX )
#define _MACRO_OPEN             "{"
#define _MACRO_CLOSE            "}"

#define _CMDSUBST_OPEN          "`"
#define _CMDSUBST_CLOSE         _CMDSUBST_OPEN

#define _LNG_MARKER             ( _MACRO_LATE_PREFIX + _MACRO_OPEN + "hb_lng" + _MACRO_CLOSE )

#define _HBMK_ENV_NAME          "HBMK_OPTIONS"
#define _HBMK_AUTOHBC_NAME      "hbmk.hbc"
#define _HBMK_AUTOHBM_NAME      "hbmk.hbm"

#define _HBMK_WITH_TPL          "HBMK_WITH_%1$s"
#define _HBMK_HAS_TPL           "HBMK_HAS_%1$s"
#define _HBMK_HAS_TPL_LOCAL     "HBMK_HAS_%1$s_LOCAL"
#define _HBMK_SCRIPT            "__HBSCRIPT__HBMK"

#define _HBMK_IMPLIB_EXE_POST   "_exe"
#define _HBMK_IMPLIB_DLL_POST   "_dll"

#define _HBMK_TARGENAME_ADHOC   ".adhoc."

#define _HBMK_NEST_MAX          10
#define _HBMK_HEAD_NEST_MAX     10

#define _VAR_MODE_SET           1
#define _VAR_MODE_APPEND        2
#define _VAR_MODE_INSERT        3
#define _VAR_MODE_DELETE        4

#define _COMPEMBED_BASE_        ( "comp" + hb_ps() )

#if defined( __PLATFORM__DOS )
   #define _WORKDIR_BASE_          "~hbmk"
#else
   #define _WORKDIR_BASE_          ".hbmk"
#endif
#define _WORKDIR_DEF_           ( _WORKDIR_BASE_ + hb_ps() + hbmk[ _HBMK_cPLAT ] + hb_ps() + hbmk[ _HBMK_cCOMP ] )

#define _BCC_BIN_DETECT()       FindInPath( "bcc32.exe" )

/* Macro to check for uppercase extension on case-sensitive filesystems */
#if defined( __PLATFORM__DOS )
   #define _EXT_IS_UPPER( f, e ) ( .F. )
#else
   #define _EXT_IS_UPPER( f, e ) ( FNameExtGet( f ) == e )
#endif

#define HB_ISALPHA( c )         ( Upper( c ) >= "A" .AND. Upper( c ) <= "Z" )
#define HB_ISFIRSTIDCHAR( c )   ( HB_ISALPHA( c ) .OR. ( c ) == '_' )
#define HB_ISNEXTIDCHAR( c )    ( HB_ISFIRSTIDCHAR(c) .OR. IsDigit( c ) )

#define LEFTEQUAL( l, r )       ( l = r ) /* NOTE: This requires Set( _SET_EXACT, .F. ) */

/* Logic (hack) to automatically add some libs to their
   right place in the liblist. In case of 'unicows' lib,
   this should be after all app lib and before any Windows
   system libs. [vszakats] */
#define _IS_AUTOLIBSYSPRE( c )  ( hbmk[ _HBMK_cPLAT ] == "win" .AND. Lower( FNameNameGet( c ) ) == "unicows" )

#define _CHR_EOL                Chr( 10 )
#define _OUT_EOL                Chr( 10 )

#define _HBMK_IMPLIB_NOTFOUND   ( -1 )
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
#define _HBMK_cGT               10

#define _HBMK_cHB_INSTALL_BIN   11
#define _HBMK_cHB_INSTALL_LIB   12
#define _HBMK_cHB_INSTALL_DYN   13
#define _HBMK_cHB_INSTALL_INC   14

#define _HBMK_lGUI              15
#define _HBMK_lMT               16
#define _HBMK_lDEBUG            17
#define _HBMK_nHEAD             18
#define _HBMK_aINCPATH          19
#define _HBMK_lREBUILD          20
#define _HBMK_lCLEAN            21
#define _HBMK_lTRACE            22
#define _HBMK_lDONTEXEC         23
#define _HBMK_nHBMODE           24
#define _HBMK_cUILNG            25
#define _HBMK_cUICDP            26
#define _HBMK_aLIBUSER          27
#define _HBMK_aLIBUSERGT        28
#define _HBMK_aLIBUSERSYS       29
#define _HBMK_aLIBUSERSYSPRE    30
#define _HBMK_aLIBPATH          31
#define _HBMK_aINSTPATH         32
#define _HBMK_aOPTC             33
#define _HBMK_aOPTPRG           34
#define _HBMK_aOPTRES           35
#define _HBMK_aOPTL             36
#define _HBMK_aOPTA             37
#define _HBMK_aOPTD             38
#define _HBMK_aOPTI             39
#define _HBMK_lCPP              40
#define _HBMK_lSHARED           41
#define _HBMK_lSTATICFULL       42
#define _HBMK_lSHAREDDIST       43
#define _HBMK_lNULRDD           44
#define _HBMK_lMAP              45
#define _HBMK_lBEEP             46
#define _HBMK_lSTRIP            47
#define _HBMK_lOPTIM            48
#define _HBMK_nCOMPR            49
#define _HBMK_nWARN             50
#define _HBMK_lRUN              51
#define _HBMK_lINC              52
#define _HBMK_lREBUILDPO        53
#define _HBMK_lMINIPO           54
#define _HBMK_lUNICODE          55
#define _HBMK_nCONF             56
#define _HBMK_lIGNOREERROR      57
#define _HBMK_lIMPLIB           58
#define _HBMK_lHBCPPMM          59
#define _HBMK_aVAR              60
#define _HBMK_hDEP              61

#define _HBMK_lCreateLib        62
#define _HBMK_lCreateDyn        63
#define _HBMK_lCreateImpLib     64
#define _HBMK_lCreatePPO        65
#define _HBMK_lCreateHRB        66

#define _HBMK_lDynVM            67

#define _HBMK_lBLDFLGP          68
#define _HBMK_lBLDFLGC          69
#define _HBMK_lBLDFLGL          70

#define _HBMK_cFIRST            71
#define _HBMK_aPRG              72
#define _HBMK_aC                73
#define _HBMK_aCPP              74
#define _HBMK_aRESSRC           75
#define _HBMK_aRESCMP           76
#define _HBMK_aOBJUSER          77
#define _HBMK_aICON             78
#define _HBMK_aIMPLIBSRC        79
#define _HBMK_aDEF              80
#define _HBMK_aINSTFILE         81
#define _HBMK_hDEPTS            82

#define _HBMK_aPO               83
#define _HBMK_cHBL              84
#define _HBMK_cHBLDir           85
#define _HBMK_aLNG              86
#define _HBMK_cPO               87

#define _HBMK_hPLUGINHRB        88
#define _HBMK_hPLUGINVars       89
#define _HBMK_aPLUGINPars       90
#define _HBMK_hPLUGINExt        91

#define _HBMK_lDEBUGTIME        92
#define _HBMK_lDEBUGINC         93
#define _HBMK_lDEBUGSTUB        94
#define _HBMK_lDEBUGI18N        95
#define _HBMK_lDEBUGDEPD        96
#define _HBMK_lDEBUGPARS        97

#define _HBMK_cCCPATH           98
#define _HBMK_cCCPREFIX         99
#define _HBMK_cCCPOSTFIX        100
#define _HBMK_cCCEXT            101

#define _HBMK_cWorkDir          102
#define _HBMK_cWorkDirDynSub    103
#define _HBMK_nCmd_Esc          104
#define _HBMK_nScr_Esc          105
#define _HBMK_nCmd_FNF          106
#define _HBMK_nScr_FNF          107
#define _HBMK_nErrorLevel       108

#define _HBMK_cPROGDIR          109
#define _HBMK_cPROGNAME         110

#define _HBMK_hAUTOHBC          111 /* trigger header => .hbc associations */
#define _HBMK_hAUTOHBCFOUND     112 /* trigger headers found */

#define _HBMK_aDEPTHBC          113 /* .hbc references found */
#define _HBMK_hDEPTSDIR         114 /* Header dirs found for dependencies */

#define _HBMK_lStopAfterInit    115
#define _HBMK_lStopAfterHarbour 116

#define _HBMK_nCOMPVer          117
#define _HBMK_lDEPIMPLIB        118 /* Generate import libs configured in dependecy specification */
#define _HBMK_lInstForce        119 /* Force to install target even if was up to date */
#define _HBMK_lAutoHBM          120 /* Toggles processing of hbmk.hbm file in current directory */
#define _HBMK_lContainer        121 /* Target type: container */
#define _HBMK_lShowLevel        122 /* Show project nesting level in all output lines */
#define _HBMK_hFiles            123 /* Cache for the header parser (common for C and Harbour) */
#define _HBMK_cDynLibPrefix     124 /* Dynamic lib filename prefix */
#define _HBMK_cDynLibExt        125 /* Dynamic lib filename extension */
#define _HBMK_aLINK             126 /* Links to be created and pointing to the target */
#define _HBMK_hDEPTMACRO        127 /* Links to be created and pointing to the target */

#define _HBMK_aArgs             128
#define _HBMK_nArgTarget        129
#define _HBMK_lPause            130
#define _HBMK_nLevel            131

#define _HBMK_cHBX              132

#define _HBMK_MAX_              132

#define _HBMK_DEP_CTRL_MARKER   ".control." /* must be an invalid path */

#define _HBMKDEP_cName          1
#define _HBMKDEP_aPKG           2
#define _HBMKDEP_aKeyHeader     3
#define _HBMKDEP_cControl       4
#define _HBMKDEP_lOptional      5
#define _HBMKDEP_aINCPATH       6
#define _HBMKDEP_aINCPATHLOCAL  7
#define _HBMKDEP_aIMPLIBSRC     8
#define _HBMKDEP_cIMPLIBDST     9
#define _HBMKDEP_cFound         10
#define _HBMKDEP_lFound         11
#define _HBMKDEP_lFoundLOCAL    12
#define _HBMKDEP_cVersion       13
#define _HBMKDEP_lForced        14
#define _HBMKDEP_lDetected      15
#define _HBMKDEP_MAX_           15

#ifndef _HBMK_EMBEDDED_

#define _ERRLEV_OK              0
#define _ERRLEV_UNKNPLAT        1
#define _ERRLEV_UNKNCOMP        2
#define _ERRLEV_FAILHBDETECT    3
#define _ERRLEV_STUBCREATE      5
#define _ERRLEV_COMPPRG         6
#define _ERRLEV_RUNRES          6
#define _ERRLEV_COMPC           6
#define _ERRLEV_RUNLINKER       7
#define _ERRLEV_RUNLIB          7
#define _ERRLEV_UNSUPPORTED     8
#define _ERRLEV_WORKDIRCREATE   9
#define _ERRLEV_HELP            19
#define _ERRLEV_MISSDEPT        10
#define _ERRLEV_PLUGINPREALL    20
#define _ERRLEV_DEEPPROJNESTING 30
#define _ERRLEV_STOP            50

#define HBMK_IS_IN( str, list ) ( "|" + str + "|" $ "|" + list + "|" )

#define HBMK_ISPLAT( list )     HBMK_IS_IN( hbmk[ _HBMK_cPLAT ], list )
#define HBMK_ISCOMP( list )     HBMK_IS_IN( hbmk[ _HBMK_cCOMP ], list )

#define hb_DirCreate( d )       MakeDir( d )
#define hb_DirDelete( d )       DirRemove( d )

/* Request some functions for plugins */
REQUEST HB_REGEX
REQUEST HBCLASS
REQUEST __CLSLOCKDEF
REQUEST HB_HKEEPORDER
REQUEST HB_CRC32
REQUEST __HBDOC_TOSOURCE

/* NOTE: Security token to protect against plugins accessing our
         internal structures referenced from context variable */
STATIC s_cSecToken := NIL

PROCEDURE _APPMAIN( ... )
   LOCAL aArgsProc
   LOCAL nResult
   LOCAL tmp, tmp1

   LOCAL lPause := hb_gtInfo( HB_GTI_ISGRAPHIC )

   LOCAL aArgsTarget
   LOCAL nTarget
   LOCAL nTargetTODO
   LOCAL nTargetPos
   LOCAL lHadTarget

   LOCAL lOldExact

   /* Expand wildcard project specs */

   aArgsProc := {}
   FOR EACH tmp IN hb_AParams()
      DO CASE
      CASE !( Left( tmp, 1 ) $ "-@" ) .AND. Lower( FNameExtGet( tmp ) ) == ".hbp"
         FOR EACH tmp1 IN FN_Expand( tmp, .T. )
            AAdd( aArgsProc, tmp1 )
         NEXT
      CASE Lower( Left( tmp, Len( "-target=" ) ) ) == "-target="
         FOR EACH tmp1 IN FN_Expand( SubStr( tmp, Len( "-target=" ) + 1 ), .F. )
            AAdd( aArgsProc, "-target=" + tmp1 )
         NEXT
#if 0
      CASE Lower( FNameExtGet( tmp ) ) == ".hbs"
         hbrun_main( tmp )
         QUIT
      CASE Lower( tmp ) == "-ui"
         #if defined( __PLATFORM__WINCE )
            hb_gtSelect( hb_gtCreate( "GTWVT" ) )
         #elif defined( __PLATFORM__WINDOWS )
            hb_gtSelect( hb_gtCreate( "GTWIN" ) )
         #endif
         hbrun_main()
         QUIT
#endif
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
      ENDIF

      DO CASE
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

   lOldExact := Set( _SET_EXACT, .F. )

   nTargetTODO := 1
   DO WHILE .T.

      aArgsTarget := {}
      nTarget := 0
      nTargetPos := 0
      lHadTarget := .F.

      FOR EACH tmp IN aArgsProc
         DO CASE
         CASE !( Left( tmp, 1 ) $ "-@" ) .AND. ;
              Lower( FNameExtGet( tmp ) ) == ".hbp" .AND. ;
              ! lHadTarget
            ++nTarget
            IF nTarget == nTargetTODO
               AAdd( aArgsTarget, tmp )
               nTargetPos := Len( aArgsTarget )
            ENDIF
         CASE Lower( Left( tmp, Len( "-target=" ) ) ) == "-target="
            ++nTarget
            IF nTarget == nTargetTODO
               AAdd( aArgsTarget, SubStr( tmp, Len( "-target=" ) + 1 ) )
               nTargetPos := Len( aArgsTarget )
            ENDIF
         OTHERWISE
            IF ! lHadTarget .OR. nTarget == nTargetTODO
               AAdd( aArgsTarget, tmp )
            ENDIF
         ENDCASE
      NEXT

      /* Exit if there was no more targets found on the command line */
      IF nTarget < nTargetTODO .AND. nTargetTODO != 1
         EXIT
      ENDIF

      /* Build one target */
      nResult := hbmk2( aArgsTarget, nTargetPos, @lPause, 1 )

      /* Exit on first failure */
      IF nResult != 0
         EXIT
      ENDIF

      ++nTargetTODO
   ENDDO

   Set( _SET_EXACT, lOldExact )

   IF nResult != 0 .AND. lPause
      OutStd( I_( "Press any key to continue..." ) )
      Inkey( 0 )
   ENDIF

   ErrorLevel( nResult )

   RETURN

#endif

STATIC PROCEDURE hbmk_COMP_Setup( cARCH, cCOMP, cBasePath )

   /* TODO: Use HB_CCPREFIX instead of PATH modification, where possible. */

   /* NOTE: We have to retain existing PATH as we may need some tools
            from it, like upx compressor. [vszakats] */

   cBasePath := PathNormalize( cBasePath )

   DO CASE
   CASE cARCH == "dos" .AND. cCOMP == "djgpp"

      hb_SetEnv( "DJGPP", cBasePath + hb_ps() + "djgpp.env" )
      hb_SetEnv( "PATH", cBasePath + hb_ps() + "bin" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )

   CASE cARCH == "win" .AND. cCOMP == "mingw"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "bin" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )

   CASE cARCH == "win" .AND. cCOMP == "pocc"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "Bin" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "Include" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Include" + hb_ps() + "Win" )
      hb_SetEnv( "LIB", cBasePath + hb_ps() + "Lib" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Lib" + hb_ps() + "Win" )

   CASE cARCH == "win" .AND. cCOMP == "pocc64"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "Bin" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "Include" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Include" + hb_ps() + "Win" )
      hb_SetEnv( "LIB", cBasePath + hb_ps() + "Lib" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Lib" + hb_ps() + "Win64" )

   CASE cARCH == "wce" .AND. cCOMP == "poccarm"

      hb_SetEnv( "PATH", cBasePath + hb_ps() + "Bin" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_ps() + "Include" + hb_ps() + "WinCE" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Include" )
      hb_SetEnv( "LIB", cBasePath + hb_ps() + "Lib" + hb_osPathListSeparator() + cBasePath + hb_ps() + "Lib" + hb_ps() + "WinCE" )

   CASE cCOMP == "watcom"

      hb_SetEnv( "WATCOM", cBasePath )
      hb_SetEnv( "EDPATH", cBasePath + hb_ps() + "eddat" )

      #if   defined( __PLATFORM__WINDOWS )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binnt" + hb_osPathListSeparator() + cBasePath + hb_ps() + "binw" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
      #elif defined( __PLATFORM__OS2 )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binp" + hb_osPathListSeparator() + cBasePath + hb_ps() + "binw" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
      #elif defined( __PLATFORM__DOS )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binw" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
      #elif defined( __PLATFORM__LINUX )
         hb_SetEnv( "PATH", cBasePath + hb_ps() + "binl" + hb_osPathListSeparator() + hb_GetEnv( "PATH" ) )
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

FUNCTION hbmk2( aArgs, nArgTarget, /* @ */ lPause, nLevel )

   LOCAL hbmk[ _HBMK_MAX_ ]

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

   LOCAL l_cHB_INSTALL_PREFIX
   LOCAL l_cHB_INSTALL_BIN
   LOCAL l_cHB_INSTALL_LIB
   LOCAL l_cHB_INSTALL_DYN
   LOCAL l_cHB_INSTALL_INC
   LOCAL l_cHB_INSTALL_ADD

   LOCAL l_aPRG_TODO
   LOCAL l_aC_TODO
   LOCAL l_aCPP_TODO
   LOCAL l_aCGEN_TODO
   LOCAL l_aRESSRC_TODO
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
   LOCAL l_cHBPOSTFIX := ""
   LOCAL l_lNOHBLIB := .F.
   LOCAL l_lLIBSYSMISC := .T.
   LOCAL l_cCMAIN := NIL
   LOCAL l_lTargetSelected := .F.

   /* hbmk2 lib ordering tries to satisfy linkers which require this
      (mingw*, linux/gcc, bsd/gcc and dos/djgpp), but this won't solve
      potential problems when users are speccing custom libs themselves
      and expect them to work the same way on all supported platforms/compilers.
      So I decided to readd this feature until we find a solution which
      doesn't have such bad side-effect.
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

   LOCAL cCommand
   LOCAL aCommand
   LOCAL cOpt_CompC
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
   LOCAL tmp, tmp1, tmp2, tmp3, array
   LOCAL cLibBCC_CRTL
   LOCAL cScriptFile
   LOCAL fhnd
   LOCAL cFile
   LOCAL lSysLoc
   LOCAL cPrefix
   LOCAL cPostfix

   LOCAL lSkipBuild := .F.
   LOCAL lStopAfterCComp := .F.
   LOCAL lAcceptCFlag := .F.
   LOCAL lAcceptLDFlag := .F.
   LOCAL lAcceptLDClipper := .F.
   LOCAL lAcceptIFlag := .F.
   LOCAL lHarbourInfo := .F.
   LOCAL lDumpInfo := .F.

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

   LOCAL aTODO
   LOCAL aThreads
   LOCAL thread

   LOCAL nStart := Seconds()

   LOCAL lDoSupportDetection
   LOCAL lDeleteWorkDir := .F.

   LOCAL lHBMAINDLLP

   IF s_cSecToken == NIL
      s_cSecToken := StrZero( hb_Random( 1, 4294967294 ), 10, 0 )
   ENDIF

   hbmk[ _HBMK_cBUILD ] := ""

   hbmk[ _HBMK_lStopAfterInit ] := .F.
   hbmk[ _HBMK_lStopAfterHarbour ] := .F.

   hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_OK

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
   hbmk[ _HBMK_aVAR ] := {}
   hbmk[ _HBMK_hDEP ] := { => }
   hbmk[ _HBMK_hAUTOHBC ] := { => }
   hbmk[ _HBMK_hAUTOHBCFOUND ] := { => }
   hbmk[ _HBMK_aDEPTHBC ] := {}
   hbmk[ _HBMK_lDEPIMPLIB ] := .T.

   hb_HSetCaseMatch( hbmk[ _HBMK_hDEP ], .F. )

   hbmk[ _HBMK_lBLDFLGP ] := .F.
   hbmk[ _HBMK_lBLDFLGC ] := .F.
   hbmk[ _HBMK_lBLDFLGL ] := .F.

   hbmk[ _HBMK_hPLUGINHRB ] := { => }
   hbmk[ _HBMK_hPLUGINVars ] := { => }
   hbmk[ _HBMK_aPLUGINPars ] := {}
   hbmk[ _HBMK_hPLUGINExt ] := { => }

   hb_HSetCaseMatch( hbmk[ _HBMK_hPLUGINExt ], .F. )

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

   hbmk[ _HBMK_aArgs ] := aArgs
   hbmk[ _HBMK_nArgTarget ] := nArgTarget
   hbmk[ _HBMK_lPause ] := lPause
   hbmk[ _HBMK_nLevel ] := nLevel

   GetUILangCDP( @hbmk[ _HBMK_cUILNG ], @hbmk[ _HBMK_cUICDP ] )
   SetUILang( hbmk )

   IF Empty( aArgs )
      ShowHeader( hbmk )
      ShowHelp( hbmk )
      RETURN _ERRLEV_HELP
   ENDIF

   /* Process environment */

   cEnv := GetEnv( _HBMK_ENV_NAME )

   /* Compatibility */

   FOR EACH tmp IN ListToArray( PathSepToSelf( GetEnv( "HB_USER_LIBPATHS" ) ) )
      cEnv += " -L" + tmp
   NEXT
   FOR EACH tmp IN ListToArray( PathSepToSelf( GetEnv( "HB_USER_LIBS" ) ) )
      cEnv += " -l" + tmp
   NEXT
   IF ! Empty( GetEnv( "HB_PLATFORM" ) )
      cEnv += " -platform=" + GetEnv( "HB_PLATFORM" )
   ENDIF
   IF ! Empty( GetEnv( "HB_COMPILER" ) )
      cEnv += " -compiler=" + GetEnv( "HB_COMPILER" )
   ENDIF
   IF ! Empty( GetEnv( "HB_CPU" ) )
      cEnv += " -cpu=" + GetEnv( "HB_CPU" )
   ENDIF
   IF ! Empty( GetEnv( "HB_BUILD_NAME" ) )
      cEnv += " -build=" + PathSepToSelf( GetEnv( "HB_BUILD_NAME" ) )
   ENDIF
   cEnv := AllTrim( cEnv )

   IF ! Empty( cEnv )
      aArgs := ArrayJoin( ListToArray( cEnv ), aArgs )
   ENDIF

   /* ; */

   FOR EACH cParam IN aArgs

      cParamL := Lower( cParam )

      /* NOTE: Don't forget to make these ignored in the main
               option processing loop. */
      DO CASE
      CASE cParamL             == "-quiet"     ; hbmk[ _HBMK_lQuiet ] := .T. ; hbmk[ _HBMK_lInfo ] := .F.
      CASE cParamL             == "-quiet-"    ; hbmk[ _HBMK_lQuiet ] := .F.
      CASE Left( cParamL, 6 )  == "-comp="     ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 7 ), _TARG_COMP )
      CASE Left( cParamL, 10 ) == "-compiler=" ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 11 ), _TARG_COMP )
      CASE Left( cParamL, 6 )  == "-plat="     ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 7 ), _TARG_PLAT )
      CASE Left( cParamL, 10 ) == "-platform=" ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 11 ), _TARG_PLAT )
      CASE Left( cParamL, 6 )  == "-arch="     ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 7 ), _TARG_PLAT ) /* Compatibility */
      CASE Left( cParamL, 5 )  == "-cpu="      ; ParseCOMPPLATCPU( hbmk, SubStr( cParam, 6 ), _TARG_CPU )
      CASE Left( cParamL, 7 )  == "-build="    ; hbmk[ _HBMK_cBUILD ] := StrTran( PathSepToSelf( SubStr( cParam, 8 ) ), hb_ps() )
      CASE Left( cParamL, 6 )  == "-build"     ; hbmk[ _HBMK_lStopAfterHarbour ] := .T.
      CASE Left( cParamL, 6 )  == "-lang="     ; hbmk[ _HBMK_cUILNG ] := SubStr( cParam, 7 ) ; SetUILang( hbmk )
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
      CASE cParamL             == "-hbc"       ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_RAW_C ; lAcceptCFlag := .T.
      CASE Left( cParamL, 5 )  == "-env:"

         tmp := SubStr( cParam, 6 )
         IF ! Empty( tmp )
            IF     ( tmp1 := At( "=", tmp ) ) > 1
               tmp2 := _VAR_MODE_SET
            ELSEIF ( tmp1 := At( "+", tmp ) ) > 1
               tmp2 := _VAR_MODE_APPEND
            ELSEIF ( tmp1 := At( "#", tmp ) ) > 1
               tmp2 := _VAR_MODE_INSERT
            ELSEIF ( tmp1 := At( "-", tmp ) ) > 1
               tmp2 := _VAR_MODE_DELETE
            ELSE
               tmp2 := _VAR_MODE_SET
               tmp1 := Len( tmp ) + 1
            ENDIF
            AAdd( hbmk[ _HBMK_aVAR ], { tmp2, Left( tmp, tmp1 - 1 ), SubStr( tmp, tmp1 + 1 ) } )
         ENDIF

      CASE cParamL == "-help" .OR. ;
           cParamL == "--help"

         ShowHeader( hbmk )
         ShowHelp( hbmk, .T. )
         RETURN _ERRLEV_HELP

      CASE Left( cParamL, 8 ) == "-hbmake="

         convert_hbmake_to_hbp( hbmk, SubStr( cParam, 9 ) )
         RETURN _ERRLEV_OK

      CASE Left( cParamL, 5 ) == "-xbp="

         convert_xbp_to_hbp( hbmk, SubStr( cParam, 6 ) )
         RETURN _ERRLEV_OK

      CASE Left( cParamL, 5 ) == "-xhp="

         convert_xhp_to_hbp( hbmk, SubStr( cParam, 6 ) )
         RETURN _ERRLEV_OK

      CASE cParamL == "--version"

         ShowHeader( hbmk )
         RETURN _ERRLEV_OK

      ENDCASE
   NEXT

   IF nLevel > _HBMK_NEST_MAX
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Cannot nest projects deeper than %1$s levels" ), hb_ntos( _HBMK_NEST_MAX ) ) )
      RETURN _ERRLEV_DEEPPROJNESTING
   ENDIF

   IF nLevel > 1
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Building sub-project (level %1$s): %2$s" ), hb_ntos( nLevel ), hbmk[ _HBMK_aArgs ][ hbmk[ _HBMK_nArgTarget ] ] ) )
      ENDIF
   ENDIF

   IF ! Empty( cEnv )
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing environment options: %1$s" ), cEnv ) )
      ENDIF
   ENDIF

   FOR EACH tmp IN hbmk[ _HBMK_aVAR ]
      SWITCH tmp[ 1 ]
      CASE _VAR_MODE_SET    ; hb_SetEnv( tmp[ 2 ], tmp[ 3 ] ) ; EXIT
      CASE _VAR_MODE_INSERT ; hb_SetEnv( tmp[ 2 ], tmp[ 3 ] + hb_GetEnv( tmp[ 2 ] ) ) ; EXIT
      CASE _VAR_MODE_APPEND ; hb_SetEnv( tmp[ 2 ], hb_GetEnv( tmp[ 2 ] ) + tmp[ 3 ] ) ; EXIT
      CASE _VAR_MODE_DELETE ; hb_SetEnv( tmp[ 2 ] ) ; EXIT
      ENDSWITCH
   NEXT

   /* Initialize Harbour libs */

   IF ! _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )

      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
         cDL_Version_Alter := "-" +;
                              hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) +;
                              hb_ntos( hb_Version( HB_VERSION_MINOR ) )
         cDL_Version       := "." +;
                              hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) + "." +;
                              hb_ntos( hb_Version( HB_VERSION_MINOR ) ) + "." +;
                              hb_ntos( hb_Version( HB_VERSION_RELEASE ) )
      ELSE
         cDL_Version_Alter := "-" +;
                              hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ], -16 ), 0xFF ) ) +;
                              hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ],  -8 ), 0xFF ) )
         cDL_Version       := "." +;
                              hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ], -16 ), 0xFF ) ) + "." +;
                              hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ],  -8 ), 0xFF ) ) + "." +;
                              hb_ntos( hb_bitAnd( hb_bitShift( hbmk[ _HBMK_nHBMODE ],   0 ), 0xFF ) )
      ENDIF

      aLIB_BASE_EXTERN  := { "hbextern" }
      aLIB_BASE_DEBUG   := { "hbdebug" }
      aLIB_BASE_1       := { "hbvm", "hbrtl", "hblang", "hbcpage" }
      aLIB_BASE_1_MT    := iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_HB10, { "hbvmmt", "hbrtl", "hblang", "hbcpage" }, aLIB_BASE_1 )
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
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected platform: %1$s" ), hbmk[ _HBMK_cPLAT ] ) )
         ENDIF
      ENDIF
   ENDIF

   hbmk[ _HBMK_cCCPATH ]    := GetEnv( "HB_CCPATH" )
   hbmk[ _HBMK_cCCPREFIX ]  := GetEnv( "HB_CCPREFIX" )
   hbmk[ _HBMK_cCCPOSTFIX ] := GetEnv( "HB_CCPOSTFIX" )

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

   cBin_CompPRG := "harbour" + l_cHBPOSTFIX

   DO CASE
   CASE HBMK_ISPLAT( "darwin|bsd|hpux|sunos|beos|qnx|vxworks|symbian|linux|cygwin" )
      DO CASE
      CASE hbmk[ _HBMK_cPLAT ] == "linux"
         aCOMPSUP := { "gcc", "clang", "icc", "watcom", "sunpro", "open64" }
      CASE hbmk[ _HBMK_cPLAT ] == "darwin"
         aCOMPSUP := { "gcc", "clang", "icc" }
      CASE hbmk[ _HBMK_cPLAT ] == "bsd"
         aCOMPSUP := { "gcc", "clang", "pcc" }
      CASE hbmk[ _HBMK_cPLAT ] == "sunos"
         aCOMPSUP := { "gcc", "sunpro" }
      CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
         aCOMPSUP := { "gcc", "diab" }
      CASE hbmk[ _HBMK_cPLAT ] == "aix"
         aCOMPSUP := { "gcc", "icc" }
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
      aCOMPDET := { { {|| FindInPath( "gcc"      ) }, "djgpp"  },;
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
      aCOMPDET := { { {|| FindInPath( "gcc"      ) }, "gcc"    },;
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
      aCOMPDET := { { {|| FindInSamePath( "cygstart.exe", "gcc" ) }, "gcc",,, "cygwin" },;
                    { {|| FindInPath( "gcc-dw2" ) }, "mingw", "", "-dw2" },; /* tdragon DWARF-2 build */
                    { {|| FindInPath( "x86_64-pc-mingw32-gcc" ) }, "mingw64" },; /* Equation Solution build */
                    { {|| FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCPOSTFIX ] ) }, "mingw" },;
                    { {|| iif( ! Empty( GetEnv( "WATCOM" ) ),;
                               FindInPath( "wcc386"   ),;
                               NIL )               }, "watcom" },;
                    { {|| FindInPath( "clarm.exe"  ) }, "msvcarm" },;
                    { {|| FindInPath( "armasm.exe" ) }, "msvcarm" },;
                    { {|| FindInPath( "ml64.exe"   ) }, "msvc64" },;
                    { {|| FindInPath( "ias.exe"    ) }, "msvcia64" },;
                    { {|| iif( FindInPath( "wcc386"   ) == NIL,;
                               FindInPath( "cl.exe"   ),;
                               NIL )                      }, "msvc"    },;
                    { {|| _BCC_BIN_DETECT()        }, "bcc"    },;
                    { {|| iif( FindInPath( "dbgeng.lib", GetEnv( "LIB" ) ) != NIL .AND. ( tmp1 := FindInPath( "pocc.exe" ) ) != NIL, tmp1, NIL ) }, "pocc64"  },;
                    { {|| FindInPath( "pocc.exe" ) }, "pocc"   },;
                    { {|| iif( ( tmp1 := FindInPath( "icl.exe" ) ) != NIL .AND. "itanium" $ Lower( tmp1 ), tmp1, NIL ) }, "iccia64" },;
                    { {|| FindInPath( "icl.exe"  ) }, "icc"    },;
                    { {|| FindInPath( "xCC.exe"  ) }, "xcc"    },;
                    { {|| FindInPath( "dmc.exe"  ) }, "dmc"    },;
                    { {|| FindInPath( "i686-w64-mingw32-gcc" ) }, "mingw64", "i686-w64-mingw32-" },; /* mingw-w64 build */
                    { {|| FindInPath( "x86_64-w64-mingw32-gcc" ) }, "mingw64", "x86_64-w64-mingw32-" }} /* mingw-w64 build */
#endif
      aCOMPSUP := { "mingw", "msvc", "bcc", "watcom", "icc", "pocc", "xcc",;
                    "mingw64", "msvc64", "msvcia64", "iccia64", "pocc64" }
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
      aCOMPDET := { { {|| FindInPath( "clarm.exe"  ) }, "msvcarm" },;
                    { {|| FindInPath( "armasm.exe" ) }, "msvcarm" },;
                    { {|| FindInPath( "pocc.exe"   ) }, "poccarm" },;
                    { {|| FindInPath( "arm-mingw32ce-gcc"       ) }, "mingwarm", "arm-mingw32ce-" } ,;
                    { {|| FindInPath( "arm-wince-mingw32ce-gcc" ) }, "mingwarm", "arm-wince-mingw32ce-" } ,;
                    { {|| FindInPath( "i386-mingw32ce-gcc"      ) }, "mingw"   , "i386-mingw32ce-" } ,;
                    { {|| FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCPOSTFIX ] ) }, "mingwarm" } }
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
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Platform value unknown: %1$s" ), hbmk[ _HBMK_cPLAT ] ) )
      RETURN _ERRLEV_UNKNPLAT
   ENDCASE

   hbmk[ _HBMK_aLIBCOREGT ] := ArrayJoin( aLIB_BASE_GT, l_aLIBHBGT )

   /* Setup GUI state for Harbour default */
   SetupForGT( hbmk[ _HBMK_cGTDEFAULT ], NIL, @hbmk[ _HBMK_lGUI ] )

   /* Autodetect Harbour environment */

   IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

      l_cHB_INSTALL_BIN := PathSepToSelf( GetEnv( "HB_INSTALL_BIN" ) )
      l_cHB_INSTALL_LIB := PathSepToSelf( GetEnv( "HB_INSTALL_LIB" ) )
      l_cHB_INSTALL_INC := PathSepToSelf( GetEnv( "HB_INSTALL_INC" ) )

      l_cHB_INSTALL_PREFIX := MacroProc( hbmk, PathSepToSelf( GetEnv( "HB_INSTALL_PREFIX" ) ), NIL, _MACRO_NO_PREFIX )
      IF Empty( l_cHB_INSTALL_PREFIX )
         DO CASE
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + cBin_CompPRG + hbmk[ _HBMK_cCCEXT ] )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".."
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + "bin" + hb_ps() + cBin_CompPRG + hbmk[ _HBMK_cCCEXT ] )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() )
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_ps() + ".." + hb_ps() + "bin" + hb_ps() + cBin_CompPRG + hbmk[ _HBMK_cCCEXT ] )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_ps() + ".."
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_ps() + ".." + hb_ps() + ".." + hb_ps() + "bin" + hb_ps() + cBin_CompPRG + hbmk[ _HBMK_cCCEXT ] )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_ps() + ".." + hb_ps() + ".."
         OTHERWISE
            hbmk_OutErr( hbmk, I_( "Error: HB_INSTALL_PREFIX not set, failed to autodetect.\nPlease run this tool from its original location inside the Harbour installation or set HB_INSTALL_PREFIX environment variable to Harbour's root directory." ) )
            RETURN _ERRLEV_FAILHBDETECT
         ENDCASE
      ENDIF

      /* Detect special non-installed dir layout (after simple 'make') */
      IF hb_FileExists( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + ".." + hb_ps() + ".." + hb_ps() + "include" +;
                                       hb_ps() + "hbvm.h" )
         l_cHB_INSTALL_PREFIX := DirAddPathSep( l_cHB_INSTALL_PREFIX ) + ".." + hb_ps() + ".." + hb_ps()
      /* Detect special multi-host dir layout */
      ELSEIF hb_FileExists( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + ".." + hb_ps() + "include" +;
                                           hb_ps() + "hbvm.h" )
         l_cHB_INSTALL_PREFIX := DirAddPathSep( l_cHB_INSTALL_PREFIX ) + ".." + hb_ps()
      /* Detect non-installed dir layout with build name containing sub-dirs */
      ELSEIF PathSepCount( hbmk[ _HBMK_cBUILD ] ) > 0 .AND. ;
             hb_FileExists( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + Replicate( ".." + hb_ps(), PathSepCount( hbmk[ _HBMK_cBUILD ] ) ) + ".." + hb_ps() + ".." + hb_ps() + "include" +;
                                       hb_ps() + "hbvm.h" )
         l_cHB_INSTALL_PREFIX := DirAddPathSep( l_cHB_INSTALL_PREFIX ) + Replicate( ".." + hb_ps(), PathSepCount( hbmk[ _HBMK_cBUILD ] ) ) + ".." + hb_ps() + ".." + hb_ps()
      ENDIF

      /* Detect special *nix dir layout (/bin, /lib/harbour, /lib64/harbour, /include/harbour) */
      IF hb_FileExists( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "include" +;
                                       hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) +;
                                       hb_ps() + "hbvm.h" )
         IF Empty( l_cHB_INSTALL_BIN )
            l_cHB_INSTALL_BIN := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "bin" )
         ENDIF
         IF Empty( l_cHB_INSTALL_LIB )
            IF hb_DirExists( tmp := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "lib64" + hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) ) )
               l_cHB_INSTALL_LIB := tmp
            ELSE
               l_cHB_INSTALL_LIB := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "lib" + hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) )
            ENDIF
         ENDIF
         IF Empty( l_cHB_INSTALL_INC )
            l_cHB_INSTALL_INC := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "include" + hb_ps() + iif( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ), "xharbour", "harbour" ) )
         ENDIF
      ENDIF

      #if defined( __PLATFORM__UNIX )
         /* Detect system locations to enable shared library option by default */
         IF hbmk[ _HBMK_cPLAT ] == "beos"
            lSysLoc := LEFTEQUAL( l_cHB_INSTALL_BIN, "/boot/common"      ) .OR. ;
                       LEFTEQUAL( l_cHB_INSTALL_BIN, "/boot/system"      ) .OR. ;
                       LEFTEQUAL( l_cHB_INSTALL_BIN, "/boot/home/config" ) .OR. ;
                       AScan( ListToArray( GetEnv( "LIBRARY_PATH" ), ":" ), {| tmp | LEFTEQUAL( l_cHB_INSTALL_LIB, tmp ) } ) > 0
         ELSE
            lSysLoc := LEFTEQUAL( l_cHB_INSTALL_BIN, "/usr/local/bin" ) .OR. ;
                       LEFTEQUAL( l_cHB_INSTALL_BIN, "/usr/bin"       ) .OR. ;
                       AScan( ListToArray( GetEnv( "LD_LIBRARY_PATH" ), ":" ), {| tmp | LEFTEQUAL( l_cHB_INSTALL_LIB, tmp ) } ) > 0
         ENDIF
      #else
         lSysLoc := .F.
      #endif
   ELSE
      lSysLoc := .F.

      l_cHB_INSTALL_BIN := ""
      l_cHB_INSTALL_LIB := ""
      l_cHB_INSTALL_INC := ""
      l_cHB_INSTALL_PREFIX := ""
   ENDIF

   aCOMPDET_EMBED := {}

   IF HBMK_ISPLAT( "win|wce|dos|os2|linux" )

      #if defined( __PLATFORM__WINDOWS )

         tmp3 := NIL; HB_SYMBOL_UNUSED( tmp3 )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingw"    + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win"  , "mingw"   , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingw64"  + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win"  , "mingw64" , "i686-w64-mingw32-"   , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingw64"  + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "win"  , "mingw64" , "x86_64-w64-mingw32-" , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingwarm" + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce"  , "mingwarm", "arm-mingw32ce-"      , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingwarm" + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce"  , "mingwarm", "arm-wince-mingw32ce-", NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingwarm" + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc" + hbmk[ _HBMK_cCCEXT ] ), tmp1, NIL ) }, "wce"  , "mingw"   , "i386-mingw32ce-"     , NIL, NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "djgpp"    + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc.exe"                    ), tmp1, NIL ) }, "dos"  , "djgpp"   , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "win"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "dos"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "os2"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binnt" ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "linux", "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "pocc"     + hb_ps() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "pocc.exe"                   ), tmp1, NIL ) }, "win"  , "pocc"    , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "pocc"     + hb_ps() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "pocc.exe"                   ), tmp1, NIL ) }, "win"  , "pocc64"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "pocc"     + hb_ps() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "pocc.exe"                   ), tmp1, NIL ) }, "wce"  , "poccarm" , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )

      #elif defined( __PLATFORM__DOS )

         tmp3 := NIL; HB_SYMBOL_UNUSED( tmp3 )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "djgpp"    + hb_ps() + "bin"   ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "gcc.exe"                    ), tmp1, NIL ) }, "dos"  , "djgpp"   , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "dos"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "win"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "os2"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binw"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "linux", "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )

      #elif defined( __PLATFORM__OS2 )

         tmp3 := NIL; HB_SYMBOL_UNUSED( tmp3 )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "os2"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "win"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "dos"  , "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_ps() + "binp"  ), iif( hb_FileExists( tmp1 + hb_ps() + cPrefix + "wcc386.exe"                 ), tmp1, NIL ) }, "linux", "watcom"  , ""                    , NIL, {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_ps() + ".." ) } } )

      #elif defined( __PLATFORM__UNIX )

         IF Empty( hbmk[ _HBMK_cCCPATH ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCPREFIX ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCPOSTFIX ] )

            DO CASE
            CASE hbmk[ _HBMK_cPLAT ] == "win"
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
      /* If we're just compiling .prg to .c we don't need a C compiler. */
      hbmk[ _HBMK_cCOMP ] := ""
   ELSE
      IF Empty( hbmk[ _HBMK_cCOMP ] ) .OR. hbmk[ _HBMK_cCOMP ] == "bld"
         IF Len( aCOMPSUP ) == 1
            hbmk[ _HBMK_cCOMP ] := aCOMPSUP[ 1 ]
         ELSEIF HBMK_ISPLAT( "darwin|bsd|hpux|sunos|beos|qnx|vxworks|linux|cygwin" ) .OR. ;
                hbmk[ _HBMK_cCOMP ] == "bld"
            hbmk[ _HBMK_cCOMP ] := hb_Version( HB_VERSION_BUILD_COMP )
            IF AScan( aCOMPSUP, { |tmp | tmp == hbmk[ _HBMK_cCOMP ] } ) == 0
               hbmk[ _HBMK_cCOMP ] := NIL
            ENDIF
         ELSE
            IF Empty( hbmk[ _HBMK_cCOMP ] ) .AND. ! Empty( aCOMPDET )
               lDoSupportDetection := Empty( l_cHB_INSTALL_LIB ) .AND. ;
                                      hb_DirExists( PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) ) + "lib" + hb_ps() + hbmk[ _HBMK_cPLAT ] )
               /* Check compilers */
               FOR tmp := 1 TO Len( aCOMPDET )
                  IF ! Empty( cPath_CompC := Eval( aCOMPDET[ tmp ][ _COMPDET_bBlock ] ) )
                     IF ! lDoSupportDetection .OR. ;
                        hb_DirExists( PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) ) + "lib" +;
                                                     hb_ps() + hbmk[ _HBMK_cPLAT ] +;
                                                     hb_ps() + aCOMPDET[ tmp ][ _COMPDET_cCOMP ] +;
                                                     PathSepToSelf( hbmk[ _HBMK_cBUILD ] ) )
                        hbmk[ _HBMK_cCOMP ] := aCOMPDET[ tmp ][ _COMPDET_cCOMP ]
                        IF Len( aCOMPDET[ tmp ] ) >= _COMPDET_cCCPREFIX .AND. aCOMPDET[ tmp ][ _COMPDET_cCCPREFIX ] != NIL
                           hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET[ tmp ][ _COMPDET_cCCPREFIX ]
                        ENDIF
                        IF Len( aCOMPDET[ tmp ] ) >= _COMPDET_cCCPOSTFIX .AND. aCOMPDET[ tmp ][ _COMPDET_cCCPOSTFIX ] != NIL
                           hbmk[ _HBMK_cCCPOSTFIX ] := aCOMPDET[ tmp ][ _COMPDET_cCCPOSTFIX ]
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
                           hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected platform: %1$s (adjusted)" ), hbmk[ _HBMK_cPLAT ] ) )
                        ENDIF
                        EXIT
                     ELSE
                        IF hbmk[ _HBMK_lInfo ]
                           hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected C compiler '%1$s' skipped because required Harbour core libraries are not found." ), aCOMPDET[ tmp ][ _COMPDET_cCOMP ] ) )
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
                  IF ISBLOCK( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ] )
                     Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ], hbmk[ _HBMK_cPLAT ], hbmk[ _HBMK_cCOMP ], cPath_CompC )
                  ENDIF
                  EXIT
               ENDIF
            NEXT
         ENDIF
         IF ! Empty( hbmk[ _HBMK_cCOMP ] )
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected C compiler: %1$s" ), hbmk[ _HBMK_cCOMP ] ) )
            ENDIF
         ELSE
            IF Empty( aCOMPDET )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Please choose a C compiler by using -compiler= option.\nYou have the following choices on your platform: %1$s" ), ArrayToList( aCOMPSUP, ", " ) ) )
            ELSE
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Could not detect any supported C compiler in your PATH.\nPlease setup one or set -compiler= option to one of these values: %1$s" ), ArrayToList( aCOMPSUP, ", " ) ) )
            ENDIF
            RETURN _ERRLEV_UNKNCOMP
         ENDIF
      ELSE
         IF AScan( aCOMPSUP, {| tmp | tmp == hbmk[ _HBMK_cCOMP ] } ) == 0
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Compiler value unknown: %1$s" ), hbmk[ _HBMK_cCOMP ] ) )
            RETURN _ERRLEV_UNKNCOMP
         ENDIF
         /* Detect cross platform CCPREFIX and CCPATH if embedded installation is detected */
         FOR tmp := 1 TO Len( aCOMPDET_EMBED )
            IF aCOMPDET_EMBED[ tmp ][ _COMPDETE_cPLAT ] == hbmk[ _HBMK_cPLAT ] .AND. ;
               aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCOMP ] == hbmk[ _HBMK_cCOMP ] .AND. ;
               ! Empty( cPath_CompC := Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bBlock ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPATH ] ) )
               hbmk[ _HBMK_cCCPATH ] := cPath_CompC
               hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ]
               IF ISBLOCK( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ] )
                  Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ], hbmk[ _HBMK_cPLAT ], hbmk[ _HBMK_cCOMP ], cPath_CompC )
               ENDIF
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   hbmk[ _HBMK_aINCPATH ] := {}
   hbmk[ _HBMK_aLIBPATH ] := {}

   IF Empty( hbmk[ _HBMK_cCPU ] )
      hbmk[ _HBMK_cCPU ] := hbmk_CPU( hbmk )
   ENDIF

   /* Tweaks to compiler/platform environments */

   IF hbmk[ _HBMK_cCOMP ] == "bcc"
      /* NOTE: Hack to tweak bcc setup by hbmk2 to include one additional
               compiler lib dir to lib search path. */
      IF Empty( cPath_CompC )
         cPath_CompC := _BCC_BIN_DETECT()
      ENDIF
      IF ! Empty( cPath_CompC )
         /* NOTE: Automatically configure bcc installation with missing configuration. [vszakats]
                  Permanently enabled. Apparently this is still top problem for bcc users. It's
                  also in sync this way with Harbour core build system. */
         IF .T. .OR. ;
            ! hb_FileExists( FNameDirGet( cPath_CompC ) + ".." + hb_ps() + "Bin" + hb_ps() + "bcc32.cfg" ) .OR. ;
            ! hb_FileExists( FNameDirGet( cPath_CompC ) + ".." + hb_ps() + "Bin" + hb_ps() + "ilink32.cfg" )
            /* NOTE: BCC 5.8 has different casing: 'include', 'lib', 'psdk' respectively. */
            AAdd( hbmk[ _HBMK_aINCPATH ], PathNormalize( FNameDirGet( cPath_CompC ) + ".." + hb_ps() + "Include" ) )
            AAdd( hbmk[ _HBMK_aLIBPATH ], PathNormalize( FNameDirGet( cPath_CompC ) + ".." + hb_ps() + "Lib" ) )
            /* NOTE: BCC 5.8 (and upper ?) thing */
            tmp := PathNormalize( FNameDirGet( cPath_CompC ) + ".." + hb_ps() + "Include" + hb_ps() + "dinkumware" )
            IF hb_DirExists( tmp )
               AAdd( hbmk[ _HBMK_aINCPATH ], tmp )
            ENDIF
         ENDIF
         AAdd( hbmk[ _HBMK_aLIBPATH ], PathNormalize( FNameDirGet( cPath_CompC ) + ".." + hb_ps() + "Lib" + hb_ps() + "PSDK" ) )
      ENDIF
   ENDIF

   DO CASE
   CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
      AAdd( hbmk[ _HBMK_aINCPATH ], PathSepToSelf( GetEnv( "WIND_BASE" ) + "/target/usr/h" ) )
      AAdd( hbmk[ _HBMK_aINCPATH ], PathSepToSelf( GetEnv( "WIND_BASE" ) + "/target/usr/h/wrn/coreip" ) )
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
   ENDCASE

   /* Tweaks to compiler setup */

   IF hbmk[ _HBMK_cCOMP ] == "djgpp"
      hbmk[ _HBMK_cDynLibExt ] := ".dxe"
   ENDIF

   /* Detect compiler version (where applicable) */

   IF hbmk[ _HBMK_nCOMPVer ] == 0 .AND. ! Empty( cPath_CompC )

      DO CASE
      CASE ( hbmk[ _HBMK_cPLAT ] == "cygwin" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" )

         IF File( FNameDirGet( cPath_CompC ) + "i686-pc-cygwin-gcc-3.4" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 34
         ENDIF

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw64" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingwarm" )

         DO CASE
         CASE File( FNameDirGet( cPath_CompC ) + "mingw32-gcc-4.5" + hb_osFileMask() ) .OR. ;
              File( FNameDirGet( cPath_CompC ) + "i686-w64-mingw32-gcc-4.5" + hb_osFileMask() ) .OR. ;
              File( FNameDirGet( cPath_CompC ) + "x86_64-pc-mingw32-gcc-4.5" + hb_osFileMask() ) .OR. ;
              File( FNameDirGet( cPath_CompC ) + "x86_64-w64-mingw32-gcc-4.5" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 45
         CASE File( FNameDirGet( cPath_CompC ) + "mingw32-gcc-4.4" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 44
         CASE File( FNameDirGet( cPath_CompC ) + "mingw32-gcc-4.3" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 43
         CASE File( FNameDirGet( cPath_CompC ) + "mingw32-gcc-3.4" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 34
         CASE File( FNameDirGet( cPath_CompC ) + "x86_64-w64-mingw32-gcc-4.6" + hb_osFileMask() )
            hbmk[ _HBMK_nCOMPVer ] := 46
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
            OTHERWISE
               hbmk[ _HBMK_nCOMPVer ] := 1400
            ENDCASE
         ENDIF
      ENDCASE
   ENDIF

   /* Finish detecting bin/lib/include dirs */

   IF Empty( l_cHB_INSTALL_BIN )
      /* Autodetect multi-compiler/platform bin structure (also .dlls are in bin dir on non-*nix platforms) */
      IF hb_DirExists( tmp := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) ) + "bin" +;
                                             hb_ps() + hbmk[ _HBMK_cPLAT ] +;
                                             hb_ps() + hbmk[ _HBMK_cCOMP ] +;
                                             PathSepToSelf( hbmk[ _HBMK_cBUILD ] ) )
         l_cHB_INSTALL_BIN := tmp
      ELSE
         l_cHB_INSTALL_BIN := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "bin" )
      ENDIF
   ENDIF
   IF Empty( l_cHB_INSTALL_LIB )
      /* Autodetect multi-compiler/platform lib structure */
      IF hb_DirExists( tmp := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) ) + "lib" +;
                                             hb_ps() + hbmk[ _HBMK_cPLAT ] +;
                                             hb_ps() + hbmk[ _HBMK_cCOMP ] +;
                                             PathSepToSelf( hbmk[ _HBMK_cBUILD ] ) )
         l_cHB_INSTALL_LIB := tmp
      ELSE
         l_cHB_INSTALL_LIB := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "lib" )
      ENDIF
   ENDIF
   IF Empty( l_cHB_INSTALL_INC )
      l_cHB_INSTALL_INC := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "include" )
   ENDIF

   IF l_cHB_INSTALL_DYN == NIL
      IF HBMK_ISPLAT( "win|wce|os2|dos|cygwin" )
         l_cHB_INSTALL_DYN := l_cHB_INSTALL_BIN
      ELSE
         l_cHB_INSTALL_DYN := l_cHB_INSTALL_LIB
      ENDIF
   ENDIF

   IF hbmk[ _HBMK_lInfo ]
      hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using Harbour: %1$s %2$s %3$s %4$s" ), l_cHB_INSTALL_BIN, l_cHB_INSTALL_INC, l_cHB_INSTALL_LIB, l_cHB_INSTALL_DYN ) )
      IF ! Empty( cPath_CompC )
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using C compiler: %1$s" ), cPath_CompC ) )
      ENDIF
   ENDIF

   /* Make a copy to hbmk structure so that we can use it in deeper
      functions. The only reason I kept the local version is to
      keep above code parts easier to read. [vszakats] */
   hbmk[ _HBMK_cHB_INSTALL_BIN ] := l_cHB_INSTALL_BIN := DirDelPathSep( PathSepToSelf( l_cHB_INSTALL_BIN ) )
   hbmk[ _HBMK_cHB_INSTALL_LIB ] := l_cHB_INSTALL_LIB := DirDelPathSep( PathSepToSelf( l_cHB_INSTALL_LIB ) )
   hbmk[ _HBMK_cHB_INSTALL_DYN ] := l_cHB_INSTALL_DYN := DirDelPathSep( PathSepToSelf( l_cHB_INSTALL_DYN ) )
   hbmk[ _HBMK_cHB_INSTALL_INC ] := l_cHB_INSTALL_INC := DirDelPathSep( PathSepToSelf( l_cHB_INSTALL_INC ) )

   /* Add main Harbour library dir to lib path list */
   AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], l_cHB_INSTALL_LIB )
   IF ! Empty( l_cHB_INSTALL_DYN ) .AND. !( l_cHB_INSTALL_DYN == l_cHB_INSTALL_LIB )
      AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], l_cHB_INSTALL_DYN )
   ENDIF

   /* Add main Harbour header dir to header path list */
   AAddNotEmpty( hbmk[ _HBMK_aINCPATH ], l_cHB_INSTALL_INC )

   /* Add default search paths for .hbc files */
   l_cHB_INSTALL_ADD := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) )
   AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_INSTALL_ADD + "contrib" + hb_ps() + "%{hb_name}" )
   AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_INSTALL_ADD + "addons" + hb_ps() + "%{hb_name}" )
#if defined( __PLATFORM__UNIX )
   IF hb_DirExists( "/opt/harbour" )
      AAdd( hbmk[ _HBMK_aLIBPATH ], "/opt/harbour/contrib/%{hb_name}" )
      AAdd( hbmk[ _HBMK_aLIBPATH ], "/opt/harbour/addons/%{hb_name}" )
   ENDIF
#endif

   /* Build with shared libs by default, if we're installed to default system locations. */

   IF lSysLoc .AND. HBMK_ISPLAT( "darwin|bsd|hpux|sunos|beos|qnx|vxworks|linux|cygwin" )
      hbmk[ _HBMK_lSHARED ] := .T.
      hbmk[ _HBMK_lSTATICFULL ] := .F.
   ELSE
      hbmk[ _HBMK_lSHARED ] := .F.
      hbmk[ _HBMK_lSTATICFULL ] := .F.
   ENDIF

   /* Process command line */

   hbmk[ _HBMK_aPRG ] := {}
   hbmk[ _HBMK_aC ] := {}
   hbmk[ _HBMK_aCPP ] := {}
   hbmk[ _HBMK_hDEPTS ] := { => }
   hbmk[ _HBMK_aOPTPRG ] := {}
   hbmk[ _HBMK_aOPTC ] := {}
   hbmk[ _HBMK_aOPTRES ] := {}
   hbmk[ _HBMK_aOPTL ] := {}
   hbmk[ _HBMK_aOPTA ] := {}
   hbmk[ _HBMK_aOPTD ] := {}
   hbmk[ _HBMK_aOPTI ] := {}
   l_aOPTRUN := {}
   hbmk[ _HBMK_aRESSRC ] := {}
   hbmk[ _HBMK_aRESCMP ] := {}
   hbmk[ _HBMK_aLIBUSER ] := {}
   hbmk[ _HBMK_aLIBUSERGT ] := {}
   hbmk[ _HBMK_aLIBUSERSYS ] := {}
   hbmk[ _HBMK_aLIBUSERSYSPRE ] := {}
   hbmk[ _HBMK_aOBJUSER ] := {}
   hbmk[ _HBMK_aICON ] := {}
   hbmk[ _HBMK_aIMPLIBSRC ] := {}
   hbmk[ _HBMK_aDEF ] := {}
   hbmk[ _HBMK_aINSTFILE ] := {}
   l_aOBJA := {}
   hbmk[ _HBMK_cPROGDIR ] := NIL
   hbmk[ _HBMK_cPROGNAME ] := NIL
   l_cLIBSELF := NIL
   l_cIMPLIBDIR := NIL
   l_cIMPLIBNAME := NIL
   hbmk[ _HBMK_cFIRST ] := NIL
   hbmk[ _HBMK_aPO ] := {}
   hbmk[ _HBMK_cHBL ] := NIL
   hbmk[ _HBMK_cHBLDir ] := ""
   hbmk[ _HBMK_cPO ] := NIL
   hbmk[ _HBMK_aLNG ] := {}
   hbmk[ _HBMK_aINSTPATH ] := {}
   hbmk[ _HBMK_lUNICODE ] := ( hbmk[ _HBMK_cPLAT ] == "wce" )
   hbmk[ _HBMK_cHBX ] := NIL

   aParams := {}

   /* Process build-time configuration */

   #if defined( HB_HAS_GPM )
      IF hbmk[ _HBMK_cPLAT ] == "linux"
         AAdd( hbmk[ _HBMK_aLIBUSERSYS ], "gpm" )
      ENDIF
   #endif

   #if defined( HB_HAS_WATT )
      IF hbmk[ _HBMK_cPLAT ] == "dos"
         SWITCH hbmk[ _HBMK_cCOMP ]
         CASE "djgpp"  ; AAdd( hbmk[ _HBMK_aLIBUSERSYS ], "watt" ) ; EXIT
         CASE "watcom" ; AAdd( hbmk[ _HBMK_aLIBUSERSYS ], "wattcpwf" ) ; EXIT
         ENDSWITCH
         AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToSelf( GetEnv( "WATT_ROOT" ) ) + hb_ps() + "lib" )
      ENDIF
   #endif

   /* Process automatic make files in current dir. */
   IF hbmk[ _HBMK_lAutoHBM ] .AND. hb_FileExists( _HBMK_AUTOHBM_NAME )
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing local make script: %1$s" ), _HBMK_AUTOHBM_NAME ) )
      ENDIF
      HBM_Load( hbmk, aParams, _HBMK_AUTOHBM_NAME, 1, .F. ) /* Do not allow sub-projects in automatic make file */
   ENDIF

   /* Collect all command line parameters */
   FOR EACH cParam IN aArgs
      DO CASE
      CASE !( Left( cParam, 1 ) == "-" ) .AND. Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" .AND. ;
         !( Lower( FNameExtGet( cParam ) ) == ".clp" )
         cParam := SubStr( cParam, 2 )
         IF Empty( FNameExtGet( cParam ) )
            cParam := FNameExtSet( cParam, ".hbm" )
         ENDIF
         IF !( Lower( FNameExtGet( cParam ) ) == ".hbm" ) .AND. lAcceptLDClipper
            rtlnk_process( hbmk, MemoRead( PathSepToSelf( cParam ) ), @hbmk[ _HBMK_cPROGNAME ], @hbmk[ _HBMK_aOBJUSER ], @hbmk[ _HBMK_aLIBUSER ] )
            IF ! Empty( hbmk[ _HBMK_aOBJUSER ] )
               DEFAULT hbmk[ _HBMK_cFIRST ] TO hbmk[ _HBMK_aOBJUSER ][ 1 ]
            ENDIF
         ELSE
            tmp := HBM_Load( hbmk, aParams, PathSepToSelf( cParam ), 1, .T. ) /* Load parameters from script file */
            IF tmp != _ERRLEV_OK .AND. ;
               tmp != _ERRLEV_STOP
               RETURN tmp
            ENDIF
         ENDIF
      CASE !( Left( cParam, 1 ) == "-" ) .AND. ;
           ( Lower( FNameExtGet( cParam ) ) == ".hbm" .OR. ;
             Lower( FNameExtGet( cParam ) ) == ".hbp" )
         tmp := HBM_Load( hbmk, aParams, PathSepToSelf( cParam ), 1, .T. ) /* Load parameters from script file */
         IF tmp != _ERRLEV_OK .AND. ;
            tmp != _ERRLEV_STOP
            RETURN tmp
         ENDIF
      OTHERWISE
         AAdd( aParams, { cParam, "", 0 } )
      ENDCASE
   NEXT

   /* Process automatic control files. */
   HBC_ProcessAll( hbmk )

   /* Process command line (2nd pass) */
   FOR EACH aParam IN aParams

      cParam := ArchCompFilter( hbmk, aParam[ _PAR_cParam ] )
      cParamL := Lower( cParam )

      DO CASE
      CASE Empty( cParam )
         /* do nothing */
      CASE Left( cParamL, 6 )  == "-comp=" .OR. ;
           Left( cParamL, 10 ) == "-compiler=" .OR. ;
           Left( cParamL, 6 )  == "-plat=" .OR. ;
           Left( cParamL, 10 ) == "-platform=" .OR. ;
           Left( cParamL, 6 )  == "-arch=" .OR. ; /* Compatibility */
           Left( cParamL, 5 )  == "-cpu=" .OR. ;
           Left( cParamL, 7 )  == "-build=" .OR. ;
           Left( cParamL, 6 )  == "-lang=" .OR. ;
           Left( cParamL, 4 )  == "-shl" .OR. ;
           Left( cParamL, 7 )  == "-width=" .OR. ;
           Left( cParamL, 5 )  == "-env:" .OR. ;
           cParamL             == "-autohbm" .OR. ;
           cParamL             == "-autohbm-" .OR. ;
           cParamL             == "-hbrun" .OR. ;
           cParamL             == "-hbraw" .OR. ;
           cParamL             == "-hbcmp" .OR. ;
           cParamL             == "-hbcc"  .OR. ;
           cParamL             == "-hblnk" .OR. ;
           cParamL             == "-nohbc" .OR. ; /* Ignore it for compatibility */
           cParamL             == "-xhb" .OR. ;
           cParamL             == "-hb10" .OR. ;
           cParamL             == "-hb20" .OR. ;
           cParamL             == "-hbc" .OR. ;
           cParamL             == "-clipper" .OR. ;
           cParamL             == "-rtlink" .OR. ;
           cParamL             == "-blinker" .OR. ;
           cParamL             == "-exospace"

         /* Simply ignore. They were already processed in the first pass. */

      CASE cParamL == "-quiet"           ; hbmk[ _HBMK_lQuiet ] := .T. ; hbmk[ _HBMK_lInfo ] := .F.
      CASE cParamL == "-quiet-"          ; hbmk[ _HBMK_lQuiet ] := .F.
      CASE cParamL == "-info"            ; hbmk[ _HBMK_lInfo ] := .T.
      CASE cParamL == "-pause"           ; lPause := .T.
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

      CASE cParamL == "-gui" .OR. ;
           cParamL == "-mwindows"        ; hbmk[ _HBMK_lGUI ]       := .T. /* Compatibility */
      CASE cParamL == "-std" .OR. ;
           cParamL == "-mconsole"        ; hbmk[ _HBMK_lGUI ]       := .F. /* Compatibility */
      CASE cParamL == "-mt"              ; hbmk[ _HBMK_lMT ]        := .T.
      CASE cParamL == "-st"              ; hbmk[ _HBMK_lMT ]        := .F.
      CASE cParamL == "-shared"          ; hbmk[ _HBMK_lSHARED ]    := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL
      CASE cParamL == "-fullshared"      ; hbmk[ _HBMK_lSHARED ]    := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := .T.
      CASE cParamL == "-fixshared"       ; hbmk[ _HBMK_lSHARED ]    := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := .F.
      CASE cParamL == "-static"          ; hbmk[ _HBMK_lSHARED ]    := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL
      CASE cParamL == "-fullstatic"      ; hbmk[ _HBMK_lSHARED ]    := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .T. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL
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
      CASE cParamL == "-debug-" .OR. ;
           cParamL == "-nodebug"         ; hbmk[ _HBMK_lDEBUG ]       := .F.
      CASE cParamL == "-optim"           ; hbmk[ _HBMK_lOPTIM ]       := .T.
      CASE cParamL == "-optim-" .OR. ;
           cParamL == "-noopt"           ; hbmk[ _HBMK_lOPTIM ]       := .F.
      CASE cParamL == "-debugtime"       ; hbmk[ _HBMK_lDEBUGTIME ]   := .T.
      CASE cParamL == "-debuginc"        ; hbmk[ _HBMK_lDEBUGINC ]    := .T.
      CASE cParamL == "-debugstub"       ; hbmk[ _HBMK_lDEBUGSTUB ]   := .T.
      CASE cParamL == "-debugi18n"       ; hbmk[ _HBMK_lDEBUGI18N ]   := .T.
      CASE cParamL == "-debugdepd"       ; hbmk[ _HBMK_lDEBUGDEPD ]   := .T.
      CASE cParamL == "-debugpars"       ; hbmk[ _HBMK_lDEBUGPARS ]   := .T.
      CASE cParamL == "-nulrdd"          ; hbmk[ _HBMK_lNULRDD ]      := .T.
      CASE cParamL == "-nulrdd-"         ; hbmk[ _HBMK_lNULRDD ]      := .F.
      CASE cParamL == "-map"             ; hbmk[ _HBMK_lMAP ]         := .T.
      CASE cParamL == "-map-" .OR. ;
           cParamL == "-nomap"           ; hbmk[ _HBMK_lMAP ]         := .F.
      CASE cParamL == "-implib"          ; hbmk[ _HBMK_lIMPLIB ]      := .T.
      CASE cParamL == "-implib-" .OR. ;
           cParamL == "-noimplib"        ; hbmk[ _HBMK_lIMPLIB ]      := .F.
      CASE cParamL == "-beep"            ; hbmk[ _HBMK_lBEEP ]        := .T.
      CASE cParamL == "-beep-" .OR. ;
           cParamL == "-nobeep"          ; hbmk[ _HBMK_lBEEP ]        := .F.
      CASE cParamL == "-rebuild"

         hbmk[ _HBMK_lINC ] := .T.
         IF nLevel == 1
            hbmk[ _HBMK_lREBUILD ] := .T.
         ENDIF

      CASE cParamL == "-rebuildall"

         hbmk[ _HBMK_lINC ] := .T.
         hbmk[ _HBMK_lREBUILD ] := .T.

      CASE cParamL == "-rebuildpo"       ; hbmk[ _HBMK_lREBUILDPO ]   := .T.
      CASE cParamL == "-minipo"          ; hbmk[ _HBMK_lMINIPO ]      := .T.
      CASE cParamL == "-minipo-" .OR. ;
           cParamL == "-nominipo"        ; hbmk[ _HBMK_lMINIPO ]      := .F.
      CASE cParamL == "-clean"           ; hbmk[ _HBMK_lINC ]         := .T. ; hbmk[ _HBMK_lCLEAN ] := .T.
      CASE cParamL == "-inc"             ; hbmk[ _HBMK_lINC ]         := .T.
      CASE cParamL == "-inc-" .OR. ;
           cParamL == "-noinc"           ; hbmk[ _HBMK_lINC ]         := .F.
      CASE cParamL == "-ignore"          ; hbmk[ _HBMK_lIGNOREERROR ] := .T.
      CASE cParamL == "-ignore-" .OR. ;
           cParamL == "-noignore"        ; hbmk[ _HBMK_lIGNOREERROR ] := .F.
      CASE cParamL == "-hbcppmm"         ; hbmk[ _HBMK_lHBCPPMM ]     := .T.
      CASE cParamL == "-hbcppmm-" .OR. ;
           cParamL == "-nohbcppmm"       ; hbmk[ _HBMK_lHBCPPMM ]     := .F.
      CASE cParamL == "-strip"           ; hbmk[ _HBMK_lSTRIP ]       := .T.
      CASE cParamL == "-strip-" .OR. ;
           cParamL == "-nostrip"         ; hbmk[ _HBMK_lSTRIP ]       := .F.
      CASE cParamL == "-depimplib"       ; hbmk[ _HBMK_lDEPIMPLIB ]   := .T.
      CASE cParamL == "-depimplib-"      ; hbmk[ _HBMK_lDEPIMPLIB ]   := .F.
      CASE cParamL == "-instforce"       ; hbmk[ _HBMK_lInstForce ]   := .T.
      CASE cParamL == "-instforce-"      ; hbmk[ _HBMK_lInstForce ]   := .F.

      CASE cParamL == "--harbourhelp"    ; AAdd( hbmk[ _HBMK_aOPTPRG ], "--help" ) ; lHarbourInfo := .T.
      CASE cParamL == "-harbourhelp"     ; AAdd( hbmk[ _HBMK_aOPTPRG ], "--help" ) ; lHarbourInfo := .T.
      CASE cParamL == "-build"           ; AAdd( hbmk[ _HBMK_aOPTPRG ], "-build" ) ; lHarbourInfo := .T.

      CASE cParamL == "-warn" .OR. ;
           Left( cParamL, 6 ) == "-warn="

         DO CASE
         CASE SubStr( cParamL, 7 ) == "def" ; hbmk[ _HBMK_nWARN ] := _WARN_DEF
         CASE SubStr( cParamL, 7 ) == "no"  ; hbmk[ _HBMK_nWARN ] := _WARN_NO
         CASE SubStr( cParamL, 7 ) == "low" ; hbmk[ _HBMK_nWARN ] := _WARN_LOW
         CASE SubStr( cParamL, 7 ) == "max" ; hbmk[ _HBMK_nWARN ] := _WARN_MAX
         OTHERWISE                          ; hbmk[ _HBMK_nWARN ] := _WARN_YES
         ENDCASE

      CASE cParamL == "-warn-" .OR. ;
           cParamL == "-nowarn"               ; hbmk[ _HBMK_nWARN ] := _WARN_NO

      CASE cParamL == "-compr" .OR. ;
           Left( cParamL, 7 ) == "-compr="

         DO CASE
         CASE SubStr( cParamL, 8 ) == "min" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MIN
         CASE SubStr( cParamL, 8 ) == "max" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MAX
         OTHERWISE                          ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
         ENDCASE

      CASE cParamL == "-compr-" .OR. ;
           cParamL == "-nocompr"              ; hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF

      CASE cParamL == "-head" .OR. ;
           Left( cParamL, 6 ) == "-head="

         DO CASE
         CASE SubStr( cParamL, 7 ) == "off"    ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF
         CASE SubStr( cParamL, 7 ) == "full"   ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
         CASE SubStr( cParamL, 7 ) == "native" ; hbmk[ _HBMK_nHEAD ] := _HEAD_NATIVE
         OTHERWISE                             ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
         ENDCASE

      CASE cParamL == "-head-" .OR. ;
           cParamL == "-nohead"               ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF

      CASE Left( cParamL, 5 ) == "-cpp="

         DO CASE
         CASE SubStr( cParamL, 6 ) == "def" ; hbmk[ _HBMK_lCPP ] := NIL
         CASE SubStr( cParamL, 6 ) == "yes" ; hbmk[ _HBMK_lCPP ] := .T.
         CASE SubStr( cParamL, 6 ) == "no"  ; hbmk[ _HBMK_lCPP ] := .F.
         ENDCASE

      CASE cParamL == "-cpp"             ; hbmk[ _HBMK_lCPP ]       := .T.
      CASE cParamL == "-cpp-" .OR. ;
           cParamL == "-nocpp"           ; hbmk[ _HBMK_lCPP ]       := .F.

      CASE cParamL == "-run"

         IF hbmk[ _HBMK_nLevel ] == 1
            hbmk[ _HBMK_lRUN ] := .T.
         ENDIF

      CASE cParamL == "-run-" .OR. ;
           cParamL == "-norun"           ; hbmk[ _HBMK_lRUN ]       := .F.
      CASE cParamL == "-trace"           ; hbmk[ _HBMK_lTRACE ]     := .T.
      CASE cParamL == "-trace-" .OR. ;
           cParamL == "-notrace"         ; hbmk[ _HBMK_lTRACE ]     := .F.
      CASE cParamL == "-traceonly"       ; hbmk[ _HBMK_lTRACE ]     := .T. ; hbmk[ _HBMK_lDONTEXEC ] := .T.

      CASE cParamL == "--hbdirbin"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( l_cHB_INSTALL_BIN )

      CASE cParamL == "--hbdirdyn"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( l_cHB_INSTALL_DYN )

      CASE cParamL == "--hbdirlib"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( l_cHB_INSTALL_LIB )

      CASE cParamL == "--hbdirinc"       ; hbmk[ _HBMK_lStopAfterInit ] := .T.

         OutStd( l_cHB_INSTALL_INC )

      CASE cParamL == "--hbinfo"

         lDumpInfo := .T.

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
         ENDIF

      CASE Left( cParamL, 5 ) == "-hbl="

         hbmk[ _HBMK_cHBL ] := PathSepToSelf( SubStr( cParam, 6 ) )
         hbmk[ _HBMK_cHBLDir ] := FNameDirGet( aParam[ _PAR_cFileName ] )

      CASE Left( cParamL, 4 ) == "-po="

         hbmk[ _HBMK_cPO ] := PathMakeAbsolute( PathSepToSelf( SubStr( cParam, 5 ) ), FNameDirGet( aParam[ _PAR_cFileName ] ) )

      CASE Left( cParamL, 4 ) == "-hbl"

         hbmk[ _HBMK_cHBL ] := ""
         hbmk[ _HBMK_cHBLDir ] := ""

      CASE Left( cParamL, 5 ) == "-hbx="

         cParam := MacroProc( hbmk, SubStr( cParam, 6 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            hbmk[ _HBMK_cHBX ] := PathMakeAbsolute( PathSepToSelf( cParam ), FNameDirGet( aParam[ _PAR_cFileName ] ) )
         ENDIF

      CASE Left( cParamL, 6 ) == "-main="

         IF IsValidHarbourID( cParam := SubStr( cParam, 7 ) )
            l_cMAIN := "@" + cParam
         ELSE
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -main value ignored: %1$s" ), cParam ) )
         ENDIF

      CASE Left( cParamL, 3 ) == "-gt"

         cParam := MacroProc( hbmk, SubStr( cParam, 2 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            IF hbmk[ _HBMK_cGT ] == NIL
               IF ! SetupForGT( cParam, @hbmk[ _HBMK_cGT ], @hbmk[ _HBMK_lGUI ] )
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -gt value ignored: %1$s" ), cParam ) )
                  cParam := NIL
               ENDIF
            ENDIF
            IF !( Lower( cParam ) == "gtnul" )
               IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == Lower( cParam ) } ) == 0 .AND. ;
                  AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == Lower( cParam ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], PathSepToSelf( cParam ) )
               ENDIF
            ENDIF
         ENDIF

#if ! defined( __PLATFORM__UNIX )
      CASE Left( cParamL, 2 ) == "/o" .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]

         /* Swallow this switch. We don't pass it to Harbour, as it may badly
            interact with hbmk. */
#endif

      CASE Left( cParam, 2 ) == "-o"

         tmp := SubStr( cParam, 3 )

         IF hbmk[ _HBMK_lStopAfterHarbour ]
            tmp := MacroProc( hbmk, tmp, aParam[ _PAR_cFileName ] )
            IF ! Empty( tmp )
               AAddNotEmpty( hbmk[ _HBMK_aOPTPRG ], "-o" + PathNormalize( PathMakeAbsolute( PathSepToSelf( tmp ), aParam[ _PAR_cFileName ] ) ) )
            ENDIF
         ELSE
            IF ! Empty( tmp )
               tmp := MacroProc( hbmk, tmp, aParam[ _PAR_cFileName ] )
               IF ! Empty( tmp )
                  tmp := PathSepToSelf( tmp )
                  hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                  DO CASE
                  CASE Empty( cDir )
                     tmp := PathNormalize( PathMakeAbsolute( tmp, aParam[ _PAR_cFileName ] ) )
                     hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                     IF hbmk[ _HBMK_cPROGDIR ] == NIL
                        hbmk[ _HBMK_cPROGDIR ] := cDir
                     ENDIF
                     hbmk[ _HBMK_cPROGNAME ] := FNameNameExtGet( tmp )
                  CASE ! Empty( cDir ) .AND. Empty( cName ) .AND. Empty( cExt )
                     hbmk[ _HBMK_cPROGDIR ] := PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
                  OTHERWISE /* ! Empty( cDir ) .AND. !( Empty( cName ) .AND. Empty( cExt ) ) */
                     hbmk[ _HBMK_cPROGDIR ] := PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
                     hbmk[ _HBMK_cPROGNAME ] := FNameNameExtGet( tmp )
                  ENDCASE
               ENDIF
            ELSE
               hbmk[ _HBMK_cPROGDIR ] := NIL
               hbmk[ _HBMK_cPROGNAME ] := NIL
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-implib=" ) ) == "-implib="

         hbmk[ _HBMK_lIMPLIB ] := .T.

         tmp := SubStr( cParam, Len( "-implib=" ) + 1 )

         IF ! Empty( tmp )
            tmp := MacroProc( hbmk, tmp, aParam[ _PAR_cFileName ] )
            IF ! Empty( tmp )
               tmp := PathSepToSelf( tmp )
               hb_FNameSplit( tmp, @cDir, @cName, @cExt )
               DO CASE
               CASE Empty( cDir )
                  tmp := PathNormalize( PathMakeAbsolute( tmp, aParam[ _PAR_cFileName ] ) )
                  hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                  IF l_cIMPLIBDIR == NIL
                     l_cIMPLIBDIR := cDir
                  ENDIF
                  l_cIMPLIBNAME := FNameNameExtGet( tmp )
               CASE ! Empty( cDir ) .AND. Empty( cName ) .AND. Empty( cExt )
                  l_cIMPLIBDIR := PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
               OTHERWISE /* ! Empty( cDir ) .AND. !( Empty( cName ) .AND. Empty( cExt ) ) */
                  l_cIMPLIBDIR := PathNormalize( PathMakeAbsolute( cDir, aParam[ _PAR_cFileName ] ) )
                  l_cIMPLIBNAME := FNameNameExtGet( tmp )
               ENDCASE
            ENDIF
         ELSE
            l_cIMPLIBDIR := NIL
            l_cIMPLIBNAME := NIL
         ENDIF

      CASE Left( cParamL, Len( "-ln=" ) ) == "-ln="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-ln=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAddNewNotEmpty( hbmk[ _HBMK_aLINK ], PathSepToSelf( cParam ) )
         ENDIF

      CASE Left( cParam, 2 ) == "-L" .AND. ;
           Len( cParam ) > 2

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aLIBPATH ], DirDelPathSep( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE Left( cParamL, Len( "-instfile=" ) ) == "-instfile="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-instfile=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF inst_split_arg( cParam, @tmp, @cParam )
            FOR EACH cParam IN FN_Expand( PathMakeAbsolute( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { tmp, cParam } )
            NEXT
         ENDIF

      CASE Left( cParamL, Len( "-instpath=" ) ) == "-instpath=" .AND. ;
           Len( cParamL ) > Len( "-instpath=" )

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-instpath=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF inst_split_arg( cParam, @tmp, @cParam )
            AAddNewINST( hbmk[ _HBMK_aINSTPATH ], { tmp, PathNormalize( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ) ) } )
         ENDIF

      CASE Left( cParamL, Len( "-incpath=" ) ) == "-incpath=" .AND. ;
           Len( cParamL ) > Len( "-incpath=" )

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-incpath=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAddNew( hbmk[ _HBMK_aINCPATH ], DirDelPathSep( PathNormalize( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ) ) ) )
         ENDIF

      CASE Left( cParamL, Len( "-icon=" ) ) == "-icon="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-icon=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aICON ], PathNormalize( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE Left( cParamL, Len( "-iflag=" ) ) == "-iflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-iflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTI ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, 2 ) == "-i" .AND. ;
           Len( cParamL ) > 2 .AND. !( cParamL == "-i-" )

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAddNew( hbmk[ _HBMK_aINCPATH ], DirDelPathSep( PathNormalize( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ) ) ) )
         ENDIF

      CASE Left( cParamL, Len( "-stop" ) ) == "-stop"

         hbmk[ _HBMK_lStopAfterInit ] := .T.
         hbmk[ _HBMK_lRUN ] := .F.
         hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_STOP

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
               AAddNewNotEmpty( hbmk[ _HBMK_aOPTPRG ], PathSepToSelf( cParam, 2 ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-cflag=" ) ) == "-cflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-cflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTC ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-resflag=" ) ) == "-resflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-resflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTRES ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-ldflag=" ) ) == "-ldflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-ldflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTL ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-dflag=" ) ) == "-dflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-dflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTD ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-aflag=" ) ) == "-aflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-aflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTA ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-runflag=" ) ) == "-runflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-runflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( l_aOPTRUN, cParam )
         ENDIF

      CASE Left( cParamL, Len( "-pflag=" ) ) == "-pflag="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-pflag=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aPLUGINPars ], PathSepToSelf( cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-pi=" ) ) == "-pi="

         cParam := PathSepToSelf( MacroProc( hbmk, SubStr( cParam, Len( "-pi=" ) + 1 ), aParam[ _PAR_cFileName ] ) )
         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPLUGINPars ], cParam )
         NEXT

      CASE Left( cParamL, Len( "-3rd=" ) ) == "-3rd="

         /* Silently ignore these. These options can be used to store options
            processed by other tools allowing them to keep additional information
            in hbmk2 script files. */

      CASE Left( cParamL, Len( "-workdir=" ) ) == "-workdir="

         hbmk[ _HBMK_cWorkDir ] := PathNormalize( PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, SubStr( cParam, Len( "-workdir=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] ) )

      CASE Left( cParamL, Len( "-vcshead=" ) ) == "-vcshead="

         l_cVCSDIR := FNameDirGet( aParam[ _PAR_cFileName ] )
         l_cVCSHEAD := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, SubStr( cParam, Len( "-vcshead=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF Empty( FNameExtGet( l_cVCSHEAD ) )
            l_cVCSHEAD := FNameExtSet( l_cVCSHEAD, ".ch" )
         ENDIF

      CASE Left( cParamL, Len( "-tshead=" ) ) == "-tshead="

         l_cTSHEAD := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, SubStr( cParam, Len( "-tshead=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF Empty( FNameExtGet( l_cTSHEAD ) )
            l_cTSHEAD := FNameExtSet( l_cTSHEAD, ".ch" )
         ENDIF

      CASE Left( cParamL, Len( "-plugin=" ) ) == "-plugin="

         cParam := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, SubStr( cParam, Len( "-plugin=" ) + 1 ), aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF ( tmp := FindInPathPlugIn( cParam ) ) != NIL
            PlugIn_Load( hbmk, tmp )
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Warning: Plugin not found: %1$s" ), cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParam, 2 ) == "-l" .AND. ;
           Len( cParam ) > 2 .AND. ;
           !( Left( cParam, 3 ) == "-l-" )

         cParam := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            cParam := PathSepToSelf( cParam )
            IF _IS_AUTOLIBSYSPRE( cParam )
               AAdd( hbmk[ _HBMK_aLIBUSERSYSPRE ], cParam )
            ELSE
               AAdd( hbmk[ _HBMK_aLIBUSER ], cParam )
            ENDIF
         ENDIF

      CASE Left( cParam, Len( "-autohbc=" ) ) == "-autohbc="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-autohbc=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF autohbc_split_arg( cParam, @tmp, @cParam )

            cParam := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )

            IF Empty( FNameExtGet( tmp ) )
               tmp := FNameExtSet( tmp, ".ch" )
            ENDIF
            IF Empty( FNameExtGet( cParam ) )
               cParam := FNameExtSet( cParam, ".hbc" )
            ENDIF

            IF ! hb_FileExists( cParam )
               FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                  IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cParam ) )
                     cParam := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cParam )
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

      CASE Left( cParam, Len( "-depincpath=" ) ) == "-depincpath="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depincpath=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aINCPATH ], PathNormalize( PathMakeAbsolute( PathSepToSelf( tmp ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE Left( cParam, Len( "-depincpathlocal=" ) ) == "-depincpathlocal="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depincpathlocal=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aINCPATHLOCAL ], PathNormalize( PathMakeAbsolute( PathSepToSelf( tmp ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE Left( cParam, Len( "-depimplibs=" ) ) == "-depimplibs="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depimplibs=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            AAddNew( hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_aIMPLIBSRC ], PathSepToSelf( tmp ) )
         ENDIF

      CASE Left( cParam, Len( "-depimplibd=" ) ) == "-depimplibd="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depimplibd=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF dep_split_arg( hbmk, cParam, @cParam, @tmp )
            hbmk[ _HBMK_hDEP ][ cParam ][ _HBMKDEP_cIMPLIBDST ] := FNameNameExtGet( PathSepToSelf( tmp ) )
         ENDIF

      CASE Left( cParam, Len( "-depfinish=" ) ) == "-depfinish="

         cParam := MacroProc( hbmk, SubStr( cParam, Len( "-depfinish=" ) + 1 ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam ) .AND. cParam $ hbmk[ _HBMK_hDEP ]
            dep_try_detection( hbmk, hbmk[ _HBMK_hDEP ][ cParam ] )
         ENDIF

      CASE Left( cParam, 1 ) $ cOptPrefix

         DO CASE
         CASE lAcceptLDFlag
            AAddNotEmpty( hbmk[ _HBMK_aOPTL ], PathSepToSelf( cParam, 2 ) )
         CASE lAcceptCFlag
            IF SubStr( cParamL, 2 ) == "c"
               lStopAfterCComp := .T.
            ELSE
               AAddNotEmpty( hbmk[ _HBMK_aOPTC ], PathSepToSelf( cParam, 2 ) )
            ENDIF
         CASE lAcceptIFlag
            AAddNotEmpty( hbmk[ _HBMK_aOPTI ], PathSepToSelf( cParam, 2 ) )
         OTHERWISE
            IF SubStr( cParamL, 2 ) == "gh"
               hbmk[ _HBMK_lStopAfterHarbour ] := .T.
               hbmk[ _HBMK_lCreateHRB ] := .T.
            ENDIF
            /* Detect if Harbour is only used as preprocessor (-p + -s options) */
            IF SubStr( cParamL, 2 ) == "p"
               ++nHarbourPPO
               tmp := MacroProc( hbmk, SubStr( cParam, 3 ), aParam[ _PAR_cFileName ] )
               IF ! Empty( tmp )
                  tmp := PathMakeAbsolute( PathSepToSelf( tmp ), aParam[ _PAR_cFileName ] )
                  hb_FNameSplit( tmp, @cDir, @cName, @cExt )
                  cHarbourPPODir := cDir
               ENDIF
            ENDIF
            IF SubStr( cParamL, 2 ) == "s"
               hbmk[ _HBMK_lStopAfterHarbour ] := .T.
               ++nHarbourPPO
            ENDIF
            IF nHarbourPPO >= 2
               hbmk[ _HBMK_lCreatePPO ] := .T.
            ENDIF
            AAddNewNotEmpty( hbmk[ _HBMK_aOPTPRG ], PathSepToSelf( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ), 2 ) )
         ENDCASE

      CASE hbmk[ _HBMK_lCreateImpLib ]

         cParam := MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aIMPLIBSRC ], PathNormalize( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ) ) )
         ENDIF

      CASE FNameExtGet( cParamL ) == ".lib" .OR. ;
           ( ! Empty( hbmk[ _HBMK_cDynLibExt ] ) .AND. FNameExtGet( cParamL ) == hbmk[ _HBMK_cDynLibExt ] )

         cParam := PathSepToSelf( cParam )
         IF _IS_AUTOLIBSYSPRE( cParam )
            AAdd( hbmk[ _HBMK_aLIBUSERSYSPRE ], cParam )
         ELSE
            AAdd( hbmk[ _HBMK_aLIBUSER ], cParam )
         ENDIF

      CASE FNameExtGet( cParamL ) == ".hbc"

         cParam := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF ! hb_FileExists( cParam )
            FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
               IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cParam ) )
                  cParam := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cParam, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cParam )
                  EXIT
               ENDIF
            NEXT
         ENDIF

         cParam := PathNormalize( cParam )

         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cParam ) )
         ENDIF

         HBC_ProcessOne( hbmk, cParam, 1 )

      CASE FNameExtGet( cParamL ) == ".hrb"

         cParam := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) ), aParam[ _PAR_cFileName ] )
         IF ( tmp := FindInPathPlugIn( cParam ) ) != NIL
            PlugIn_Load( hbmk, tmp )
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Warning: Plugin not found: %1$s" ), cParam ) )
            ENDIF
         ENDIF

      CASE FNameExtGet( cParamL ) == ".prg" .OR. ;
           FNameExtGet( cParamL ) == ".hbs"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPRG ], cParam )
            DEFAULT hbmk[ _HBMK_cFIRST ] TO cParam
         NEXT

      CASE FNameExtGet( cParamL ) == ".rc"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aRESSRC ], cParam )
         NEXT

      CASE FNameExtGet( cParamL ) == ".res"

         IF HBMK_ISCOMP( "mingw|mingw64|mingwarm" ) .OR. ;
            ( hbmk[ _HBMK_cPLAT ] == "os2" .AND. HBMK_ISCOMP( "gcc|gccomf" ) )
            /* For MinGW/EMX GCC family add .res files as source input, as they
               will need to be converted to coff format with windres (just
               like plain .rc files) before feeding them to gcc. */
            FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aRESSRC ], cParam )
            NEXT
         ELSE
            FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aRESCMP ], cParam )
            NEXT
         ENDIF

      CASE FNameExtGet( cParamL ) == ".a"

         cParam := PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] )
         AAdd( l_aOBJA, cParam )

      CASE FNameExtGet( cParamL ) == ".def"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aDEF ], cParam )
         NEXT

      CASE FNameExtGet( cParamL ) == ".o" .OR. ;
           FNameExtGet( cParamL ) == ".obj"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aOBJUSER ], cParam )
            DEFAULT hbmk[ _HBMK_cFIRST ] TO cParam
         NEXT

      CASE FNameExtGet( cParamL ) == ".cpp" .OR. ;
           FNameExtGet( cParamL ) == ".cc" .OR. ;
           FNameExtGet( cParamL ) == ".cxx" .OR. ;
           FNameExtGet( cParamL ) == ".cx" .OR. ;
           _EXT_IS_UPPER( cParam, ".C" )

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aCPP ], cParam )
            DEFAULT hbmk[ _HBMK_cFIRST ] TO cParam
         NEXT

      CASE FNameExtGet( cParamL ) == ".c" .OR. ;
           FNameExtGet( cParamL ) == ".m"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aC ], cParam )
            DEFAULT hbmk[ _HBMK_cFIRST ] TO cParam
         NEXT

      CASE FNameExtGet( cParamL ) == ".d"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            deplst_read( hbmk, hbmk[ _HBMK_hDEPTS ], cParam )
         NEXT

      CASE FNameExtGet( cParamL ) == ".po" .OR. ;
           FNameExtGet( cParamL ) == ".pot"

         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPO ], cParam )
         NEXT

      CASE FNameExtGet( cParamL ) == ".hbl"

         hbmk[ _HBMK_cHBL ] := PathSepToSelf( cParam )
         hbmk[ _HBMK_cHBLDir ] := FNameDirGet( aParam[ _PAR_cFileName ] )

      CASE FNameExtGet( cParamL ) $ hbmk[ _HBMK_hPLUGINExt ]

         cParam := PathSepToSelf( MacroProc( hbmk, cParam, aParam[ _PAR_cFileName ] ) )
         FOR EACH cParam IN FN_Expand( PathMakeAbsolute( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPLUGINPars ], cParam )
         NEXT

      OTHERWISE

         cParam := PathMakeAbsolute( PathSepToSelf( cParam ), aParam[ _PAR_cFileName ] )
         IF Empty( FNameExtGet( cParam ) )
            cParam := FNameExtSet( cParam, ".prg" )
         ENDIF
         AAdd( hbmk[ _HBMK_aPRG ], cParam )
         DEFAULT hbmk[ _HBMK_cFIRST ] TO cParam

      ENDCASE
   NEXT

   IF hbmk[ _HBMK_lDEBUGPARS ]
      FOR EACH aParam IN aParams
         hbmk_OutStd( hbmk, hb_StrFormat( "debugpars: %1$s '%2$s' (%3$s:%4$s)", Str( aParam:__enumIndex(), 3 ), aParam[ _PAR_cParam ], aParam[ _PAR_cFileName ], hb_ntos( aParam[ _PAR_nLine ] ) ) )
      NEXT
   ENDIF

   IF ! l_lLIBSYSMISC
      l_aLIBSYSMISC := {}
   ENDIF

   IF lHarbourInfo
      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
         /* Use integrated compiler */
         hb_compile( "harbour", hbmk[ _HBMK_aOPTPRG ] )
      ELSE
         /* Use external compiler */
         cCommand := FNameEscape( DirAddPathSep( PathSepToSelf( l_cHB_INSTALL_BIN ) ) + cBin_CompPRG + cBinExt, hbmk[ _HBMK_nCmd_Esc ] ) +;
                     iif( ! Empty( hbmk[ _HBMK_aOPTPRG ] ), " " + ArrayToList( hbmk[ _HBMK_aOPTPRG ] ), "" )
         hb_processRun( AllTrim( cCommand ) )
      ENDIF
      RETURN _ERRLEV_OK
   ENDIF

   /* Strip leading @ char of .clp files */
   IF ! Empty( hbmk[ _HBMK_cFIRST ] ) .AND. Left( hbmk[ _HBMK_cFIRST ], 1 ) == "@" .AND. Lower( FNameExtGet( hbmk[ _HBMK_cFIRST ] ) ) == ".clp"
      hbmk[ _HBMK_cFIRST ] := SubStr( hbmk[ _HBMK_cFIRST ], 2 )
   ENDIF

#if 0 /* disabled to experiment with '-hbdyn -shared' combination. */
   IF hbmk[ _HBMK_lCreateDyn ] .AND. hbmk[ _HBMK_lSHARED ]
      hbmk[ _HBMK_lSHARED ] := .F.
   ENDIF
#endif

   /* Force MT mode off in 1.0.x and xhb/dos compatibility modes. */
   IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
      ( _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] ) .AND. hbmk[ _HBMK_cPLAT ] == "dos" )
      hbmk[ _HBMK_lMT ] := .F.
   ENDIF

   /* Start doing the make process. */
   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lCreateImpLib ] .AND. ( Len( hbmk[ _HBMK_aPLUGINPars ] ) + Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) + Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ) ) == 0 .AND. ! hbmk[ _HBMK_lContainer ]
      hbmk_OutErr( hbmk, I_( "Warning: No source files were specified." ) )
      RETURN _ERRLEV_OK
   ENDIF

   /* Decide about output name */
   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lCreateImpLib ]

      /* If -o with full name wasn't specified, let's
         make it the first source file specified. */
      DEFAULT hbmk[ _HBMK_cPROGNAME ] TO FNameNameGet( hbmk[ _HBMK_cFIRST ] )

      /* Combine output dir with output name. */
      IF ! Empty( hbmk[ _HBMK_cPROGDIR ] )
         hb_FNameSplit( hbmk[ _HBMK_cPROGNAME ], @cDir, @cName, @cExt )
         hbmk[ _HBMK_cPROGNAME ] := hb_FNameMerge( iif( Empty( cDir ), hbmk[ _HBMK_cPROGDIR ], cDir ), cName, cExt )
      ENDIF
   ENDIF

   /* Decide about working dir */
   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lCreateImpLib ] .AND. ! lDumpInfo
      IF hbmk[ _HBMK_lINC ]
         DEFAULT hbmk[ _HBMK_cWorkDir ] TO FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ) + _WORKDIR_DEF_ + hbmk[ _HBMK_cWorkDirDynSub ]
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            IF ! DirBuild( hbmk[ _HBMK_cWorkDir ] )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Working directory cannot be created: %1$s" ), hbmk[ _HBMK_cWorkDir ] ) )
               IF hbmk[ _HBMK_lBEEP ]
                  DoBeep( .F. )
               ENDIF
               RETURN _ERRLEV_WORKDIRCREATE
            ENDIF
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lStopAfterInit ] .OR. ;
            hbmk[ _HBMK_lStopAfterHarbour ] .OR. ;
            ( lStopAfterCComp .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] )
            /* It's controlled by -o option in these cases */
            hbmk[ _HBMK_cWorkDir ] := ""
         ELSE
            IF hbmk[ _HBMK_cWorkDir ] == NIL
               FClose( hb_FTempCreateEx( @hbmk[ _HBMK_cWorkDir ], NIL, "hbmk_", ".dir" ) )
               FErase( hbmk[ _HBMK_cWorkDir ] )
               IF hb_DirCreate( hbmk[ _HBMK_cWorkDir ] ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Temporary Working directory cannot be created: %1$s" ), hbmk[ _HBMK_cWorkDir ] ) )
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _ERRLEV_WORKDIRCREATE
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

         /opt/local/include                          (darwin MacPorts)
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

      DEFAULT hbmk[ _HBMK_lSHAREDDIST ] TO lSysLoc

      IF hbmk[ _HBMK_lSHAREDDIST ] .OR. ! HBMK_ISCOMP( "gcc|clang|open64" )
         cPrefix := ""
      ELSE
         /* Only supported by gcc, clang, open64 compilers. */
         cPrefix := DirAddPathSep( l_cHB_INSTALL_DYN )
      ENDIF
#if 1
      cPostfix := ""
      HB_SYMBOL_UNUSED( cDL_Version )
#else
      cPostfix := cDL_Version
#endif

      DO CASE
      CASE HBMK_ISPLAT( "darwin|bsd|linux|hpux|beos|qnx|vxworks|sunos" )
         IF Empty( cPrefix )
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cPostfix,;
                                                      "harbour"   + cPostfix ) }
         ELSE
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], cPrefix + hbmk[ _HBMK_cDynLibPrefix ] + "harbourmt" + cPostfix + hbmk[ _HBMK_cDynLibExt ],;
                                                      cPrefix + hbmk[ _HBMK_cDynLibPrefix ] + "harbour"   + cPostfix + hbmk[ _HBMK_cDynLibExt ] ) }
         ENDIF
      CASE HBMK_ISPLAT( "os2|win|wce" )
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], hbmk[ _HBMK_cDynLibPrefix ] + "harbourmt",;
                                                   hbmk[ _HBMK_cDynLibPrefix ] + "harbour" ) }
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
         {LB}     list of lib files with paths
         {FC}     flags for C compiler (user + automatic)
         {FL}     flags for linker (user + automatic)
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

      #define _HBLIB_FULLPATH( cName ) ( DirAddPathSep( l_cHB_INSTALL_LIB ) + hb_ps() + cLibLibPrefix + cName + cLibLibExt )

      cLibHBX_Regex := "[[:space:]]_?HB_FUN_([A-Z0-9_]*)[[:space:]]"

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
           ( hbmk[ _HBMK_cPLAT ] == "beos"    .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "qnx"     .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "vxworks" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "symbian" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "cygwin"  .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "linux"   .AND. hbmk[ _HBMK_cCOMP ] == "open64" )

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
         IF hbmk[ _HBMK_cPLAT ] == "darwin" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" /* TODO: Check what to use for icc */
            cBin_Lib := "libtool"
            cOpt_Lib := "-static {FA} -o {OL} {LO}"
         ELSE
            DO CASE
            CASE hbmk[ _HBMK_cCOMP ] == "icc"
               cBin_Lib := "xiar"
            CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
               cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + hbmk[ _HBMK_cCCPOSTFIX ]
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
            cBin_CompC := hbmk[ _HBMK_cCCPREFIX ] + "clang" + hbmk[ _HBMK_cCCPOSTFIX ]
            cBin_CompCPP := cBin_CompC
         CASE hbmk[ _HBMK_cCOMP ] == "open64"
            cBin_CompCPP := "openCC"
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, "opencc" )
         CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
            cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCPOSTFIX ]
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "cc" + hbmk[ _HBMK_cCCPOSTFIX ] )
         OTHERWISE
            cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCPOSTFIX ]
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCPOSTFIX ] )
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
            CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration -Wmissing-braces -Wreturn-type -Wformat" ) ; EXIT
            CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
            ENDSWITCH
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "vxworks"
            AAdd( hbmk[ _HBMK_aOPTC ], "-mrtp" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-mrtp" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-mrtp" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-fno-strict-aliasing" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_C99" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_HAS_C9X" )
         ENDIF
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            /* Symbian gcc cross-compiler (on Windows) crashes if compiling multiple files at once */
            IF !( hbmk[ _HBMK_cPLAT ] == "symbian" ) /* EXPERIMENTAL */
               lCHD_Comp := .T.
               cOpt_CompC += " {LC}"
            ELSE
               IF HBMK_ISPLAT( "linux|bsd" ) .AND. hbmk[ _HBMK_cCOMP ] == "clang"
                  /* NOTE: It's also accepted by darwin/clang */
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
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-shared -o {OD} {LO} {FD} {DL} {LS}"
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
         IF l_lLIBGROUPING .AND. ;
            ( hbmk[ _HBMK_cPLAT ] == "linux" .OR. ;
              hbmk[ _HBMK_cPLAT ] == "beos" .OR. ;
              hbmk[ _HBMK_cPLAT ] == "qnx" .OR. ;
              hbmk[ _HBMK_cPLAT ] == "vxworks" .OR. ;
              hbmk[ _HBMK_cPLAT ] == "cygwin" .OR. ;
              hbmk[ _HBMK_cPLAT ] == "bsd" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "{LL} {LB}" )
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
            /* It's to avoid warning message generated when 'long double' is used
               remove it if you have newer compiler version */
/*          AAdd( hbmk[ _HBMK_aOPTC ], "-Wno-long-double" ) */
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
                  cBin_Post := "strip" + hbmk[ _HBMK_cCCPOSTFIX ]
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
               IF HBMK_ISPLAT( "darwin|sunos" )
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o {OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
               ENDIF
            ENDIF
         ELSE
            IF HBMK_ISPLAT( "darwin|sunos" )
               AAdd( hbmk[ _HBMK_aOPTL ], "-o {OE}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
            ENDIF
         ENDIF

         IF hbmk[ _HBMK_lCreateDyn ] .AND. ! HBMK_ISPLAT( "darwin|cygwin" )
            IF HBMK_ISPLAT( "hpux|sunos|linux" )
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
                  IF !( hbmk[ _HBMK_cPLAT ] == "qnx" )
                     AAdd( l_aLIBSYS, "pthread" )
                  ENDIF
               ENDIF
            ENDIF
            DO CASE
            CASE HBMK_ISPLAT( "linux|cygwin" )
               AAdd( l_aLIBSYS, "dl" )
               AAdd( l_aLIBSYS, "rt" )
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
            AAdd( l_aLIBSYS, iif( HBMK_ISPLAT( "sunos|bsd" ), "curses", "ncurses" ) )
         ENDIF
         IF IsGTRequested( hbmk, "gtsln" )
            IF hbmk[ _HBMK_cPLAT ] == "bsd" .AND. ;
               hb_FileExists( "/usr/pkg/lib/libslang2.so" ) /* For pkgsrc */
               AAdd( l_aLIBSYS, "slang2" )
            ELSE
               AAdd( l_aLIBSYS, "slang" )
            ENDIF
            /* Add paths, where this isn't a system component */
            DO CASE
            CASE hbmk[ _HBMK_cPLAT ] == "darwin"
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/sw/lib" ) /* For Fink */
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/opt/local/lib" ) /* For MacPorts (formerly DarwinPorts) */
            CASE hbmk[ _HBMK_cPLAT ] == "bsd"
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" ) /* For ports */
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/pkg/lib" ) /* For pkgsrc */
            ENDCASE
         ENDIF
         IF IsGTRequested( hbmk, "gtxwc" )
            IF hbmk[ _HBMK_cPLAT ] == "linux" .AND. hb_DirExists( "/usr/X11R6/lib64" )
               AAddNew( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib64" )
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
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ),;
                                                      "harbour" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) ) }
         ENDIF

      CASE ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw64" ) .OR. ;
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
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ] )
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
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration -Wmissing-braces -Wreturn-type -Wformat" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
         ENDSWITCH
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
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-shared -o {OD} {LO} {FD} {IM} {DL} {LS}"
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {LS} {FL} {IM} {DL}"
         bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_gcc( hbmk, hbmk[ _HBMK_cCCPREFIX ] + "dlltool" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ] + " {FI} -d {ID} -l {OL}", cSourceDLL, cTargetLib, cFlags ) }
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cImpLibExt := cLibLibExt
         cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + hbmk[ _HBMK_cCCEXT ]
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
         IF !( hbmk[ _HBMK_cPLAT ] == "wce" )
            IF hbmk[ _HBMK_lGUI ]
               AAdd( hbmk[ _HBMK_aOPTL ], "-mwindows" )
               l_cCMAIN := "hb_forceLinkMainWin"
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-mconsole" )
            ENDIF
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
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
            AAdd( hbmk[ _HBMK_aOPTD ], "{LL} {LB}" )
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
         IF hbmk[ _HBMK_cPLAT ] == "wce"
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNDER_CE" )
            AAdd( hbmk[ _HBMK_aOPTRES ], "-DUNDER_CE" )
         ENDIF
         DO CASE
         CASE _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            /* NOTE: Newer xhb versions use "-x.y.z" version numbers. */
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "xharbourmt",;
                                                      "xharbour" ) }
         OTHERWISE
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ),;
                                                      "harbour" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) ) }
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

         IF HBMK_ISCOMP( "mingw|mingw64|mingwarm" )
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
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "g++" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ] )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall -pedantic" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall" )           ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration -Wmissing-braces -Wreturn-type -Wformat" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            cOpt_CompC += " {IC} -o {OO}"
            AAdd( hbmk[ _HBMK_aOPTC ], "-pipe" )
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-shared -o {OD} {LO} {LL} {LB} {FD} {IM} {DL} {LS}"
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
         AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
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
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourm",;
                                                   "harbour" ) }

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
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "gpp" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "gcc" + hbmk[ _HBMK_cCCPOSTFIX ] + hbmk[ _HBMK_cCCEXT ] )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall -pedantic" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-W -Wall" )           ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-Wimplicit-int -Wimplicit-function-declaration -Wmissing-braces -Wreturn-type -Wformat" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w" )                 ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC}"
         IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Dyn := "dxe3gen"
         cOpt_Dyn := "--whole-archive -U {FD} -o {OD} {DL} {LO} {LL} {LB} {LS}"
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
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
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

         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourm" + cLibExt,;
                                                   "harbour" + cLibExt ) }

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
            ENDIF
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-wx" ) ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-w3" ) ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-w1 -wcd124 -wcd136 -wcd201 -wcd367 -wcd368" ) ; EXIT
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
         IF lStopAfterCComp .AND. ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ]
            IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aCPP ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OO}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OD}" )
            ENDIF
         ENDIF
         cBin_Link := "wlink" + hbmk[ _HBMK_cCCEXT ]
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "linux" ; cOpt_Link := "OP quiet SYS linux {FL} NAME {OE} {LO} {DL} {LL} {LB}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "dos"   ; cOpt_Link := "OP quiet SYS dos32a {FL} NAME {OE} {LO} {DL} {LL} {LB}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "win"   ; cOpt_Link := "OP quiet {FL} {IM} NAME {OE} {LO} {DL} {LL} {LB} {LS}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "os2"   ; cOpt_Link := "OP quiet SYS os2v2 {FL} {IM} NAME {OE} {LO} {DL} {LL} {LB} {LS}{SCRIPT}"
         ENDCASE
         cBin_Dyn := cBin_Link
         cDynObjPrefix := cObjPrefix
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "dos"   ; cBin_Dyn := NIL
         CASE hbmk[ _HBMK_cPLAT ] == "linux" ; cOpt_Dyn := "OP quiet FORM elf dll OP exportall {FD} NAME {OD} {LO} {DL} {LL} {LB}{SCRIPT}"
            IF hbmk[ _HBMK_lCreateDyn ]
               AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToSelf( GetEnv( "WATCOM") + hb_ps() + "lib386" ) )
               AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToSelf( GetEnv( "WATCOM") + hb_ps() + "lib386" + hb_ps() + "linux" ) )
            ENDIF
         CASE hbmk[ _HBMK_cPLAT ] == "win"   ; cOpt_Dyn := "OP quiet SYS nt_dll {FD} {IM} NAME {OD} {LO} {DL} {LL} {LB} {LS}{SCRIPT}"
         CASE hbmk[ _HBMK_cPLAT ] == "os2"   ; cOpt_Dyn := "OP quiet SYS os2v2_dll {FD} {IM} NAME {OD} {LO} {DL} {LL} {LB} {LS}{SCRIPT}"
         ENDCASE
         IF HBMK_ISPLAT( "win|os2" ) .AND. ! Empty( hbmk[ _HBMK_aDEF ] )
            /* TODO: Watcom wlink requires a non-standard internal layout for .def files.
                     We will need a converter and implement on-the-fly conversion
                     to a temp file and pass that via {IM}. */
            cDefPrefix := "@"
         ENDIF
         IF hbmk[ _HBMK_cPLAT ] == "dos"
            /* workaround for not included automatically CLIB in pure C mode DOS builds */
            AAdd( l_aLIBSYS, "clib3r" )
         ENDIF
         cBin_Lib := "wlib" + hbmk[ _HBMK_cCCEXT ]
         cOpt_Lib := "-q {FA} {OL} {LO}{SCRIPT}"
         cBin_LibHBX := cBin_Lib
         cOpt_LibHBX := "{LI}"
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
         DO CASE
         CASE hbmk[ _HBMK_cPLAT ] == "win"
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + cLibExt ) }

            IF hbmk[ _HBMK_lSHARED ]
               AAdd( hbmk[ _HBMK_aOPTL ], "FILE " + FNameExtSet( l_cHB_INSTALL_LIB + hb_ps() + iif( hbmk[ _HBMK_lGUI ], "hbmainwin", "hbmainstd" ), cLibExt ) )
            ENDIF
         CASE hbmk[ _HBMK_cPLAT ] == "os2"
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourm" + cLibExt,;
                                                      "harbour" + cLibExt ) }

            IF hbmk[ _HBMK_lSHARED ]
               /* TOFIX: This line is plain guessing. */
               AAdd( hbmk[ _HBMK_aOPTL ], "FILE " + FNameExtSet( l_cHB_INSTALL_LIB + hb_ps() + iif( hbmk[ _HBMK_lGUI ], "hbmainstd", "hbmainstd" ), cLibExt ) )
            ENDIF
         CASE hbmk[ _HBMK_cPLAT ] == "linux"
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], hbmk[ _HBMK_cDynLibPrefix ] + "harbourmt" + cDL_Version + hbmk[ _HBMK_cDynLibExt ],;
                                                      hbmk[ _HBMK_cDynLibPrefix ] + "harbour" + cDL_Version + hbmk[ _HBMK_cDynLibExt ] ) }
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

      CASE hbmk[ _HBMK_cPLAT ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "bcc"
         hbmk[ _HBMK_nCmd_FNF ] := _FNF_BCKSLASH
         #if defined( __PLATFORM__UNIX )
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_NIX
         #else
            hbmk[ _HBMK_nCmd_Esc ] := _ESC_DBLQUOTE
         #endif
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-y -v" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-v" )
         ELSE
            AAdd( l_aCLEAN, PathSepToSelf( FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".tds" ) ) )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-tW" )
         ENDIF
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-P" )
         ENDIF
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cBin_Lib := "tlib.exe"
         /* Only forward slash is accepted here as option prefix. */
         cOpt_Lib := "/P128 {FA} {OL} {LO}{SCRIPT}"
         cBin_LibHBX := cBin_Lib
         cOpt_LibHBX := "{LI}, {OT}"
         cLibLibExt := cLibExt
         cImpLibExt := cLibLibExt
         cLibObjPrefix := "-+ "
         cOptIncMask := "-I{DI}"
         cBin_CompC := "bcc32.exe"
         cBin_CompCPP := cBin_CompC
         cOpt_CompC := "-c -q"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -d -6 -O2 -OS -Ov -Oi -Oc"
         ENDIF
         cLibBCC_CRTL := "cw32mt.lib"
         IF _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
            /* Adding weird hack for xhb to make it possible to force ST C mode. */
            IF AScan( hbmk[ _HBMK_aOPTC ], {| tmp | tmp == "-tW" } ) == 0
               AAdd( hbmk[ _HBMK_aOPTC ], "-tWM" )
            ELSE
               cLibBCC_CRTL := "cw32.lib"
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTC ], "-tWM" )
         ENDIF
         SWITCH hbmk[ _HBMK_nWARN ]
         CASE _WARN_MAX ; AAdd( hbmk[ _HBMK_aOPTC ], "-w -Q" )         ; EXIT
         CASE _WARN_YES ; AAdd( hbmk[ _HBMK_aOPTC ], "-w -Q -w-sig-" ) ; EXIT
         CASE _WARN_LOW ; AAdd( hbmk[ _HBMK_aOPTC ], "-w-sig- -w-aus- -w-ccc- -w-csu- -w-par- -w-rch- -w-ucp- -w-use- -w-prc-" ) ; EXIT
         CASE _WARN_NO  ; AAdd( hbmk[ _HBMK_aOPTC ], "-w-" )           ; EXIT
         ENDSWITCH
         cOpt_CompC += " {FC} {LC}"
         cBin_Res := "brcc32.exe"
         cOpt_Res := "{FR} {IR} -fo{OS}"
         cResExt := ".res"
         cBin_Link := "ilink32.exe"
         cBin_Dyn := cBin_Link
         cOpt_Link := '-Gn -Tpe -L{DL} {FL} ' + iif( hbmk[ _HBMK_lGUI ], "c0w32.obj", "c0x32.obj" ) + " {LO}, {OE}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} " + cLibBCC_CRTL + " import32.lib, {IM}, {LS}{SCRIPT}"
         cOpt_Dyn  := '-Gn -Tpd -L{DL} {FD} ' +                          "c0d32.obj"                + " {LO}, {OD}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} " + cLibBCC_CRTL + " import32.lib, {IM}, {LS}{SCRIPT}"
         bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_bcc( hbmk, "implib.exe -c {FI} {OL} {ID}", cSourceDLL, cTargetLib, cFlags ) }
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
            /* NOTE: Borland C doesn't support creating implibs with a specific name,
                     so it's done using post command. The resulting implib won't be
                     as optimal as the generated one, but it _should_ be the same.
                     [vszakats] */
            /* AAdd( hbmk[ _HBMK_aOPTL ], "-Gi" ) */
            /* AAdd( hbmk[ _HBMK_aOPTD ], "-Gi" ) */
            cBin_Post := "implib.exe"
            cOpt_Post := "-c {OI} {OB}"
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
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-bcc" + cLibExt,;
                                                   "harbour" + cDL_Version_Alter + "-bcc" + cLibExt ) }
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
         cOpt_Lib := "-nologo {FA} -out:{OL} {LO}"
         cOpt_Dyn := "-nologo {FD} {IM} -dll -out:{OD} {DL} {LO} {LL} {LB} {LS}"
         cOpt_CompC := "-nologo -c"
         cBin_LibHBX := "dumpbin.exe"
         cOpt_LibHBX := "-symbols {LI}"
         cLibHBX_Regex := "SECT[0-9A-Z][0-9A-Z ].*[Ee]xternal.*_?HB_FUN_([A-Z0-9_]*)[[:space:]]"
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
         cOpt_CompC += " {FC} {LC}"
         cOptIncMask := "-I{DI}"
         cOpt_Link := "-nologo -out:{OE} {LO} {DL} {FL} {IM} {LL} {LB} {LS}"
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
         IF hbmk[ _HBMK_cPLAT ] == "wce"
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
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
            AAdd( hbmk[ _HBMK_aOPTC ], "-Fo" + FNameEscape( DirAddPathSep( hbmk[ _HBMK_cWorkDir ] ), hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) /* NOTE: Ending path sep is important. */
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
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) + cLibExt,;
                                                   "harbour" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) + cLibExt ) }
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
            !( hbmk[ _HBMK_cCOMP ] == "xcc" ) /* xcc doesn't have this enabled in default Harbour builds. */
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
         cOpt_Dyn := "{FD} {IM} -dll -out:{OD} {DL} {LO} {LL} {LB} {LS}"
         bBlk_ImpLib := {| cSourceDLL, cTargetLib, cFlags | win_implib_command_pocc( hbmk, cBin_Lib + " {ID} -out:{OL}", cSourceDLL, cTargetLib, cFlags ) }
         cBin_LibHBX := "podump.exe"
         cOpt_LibHBX := "-symbols {LI}"
         cLibHBX_Regex := "SECT[0-9A-Z][0-9A-Z ].*[Ee]xternal.*_?HB_FUN_([A-Z0-9_]*)[[:space:]]"
         IF hbmk[ _HBMK_cPLAT ] == "wce"
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
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
         cOpt_Link := "-out:{OE} {LO} {DL} {FL} {IM} {LL} {LB} {LS}"
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
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) + cLibExt,;
                                                   "harbour" + cDL_Version_Alter + hbmk_DYNSUFFIX( hbmk ) + cLibExt ) }
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
         cBin_CompCPP := hbmk[ _HBMK_cCCPREFIX ] + "sunCC" + hbmk[ _HBMK_cCCPOSTFIX ]
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], cBin_CompCPP, hbmk[ _HBMK_cCCPREFIX ] + "suncc" + hbmk[ _HBMK_cCCPOSTFIX ] )
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
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-G {FD} -o {OD} {DL} {LO} {LL} {LB} {LS}"
         IF ! lStopAfterCComp
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
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

         IF hbmk[ _HBMK_lCreateDyn ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-KPIC" )
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
         tmp := "-WDVSB_DIR=" + PathSepToSelf( GetEnv( "WIND_BASE" ) + "/target/lib" )
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
         /* lib path list ({DL}) must precede lib list */
         cBin_Dyn := cBin_CompC
         cOpt_Dyn := "-Xpic -Wl, -Xshared -o {OD} {LO} {DL} {FD} {LS}"
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {DL} {FL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
         AAdd( hbmk[ _HBMK_aOPTD ], "{LL} {LB}" )
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

         IF hbmk[ _HBMK_lCreateDyn ]
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
      RETURN _ERRLEV_PLUGINPREALL
   ENDIF

   /* ; */

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. hbmk[ _HBMK_lCreateImpLib ] .AND. ! lDumpInfo
      /* OBSOLETE functionality */
      IF DoIMPLIB( hbmk, bBlk_ImpLib, cLibLibPrefix, cLibLibExt, hbmk[ _HBMK_aIMPLIBSRC ], hbmk[ _HBMK_cPROGNAME ], "" )
         DoInstCopy( hbmk )
      ENDIF
      hbmk[ _HBMK_lStopAfterInit ] := .T.
   ENDIF

   DEFAULT hbmk[ _HBMK_nScr_Esc ] TO hbmk[ _HBMK_nCmd_Esc ]

   /* Delete all lib paths which contain late-evaluation macros. */
   FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ] DESCEND
      IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ tmp
         hb_ADel( hbmk[ _HBMK_aLIBPATH ], tmp:__enumIndex(), .T. )
      ENDIF
   NEXT

   IF ! hbmk[ _HBMK_lStopAfterInit ]
      IF ! Empty( hbmk[ _HBMK_cWorkDir ] )
         /* NOTE: Ending path sep is important. */
         /* Different escaping for internal and external compiler. */
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-o" + DirAddPathSep( hbmk[ _HBMK_cWorkDir ] ) )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-o" + FNameEscape( DirAddPathSep( hbmk[ _HBMK_cWorkDir ] ), hbmk[ _HBMK_nCmd_Esc ] ) )
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
         /* TOFIX: ? Add l_cLIBSELF for *nix dynamic builds to exclude self name. */
         l_cIMPLIBNAME := hb_FNameMerge( l_cIMPLIBDIR, cLibLibPrefix + l_cIMPLIBNAME, cImpLibExt )
      CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ]
         l_cLIBSELF := cName
         hbmk[ _HBMK_cPROGNAME ] := hb_FNameMerge( cDir, cLibLibPrefix + cName, iif( Empty( cLibLibExt ), cExt, cLibLibExt ) )
      ENDCASE
   ENDIF

   DoLinkCalc( hbmk )

   /* Generate header with repository ID information */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! lDumpInfo
      IF ! Empty( l_cVCSHEAD )
         tmp1 := VCSID( l_cVCSDIR, l_cVCSHEAD, @tmp2 )
         /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
         tmp := "/* Automatically generated by hbmk2. Do not edit. */" + Chr( 10 ) +;
                "#define _HBMK_VCS_TYPE_ " + '"' + tmp2 + '"' + Chr( 10 ) +;
                "#define _HBMK_VCS_ID_   " + '"' + tmp1 + '"' + Chr( 10 )
         /* Update only if something changed to trigger rebuild only if really needed */
         IF hbmk[ _HBMK_lREBUILD ] .OR. !( hb_MemoRead( l_cVCSHEAD ) == tmp )
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating VCS header: %1$s" ), l_cVCSHEAD ) )
            ENDIF
            hb_MemoWrit( l_cVCSHEAD, tmp )
         ENDIF
      ENDIF
      IF ! Empty( l_cTSHEAD )
         /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
         tmp1 := hb_DateTime()
         tmp := "/* Automatically generated by hbmk2. Do not edit. */" + Chr( 10 ) +;
                "#define _HBMK_BUILD_DATE_      " + '"' +            DToS( tmp1 )         + '"' + Chr( 10 ) +;
                "#define _HBMK_BUILD_TIME_      " + '"' + SubStr( hb_TToS( tmp1 ), 9, 6 ) + '"' + Chr( 10 ) +;
                "#define _HBMK_BUILD_TIMESTAMP_ " + '"' +         hb_TToS( tmp1 )         + '"' + Chr( 10 )
         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating timestamp header: %1$s" ), l_cTSHEAD ) )
         ENDIF
         hb_MemoWrit( l_cTSHEAD, tmp )
      ENDIF
   ENDIF

   /* Header paths */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ]
      convert_incpaths_to_options( hbmk, cOptIncMask, lCHD_Comp )
   ENDIF

   /* Do header detection and create incremental file list for .c files */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! lDumpInfo

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
         l_aC_TODO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aC ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C %1$s %2$s", tmp, FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .T., cBin_CompC ) ) .OR. ;
               hb_FSize( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) == 0
               AAdd( l_aC_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aC_TODO := AClone( hbmk[ _HBMK_aC ] )
      ENDIF
   ENDIF

   /* Do header detection and create incremental file list for .cpp files */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! lDumpInfo

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
         l_aCPP_TODO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aCPP ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C++ %1$s %2$s", tmp, FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .T., cBin_CompCPP ) ) .OR. ;
               hb_FSize( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) == 0
               AAdd( l_aCPP_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aCPP_TODO := AClone( hbmk[ _HBMK_aCPP ] )
      ENDIF
   ENDIF

   /* Create incremental file list for .prg files */

   IF ( ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C ) .OR. ;
      ( hbmk[ _HBMK_lCreatePPO ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] ) /* or in preprocessor mode */

      IF ! lDumpInfo
         PlugIn_Execute_All( hbmk, "pre_prg" )
      ENDIF

      /* Incremental */

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ] .AND. ! lDumpInfo
         IF hbmk[ _HBMK_lCreatePPO ] .AND. hbmk[ _HBMK_lStopAfterHarbour ] /* .ppo files are the dependents in preprocessor mode */
            cHarbourOutputExt := ".ppo"
            cHarbourOutputDir := cHarbourPPODir
         ELSE
            cHarbourOutputExt := ".c"
            cHarbourOutputDir := hbmk[ _HBMK_cWorkDir ]
         ENDIF
         l_aPRG_TODO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
            IF LEFTEQUAL( tmp, "@" ) .AND. Lower( FNameExtGet( tmp ) ) == ".clp"
               tmp3 := SubStr( tmp, 2 )
            ELSE
               tmp3 := tmp
            ENDIF
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: PRG %1$s %2$s",;
                  tmp3, FNameDirExtSet( tmp3, cHarbourOutputDir, cHarbourOutputExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp3, cHarbourOutputDir, cHarbourOutputExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp3, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .F., cBin_CompC ) )
               AAdd( l_aPRG_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         IF ! Empty( hbmk[ _HBMK_hAUTOHBC ] )
            FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
               FindNewerHeaders( hbmk, tmp, NIL, .F., cBin_CompC )
            NEXT
         ENDIF

         l_aPRG_TODO := hbmk[ _HBMK_aPRG ]
      ENDIF

      IF ! Empty( hbmk[ _HBMK_hAUTOHBCFOUND ] )
         FOR EACH tmp IN hbmk[ _HBMK_hAUTOHBCFOUND ]
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing (triggered by '%1$s' header): %2$s" ), tmp:__enumKey(), tmp ) )
            ENDIF
            HBC_ProcessOne( hbmk, tmp, 1 )
         NEXT

         convert_incpaths_to_options( hbmk, cOptIncMask, lCHD_Comp )
      ENDIF
   ELSE
      l_aPRG_TODO := hbmk[ _HBMK_aPRG ]
   ENDIF

   /* Dump hbmk2 build information */

   IF lDumpInfo

      OutStd( "{{{" + hb_eol() )
      OutStd( "platform{{" + hbmk[ _HBMK_cPLAT ] + "}}" + hb_eol() )
      OutStd( "compiler{{" + hbmk[ _HBMK_cCOMP ] + "}}" + hb_eol() )
      OutStd( "cpu{{" + hbmk[ _HBMK_cCPU ] + "}}" + hb_eol() )
      OutStd( "buildname{{" + hbmk[ _HBMK_cBUILD ] + "}}" + hb_eol() )
      IF ! Empty( hbmk[ _HBMK_cPROGNAME ] )
         OutStd( "outputname{{" + PathSepToForward( hbmk[ _HBMK_cPROGNAME ] ) + "}}" + hb_eol() )
      ENDIF
      OutStd( "targetname{{" + hbmk_TARGETNAME( hbmk ) + "}}" + hb_eol() )
      OutStd( "targettype{{" + hbmk_TARGETTYPE( hbmk ) + "}}" + hb_eol() )
      OutStd( "dynsuffix{{" + hbmk_DYNSUFFIX( hbmk ) + "}}" + hb_eol() )
      OutStd( "inc{{" + iif( hbmk[ _HBMK_lINC ], "yes", "no" ) + "}}" + hb_eol() )

      OutStd( "hbctree{{" + hb_eol() )
      FOR EACH tmp IN hbmk[ _HBMK_aDEPTHBC ]
         OutStd( Replicate( Chr( 9 ), tmp[ 2 ] ) + PathSepToForward( PathNormalize( tmp[ 1 ] ) ) + hb_eol() )
      NEXT
      OutStd( "}}" + hb_eol() )
      OutStd( "}}}" + hb_eol() )

      RETURN _ERRLEV_OK
   ENDIF

   /* Check if we've found all dependencies */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]
      IF ! dep_evaluate( hbmk )
         IF hbmk[ _HBMK_lBEEP ]
            DoBeep( .F. )
         ENDIF
         RETURN _ERRLEV_MISSDEPT
      ENDIF
   ENDIF

   /* Creating implibs requested in dependency specification */

   IF ! hbmk[ _HBMK_lStopAfterInit ] .AND. hbmk[ _HBMK_lDEPIMPLIB ] .AND. ISBLOCK( bBlk_ImpLib )
      FOR EACH tmp IN hbmk[ _HBMK_hDEP ]
         IF tmp[ _HBMKDEP_lFound ] .AND. ! Empty( tmp[ _HBMKDEP_aIMPLIBSRC ] )
            DoIMPLIB( hbmk, bBlk_ImpLib, cLibLibPrefix, cLibLibExt, tmp[ _HBMKDEP_aIMPLIBSRC ], tmp[ _HBMKDEP_cIMPLIBDST ], "depimplib" )
         ENDIF
      NEXT
   ENDIF

   /* Harbour compilation */

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. Len( l_aPRG_TODO ) > 0 .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, I_( "Compiling Harbour sources..." ) )
      ENDIF

      IF ! Empty( hbmk[ _HBMK_cPO ] )
         AAdd( hbmk[ _HBMK_aOPTPRG ], "-j" )
      ENDIF

      PlatformPRGFlags( hbmk, hbmk[ _HBMK_aOPTPRG ] )

      IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE

         /* Use integrated compiler */

         aThreads := {}
         FOR EACH aTODO IN ArraySplit( l_aPRG_TODO, l_nJOBS )
            aCommand := ArrayAJoin( { { iif( hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ], "-n1", "-n2" ) },;
                                      aTODO,;
                                      iif( hbmk[ _HBMK_lBLDFLGP ], { hb_Version( HB_VERSION_FLAG_PRG ) }, {} ),;
                                      ListToArray( iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) ),;
                                      hbmk[ _HBMK_aOPTPRG ] } )

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  IF Len( aTODO:__enumBase() ) > 1
                     hbmk_OutStd( hbmk, hb_StrFormat( I_( "Harbour compiler command (embedded) job #%1$s:" ), hb_ntos( aTODO:__enumIndex() ) ) )
                  ELSE
                     hbmk_OutStd( hbmk, I_( "Harbour compiler command (embedded):" ) )
                  ENDIF
               ENDIF
               OutStd( "(" + FNameEscape( DirAddPathSep( hb_DirBase() ) + cBin_CompPRG + cBinExt + ")", hbmk[ _HBMK_nCmd_Esc ] ) +;
                       " " + ArrayToList( aCommand ) + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ]
               IF hb_mtvm() .AND. Len( aTODO:__enumBase() ) > 1
                  AAdd( aThreads, { hb_threadStart( @hb_compile(), "harbour", aCommand ), aCommand } )
               ELSE
                  IF ( tmp := hb_compile( "harbour", aCommand ) ) != 0
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler (embedded). %1$s" ), hb_ntos( tmp ) ) )
                     IF ! hbmk[ _HBMK_lQuiet ]
                        OutErr( "(" + FNameEscape( DirAddPathSep( hb_DirBase() ) + cBin_CompPRG + cBinExt + ")", hbmk[ _HBMK_nCmd_Esc ] ) +;
                                " " + ArrayToList( aCommand ) + _OUT_EOL )
                     ENDIF
                     IF ! hbmk[ _HBMK_lIGNOREERROR ]
                        IF lDeleteWorkDir
                           hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                        ENDIF
                        IF hbmk[ _HBMK_lBEEP ]
                           DoBeep( .F. )
                        ENDIF
                        RETURN _ERRLEV_COMPPRG
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
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler job #%1$s. %2$s" ), hb_ntos( thread:__enumIndex() ), hb_ntos( tmp ) ) )
                  ELSE
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler. %1$s" ), hb_ntos( tmp ) ) )
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
                     RETURN _ERRLEV_COMPPRG
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ELSE
         /* Use external compiler */

         cCommand := FNameEscape( DirAddPathSep( PathSepToSelf( l_cHB_INSTALL_BIN ) ) + cBin_CompPRG + cBinExt, hbmk[ _HBMK_nCmd_Esc ] ) +;
                     " " + iif( hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ], "-n1", iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_NATIVE, "-n", "-n2" ) ) +;
                     " " + ArrayToList( l_aPRG_TODO,, hbmk[ _HBMK_nCmd_Esc ] ) +;
                     iif( hbmk[ _HBMK_lBLDFLGP ], " " + hb_Version( HB_VERSION_FLAG_PRG ), "" ) +;
                     iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                     iif( ! Empty( hbmk[ _HBMK_aOPTPRG ] ), " " + ArrayToList( hbmk[ _HBMK_aOPTPRG ] ), "" )

         cCommand := AllTrim( cCommand )

         IF hbmk[ _HBMK_lTRACE ]
            IF ! hbmk[ _HBMK_lQuiet ]
               hbmk_OutStd( hbmk, I_( "Harbour compiler command:" ) )
            ENDIF
            OutStd( cCommand + _OUT_EOL )
         ENDIF

         IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler. %1$s" ), hb_ntos( tmp ) ) )
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
               RETURN _ERRLEV_COMPPRG
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ! lSkipBuild .AND. ! hbmk[ _HBMK_lStopAfterInit ] .AND. ! hbmk[ _HBMK_lStopAfterHarbour ]

      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

         /* Do entry function detection on platform required and supported */
         IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ! lStopAfterCComp .AND. l_cMAIN == NIL
            tmp := iif( HBMK_IS_IN( Lower( FNameExtGet( hbmk[ _HBMK_cFIRST ] ) ), ".prg|.hbs|.clp" ) .OR. Empty( FNameExtGet( hbmk[ _HBMK_cFIRST ] ) ), FNameDirExtSet( hbmk[ _HBMK_cFIRST ], hbmk[ _HBMK_cWorkDir ], ".c" ), hbmk[ _HBMK_cFIRST ] )
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
                ! Empty( hbmk[ _HBMK_aLIBUSERGT ] ) .OR. ;
                hbmk[ _HBMK_cGT ] != NIL .OR. ;
                l_cCMAIN != NIL ) ) .OR. lHBMAINDLLP

            l_cCSTUB := DirAddPathSep( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkaut.c"

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
               CASE HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|cygwin" ) /* TOFIX: cygwin is now a platform */
                  tmp := "__attribute__ (( dllimport ))"
               CASE HBMK_ISCOMP( "bcc|watcom" )
                  tmp := "__declspec( dllimport )"
               OTHERWISE
                  tmp := "_declspec( dllimport )"
               ENDCASE

               /* Create list of requested symbols */
               array := {}
               IF l_cMAIN != NIL
                  /* NOTE: Request this function to generate link error, rather
                           than starting with the wrong (default) function. */
                  AAdd( array, Upper( iif( Left( l_cMAIN, 1 ) == "@", SubStr( l_cMAIN, 2 ), l_cMAIN ) ) )
               ENDIF
               IF hbmk[ _HBMK_cGT ] != NIL
                  /* Always request default GT first */
                  AAdd( array, "HB_GT_" + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) )
               ENDIF
               IF ! Empty( hbmk[ _HBMK_aLIBUSERGT ] )
                  AEval( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | AAdd( array, "HB_GT_" + Upper( SubStr( tmp, 3 ) ) ) } )
               ENDIF

               /* Build C stub */
               /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
               cFile := '/* This temp source file was generated by hbmk2 tool. */'                + Chr( 10 ) +;
                        '/* You can safely delete it. */'                                         + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        '#include "hbapi.h"'                                                      + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 )

               IF ( ! Empty( array ) .OR. l_cCMAIN != NIL ) .AND. ! lHBMAINDLLP
                  AEval( array, {| tmp, i | array[ i ] := FuncNameEncode( tmp ) } )
                  AEval( array, {| tmp | cFile += 'HB_FUNC_EXTERN( ' + tmp + ' );'                + Chr( 10 ) } )
                  IF l_cCMAIN != NIL
                     IF ! Empty( array )
                        cFile += ''                                                               + Chr( 10 )
                     ENDIF
                     cFile += 'HB_EXTERN_BEGIN'                                                   + Chr( 10 ) +;
                              'void ' + l_cCMAIN + '( void );'                                    + Chr( 10 ) +;
                              'HB_EXTERN_END'                                                     + Chr( 10 )
                  ENDIF
                  cFile += ''                                                                     + Chr( 10 )
                  cFile += 'void _hb_lnk_ForceLink_hbmk2( void )'                                 + Chr( 10 )
                  cFile += '{'                                                                    + Chr( 10 )
                  AEval( array, {| tmp | cFile += '   HB_FUNC_EXEC( ' + tmp + ' );'               + Chr( 10 ) } )
                  IF l_cCMAIN != NIL
                     IF ! Empty( array )
                        cFile += ''                                                               + Chr( 10 )
                     ENDIF
                     cFile += '   ' + l_cCMAIN + '();'                                            + Chr( 10 )
                  ENDIF
                  cFile += '}'                                                                    + Chr( 10 )
                  cFile += ''                                                                     + Chr( 10 )
               ENDIF

               IF lHBMAINDLLP .and. .F.
                  cFile += 'HB_EXPORT_ATTR PHB_FUNC dll_hb_vmProcAddress( const char * szFuncName )' + Chr( 10 )
                  cFile += '{'                                                                       + Chr( 10 )
                  cFile += '   return hb_vmProcAddress( szFuncName );'                               + Chr( 10 )
                  cFile += '}'                                                                       + Chr( 10 )
                  cFile += ''                                                                        + Chr( 10 )
               ENDIF

               IF hbmk[ _HBMK_cGT ] != NIL .OR. ;
                  l_cMAIN != NIL
                  IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                     _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
                     cFile += '#include "hbinit.h"'                                                  + Chr( 10 ) +;
                              ''                                                                     + Chr( 10 ) +;
                              'HB_EXTERN_BEGIN'                                                      + Chr( 10 ) +;
                              'extern ' + tmp + ' const char * s_defaultGT;'                         + Chr( 10 ) +;
                              'extern ' + tmp + ' const char * s_pszLinkedMain;'                     + Chr( 10 ) +;
                              'HB_EXTERN_END'                                                        + Chr( 10 ) +;
                              ''                                                                     + Chr( 10 ) +;
                              'HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )'                         + Chr( 10 )
                  ELSE
                     cFile += '#include "hbinit.h"'                                                  + Chr( 10 ) +;
                              ''                                                                     + Chr( 10 ) +;
                              'HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )'                         + Chr( 10 )
                  ENDIF
                  IF hbmk[ _HBMK_cGT ] != NIL
                     IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                        _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
                        cFile += '   s_defaultGT = "' + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) + '";'           + Chr( 10 )
                     ELSE
                        cFile += '   hb_vmSetDefaultGT( "' + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) + '" );'    + Chr( 10 )
                     ENDIF
                  ENDIF
                  IF l_cMAIN != NIL
                     IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                        _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
                        cFile += '   s_pszLinkedMain = "' + Upper( l_cMAIN ) + '";'                  + Chr( 10 )
                     ELSE
                        cFile += '   hb_vmSetLinkedMain( "' + Upper( l_cMAIN ) + '" );'              + Chr( 10 )
                     ENDIF
                  ENDIF
                  cFile += 'HB_CALL_ON_STARTUP_END( _hb_hbmk_setdef_ )'                           + Chr( 10 ) +;
                           ''                                                                     + Chr( 10 ) +;
                           '#if defined( HB_PRAGMA_STARTUP )'                                     + Chr( 10 ) +;
                           '   #pragma startup _hb_hbmk_setdef_'                                  + Chr( 10 ) +;
                           '#elif defined( HB_DATASEG_STARTUP )'                                  + Chr( 10 ) +;
                           '   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_hbmk_setdef_ )'    + Chr( 10 ) +;
                           '   #include "hbiniseg.h"'                                             + Chr( 10 ) +;
                           '#endif'                                                               + Chr( 10 )
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
                  AAdd( l_aC_TODO, l_cCSTUB )
               ELSE
                  hbmk_OutErr( hbmk, I_( "Warning: Stub helper .c program could not be created." ) )
                  IF ! hbmk[ _HBMK_lINC ]
                     AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".c", .T. ), {| tmp | FErase( tmp ) } )
                  ENDIF
                  IF lDeleteWorkDir
                     hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                  ENDIF
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _ERRLEV_STUBCREATE
               ENDIF
               /* Don't delete stub in workdir in incremental mode. */
               IF hbmk[ _HBMK_lINC ]
                  l_cCSTUB := NIL
               ENDIF
            ENDIF
         ENDIF

         /* HACK: Override memory allocation functions for apps that request it. */
         IF ! lStopAfterCComp .AND. ;
            ! Empty( cBin_CompCPP ) .AND. ;
            hbmk[ _HBMK_lHBCPPMM ]

            l_cCPPSTUB := DirAddPathSep( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkcpp.cpp"

            IF ! hbmk[ _HBMK_lCLEAN ]

               /* Build C++ stub */
               /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
               cFile := '/* This temp source file was generated by hbmk2 tool. */'                + Chr( 10 ) +;
                        '/* You can safely delete it. */'                                         + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        '#include "hbapi.h"'                                                      + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        '#if defined( __cplusplus )'                                              + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'const char * __hbmk2_hbcppmm( void )'                                    + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   return "HBCPPMM";'                                                    + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'void * operator new[]( size_t nSize )'                                   + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   if( nSize == 0 )'                                                     + Chr( 10 ) +;
                        '      nSize = 1;'                                                        + Chr( 10 ) +;
                        '   return hb_xgrab( nSize );'                                            + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'void * operator new( size_t nSize )'                                     + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   if( nSize == 0 )'                                                     + Chr( 10 ) +;
                        '      nSize = 1;'                                                        + Chr( 10 ) +;
                        '   return hb_xgrab( nSize );'                                            + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'void operator delete[]( void * ptr )'                                    + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   if( ptr )'                                                            + Chr( 10 ) +;
                        '      hb_xfree( ptr );'                                                  + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'void operator delete[]( void * ptr, size_t )'                            + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   if( ptr )'                                                            + Chr( 10 ) +;
                        '      hb_xfree( ptr );'                                                  + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'void operator delete( void * ptr )'                                      + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   if( ptr )'                                                            + Chr( 10 ) +;
                        '      hb_xfree( ptr );'                                                  + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        'void operator delete( void * ptr, size_t )'                              + Chr( 10 ) +;
                        '{'                                                                       + Chr( 10 ) +;
                        '   if( ptr )'                                                            + Chr( 10 ) +;
                        '      hb_xfree( ptr );'                                                  + Chr( 10 ) +;
                        '}'                                                                       + Chr( 10 ) +;
                        ''                                                                        + Chr( 10 ) +;
                        '#endif'                                                                  + Chr( 10 )

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
                  AAdd( l_aCPP_TODO, l_cCPPSTUB )
               ELSE
                  hbmk_OutErr( hbmk, I_( "Warning: Stub helper .cpp program could not be created." ) )
                  IF ! hbmk[ _HBMK_lINC ]
                     AEval( ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".c", .T. ), {| tmp | FErase( tmp ) } )
                  ENDIF
                  IF lDeleteWorkDir
                     hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
                  ENDIF
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( .F. )
                  ENDIF
                  RETURN _ERRLEV_STUBCREATE
               ENDIF
               /* Don't delete stub in workdir in incremental mode. */
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
            /* Don't link Harbour dynamic/static libs when in '-hbdyn -shared' mode */
            IF !( hbmk[ _HBMK_lCreateDyn ] .AND. ! hbmk[ _HBMK_lDynVM ] )
               l_aLIBHB := AClone( l_aLIBSHAREDPOST )
               /* NOTE: Make sure to add these static libs only if they can be found.
                        This will ensure that hbmk2 can be used to build shared mode binaries
                        even when static libs are not installed (typically on *nix systems).
                        [vszakats] */
               FOR EACH tmp IN ArrayAJoin( { aLIB_BASE_CPLR,;
                                             aLIB_BASE_DEBUG } )
                  IF hb_FileExists( _HBLIB_FULLPATH( tmp ) )
                     AAdd( l_aLIBHB, tmp )
                  ENDIF
               NEXT
            ELSE
               l_aLIBHB := {}
            ENDIF
         ELSE
            l_aLIBHB := ArrayAJoin( { aLIB_BASE_EXTERN,;
                                      aLIB_BASE_DEBUG,;
                                      iif( hbmk[ _HBMK_lMT ], aLIB_BASE_1_MT, aLIB_BASE_1 ),;
                                      hbmk[ _HBMK_aLIBCOREGT ],;
                                      iif( hbmk[ _HBMK_lNULRDD ], aLIB_BASE_NULRDD, iif( hbmk[ _HBMK_lMT ], aLIB_BASE_RDD_MT, aLIB_BASE_RDD ) ),;
                                      l_aLIBHBBASE_2,;
                                      iif( hbmk[ _HBMK_lMT ], aLIB_BASE_3_MT, aLIB_BASE_3 ),;
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
         FOR EACH tmp IN hbmk[ _HBMK_aLIBUSER ] DESCEND
            IF Lower( tmp ) == Lower( l_cLIBSELF )
               hb_ADel( hbmk[ _HBMK_aLIBUSER ], tmp:__enumIndex(), .T. )
            ENDIF
         NEXT
      ENDIF

      /* Merge lib lists. */
      l_aLIBRAW := ArrayAJoin( { hbmk[ _HBMK_aLIBUSER ], l_aLIBHB, l_aLIB3RD, hbmk[ _HBMK_aLIBUSERSYSPRE ], l_aLIBSYS, hbmk[ _HBMK_aLIBUSERSYS ] } )
      /* Dress lib names. */
      l_aLIB := {}
      l_aLIBA := {}
      ListCookLib( hbmk, l_aLIB, l_aLIBA, l_aLIBRAW, NIL, cLibExt )
      IF hbmk[ _HBMK_lSHARED ] .AND. ! Empty( l_aLIBSHARED )
         /* Don't link Harbour dynamic/static libs when in '-hbdyn -shared' mode */
         IF !( hbmk[ _HBMK_lCreateDyn ] .AND. ! hbmk[ _HBMK_lDynVM ] ) .OR. lHBMAINDLLP
            l_aLIBRAW := ArrayJoin( l_aLIBSHARED, l_aLIBRAW )
            ListCookLib( hbmk, l_aLIB, l_aLIBA, l_aLIBSHARED, NIL )
         ENDIF
      ENDIF
      /* Dress obj names. */
      IF cObjExt == NIL
         /* NOTE: May only happen if the plat/comp combination isn't supported.
                  Don't let the obj filelist be the exact same as the source list,
                  it would cause unwanted deletion of source at cleanup stage.
                  [vszakats] */
         l_aOBJ := {}
      ELSE
         l_aOBJ := ListDirExt( ArrayAJoin( { hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_aC ], hbmk[ _HBMK_aCPP ] } ), hbmk[ _HBMK_cWorkDir ], cObjExt, .T. )
      ENDIF
      hbmk[ _HBMK_aOBJUSER ] := ListCook( hbmk[ _HBMK_aOBJUSER ], cObjExt )

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
         l_aRESSRC_TODO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aRESSRC ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: RESSRC %1$s %2$s", tmp, FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, tmp2, .T., cBin_CompC ) ) .OR. ;
               hb_FSize( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ) ) == 0
               AAdd( l_aRESSRC_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aRESSRC_TODO := AClone( hbmk[ _HBMK_aRESSRC ] )
      ENDIF

      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C
         IF hbmk[ _HBMK_lREBUILDPO ]
            IF ! Empty( hbmk[ _HBMK_cPO ] ) .AND. ! Empty( hbmk[ _HBMK_aPRG ] )
               RebuildPO( hbmk, ListDirExt( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_cWorkDir ], ".pot", .T. ) )
            ENDIF
         ELSE
            IF ! Empty( hbmk[ _HBMK_cPO ] ) .AND. Len( l_aPRG_TODO ) > 0
               UpdatePO( hbmk, ListDirExt( l_aPRG_TODO, hbmk[ _HBMK_cWorkDir ], ".pot", .T. ) )
            ENDIF
         ENDIF

         IF Len( hbmk[ _HBMK_aPO ] ) > 0 .AND. hbmk[ _HBMK_cHBL ] != NIL .AND. ! hbmk[ _HBMK_lCLEAN ]

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

      IF HBMK_ISPLAT( "win|wce|os2" ) .AND. ;
         ! Empty( hbmk[ _HBMK_aICON ] )

         l_cRESSTUB := DirAddPathSep( hbmk[ _HBMK_cWorkDir ] ) + "_hbmkaut.rc"

         IF ! hbmk[ _HBMK_lCLEAN ]
            /* Build .rc stub */
            /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
            cFile := '/* This temp source file was generated by hbmk2 tool. */'                + Chr( 10 ) +;
                     '/* You can safely delete it. */'                                         + Chr( 10 ) +;
                     ''                                                                        + Chr( 10 )
            IF hbmk[ _HBMK_cPLAT ] == "os2"
               AEval( hbmk[ _HBMK_aICON ], {| tmp, tmp1 | cFile += 'ICON ' + hb_ntos( tmp1 ) + ' DISCARDABLE "' + PathSepToForward( tmp ) + '"' + Chr( 10 ) } )
            ELSE
               AEval( hbmk[ _HBMK_aICON ], {| tmp, tmp1 | cFile += hb_ntos( tmp1 ) + ' ICON DISCARDABLE "' + PathSepToForward( tmp ) + '"' + Chr( 10 ) } )
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
               AAdd( l_aRESSRC_TODO, l_cRESSTUB )
            ELSE
               hbmk_OutErr( hbmk, I_( "Warning: Stub helper .rc file could not be created." ) )
            ENDIF
            /* Don't delete stub in workdir in incremental mode. */
            IF hbmk[ _HBMK_lINC ]
               l_cRESSTUB := NIL
            ENDIF
         ENDIF
      ENDIF

      /* Avoid this list being added at link phase if resource compiling isn't available
         in target compiler. */
      IF Empty( cBin_Res )
         ASize( hbmk[ _HBMK_aRESSRC ], 0 )
      ENDIF

      IF Len( l_aRESSRC_TODO ) > 0 .AND. ! Empty( cBin_Res ) .AND. ! hbmk[ _HBMK_lCLEAN ]

         PlugIn_Execute_All( hbmk, "pre_res" )

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, I_( "Compiling resources..." ) )
         ENDIF

         /* Compiling resource */

         nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Res, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
         nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Res, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

         cOpt_Res := StrTran( cOpt_Res, "{FR}"  , GetEnv( "HB_USER_RESFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTRES ] ) )
         cOpt_Res := StrTran( cOpt_Res, "{DI}"  , FNameEscape( l_cHB_INSTALL_INC, nOpt_Esc, nOpt_FNF ) )

         IF "{IR}" $ cOpt_Res

            FOR EACH tmp IN l_aRESSRC_TODO

               cCommand := cOpt_Res
               cCommand := StrTran( cCommand, "{IR}", FNameEscape( tmp, nOpt_Esc, nOpt_FNF ) )
               cCommand := StrTran( cCommand, "{OS}", FNameEscape( FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cResExt ), nOpt_Esc, nOpt_FNF ) )

               cCommand := cBin_Res + " " + AllTrim( cCommand )

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Resource compiler command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hb_processRun( cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running resource compiler. %1$s" ), hb_ntos( tmp1 ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_RUNRES
                     EXIT
                  ENDIF
               ENDIF
            NEXT
         ELSE
            cOpt_Res := StrTran( cOpt_Res, "{LR}"  , ArrayToList( l_aRESSRC_TODO,, nOpt_Esc, nOpt_FNF ) )

            cOpt_Res := AllTrim( cOpt_Res )

            /* Handle moving the whole command line to a script, if requested. */
            cScriptFile := NIL
            IF "{SCRIPT}" $ cOpt_Res
               fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
               IF fhnd != F_ERROR
                  FWrite( fhnd, StrTran( cOpt_Res, "{SCRIPT}" ) )
                  FClose( fhnd )
                  cOpt_Res := "@" + cScriptFile
               ELSE
                  hbmk_OutErr( hbmk, I_( "Warning: Resource compiler script could not be created, continuing in command line." ) )
               ENDIF
            ENDIF

            cCommand := cBin_Res + " " + cOpt_Res

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, I_( "Resource compiler command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
               IF ! Empty( cScriptFile )
                  hbmk_OutStd( hbmk, I_( "Resource compiler script:" ) )
                  OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
               ENDIF
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running resource compiler. %1$s" ), hb_ntos( tmp ) ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + _OUT_EOL )
               ENDIF
               IF ! hbmk[ _HBMK_lIGNOREERROR ]
                  hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_RUNRES
               ENDIF
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF
         ENDIF
      ENDIF

      IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
            l_aPRG_TODO := {}

            FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
               IF LEFTEQUAL( tmp, "@" ) .AND. Lower( FNameExtGet( tmp ) ) == ".clp"
                  tmp3 := SubStr( tmp, 2 )
               ELSE
                  tmp3 := tmp
               ENDIF
               IF hbmk[ _HBMK_lDEBUGINC ]
                  hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CPRG %1$s %2$s",;
                     FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], ".c" ),;
                     FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) )
               ENDIF
               IF ! hb_FGetDateTime( FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], ".c" ), @tmp1 ) .OR. ;
                  ! hb_FGetDateTime( FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], cObjExt ), @tmp2 ) .OR. ;
                  tmp1 > tmp2 .OR. ;
                  hb_FSize( FNameDirExtSet( tmp3, hbmk[ _HBMK_cWorkDir ], cObjExt ) ) == 0
                  AAdd( l_aPRG_TODO, tmp )
               ENDIF
            NEXT
         ELSE
            l_aPRG_TODO := hbmk[ _HBMK_aPRG ]
         ENDIF
      ENDIF

      PlugIn_Execute_All( hbmk, "pre_c" )

      IF ! hbmk[ _HBMK_lCLEAN ]

         FOR EACH tmp3 IN { _CCOMP_PASS_C, _CCOMP_PASS_CPP }

            IF tmp3 == _CCOMP_PASS_C
               l_aCGEN_TODO := ArrayJoin( ListDirExt( l_aPRG_TODO, hbmk[ _HBMK_cWorkDir ], ".c", .T. ), l_aC_TODO )
               cBin_CompCGEN := cBin_CompC
            ELSE
               l_aCGEN_TODO := AClone( l_aCPP_TODO )
               cBin_CompCGEN := cBin_CompCPP
            ENDIF

            IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK .AND. Len( l_aCGEN_TODO ) > 0

               IF ! Empty( cBin_CompCGEN )

                  IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
                     IF tmp3 == _CCOMP_PASS_C
                        hbmk_OutStd( hbmk, I_( "Compiling..." ) )
                     ELSE
                        hbmk_OutStd( hbmk, I_( "Compiling C++..." ) )
                     ENDIF
                  ENDIF

                  /* Compiling */

                  nOpt_Esc := iif( "{SCRIPT}" $ cOpt_CompC, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
                  nOpt_FNF := iif( "{SCRIPT}" $ cOpt_CompC, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

                  /* Order is significant */
                  cOpt_CompC := StrTran( cOpt_CompC, "{FC}"  , iif( hbmk[ _HBMK_lBLDFLGC ], hb_Version( HB_VERSION_FLAG_C ) + " ", "" ) +;
                                                               GetEnv( "HB_USER_CFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTC ] ) )
                  cOpt_CompC := StrTran( cOpt_CompC, "{OD}"  , FNameEscape( FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ), nOpt_Esc, nOpt_FNF ) )
                  cOpt_CompC := StrTran( cOpt_CompC, "{DI}"  , FNameEscape( l_cHB_INSTALL_INC, nOpt_Esc, nOpt_FNF ) )

                  IF "{IC}" $ cOpt_CompC

                     aThreads := {}
                     FOR EACH aTODO IN ArraySplit( l_aCGEN_TODO, l_nJOBS )
                        IF hb_mtvm() .AND. Len( aTODO:__enumBase() ) > 1
                           AAdd( aThreads, hb_threadStart( @CompileCLoop(), hbmk, aTODO, cBin_CompCGEN, cOpt_CompC, cObjExt, nOpt_Esc, nOpt_FNF, aTODO:__enumIndex(), Len( aTODO:__enumBase() ) ) )
                        ELSE
                           IF ! CompileCLoop( hbmk, aTODO, cBin_CompCGEN, cOpt_CompC, cObjExt, nOpt_Esc, nOpt_FNF, 0, 0 )
                              IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                 hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_COMPC
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
                                 hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_COMPC
                              ENDIF
                           ENDIF
                        NEXT
                     ENDIF
                  ELSE
                     cOpt_CompC := StrTran( cOpt_CompC, "{OO}"  , FNameEscape( FNameExtSet( hbmk[ _HBMK_cPROGNAME ], cObjExt ), nOpt_Esc, nOpt_FNF ) )
                     cOpt_CompC := StrTran( cOpt_CompC, "{OW}"  , FNameEscape( hbmk[ _HBMK_cWorkDir ], nOpt_Esc, nOpt_FNF ) )

                     IF lCHD_Comp
                        tmp2 := DirAddPathSep( PathMakeRelative( PathNormalize( PathMakeAbsolute( hbmk[ _HBMK_cWorkDir ], hb_pwd() ) ), hb_pwd(), .T. ) )
                        IF hbmk[ _HBMK_lDONTEXEC ]
                           cCHD_DirOld := NIL
                        ELSE
                           cCHD_DirOld := hb_pwd()
                           IF hbmk[ _HBMK_lTRACE ] .AND. hbmk[ _HBMK_lInfo ]
                              hbmk_OutStd( hbmk, hb_StrFormat( I_( "'cd' to: %1$s" ), hbmk[ _HBMK_cWorkDir ] ) )
                           ENDIF
                           DirChange( hbmk[ _HBMK_cWorkDir ] )
                        ENDIF
                     ENDIF

                     aThreads := {}
                     FOR EACH aTODO IN ArraySplit( l_aCGEN_TODO, l_nJOBS )

                        IF lCHD_Comp
                           /* Convert source filenames relative to the target dir */
                           tmp := AClone( aTODO )
                           FOR EACH tmp1 IN tmp
                              tmp1 := PathNormalize( PathMakeAbsolute( tmp1, tmp2 ) )
                           NEXT
                           cOpt_CompCLoop := AllTrim( StrTran( cOpt_CompC, "{LC}"  , ArrayToList( tmp,, nOpt_Esc, nOpt_FNF ) ) )
                        ELSE
                           cOpt_CompCLoop := AllTrim( StrTran( cOpt_CompC, "{LC}"  , ArrayToList( aTODO,, nOpt_Esc, nOpt_FNF ) ) )
                        ENDIF

                        /* Handle moving the whole command line to a script, if requested. */
                        cScriptFile := NIL
                        IF "{SCRIPT}" $ cOpt_CompCLoop
                           fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".cpl" )
                           IF fhnd != F_ERROR
                              FWrite( fhnd, StrTran( cOpt_CompCLoop, "{SCRIPT}" ) )
                              FClose( fhnd )
                              cOpt_CompCLoop := "@" + cScriptFile
                           ELSE
                              hbmk_OutErr( hbmk, I_( "Warning: C/C++ compiler script could not be created, continuing in command line." ) )
                           ENDIF
                        ENDIF

                        cCommand := cBin_CompCGEN + " " + cOpt_CompCLoop

                        IF hbmk[ _HBMK_lTRACE ]
                           IF ! hbmk[ _HBMK_lQuiet ]
                              IF Len( aTODO:__enumBase() ) > 1
                                 hbmk_OutStd( hbmk, hb_StrFormat( I_( "C/C++ compiler command job #%1$s:" ), hb_ntos( aTODO:__enumIndex() ) ) )
                              ELSE
                                 hbmk_OutStd( hbmk, I_( "C/C++ compiler command:" ) )
                              ENDIF
                           ENDIF
                           OutStd( cCommand + _OUT_EOL )
                           IF ! Empty( cScriptFile )
                              hbmk_OutStd( hbmk, I_( "C/C++ compiler script:" ) )
                              OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                           ENDIF
                        ENDIF

                        IF ! hbmk[ _HBMK_lDONTEXEC ]
                           IF hb_mtvm() .AND. Len( aTODO:__enumBase() ) > 1
                              AAdd( aThreads, { hb_threadStart( @hb_processRun(), cCommand ), cCommand } )
                           ELSE
                              IF ( tmp := hb_processRun( cCommand ) ) != 0
                                 hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler. %1$s" ), hb_ntos( tmp ) ) )
                                 IF ! hbmk[ _HBMK_lQuiet ]
                                    OutErr( cCommand + _OUT_EOL )
                                 ENDIF
                                 IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                    hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_COMPC
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
                                 hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler job #%1$s. %2$s" ), hb_ntos( thread:__enumIndex() ), hb_ntos( tmp ) ) )
                              ELSE
                                 hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler. %1$s" ), hb_ntos( tmp ) ) )
                              ENDIF
                              IF ! hbmk[ _HBMK_lQuiet ]
                                 OutErr( thread[ 2 ] + _OUT_EOL )
                              ENDIF
                              IF ! hbmk[ _HBMK_lIGNOREERROR ]
                                 hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_COMPC
                              ENDIF
                           ENDIF
                        NEXT
                     ENDIF

                     IF lCHD_Comp .AND. cCHD_DirOld != NIL
                        DirChange( cCHD_DirOld )
                        IF hbmk[ _HBMK_lTRACE ] .AND. hbmk[ _HBMK_lInfo ]
                           hbmk_OutStd( hbmk, I_( "'cd' back." ) )
                        ENDIF
                     ENDIF
                  ENDIF
               ELSE
                  hbmk_OutErr( hbmk, I_( "Error: C/C++ command is not implemented for this platform/compiler." ) )
                  hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_UNSUPPORTED
               ENDIF
            ENDIF
         NEXT
      ENDIF

      IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK

         lTargetUpToDate := .F.

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]

            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: target %1$s", hbmk[ _HBMK_cPROGNAME ] ) )
            ENDIF

            IF hb_FGetDateTime( hbmk[ _HBMK_cPROGNAME ], @tTarget )

               lTargetUpToDate := .T.
               IF lTargetUpToDate
                  FOR EACH tmp IN ArrayAJoin( { l_aOBJ, hbmk[ _HBMK_aOBJUSER ], l_aOBJA, hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_aRESCMP ] } )
                     IF hbmk[ _HBMK_lDEBUGINC ]
                        hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: EXEDEP %1$s", tmp ) )
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
                           hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: EXEDEPLIB %1$s", tmp2 ) )
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

      IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK .AND. ( Len( l_aOBJ ) + Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ) ) > 0 .AND. ! hbmk[ _HBMK_lCLEAN ]

         /* Must be called before target creation to avoid errors. */
         DoLinkDelete( hbmk )

         IF lTargetUpToDate
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Target up to date: %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )

            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )
               l_lIMPLIBToProcess := .T.
            CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateDyn ] .AND. ! Empty( cBin_Dyn )
               l_lIMPLIBToProcess := .T.
            ENDCASE
         ELSE
            IF ! DirBuild( FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ) )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create directory for target '%1$s'." ), hbmk[ _HBMK_cPROGNAME ] ) )
            ENDIF

            IF hbmk[ _HBMK_lREBUILD ] .OR. ;
               ( ! hbmk[ _HBMK_lINC ] .AND. lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ] .AND. ! Empty( cBin_Lib ) ) /* non-incremental + static lib */
               IF hb_FileExists( hbmk[ _HBMK_cPROGNAME ] ) .AND. ;
                  FErase( hbmk[ _HBMK_cPROGNAME ] ) == F_ERROR
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot delete existing target '%1$s'." ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF
            ENDIF

            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )

               PlugIn_Execute_All( hbmk, "pre_link" )

               IF ( hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ] ) .OR. hbmk[ _HBMK_lInfo ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Linking... %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF

               /* Linking */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Link, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
               nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Link, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

               /* Order is significant */
               cOpt_Link := StrTran( cOpt_Link, "{FL}"  , iif( hbmk[ _HBMK_lBLDFLGL ], hb_Version( HB_VERSION_FLAG_LINKER ) + " ", "" ) +;
                                                          GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTL ] ) )
               cOpt_Link := StrTran( cOpt_Link, "{LO}"  , ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, nOpt_FNF, cObjPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{LS}"  , ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_cWorkDir ], cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, nOpt_FNF, cResPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{LA}"  , ArrayToList( l_aOBJA,, nOpt_Esc, nOpt_FNF ) )
               cOpt_Link := StrTran( cOpt_Link, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, nOpt_FNF, cLibPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc, nOpt_FNF ) )
               cOpt_Link := StrTran( cOpt_Link, "{IM}"  , ArrayToList( hbmk[ _HBMK_aDEF ],, nOpt_Esc, nOpt_FNF, cDefPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{OE}"  , FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ) )
               cOpt_Link := StrTran( cOpt_Link, "{OM}"  , FNameEscape( FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".map" ), nOpt_Esc, nOpt_FNF ) )
               cOpt_Link := StrTran( cOpt_Link, "{OI}"  , FNameEscape( l_cIMPLIBNAME, nOpt_Esc, nOpt_FNF ) )
               cOpt_Link := StrTran( cOpt_Link, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, nOpt_FNF, cLibPathPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{DB}"  , l_cHB_INSTALL_BIN )

               cOpt_Link := AllTrim( cOpt_Link )

               /* Handle moving the whole command line to a script, if requested. */
               cScriptFile := NIL
               IF "{SCRIPT}" $ cOpt_Link
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Link, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Link := "@" + cScriptFile
                  ELSE
                     hbmk_OutErr( hbmk, I_( "Warning: Link script could not be created, continuing in command line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Link + " " + cOpt_Link

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Linker command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
                  IF ! Empty( cScriptFile )
                     hbmk_OutStd( hbmk, I_( "Linker script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running linker. %1$s" ), hb_ntos( tmp ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_RUNLINKER
                  ENDIF
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

               IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK
                  l_lIMPLIBToProcess := .T.
               ENDIF

               IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK .AND. hbmk[ _HBMK_lGUI ] .AND. hbmk[ _HBMK_cPLAT ] == "darwin"
                  /* Build app bundle for OS X GUI apps. (experimental) */
                  tmp := FNameDirGet( hbmk[ _HBMK_cPROGNAME ] )
                  IF ! Empty( tmp )
                     tmp += hb_ps()
                  ENDIF
                  tmp += FNameNameGet( hbmk[ _HBMK_cPROGNAME ] ) + ".app" + hb_ps() + "Contents"
                  IF DirBuild( tmp + hb_ps() + "MacOS" )
                     hb_FCopy( hbmk[ _HBMK_cPROGNAME ], tmp + hb_ps() + "MacOS" + hb_ps() + FNameNameGet( hbmk[ _HBMK_cPROGNAME ] ) )
                     IF ! hb_FileExists( tmp + hb_ps() + "Info.plist" )
                        hb_MemoWrit( tmp + hb_ps() + "Info.plist", MacOSXFiles( hbmk, 1, FNameNameGet( hbmk[ _HBMK_cPROGNAME ] ) ) )
                     ENDIF
                     IF ! hb_FileExists( tmp + hb_ps() + "PkgInfo" )
                        hb_MemoWrit( tmp + hb_ps() + "PkgInfo", MacOSXFiles( hbmk, 2, FNameNameGet( hbmk[ _HBMK_cPROGNAME ] ) ) )
                     ENDIF
                     IF ! Empty( hbmk[ _HBMK_aICON ] )
                        IF DirBuild( tmp + hb_ps() + "Resources" )
                           FOR EACH tmp1 IN hbmk[ _HBMK_aICON ]
                              hb_FCopy( tmp1, tmp + hb_ps() + "Resources" + hb_ps() + FNameNameExtGet( tmp1 ) )
                           NEXT
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF

            CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateDyn ] .AND. ! Empty( cBin_Dyn )

               PlugIn_Execute_All( hbmk, "pre_link" )

               IF ( hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ] ) .OR. hbmk[ _HBMK_lInfo ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating dynamic library... %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF

               /* Lib creation (dynamic) */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Dyn, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
               nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Dyn, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

               /* Order is significant */
               cOpt_Dyn := StrTran( cOpt_Dyn, "{FD}"  , GetEnv( "HB_USER_DFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTD ] ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LO}"  , ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, nOpt_FNF, cDynObjPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LS}"  , ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_cWorkDir ], cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, nOpt_FNF, cResPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, nOpt_FNF, cLibPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc, nOpt_FNF ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{IM}"  , ArrayToList( hbmk[ _HBMK_aDEF ],, nOpt_Esc, nOpt_FNF, cDefPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OD}"  , FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OM}"  , FNameEscape( FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".map" ), nOpt_Esc, nOpt_FNF ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OI}"  , FNameEscape( l_cIMPLIBNAME, nOpt_Esc, nOpt_FNF ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, nOpt_FNF, cLibPathPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{DB}"  , l_cHB_INSTALL_BIN )

               cOpt_Dyn := AllTrim( cOpt_Dyn )

               /* Handle moving the whole command line to a script, if requested. */
               cScriptFile := NIL
               IF "{SCRIPT}" $ cOpt_Dyn
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Dyn, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Dyn := "@" + cScriptFile
                  ELSE
                     hbmk_OutErr( hbmk, I_( "Warning: Dynamic lib link script could not be created, continuing in command line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Dyn + " " + cOpt_Dyn

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Dynamic lib link command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
                  IF ! Empty( cScriptFile )
                     hbmk_OutStd( hbmk, I_( "Dynamic lib link script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running dynamic lib link command. %1$s" ), hb_ntos( tmp ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_RUNLINKER
                  ENDIF
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

               IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK
                  l_lIMPLIBToProcess := .T.
               ENDIF

            CASE lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ] .AND. ! Empty( cBin_Lib )

               PlugIn_Execute_All( hbmk, "pre_lib" )

               IF ( hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ] ) .OR. hbmk[ _HBMK_lInfo ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating static library... %1$s" ), hbmk[ _HBMK_cPROGNAME ] ) )
               ENDIF

               /* Lib creation (static) */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Lib, hbmk[ _HBMK_nScr_Esc ], hbmk[ _HBMK_nCmd_Esc ] )
               nOpt_FNF := iif( "{SCRIPT}" $ cOpt_Lib, hbmk[ _HBMK_nScr_FNF ], hbmk[ _HBMK_nCmd_FNF ] )

               /* Order is significant */
               cOpt_Lib := StrTran( cOpt_Lib, "{FA}"  , GetEnv( "HB_USER_AFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTA ] ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LO}"  , ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, nOpt_FNF, cLibObjPrefix ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, nOpt_FNF, cLibPrefix ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc, nOpt_FNF ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{OL}"  , FNameEscape( hbmk[ _HBMK_cPROGNAME ], nOpt_Esc, nOpt_FNF ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, nOpt_FNF, cLibPathPrefix ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{DB}"  , l_cHB_INSTALL_BIN )

               cOpt_Lib := AllTrim( cOpt_Lib )

               /* Handle moving the whole command line to a script, if requested. */
               cScriptFile := NIL
               IF "{SCRIPT}" $ cOpt_Lib
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Lib, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Lib := "@" + cScriptFile
                  ELSE
                     hbmk_OutErr( hbmk, I_( "Warning: Lib script could not be created, continuing in command line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Lib + " " + cOpt_Lib

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Lib command:" ) )
                  ENDIF
                  OutStd( cCommand + _OUT_EOL )
                  IF ! Empty( cScriptFile )
                     hbmk_OutStd( hbmk, I_( "Lib script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + _OUT_EOL )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running lib command. %1$s" ), hb_ntos( tmp ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + _OUT_EOL )
                  ENDIF
                  IF ! hbmk[ _HBMK_lIGNOREERROR ]
                     hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_RUNLIB
                  ENDIF
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            ENDCASE
         ENDIF

         IF lTargetUpToDate .OR. hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK

            IF ! Empty( hbmk[ _HBMK_cHBX ] )
               mk_extern( hbmk, hbmk[ _HBMK_cPROGNAME ], cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex, hbmk[ _HBMK_cHBX ] )
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

      /* Cleanup */

      PlugIn_Execute_All( hbmk, "pre_cleanup" )

      IF hbmk[ _HBMK_lCLEAN ]
         FErase( hbmk[ _HBMK_cPROGNAME ] )
         IF hbmk[ _HBMK_lIMPLIB ] .AND. HBMK_ISPLAT( "win|os2|dos" ) .AND. l_cIMPLIBNAME != NIL
            FErase( l_cIMPLIBNAME )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            FErase( FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".map" ) )
         ENDIF
         IF lStopAfterCComp .AND. hbmk[ _HBMK_lCreateLib ]
            /* bcc is known to create it for static libs */
            FErase( FNameExtSet( hbmk[ _HBMK_cPROGNAME ], ".bak" ) )
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
      IF ! lStopAfterCComp .OR. hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ]
         IF ! hbmk[ _HBMK_lINC ] .OR. hbmk[ _HBMK_lCLEAN ]
            IF ! Empty( cResExt )
               AEval( ListDirExt( hbmk[ _HBMK_aRESSRC ], hbmk[ _HBMK_cWorkDir ], cResExt ), {| tmp | FErase( tmp ) } )
            ENDIF
            AEval( l_aOBJ, {| tmp | FErase( tmp ) } )
         ENDIF
      ENDIF
      AEval( l_aCLEAN, {| tmp | FErase( tmp ) } )
      IF lDeleteWorkDir
         hb_DirDelete( hbmk[ _HBMK_cWorkDir ] )
      ENDIF
      IF hbmk[ _HBMK_lCLEAN ]
         DirUnbuild( hbmk[ _HBMK_cWorkDir ] )
      ENDIF

      IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. ! lTargetUpToDate .AND. ;
         ( ! lStopAfterCComp .OR. hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ] )

         IF ! Empty( cBin_Post )

            cOpt_Post := StrTran( cOpt_Post, "{OB}", FNameEscape( hbmk[ _HBMK_cPROGNAME ], hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )
            IF l_cIMPLIBNAME != NIL
               cOpt_Post := StrTran( cOpt_Post, "{OI}", FNameEscape( l_cIMPLIBNAME, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )
            ENDIF
            cOpt_Post := AllTrim( cOpt_Post )

            cCommand := cBin_Post + " " + cOpt_Post

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, I_( "Post processor command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running post processor command. %1$s:" ), hb_ntos( tmp ) ) )
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
               IF hb_FileExists( DirAddPathSep( hb_DirBase() ) + cBin_Cprs )
                  cBin_Cprs := DirAddPathSep( hb_DirBase() ) + cBin_Cprs
               ENDIF
            #endif

            DO CASE
            CASE hbmk[ _HBMK_nCOMPR ] == _COMPR_MIN ; cOpt_Cprs += " " + cOpt_CprsMin
            CASE hbmk[ _HBMK_nCOMPR ] == _COMPR_MAX ; cOpt_Cprs += " " + cOpt_CprsMax
            ENDCASE

            cOpt_Cprs := StrTran( cOpt_Cprs, "{OB}", hbmk[ _HBMK_cPROGNAME ] )
            cOpt_Cprs := AllTrim( cOpt_Cprs )

            cCommand := cBin_Cprs + " " + cOpt_Cprs

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, I_( "Compression command:" ) )
               ENDIF
               OutStd( cCommand + _OUT_EOL )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hb_processRun( cCommand ) ) != 0
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running compression command. %1$s:" ), hb_ntos( tmp ) ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + _OUT_EOL )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. ;
         ( ! lStopAfterCComp .OR. hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ] )

         DoInstCopy( hbmk )
      ENDIF

      PlugIn_Execute_All( hbmk, "post_build" )
   ENDIF

   PlugIn_Execute_All( hbmk, "post_all" )

   IF hbmk[ _HBMK_lDEBUGTIME ]
      hbmk_OutStd( hbmk, hb_StrFormat( I_( "Running time: %1$ss" ), hb_ntos( TimeElapsed( nStart, Seconds() ) ) ) )
   ENDIF

   IF ! lSkipBuild .AND. hbmk[ _HBMK_lBEEP ]
      DoBeep( hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK )
   ENDIF

   IF ! hbmk[ _HBMK_lStopAfterHarbour ] .AND. ! lStopAfterCComp .AND. ;
      ! hbmk[ _HBMK_lCreateLib ] .AND. ! hbmk[ _HBMK_lCreateDyn ] .AND. ;
      hbmk[ _HBMK_nErrorLevel ] == _ERRLEV_OK .AND. ! hbmk[ _HBMK_lCLEAN ] .AND. hbmk[ _HBMK_lRUN ]
      cCommand := hbmk[ _HBMK_cPROGNAME ]
      #if defined( __PLATFORM__UNIX )
         IF Empty( FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ) )
            cCommand := "." + hb_ps() + hbmk[ _HBMK_cPROGNAME ]
         ENDIF
      #endif
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
            hbmk_OutStd( hbmk, I_( "Running executable:" ) )
         ENDIF
         OutStd( cCommand + _OUT_EOL )
      ENDIF
      IF ! hbmk[ _HBMK_lDONTEXEC ]
         hbmk[ _HBMK_nErrorLevel ] := hb_run( cCommand )
      ENDIF
   ENDIF

   RETURN hbmk[ _HBMK_nErrorLevel ]

STATIC PROCEDURE convert_incpaths_to_options( hbmk, cOptIncMask, lCHD_Comp )
   LOCAL cBaseDir
   LOCAL cINCPATH

   IF lCHD_Comp
      cBaseDir := DirAddPathSep( PathMakeRelative( PathNormalize( PathMakeAbsolute( hbmk[ _HBMK_cWorkDir ], hb_pwd() ) ), hb_pwd(), .T. ) )
   ENDIF

   FOR EACH cINCPATH IN hbmk[ _HBMK_aINCPATH ]
      IF ! Empty( cINCPATH )
         /* Different escaping for internal and external compiler. */
         AAddNew( hbmk[ _HBMK_aOPTPRG ], "-i" + iif( hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE, cINCPATH, FNameEscape( cINCPATH, hbmk[ _HBMK_nCmd_Esc ] ) ) )
         IF ! hbmk[ _HBMK_lStopAfterHarbour ]
            IF lCHD_Comp
               /* Rebase source dirs relative to the target dir */
               AAddNew( hbmk[ _HBMK_aOPTC ], StrTran( cOptIncMask, "{DI}", FNameEscape( PathNormalize( PathMakeAbsolute( cINCPATH, cBaseDir ) ), hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) )
            ELSE
               AAddNew( hbmk[ _HBMK_aOPTC ], StrTran( cOptIncMask, "{DI}", FNameEscape( cINCPATH, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) )
            ENDIF
            AAddNew( hbmk[ _HBMK_aOPTRES ], StrTran( cOptIncMask, "{DI}", FNameEscape( cINCPATH, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) ) )
         ENDIF
      ENDIF
   NEXT

   RETURN

#if defined( __PLATFORM__DOS )
STATIC FUNCTION hbmk_dos_FileExists( cFileName )
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )

   IF Len( cName ) > 8 .OR. ;
      Len( cExt ) > ( 1 + 3 )
      /* Return failure instead of loading wrong file or
         do other unpredictable operation */
      RETURN .F.
   ENDIF

   RETURN hb_FileExists( cFileName )
#endif

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

STATIC PROCEDURE vxworks_env_init( hbmk )

   /* Array positions for aTable */
   #define _VX_CCPOSTFIX        1
   #define _VX_DIAB_CPU         2
   #define _VX_CPU              3
   #define _VX_LIB_SUBDIR       4

   #define _VX_DIAB_ENV         "rtp"

   /* Conversion table between hbmk2 CPU and vxworks values required to target that CPU */
   LOCAL aTable := {;
      "x86"  => { "pentium", "X86LH"  , "_VX_SIMPENTIUM", "simpentium/SIMPENTIUM" },;
      "arm"  => { "arm"    , "ARMV7LS", "_VX_ARMARCH7"  , "arm/ARMARCH7"          },;
      "mips" => { "mips"   , ""       , ""              , ""                      },;
      "ppc"  => { "ppc"    , ""       , ""              , ""                      }}

   IF hbmk[ _HBMK_cCPU ] $ aTable
      IF Empty( hbmk[ _HBMK_cCCPOSTFIX ] )
         /* Used by gcc, and it's also used for strip even with diab compiler */
         hbmk[ _HBMK_cCCPOSTFIX ] := aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_CCPOSTFIX ]
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
            AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToSelf( GetEnv( "WIND_BASE" ) + "/target/lib/usr/lib/" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_LIB_SUBDIR ] + "/common/PIC" ) )
         ELSE
            AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToSelf( GetEnv( "WIND_BASE" ) + "/target/lib/usr/lib/" + aTable[ hbmk[ _HBMK_cCPU ] ][ _VX_LIB_SUBDIR ] + "/common" ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE DoLinkCalc( hbmk )
   LOCAL cDir, cName, cExt
   LOCAL tmp

   FOR EACH tmp IN hbmk[ _HBMK_aLINK ]

      cDir := PathMakeRelative( FNameDirGet( PathMakeAbsolute( tmp, FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ) ) ), FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ), .T. )
      /* Cheap hack */
      IF cDir == "." + hb_ps() .OR. ;
         cDir == hb_ps()
         cDir := ""
      ENDIF
      hb_FNameSplit( hbmk[ _HBMK_cPROGNAME ],, @cName, @cExt )

      tmp := { /* <cNewFileName>    */ PathNormalize( PathMakeAbsolute( tmp, FNameDirGet( hbmk[ _HBMK_cPROGNAME ] ) ) ),;
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
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Failed creating symbolic link %1$s to %2$s" ), tmp[ 1 ], tmp[ 2 ] ) )
      ELSE
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created symbolic link %1$s to %2$s" ), tmp[ 1 ], tmp[ 2 ] ) )
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION DoIMPLIB( hbmk, bBlk_ImpLib, cLibLibPrefix, cLibLibExt, aIMPLIBSRC, cPROGNAME, cInstCat )
   LOCAL cMakeImpLibDLL
   LOCAL tmp, tmp1
   LOCAL nNotFound

   LOCAL aToDelete
   LOCAL lRetVal := .F.

   IF ISBLOCK( bBlk_ImpLib )
      IF ! Empty( aIMPLIBSRC )
         aToDelete := {}
         nNotFound := 0
         FOR EACH cMakeImpLibDLL IN aIMPLIBSRC

            cMakeImpLibDLL := FNameExtDef( cMakeImpLibDLL, ".dll" )
            tmp1 := cPROGNAME
            DEFAULT tmp1 TO FNameNameGet( cMakeImpLibDLL )
            tmp := FN_CookLib( hb_FNameMerge( hbmk[ _HBMK_cPROGDIR ], tmp1 ), cLibLibPrefix, cLibLibExt )

            IF hbmk[ _HBMK_lCLEAN ]
               AAddNew( aToDelete, tmp )
            ELSE
               SWITCH Eval( bBlk_ImpLib, cMakeImpLibDLL, tmp, ArrayToList( hbmk[ _HBMK_aOPTI ] ) )
               CASE _HBMK_IMPLIB_OK
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created import library: %1$s <= %2$s" ), tmp, cMakeImpLibDLL ) )
                  AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { cInstCat, tmp }, .T. )
                  EXIT
               CASE _HBMK_IMPLIB_FAILED
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Failed creating import library %1$s from %2$s." ), tmp, cMakeImpLibDLL ) )
                  EXIT
               CASE _HBMK_IMPLIB_NOTFOUND
                  ++nNotFound
                  EXIT
               ENDSWITCH
            ENDIF
         NEXT

         IF nNotFound == Len( aIMPLIBSRC )
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: No import library sources were found." ) ) )
         ELSE
            IF hbmk[ _HBMK_lCLEAN ]
               AEval( aToDelete, {| tmp | FErase( tmp ) } )
            ELSE
               lRetVal := .T.
            ENDIF
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutErr( hbmk, I_( "Warning: No import library source was specified" ) )
         ENDIF
      ENDIF
   ELSE
      hbmk_OutErr( hbmk, I_( "Error: Creating import libraries is not supported for this platform or compiler." ) )
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

            IF ISARRAY( aInstFile[ _INST_cData ] )
               cInstFile := aInstFile[ _INST_cData ][ 1 ]
               cLink := aInstFile[ _INST_cData ][ 2 ]
            ELSE
               cInstFile := aInstFile[ _INST_cData ]
               cLink := NIL
            ENDIF

            IF aInstPath[ _INST_cGroup ] == aInstFile[ _INST_cGroup ]
               IF Empty( FNameNameExtGet( cInstPath ) )
                  cDestFileName := DirAddPathSep( cInstPath ) + FNameNameExtGet( cInstFile )
               ELSE
                  /* If destination is a full name, don't copy the extra files, only the first one.
                     (for the empty group name, this will be the build target) */
                  IF nCopied > 0
                     IF hbmk[ _HBMK_lInfo ]
                        hbmk_OutStd( hbmk, hb_StrFormat( I_( "Warning: Install path not a directory (%1$s). Extra install files not copied." ), cInstPath ) )
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

                  IF DirBuild( FNameDirGet( cDestFileName ) )
                     ++nCopied
                     IF cLink != NIL
                        FErase( cDestFileName )
                        IF hb_FLinkSym( cLink, cDestFileName ) == F_ERROR
                           hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Copying symbolic link %1$s to %2$s failed with %3$s." ), cInstFile, cDestFileName, hb_ntos( FError() ) ) )
                        ELSEIF hbmk[ _HBMK_lInfo ]
                           hbmk_OutStd( hbmk, hb_StrFormat( I_( "Copied symbolic link %1$s to %2$s" ), cInstFile, cDestFileName ) )
                        ENDIF
                     ELSE
                        IF hb_FCopy( cInstFile, cDestFileName ) == F_ERROR
                           hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Copying %1$s to %2$s failed with %3$s." ), cInstFile, cDestFileName, hb_ntos( FError() ) ) )
                        ELSEIF hbmk[ _HBMK_lInfo ]
                           hbmk_OutStd( hbmk, hb_StrFormat( I_( "Copied %1$s to %2$s" ), cInstFile, cDestFileName ) )
                        ENDIF
                     ENDIF
                  ELSE
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create install directory for install target %1$s." ), cDestFileName ) )
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
      OutStd( Replicate( Chr( 7 ), nRepeat ) )
   ENDIF

   RETURN

STATIC FUNCTION CompileCLoop( hbmk, aTODO, cBin_CompC, cOpt_CompC, cObjExt, nOpt_Esc, nOpt_FNF, nJob, nJobs )
   LOCAL lResult := .T.
   LOCAL cCommand
   LOCAL tmp, tmp1

   LOCAL lOutputSpecified
   LOCAL cOutputFile

   FOR EACH tmp IN aTODO

      cCommand := cOpt_CompC

      lOutputSpecified := "{OO}" $ cCommand
      cOutputFile := FNameDirExtSet( tmp, hbmk[ _HBMK_cWorkDir ], cObjExt )

      cCommand := StrTran( cCommand, "{IC}", FNameEscape( tmp, nOpt_Esc, nOpt_FNF ) )
      cCommand := StrTran( cCommand, "{OO}", FNameEscape( cOutputFile, nOpt_Esc, nOpt_FNF ) )

      cCommand := cBin_CompC + " " + AllTrim( cCommand )

      IF hbmk[ _HBMK_lTRACE ]
         IF ! hbmk[ _HBMK_lQuiet ]
            IF nJobs > 1
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "C/C++ compiler command job #%1$s:" ), hb_ntos( nJob ) ) )
            ELSE
               hbmk_OutStd( hbmk, I_( "C/C++ compiler command:" ) )
            ENDIF
         ENDIF
         OutStd( cCommand + _OUT_EOL )
      ENDIF

      IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hb_processRun( cCommand ) ) != 0
         IF nJobs > 1
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler job #%1$s. %2$s" ), hb_ntos( nJob ), hb_ntos( tmp1 ) ) )
         ELSE
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C/C++ compiler. %1$s" ), hb_ntos( tmp1 ) ) )
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
         (please don't add contrib/3rd parties here) */
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
   root file. It won't attempt to parse all possible #include syntaxes
   and source code formats, won't try to interpret comments, line
   continuation, different keyword and filename cases, etc, etc. In
   order to work, it will need #include "filename" and #include
   <filename> format in source. If this isn't enough for your needs,
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

   STATIC s_hRegexInclude := NIL
   STATIC s_hExclStd := NIL

   IF hbmk[ _HBMK_nHEAD ] == _HEAD_OFF
      RETURN .F.
   ENDIF

   IF tTimeParent != NIL .AND. hb_FGetDateTime( cFileName, @tTimeSelf ) .AND. tTimeSelf > tTimeParent
      RETURN .T.
   ENDIF

   cExt := Lower( FNameExtGet( cFileName ) )

   /* Filter out non-source format inputs for MinGW / windres */
   IF HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|cygwin" ) .AND. HBMK_ISPLAT( "win|wce" ) .AND. cExt == ".res" /* TOFIX: cygwin is now a platform */
      RETURN .F.
   ENDIF

   IF ! lCMode .AND. hbmk[ _HBMK_nHEAD ] == _HEAD_NATIVE .AND. hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE

      IF hbmk[ _HBMK_lDEBUGINC ]
         hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: Calling Harbour compiler to detect dependencies of %1$s", cFileName ) )
      ENDIF

      aCommand := ArrayAJoin( { { "-q0", "-sm" },;
                                { iif( hbmk[ _HBMK_lCreateLib ] .OR. hbmk[ _HBMK_lCreateDyn ], "-n1", "-n2" ) },;
                                { cFileName },;
                                iif( hbmk[ _HBMK_lBLDFLGP ], { hb_Version( HB_VERSION_FLAG_PRG ) }, {} ),;
                                ListToArray( iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) ),;
                                hbmk[ _HBMK_aOPTPRG ] } )

      IF ! ISCHARACTER( tmp := hb_compileBuf( "harbour", aCommand ) )
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
                     hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER (NATIVE) %1$s", cDependency ) )
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

   ELSEIF ! lCMode .AND. LEFTEQUAL( cFileName, "@" ) .AND. cExt == ".clp"

      FOR EACH cDependency IN clpfile_read( SubStr( cFileName, 2 ) )
         IF ! Empty( cDependency )
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER (CLP) %1$s", cDependency ) )
            ENDIF
            IF hb_FGetDateTime( cDependency, @tTimeDependency ) .AND. tTimeDependency > tTimeParent
               RETURN .T.
            ENDIF
         ENDIF
      NEXT

   ELSEIF lCMode .AND. hbmk[ _HBMK_nHEAD ] == _HEAD_NATIVE .AND. HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|cygwin|djgpp|gccomf|clang|open64" ) /* TOFIX: cygwin is now a platform */

      IF hbmk[ _HBMK_lDEBUGINC ]
         hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: Calling C/C++ compiler to detect dependencies of %1$s", cFileName ) )
      ENDIF

      tmp := ""
      hb_processRun( cBin_CompC + " -MM" +;
                     " " + iif( hbmk[ _HBMK_lBLDFLGC ], hb_Version( HB_VERSION_FLAG_C ) + " ", "" ) +;
                           GetEnv( "HB_USER_CFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTC ] ) +;
                     " " + FNameEscape( hbmk[ _HBMK_cHB_INSTALL_INC ], hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) +;
                     " " + cFileName,, @tmp )

      tmp := StrTran( tmp, Chr( 13 ) )
      tmp := StrTran( tmp, " \" + Chr( 10 ) )

      FOR EACH cModule IN hb_ATokens( tmp, Chr( 10 ) )
         IF ! Empty( cModule )
            FOR EACH cDependency IN hb_ATokens( cModule, " " )
               IF cDependency:__enumIndex() > 2 .AND. ; /* Skip own (module) name as object and source */
                  ! Empty( cDependency )
                  IF hbmk[ _HBMK_lDEBUGINC ]
                     hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C HEADER (NATIVE) %1$s", cDependency ) )
                  ENDIF
                  IF hb_FGetDateTime( cDependency, @tTimeDependency ) .AND. tTimeDependency > tTimeParent
                     RETURN .T.
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      NEXT
   ELSE
      IF getNewestTime( hbmk, cFileName, @hbmk[ _HBMK_hFiles ], lCMode ) > tTimeParent
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

#define _HBMK_HEADER_cHeader        1
#define _HBMK_HEADER_lSystemHeader  2
#define _HBMK_HEADER_LEN_           2

STATIC FUNCTION s_getIncludedFiles( hbmk, cFile, cParentDir, lCMode )
   STATIC s_hRegexInclude
   STATIC s_hExclStd

   LOCAL aDeps
   LOCAL cFileBody
   LOCAL lSystemHeader
   LOCAL cHeader
   LOCAL aDep
   LOCAL tmp

   /* NOTE:
         http://en.wikipedia.org/wiki/PCRE
         http://www.pcre.org/pcre.txt */

   IF s_hRegexInclude == NIL
      s_hRegexInclude := hb_regexComp( '^[[:blank:]]*#[[:blank:]]*(include|import)[[:blank:]]*(\".+?\"|<.+?>'+"|'.+?'|`.+?'"+')',;
         .F. /* lCaseSensitive */,;
         .T. /* lNewLine */ )
      IF Empty( s_hRegexInclude )
         hbmk_OutErr( hbmk, I_( "Internal Error: Regular expression engine missing or unsupported. Please check your Harbour build settings." ) )
         s_hRegexInclude := {} /* To show the error only once by setting to non-NIL empty value */
      ENDIF
   ENDIF

   aDeps := {}
   IF ! Empty( s_hRegexInclude )

      cFileBody := MemoRead( cFile )

      IF ! Empty( cFileBody )
         FOR EACH tmp IN hb_regexAll( s_hRegexInclude, cFileBody, ;
                                      NIL /* lCaseSensitive */, ;
                                      NIL /* lNewLine */, NIL, ;
                                      NIL /* nGetMatch */, ;
                                      .T. /* lOnlyMatch */ )
            cHeader := tmp[ 3 ] /* First match marker */
            lSystemHeader := ( Left( cHeader, 1 ) == "<" )
            cHeader := SubStr( cHeader, 2, Len( cHeader ) - 2 )

            /* Don't spend time on known system headers */
            IF lSystemHeader

               IF s_hExclStd == NIL
                  s_hExclStd := {;
                     "assert.h"       => NIL ,; /* Standard C */
                     "ctype.h"        => NIL ,;
                     "errno.h"        => NIL ,;
                     "float.h"        => NIL ,;
                     "limits.h"       => NIL ,;
                     "locale.h"       => NIL ,;
                     "math.h"         => NIL ,;
                     "setjmp.h"       => NIL ,;
                     "signal.h"       => NIL ,;
                     "stdarg.h"       => NIL ,;
                     "stddef.h"       => NIL ,;
                     "stdio.h"        => NIL ,;
                     "stdlib.h"       => NIL ,;
                     "string.h"       => NIL ,;
                     "time.h"         => NIL ,;
                     "iso646.h"       => NIL ,; /* ISO C NA1 */
                     "wchar.h"        => NIL ,;
                     "wctype.h"       => NIL ,;
                     "complex.h"      => NIL ,; /* ISO C C99 */
                     "fenv.h"         => NIL ,;
                     "inttypes.h"     => NIL ,;
                     "stdbool.h"      => NIL ,;
                     "stdint.h"       => NIL ,;
                     "tgmath.h"       => NIL ,;
                     "unistd.h"       => NIL ,; /* Standard C POSIX */
                     "aio.h"          => NIL ,;
                     "arpa/inet.h"    => NIL ,;
                     "cpio.h"         => NIL ,;
                     "dirent.h"       => NIL ,;
                     "dlfcn.h"        => NIL ,;
                     "fcntl.h"        => NIL ,;
                     "fmtmsg.h"       => NIL ,;
                     "fnmatch.h"      => NIL ,;
                     "ftw.h"          => NIL ,;
                     "glob.h"         => NIL ,;
                     "grp.h"          => NIL ,;
                     "iconv.h"        => NIL ,;
                     "langinfo.h"     => NIL ,;
                     "libgen.h"       => NIL ,;
                     "monetary.h"     => NIL ,;
                     "mqueue.h"       => NIL ,;
                     "ndbm.h"         => NIL ,;
                     "net/if.h"       => NIL ,;
                     "netdb.h"        => NIL ,;
                     "netinet/in.h"   => NIL ,;
                     "netinet/tcp.h"  => NIL ,;
                     "nl_types.h"     => NIL ,;
                     "poll.h"         => NIL ,;
                     "pthread.h"      => NIL ,;
                     "pwd.h"          => NIL ,;
                     "regex.h"        => NIL ,;
                     "sched.h"        => NIL ,;
                     "search.h"       => NIL ,;
                     "semaphore.h"    => NIL ,;
                     "spawn.h"        => NIL ,;
                     "strings.h"      => NIL ,;
                     "stropts.h"      => NIL ,;
                     "sys/ipc.h"      => NIL ,;
                     "sys/mman.h"     => NIL ,;
                     "sys/msg.h"      => NIL ,;
                     "sys/resource.h" => NIL ,;
                     "sys/select.h"   => NIL ,;
                     "sys/sem.h"      => NIL ,;
                     "sys/shm.h"      => NIL ,;
                     "sys/socket.h"   => NIL ,;
                     "sys/stat.h"     => NIL ,;
                     "sys/statvfs.h"  => NIL ,;
                     "sys/time.h"     => NIL ,;
                     "sys/times.h"    => NIL ,;
                     "sys/types.h"    => NIL ,;
                     "sys/uio.h"      => NIL ,;
                     "sys/un.h"       => NIL ,;
                     "sys/utsname.h"  => NIL ,;
                     "sys/wait.h"     => NIL ,;
                     "syslog.h"       => NIL ,;
                     "tar.h"          => NIL ,;
                     "termios.h"      => NIL ,;
                     "trace.h"        => NIL ,;
                     "ulimit.h"       => NIL ,;
                     "unistd.h"       => NIL ,;
                     "utime.h"        => NIL ,;
                     "utmpx.h"        => NIL ,;
                     "wordexp.h"      => NIL ,;
                     "windows.h"      => NIL ,; /* OS (win) */
                     "winspool.h"     => NIL ,;
                     "shellapi.h"     => NIL ,;
                     "ole2.h"         => NIL ,;
                     "dos.h"          => NIL ,; /* OS (dos) */
                     "os2.h"          => NIL }  /* OS (os2) */
               ENDIF

               IF StrTran( Lower( cHeader ), "\", "/" ) $ s_hExclStd
                  LOOP
               ENDIF
            ENDIF

            IF ! lCMode .AND. cHeader $ hbmk[ _HBMK_hAUTOHBC ]
               hbmk[ _HBMK_hAUTOHBCFOUND ][ cHeader ] := hbmk[ _HBMK_hAUTOHBC ][ cHeader ]
               hb_HDel( hbmk[ _HBMK_hAUTOHBC ], cHeader )
            ENDIF

            IF ( cHeader := FindHeader( hbmk, cHeader, cParentDir, lSystemHeader, lSystemHeader ) ) != NIL

               IF hbmk[ _HBMK_lDEBUGINC ]
                  hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER %1$s %2$s", cHeader, iif( lSystemHeader, "(system)", "" ) ) )
               ENDIF

               aDep := Array( _HBMK_HEADER_LEN_ )
               aDep[ _HBMK_HEADER_cHeader ]       := cHeader
               aDep[ _HBMK_HEADER_lSystemHeader ] := lSystemHeader
               AAdd( aDeps, aDep )
            ENDIF
         NEXT
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
         /* Don't scan into system headers */
         aDeps := {}
      ELSE
         aDeps := s_getIncludedFiles( hbmk, cFile, iif( lCMode, FNameDirGet( cFile ), cParentDir ), lCMode )
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
         s_getFilesDep( hbmk, aDep[ _HBMK_HEADER_cHeader ],;
                              hFiles,;
                              cParentDir,;
                              aDep[ _HBMK_HEADER_lSystemHeader ],;
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
             * scanned. It's possible that these are circular references
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
      /* hb_hKeepOrder( hFiles, .T. ) */
   ENDIF
   s_getFilesDep( hbmk, cFile, hFiles, FNameDirGet( cFile ), .F., lCMode )
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

STATIC FUNCTION clpfile_read( cFileName )
   LOCAL cFileBody := MemoRead( cFileName )
   LOCAL aFiles
   LOCAL cFile

   cFileBody := StrTran( cFileBody, Chr( 13 ) )
   cFileBody := StrTran( cFileBody, Chr( 10 ), " " )
   cFileBody := StrTran( cFileBody, Chr( 9 ), " " )

   aFiles := hb_ATokens( cFileBody,, .T. )
   FOR EACH cFile IN aFiles
      cFile := FNameExtDef( StrTran( cFile, '"' ), ".prg" )
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
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: In %1$s at line %2$s %3$s:" ), cFileName, hb_ntos( nLine ), cList ) )
            RETURN NIL
         ENDIF
         cList := ""
      ENDIF
   NEXT

   IF ! deplst_add( hDeps, cList )
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: In %1$s at line %2$s %3$s:" ), cFileName, hb_ntos( nLine ), cList ) )
      RETURN NIL
   ENDIF

   RETURN hDeps

STATIC FUNCTION deplst_add( hDeps, cList )
   LOCAL cFile
   LOCAL aList
   LOCAL n

   IF ! Empty( cList )
      n := At( ":", cList )
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
      install group must be at least 2 chars, otherwise it's considered as drive letter */

   IF ( nPos := At( ":", cParam ) ) > 2
      cName := Left( cParam, nPos - 1 )
      cData := SubStr( cParam, nPos + 1 )
   ELSE
      cName := ""
      cData := cParam
   ENDIF

   cData := PathSepToSelf( cData )

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
      cHeader := FNameExtSet( cHBC )
   ELSEIF Empty( cHBC ) .AND. ! Empty( cHeader )
      cHBC := FNameExtSet( cHeader )
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
      dep[ _HBMKDEP_aINCPATH ] := {}
      dep[ _HBMKDEP_aINCPATHLOCAL ] := {}
      dep[ _HBMKDEP_aIMPLIBSRC ] := {}
      dep[ _HBMKDEP_cIMPLIBDST ] := NIL
      dep[ _HBMKDEP_lForced ] := .T.
   CASE cControlL == "local"
      dep[ _HBMKDEP_cControl ] := cControlL
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
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Dependency '%1$s' found: %2$s%3$s%4$s%5$s" ),;
               dep[ _HBMKDEP_cName ],;
               dep[ _HBMKDEP_cFound ],;
               iif( Empty( dep[ _HBMKDEP_cVersion ] ), "", " (" + dep[ _HBMKDEP_cVersion ] + ")" ),;
               iif( dep[ _HBMKDEP_lFoundLOCAL ], " (local)", "" ),;
               iif( dep[ _HBMKDEP_lForced ], " (forced)", "" ) ) )
         ENDIF
      ELSE
         IF dep[ _HBMKDEP_lForced ]
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Dependency '%1$s' forcefully disabled" ), dep[ _HBMKDEP_cName ] ) )
            ENDIF
            lAnyForcedOut := .T.
            LOOP
         ELSE
            IF hbmk[ _HBMK_lDEBUGDEPD ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debugdepd: REQ %1$s: missing", dep[ _HBMKDEP_cName ] ) )
            ENDIF
         ENDIF
         IF dep[ _HBMKDEP_lOptional ]
            AAdd( aOPT, dep[ _HBMKDEP_cName ] )
         ELSE
            /* Don't issue a missing dependency error (just warning) for non-*nix
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
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing optional dependencies: %1$s" ), ArrayToList( aOPT, ", " ) ) )
      ELSE
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing optional dependency: %1$s" ), ArrayToList( aOPT, ", " ) ) )
      ENDIF
   ENDIF

   IF ! Empty( aREQ )
      IF Len( aREQ ) > 1
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Missing dependencies: %1$s" ), ArrayToList( aREQ, ", " ) ) )
      ELSE
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Missing dependency: %1$s" ), ArrayToList( aREQ, ", " ) ) )
      ENDIF
   ENDIF

   IF ! Empty( aWRN )
      IF Len( aWRN ) > 1
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing dependencies: %1$s" ), ArrayToList( aWRN, ", " ) ) )
      ELSE
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Missing dependency: %1$s" ), ArrayToList( aWRN, ", " ) ) )
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
            /* DarwinPorts */
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
                        AAdd( hbmk[ _HBMK_aLIBPATH ], DirDelPathSep( PathSepToSelf( cItem ) ) )
                     CASE Left( cItem, Len( "-I" ) ) == "-I"
                        cItem := DirDelPathSep( PathSepToSelf( SubStr( cItem, Len( "-I" ) + 1 ) ) )
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
                     /* Adjust implib source names with component path */
                     FOR EACH tmp IN dep[ _HBMKDEP_aIMPLIBSRC ]
                        tmp := PathNormalize( PathMakeAbsolute( tmp, DirAddPathSep( cIncludeDir ) ) )
                     NEXT
                  ENDIF
                  IF hbmk[ _HBMK_lDEBUGDEPD ]
                     hbmk_OutStd( hbmk, hb_StrFormat( "debugdepd: REQ %1$s: found as pkg at %2$s (%3$s)", dep[ _HBMKDEP_cName ], dep[ _HBMKDEP_cFound ], dep[ _HBMKDEP_cVersion ] ) )
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
   LOCAL cDir
   LOCAL cFileName

   LOCAL tmp

   /* Check dependency include path list */

   IF ! dep[ _HBMKDEP_lFound ]
      FOR EACH aINCPATH IN { dep[ _HBMKDEP_aINCPATH ],;
                             dep[ _HBMKDEP_aINCPATHLOCAL ] }
         FOR EACH cDir IN aINCPATH
            FOR EACH cFileName IN dep[ _HBMKDEP_aKeyHeader ]
               IF HeaderExists( cDir, cFileName ) != NIL
                  dep[ _HBMKDEP_cFound ] := DirDelPathSep( PathSepToSelf( cDir ) )
                  hbmk[ _HBMK_hDEPTSDIR ][ dep[ _HBMKDEP_cFound ] ] := NIL
                  /* Adjust implib source names with component path */
                  FOR EACH tmp IN dep[ _HBMKDEP_aIMPLIBSRC ]
                     tmp := PathNormalize( PathMakeAbsolute( tmp, DirAddPathSep( dep[ _HBMKDEP_cFound ] ) ) )
                  NEXT
                  dep[ _HBMKDEP_lFound ] := .T.
                  dep[ _HBMKDEP_lFoundLOCAL ] := ( aINCPATH:__enumIndex() == 2 )
                  IF hbmk[ _HBMK_lDEBUGDEPD ]
                     hbmk_OutStd( hbmk, hb_StrFormat( "debugdepd: REQ %1$s: found by %2$s header at %3$s %4$s", dep[ _HBMKDEP_cName ], PathSepToSelf( cFileName ), dep[ _HBMKDEP_cFound ], iif( dep[ _HBMKDEP_lFoundLOCAL ], "(local)", "" ) ) )
                  ENDIF
                  AAddNew( hbmk[ _HBMK_aINCPATH ], DirDelPathSep( PathSepToSelf( cDir ) ) )
                  AAdd( hbmk[ _HBMK_aOPTC ], "-D" + hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) )
                  hbmk[ _HBMK_hDEPTMACRO ][ hb_StrFormat( _HBMK_HAS_TPL, StrToDefine( dep[ _HBMKDEP_cName ] ) ) ] := NIL
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

   FOR EACH c IN Upper( cString )
      IF c $ "- "
         cDefine += "_"
      ELSEIF IsDigit( c ) .OR. hb_asciiIsAlpha( c ) .OR. c == "_"
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
         IF hb_FileExists( PathSepToSelf( cFileName ) )
            RETURN PathSepToSelf( cFileName )
         ENDIF
      ELSE
         /* Check in parent dir */
         tmp := DirAddPathSep( PathSepToSelf( cParentDir ) ) + PathSepToSelf( cFileName )
         IF hb_FileExists( tmp )
            RETURN tmp
         ENDIF
      ENDIF
   ENDIF

   /* Check in include path list specified via -incpath options */
   IF lSkipDept
      FOR EACH cDir IN hbmk[ _HBMK_aINCPATH ]
         IF !( cDir $ hbmk[ _HBMK_hDEPTSDIR ] )
            tmp := DirAddPathSep( PathSepToSelf( cDir ) ) + PathSepToSelf( cFileName )
            IF hb_FileExists( tmp )
               RETURN tmp
            ENDIF
         ENDIF
      NEXT
   ELSE
      FOR EACH cDir IN hbmk[ _HBMK_aINCPATH ]
         tmp := DirAddPathSep( PathSepToSelf( cDir ) ) + PathSepToSelf( cFileName )
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
      tmp := DirAddPathSep( PathSepToSelf( cDir ) ) + Left( cFileName, nPos - 1 ) + ".framework" + hb_ps() + "Headers" + hb_ps() + SubStr( cFileName, nPos + 1 )
      IF hb_FileExists( tmp )
         RETURN tmp
      ENDIF
   ENDIF
#endif
   tmp := DirAddPathSep( PathSepToSelf( cDir ) ) + PathSepToSelf( cFileName )
   RETURN iif( hb_FileExists( tmp ), tmp, NIL )

/* Replicating logic used by compilers. */

STATIC FUNCTION FindLib( hbmk, cLib, aLIBPATH, cLibPrefix, cLibExt )
   LOCAL cDir
   LOCAL tmp

   /* Check libs in their full paths */
   IF HBMK_ISCOMP( "msvc|msvc64|msvcarm|bcc|pocc|pocc64|poccarm|watcom" )
      IF ! Empty( FNameDirGet( cLib ) )
         IF hb_FileExists( cLib := FNameExtSet( cLib, cLibExt ) )
            RETURN cLib
         ENDIF
         IF HBMK_ISCOMP( "pocc|pocc64|poccarm" )
            IF hb_FileExists( cLib := FNameExtSet( cLib, ".a" ) )
               RETURN cLib
            ENDIF
         ENDIF
         RETURN NIL
      ENDIF
   ENDIF

   /* Check in current dir */
   IF HBMK_ISCOMP( "msvc|msvc64|msvcarm|bcc|pocc|pocc64|poccarm|watcom" )
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

   cDir := DirAddPathSep( PathSepToSelf( cDir ) )

   DO CASE
   CASE HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm" ) .AND. HBMK_ISPLAT( "win|wce|cygwin" )
      /* NOTE: ld/gcc option -dll-search-prefix isn't taken into account here,
               So, '<prefix>xxx.dll' format libs won't be found by hbmk2. */
      DO CASE
      CASE                                       hb_FileExists( tmp := cDir + "lib" + FNameExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir +         FNameExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir + "lib" + FNameExtSet( cLib, ".a" )     ) ; RETURN tmp
      CASE hbmk[ _HBMK_cPLAT ] == "cygwin" .AND. hb_FileExists( tmp := cDir + "cyg" + FNameExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir + "lib" + FNameExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir +         FNameExtSet( cLib, ".dll" )   ) ; RETURN tmp
      ENDCASE
   CASE hbmk[ _HBMK_cCOMP ] == "gcc" .AND. HBMK_ISPLAT( "linux|sunos" )
      DO CASE
      CASE                                       hb_FileExists( tmp := cDir + "lib" + FNameExtSet( cLib, ".so" )    ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir + "lib" + FNameExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   CASE HBMK_ISCOMP( "pocc|pocc64|poccarm" )
      DO CASE
      CASE                                       hb_FileExists( tmp := cDir +         FNameExtSet( cLib, cLibExt )  ) ; RETURN tmp
      CASE                                       hb_FileExists( tmp := cDir +         FNameExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   OTHERWISE
      DO CASE
      CASE                                   hb_FileExists( tmp := cDir + cLibPrefix + FNameExtSet( cLib, cLibExt ) ) ; RETURN tmp
      ENDCASE
   ENDCASE

   RETURN NIL

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

STATIC PROCEDURE PlugIn_Load( hbmk, cFileName )
   LOCAL cFile
   LOCAL cExt
   LOCAL lOK
   LOCAL cType
   LOCAL hrb

   cFileName := PathNormalize( cFileName )

   IF !( cFileName $ hbmk[ _HBMK_hPLUGINHRB ] )

      hrb := NIL

      hb_FNameSplit( cFileName, NIL, NIL, @cExt )

      cFile := hb_MemoRead( cFileName )

      IF ! Empty( cFile )
         lOK := .F.
         /* Optimization: Don't try to load it as .hrb if the extension is .prg or .hbs (Harbour script) */
         IF !( Lower( cExt ) == ".prg" ) .AND. ;
            !( Lower( cExt ) == ".hbs" )
            BEGIN SEQUENCE WITH {| oError | Break( oError ) }
               hrb := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
               cType := I_( "(compiled)" )
               lOK := .T.
            END SEQUENCE
         ENDIF
         IF ! lOK .AND. !( Lower( cExt ) == ".hrb" ) /* Optimization: Don't try to load it as .prg if the extension is .hrb */
            cType := I_( "(source)" )
            cFile := hb_compileFromBuf( cFile, "-n2", "-w3", "-es2", "-q0", "-i" + hbmk[ _HBMK_cHB_INSTALL_INC ], "-D" + _HBMK_SCRIPT )
            IF ! Empty( cFile )
               hrb := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cFile )
            ENDIF
         ENDIF
      ENDIF

      IF ! Empty( hrb )
         IF ! PlugIn_call_low( hbmk, cFileName, hrb, PlugIn_make_ctx( hbmk, "init" ) )
            /* Don't call plugin any further if initialization returned error */
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Initializing plugin: %1$s" ), cFileName ) )
            ENDIF
         ELSE
            hbmk[ _HBMK_hPLUGINHRB ][ cFileName ] := hrb
            IF hbmk[ _HBMK_lTRACE ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loaded plugin: %1$s %2$s" ), cFileName, cType ) )
            ENDIF
         ENDIF
      ELSE
         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Loading plugin: %1$s" ), cFileName ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN

/* Public functions accessible for plugins */

FUNCTION hbmk2_CWD()                   ; RETURN hb_pwd()
FUNCTION hbmk2_FindInPath( ... )       ; RETURN FindInPath( ... )
FUNCTION hbmk2_PathNormalize( ... )    ; RETURN PathNormalize( ... )
FUNCTION hbmk2_PathMakeAbsolute( ... ) ; RETURN PathMakeAbsolute( ... )
FUNCTION hbmk2_PathMakeRelative( ... ) ; RETURN PathMakeRelative( ... )
FUNCTION hbmk2_PathSepToForward( ... ) ; RETURN PathSepToForward( ... )
FUNCTION hbmk2_PathSepToSelf( ... )    ; RETURN PathSepToSelf( ... )
FUNCTION hbmk2_DirAddPathSep( ... )    ; RETURN DirAddPathSep( ... )
FUNCTION hbmk2_DirDelPathSep( ... )    ; RETURN DirDelPathSep( ... )
FUNCTION hbmk2_DirBuild( ... )         ; RETURN DirBuild( ... )
FUNCTION hbmk2_DirUnbuild( ... )       ; RETURN DirUnbuild( ... )
FUNCTION hbmk2_FNameDirGet( ... )      ; RETURN FNameDirGet( ... )
FUNCTION hbmk2_FNameDirExtSet( ... )   ; RETURN FNameDirExtSet( ... )
FUNCTION hbmk2_FNameNameGet( ... )     ; RETURN FNameNameGet( ... )
FUNCTION hbmk2_FNameNameExtGet( ... )  ; RETURN FNameNameExtGet( ... )
FUNCTION hbmk2_FNameExtGet( ... )      ; RETURN FNameExtGet( ... )
FUNCTION hbmk2_FNameExtDef( ... )      ; RETURN FNameExtDef( ... )
FUNCTION hbmk2_FNameExtSet( ... )      ; RETURN FNameExtSet( ... )
FUNCTION hbmk2_FNameEscape( ... )      ; RETURN FNameEscape( ... )
FUNCTION hbmk2_FNameToSymbol( ... )    ; RETURN FuncNameEncode( ... )
FUNCTION hbmk2_StrStripQuote( ... )    ; RETURN StrStripQuote( ... )
FUNCTION hbmk2_OutStdRaw( ... )        ; RETURN ( OutStd( ... ), OutStd( _OUT_EOL ) )
FUNCTION hbmk2_OutErrRaw( ... )        ; RETURN ( OutErr( ... ), OutErr( _OUT_EOL ) )

FUNCTION hbmk2_ArrayToList( array, cSeparator )
   LOCAL cString := ""
   LOCAL tmp

   IF ! ISCHARACTER( cSeparator )
      cSeparator := " "
   ENDIF

   FOR tmp := 1 TO Len( array )
      cString += array[ tmp ]
      IF tmp < Len( array )
         cString += cSeparator
      ENDIF
   NEXT

   RETURN cString

STATIC FUNCTION ctx_to_hbmk( ctx )
   LOCAL hbmk
   IF hb_isHash( ctx ) .AND. s_cSecToken $ ctx
      hbmk := ctx[ s_cSecToken ]
      IF ISARRAY( hbmk ) .AND. Len( hbmk ) == _HBMK_MAX_
         RETURN hbmk
      ENDIF
   ENDIF
   RETURN NIL

FUNCTION hbmk2_PathFromWorkdirToCWD( ctx )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL
      RETURN DirAddPathSep( PathMakeRelative( PathNormalize( PathMakeAbsolute( hbmk[ _HBMK_cWorkDir ], hb_pwd() ) ), hb_pwd(), .T. ) )
   ENDIF
   RETURN ""

FUNCTION hbmk2_Macro( ctx, cString )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL
      RETURN MacroProc( hbmk, cString )
   ENDIF
   RETURN ""

FUNCTION hbmk2_OutStd( ctx, cText )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL
      RETURN hbmk_OutStd( hbmk, hb_StrFormat( I_( "plugin: %1$s" ), cText ) )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_OutErr( ctx, cText )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL
      RETURN hbmk_OutErr( hbmk, hb_StrFormat( I_( "plugin: %1$s" ), cText ) )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_PathSepToTarget( ctx, ... )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL
      RETURN PathSepToTarget( hbmk, ... )
   ENDIF
   RETURN ""

FUNCTION hbmk2_AddInput_PRG( ctx, cFileName )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cFileName )
      AAdd( hbmk[ _HBMK_aPRG ], PathSepToSelf( cFileName ) )
      DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cFileName )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_AddInput_C( ctx, cFileName )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cFileName )
      AAdd( hbmk[ _HBMK_aC ], PathSepToSelf( cFileName ) )
      DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cFileName )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_AddInput_CPP( ctx, cFileName )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cFileName )
      AAdd( hbmk[ _HBMK_aCPP ], PathSepToSelf( cFileName ) )
      DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cFileName )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_AddInput_RC( ctx, cFileName )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cFileName )
      AAdd( hbmk[ _HBMK_aRESSRC ], PathSepToSelf( cFileName ) )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_AddInput_OBJ( ctx, cFileName )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cFileName )
      AAdd( hbmk[ _HBMK_aOBJUSER ], PathSepToSelf( cFileName ) )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_AddInput_INSTFILE( ctx, cFileName, cGroup )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cFileName )
      IF ! ISCHARACTER( cGroup )
         cGroup := ""
      ENDIF
      AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { cGroup, PathSepToSelf( cFileName ) } )
   ENDIF
   RETURN NIL

FUNCTION hbmk2_Register_Input_File_Extension( ctx, cExt )
   LOCAL hbmk := ctx_to_hbmk( ctx )
   IF hbmk != NIL .AND. ISCHARACTER( cExt )
      IF ! Empty( cExt )
         IF !( Left( cExt, 1 ) == "." )
            cExt := "." + cExt
         ENDIF
         hbmk[ _HBMK_hPLUGINExt ][ Lower( cExt ) ] := NIL
      ENDIF
   ENDIF
   RETURN NIL

/* ; */

STATIC FUNCTION PlugIn_make_ctx( hbmk, cState )
   RETURN {;
         "cSTATE"       => cState                     ,;
         "params"       => hbmk[ _HBMK_aPLUGINPars ]  ,;
         "vars"         => hbmk[ _HBMK_hPLUGINVars ]  ,;
         "cPLAT"        => hbmk[ _HBMK_cPLAT ]        ,;
         "cCOMP"        => hbmk[ _HBMK_cCOMP ]        ,;
         "cCPU"         => hbmk[ _HBMK_cCPU ]         ,;
         "cBUILD"       => hbmk[ _HBMK_cBUILD ]       ,;
         "cOUTPUTNAME"  => hbmk[ _HBMK_cPROGNAME ]    ,;
         "cTARGETNAME"  => hbmk_TARGETNAME( hbmk )    ,;
         "cTARGETTYPE"  => hbmk_TARGETTYPE( hbmk )    ,;
         "lREBUILD"     => hbmk[ _HBMK_lREBUILD ]     ,;
         "lCLEAN"       => hbmk[ _HBMK_lCLEAN ]       ,;
         "lDEBUG"       => hbmk[ _HBMK_lDEBUG ]       ,;
         "lMAP"         => hbmk[ _HBMK_lMAP ]         ,;
         "lSTRIP"       => hbmk[ _HBMK_lSTRIP ]       ,;
         "lDONTEXEC"    => hbmk[ _HBMK_lDONTEXEC ]    ,;
         "lIGNOREERROR" => hbmk[ _HBMK_lIGNOREERROR ] ,;
         "lTRACE"       => hbmk[ _HBMK_lTRACE ]       ,;
         "lQUIET"       => hbmk[ _HBMK_lQuiet ]       ,;
         "lINFO"        => hbmk[ _HBMK_lInfo ]        ,;
         "lBEEP"        => hbmk[ _HBMK_lBEEP ]        ,;
         "lRUN"         => hbmk[ _HBMK_lRUN ]         ,;
         "lINC"         => hbmk[ _HBMK_lINC ]         ,;
         "cCCPATH"      => hbmk[ _HBMK_cCCPATH ]      ,;
         "cCCPREFIX"    => hbmk[ _HBMK_cCCPREFIX ]    ,;
         "cCCPOSTFIX"   => hbmk[ _HBMK_cCCPOSTFIX ]   ,;
         "cCCEXT"       => hbmk[ _HBMK_cCCEXT ]       ,;
         "nCmd_Esc"     => hbmk[ _HBMK_nCmd_Esc ]     ,;
         "nScr_Esc"     => hbmk[ _HBMK_nScr_Esc ]     ,;
         "nCmd_FNF"     => hbmk[ _HBMK_nCmd_FNF ]     ,;
         "nScr_FNF"     => hbmk[ _HBMK_nScr_FNF ]     ,;
         "cWorkDir"     => hbmk[ _HBMK_cWorkDir ]     ,;
         "nErrorLevel"  => hbmk[ _HBMK_nErrorLevel ]  ,;
         s_cSecToken    => hbmk                       }

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
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Plugin %1$s returned at '%2$s': '%3$s'" ), cName, PlugIn_ctx_get_state( ctx ), hb_cstr( xResult ) ) )
         ENDIF
         IF ! hbmk[ _HBMK_lIGNOREERROR ]
            lSuccess := .F.
         ENDIF
      ENDIF
   RECOVER USING oError
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Executing plugin: %1$s at %3$s(%4$s)\n'%2$s'" ), cName, hbmk_ErrorMessage( oError ), oError:cargo[ 1 ], hb_ntos( oError:cargo[ 2 ] ) ) )
      ENDIF
   END SEQUENCE

   RETURN lSuccess

STATIC FUNCTION PlugIn_Execute_All( hbmk, cState )
   LOCAL hrb
   LOCAL ctx
   LOCAL lSuccess := .T.

   IF ! Empty( hbmk[ _HBMK_hPLUGINHRB ] )
      ctx := PlugIn_make_ctx( hbmk, cState )
      FOR EACH hrb IN hbmk[ _HBMK_hPLUGINHRB ]
         IF ! PlugIn_call_low( hbmk, hrb:__enumKey(), hrb, ctx )
            lSuccess := .F.
         ENDIF
      NEXT
   ENDIF

   RETURN lSuccess

STATIC FUNCTION hbmk_ErrorMessage( oError )
   /* start error message */
   LOCAL cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   /* add subsystem name if available */
   cMessage += iif( ISCHARACTER( oError:subsystem ), oError:subsystem(), "???" )

   /* add subsystem's error code if available */
   cMessage += "/" + iif( ISNUMBER( oError:subCode ), hb_ntos( oError:subCode ), "???" )

   /* add error description if available */
   IF ISCHARACTER( oError:description )
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
      cExt := ".prg"
   ENDIF

   cFileName := hb_FNameMerge( cDir, cName, cExt )

   RETURN FindInPath( cFileName )

STATIC FUNCTION FindInPath( cFileName, cPath )
   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )
   #if defined( __PLATFORM__WINDOWS ) .OR. ;
       defined( __PLATFORM__DOS ) .OR. ;
       defined( __PLATFORM__OS2 )
      IF Empty( cExt )
         cExt := ".exe"
      ENDIF
   #endif

   /* Check original filename (in supplied path or current dir) */
   IF hb_FileExists( cFileName := hb_FNameMerge( cDir, cName, cExt ) )
      RETURN cFileName
   ENDIF

   /* Check in the dir of this executable. */
   IF ! Empty( hb_DirBase() )
      IF hb_FileExists( cFileName := hb_FNameMerge( hb_DirBase(), cName, cExt ) )
         RETURN cFileName
      ENDIF
   ENDIF

   IF ! ISCHARACTER( cPath )
      cPath := GetEnv( "PATH" )
   ENDIF

   /* Check in the PATH. */
   #if defined( __PLATFORM__WINDOWS ) .OR. ;
       defined( __PLATFORM__DOS ) .OR. ;
       defined( __PLATFORM__OS2 )
   FOR EACH cDir IN hb_ATokens( cPath, hb_osPathListSeparator(), .T., .T. )
   #else
   FOR EACH cDir IN hb_ATokens( cPath, hb_osPathListSeparator() )
   #endif
      IF ! Empty( cDir )
         IF hb_FileExists( cFileName := hb_FNameMerge( DirAddPathSep( StrStripQuote( cDir ) ), cName, cExt ) )
            RETURN cFileName
         ENDIF
      ENDIF
   NEXT

   RETURN NIL

STATIC FUNCTION ArrayJoinNoClone( arraySrc1, arraySrc2 )
   LOCAL nLen1 := Len( arraySrc1 )

   ASize( arraySrc1, nLen1 + Len( arraySrc2 ) )

   RETURN ACopy( arraySrc2, arraySrc1, , , nLen1 + 1 )

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

STATIC FUNCTION AAddNewAtTop( array, xItem )

   IF AScan( array, {| tmp | tmp == xItem } ) == 0
      hb_AIns( array, 1, xItem, .T. )
   ENDIF

   RETURN array

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

STATIC FUNCTION DepTreeToList( aTree )
   LOCAL aList := {}

   DepTreeWorker( aList, aTree )

   RETURN aList

STATIC PROCEDURE DepTreeWorker( aList, aTree )
   LOCAL xItem

   FOR EACH xItem IN aTree DESCEND
      IF ISARRAY( xItem ) .AND. Len( xItem ) == 2
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

   DEFAULT lStripClpAt TO .F.

   IF lStripClpAt
      FOR EACH cFileName IN array
         IF Left( cFileName, 1 ) == "@" .AND. ;
            Lower( FNameExtGet( cFileName ) ) == ".clp"
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
            /* Don't attempt to strip this as it can be valid for libs which have double lib prefixes (f.e. libpng) */
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
               IF IsDigit( SubStr( cExt, 2, 1 ) )
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
            AAdd( aLIB, FNameExtSet( cLibName, cExtNew ) )
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
         cItem := FNameExtSet( cItem, cExtNew )
      NEXT
   ENDIF

   RETURN array

STATIC FUNCTION ArrayToList( array, cSeparator, nEscapeMode, nFNNotation, cPrefix )
   LOCAL cString := ""
   LOCAL tmp

   DEFAULT cSeparator TO " "
   DEFAULT cPrefix TO ""

   IF nEscapeMode == NIL .AND. nFNNotation == NIL
      FOR tmp := 1 TO Len( array )
         cString += cPrefix + array[ tmp ]
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
   ELSE
      FOR tmp := 1 TO Len( array )
         cString += cPrefix + FNameEscape( array[ tmp ], nEscapeMode, nFNNotation )
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

#define _ISDRIVESPEC( cDir ) ( ! Empty( hb_osDriveSeparator() ) .AND. Right( cDir, Len( hb_osDriveSeparator() ) ) == hb_osDriveSeparator() )

/* NOTE: Can hurt if there are symlinks on the way. */
STATIC FUNCTION PathNormalize( cPath )
   LOCAL aDir
   LOCAL cDir

   IF ! Empty( cPath )

      aDir := hb_ATokens( cPath, hb_ps() )

      FOR EACH cDir IN aDir DESCEND
         IF cDir == "."
            hb_ADel( aDir, cDir:__enumIndex(), .T. )
         ELSEIF !( cDir == ".." ) .AND. ;
            ! Empty( cDir ) .AND. ;
            ! _ISDRIVESPEC( cDir )
            IF cDir:__enumIndex() < Len( cDir:__enumBase() ) .AND. ;
               aDir[ cDir:__enumIndex() + 1 ] == ".."
               hb_ADel( aDir, cDir:__enumIndex() + 1, .T. )
               hb_ADel( aDir, cDir:__enumIndex(), .T. )
            ENDIF
         ENDIF
      NEXT

      cPath := ""
      FOR EACH cDir IN aDir
         cPath += cDir
         IF cDir:__enumIndex() < Len( cDir:__enumBase() )
            cPath += hb_ps()
         ENDIF
      NEXT

      IF Empty( cPath )
         cPath := "." + hb_ps()
      ENDIF
   ENDIF

   RETURN cPath

STATIC FUNCTION PathMakeAbsolute( cPathR, cPathA )
   LOCAL cDirA
   LOCAL cDirR, cDriveR, cNameR, cExtR

   IF Empty( cPathA )
      RETURN cPathR
   ENDIF

   hb_FNameSplit( cPathR, @cDirR, @cNameR, @cExtR, @cDriveR )

   IF ! Empty( cDriveR ) .OR. ( ! Empty( cDirR ) .AND. Left( cDirR, 1 ) $ hb_osPathDelimiters() )
      RETURN cPathR
   ENDIF

   hb_FNameSplit( cPathA, @cDirA )

   IF Empty( cDirA )
      RETURN cPathR
   ENDIF

   RETURN hb_FNameMerge( cDirA + cDirR, cNameR, cExtR )

STATIC FUNCTION PathMakeRelative( cPathBase, cPathTarget, lForceRelative )
   LOCAL tmp

   LOCAL aPathBase
   LOCAL aPathTarget

   LOCAL cTestBase
   LOCAL cTestTarget

   LOCAL cTargetFileName

   IF ! ISLOGICAL( lForceRelative )
      lForceRelative := .F.
   ENDIF

   cPathBase   := PathMakeAbsolute( DirAddPathSep( cPathBase ), hb_dirBase() )
   cPathTarget := PathMakeAbsolute( cPathTarget, hb_dirBase() )

   /* TODO: Optimize to operate on strings instead of arrays */

   aPathBase   := FN_ToArray( cPathBase )
   aPathTarget := FN_ToArray( cPathTarget, @cTargetFileName )

   tmp := 1
   cTestBase := ""
   cTestTarget := ""
   DO WHILE tmp <= Len( aPathTarget ) .AND. tmp <= Len( aPathBase )
      cTestBase   += aPathBase[ tmp ]
      cTestTarget += aPathTarget[ tmp ]
      IF ! hb_FileMatch( cTestBase, cTestTarget )
         EXIT
      ENDIF
      ++tmp
   ENDDO

   IF tmp > Len( aPathTarget ) .AND. tmp > Len( aPathBase )
      tmp--
   ENDIF

   IF tmp == Len( aPathBase )
      RETURN FN_FromArray( aPathTarget, tmp, NIL, cTargetFileName )
   ENDIF

   /* Different drive spec. There is no way to solve that using relative dirs. */
   IF ! Empty( hb_osDriveSeparator() ) .AND. ;
      tmp == 1 .AND. ;
      ( Right( aPathBase[ 1 ]  , 1 ) == hb_osDriveSeparator() .OR. ;
        Right( aPathTarget[ 1 ], 1 ) == hb_osDriveSeparator() )
      RETURN cPathTarget
   ENDIF

   /* Force to return relative paths even when base is different. */
   IF lForceRelative
      RETURN FN_FromArray( aPathTarget, tmp, NIL, cTargetFileName, Replicate( ".." + hb_ps(), Len( aPathBase ) - tmp ) )
   ENDIF

   RETURN cPathTarget

STATIC FUNCTION FN_ToArray( cPath, /* @ */ cFileName  )
   LOCAL cDir, cName, cExt

   hb_FNameSplit( cPath, @cDir, @cName, @cExt )

   IF ! Empty( cName ) .OR. ! Empty( cExt )
      cFileName := cName + cExt
   ENDIF

   RETURN hb_ATokens( cDir, hb_ps() )

STATIC FUNCTION FN_FromArray( aPath, nFrom, nTo, cFileName, cDirPrefix )
   LOCAL cDir
   LOCAL tmp

   DEFAULT nFrom      TO 1
   DEFAULT nTo        TO Len( aPath )

   IF nFrom > Len( aPath ) .OR. nTo < 1
      RETURN ""
   ENDIF

   DEFAULT cDirPrefix TO ""

   IF nFrom < 1
      nFrom := 1
   ENDIF

   IF nTo > Len( aPath )
      nTo := Len( aPath )
   ENDIF

   cDir := ""
   FOR tmp := nFrom TO nTo
      cDir += aPath[ tmp ] + hb_ps()
   NEXT

   RETURN hb_FNameMerge( DirDelPathSep( DirAddPathSep( cDirPrefix ) + cDir ), cFileName )

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
   RETURN StrTran( cFileName, "\", "/" )

STATIC FUNCTION PathSepToSelf( cFileName, nStart )
   RETURN iif( nStart == NIL, StrTran( cFileName, iif( hb_ps() == "\", "/", "\" ), hb_ps() ),;
                              Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), iif( hb_ps() == "\", "/", "\" ), hb_ps() ) )

STATIC FUNCTION PathSepToTarget( hbmk, cFileName, nStart )

   IF ! ISNUMBER( nStart )
      nStart := 1
   ENDIF

   IF HBMK_ISPLAT( "win|wce|dos|os2" ) .AND. ! HBMK_ISCOMP( "mingw|mingw64|mingwarm|cygwin" ) /* TOFIX: cygwin is now a platform */
      RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "/", "\" )
   ENDIF

   RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "\", "/" )

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_ps() )
      cDir += hb_ps()
   ENDIF

   RETURN cDir

STATIC FUNCTION DirDelPathSep( cDir )

   IF Empty( hb_osDriveSeparator() )
      DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) == hb_ps()
         cDir := hb_StrShrink( cDir, 1 )
      ENDDO
   ELSE
      DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) == hb_ps() .AND. ;
               !( Right( cDir, 2 ) == hb_osDriveSeparator() + hb_ps() )
         cDir := hb_StrShrink( cDir, 1 )
      ENDDO
   ENDIF

   RETURN cDir

STATIC FUNCTION DirBuild( cDir )
   LOCAL cDirTemp
   LOCAL cDirItem
   LOCAL tmp

   cDir := PathNormalize( PathSepToSelf( cDir ) )

   IF ! hb_DirExists( cDir )

      cDir := DirAddPathSep( cDir )

      IF ! Empty( hb_osDriveSeparator() ) .AND. ;
         ( tmp := At( hb_osDriveSeparator(), cDir ) ) > 0
         cDirTemp := Left( cDir, tmp )
         cDir := SubStr( cDir, tmp + 1 )
      ELSEIF Left( cDir, 1 ) == hb_ps()
         cDirTemp := Left( cDir, 1 )
         cDir := SubStr( cDir, 2 )
      ELSE
         cDirTemp := ""
      ENDIF

      FOR EACH cDirItem IN hb_ATokens( cDir, hb_ps() )
         IF !( Right( cDirTemp, 1 ) == hb_ps() ) .AND. ! Empty( cDirTemp )
            cDirTemp += hb_ps()
         ENDIF
         IF ! Empty( cDirItem )  /* Skip root path, if any */
            cDirTemp += cDirItem
            IF hb_FileExists( cDirTemp )
               RETURN .F.
            ELSEIF ! hb_DirExists( cDirTemp )
               IF hb_DirCreate( cDirTemp ) != 0
                  RETURN .F.
               ENDIF
               #if ! defined( __PLATFORM__UNIX )
                  IF Lower( Left( cDirItem, Len( _WORKDIR_BASE_ ) ) ) == _WORKDIR_BASE_
                     hb_FSetAttr( cDirTemp, FC_HIDDEN )
                  ENDIF
               #endif
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN .T.

STATIC FUNCTION DirUnbuild( cDir )
   LOCAL cDirTemp
   LOCAL tmp

   IF hb_DirExists( cDir )

      cDir := DirDelPathSep( cDir )

      cDirTemp := cDir
      DO WHILE ! Empty( cDirTemp )
         IF hb_DirExists( cDirTemp )
            IF hb_DirDelete( cDirTemp ) != 0
               RETURN .F.
            ENDIF
         ENDIF
         IF ( tmp := RAt( hb_ps(), cDirTemp ) ) == 0
            EXIT
         ENDIF
         cDirTemp := Left( cDirTemp, tmp - 1 )
         IF ! Empty( hb_osDriveSeparator() ) .AND. ;
            Right( cDirTemp, 1 ) == hb_osDriveSeparator()
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN .T.

STATIC FUNCTION FNameEscape( cFileName, nEscapeMode, nFNNotation )
   LOCAL cDir, cName, cExt, cDrive

   IF ! ISNUMBER( nEscapeMode )
      nEscapeMode := _ESC_NONE
   ENDIF
   IF ! ISNUMBER( nFNNotation )
#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
      nFNNotation := _FNF_BCKSLASH
#else
      nFNNotation := _FNF_FWDSLASH
#endif
   ENDIF

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
      cFileName := "'" + StrTran( cFileName, "'", "'\''" ) + "'"
      EXIT
   CASE _ESC_BCKSLASH
      cFileName := StrTran( cFileName, "\", "\\" )
      EXIT
   ENDSWITCH

   RETURN cFileName

STATIC FUNCTION FNameDirGet( cFileName )
   LOCAL cDir

   hb_FNameSplit( cFileName, @cDir )

   RETURN cDir

STATIC FUNCTION FNameNameGet( cFileName )
   LOCAL cName

   hb_FNameSplit( cFileName,, @cName )

   RETURN cName

STATIC FUNCTION FNameNameExtGet( cFileName )
   LOCAL cName, cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )

   RETURN hb_FNameMerge( NIL, cName, cExt )

STATIC FUNCTION FNameExtGet( cFileName )
   LOCAL cExt

   hb_FNameSplit( cFileName,,, @cExt )

   RETURN cExt

STATIC FUNCTION FNameExtDef( cFileName, cDefExt )
   LOCAL cDir, cName, cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )
   IF Empty( cExt )
      cExt := cDefExt
   ENDIF

   RETURN hb_FNameMerge( cDir, cName, cExt )

STATIC FUNCTION FNameExtSet( cFileName, cExt )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName, cExt )

STATIC FUNCTION FNameDirExtSet( cFileName, cDirNew, cExtNew )
   LOCAL cDir, cName, cExt

   hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

   IF cDirNew != NIL
      cDir := cDirNew
   ENDIF
   IF cExtNew != NIL
      cExt := cExtNew
   ENDIF

   RETURN hb_FNameMerge( cDir, cName, cExt )

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
      AAdd( aFilelist, hb_FNameMerge( FNameDirGet( cFileName ), aFile[ F_NAME ] ) )
   NEXT

   RETURN aFileList

STATIC FUNCTION FNameHasWildcard( cFileName )
   RETURN "?" $ cFileName .OR. ;
          "*" $ cFileName

STATIC PROCEDURE HBC_ProcessAll( hbmk )
   LOCAL cDir
   LOCAL cFileName

   LOCAL aCFGDirs

   #if defined( __PLATFORM__UNIX )
      aCFGDirs := { DirAddPathSep( GetEnv( "HOME" ) ) + ".harbour",;
                    "/etc/harbour",;
                    DirAddPathSep( hb_DirBase() ) + "../etc/harbour",;
                    DirAddPathSep( hb_DirBase() ) + "../etc",;
                    hb_DirBase() }
   #else
      aCFGDirs := { hb_DirBase() }
   #endif

   FOR EACH cDir IN aCFGDirs
      IF hb_FileExists( cFileName := ( PathNormalize( DirAddPathSep( cDir ) ) + _HBMK_AUTOHBC_NAME ) )
         IF ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing configuration: %1$s" ), cFileName ) )
         ENDIF
         HBC_ProcessOne( hbmk, cFileName, 1 )
         EXIT
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION HBC_ProcessOne( hbmk, cFileName, nNestingLevel )
   LOCAL cFile
   LOCAL cLine
   LOCAL cItem
   LOCAL cItemL
   LOCAL cName
   LOCAL tmp

#if defined( __PLATFORM__DOS )
   IF ! hbmk_dos_FileExists( cFileName )
#else
   IF ! hb_FileExists( cFileName )
#endif
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Opening: %1$s" ), cFileName ) )
      RETURN .F.
   ENDIF

   AAddNew( hbmk[ _HBMK_aDEPTHBC ], { cFileName, nNestingLevel - 1 } )

   cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */

   IF !( hb_eol() == _CHR_EOL )
      cFile := StrTran( cFile, hb_eol(), _CHR_EOL )
   ENDIF
   IF !( hb_eol() == Chr( 13 ) + Chr( 10 ) )
      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _CHR_EOL )
   ENDIF

   FOR EACH cLine IN hb_ATokens( cFile, _CHR_EOL )

      cLine := AllTrim( ArchCompFilter( hbmk, AllTrim( cLine ) ) )

      DO CASE
      CASE Lower( Left( cLine, Len( "skip="        ) ) ) == "skip="          ; cLine := SubStr( cLine, Len( "skip="         ) + 1 )

         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + _OUT_EOL )
         ENDIF

         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Skipping from: %1$s" ), cFileName ) )
         ENDIF
         EXIT

      CASE Lower( Left( cLine, Len( "sources="     ) ) ) == "sources="       ; cLine := SubStr( cLine, Len( "sources="      ) + 1 )

         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               cItem := PathNormalize( PathMakeAbsolute( PathSepToSelf( cItem ), FNameDirGet( cFileName ) ) )
               cItemL := Lower( cItem )
               DO CASE
               CASE FNameExtGet( cItemL ) == ".o" .OR. ;
                    FNameExtGet( cItemL ) == ".obj"
                  AAddNew( hbmk[ _HBMK_aOBJUSER ], cItem )
               CASE FNameExtGet( cItemL ) == ".cpp" .OR. ;
                    FNameExtGet( cItemL ) == ".cc" .OR. ;
                    FNameExtGet( cItemL ) == ".cxx" .OR. ;
                    FNameExtGet( cItemL ) == ".cx" .OR. ;
                    _EXT_IS_UPPER( cItem, ".C" )
                  AAddNew( hbmk[ _HBMK_aCPP ], cItem )
               CASE FNameExtGet( cItemL ) == ".c" .OR. ;
                    FNameExtGet( cItemL ) == ".m"
                  AAddNew( hbmk[ _HBMK_aC ], cItem )
               CASE FNameExtGet( cItemL ) == ".d"
                  deplst_read( hbmk, hbmk[ _HBMK_hDEPTS ], cItem )
               CASE FNameExtGet( cItemL ) == ".po" .OR. ;
                    FNameExtGet( cItemL ) == ".pot"
                  AAddNew( hbmk[ _HBMK_aPO ], cItem )
               CASE FNameExtGet( cItemL ) == ".rc"
                  FOR EACH tmp IN FN_Expand( cItem, .F. )
                     AAddNew( hbmk[ _HBMK_aRESSRC ], tmp )
                  NEXT
               CASE FNameExtGet( cItemL ) == ".def"
                  FOR EACH tmp IN FN_Expand( cItem, .F.  )
                     AAddNew( hbmk[ _HBMK_aDEF ], tmp )
                  NEXT
               CASE FNameExtGet( cItemL ) == ".res"
                  IF HBMK_ISCOMP( "mingw|mingw64|mingwarm" )
                     /* For MinGW family add .res files as source input, as they
                        will need to be converted to coff format with windres (just
                        like plain .rc files) before feeding them to gcc. */
                     FOR EACH tmp IN FN_Expand( cItem, .F.  )
                        AAddNew( hbmk[ _HBMK_aRESSRC ], tmp )
                     NEXT
                  ELSE
                     FOR EACH tmp IN FN_Expand( cItem, .F.  )
                        AAddNew( hbmk[ _HBMK_aRESCMP ], tmp )
                     NEXT
                  ENDIF
               CASE FNameExtGet( cItemL ) $ hbmk[ _HBMK_hPLUGINExt ]
                  FOR EACH tmp IN FN_Expand( cItem, .F. )
                     AAddNew( hbmk[ _HBMK_aPLUGINPars ], tmp )
                  NEXT
               OTHERWISE /* .prg */
                  IF Empty( FNameExtGet( cItem ) )
                     cItem := FNameExtSet( cItem, ".prg" )
                  ENDIF
                  AAddNew( hbmk[ _HBMK_aPRG ], cItem )
               ENDCASE
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libs="         ) ) ) == "libs="         ; cLine := SubStr( cLine, Len( "libs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF FNameExtGet( cItem ) == ".hbc"
               cItem := PathMakeAbsolute( PathSepToSelf( cItem ), FNameDirGet( cFileName ) )
               IF nNestingLevel < _HBMK_NEST_MAX
                  IF ! hb_FileExists( cItem )
                     FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                        IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cItem ) )
                           cItem := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cItem )
                           EXIT
                        ENDIF
                     NEXT
                  ENDIF

                  cItem := PathNormalize( cItem )

                  IF hbmk[ _HBMK_lInfo ]
                     hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cItem ) )
                  ENDIF

                  HBC_ProcessOne( hbmk, cItem, nNestingLevel + 1 )
               ELSE
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
               ENDIF
            ELSE
               cItem := PathSepToSelf( cItem )
               IF _IS_AUTOLIBSYSPRE( cItem )
                  AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSERSYSPRE ], cItem )
               ELSE
                  AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSER ], cItem )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "syslibs="      ) ) ) == "syslibs="      ; cLine := SubStr( cLine, Len( "syslibs="      ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aLIBUSERSYS ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "hbcs="         ) ) ) == "hbcs="         ; cLine := SubStr( cLine, Len( "hbcs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            IF nNestingLevel < _HBMK_NEST_MAX

               cItem := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) ), FNameDirGet( cFileName ) )

               IF Empty( FNameExtGet( cItem ) )
                  cItem := FNameExtSet( cItem, ".hbc" )
               ENDIF

               IF ! hb_FileExists( cItem )
                  FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                     IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cItem ) )
                        cItem := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cItem )
                        EXIT
                     ENDIF
                  NEXT
               ENDIF

               cItem := PathNormalize( cItem )

               IF hbmk[ _HBMK_lInfo ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cItem ) )
               ENDIF

               HBC_ProcessOne( hbmk, cItem, nNestingLevel + 1 )
            ELSE
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "autohbcs="     ) ) ) == "autohbcs="     ; cLine := SubStr( cLine, Len( "autohbcs="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF autohbc_split_arg( cItem, @cName, @cItem )

               cItem := PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) ), FNameDirGet( cFileName ) )

               IF Empty( FNameExtGet( cName ) )
                  cName := FNameExtSet( cName, ".ch" )
               ENDIF
               IF Empty( FNameExtGet( cItem ) )
                  cItem := FNameExtSet( cItem, ".hbc" )
               ENDIF

               IF ! hb_FileExists( cItem )
                  FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                     IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cItem ) )
                        cItem := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, _MACRO_LATE_PREFIX ) ) ) + FNameNameExtGet( cItem )
                        EXIT
                     ENDIF
                  NEXT
               ENDIF

               hbmk[ _HBMK_hAUTOHBC ][ AllTrim( StrTran( cName, "\", "/" ) ) ] := cItem
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libpaths="     ) ) ) == "libpaths="     ; cLine := SubStr( cLine, Len( "libpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               AAddNew( hbmk[ _HBMK_aLIBPATH ], DirDelPathSep( PathNormalize( PathMakeAbsolute( PathSepToSelf( cItem ), FNameDirGet( cFileName ) ) ) ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "incpaths="     ) ) ) == "incpaths="     ; cLine := SubStr( cLine, Len( "incpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               AAddNew( hbmk[ _HBMK_aINCPATH ], DirDelPathSep( PathNormalize( PathMakeAbsolute( PathSepToSelf( cItem ), FNameDirGet( cFileName ) ) ) ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "instfiles="    ) ) ) == "instfiles="    ; cLine := SubStr( cLine, Len( "instfiles="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF inst_split_arg( cItem, @cName, @cItem )
               cItem := PathNormalize( PathMakeAbsolute( cItem, FNameDirGet( cFileName ) ) )
               FOR EACH tmp IN FN_Expand( cItem, .F. )
                  AAddNewINST( hbmk[ _HBMK_aINSTFILE ], { cName, tmp } )
               NEXT
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "instpaths="    ) ) ) == "instpaths="    ; cLine := SubStr( cLine, Len( "instpaths="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF inst_split_arg( cItem, @cName, @cItem )
               AAddNewINST( hbmk[ _HBMK_aINSTPATH ], { cName, PathNormalize( PathMakeAbsolute( PathSepToSelf( cItem ), FNameDirGet( cFileName ) ) ) } )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "echo="         ) ) ) == "echo="         ; cLine := SubStr( cLine, Len( "echo="         ) + 1 )
         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + _OUT_EOL )
         ENDIF

      CASE Lower( Left( cLine, Len( "stop="        ) ) ) == "stop="          ; cLine := SubStr( cLine, Len( "stop="         ) + 1 )

         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + _OUT_EOL )
         ENDIF

         hbmk[ _HBMK_lStopAfterInit ] := .T.
         hbmk[ _HBMK_lRUN ] := .F.
         hbmk[ _HBMK_nErrorLevel ] := _ERRLEV_STOP
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
            AAddNewNotEmpty( hbmk[ _HBMK_aOPTL ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "pflags="       ) ) ) == "pflags="       ; cLine := SubStr( cLine, Len( "pflags="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            AAddNewNotEmpty( hbmk[ _HBMK_aPLUGINPars ], MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
         NEXT

      CASE Lower( Left( cLine, Len( "psources="     ) ) ) == "psources="     ; cLine := SubStr( cLine, Len( "psources="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF ! Empty( cItem )
               AAddNew( hbmk[ _HBMK_aPLUGINPars ], PathNormalize( PathMakeAbsolute( PathSepToSelf( cItem ), FNameDirGet( cFileName ) ) ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "gui="          ) ) ) == "gui="          ; cLine := SubStr( cLine, Len( "gui="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lGUI ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lGUI ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "mt="           ) ) ) == "mt="           ; cLine := SubStr( cLine, Len( "mt="           ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "mt" ; hbmk[ _HBMK_lMT ] := .T. /* Compatibility */
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lMT ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lMT ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "shareddef="    ) ) ) == "shareddef="    ; cLine := SubStr( cLine, Len( "shareddef="    ) + 1 )
         IF hbmk[ _HBMK_lSHARED ] == NIL
            DO CASE
            CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSHARED ] := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
            CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
            ENDCASE
         ENDIF
      CASE Lower( Left( cLine, Len( "shared="       ) ) ) == "shared="       ; cLine := SubStr( cLine, Len( "shared="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSHARED ] := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "fullstatic="   ) ) ) == "fullstatic="   ; cLine := SubStr( cLine, Len( "fullstatic="   ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSHARED ] := .F. ; hbmk[ _HBMK_lSTATICFULL ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "debug="        ) ) ) == "debug="        ; cLine := SubStr( cLine, Len( "debug="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lDEBUG ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lDEBUG ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "optim="        ) ) ) == "optim="        ; cLine := SubStr( cLine, Len( "optim="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lOPTIM ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lOPTIM ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "nulrdd="       ) ) ) == "nulrdd="       ; cLine := SubStr( cLine, Len( "nulrdd="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lNULRDD ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lNULRDD ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "map="          ) ) ) == "map="          ; cLine := SubStr( cLine, Len( "map="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lMAP ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lMAP ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "implib="       ) ) ) == "implib="       ; cLine := SubStr( cLine, Len( "implib="       ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lIMPLIB ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lIMPLIB ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "hbcppmm="      ) ) ) == "hbcppmm="      ; cLine := SubStr( cLine, Len( "hbcppmm="      ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lHBCPPMM ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lHBCPPMM ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "strip="        ) ) ) == "strip="        ; cLine := SubStr( cLine, Len( "strip="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lSTRIP ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lSTRIP ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "cpp="          ) ) ) == "cpp="          ; cLine := SubStr( cLine, Len( "cpp="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )       ; hbmk[ _HBMK_lCPP ] := .T.
         CASE ValueIsF( cLine )       ; hbmk[ _HBMK_lCPP ] := .F.
         CASE Lower( cLine ) == "def" ; hbmk[ _HBMK_lCPP ] := NIL
         ENDCASE

      CASE Lower( Left( cLine, Len( "warn="         ) ) ) == "warn="         ; cLine := SubStr( cLine, Len( "warn="         ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )        ; hbmk[ _HBMK_nWARN ] := _WARN_YES
         CASE ValueIsF( cLine )        ; hbmk[ _HBMK_nWARN ] := _WARN_NO
         CASE Lower( cLine ) == "low"  ; hbmk[ _HBMK_nWARN ] := _WARN_LOW
         CASE Lower( cLine ) == "max"  ; hbmk[ _HBMK_nWARN ] := _WARN_MAX
         CASE Lower( cLine ) == "def"  ; hbmk[ _HBMK_nWARN ] := _WARN_DEF
         ENDCASE

      CASE Lower( Left( cLine, Len( "compr="        ) ) ) == "compr="        ; cLine := SubStr( cLine, Len( "compr="        ) + 1 )
         DO CASE
         CASE ValueIsT( cLine )       ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
         CASE ValueIsF( cLine )       ; hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF
         CASE Lower( cLine ) == "def" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
         CASE Lower( cLine ) == "min" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MIN
         CASE Lower( cLine ) == "max" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MAX
         ENDCASE

      CASE Lower( Left( cLine, Len( "head="         ) ) ) == "head="         ; cLine := SubStr( cLine, Len( "head="         ) + 1 )
         DO CASE
         CASE Lower( cLine ) == "off"     ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF
         CASE Lower( cLine ) == "full"    ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
         CASE Lower( cLine ) == "native"  ; hbmk[ _HBMK_nHEAD ] := _HEAD_NATIVE
         ENDCASE

      CASE Lower( Left( cLine, Len( "run="          ) ) ) == "run="          ; cLine := SubStr( cLine, Len( "run="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lRUN ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lRUN ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "inc="          ) ) ) == "inc="          ; cLine := SubStr( cLine, Len( "inc="          ) + 1 )
         DO CASE
         CASE ValueIsT( cLine ) ; hbmk[ _HBMK_lINC ] := .T.
         CASE ValueIsF( cLine ) ; hbmk[ _HBMK_lINC ] := .F.
         ENDCASE

      CASE Lower( Left( cLine, Len( "plugins="      ) ) ) == "plugins="      ; cLine := SubStr( cLine, Len( "plugins="      ) + 1 )

         cLine := PathNormalize( PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, cLine, cFileName ) ), FNameDirGet( cFileName ) ) )
         IF ( tmp := FindInPathPlugIn( cLine ) ) != NIL
            PlugIn_Load( hbmk, tmp )
         ELSE
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Warning: Plugin not found: %1$s" ), cLine ) )
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
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], PathSepToSelf( cLine ) )
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
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], PathSepToSelf( cLine ) )
               ENDIF
            ENDIF
         ENDIF

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

      CASE Lower( Left( cLine, Len( "depincpath="   ) ) ) == "depincpath="   ; cLine := SubStr( cLine, Len( "depincpath="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aINCPATH ], PathNormalize( PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, cItem, cFileName ) ), FNameDirGet( cFileName ) ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depincpathlocal=" ) ) ) == "depincpathlocal=" ; cLine := SubStr( cLine, Len( "depincpathlocal=" ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aINCPATHLOCAL ], PathNormalize( PathMakeAbsolute( PathSepToSelf( MacroProc( hbmk, cItem, cFileName ) ), FNameDirGet( cFileName ) ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depimplibs="   ) ) ) == "depimplibs="   ; cLine := SubStr( cLine, Len( "depimplibs="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            FOR EACH cItem IN hb_ATokens( cLine,, .T. )
               AAddNewNotEmpty( hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_aIMPLIBSRC ], PathSepToSelf( MacroProc( hbmk, cItem, cFileName ) ) )
            NEXT
         ENDIF

      CASE Lower( Left( cLine, Len( "depimplibd="   ) ) ) == "depimplibd="   ; cLine := SubStr( cLine, Len( "depimplibd="   ) + 1 )

         IF dep_split_arg( hbmk, cLine, @cName, @cLine )
            hbmk[ _HBMK_hDEP ][ cName ][ _HBMKDEP_cIMPLIBDST ] := FNameNameExtGet( PathSepToSelf( cLine ) )
         ENDIF

      /* .hbc identification strings. Similar to pkgconfig ones. */
      CASE Lower( Left( cLine, Len( "name="         ) ) ) == "name="         ; cLine := SubStr( cLine, Len( "name="         ) + 1 )

         /* Silently ignore */

      CASE Lower( Left( cLine, Len( "description="  ) ) ) == "description="  ; cLine := SubStr( cLine, Len( "description="  ) + 1 )

         /* Silently ignore */

      CASE Lower( Left( cLine, Len( "version="      ) ) ) == "version="      ; cLine := SubStr( cLine, Len( "version="      ) + 1 )

         /* Silently ignore */

      ENDCASE
   NEXT

   RETURN .T.

STATIC FUNCTION IsGTRequested( hbmk, cWhichGT )
   /* Check if it's a core/user GT. */
   RETURN AScan( hbmk[ _HBMK_aLIBCOREGT ], {| tmp | Lower( tmp ) == cWhichGT } ) > 0 .OR. ;
          AScan( hbmk[ _HBMK_aLIBUSERGT ], {| tmp | Lower( tmp ) == cWhichGT } ) > 0

STATIC FUNCTION StrStripQuote( cString )
   RETURN iif( Left( cString, 1 ) == '"' .AND. Right( cString, 1 ) == '"',;
               SubStr( cString, 2, Len( cString ) - 2 ),;
               cString )

STATIC FUNCTION ValueIsT( cString )
   cString := Lower( cString )
   RETURN cString == "yes" .OR. ;
          cString == "1" /* Compatibility */

STATIC FUNCTION ValueIsF( cString )
   cString := Lower( cString )
   RETURN cString == "no" .OR. ;
          cString == "0" /* Compatibility */

STATIC FUNCTION HBM_Load( hbmk, aParams, cFileName, nNestingLevel, lProcHBP )
   LOCAL cFile
   LOCAL cLine
   LOCAL cParam
   LOCAL aArgs
   LOCAL nResult
   LOCAL cHBP

#if defined( __PLATFORM__DOS )
   IF hbmk_dos_FileExists( cFileName )
#else
   IF hb_FileExists( cFileName )
#endif

      cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */

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
                       !( Lower( FNameExtGet( cParam ) ) == ".clp" )
                     IF nNestingLevel < _HBMK_NEST_MAX
                        cParam := SubStr( cParam, 2 )
                        IF Empty( FNameExtGet( cParam ) )
                           cParam := FNameExtSet( cParam, ".hbm" )
                        ENDIF
                        /* TODO: Modify '@script.ext' (@ prefixes) inclusion to not inherit path from parent */
                        nResult := HBM_Load( hbmk, aParams, PathMakeAbsolute( PathSepToSelf( cParam ), cFileName ), nNestingLevel + 1, .T. ) /* Load parameters from script file */
                        IF nResult != _ERRLEV_OK .AND. ;
                           nResult != _ERRLEV_STOP
                           RETURN nResult
                        ENDIF
                     ELSE
                        hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
                     ENDIF
                  CASE !( Left( cParam, 1 ) == "-" ) .AND. ;
                       Lower( FNameExtGet( cParam ) ) == ".hbm"
                     IF nNestingLevel < _HBMK_NEST_MAX
                        nResult := HBM_Load( hbmk, aParams, PathMakeAbsolute( PathSepToSelf( cParam ), cFileName ), nNestingLevel + 1, .T. ) /* Load parameters from script file */
                        IF nResult != _ERRLEV_OK .AND. ;
                           nResult != _ERRLEV_STOP
                           RETURN nResult
                        ENDIF
                     ELSE
                        hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
                     ENDIF
                  CASE !( Left( cParam, 1 ) == "-" ) .AND. ;
                       Lower( FNameExtGet( cParam ) ) == ".hbp"
                     cHBP := PathMakeAbsolute( PathSepToSelf( cParam ), cFileName )
                     IF lProcHBP
                        IF hbmk[ _HBMK_nArgTarget ] > 0
                           IF hb_FileExists( cHBP )
                              aArgs := AClone( hbmk[ _HBMK_aArgs ] )
                              aArgs[ hbmk[ _HBMK_nArgTarget ] ] := cHBP
                              nResult := hbmk2( aArgs, hbmk[ _HBMK_nArgTarget ], @hbmk[ _HBMK_lPause ], hbmk[ _HBMK_nLevel ] + 1 )
                              IF nResult != 0
                                 RETURN nResult
                              ENDIF
                           ELSE
                              hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Project reference (%1$s) ignored. File not found." ), cHBP ) )
                           ENDIF
                        ELSE
                           hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Project reference (%1$s) ignored. Project references require hbmk2 to be invoced with a main project." ), cHBP ) )
                        ENDIF
                     ELSE
                        hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Project reference (%1$s) ignored in automatic make file: %2$s" ), cHBP, cFileName ) )
                     ENDIF
                  OTHERWISE
                     AAdd( aParams, { cParam, cFileName, cLine:__enumIndex() } )
                  ENDCASE
               ENDIF
            NEXT
         ENDIF
      NEXT
   ELSE
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: File cannot be found: %1$s" ), cFileName ) )
   ENDIF

   RETURN 0

/* Filter microformat:
   {[!][<plat|comp>]['&'|'|'][...]}
*/

STATIC FUNCTION ArchCompFilter( hbmk, cItem )
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

   LOCAL cExpr := "hbmk_KEYW( hbmk, '%1' )"
   LOCAL cExprWithValue := "hbmk_KEYW( hbmk, '%1', '%2', '%3' )"
   LOCAL tmp

   IF ( nStart := At( _MACRO_OPEN, cItem ) ) > 0 .AND. ;
      !( SubStr( cItem, nStart - 1, 1 ) $ _MACRO_PREFIX_ALL ) .AND. ;
      ( nEnd := hb_At( _MACRO_CLOSE, cItem, nStart + Len( _MACRO_OPEN ) ) ) > 0

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
         FOR EACH cChar IN cFilterSrc
            IF cValue == NIL
               IF iif( Empty( cKeyword ),;
                     HB_ISFIRSTIDCHAR( cChar ),;
                     HB_ISNEXTIDCHAR( cChar ) )
                  cKeyword += cChar
               ELSEIF cChar $ "=<>" .AND. SubStr( cFilterSrc, cChar:__enumIndex() + 1, 1 ) == "'"
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
                     tmp := StrTran( tmp, "%1", cKeyword )
                     tmp := StrTran( tmp, "%2", cValue )
                     tmp := StrTran( tmp, "%3", cOperator )
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
               tmp := StrTran( tmp, "%1", cKeyword )
               tmp := StrTran( tmp, "%2", cValue )
               tmp := StrTran( tmp, "%3", cOperator )
               cFilterHarb += tmp
            ELSE
               cFilterHarb += StrTran( cExpr, "%1", cKeyword )
            ENDIF
         ENDIF

         cFilterHarb := StrTran( cFilterHarb, "&&", "&" )
         cFilterHarb := StrTran( cFilterHarb, "||", "|" )

         cFilterHarb := StrTran( cFilterHarb, "&", ".AND." )
         cFilterHarb := StrTran( cFilterHarb, "|", ".OR." )

         /* Evaluate filter */
         BEGIN SEQUENCE WITH {| oError | Break( oError ) }
            bFilter := &( "{| hbmk |" + cFilterHarb + "}" )
         RECOVER
            bFilter := NIL
         END SEQUENCE

         IF ISBLOCK( bFilter ) .AND. ISLOGICAL( xResult := Eval( bFilter, hbmk ) )
            IF xResult
               RETURN cItem
            ENDIF
         ENDIF

         RETURN ""
      ENDIF
   ENDIF

   RETURN cItem

STATIC FUNCTION hb_pwd()
   RETURN DirAddPathSep( hb_CurDrive() + hb_osDriveSeparator() + hb_ps() + CurDir() )

STATIC FUNCTION MacroProc( hbmk, cString, cFileName, cMacroPrefix )
   LOCAL nStart
   LOCAL nEnd
   LOCAL cMacro

   LOCAL cStart := iif( ISCHARACTER( cMacroPrefix ), cMacroPrefix, _MACRO_NORM_PREFIX ) + _MACRO_OPEN

   LOCAL cStdOut

   DO WHILE ( nStart := At( cStart, cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( _MACRO_CLOSE, cString, nStart + Len( cStart ) ) ) > 0

      cMacro := MacroGet( hbmk, SubStr( cString, nStart + Len( cStart ), nEnd - nStart - Len( cStart ) ), cFileName )

      cString := Left( cString, nStart - 1 ) + cMacro + SubStr( cString, nEnd + Len( _MACRO_CLOSE ) )
   ENDDO

   DO WHILE ( nStart := At( _CMDSUBST_OPEN, cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( _CMDSUBST_CLOSE, cString, nStart + Len( _CMDSUBST_OPEN ) ) ) > 0
      cMacro := SubStr( cString, nStart + Len( _CMDSUBST_OPEN ), nEnd - nStart - Len( _CMDSUBST_OPEN ) )
      cStdOut := ""
      IF ! Empty( cMacro )
         hb_processRun( cMacro,, @cStdOut )
      ENDIF
      cString := Left( cString, nStart - 1 ) + cStdOut + SubStr( cString, nEnd + Len( _CMDSUBST_CLOSE ) )
   ENDDO

   RETURN cString

STATIC FUNCTION MacroGet( hbmk, cMacro, cFileName )

   SWITCH Upper( cMacro )
   CASE "HB_ROOT"
      cMacro := DirAddPathSep( hb_DirBase() ) ; EXIT
   CASE "HB_DIR"
      cMacro := PathSepToSelf( FNameDirGet( cFileName ) ) ; EXIT
   CASE "HB_DIRNAME"
      cMacro := FNameNameGet( DirDelPathSep( PathSepToSelf( FNameDirGet( cFileName ) ) ) ) ; EXIT
   CASE "HB_NAME"
      cMacro := PathSepToSelf( FNameNameGet( cFileName ) ) ; EXIT
   CASE "HB_SELF"
      cMacro := PathSepToSelf( cFileName ) ; EXIT
   CASE "HB_CURDIR"
      cMacro := hb_pwd() ; EXIT
   CASE "HB_TEMPDIR"
      cMacro := hb_DirTemp() ; EXIT
   CASE "HB_TARGETNAME"
      cMacro := FNameNameGet( PathSepToSelf( hbmk_TARGETNAME( hbmk ) ) ) ; EXIT
   CASE "HB_TARGETTYPE"
      cMacro := hbmk_TARGETTYPE( hbmk ) ; EXIT
   CASE "HB_PLAT"
   CASE "HB_PLATFORM" /* Compatibility */
   CASE "HB_ARCH" /* Compatibility */
      cMacro := hbmk[ _HBMK_cPLAT ] ; EXIT
   CASE "HB_COMP"
   CASE "HB_COMPILER" /* Compatibility */
      cMacro := hbmk[ _HBMK_cCOMP ] ; EXIT
   CASE "HB_COMP_VER"
      cMacro := hb_ntos( hbmk[ _HBMK_nCOMPVer ] ) ; EXIT
   CASE "HB_BUILD"
      cMacro := hbmk[ _HBMK_cBUILD ] ; EXIT
   CASE "HB_CPU"
      cMacro := hbmk[ _HBMK_cCPU ] ; EXIT
   CASE "HB_WORK"
      cMacro := _WORKDIR_BASE_ ; EXIT
   CASE "HB_WORKDYNSUB"
      cMacro := hbmk[ _HBMK_cWorkDirDynSub ] ; EXIT
   CASE "HB_DYNPREFIX"
      cMacro := hbmk[ _HBMK_cDynLibPrefix ] ; EXIT
   CASE "HB_DYNSUFFIX"
      cMacro := hbmk_DYNSUFFIX( hbmk ) ; EXIT
   CASE "HB_DYNEXT"
      cMacro := hbmk[ _HBMK_cDynLibExt ] ; EXIT
   CASE "HB_MAJOR"
      cMacro := hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) ; EXIT
   CASE "HB_MINOR"
      cMacro := hb_ntos( hb_Version( HB_VERSION_MINOR ) ) ; EXIT
   CASE "HB_RELEASE"
      cMacro := hb_ntos( hb_Version( HB_VERSION_RELEASE ) ) ; EXIT
   CASE "HB_STATUS"
      cMacro := hb_Version( HB_VERSION_STATUS ) ; EXIT
   CASE "HB_REVISION"
      cMacro := hb_ntos( hb_Version( HB_VERSION_REVISION ) ) ; EXIT
   CASE "HB_HOST_PLAT"
      cMacro := hb_Version( HB_VERSION_PLATFORM ) ; EXIT
   CASE "HB_HOST_PLAT_UNIX"
      cMacro := iif( hb_Version( HB_VERSION_UNIX_COMPAT ), "1", "" ) ; EXIT
   CASE "HB_BIN"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_BIN ] ; EXIT
   CASE "HB_LIB"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_LIB ] ; EXIT
   CASE "HB_DYN"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_DYN ] ; EXIT
   CASE "HB_INC"
      cMacro := hbmk[ _HBMK_cHB_INSTALL_INC ] ; EXIT
   CASE "HB_FIRST"
      cMacro := FNameNameGet( hbmk[ _HBMK_cFIRST ] ) ; EXIT
   CASE "HB_OUTPUTDIR"
      cMacro := iif( ISCHARACTER( hbmk[ _HBMK_cPROGDIR ] ), FNameDirGet( hbmk[ _HBMK_cPROGDIR ] ), "" ) ; EXIT
   CASE "HB_OUTPUTNAME"
      cMacro := iif( ISCHARACTER( hbmk[ _HBMK_cPROGNAME ] ), FNameNameGet( hbmk[ _HBMK_cPROGNAME ] ), "" ) ; EXIT
   CASE "HB_LEVEL"
      cMacro := hb_ntos( hbmk[ _HBMK_nLevel ] ) ; EXIT
   OTHERWISE
      IF cMacro $ hbmk[ _HBMK_hDEPTMACRO ] /* Check for dependency detection macros */
         cMacro := "1"
      ELSE
         /* NOTE: If macro not found, try to interpret as
                  envvar. If it doesn't exist, empty string
                  will be returned (without warning) [vszakats] */
         cMacro := GetEnv( cMacro )
      ENDIF
   ENDSWITCH

   IF ! ISCHARACTER( cMacro )
      cMacro := ""
   ENDIF

   RETURN cMacro

STATIC FUNCTION TimeElapsed( nStartSec, nEndSec )
   RETURN Round( ( nEndSec - iif( nEndSec < nStartSec, nStartSec - 86399, nStartSec ) ), 1 )

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
   LOCAL cResult, c

   cResult := ""
   FOR EACH c IN cName
      IF c == "_" .OR. IsAlpha( c ) .OR. ( ! cResult == "" .AND. IsDigit( c ) )
         cResult += c
      ELSE
         cResult += "x" + Lower( hb_NumToHex( Asc( c ), 2 ) )
      ENDIF
   NEXT
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
   IF HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|cygwin|gccomf" ) /* TOFIX: cygwin is now a platform */
      hb_FNameSplit( cFile,,, @cExt )
      IF cExt == ".c"
         FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFile ), Chr( 13 ), Chr( 10 ) ), Chr( 10 ) )
            cLine := AllTrim( cLine )
            IF LEFTEQUAL( cLine, '{ "' ) .AND. "HB_FS_FIRST" $ cLine .AND. !( "HB_FS_STATIC" $ cLine )
               n := 4
               DO WHILE ( c := SubStr( cLine, n++, 1 ) ) != '"'
                  cFuncName += c
               ENDDO
               EXIT
            ENDIF
         NEXT
      ELSEIF Lower( cExt ) == ".cpp" .OR. ;
             Lower( cExt ) == ".cc" .OR. ;
             Lower( cExt ) == ".cxx" .OR. ;
             Lower( cExt ) == ".cx" .OR. ;
             _EXT_IS_UPPER( cExt, ".C" )
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
            DO WHILE ( c := SubStr( cFuncList, n++, 1 ) ) == "_" .OR. ;
                     IsDigit( c ) .OR. IsAlpha( c )
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
      CASE hbmk[ _HBMK_cPLAT ] == "vxworks"
         AAdd( aDf, "__PLATFORM__VXWORKS" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "symbian"
         AAdd( aDf, "__PLATFORM__SYMBIAN" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cPLAT ] == "cygwin"
         AAdd( aDf, "__PLATFORM__CYGWIN" )
         AAdd( aDf, "__PLATFORM__UNIX" )
      ENDCASE

      /* Setup those CPU flags which we can be sure about.
         This is not fully generic solution, cross builds
         to *nix systems aren't covered. Anyway, it's not
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
                  Let's assume the most probable CPU platform (as of 2009). */
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

STATIC PROCEDURE rtlnk_libtrans( aLibList )
   STATIC s_hTrans := { ;
      "CT"        => "hbct"   , ;
      "CTP"       => "hbct"   , ;
      "CLASSY"    => NIL      , ;
      "CSYINSP"   => NIL      , ;
      "SIX3"      => NIL      , ;
      "NOMACH6"   => NIL      , ;
      "BLXRATEX"  => NIL      , ;
      "BLXCLP50"  => NIL      , ;
      "BLXCLP52"  => NIL      , ;
      "BLXCLP53"  => NIL      , ;
      "EXOSPACE"  => NIL      , ;
      "CLIPPER"   => NIL      , ;
      "EXTEND"    => NIL      , ;
      "TERMINAL"  => NIL      , ;
      "PCBIOS"    => NIL      , ;
      "ANSITERM"  => NIL      , ;
      "DBFBLOB"   => NIL      , ;
      "DBFMEMO"   => NIL      , ;
      "DBFNTX"    => NIL      , ;
      "DBFCDX"    => NIL      , ;
      "_DBFCDX"   => NIL      , ;
      "CLD"       => NIL      , ;
      "CLDR"      => NIL      , ;
      "LLIBCE"    => NIL      , ;
      "LLIBCA"    => NIL      }
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
      "CTUS"      => NIL      , ;
      "CTUSP"     => NIL      , ;
      "CTINT"     => NIL      , ;
      "CTINTP"    => NIL      , ;
      "__WAIT"    => NIL      , ;
      "__WAIT_4"  => NIL      , ;
      "__WAIT_B"  => NIL      , ;
      "BLXCLP50"  => NIL      , ;
      "BLXCLP52"  => NIL      , ;
      "BLXCLP53"  => NIL      , ;
      "BLDCLP50"  => NIL      , ;
      "BLDCLP52"  => NIL      , ;
      "BLDCLP53"  => NIL      , ;
      "SIXCDX"    => NIL      , ;
      "SIXNSX"    => NIL      , ;
      "SIXNTX"    => NIL      , ;
      "DBT"       => NIL      , ;
      "FPT"       => NIL      , ;
      "SMT"       => NIL      , ;
      "NOMEMO"    => NIL      , ;
      "CLD.LIB"   => NIL      }
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

STATIC FUNCTION rtlnk_read( cFileName, aPrevFiles )
   LOCAL cFileBody
   LOCAL cPath, cFile, cExt
   LOCAL hFile

   hb_FNameSplit( cFileName, @cPath, @cFile, @cExt )
   IF Empty( cExt )
      cExt := ".lnk"
   ENDIF

   cFileName := hb_FNameMerge( cPath, cFile, ".lnk" )
   /* it's blinker extension, look for .lnk file in paths
    * specified by LIB envvar
    */
   IF !hb_fileExists( cFileName ) .AND. ;
      !Left( cFileName, 1 ) $ hb_osPathDelimiters() .AND. ;
      !SubStr( cFileName, 2, 1 ) == hb_osDriveSeparator()
      FOR EACH cPath IN hb_aTokens( GetEnv( "LIB" ), hb_osPathListSeparator() )
         cFile := hb_FNameMerge( cPath, cFileName )
         IF hb_fileExists( cFile )
            cFileName := cFile
            EXIT
         ENDIF
      NEXT
   ENDIF

   /* protection against recursive calls */
   IF AScan( aPrevFiles, { |x| x == cFileName } ) == 0
      IF ( hFile := FOpen( cFileName ) ) != -1
         cFileBody := Space( FSeek( hFile, 0, FS_END ) )
         FSeek( hFile, 0, FS_SET )
         IF FRead( hFile, @cFileBody, Len( cFileBody ) ) != Len( cFileBody )
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

STATIC FUNCTION rtlnk_process( hbmk, cCommands, cFileOut, aFileList, aLibList, ;
                               aPrevFiles )
   LOCAL cLine, cWord
   LOCAL nMode

   cCommands := StrTran( StrTran( cCommands, Chr( 13 ) ), "//", "# " )

   nMode := RTLNK_MODE_NONE
   IF ! ISARRAY( aPrevFiles )
      aPrevFiles := {}
   ENDIF
   FOR EACH cLine IN hb_ATokens( cCommands, Chr( 10 ) )
      cLine := AllTrim( cLine )
      IF ! Empty( cLine )
         FOR EACH cWord IN rtlnk_tokens( cLine )
            IF LEFTEQUAL( cWord, "#" )
               EXIT
            ELSEIF nMode == RTLNK_MODE_OUT
               cFileOut := cWord
               nMode := RTLNK_MODE_FILENEXT
            ELSEIF nMode == RTLNK_MODE_FILE
               IF !( cWord == "," )
                  IF AScan( aFileList, { |x| x == cWord } ) == 0
                     AAdd( aFileList, PathSepToSelf( cWord ) )
                  ENDIF
                  nMode := RTLNK_MODE_FILENEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_LIB
               IF !( cWord == "," )
                  AAdd( aLibList, PathSepToSelf( cWord ) )
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
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Cannot open file: %1$s" ), cWord ) )
                  RETURN .F.
               ENDIF
               IF ! rtlnk_process( hbmk, cCommands, @cFileOut, @aFileList, @aLibList, aPrevFiles )
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
                  /* blinker extension */
                  ELSEIF LEFTEQUAL( "BLINKER", cWord )
                     /* skip blinker commands */
                     EXIT
                  ELSEIF LEFTEQUAL( "ECHO", cWord )
                     hbmk_OutStd( hbmk, hb_StrFormat( I_( "Blinker ECHO: %1$s" ), SubStr( cLine, 6 ) ) )
                     EXIT
                  ELSEIF LEFTEQUAL( "MAP", cWord )
                     hbmk[ _HBMK_lMAP ] := .T.
                     EXIT
                  ELSEIF LEFTEQUAL( "NOBELL", cWord )
                     hbmk[ _HBMK_lBEEP ] := .F.
                     EXIT
                  ELSE /* TODO: add other blinker commands */
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
               hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: file .pot list: %1$s", ArrayToList( aPOTIN, ", " ) ) )
               hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: temp unified .po: %1$s", cPOTemp ) )
            ENDIF
            POTMerge( hbmk, aPOTIN, NIL, cPOTemp )
         ELSE
            hbmk_OutStd( hbmk, I_( "Error: Cannot create temporary unified .po file." ) )
         ENDIF
      ENDIF
      cPOCooked := StrTran( hbmk[ _HBMK_cPO ], _LNG_MARKER, cLNG )
      IF hb_FileExists( cPOCooked )
         IF hbmk[ _HBMK_lDEBUGI18N ]
            hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: updating unified .po: %1$s", cPOCooked ) )
         ENDIF
         AutoTrans( hbmk, cPOTemp, { cPOCooked }, cPOCooked )
         AAdd( aUpd, cLNG )
      ELSE
         IF hbmk[ _HBMK_lDEBUGI18N ]
            hbmk_OutStd( hbmk, hb_StrFormat( "RebuildPO: creating unified .po: %1$s", cPOCooked ) )
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
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .po file '%1$s'" ), hbmk[ _HBMK_cPO ] ) )
         ELSE
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .po file '%1$s' for language(s): %2$s" ), hbmk[ _HBMK_cPO ], ArrayToList( aNew, "," ) ) )
         ENDIF
      ENDIF
      IF ! Empty( aUpd )
         IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] )
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Rebuilt .po file '%1$s'" ), hbmk[ _HBMK_cPO ] ) )
         ELSE
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Rebuilt .po file '%1$s' for language(s): %2$s" ), hbmk[ _HBMK_cPO ], ArrayToList( aUpd, "," ) ) )
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
      hbmk_OutStd( hbmk, hb_StrFormat( "UpdatePO: file .pot list: %1$s", ArrayToList( aPOTIN, ", " ) ) )
      hbmk_OutStd( hbmk, hb_StrFormat( "UpdatePO: for .po: %1$s", hbmk[ _HBMK_cPO ] ) )
      hbmk_OutStd( hbmk, hb_StrFormat( "UpdatePO: for languages: %1$s", ArrayToList( hbmk[ _HBMK_aLNG ], ", " ) ) )
   ENDIF

   IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ hbmk[ _HBMK_cPO ] )
      hbmk_OutStd( hbmk, hb_StrFormat( I_( "Updated .po file '%1$s'" ), hbmk[ _HBMK_cPO ] ) )
   ELSE
      hbmk_OutStd( hbmk, hb_StrFormat( I_( "Updated .po file '%1$s' for language(s): %2$s" ), hbmk[ _HBMK_cPO ], ArrayToList( aUpd, "," ) ) )
   ENDIF

   RETURN

/* .hbl generation */

STATIC PROCEDURE MakeHBL( hbmk, cHBL )
   LOCAL cPO
   LOCAL tPO
   LOCAL cLNG
   LOCAL tLNG
   LOCAL aPO_TODO
   LOCAL lUpdateNeeded

   LOCAL aNew := {}

   IF ! Empty( hbmk[ _HBMK_aPO ] )
      IF hbmk[ _HBMK_lDEBUGI18N ]
         hbmk_OutStd( hbmk, hb_StrFormat( "po: in: %1$s", ArrayToList( hbmk[ _HBMK_aPO ] ) ) )
      ENDIF
      IF Empty( cHBL )
         cHBL := FNameNameGet( hbmk[ _HBMK_aPO ][ 1 ] )
      ENDIF
      IF Empty( FNameExtGet( cHBL ) )
         cHBL := FNameExtSet( cHBL, ".hbl" )
      ENDIF

      FOR EACH cLNG IN iif( Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ cHBL ), { _LNG_MARKER }, hbmk[ _HBMK_aLNG ] )
         tLNG := NIL
         hb_FGetDateTime( StrTran( cHBL, _LNG_MARKER, cLNG ), @tLNG )
         lUpdateNeeded := .F.
         aPO_TODO := {}
         FOR EACH cPO IN hbmk[ _HBMK_aPO ]
            IF tLNG == NIL .OR. ( hb_FGetDateTime( StrTran( cPO, _LNG_MARKER, cLNG ), @tPO ) .AND. tPO > tLNG )
               lUpdateNeeded := .T.
            ENDIF
            AAdd( aPO_TODO, StrTran( cPO, _LNG_MARKER, cLNG ) )
         NEXT
         IF lUpdateNeeded
            IF hbmk[ _HBMK_lDEBUGI18N ]
               hbmk_OutStd( hbmk, hb_StrFormat( "po: %1$s -> %2$s", ArrayToList( aPO_TODO ), StrTran( cHBL, _LNG_MARKER, cLNG ) ) )
            ENDIF
            GenHBL( hbmk, aPO_TODO, StrTran( cHBL, _LNG_MARKER, cLNG ) )
            AAdd( aNew, cLNG )
         ENDIF
      NEXT
   ENDIF

   IF ! hbmk[ _HBMK_lQuiet ]
      IF ! Empty( aNew )
         IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ cHBL )
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .hbl file '%1$s'" ), cHBL ) )
         ELSE
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .hbl file '%1$s' for language(s): %2$s" ), cHBL, ArrayToList( aNew, "," ) ) )
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
            hbmk_OutErr( hbmk, hb_StrFormat( I_( ".pot error: %1$s" ), cErrorMsg ) )
         ENDIF
         cErrorMsg := NIL
      ENDIF
   NEXT

   IF hbmk[ _HBMK_lDEBUGI18N ] .AND. aTrans == NIL
      hbmk_OutErr( hbmk, "LoadPOTFiles() did not load anything" )
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
            hbmk_OutStd( hbmk, hb_StrFormat( "LoadPOTFilesAsHash(): %1$s", cFileName ) )
         ENDIF
         hTrans := __i18n_potArrayToHash( aTrans,, hTrans )
      ELSE
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: %1$s" ), cErrorMsg ) )
      ENDIF
   NEXT

   RETURN hTrans

STATIC PROCEDURE POTMerge( hbmk, aFiles, cFileBase, cFileOut )
   LOCAL cErrorMsg
   LOCAL aTrans := LoadPOTFiles( hbmk, aFiles, cFileBase, .T. )

   IF aTrans != NIL
      IF ! __i18n_potArraySave( cFileOut, aTrans, @cErrorMsg, ! hbmk[ _HBMK_lMINIPO ], ! hbmk[ _HBMK_lMINIPO ] )
         hbmk_OutErr( hbmk, hb_StrFormat( I_( ".pot merge error: %1$s" ), cErrorMsg ) )
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
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: %1$s" ), cErrorMsg ) )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION GenHBL( hbmk, aFiles, cFileOut, lEmpty )
   LOCAL cHBLBody
   LOCAL pI18N
   LOCAL aTrans := LoadPOTFiles( hbmk, aFiles, NIL, .F. )
   LOCAL lRetVal := .F.

   IF ISARRAY( aTrans )
      pI18N := __i18n_hashTable( __i18n_potArrayToHash( aTrans, lEmpty ) )
      cHBLBody := hb_i18n_SaveTable( pI18N )
      IF hb_MemoWrit( cFileOut, cHBLBody )
         lRetVal := .T.
      ELSE
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Cannot create file: %1$s" ), cFileOut ) )
      ENDIF
   ENDIF

   RETURN lRetVal

STATIC FUNCTION win_implib_command( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )

   IF ! hb_FileExists( cSourceDLL )
      IF hbmk[ _HBMK_lInfo ]
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Source dynamic library not found: %1$s" ), cSourceDLL ) )
      ENDIF
      RETURN _HBMK_IMPLIB_NOTFOUND
   ENDIF

   DEFAULT cFlags TO ""

   cCommand := StrTran( cCommand, "{FI}", cFlags )
   cCommand := StrTran( cCommand, "{ID}", FNameEscape( cSourceDLL, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )
   cCommand := StrTran( cCommand, "{OL}", FNameEscape( cTargetLib, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )

   IF hbmk[ _HBMK_lTRACE ]
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, I_( "Import library creation command:" ) )
      ENDIF
      OutStd( cCommand + _OUT_EOL )
   ENDIF

   RETURN iif( hb_processRun( cCommand ) == 0, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )

#define _COFF_LIB_SIGNATURE "!<arch>"

STATIC FUNCTION IsCOFFLib( cFileName )
   LOCAL fhnd := FOpen( cFileName, FO_READ )
   LOCAL cBuffer

   IF fhnd != F_ERROR
      cBuffer := Space( Len( _COFF_LIB_SIGNATURE ) )
      FRead( fhnd, @cBuffer, Len( cBuffer ) )
      FClose( fhnd )
      IF cBuffer == _COFF_LIB_SIGNATURE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

#define _OMF_LIB_SIGNATURE Chr( 0xF0 )

STATIC FUNCTION IsOMFLib( cFileName )
   LOCAL fhnd := FOpen( cFileName, FO_READ )
   LOCAL cBuffer

   IF fhnd != F_ERROR
      cBuffer := Space( Len( _OMF_LIB_SIGNATURE ) )
      FRead( fhnd, @cBuffer, Len( cBuffer ) )
      FClose( fhnd )
      IF cBuffer == _OMF_LIB_SIGNATURE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION win_implib_coff( hbmk, cSourceDLL, cTargetLib )
   LOCAL cSourceLib

   /* Try to find COFF .lib with the same name */
   IF hb_FileExists( cSourceLib := FNameExtSet( cSourceDLL, ".lib" ) )
      IF IsCOFFLib( cSourceLib )
         IF ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, I_( "Found COFF .lib with the same name, falling back to using it instead of the .dll." ) )
         ENDIF
         RETURN iif( hb_FCopy( cSourceLib, cTargetLib ) != F_ERROR, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )
      ENDIF
   ENDIF

   RETURN _HBMK_IMPLIB_NOTFOUND

STATIC FUNCTION win_implib_omf( hbmk, cSourceDLL, cTargetLib )
   LOCAL cSourceLib

   /* Try to find COFF .lib with the same name */
   IF hb_FileExists( cSourceLib := FNameExtSet( cSourceDLL, ".lib" ) )
      IF IsOMFLib( cSourceLib )
         IF ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, I_( "Found OMF .lib with the same name, falling back to using it instead of the .dll." ) )
         ENDIF
         RETURN iif( hb_FCopy( cSourceLib, cTargetLib ) != F_ERROR, _HBMK_IMPLIB_OK, _HBMK_IMPLIB_FAILED )
      ENDIF
   ENDIF

   RETURN _HBMK_IMPLIB_NOTFOUND

STATIC FUNCTION win_implib_def( hbmk, cCommand, cSourceDLL, cTargetLib, cFlags )
   LOCAL cSourceDef

   /* Try to find .def file with the same name */
   IF hb_FileExists( cSourceDef := FNameExtSet( cSourceDLL, ".def" ) )
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, I_( "Found .def file with the same name, falling back to using it instead of the .dll." ) )
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
         hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Source dynamic library not found: %1$s" ), cSourceDLL ) )
      ENDIF
      RETURN _HBMK_IMPLIB_NOTFOUND
   ENDIF

   cCommandDump := "dumpbin.exe -exports {ID}"
   cCommandDump := StrTran( cCommandDump, "{ID}", FNameEscape( cSourceDLL, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )

   IF hbmk[ _HBMK_lTRACE ]
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, I_( "Import library creation command:" ) )
      ENDIF
      OutStd( cCommandDump + _OUT_EOL )
   ENDIF

   nResult := _HBMK_IMPLIB_FAILED

   IF hb_processRun( cCommandDump,, @cExports ) == 0

      cFuncList := "LIBRARY " + '"' + FNameNameExtGet( cSourceDLL ) + '"' + hb_eol() +;
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

   DEFAULT cDir TO ""

   IF ! Empty( cDir )
      cDir := DirAddPathSep( cDir )
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

STATIC FUNCTION VCSID( cDir, cVCSHEAD, /* @ */ cType )
   LOCAL hnd, cStdOut
   LOCAL nType := VCSDetect( cDir )
   LOCAL cCommand
   LOCAL cResult := ""
   LOCAL cTemp
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

      hnd := hb_FTempCreateEx( @cTemp )
      IF hnd != F_ERROR
         FClose( hnd )
         cCommand += ">" + cTemp
         hb_run( cCommand )
         cStdOut := hb_MemoRead( cTemp )
         FErase( cTemp )

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
   CASE HBMK_ISCOMP( "gcc|icc|clang|sunpro|diab|pcc" )
      /* TOFIX: This isn't necessarily correct, since these inherit the
                default CPU architecture from OS default, by and large,
                and targets can be overridden using user options. */
      RETURN "x86"
   CASE hbmk[ _HBMK_cCOMP ] == "mingw64" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "msvc64" .OR. ;
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

/* Keep this public, it's used from macro. */
FUNCTION hbmk_KEYW( hbmk, cKeyword, cValue, cOperator )
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
   CASE "unicode"  ; RETURN hbmk[ _HBMK_lUNICODE ]
   CASE "ascii"    ; RETURN ! hbmk[ _HBMK_lUNICODE ]
   CASE "unix"     ; RETURN HBMK_ISPLAT( "bsd|hpux|sunos|beos|qnx|vxworks|symbian|linux|darwin|cygwin" )
   CASE "allwin"   ; RETURN HBMK_ISPLAT( "win|wce" )
   CASE "allgcc"   ; RETURN HBMK_ISCOMP( "gcc|mingw|mingw64|mingwarm|djgpp|gccomf|clang|open64" )
   CASE "allmingw" ; RETURN HBMK_ISCOMP( "mingw|mingw64|mingwarm" )
   CASE "allmsvc"  ; RETURN HBMK_ISCOMP( "msvc|msvc64|msvcia64|msvcarm" )
   CASE "allpocc"  ; RETURN HBMK_ISCOMP( "pocc|pocc64|poccarm" )
   CASE "allicc"   ; RETURN HBMK_ISCOMP( "icc|iccia64" )
   CASE "xhb"      ; RETURN _HBMODE_IS_XHB( hbmk[ _HBMK_nHBMODE ] )
   CASE "hb10"     ; RETURN hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10
   CASE "hb20"     ; RETURN hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB20
   ENDSWITCH

   IF cKeyword == hbmk_CPU( hbmk )
      RETURN .T.
   ENDIF

   IF cKeyword == hbmk_TARGETTYPE( hbmk )
      RETURN .T.
   ENDIF

   IF ! HBMK_IS_IN( cKeyword, "|win|wce|dos|os2" + ;
                              "|bsd|hpux|sunos|beos|qnx|vxworks|symbian|linux|darwin|cygwin" + ;
                              "|msvc|msvc64|msvcia64|msvcarm" + ;
                              "|pocc|pocc64|poccarm|xcc" + ;
                              "|mingw|mingw64|mingwarm|bcc|watcom" + ;
                              "|gcc|gccomf|djgpp" + ;
                              "|hblib|hbdyn|hbdynvm|hbimplib|hbexe" + ;
                              "|icc|iccia64|clang|open64|sunpro|diab|pcc" + ;
                              "|x86|x86_64|ia64|arm|mips|sh" )

      tmp := MacroGet( hbmk, cKeyWord, "" )
      IF cValue != NIL
         SWITCH cOperator
         CASE "="
            RETURN hb_asciiUpper( tmp ) == hb_asciiUpper( cValue )
         CASE ">"
            RETURN hb_asciiUpper( tmp ) > hb_asciiUpper( cValue )
         CASE "<"
            RETURN hb_asciiUpper( tmp ) < hb_asciiUpper( cValue )
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

STATIC FUNCTION MacOSXFiles( hbmk, nType, cPROGNAME )
   LOCAL cString

   HB_SYMBOL_UNUSED( hbmk )

   SWITCH nType
   CASE 1

      #pragma __cstream|cString := %s
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
<plist version="0.9">
<dict>
%TAB%<key>CFBundleInfoDictionaryVersion</key>
%TAB%<string>6.0</string>
%TAB%<key>CFBundleIdentifier</key>
%TAB%<string>%__APPID__%</string>
%TAB%<key>CFBundleDevelopmentRegion</key>
%TAB%<string>English</string>
%TAB%<key>CFBundleExecutable</key>
%TAB%<string>%__APPNAME__%</string>
%TAB%<key>CFBundleIconFile</key>
%TAB%<string>%__APPICON__%</string>
%TAB%<key>CFBundleName</key>
%TAB%<string>%__APPNAME__%</string>
%TAB%<key>CFBundlePackageType</key>
%TAB%<string>%__APPTYPE__%</string>
%TAB%<key>CFBundleSignature</key>
%TAB%<string>%__APPSIGN__%</string>
%TAB%<key>CFBundleGetInfoString</key>
%TAB%<string>%__APPNAME__% version %__APPVERSION__%, %__APPCOPYRIGHT__%</string>
%TAB%<key>CFBundleLongVersionString</key>
%TAB%<string>%__APPVERSION__%, %__APPCOPYRIGHT__%</string>
%TAB%<key>NSHumanReadableCopyright</key>
%TAB%<string>%__APPCOPYRIGHT__%</string>
%TAB%<key>LSRequiresCarbon</key>
%TAB%<true/>
%TAB%<key>CSResourcesFileMapped</key>
%TAB%<true/>
</dict>
</plist>
      #pragma __endtext
      EXIT
   CASE 2
      cString := "%__APPTYPE__%%__APPSIGN__%"
      EXIT
   ENDSWITCH

   cString := StrTran( cString, "%TAB%", Chr( 9 ) )

   cString := StrTran( cString, "%__APPNAME__%", cPROGNAME )
   cString := StrTran( cString, "%__APPTYPE__%", "APPL" )
   cString := StrTran( cString, "%__APPSIGN__%", PadR( cPROGNAME, 4, "?" ) )
   cString := StrTran( cString, "%__APPID__%" ) /* TODO */
   cString := StrTran( cString, "%__APPVERSION__%" ) /* TODO */
   cString := StrTran( cString, "%__APPCOPYRIGHT__%" ) /* TODO */
   IF ! Empty( hbmk[ _HBMK_aICON ] )
      cString := StrTran( cString, "%__APPICON__%", FNameNameExtGet( hbmk[ _HBMK_aICON ][ 1 ] ) )
   ENDIF

   RETURN cString

STATIC FUNCTION mk_extern( hbmk, cInputName, cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex, cOutputName )
   LOCAL aExtern

   IF ( aExtern := __hb_extern_get_list( hbmk, cInputName, cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex ) ) != NIL

      IF hbmk[ _HBMK_lInfo ]
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating extern header... %1$s" ), cOutputName ) )
      ENDIF

      __hb_extern_gen( aExtern, cOutputName )

      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION __hb_extern_get_list( hbmk, cInputName, cBin_LibHBX, cOpt_LibHBX, cLibHBX_Regex )
   LOCAL aExtern := NIL

   LOCAL cStdOut, cStdErr
   LOCAL cTempFile
   LOCAL hRegex
   LOCAL aResult
   LOCAL tmp

   IF ! Empty( cBin_LibHBX ) .AND. ;
      ! Empty( cLibHBX_Regex )

      IF hb_FileExists( cInputName )

         cOpt_LibHBX := StrTran( cOpt_LibHBX, "{LI}", FNameEscape( cInputName, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )
         IF "{OT}" $ cOpt_LibHBX
            FClose( hb_FTempCreateEx( @cTempFile,,, ".tmp" ) )
            cOpt_LibHBX := StrTran( cOpt_LibHBX, "{OT}", FNameEscape( cTempFile, hbmk[ _HBMK_nCmd_Esc ], hbmk[ _HBMK_nCmd_FNF ] ) )
         ENDIF

         IF hb_processRun( cBin_LibHBX + " " + cOpt_LibHBX, @cStdErr, @cStdOut ) == 0
            IF ! Empty( cTempFile )
               cStdOut := MemoRead( cTempFile )
            ENDIF
            IF ! Empty( hRegex := hb_regexComp( cLibHBX_Regex, .T., .T. ) )
               aResult := hb_regexAll( hRegex, StrTran( cStdOut, Chr( 13 ) ),,,,, .T. )
               aExtern := {}
               FOR EACH tmp IN aResult
                  AAdd( aExtern, tmp[ 2 ] )
               NEXT
               ASort( aExtern,,, {| tmp, tmp1 | tmp < tmp1 } )
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

STATIC PROCEDURE __hb_extern_get_exception_list( cInputName, /* @ */ aInclude, /* @ */ aExclude )
   LOCAL cFile
   LOCAL hRegex
   LOCAL tmp

   aInclude := {}
   aExclude := {}

   IF ! Empty( cFile := MemoRead( cInputName ) )
      IF ! Empty( hRegex := hb_regexComp( "[[:space:]]" + _HB_FUNC_INCLUDE_ + "[[:space:]]([a-zA-z0-9_].[^ \t\n\r]*)", .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( hRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            AAdd( aInclude, Upper( tmp[ 2 ] ) )
         NEXT
      ENDIF
      IF ! Empty( hRegex := hb_regexComp( "[[:space:]]" + _HB_FUNC_EXCLUDE_ + "[[:space:]]([a-zA-z0-9_].[^ \t\n\r]*)", .T., .T. ) )
         FOR EACH tmp IN hb_regexAll( hRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
            AAdd( aExclude, Upper( tmp[ 2 ] ) )
         NEXT
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION __hb_extern_gen( aFuncList, cOutputName )
   LOCAL aExtern
   LOCAL cExtern
   LOCAL tmp

   LOCAL aInclude
   LOCAL aExclude

   LOCAL cSelfName := _HB_SELF_PREFIX + Upper( FNameNameGet( cOutputName ) ) + _HB_SELF_SUFFIX

   LOCAL cLine := "/* -------------------------------------------------------------------- */" + hb_eol()
   LOCAL cHelp := "/*          Syntax: // HB_FUNC_INCLUDE <func>                           */" + hb_eol() +;
                  "/*                  // HB_FUNC_EXCLUDE <func>                           */" + hb_eol()

   __hb_extern_get_exception_list( cOutputName, @aInclude, @aExclude )

   cExtern := "/*" + hb_eol()
   cExtern += " * $" + "Id" + "$" + hb_eol()
   cExtern += " */" + hb_eol()
   IF Empty( aInclude ) .AND. ;
      Empty( aExclude )
      cExtern += hb_eol()
      cExtern += cLine
      cExtern += "/* NOTE: You can add manual override which functions to include or      */" + hb_eol()
      cExtern += "/*       exclude from automatically generated EXTERNAL/DYNAMIC list.    */" + hb_eol()
      cExtern += cHelp
      cExtern += cLine
   ELSE
      cExtern += hb_eol()
      cExtern += cLine
      cExtern += "/* NOTE: Following comments are control commands for the generator.     */" + hb_eol()
      cExtern += "/*       Do not edit them unless you know what you are doing.           */" + hb_eol()
      cExtern += cHelp
      cExtern += cLine
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
   cExtern += "/* WARNING: Automatically generated code below. DO NOT EDIT!            */" + hb_eol()
   cExtern += "/*          Regenerate with HB_REBUILD_EXTERN=yes while using GCC       */" + hb_eol()
   cExtern += "/*          compiler family. [vszakats]                                 */" + hb_eol()
   cExtern += cLine
   cExtern += hb_eol()
   cExtern += "#ifndef " + "__HBEXTERN_CH__" + Upper( FNameNameGet( cOutputName ) ) + "__" + hb_eol()
   cExtern += "#define " + "__HBEXTERN_CH__" + Upper( FNameNameGet( cOutputName ) ) + "__" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#ifdef " + cSelfName + "ANNOUNCE" + hb_eol()
   cExtern += "   ANNOUNCE " + cSelfName + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#ifdef " + cSelfName + "REQUEST" + hb_eol()
   cExtern += "   #command DYNAMIC <fncs,...> => EXTERNAL <fncs>" + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   IF Empty( aInclude )
      aExtern := aFuncList
   ELSE
      aExtern := {}
      FOR EACH tmp IN aFuncList
         IF AScan( aInclude, {| flt | hb_WildMatch( flt, tmp ) } ) > 0
            AAdd( aExtern, tmp )
         ENDIF
      NEXT
   ENDIF
   FOR EACH tmp IN aExtern
      IF ! hb_WildMatch( "HB_GT_*_DEFAULT", tmp ) .AND. ;
         ! hb_WildMatch( _HB_SELF_PREFIX + "*" + _HB_SELF_SUFFIX, tmp ) .AND. ;
         AScan( aExclude, {| flt | hb_WildMatch( flt, tmp ) } ) == 0
         cExtern += "DYNAMIC " + tmp + hb_eol()
      ENDIF
   NEXT
   cExtern += hb_eol()
   cExtern += "#ifdef " + cSelfName + "REQUEST" + hb_eol()
   cExtern += "   #uncommand DYNAMIC <fncs,...> => EXTERNAL <fncs>" + hb_eol()
   cExtern += "#endif" + hb_eol()
   cExtern += hb_eol()
   cExtern += "#endif" + hb_eol()

   /* Do not touch the file if the content is unchanged */
   IF hb_MemoRead( cOutputName ) == cExtern
      RETURN .T.
   ENDIF

   RETURN hb_MemoWrit( cOutputName, cExtern )

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

   hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loading hbmake project file: %1$s" ), cSrcName ) )

   IF Empty( cDstName )
      cDstName := FNameExtSet( cSrcName, ".hbp" )
   ENDIF

   AAdd( aDst, "# Automatically converted by hbmk2 from hbmake project:" )
   AAdd( aDst, cSrcName )
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
                  AAdd( aDst, "-compr=on" )
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
                  AAdd( aDst, "-o" + FNameNameGet( aValue[ 1 ] ) )
               ENDIF
               EXIT
            CASE "USERLIBS"
               FOR EACH tmp IN aValue
                  AAdd( aDst, "-l" + FNameNameGet( tmp ) )
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
                  AAdd( aDst, "-l" + FNameNameGet( tmp ) )
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

   hbmk_OutStd( hbmk, hb_StrFormat( I_( "Saving as .hbp file: %1$s" ), cDstName ) )

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

   hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loading xbp (xbuild) project file: %1$s" ), cSrcName ) )

   IF Empty( cDstName )
      cDstName := FNameExtSet( cSrcName, ".hbp" )
   ENDIF

   AAdd( aDst, "# Automatically converted by hbmk2 from xbuild project:" )
   AAdd( aDst, cSrcName )
   AAdd( aDst, "" )

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
                     AAdd( aDst, "-o" + DirAddPathSep( cValue ) )
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

   hbmk_OutStd( hbmk, hb_StrFormat( I_( "Saving as .hbp file: %1$s" ), cDstName ) )

   hb_MemoWrit( cDstName, cDst )

   RETURN

STATIC PROCEDURE convert_xhp_to_hbp( hbmk, cSrcName, cDstName )
   LOCAL cSrc := MemoRead( cSrcName )
   LOCAL cDst
   LOCAL aDst := {}
   LOCAL tmp
   LOCAL cLine
   LOCAL cSetting
   LOCAL cValue
   LOCAL aValue
   LOCAL cFile

   LOCAL hLIBPATH := { => }

   LOCAL cMAIN := NIL

   LOCAL lFileSection := .F.

   hbmk_OutStd( hbmk, hb_StrFormat( I_( "Loading xhp (xMate) project file: %1$s" ), cSrcName ) )

   IF Empty( cDstName )
      cDstName := FNameExtSet( cSrcName, ".hbp" )
   ENDIF

   AAdd( aDst, "# Automatically converted by hbmk2 from xMate project:" )
   AAdd( aDst, cSrcName )
   AAdd( aDst, "" )

   cSrc := StrTran( cSrc, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   cSrc := StrTran( cSrc, Chr( 9 ), " " )

   FOR EACH cLine IN hb_ATokens( cSrc, Chr( 10 ) )
      IF cLine == "[Files]"
         lFileSection := .T.
      ELSEIF lFileSection
         tmp := At( "=", cLine )
         IF tmp > 0
            cFile := AllTrim( Left( cLine, tmp - 1 ) )
            SWITCH Lower( FNameExtGet( cFile ) )
            CASE ".c"
            CASE ".prg"
               IF !( "%HB_INSTALL%\" $ cFile )
                  AAdd( aDst, StrTran( cFile, "%HOME%\" ) )
               ENDIF
               EXIT
            CASE ".lib"
            CASE ".a"
               IF !( "%C_LIB_INSTALL%\" $ cFile ) .AND. ;
                  !( "%HB_LIB_INSTALL%\" $ cFile )
                  cFile := StrTran( cFile, "%HOME%\" )
                  IF !( FNameDirGet( cFile ) $ hLIBPATH )
                     hLIBPATH[ FNameDirGet( cFile ) ] := NIL
                  ENDIF
                  AAdd( aDst, "-l" + FNameNameGet( cFile ) )
               ENDIF
               EXIT
            CASE ".obj"
            CASE ".o"
               IF !( "%C_LIB_INSTALL%\" $ cFile ) .AND. ;
                  !( "%HB_LIB_INSTALL%\" $ cFile )
                  AAdd( aDst, StrTran( cFile, "%HOME%\" ) )
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
                     AAdd( aDst, "-o" + DirAddPathSep( StrTran( cValue, "%HOME%\" ) ) )
                  ENDIF
                  EXIT
               CASE "Include"
                  FOR EACH tmp IN aValue
                     IF Left( tmp, 2 ) == "-I"
                        tmp := SubStr( tmp, 3 )
                     ENDIF
                     AAdd( aDst, "-incpath=" + StrTran( StrTran( tmp, '"' ), "%HOME%\" ) )
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

   hbmk_OutStd( hbmk, hb_StrFormat( I_( "Saving as .hbp file: %1$s" ), cDstName ) )

   hb_MemoWrit( cDstName, cDst )

   RETURN

STATIC PROCEDURE GetUILangCDP( /* @ */ cLNG, /* @ */ cCDP )

   IF Empty( cLNG := GetEnv( "HB_LANG" ) )
      IF Empty( cLNG := hb_UserLang() )
         cLNG := "en"
      ENDIF
   ENDIF

   cLNG := StrTran( cLNG, "_", "-" )
   cCDP := "" /* TODO: 1) Detect it 2) use it - this would need generic Harbour CP support */

   RETURN

STATIC PROCEDURE SetUILang( hbmk )
   LOCAL tmp

   IF hbmk[ _HBMK_cUILNG ] == "en"
      hb_i18n_set( NIL )
   ELSE
      tmp := "${hb_root}hbmk2.${hb_lng}.hbl"
      tmp := StrTran( tmp, "${hb_root}", DirAddPathSep( hb_DirBase() ) )
      tmp := StrTran( tmp, "${hb_lng}", StrTran( hbmk[ _HBMK_cUILNG ], "-", "_" ) )
      hb_i18n_set( iif( hb_i18n_check( tmp := hb_MemoRead( tmp ) ), hb_i18n_restoretable( tmp ), NIL ) )
   ENDIF

   /* Setup input CP of the translation */
   hb_cdpSelect( Upper( SubStr( I_( "cdp=EN" ), Len( "cdp=" ) + 1 ) ) )

   /* Setup output CP, separate for Windows/DOS/OS2 and *nix systems */
   /* NOTE: Intentionally doing runtime branching to include both strings in translation files. */
   tmp := Upper( SubStr( iif( hb_Version( HB_VERSION_UNIX_COMPAT ), I_( "nix=EN" ), I_( "wdo=EN" ) ), Len( "xxx=" ) + 1 ) )
   IF tmp == "UTF8" .OR. tmp == "UTF-8"
      hb_setDispCP( "UTF8" )
   ELSE
      hb_setDispCP( tmp )
   ENDIF

   RETURN

STATIC PROCEDURE ShowHeader( hbmk )

   OutStd( "Harbour Make (hbmk2) " + HBRawVersion() + _OUT_EOL +;
           "Copyright (c) 1999-2011, Viktor Szakats" + _OUT_EOL +;
           "http://harbour-project.org/" + _OUT_EOL )

   IF !( hbmk[ _HBMK_cUILNG ] == "en" ) .AND. ;
      !( hbmk[ _HBMK_cUILNG ] == "en-GB" ) .AND. ;
      !( hbmk[ _HBMK_cUILNG ] == "en-US" )
      OutStd( hb_StrFormat( I_( "Translation (%1$s): (add your name here)" ), hbmk[ _HBMK_cUILNG ] ) + _OUT_EOL )
   ENDIF

   OutStd( _OUT_EOL )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

STATIC PROCEDURE ShowHelp( hbmk, lLong )

   LOCAL aText_Basic := {;
      I_( "Syntax:" ),;
      "",;
      I_( "  hbmk2 [options] [<script[s]>] <src[s][.prg|.c|.obj|.o|.rc|.res|.def|.po|.pot|.hbl|@.clp|.d]>" ),;
      "",;
      I_( "Options:" ) }

   LOCAL aText_Supp := {;
      "",;
      I_( "Supported <comp> values for each supported <plat> value:" ),;
      "  - linux   : gcc, clang, icc, watcom, sunpro, open64",;
      "  - darwin  : gcc, clang, icc",;
      "  - win     : mingw, msvc, bcc, watcom, icc, pocc, xcc,",;
      "  -           mingw64, msvc64, msvcia64, iccia64, pocc64",;
      "  - wce     : mingwarm, mingw, msvcarm, poccarm",;
      "  - os2     : gcc, gccomf, watcom",;
      "  - dos     : djgpp, watcom",;
      "  - bsd     : gcc, clang, pcc",;
      "  - hpux    : gcc",;
      "  - beos    : gcc",;
      "  - qnx     : gcc",;
      "  - vxworks : gcc, diab",;
      "  - symbian : gcc",;
      "  - cygwin  : gcc",;
      "  - sunos   : gcc, sunpro" }

   LOCAL aOpt_Basic := {;
      { "-o<outname>"        , I_( "output file name" ) },;
      { "-l<libname>"        , I_( "link with <libname> library. <libname> should be without path, extension and 'lib' prefix (unless part of libname)." ) },;
      { "-L<libpath>"        , I_( "additional path to search for libraries" ) },;
      { "-i<p>|-incpath=<p>" , I_( "additional path to search for headers" ) },;
      { "-static|-shared"    , I_( "link with static/shared libs" ) },;
      { "-mt|-st"            , I_( "link with multi/single-thread Harbour VM" ) },;
      { "-gt<name>"          , I_( "link with GT<name> GT driver, can be repeated to link with more GTs. First one will be the default at runtime" ) },;
      { "-hbexe"             , I_( "create executable (default)" ) },;
      { "-hblib"             , I_( "create static library" ) },;
      { "-hbdyn"             , I_( "create dynamic library (without linked Harbour VM)" ) },;
      { "-hbdynvm"           , I_( "create dynamic library" ) },;
      { "-hbimplib"          , I_( "create import library" ) }}

   LOCAL aOpt_Help := {;
      { "-help|--help"       , I_( "long help" ) } }

   LOCAL aOpt_Long := {;
      NIL,;
      { "-gui|-std"          , I_( "create GUI/console executable" ) },;
      { "-main=<mainfunc>"   , I_( "override the name of starting function/procedure" ) },;
      { "-fullstatic"        , I_( "link with all static libs" ) },;
      { "-[full|fix]shared"  , I_( "create shared Harbour binaries without/with absolute dir reference to Harbour library (default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise) (fix/full option in *nix only)" ) },;
      { "-nulrdd[-]"         , I_( "link with nulrdd" ) },;
      { "-[no]debug"         , I_( "add/exclude C compiler debug info. For Harbour level debug, use Harbour option -b as usual" ) },;
      { "-[no]optim"         , I_( "toggle C compiler optimizations (default: on)" ) },;
      { "-[no]cpp[=def]"     , I_( "force C/C++ mode or reset to default" ) },;
      { "-[no]map"           , I_( "create (or not) a map file" ) },;
      { "-[no]implib"        , I_( "create (or not) an import library (in -hbdyn/-hbexe mode). The name will have a postfix added." ) },;
      { "-implib=<output>"   , I_( "create import library (in -hbdyn/-hbexe mode) name to <output> (default: same as output)" ) },;
      { "-ln=<link>"         , I_( "create symbolic link pointing to <output> (<link> is considered relative to <output>)" ) },;
      { "-[no]strip"         , I_( "strip (no strip) binaries" ) },;
      { "-[no]trace"         , I_( "show commands executed" ) },;
      { "-[no]beep"          , I_( "enable (or disable) single beep on successful exit, double beep on failure" ) },;
      { "-[no]ignore"        , I_( "ignore errors when running compiler tools (default: off)" ) },;
      { "-[no]hbcppmm"       , I_( "forces to override standard C++ memory management functions with Harbour ones" ) },;
      { "-nohblib[-]"        , I_( "do not use static core Harbour libraries when linking" ) },;
      { "-nolibgrouping[-]"  , I_( "disable library grouping on gcc based compilers" ) },;
      { "-nomiscsyslib[-]"   , I_( "do not add extra list of system libraries to default library list" ) },;
      { "-traceonly"         , I_( "show commands to be executed, but do not execute them" ) },;
      { "-[no]warn[=lev]"    , I_( "set C compiler warning level\n<lev> can be: max, yes, low, no, def (default: yes)" ) },;
      { "-[no]compr[=lev]"   , I_( "compress executable/dynamic lib (needs UPX)\n<lev> can be: min, max, def" ) },;
      { "-[no]run"           , I_( "run/do not run output executable" ) },;
      { "-vcshead=<file>"    , I_( "generate .ch header file with local repository information. SVN, CVS, Git, Mercurial, Bazaar, Fossil and Monotone are currently supported. Generated header will define macro _HBMK_VCS_TYPE_ with the name of detected VCS and _HBMK_VCS_ID_ with the unique ID of local repository" ) },;
      { "-tshead=<file>"     , I_( "generate .ch header file with timestamp information. Generated header will define macros _HBMK_BUILD_DATE_, _HBMK_BUILD_TIME_, _HBMK_BUILD_TIMESTAMP_ with the date/time of build" ) },;
      { "-icon=<file>"       , I_( "set <file> as application icon. <file> should be a supported format on the target platform" ) },;
      { "-instfile=<g:file>" , I_( "add <file> in to the list of files to be copied to path specified by -instpath option. <g> is an optional copy group, it must be at least two characters long." ) },;
      { "-instpath=<g:path>" , I_( "copy target to <path>. if <path> is a directory, it should end with path separatorm, in this case files specified by -instfile option will also be copied. can be specified multiple times. <g> is an optional copy group, it must be at least two characters long. Build target will be automatically copied to default (empty) copy group." ) },;
      { "-instforce[-]"      , I_( "copy target to install path even if it is up to date" ) },;
      { "-depimplib[-]"      , I_( "enable (or disable) import library generation for import library sources specified in -depimplibs= options (default: yes)" ) },;
      { "-stop[=<text>]"     , I_( "stop without doing anything and display <text> if specified" ) },;
      { "-echo=<text>"       , I_( "echo text on screen" ) },;
      { "-pause"             , I_( "force waiting for a key on exit in case of failure (with alternate GTs only)" ) },;
      { "-info"              , I_( "turn on informational messages" ) },;
      { "-quiet[-]"          , I_( "suppress all screen messages" ) },;
      NIL,;
      { "-bldf[-]"           , I_( "inherit all/no (default) flags from Harbour build" ) },;
      { "-bldf=[p][c][l]"    , I_( "inherit .prg/.c/linker flags (or none) from Harbour build" ) },;
      { "-prgflag=<f>"       , I_( "pass flag to Harbour" ) },;
      { "-cflag=<f>"         , I_( "pass flag to C compiler" ) },;
      { "-resflag=<f>"       , I_( "pass flag to resource compiler (Windows only)" ) },;
      { "-ldflag=<f>"        , I_( "pass flag to linker (executable)" ) },;
      { "-aflag=<f>"         , I_( "pass flag to linker (static library)" ) },;
      { "-dflag=<f>"         , I_( "pass flag to linker (dynamic library)" ) },;
      { "-iflag=<f>"         , I_( "pass flag to import library creation command" ) },;
      { "-runflag=<f>"       , I_( "pass flag to output executable when -run option is used" ) },;
      { "-3rd=<f>"           , I_( "options/flags reserved for 3rd party tools, always ignored by hbmk2 itself" ) },;
      { "-jobs=<n>"          , I_( "start n compilation threads (multiprocess platforms only)" ) },;
      { "-inc"               , I_( "enable incremental build mode" ) },;
      { "-[no]head[=<m>]"    , I_( "control source header parsing (in incremental build mode)\n<m> can be: native (uses compiler to extract dependencies), full (default, uses simple text parser on the whole file), off" ) },;
      { "-rebuild"           , I_( "rebuild (in incremental build mode)" ) },;
      { "-rebuildall"        , I_( "rebuild with sub-projects (in incremental build mode)" ) },;
      { "-clean"             , I_( "clean (in incremental build mode)" ) },;
      { "-workdir=<dir>"     , hb_StrFormat( I_( "working directory\n(default: %1$s/plat/comp in incremental mode, OS temp directory otherwise)" ), _WORKDIR_BASE_ ) },;
      NIL,;
      { "-hbl[=<output>]"    , hb_StrFormat( I_( "output .hbl filename. %1$s macro is accepted in filename" ), _LNG_MARKER ) },;
      { "-lng=<languages>"   , hb_StrFormat( I_( "list of languages to be replaced in %1$s macros in .pot/.po filenames and output .hbl/.po filenames. Comma separared list:\n-lng=en,hu-HU,de" ), _LNG_MARKER ) },;
      { "-po=<output>"       , I_( "create/update .po file from source. Merge it with previous .po file of the same name" ) },;
      { "-[no]minipo"        , I_( "do (not) add Harbour version number and source file reference to .po (default: add them)" ) },;
      { "-rebuildpo"         , I_( "recreate .po file, thus removing all obsolete entries in it" ) },;
      NIL,;
      { "-hbx=<.ch>"         , I_( "Create Harbour header (in .hbx format) with all external symbols. (EXPERIMENTAL)" ) },;
      { "-autohbc=<.ch:.hbc>", I_( "<.ch> is a header file name. <.hbc> is a .hbc filename to be automatically included in case the header is found in any of the compiled sources. (EXPERIMENTAL)" ) },;
      NIL,;
      { "-deppkgname=<d:n>"       , I_( "<d> is the name of the dependency. <n> name of the package depedency. Can be specified multiple times." ) },;
      { "-depkeyhead=<d:h>"       , I_( "<d> is the name of the dependency. <h> is the key header (.h) of the package dependency. Multiple alternative headers can be specified." ) },;
      { "-depoptional=<d:f>"      , I_( "<d> is the name of the dependency. <f> can be 'yes' or 'no', specifies whether the dependency is optional. Default: no" ) },;
      { "-depcontrol=<d:v>"       , I_( "<d> is the name of the dependency. <v> is a value that controls how detection is done. Accepted values: no, yes, force, nolocal, local. Default: content of envvar HBMK2_WITH_<d>" ) },;
      { "-depincpath=<d:i>"       , I_( "<d> is the name of the dependency. Add <i> to the header detection path list" ) },;
      { "-depincpathlocal= <d:i>" , I_( "<d> is the name of the dependency. Add <i> to the header detection path list, where <i> is pointing to a directory local to the project and containing an embedded (or locally hosted) dependency." ) },;
      { "-depimplibs=<d:dll>"     , I_( "<d> is the name of the dependency. Add <dll> to the import library source list" ) },;
      { "-depimplibd=<d:lib>"     , I_( "<d> is the name of the dependency. Set generated import library name to <lib>" ) },;
      NIL,;
      { "-plugin=<.prg|.hbs|.hrb>", I_( "add plugin" ) },;
      { "-pi=<filename>"     , I_( "pass input file to plugins" ) },;
      { "-pflag=<f>"         , I_( "pass flag to plugins" ) },;
      NIL,;
      { "Options below are available on command line only:" },;
      NIL,;
      { "-target=<script>"   , I_( "specify a new build target. <script> can be .prg (or no extension) or .hbp file. Please note that .hbp files are automatically considered as separate targets." ) },;
      NIL,;
      { "-env:<e>[<o>[<v>]]" , I_( "alter local environment. <e> is the name of the environment variable to alter. <o> can be '=' to set/override, '-' to delete, '+' to append to the end of existing value, '#' to insert to the beginning of existing value. <v> is the value to set/append/insert. If multiple options are passed, they are processed from left to right." ) },;
      NIL,;
      { "-hbrun"             , I_( "run target" ) },;
      { "-hbraw"             , I_( "stop after running Harbour compiler" ) },;
      { "-hbcmp|-clipper"    , I_( "stop after creating the object files\ncreate link/copy hbmk2 to hbcmp/clipper for the same effect" ) },;
      { "-hbcc"              , I_( "stop after creating the object files and accept raw C flags\ncreate link/copy hbmk2 to hbcc for the same effect" ) },;
      { "-hblnk"             , I_( "accept raw linker flags" ) },;
      { "-autohbm[-]"        , hb_StrFormat( I_( "enable (or disable) processing of %1$s in current directory (default: yes)" ), _HBMK_AUTOHBM_NAME ) },;
      { "-hb10"              , I_( "enable Harbour 1.0.x compatibility mode" ) },;
      { "-hb20"              , I_( "enable Harbour 2.0.x compatibility mode" ) },;
      { "-xhb"               , I_( "enable xhb mode" ) },;
      { "-hbc"               , I_( "enable pure C mode" ) },;
      { "-rtlink"            , "" },;
      { "-blinker"           , "" },;
      { "-exospace"          , I_( "emulate Clipper compatible linker behavior\ncreate link/copy hbmk2 to rtlink/blinker/exospace for the same effect" ) },;
      NIL,;
      { "-hbmake=<file>"     , I_( "convert hbmake project <file> to .hbp file" ) },;
      { "-xbp=<file>"        , I_( "convert .xbp (xbuild) project <file> to .hbp file" ) },;
      { "-xhp=<file>"        , I_( "convert .xhp (xMate) project <file> to .hbp file" ) },;
      NIL,;
      { "--hbdirbin"         , I_( "output Harbour binary directory" ) },;
      { "--hbdirdyn"         , I_( "output Harbour dynamic library directory" ) },;
      { "--hbdirlib"         , I_( "output Harbour static library directory" ) },;
      { "--hbdirinc"         , I_( "output Harbour header directory" ) },;
      { "--hbinfo"           , I_( "output Harbour build information. The data output comes in the format: '<name>{{<value>}}'. The included paths always contain forward slashes." ) },;
      NIL,;
      { "-plat[form]=<plat>" , I_( "select target platform." ) },;
      { "-comp[iler]=<comp>" , I_( "select C compiler.\nSpecial value:\n - bld: use original build settings (default on *nix)" ) },;
      { "-cpu=<cpu>"         , I_( "select target CPU. (EXPERIMENTAL)" ) },;
      { "-build=<name>"      , I_( "use a specific build name" ) },;
      { "-lang=<lang>"       , I_( "override default language. Similar to HB_LANG envvar." ) },;
      { "-width=<n>"         , I_( "set output width to <n> characters (0=unlimited)." ) },;
      { "-shl"               , I_( "show sub-project level in output lines" ) },;
      { "--version"          , I_( "display version header only" ) } }

   LOCAL aText_Notes := {;
      "",;
      I_( "Notes:" ) }

   LOCAL aNotes := {;
      I_( "<script> can be:\n  <@script> or <script.hbm>: command line options in file\n  <script.hbp>: command line options in file, it also marks a new target if specified on the command line\n  <script.hbc>: package configuration file" ),;
      I_( "Multiple -l, -L and <script> parameters are accepted." ),;
      I_( "Regular Harbour compiler options are also accepted.\n(see them with -harbourhelp option)" ),;
      hb_StrFormat( I_( "%1$s option file in hbmk2 directory is always processed if it exists. On *nix platforms ~/.harbour, /etc/harbour, <base>/etc/harbour, <base>/etc are checked (in that order) before the hbmk2 directory." ), _HBMK_AUTOHBC_NAME ),;
      hb_StrFormat( I_( "%1$s make script in current directory is always processed if it exists." ), _HBMK_AUTOHBM_NAME ),;
      I_( ".hbc options (they should come in separate lines): libs=[<libname[s]>], hbcs=[<.hbc file[s]>], gt=[gtname], syslibs=[<libname[s]>], prgflags=[Harbour flags], cflags=[C compiler flags], resflags=[resource compiler flags], ldflags=[linker flags], pflags=[flags for plugins], libpaths=[paths], sources=[source files], psources=[source files for plugins], incpaths=[paths], instfiles=[files], instpaths=[paths], autohbcs=[<.ch>:<.hbc>], plugins=[plugins], gui|mt|shared|nulrdd|debug|opt|map|implib|hbcppmm|strip|run|inc=[yes|no], cpp=[yes|no|def], warn=[max|yes|low|no|def], compr=[yes|no|def|min|max], head=[off|full|native], skip=<reason>, stop=<reason>, echo=<text>\nLines starting with '#' char are ignored" ),;
      I_( "Platform filters are accepted in each .hbc line and with several options.\nFilter format: {[!][<plat>|<comp>|<cpu>|<keyword>]}. Filters can be combined using '&', '|' operators and grouped by parantheses. Ex.: {win}, {gcc}, {linux|darwin}, {win&!pocc}, {(win|linux)&!watcom}, {unix&mt&gui}, -cflag={win}-DMYDEF, -stop{dos}, -stop{!allwin}, {allwin|allmsvc|allgcc|allmingw|allicc|allpocc|unix}, {x86|x86_64|ia64|arm|mips|sh}, {debug|nodebug|gui|std|mt|st|shared|static|unicode|ascii|xhb}" ),;
      I_( "Certain .hbc lines (libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, instfiles=, instpaths=, echo=) and corresponding command line parameters will accept macros: ${hb_root}, ${hb_dir}, ${hb_name}, ${hb_plat}, ${hb_comp}, ${hb_build}, ${hb_cpu}, ${hb_bin}, ${hb_lib}, ${hb_dyn}, ${hb_inc}, ${<envvar>}. libpaths= also accepts %{hb_name} which translates to the name of the .hbc file under search." ),;
      I_( 'Options accepting macros also support command substitution. Enclose command inside ``, and, if the command contains space, also enclose in double quotes. F.e. "-cflag=`wx-config --cflags`", or ldflags={unix&gcc}"`wx-config --libs`".' ),;
      I_( "Defaults and feature support vary by platform/compiler." ) ,;
      hb_StrFormat( I_( "Options can also be specified in environment variable %1$s" ), _HBMK_ENV_NAME ) }

   DEFAULT lLong TO .F.

   AEval( aText_Basic, {| tmp | OutStd( tmp + _OUT_EOL ) } )
   AEval( aOpt_Basic, {| tmp | OutOpt( hbmk, tmp ) } )
   IF lLong
      AEval( aOpt_Long, {| tmp | OutOpt( hbmk, tmp ) } )
      AEval( aText_Notes, {| tmp | OutStd( tmp + _OUT_EOL ) } )
      AEval( aNotes, {| tmp | OutNote( hbmk, tmp ) } )
      AEval( aText_Supp, {| tmp | OutStd( tmp + _OUT_EOL ) } )
   ELSE
      AEval( aOpt_Help, {| tmp | OutOpt( hbmk, tmp ) } )
   ENDIF

   RETURN

#define _OPT_WIDTH 22

STATIC PROCEDURE OutOpt( hbmk, aOpt )
   LOCAL nLine
   LOCAL nLines

   IF Empty( aOpt )
      OutStd( _OUT_EOL )
   ELSE
      IF Len( aOpt ) > 1
         aOpt[ 2 ] := StrTran( aOpt[ 2 ], "\n", hb_eol() )
         nLines := Max( MLCount( aOpt[ 2 ], hbmk[ _HBMK_nMaxCol ] - _OPT_WIDTH ),;
                        MLCount( aOpt[ 1 ], _OPT_WIDTH ) )
         FOR nLine := 1 TO nLines
            OutStd( PadR( Space( 2 ) + MemoLine( aOpt[ 1 ], _OPT_WIDTH, nLine ), _OPT_WIDTH ) )
            OutStd( RTrim( MemoLine( aOpt[ 2 ], hbmk[ _HBMK_nMaxCol ] - _OPT_WIDTH, nLine ) ) + _OUT_EOL )
         NEXT
      ELSE
         OutStd( Space( 2 ) + aOpt[ 1 ] + _OUT_EOL )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE OutNote( hbmk, cText )
   LOCAL nLine
   LOCAL nLines
   LOCAL tmp

   cText := StrTran( cText, "\n", hb_eol() )
   nLines := MLCount( cText, hbmk[ _HBMK_nMaxCol ] - 4 )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := RTrim( MemoLine( cText, hbmk[ _HBMK_nMaxCol ] - 4, nLine ) ) )
         IF nLine == 1
            OutStd( PadR( "  -", 4 ) )
         ELSE
            OutStd( Space( 4 ) )
         ENDIF
         OutStd( tmp + _OUT_EOL )
     ENDIF
   NEXT

   RETURN

STATIC PROCEDURE hbmk_OutStd( hbmk, cText )
   LOCAL nLine
   LOCAL nLines
   LOCAL nWidth
   LOCAL cPrefix
   LOCAL tmp

   IF hbmk[ _HBMK_lShowLevel ]
      nWidth := 10
      cPrefix := hb_StrFormat( "hbmk2 #%1$s:", hb_ntos( hbmk[ _HBMK_nLevel ] ) )
   ELSE
      nWidth := 7
      cPrefix := "hbmk2:"
   ENDIF

   cText := StrTran( cText, "\n", hb_eol() )
   nLines := MLCount( cText, hbmk[ _HBMK_nMaxCol ] - nWidth )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := RTrim( MemoLine( cText, hbmk[ _HBMK_nMaxCol ] - nWidth, nLine ) ) )
         OutStd( iif( nLine == 1, PadR( cPrefix, nWidth ), Space( nWidth ) ) + tmp + _OUT_EOL )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE hbmk_OutErr( hbmk, cText )
   LOCAL nLine
   LOCAL nLines
   LOCAL nWidth
   LOCAL cPrefix
   LOCAL tmp

   IF hbmk[ _HBMK_lShowLevel ]
      nWidth := 10
      cPrefix := hb_StrFormat( "hbmk2 #%1$s:", hb_ntos( hbmk[ _HBMK_nLevel ] ) )
   ELSE
      nWidth := 7
      cPrefix := "hbmk2:"
   ENDIF

   cText := StrTran( cText, "\n", hb_eol() )
   nLines := MLCount( cText, hbmk[ _HBMK_nMaxCol ] - nWidth )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := RTrim( MemoLine( cText, hbmk[ _HBMK_nMaxCol ] - nWidth, nLine ) ) )
         OutErr( iif( nLine == 1, PadR( cPrefix, nWidth ), Space( nWidth ) ) + tmp + _OUT_EOL )
      ENDIF
   NEXT

   RETURN
