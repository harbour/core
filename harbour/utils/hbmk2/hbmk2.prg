/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Make (alias hbmk, alias hbmk2)
 *
 * Copyright 1999-2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
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
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
 *    gcc and *nix configuration elements.
 *    bash script with similar purpose for gcc family.
 *    entry point override method and detection code for gcc.
 *    rtlink/blinker link script parsers.
 *    POTMerge(), LoadPOTFilesAsHash(), GenHBL() and AutoTrans().
 *       (with local modifications by hbmk author)
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
#include "fileio.ch"
#include "hbgtinfo.ch"
#include "hbver.ch"

/* NOTE: Keep this code clean from any kind of contribs and Harbour level
         3rd party library/tool information. This (the hbmk) component
         shall only contain hard-wired knowledge on Harbour _core_
         (official interfaces preferred), C compilers and OS details on
         the smallest possible level.
         Instead, 3rd party Harbour packages are recommended to maintain
         and provide .hbc files themselves, as part of their standard
         distribution packages. You can find a few such .hbc examples in
         the 'examples' directory. For Harbour contribs, the recommended
         method is to supply and maintain .hbc files in their respective
         directories, usually under tests (or utils, samples). As of this
         writing, most of them has one created.
         Thank you. [vszakats] */

/* TODO: Support debug/release modes. Some default setting can be set
         accordingly, and user can use it to further tweak settings. */
/* TODO: Support unicode/non-unicode build modes. */
/* TODO: Create temporary .c files with mangled names, to avoid
         incidentally overwriting existing .c file with the same name.
         Problems to solve: -hbcc compatibility (the feature has to be
         disabled when this switch is uses). Collision with -o harbour
         option isn't a problem, since we're overriding it already for
         hbmk, but we will need to deal with "/" prefixed variant. Since
         we need to use -o Harbour switch, it will be a problem also when
         user tries to use -p option, .ppo files will be generated in temp
         dir. */
/* TODO: Further clean hbmk context var usage (hbmk2 scope, project scope,
         adding rest of variables). */
/* TODO: Add a way to fallback to stop if required headers couldn't be found.
         This needs a way to spec what key headers to look for. */

#ifndef _HBMK_EMBEDDED_

#pragma linenumber=on

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
#elif defined( __PLATFORM__UNIX )
   REQUEST HB_GT_TRM
#endif

/* Extend as needed */
REQUEST HB_CODEPAGE_DE850, HB_CODEPAGE_DEISO
REQUEST HB_CODEPAGE_ES850, HB_CODEPAGE_ESISO
REQUEST HB_CODEPAGE_FR850, HB_CODEPAGE_FRISO
REQUEST HB_CODEPAGE_HU852, HB_CODEPAGE_HUISO
REQUEST HB_CODEPAGE_IT850, HB_CODEPAGE_ITISO
REQUEST HB_CODEPAGE_PL852, HB_CODEPAGE_PLISO
REQUEST HB_CODEPAGE_PT850, HB_CODEPAGE_PTISO
REQUEST HB_CODEPAGE_RU866, HB_CODEPAGE_RUISO

#endif

REQUEST hbmk_ARCH
REQUEST hbmk_COMP
REQUEST hbmk_KEYW

#define I_( x )                 hb_i18n_gettext( x )

#define _PAR_cParam             1
#define _PAR_cFileName          2
#define _PAR_nLine              3

#define _COMPR_OFF              0
#define _COMPR_DEF              1
#define _COMPR_MIN              2
#define _COMPR_MAX              3

#define _HEAD_OFF               0
#define _HEAD_PARTIAL           1
#define _HEAD_FULL              2

#define _COMPDET_bBlock         1
#define _COMPDET_cCOMP          2
#define _COMPDET_cCCPREFIX      3 /* optional */

#define _COMPDETE_bBlock        1
#define _COMPDETE_cARCH         2
#define _COMPDETE_cCOMP         3
#define _COMPDETE_cCCPREFIX     4
#define _COMPDETE_bSetup        5

#define _HBMODE_NATIVE          0
#define _HBMODE_HB10            1
#define _HBMODE_XHB             2
#define _HBMODE_RAW_C           3

#define _CONF_RELEASE           0 /* No debug */
#define _CONF_DEBUG             1 /* Harbour level debug */
#define _CONF_FULLDEBUG         2 /* Harbour + C level debug */

#define _ESC_NONE               0
#define _ESC_DBLQUOTE           1
#define _ESC_SINQUOTE_WATCOM    2
#define _ESC_NIX                3

#define _MACRO_NORM_PREFIX      "$"
#define _MACRO_LATE_PREFIX      "%"
#define _MACRO_PREFIX_ALL       ( _MACRO_NORM_PREFIX + _MACRO_LATE_PREFIX )
#define _MACRO_OPEN             "{"
#define _MACRO_CLOSE            "}"

#define _CMDSUBST_OPEN          "`"
#define _CMDSUBST_CLOSE         _CMDSUBST_OPEN

#define _LNG_MARKER             ( _MACRO_LATE_PREFIX + _MACRO_OPEN + "hb_lng" + _MACRO_CLOSE )

#define _HBMK_CFG_NAME          "hbmk.cfg"
#define _HBMK_AUTOHBM_NAME      "hbmk.hbm"

#define _HBMK_NEST_MAX          10
#define _HBMK_HEAD_NEST_MAX     10

#define _COMPEMBED_BASE_        ( "comp" + hb_osPathSeparator() )

#define _WORKDIR_BASE_          ".hbmk"
#define _WORKDIR_DEF_           ( _WORKDIR_BASE_ + hb_osPathSeparator() + hbmk[ _HBMK_cARCH ] + hb_osPathSeparator() + hbmk[ _HBMK_cCOMP ] )

#define _BCC_BIN_DETECT()       FindInPath( "bcc32" )

#define OutStd( x )             low_OutStd( hbmk[ _HBMK_lUTF8 ], x )
#define OutErr( x )             low_OutErr( hbmk[ _HBMK_lUTF8 ], x )

#define HB_ISALPHA( c )         ( Upper( c ) >= "A" .AND. Upper( c ) <= "Z" )
#define HB_ISFIRSTIDCHAR( c )   ( HB_ISALPHA( c ) .OR. ( c ) == '_' )
#define HB_ISNEXTIDCHAR( c )    ( HB_ISFIRSTIDCHAR(c) .OR. IsDigit( c ) )

#define LEFTEQUAL( l, r )       ( l = r ) /* NOTE: This requires Set( _SET_EXACT, .F. ) */

#define _HBMK_lQuiet            1
#define _HBMK_lInfo             2
#define _HBMK_cARCH             3
#define _HBMK_cCOMP             4
#define _HBMK_cGTDEFAULT        5
#define _HBMK_aLIBCOREGT        6
#define _HBMK_cGT               7

#define _HBMK_cHB_BIN_INSTALL   8
#define _HBMK_cHB_LIB_INSTALL   9
#define _HBMK_cHB_DYN_INSTALL   10
#define _HBMK_cHB_INC_INSTALL   11

#define _HBMK_lGUI              12
#define _HBMK_lMT               13
#define _HBMK_lDEBUG            14
#define _HBMK_nHEAD             15
#define _HBMK_aINCPATH          16
#define _HBMK_aINCTRYPATH       17
#define _HBMK_lREBUILD          18
#define _HBMK_lTRACE            19
#define _HBMK_lDONTEXEC         20
#define _HBMK_nHBMODE           21
#define _HBMK_cUILNG            22
#define _HBMK_cUICDP            23
#define _HBMK_aLIBUSER          24
#define _HBMK_aLIBUSERGT        25
#define _HBMK_aLIBPATH          26
#define _HBMK_aLIBDYNHAS        27
#define _HBMK_aINSTPATH         28
#define _HBMK_aOPTC             29
#define _HBMK_aOPTPRG           30
#define _HBMK_aOPTRES           31
#define _HBMK_aOPTL             32
#define _HBMK_aOPTA             33
#define _HBMK_aOPTD             34
#define _HBMK_lCPP              35
#define _HBMK_lSHARED           36
#define _HBMK_lSTATICFULL       37
#define _HBMK_lSHAREDDIST       38
#define _HBMK_lNULRDD           39
#define _HBMK_lMAP              40
#define _HBMK_lBEEP             41
#define _HBMK_lSTRIP            42
#define _HBMK_lOPTIM            43
#define _HBMK_nCOMPR            44
#define _HBMK_lRUN              45
#define _HBMK_lINC              46
#define _HBMK_lREBUILDPO        47
#define _HBMK_lMINIPO           48
#define _HBMK_lUNICODE          49
#define _HBMK_nCONF             50

#define _HBMK_cFIRST            51
#define _HBMK_aPRG              52
#define _HBMK_aC                53
#define _HBMK_aRESSRC           54
#define _HBMK_aRESCMP           55
#define _HBMK_aOBJUSER          56

#define _HBMK_aPO               57
#define _HBMK_cHBL              58
#define _HBMK_cHBLDir           59
#define _HBMK_aLNG              60
#define _HBMK_cPO               61

#define _HBMK_lDEBUGTIME        62
#define _HBMK_lDEBUGINC         63
#define _HBMK_lDEBUGSTUB        64
#define _HBMK_lDEBUGI18N        65

#define _HBMK_cCCPATH           66
#define _HBMK_cCCPREFIX         67
#define _HBMK_cCCPOSTFIX        68

#define _HBMK_lUTF8             69

#define _HBMK_MAX_              69

#ifndef _HBMK_EMBEDDED_

PROCEDURE Main( ... )
   LOCAL aArgsIn := hb_AParams()
   LOCAL aArgsProc := {}
   LOCAL nResult
   LOCAL cName
   LOCAL tmp, tmp1

   LOCAL lPause := hb_gtInfo( HB_GTI_ISGRAPHIC )
   LOCAL lUTF8

   LOCAL aArgsTarget
   LOCAL nTarget
   LOCAL nTargetTODO
   LOCAL lHadTarget

   LOCAL lOldExact := Set( _SET_EXACT, .F. )

   /* Emulate -hbcmp, -hbcc, -hblnk switches when certain
      self names are detected.
      For compatibility with hbmk script aliases. */

   IF ! Empty( aArgsIn )

      hb_FNameSplit( hb_argv( 0 ),, @cName )

      tmp := Lower( cName )

      IF Left( tmp, 1 ) == "x"
         tmp := SubStr( tmp, 2 )
         AAdd( aArgsProc, "-xhb" )
      ELSEIF Right( tmp, 2 ) == "10"
         AAdd( aArgsProc, "-hb10" )
      ENDIF

      DO CASE
      CASE Right( tmp, 5 ) == "hbcmp" .OR. ;
           Left(  tmp, 5 ) == "hbcmp" .OR. ;
           tmp == "clipper"                ; AAdd( aArgsProc, "-hbcmp" )
      CASE Right( tmp, 4 ) == "hbcc" .OR. ;
           Left(  tmp, 4 ) == "hbcc"       ; AAdd( aArgsProc, "-hbcc" )
      CASE Right( tmp, 5 ) == "hblnk" .OR. ;
           Left(  tmp, 5 ) == "hblnk"      ; AAdd( aArgsProc, "-hblnk" )
      CASE tmp == "rtlink" .OR. ;
           tmp == "exospace" .OR. ;
           tmp == "blinker"                ; AAdd( aArgsProc, "-rtlink" )
      CASE Right( tmp, 5 ) == "hblib" .OR. ;
           Left(  tmp, 5 ) == "hblib"      ; AAdd( aArgsProc, "-hblib" )
      CASE Right( tmp, 5 ) == "hbdyn" .OR. ;
           Left(  tmp, 5 ) == "hbdyn"      ; AAdd( aArgsProc, "-hbdyn" )
      ENDCASE
   ENDIF

   /* Expand wildcard project specs */

   FOR EACH tmp IN aArgsIn
      DO CASE
      CASE Lower( FN_ExtGet( tmp ) ) == ".hbp"
         FOR EACH tmp1 IN FN_Expand( tmp, .T. )
            AAdd( aArgsProc, tmp1 )
         NEXT
      CASE Lower( Left( tmp, Len( "-target=" ) ) ) == "-target="
         FOR EACH tmp1 IN FN_Expand( SubStr( tmp, Len( "-target=" ) + 1 ), .F. )
            AAdd( aArgsProc, "-target=" + tmp1 )
         NEXT
      OTHERWISE
         AAdd( aArgsProc, tmp )
      ENDCASE
   NEXT

   /* Handle multitarget command lines */

   nTargetTODO := 1
   DO WHILE .T.

      aArgsTarget := {}
      nTarget := 0
      lHadTarget := .F.

      FOR EACH tmp IN aArgsProc
         DO CASE
         CASE Lower( FN_ExtGet( tmp ) ) == ".hbp" .AND. ! lHadTarget
            nTarget++
            IF nTarget == nTargetTODO
               AAdd( aArgsTarget, tmp )
            ENDIF
         CASE Lower( Left( tmp, Len( "-target=" ) ) ) == "-target="
            nTarget++
            IF nTarget == nTargetTODO
               AAdd( aArgsTarget, SubStr( tmp, Len( "-target=" ) + 1 ) )
            ENDIF
         CASE Lower( tmp ) == "-target"
            nTarget++
            lHadTarget := .T.
         CASE Lower( tmp ) == "-alltarget"
            lHadTarget := .F.
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
      nResult := hbmk( aArgsTarget, @lPause, @lUTF8 )

      /* Exit on first failure */
      IF nResult != 0
         EXIT
      ENDIF

      nTargetTODO++
   ENDDO

   IF nResult != 0 .AND. lPause
      low_OutStd( lUTF8, I_( "Press any key to continue..." ) )
      Inkey( 0 )
   ENDIF

   ErrorLevel( nResult )

   Set( _SET_EXACT, lOldExact )

   RETURN

#endif

STATIC FUNCTION hbmk_run( hbmk, cCmd, cStdOut )
#if defined( __PLATFORM__DOS )
   LOCAL nResult
   LOCAL hFile
   LOCAL cFileName
   IF PCount() >= 2
      hFile := hb_FTempCreateEx( @cFileName )
      IF hFile != F_ERROR
         FClose( hFile )
         cCmd += ">" + cFileName
         nResult := hb_run( cCmd )
         cStdOut := hb_MemoRead( cFileName )
         FErase( cFileName )
      ELSE
         nResult := -1
         cStdOut := ""
         hbmk_OutErr( hbmk, I_( "Error: Cannot create temporary file." ) )
      ENDIF
      RETURN nResult
   ENDIF
   RETURN hb_run( cCmd )
#else
   LOCAL hStdOut
   LOCAL h
   LOCAL result

   HB_SYMBOL_UNUSED( hbmk )

   IF PCount() >= 2
      h := hb_ProcessOpen( cCmd,, @hStdOut )
   ELSE
      h := hb_ProcessOpen( cCmd )
   ENDIF
   IF h != F_ERROR
      IF PCount() >= 2
         cStdOut := hbmk_ReadHnd( hStdOut )
      ENDIF
      result := hb_ProcessValue( h )
      hb_ProcessClose( h, .T. )
      IF PCount() >= 2
         FClose( hStdOut )
      ENDIF
   ELSE
      result := -1
      IF PCount() >= 2
         cStdOut := ""
      ENDIF
   ENDIF
   RETURN result

STATIC FUNCTION hbmk_ReadHnd( hFile )
   LOCAL cBuffer := Space( 4096 )
   LOCAL cString := ""
   LOCAL nLen

   DO WHILE ( nLen := FRead( hFile, @cBuffer, Len( cBuffer ) ) ) > 0
      cString += Left( cBuffer, nLen )
   ENDDO

   RETURN cString
#endif

STATIC PROCEDURE hbmk_COMP_Setup( cARCH, cCOMP, cBasePath )

   /* TODO: Use HB_CCPREFIX instead of PATH modification, where possible. */

   /* NOTE: We have to retain existing PATH as we may need some tools
            from it, like upx compressor. [vszakats] */

   cBasePath := PathNormalize( cBasePath )

   DO CASE
   CASE cARCH == "win" .AND. cCOMP == "watcom"

      hb_SetEnv( "WATCOM", cBasePath )
      hb_SetEnv( "PATH", cBasePath + hb_osPathSeparator() + "BINNT;" + cBasePath + hb_osPathSeparator() + "BINW;" + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "EDPATH", cBasePath + hb_osPathSeparator() + "EDDAT" )
      hb_SetEnv( "INCLUDE", cBasePath + hb_osPathSeparator() + "H;" + cBasePath + hb_osPathSeparator() + "H" + hb_osPathSeparator() + "NT" )

   CASE cARCH == "win" .AND. cCOMP == "pocc"

      hb_SetEnv( "PATH", cBasePath + hb_osPathSeparator() + "Bin;" + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_osPathSeparator() + "Include;" + cBasePath + hb_osPathSeparator() + "Include" + hb_osPathSeparator() + "Win" )
      hb_SetEnv( "LIB", cBasePath + hb_osPathSeparator() + "Lib;" + cBasePath + hb_osPathSeparator() + "Lib" + hb_osPathSeparator() + "Win" )

   CASE cARCH == "win" .AND. cCOMP == "pocc64"

      hb_SetEnv( "PATH", cBasePath + hb_osPathSeparator() + "Bin;" + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_osPathSeparator() + "Include;" + cBasePath + hb_osPathSeparator() + "Include" + hb_osPathSeparator() + "Win" )
      hb_SetEnv( "LIB", cBasePath + hb_osPathSeparator() + "Lib;" + cBasePath + hb_osPathSeparator() + "Lib" + hb_osPathSeparator() + "Win64" )

   CASE cARCH == "wce" .AND. cCOMP == "poccarm"

      hb_SetEnv( "PATH", cBasePath + hb_osPathSeparator() + "Bin;" + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "INCLUDE", cBasePath + hb_osPathSeparator() + "Include" + hb_osPathSeparator() + "WinCE;" + cBasePath + hb_osPathSeparator() + "Include" )
      hb_SetEnv( "LIB", cBasePath + hb_osPathSeparator() + "Lib;" + cBasePath + hb_osPathSeparator() + "Lib" + hb_osPathSeparator() + "WinCE" )

   CASE cARCH == "dos" .AND. cCOMP == "djgpp"

      hb_SetEnv( "DJGPP", cBasePath + hb_osPathSeparator() + "djgpp.env" )
      hb_SetEnv( "PATH", cBasePath + hb_osPathSeparator() + "bin;" + hb_GetEnv( "PATH" ) )

   CASE cARCH == "dos" .AND. cCOMP == "watcom"

      hb_SetEnv( "WATCOM", cBasePath )
      hb_SetEnv( "PATH", cBasePath + hb_osPathSeparator() + "BINW;" + hb_GetEnv( "PATH" ) )
      hb_SetEnv( "EDPATH", cBasePath + hb_osPathSeparator() + "EDDAT" )
      hb_SetEnv( "INCLUDE", cBasePath + hb_osPathSeparator() + "H" )

   ENDCASE

   RETURN

FUNCTION hbmk( aArgs, /* @ */ lPause, /* @ */ lUTF8 )

   LOCAL hbmk[ _HBMK_MAX_ ]

   LOCAL aLIB_BASE1
   LOCAL aLIB_BASE2
   LOCAL aLIB_BASE_GT
   LOCAL aLIB_BASE_PCRE
   LOCAL aLIB_BASE_ZLIB
   LOCAL aLIB_BASE_DEBUG
   LOCAL aLIB_BASE_CPLR
   LOCAL aLIB_BASE_ST
   LOCAL aLIB_BASE_MT
   LOCAL aLIB_BASE_NULRDD
   LOCAL aLIB_BASE_RDD_ST
   LOCAL aLIB_BASE_RDD_MT

   LOCAL l_cCSTUB

   LOCAL l_cHB_INSTALL_PREFIX
   LOCAL l_cHB_BIN_INSTALL
   LOCAL l_cHB_LIB_INSTALL
   LOCAL l_cHB_DYN_INSTALL
   LOCAL l_cHB_INC_INSTALL

   LOCAL l_aPRG_TODO
   LOCAL l_aPRG_DONE
   LOCAL l_aC_TODO
   LOCAL l_aC_DONE
   LOCAL l_aRESSRC_TODO
   LOCAL l_aLIBSHARED
   LOCAL l_aLIBSHAREDPOST := {}
   LOCAL l_aLIB
   LOCAL l_aLIBA
   LOCAL l_aLIBRAW
   LOCAL l_aLIBVM
   LOCAL l_aLIBHB
   LOCAL l_aLIBHBGT
   LOCAL l_aLIB3RD
   LOCAL l_aLIBSYS
   LOCAL l_aLIBSYSCORE := {}
   LOCAL l_aLIBSYSMISC := {}
   LOCAL l_aOPTRUN
   LOCAL l_cPROGDIR
   LOCAL l_cPROGNAME
   LOCAL l_aOBJ
   LOCAL l_aOBJA
   LOCAL l_aCLEAN
   LOCAL l_lHB_PCRE := .T.
   LOCAL l_lHB_ZLIB := .T.
   LOCAL l_cMAIN := NIL
   LOCAL l_cVCSDIR
   LOCAL l_cVCSHEAD
   LOCAL l_cTSHEAD
   LOCAL l_cHBPOSTFIX := ""
   LOCAL l_lNOHBLIB := .F.

   LOCAL l_lBLDFLGP := .F.
   LOCAL l_lBLDFLGC := .F.
   LOCAL l_lBLDFLGL := .F.
   LOCAL l_lCLEAN := .F.
   LOCAL l_nJOBS := 1

   LOCAL aCOMPDET
   LOCAL aCOMPDET_EMBED
   LOCAL aCOMPSUP

   LOCAL cLibPrefix
   LOCAL cLibExt
   LOCAL cObjPrefix
   LOCAL cObjExt
   LOCAL cLibLibExt
   LOCAL cLibLibPrefix := ""
   LOCAL cLibObjPrefix
   LOCAL cDynObjPrefix := NIL
   LOCAL cLibPathPrefix
   LOCAL cLibPathSep
   LOCAL cDynLibNamePrefix
   LOCAL cDynLibExt
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
   LOCAL nCmd_Esc := NIL
   LOCAL nScr_Esc := NIL
   LOCAL nCCompVer

   LOCAL cCommand
   LOCAL aCommand
   LOCAL cOpt_CompC
   LOCAL cOpt_CompCLoop
   LOCAL cOpt_Link
   LOCAL cOpt_Res
   LOCAL cOpt_Lib
   LOCAL cOpt_Dyn
   LOCAL cBin_CompPRG
   LOCAL cBin_CompC
   LOCAL cBin_Link
   LOCAL cBin_Res
   LOCAL cBin_Lib
   LOCAL cBin_Dyn
   LOCAL cPath_CompC
   LOCAL nErrorLevel := 0
   LOCAL tmp, tmp1, tmp2, array
   LOCAL cScriptFile
   LOCAL fhnd
   LOCAL lNOHBC
   LOCAL lSysLoc
   LOCAL cPrefix
   LOCAL cPostfix
   LOCAL cCCEXT_mingw

   LOCAL lSkipBuild := .F.
   LOCAL lStopAfterInit := .F.
   LOCAL lStopAfterHarbour := .F.
   LOCAL lStopAfterCComp := .F.
   LOCAL lAcceptCFlag := .F.
   LOCAL lAcceptLDFlag := .F.
   LOCAL lCreateLib := .F.
   LOCAL lCreateDyn := .F.
   LOCAL lAcceptLDClipper := .F.

   LOCAL cWorkDir := NIL

   LOCAL aParams
   LOCAL aParam
   LOCAL cParam
   LOCAL cParamL

   LOCAL tTarget
   LOCAL lTargetUpToDate

   LOCAL cDir, cName, cExt
   LOCAL headstate

   LOCAL cSelfFlagPRG := hb_Version( HB_VERSION_FLAG_PRG )
   LOCAL cSelfFlagC   := hb_Version( HB_VERSION_FLAG_C )
   LOCAL cSelfFlagL   := hb_Version( HB_VERSION_FLAG_LINKER )

   LOCAL cDL_Version_Alter
   LOCAL cDL_Version

   LOCAL aTODO
   LOCAL aThreads
   LOCAL thread

   LOCAL nStart := Seconds()

   hbmk[ _HBMK_lQuiet ] := .F.
   hbmk[ _HBMK_lInfo ] := .F.
   hbmk[ _HBMK_lUTF8 ] := .F.

   hbmk[ _HBMK_lCPP ] := NIL
   hbmk[ _HBMK_lGUI ] := .F.
   hbmk[ _HBMK_lMT ] := .F.
   hbmk[ _HBMK_lDEBUG ] := .F.
   hbmk[ _HBMK_nHEAD ] := _HEAD_PARTIAL
   hbmk[ _HBMK_lREBUILD ] := .F.
   hbmk[ _HBMK_lTRACE ] := .F.
   hbmk[ _HBMK_lDONTEXEC ] := .F.
   hbmk[ _HBMK_nHBMODE ] := _HBMODE_NATIVE
   hbmk[ _HBMK_lSHAREDDIST ] := NIL
   hbmk[ _HBMK_lNULRDD ] := .F.
   hbmk[ _HBMK_lMAP ] := .F.
   hbmk[ _HBMK_lBEEP ] := .F.
   hbmk[ _HBMK_lSTRIP ] := .F.
   hbmk[ _HBMK_lOPTIM ] := .T.
   hbmk[ _HBMK_nCOMPR ] := _COMPR_OFF
   hbmk[ _HBMK_lRUN ] := .F.
   hbmk[ _HBMK_lINC ] := .F.
   hbmk[ _HBMK_lREBUILDPO ] := .F.
   hbmk[ _HBMK_lMINIPO ] := .F.
   hbmk[ _HBMK_nCONF ] := _CONF_RELEASE

   hbmk[ _HBMK_lDEBUGTIME ] := .F.
   hbmk[ _HBMK_lDEBUGINC ] := .F.
   hbmk[ _HBMK_lDEBUGSTUB ] := .F.
   hbmk[ _HBMK_lDEBUGI18N ] := .F.

   GetUILangCDP( @hbmk[ _HBMK_cUILNG ], @hbmk[ _HBMK_cUICDP ] )
   SetUILang( hbmk )

   lUTF8 := hbmk[ _HBMK_lUTF8 ]

   IF Empty( aArgs )
      ShowHeader( hbmk )
      ShowHelp( hbmk )
      RETURN 19
   ENDIF

   FOR EACH cParam IN aArgs

      cParamL := Lower( cParam )

      /* NOTE: Don't forget to make these ignored in the main
               option processing loop. */
      DO CASE
      CASE cParamL            == "-quiet"   ; hbmk[ _HBMK_lQuiet ] := .T. ; hbmk[ _HBMK_lInfo ] := .F.
      CASE Left( cParamL, 6 ) == "-comp="   ; hbmk[ _HBMK_cCOMP ] := SubStr( cParam, 7 )
      CASE Left( cParamL, 6 ) == "-arch="   ; hbmk[ _HBMK_cARCH ] := SubStr( cParam, 7 )
      CASE Left( cParamL, 6 ) == "-lang="   ; hbmk[ _HBMK_cUILNG ] := SubStr( cParam, 7 ) ; SetUILang( hbmk )
      CASE cParamL            == "-hbrun"   ; lSkipBuild := .T. ; hbmk[ _HBMK_lRUN ] := .T.
      CASE cParamL            == "-hbraw"   ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .T. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .F. ; lAcceptCFlag := .F. ; lAcceptLDFlag := .F.
      CASE cParamL            == "-hbcmp" .OR. ;
           cParamL            == "-clipper" ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .F. ; lAcceptCFlag := .F. ; lAcceptLDFlag := .F.
      CASE cParamL            == "-hbcc"    ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptCFlag := .T.
      CASE cParamL            == "-hblnk"   ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDFlag := .T.
      CASE cParamL            == "-rtlink" .OR. ;
           cParamL            == "-exospace" .OR. ;
           cParamL            == "-blinker" ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .F. ; lAcceptLDClipper := .T.
      CASE cParamL            == "-info"    ; hbmk[ _HBMK_lInfo ] := .T.
      CASE cParamL            == "-xhb"     ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_XHB
      CASE cParamL            == "-hb10"    ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_HB10
      CASE cParamL            == "-hbc"     ; hbmk[ _HBMK_nHBMODE ] := _HBMODE_RAW_C ; lAcceptCFlag := .T.
      CASE cParamL            == "-nohblib" ; l_lNOHBLIB := .T.
      CASE cParamL == "-help" .OR. ;
           cParamL == "--help"

         ShowHeader( hbmk )
         ShowHelp( hbmk, .T. )
         RETURN 19

      CASE cParamL == "--version"

         ShowHeader( hbmk )
         RETURN 0

      ENDCASE
   NEXT

   /* Initialize Harbour libs */

   IF ! ( hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB )

      cDL_Version_Alter := "-" +;
                           hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) +;
                           hb_ntos( hb_Version( HB_VERSION_MINOR ) )
      cDL_Version       := "." +;
                           hb_ntos( hb_Version( HB_VERSION_MAJOR ) ) + "." +;
                           hb_ntos( hb_Version( HB_VERSION_MINOR ) ) + "." +;
                           hb_ntos( hb_Version( HB_VERSION_RELEASE ) )

      aLIB_BASE1 := {;
         "hbcpage" ,;
         "hblang" ,;
         "hbcommon" }

      aLIB_BASE2 := {;
         "hbrtl" ,;
         "hbpp" ,;
         "hbmacro" ,;
         "hbextern" }

      aLIB_BASE_GT := {;
         "gtcgi" ,;
         "gtpca" ,;
         "gtstd" }

      aLIB_BASE_PCRE := {;
         "hbpcre" }

      aLIB_BASE_ZLIB := {;
         "hbzlib" }

      aLIB_BASE_DEBUG := {;
         "hbdebug" }

      aLIB_BASE_CPLR := {;
         "hbcplr" }

      aLIB_BASE_ST := {;
         "hbvm" }
      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_HB10
         aLIB_BASE_MT := {;
            "hbvmmt" }
      ELSE
         aLIB_BASE_MT := {;
            "hbvm" }
      ENDIF

      aLIB_BASE_NULRDD := {;
         "hbnulrdd" }

      aLIB_BASE_RDD_ST := {;
         "hbrdd" ,;
         "hbusrrdd" ,;
         "hbhsx" ,;
         "hbsix" ,;
         "rddntx" ,;
         "rddcdx" ,;
         "rddfpt" }

      /* These libs have been added after 1.0.x */
      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_HB10
         AAdd( aLIB_BASE_RDD_ST, "hbuddall" )
         AAdd( aLIB_BASE_RDD_ST, "rddnsx" )
      ENDIF

      aLIB_BASE_RDD_MT := aLIB_BASE_RDD_ST

   ELSE

      cDL_Version_Alter := ""
      cDL_Version       := ""

      aLIB_BASE1 := {;
         "codepage" ,;
         "lang" ,;
         "common" }

      aLIB_BASE2 := {}

      aLIB_BASE_GT := {;
         "gtcgi" ,;
         "gtpca" ,;
         "gtstd" }

      aLIB_BASE_PCRE := {;
         "pcrepos" }

      aLIB_BASE_ZLIB := {;
         "zlib" }

      aLIB_BASE_DEBUG := {;
         "debug" }

      aLIB_BASE_CPLR := {}

      aLIB_BASE_ST := {;
         "vm" ,;
         "rtl" ,;
         "macro" ,;
         "pp" }
      aLIB_BASE_MT := {;
         "vmmt" ,;
         "rtlmt" ,;
         "macromt" ,;
         "ppmt" }

      aLIB_BASE_NULRDD := {;
         "nulsys" }

      aLIB_BASE_RDD_ST := {;
         "rdd" ,;
         "usrrdd" ,;
         "rdds" ,;
         "hsx" ,;
         "hbsix" ,;
         "dbfntx" ,;
         "dbfcdx" ,;
         "dbffpt" }

      aLIB_BASE_RDD_MT := {;
         "rddmt" ,;
         "usrrddmt" ,;
         "rddsmt" ,;
         "hsxmt" ,;
         "hbsixmt" ,;
         "dbfntxmt" ,;
         "dbfcdxmt" ,;
         "dbffptmt" }
   ENDIF

   IF l_lNOHBLIB
      aLIB_BASE1 := {}
      aLIB_BASE2 := {}
      aLIB_BASE_GT := {}
      aLIB_BASE_PCRE := {}
      aLIB_BASE_ZLIB := {}
      aLIB_BASE_DEBUG := {}
      aLIB_BASE_CPLR := {}
      aLIB_BASE_ST := {}
      aLIB_BASE_MT := {}
      aLIB_BASE_NULRDD := {}
      aLIB_BASE_RDD_ST := {}
      aLIB_BASE_RDD_MT := {}
   ENDIF

   /* Load architecture / compiler settings (compatibility) */

   IF Empty( hbmk[ _HBMK_cARCH ] )
      hbmk[ _HBMK_cARCH ] := Lower( GetEnv( "HB_ARCHITECTURE" ) )
#if 0
      IF Empty( hbmk[ _HBMK_cARCH ] )
         hbmk[ _HBMK_cARCH ] := Lower( GetEnv( "HB_ARCH" ) )
      ENDIF
#endif
   ENDIF
   IF Empty( hbmk[ _HBMK_cCOMP ] )
      hbmk[ _HBMK_cCOMP ] := Lower( GetEnv( "HB_COMPILER" ) )
#if 0
      IF Empty( hbmk[ _HBMK_cCOMP ] )
         hbmk[ _HBMK_cCOMP ] := Lower( GetEnv( "HB_COMP" ) )
      ENDIF
#endif
   ENDIF

   nCCompVer := Val( GetEnv( "HB_COMPILER_VER" ) ) /* Format: <09><00>[.<00>] = <major><minor>[.<revision>] */
#if 0
   IF Empty( nCCompVer )
      nCCompVer := Val( GetEnv( "HB_COMP_VER" ) )
   ENDIF
#endif

   /* Autodetect architecture */

   IF Empty( hbmk[ _HBMK_cARCH ] )

      /* NOTE: Keep this in sync manually. All compilers should be listed here,
               which are supported on one architecture only. In the future this
               should be automatically extracted from a comp/arch matrix. */
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
         hbmk[ _HBMK_cARCH ] := "win"
         EXIT
      CASE "mingwarm"
      CASE "msvcarm"
      CASE "poccarm"
         hbmk[ _HBMK_cARCH ] := "wce"
         EXIT
      CASE "djgpp"
         hbmk[ _HBMK_cARCH ] := "dos"
         EXIT
      OTHERWISE
         hbmk[ _HBMK_cARCH ] := hb_Version( HB_VERSION_BUILD_ARCH )
      ENDSWITCH
      IF ! Empty( hbmk[ _HBMK_cARCH ] )
         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected architecture: %1$s" ), hbmk[ _HBMK_cARCH ] ) )
         ENDIF
      ENDIF
   ENDIF

   hbmk[ _HBMK_cCCPATH ]    := GetEnv( "HB_CCPATH" )
   hbmk[ _HBMK_cCCPREFIX ]  := GetEnv( "HB_CCPREFIX" )
   hbmk[ _HBMK_cCCPOSTFIX ] := GetEnv( "HB_CCPOSTFIX" )

   #if defined( __PLATFORM__UNIX )
      cCCEXT_mingw := ""
   #else
      cCCEXT_mingw := ".exe"
   #endif

   /* Setup architecture dependent data */

   cBin_CompPRG := "harbour" + l_cHBPOSTFIX

   DO CASE
   CASE hbmk[ _HBMK_cARCH ] $ "bsd|hpux|sunos|linux" .OR. hbmk[ _HBMK_cARCH ] == "darwin" /* Separated to avoid match with 'win' */
      IF hbmk[ _HBMK_cARCH ] == "linux"
         aCOMPSUP := { "gcc", "watcom", "icc" }
      ELSE
         aCOMPSUP := { "gcc" }
      ENDIF
      l_aLIBHBGT := { "gttrm" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gttrm"
      cDynLibNamePrefix := "lib"
      cBinExt := ""
      cOptPrefix := "-"
      SWITCH hbmk[ _HBMK_cARCH ]
      CASE "darwin" ; cDynLibExt := ".dylib" ; EXIT
      CASE "hpux"   ; cDynLibExt := ".sl" ; EXIT
      OTHERWISE     ; cDynLibExt := ".so"
      ENDSWITCH
   CASE hbmk[ _HBMK_cARCH ] == "dos"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) }, "djgpp"  },;
                    { {|| FindInPath( "wpp386"   ) }, "watcom" } }
      aCOMPSUP := { "djgpp", "gcc", "watcom" }
      l_aLIBHBGT := { "gtdos" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtdos"
      cDynLibNamePrefix := ""
      cDynLibExt := ""
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE hbmk[ _HBMK_cARCH ] == "os2"
      aCOMPDET := { { {|| FindInPath( "gcc"      ) }, "gcc"    },;
                    { {|| FindInPath( "wpp386"   ) }, "watcom" } }
      aCOMPSUP := { "gcc", "watcom" }
      l_aLIBHBGT := { "gtos2" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtos2"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
   CASE hbmk[ _HBMK_cARCH ] == "win"
      /* Order is significant.
         watcom also keeps a cl.exe in its binary dir. */
      aCOMPDET := { { {|| FindInPath( "cygstart" ) }, "cygwin" },;
                    { {|| FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "gcc" ) }, "mingw"   },;
                    { {|| iif( ! Empty( GetEnv( "WATCOM" ) ),;
                               FindInPath( "wpp386"   ),;
                               NIL )               }, "watcom" },;
                    { {|| FindInPath( "ml64"     ) }, "msvc64" },;
                    { {|| iif( FindInPath( "wpp386"   ) == NIL,;
                               FindInPath( "cl"       ),;
                               NIL )                      }, "msvc"    },;
                    { {|| _BCC_BIN_DETECT()        }, "bcc"    },;
                    { {|| iif( FindInPath( "dbgeng.lib", GetEnv( "LIB" ) ) != NIL .AND. ( tmp1 := FindInPath( "pocc" ) ) != NIL, tmp1, NIL ) }, "pocc64"  },;
                    { {|| FindInPath( "pocc"     ) }, "pocc"   },;
                    { {|| iif( ( tmp1 := FindInPath( "icl" ) ) != NIL .AND. "itanium" $ Lower( tmp1 ), tmp1, NIL ) }, "iccia64" },;
                    { {|| FindInPath( "icl"      ) }, "icc"    },;
                    { {|| FindInPath( "xcc"      ) }, "xcc"    },;
                    { {|| FindInPath( "x86_64-w64-mingw32-gcc" ) }, "mingw64", "x86_64-w64-mingw32-" } }
      aCOMPSUP := { "mingw", "msvc", "bcc", "watcom", "icc", "pocc", "xcc", "cygwin",;
                    "mingw64", "msvc64", "msvcia64", "iccia64", "pocc64" }
      l_aLIBHBGT := { "gtwin", "gtwvt", "gtgui" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtwin"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
      /* NOTE: Some targets (pocc and watcom) need kernel32 explicitly. */
      l_aLIBSYSCORE := { "kernel32", "user32", "gdi32", "advapi32", "ws2_32" }
      l_aLIBSYSMISC := { "winspool", "comctl32", "comdlg32", "shell32", "ole32", "oleaut32", "uuid", "mpr", "winmm", "mapi32", "imm32", "msimg32" }
   CASE hbmk[ _HBMK_cARCH ] == "wce"
      aCOMPDET := { { {|| FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "gcc" ) }, "mingwarm" },;
                    { {|| FindInPath( "cl"       ) }, "msvcarm" },;
                    { {|| FindInPath( "pocc"     ) }, "poccarm" },;
                    { {|| FindInPath( "arm-mingw32ce-gcc" ) }, "mingwarm", "arm-mingw32ce-" } ,;
                    { {|| FindInPath( "arm-wince-mingw32ce-gcc" ) }, "mingwarm", "arm-wince-mingw32ce-" } }
      aCOMPSUP := { "mingwarm", "msvcarm", "poccarm" }
      l_aLIBHBGT := { "gtwvt", "gtgui" }
      hbmk[ _HBMK_cGTDEFAULT ] := "gtwvt"
      cDynLibNamePrefix := ""
      cDynLibExt := ".dll"
      cBinExt := ".exe"
      cOptPrefix := "-/"
      l_aLIBSYSCORE := { "wininet", "ws2", "commdlg", "commctrl" }
      l_aLIBSYSMISC := { "uuid", "ole32" }
   OTHERWISE
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Architecture value unknown: %1$s" ), hbmk[ _HBMK_cARCH ] ) )
      RETURN 1
   ENDCASE

   hbmk[ _HBMK_aLIBCOREGT ] := ArrayJoin( aLIB_BASE_GT, l_aLIBHBGT )

   /* Setup GUI state for Harbour default */
   SetupForGT( hbmk[ _HBMK_cGTDEFAULT ], NIL, @hbmk[ _HBMK_lGUI ] )

   /* Autodetect Harbour environment */

   IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

      /* Detect system locations to enable shared library option by default */
      lSysLoc := hb_DirBase() == "/usr/local/bin/" .OR. ;
                 hb_DirBase() == "/usr/bin/" .OR. ;
                 hb_DirBase() == "/opt/harbour/" .OR. ;
                 hb_DirBase() == "/opt/bin/"

      l_cHB_BIN_INSTALL := PathSepToSelf( GetEnv( "HB_BIN_INSTALL" ) )
      l_cHB_LIB_INSTALL := PathSepToSelf( GetEnv( "HB_LIB_INSTALL" ) )
      l_cHB_INC_INSTALL := PathSepToSelf( GetEnv( "HB_INC_INSTALL" ) )

      l_cHB_INSTALL_PREFIX := PathSepToSelf( GetEnv( "HB_INSTALL_PREFIX" ) )
      IF Empty( l_cHB_INSTALL_PREFIX )
         DO CASE
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + cBin_CompPRG + cBinExt )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".."
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + "bin" + hb_osPathSeparator() + cBin_CompPRG + cBinExt )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() )
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + "bin" + hb_osPathSeparator() + cBin_CompPRG + cBinExt )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".."
         CASE hb_FileExists( DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + "bin" + hb_osPathSeparator() + cBin_CompPRG + cBinExt )
            l_cHB_INSTALL_PREFIX := DirAddPathSep( hb_DirBase() ) + ".." + hb_osPathSeparator() + ".." + hb_osPathSeparator() + ".."
         OTHERWISE
            hbmk_OutErr( hbmk, I_( "Error: HB_INSTALL_PREFIX not set, failed to autodetect." ) )
            RETURN 3
         ENDCASE
      ENDIF
      /* Detect special *nix dir layout (/bin, /lib/harbour, /include/harbour) */
      IF hb_FileExists( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "include" +;
                                         hb_osPathSeparator() + iif( hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB, "xharbour", "harbour" ) +;
                                         hb_osPathSeparator() + "hbvm.h" )
         IF Empty( l_cHB_BIN_INSTALL )
            l_cHB_BIN_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "bin" )
         ENDIF
         IF Empty( l_cHB_LIB_INSTALL )
            l_cHB_LIB_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "lib" + hb_osPathSeparator() + iif( hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB, "xharbour", "harbour" ) )
         ENDIF
         IF Empty( l_cHB_INC_INSTALL )
            l_cHB_INC_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "include" + hb_osPathSeparator() + iif( hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB, "xharbour", "harbour" ) )
         ENDIF
      ENDIF
   ELSE
      lSysLoc := .F.

      l_cHB_BIN_INSTALL := ""
      l_cHB_LIB_INSTALL := ""
      l_cHB_INC_INSTALL := ""
      l_cHB_INSTALL_PREFIX := ""
   ENDIF

   aCOMPDET_EMBED := {}

   IF hbmk[ _HBMK_cARCH ] $ "win|wce|dos"

      #if defined( __PLATFORM__WINDOWS )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingw"    + hb_osPathSeparator() + "bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "win", "mingw"   , ""                    , NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingw64"  + hb_osPathSeparator() + "bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "win", "mingw64" , "x86_64-w64-mingw32-" , NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingwarm" + hb_osPathSeparator() + "bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "wce", "mingwarm", "arm-mingw32ce-"      , NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "mingwarm" + hb_osPathSeparator() + "bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "wce", "mingwarm", "arm-wince-mingw32ce-", NIL } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "djgpp"    + hb_osPathSeparator() + "bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe"            ), tmp1, NIL ) }, "dos", "djgpp"   , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_osPathSeparator() + "binnt" ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "wpp386.exe"         ), tmp1, NIL ) }, "win", "watcom"  , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_osPathSeparator() + "binw"  ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "wpp386.exe"         ), tmp1, NIL ) }, "dos", "watcom"  , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "pocc"     + hb_osPathSeparator() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "pocc.exe"           ), tmp1, NIL ) }, "win", "pocc"    , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "pocc"     + hb_osPathSeparator() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "pocc.exe"           ), tmp1, NIL ) }, "win", "pocc64"  , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "pocc"     + hb_osPathSeparator() + "Bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "pocc.exe"           ), tmp1, NIL ) }, "wce", "poccarm" , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )

      #elif defined( __PLATFORM__DOS )

         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "djgpp"    + hb_osPathSeparator() + "bin"   ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc.exe"            ), tmp1, NIL ) }, "dos", "djgpp"   , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )
         AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + _COMPEMBED_BASE_ + "watcom"   + hb_osPathSeparator() + "binw"  ), iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "wpp386.exe"         ), tmp1, NIL ) }, "dos", "watcom"  , ""                    , {| cARCH, cCOMP, cPathBin | hbmk_COMP_Setup( cARCH, cCOMP, cPathBin + hb_osPathSeparator() + ".." ) } } )

      #elif defined( __PLATFORM__UNIX )

         IF Empty( hbmk[ _HBMK_cCCPATH ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCPREFIX ] ) .AND. ;
            Empty( hbmk[ _HBMK_cCCPOSTFIX ] ) .AND. ;
            !( hbmk[ _HBMK_cARCH ] == "dos" )

            DO CASE
            CASE hbmk[ _HBMK_cCOMP ] $ "mingw"
               AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := "/opt/xmingw/bin"   , iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "win", "mingw"   , "i386-mingw-", NIL } )
            CASE hbmk[ _HBMK_cCOMP ] $ "mingwarm"
               AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := "/opt/mingw32ce/bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "wce", "mingwarm", "arm-mingw32ce-", NIL } )
               AAdd( aCOMPDET_EMBED, { {| cPrefix | tmp1 := "/opt/mingw32ce/bin", iif( hb_FileExists( tmp1 + hb_osPathSeparator() + cPrefix + "gcc" + cCCEXT_mingw ), tmp1, NIL ) }, "wce", "mingwarm", "arm-wince-mingw32ce-", NIL } )
            ENDCASE
         ENDIF

      #endif
   ENDIF

   /* Autodetect compiler */

   cPath_CompC := NIL

   IF lStopAfterHarbour
      /* If we're just compiling .prg to .c we don't need a C compiler. */
      hbmk[ _HBMK_cCOMP ] := ""
   ELSE
      IF Empty( hbmk[ _HBMK_cCOMP ] ) .OR. hbmk[ _HBMK_cCOMP ] == "bld"
         IF Len( aCOMPSUP ) == 1
            hbmk[ _HBMK_cCOMP ] := aCOMPSUP[ 1 ]
         ELSEIF hbmk[ _HBMK_cARCH ] == "linux" .OR. hbmk[ _HBMK_cCOMP ] == "bld"
            hbmk[ _HBMK_cCOMP ] := hb_Version( HB_VERSION_BUILD_COMP )
            IF AScan( aCOMPSUP, {|tmp| tmp == hbmk[ _HBMK_cCOMP ] } ) == 0
               hbmk[ _HBMK_cCOMP ] := NIL
            ENDIF
         ELSE
            IF Empty( hbmk[ _HBMK_cCOMP ] ) .AND. ! Empty( aCOMPDET )
               /* Check compilers */
               FOR tmp := 1 TO Len( aCOMPDET )
                  IF ! Empty( cPath_CompC := Eval( aCOMPDET[ tmp ][ _COMPDET_bBlock ] ) )
                     hbmk[ _HBMK_cCOMP ] := aCOMPDET[ tmp ][ _COMPDET_cCOMP ]
                     IF Len( aCOMPDET[ tmp ] ) >= _COMPDET_cCCPREFIX
                        hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET[ tmp ][ _COMPDET_cCCPREFIX ]
                     ENDIF
                     EXIT
                  ENDIF
               NEXT
            ENDIF
            IF Empty( hbmk[ _HBMK_cCOMP ] ) .AND. hbmk[ _HBMK_cARCH ] $ "win|wce|dos"
               /* Autodetect embedded MinGW installation */
               FOR tmp := 1 TO Len( aCOMPDET_EMBED )
                  IF hbmk[ _HBMK_cARCH ] == aCOMPDET_EMBED[ tmp ][ _COMPDETE_cARCH ] .AND. ;
                     ! Empty( cPath_CompC := Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bBlock ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ] ) )
                     hbmk[ _HBMK_cCOMP ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCOMP ]
                     hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ]
                     hbmk[ _HBMK_cCCPATH ] := cPath_CompC
                     IF ISBLOCK( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ] )
                        Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ], hbmk[ _HBMK_cARCH ], hbmk[ _HBMK_cCOMP ], cPath_CompC )
                     ENDIF
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF
         IF ! Empty( hbmk[ _HBMK_cCOMP ] )
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected C compiler: %1$s" ), hbmk[ _HBMK_cCOMP ] ) )
            ENDIF
         ELSE
            IF Empty( aCOMPDET )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Please choose a C compiler by using -comp= option or envvar HB_COMPILER.\nYou have the following choices on your platform: %1$s" ), ArrayToList( aCOMPSUP, ", " ) ) )
            ELSE
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Couldn't detect any supported C compiler in your PATH.\nPlease setup one or set -comp= option or envvar HB_COMPILER to one of these values: %1$s" ), ArrayToList( aCOMPSUP, ", " ) ) )
            ENDIF
            RETURN 2
         ENDIF
      ELSE
         IF AScan( aCOMPSUP, {|tmp| tmp == hbmk[ _HBMK_cCOMP ] } ) == 0
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Compiler value unknown: %1$s" ), hbmk[ _HBMK_cCOMP ] ) )
            RETURN 2
         ENDIF
         IF hbmk[ _HBMK_cARCH ] $ "win|wce|dos"
            /* Detect cross platform CCPREFIX and CCPATH if embedded MinGW installation is detected */
            FOR tmp := 1 TO Len( aCOMPDET_EMBED )
               IF aCOMPDET_EMBED[ tmp ][ _COMPDETE_cARCH ] == hbmk[ _HBMK_cARCH ] .AND. ;
                  aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCOMP ] == hbmk[ _HBMK_cCOMP ] .AND. ;
                  ! Empty( cPath_CompC := Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bBlock ], aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ] ) )
                  hbmk[ _HBMK_cCCPATH ] := cPath_CompC
                  hbmk[ _HBMK_cCCPREFIX ] := aCOMPDET_EMBED[ tmp ][ _COMPDETE_cCCPREFIX ]
                  IF ISBLOCK( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ] )
                     Eval( aCOMPDET_EMBED[ tmp ][ _COMPDETE_bSetup ], hbmk[ _HBMK_cARCH ], hbmk[ _HBMK_cCOMP ], cPath_CompC )
                  ENDIF
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   hbmk[ _HBMK_aINCPATH ] := {}
   hbmk[ _HBMK_aLIBPATH ] := {}

   /* Tweaks to compiler environments */

   DO CASE
   CASE hbmk[ _HBMK_cCOMP ] == "bcc"
      /* NOTE: Hack to tweak bcc setup by hbmk2 to include one additional
               compiler lib dir to lib search path. */
      IF Empty( cPath_CompC )
         cPath_CompC := _BCC_BIN_DETECT()
      ENDIF
      IF ! Empty( cPath_CompC )
         /* NOTE: Automatically configure bcc installation with missing configuration. [vszakats] */
         IF ! hb_FileExists( FN_DirGet( cPath_CompC ) + ".." + hb_osPathSeparator() + "Bin" + hb_osPathSeparator() + "bcc32.cfg" ) .OR. ;
            ! hb_FileExists( FN_DirGet( cPath_CompC ) + ".." + hb_osPathSeparator() + "Bin" + hb_osPathSeparator() + "ilink32.cfg" )
            AAdd( hbmk[ _HBMK_aINCPATH ], PathNormalize( FN_DirGet( cPath_CompC ) + ".." + hb_osPathSeparator() + "Include" ) )
            AAdd( hbmk[ _HBMK_aLIBPATH ], PathNormalize( FN_DirGet( cPath_CompC ) + ".." + hb_osPathSeparator() + "Lib" ) )
         ENDIF
         AAdd( hbmk[ _HBMK_aLIBPATH ], PathNormalize( FN_DirGet( cPath_CompC ) + ".." + hb_osPathSeparator() + "Lib" + hb_osPathSeparator() + "PSDK" ) )
      ENDIF
   ENDCASE

   /* Finish detecting bin/lib/include dirs */

   IF Empty( l_cHB_BIN_INSTALL )
      l_cHB_BIN_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "bin" )
   ENDIF
   IF Empty( l_cHB_LIB_INSTALL )
      /* Autodetect multi-compiler/platform lib structure */
      IF hb_DirExists( tmp := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) ) + "lib" +;
                                               hb_osPathSeparator() + hbmk[ _HBMK_cARCH ] +;
                                               hb_osPathSeparator() + hbmk[ _HBMK_cCOMP ] )
         l_cHB_DYN_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "lib" )
         l_cHB_LIB_INSTALL := tmp
      ELSE
         l_cHB_LIB_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "lib" )
      ENDIF
   ENDIF
   IF Empty( l_cHB_INC_INSTALL )
      l_cHB_INC_INSTALL := PathNormalize( DirAddPathSep( l_cHB_INSTALL_PREFIX ) + "include" )
   ENDIF

   DEFAULT l_cHB_DYN_INSTALL TO l_cHB_LIB_INSTALL

   IF hbmk[ _HBMK_lInfo ]
      hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using Harbour: %1$s %2$s %3$s %4$s" ), l_cHB_BIN_INSTALL, l_cHB_INC_INSTALL, l_cHB_LIB_INSTALL, l_cHB_DYN_INSTALL ) )
      IF ! Empty( cPath_CompC )
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Using C compiler: %1$s" ), cPath_CompC ) )
      ENDIF
   ENDIF

   /* Make a copy to hbmk structure so that we can use it in deeper
      functions. The only reason I kept the local version is to
      keep above code parts easier to read. [vszakats] */
   hbmk[ _HBMK_cHB_BIN_INSTALL ] := l_cHB_BIN_INSTALL := PathSepToTarget( hbmk, l_cHB_BIN_INSTALL )
   hbmk[ _HBMK_cHB_LIB_INSTALL ] := l_cHB_LIB_INSTALL := PathSepToTarget( hbmk, l_cHB_LIB_INSTALL )
   hbmk[ _HBMK_cHB_DYN_INSTALL ] := l_cHB_DYN_INSTALL := PathSepToTarget( hbmk, l_cHB_DYN_INSTALL )
   hbmk[ _HBMK_cHB_INC_INSTALL ] := l_cHB_INC_INSTALL := PathSepToTarget( hbmk, l_cHB_INC_INSTALL )

   /* Add main Harbour library dir to lib path list */
   AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], l_cHB_LIB_INSTALL )
   IF ! Empty( l_cHB_DYN_INSTALL ) .AND. !( l_cHB_DYN_INSTALL == l_cHB_LIB_INSTALL )
      AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], l_cHB_DYN_INSTALL )
   ENDIF

   /* Add main Harbour header dir to header path list */
   AAddNotEmpty( hbmk[ _HBMK_aINCPATH ], l_cHB_INC_INSTALL )

   /* Build with shared libs by default, if we're installed to default system locations. */

   IF lSysLoc .AND. ( hbmk[ _HBMK_cARCH ] $ "bsd|hpux|sunos|linux" .OR. hbmk[ _HBMK_cARCH ] == "darwin" )
      hbmk[ _HBMK_lSHARED ] := .T.
      hbmk[ _HBMK_lSTATICFULL ] := .F.
   ELSE
      hbmk[ _HBMK_lSHARED ] := .F.
      hbmk[ _HBMK_lSTATICFULL ] := .F.
   ENDIF

   /* Process environment */

   IF    Lower( GetEnv( "HB_MT"     ) ) == "mt" ; hbmk[ _HBMK_lMT ]     := .T. ; ENDIF /* Compatibility */
   IF ValueIsT( GetEnv( "HB_MT"     ) )         ; hbmk[ _HBMK_lMT ]     := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_GUI"    ) )         ; hbmk[ _HBMK_lGUI ]    := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_SHARED" ) )         ; hbmk[ _HBMK_lSHARED ] := .T. ; hbmk[ _HBMK_lSTATICFULL ] := .F. ; hbmk[ _HBMK_lSHAREDDIST ] := NIL ; ENDIF
   IF ValueIsT( GetEnv( "HB_DEBUG"  ) )         ; hbmk[ _HBMK_lDEBUG ]  := .T. ; ENDIF
   IF ValueIsT( GetEnv( "HB_NULRDD" ) )         ; hbmk[ _HBMK_lNULRDD ] := .T. ; ENDIF

   IF Lower( Left( GetEnv( "HB_GT" ), 2 ) ) == "gt"
      SetupForGT( GetEnv( "HB_GT" ), @hbmk[ _HBMK_cGT ], @hbmk[ _HBMK_lGUI ] )
   ENDIF

   FOR EACH tmp IN ListToArray( PathSepToTarget( hbmk, GetEnv( "HB_USER_LIBPATHS" ) ) )
      AAddNotEmpty( hbmk[ _HBMK_aLIBPATH ], tmp )
   NEXT

   /* Process command line */

   hbmk[ _HBMK_aPRG ] := {}
   hbmk[ _HBMK_aC ] := {}
   hbmk[ _HBMK_aOPTPRG ] := {}
   hbmk[ _HBMK_aOPTC ] := {}
   hbmk[ _HBMK_aOPTRES ] := {}
   hbmk[ _HBMK_aOPTL ] := {}
   hbmk[ _HBMK_aOPTA ] := {}
   hbmk[ _HBMK_aOPTD ] := {}
   l_aOPTRUN := {}
   hbmk[ _HBMK_aRESSRC ] := {}
   hbmk[ _HBMK_aRESCMP ] := {}
   hbmk[ _HBMK_aINCTRYPATH ] := {}
   hbmk[ _HBMK_aLIBUSER ] := {}
   hbmk[ _HBMK_aLIBUSERGT ] := {}
   hbmk[ _HBMK_aLIBDYNHAS ] := {}
   hbmk[ _HBMK_aOBJUSER ] := {}
   l_aOBJA := {}
   l_cPROGDIR := NIL
   l_cPROGNAME := NIL
   hbmk[ _HBMK_cFIRST ] := NIL
   hbmk[ _HBMK_aPO ] := {}
   hbmk[ _HBMK_cHBL ] := NIL
   hbmk[ _HBMK_cHBLDir ] := ""
   hbmk[ _HBMK_cPO ] := NIL
   hbmk[ _HBMK_aLNG ] := {}
   hbmk[ _HBMK_aINSTPATH ] := {}
   hbmk[ _HBMK_lUNICODE ] := ( hbmk[ _HBMK_cARCH ] == "wce" )

   aParams := {}

   /* Process automatic make files in current dir. */
   IF hb_FileExists( _HBMK_AUTOHBM_NAME )
      IF ! hbmk[ _HBMK_lQuiet ]
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing local make script: %1$s" ), _HBMK_AUTOHBM_NAME ) )
      ENDIF
      HBM_Load( hbmk, aParams, _HBMK_AUTOHBM_NAME, 1 )
   ENDIF

   /* Collect all command line parameters */
   FOR EACH cParam IN aArgs
      DO CASE
      CASE ( Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" )
         cParam := SubStr( cParam, 2 )
         IF Empty( FN_ExtGet( cParam ) )
            cParam := FN_ExtSet( cParam, ".hbm" )
         ENDIF
         IF !( Lower( FN_ExtGet( cParam ) ) == ".hbm" ) .AND. lAcceptLDClipper
            rtlnk_process( hbmk, MemoRead( cParam ), @l_cPROGNAME, @hbmk[ _HBMK_aOBJUSER ], @hbmk[ _HBMK_aLIBUSER ] )
            IF ! Empty( hbmk[ _HBMK_aOBJUSER ] )
               DEFAULT hbmk[ _HBMK_cFIRST ] TO hbmk[ _HBMK_aOBJUSER ][ 1 ]
            ENDIF
         ELSE
            HBM_Load( hbmk, aParams, cParam, 1 ) /* Load parameters from script file */
         ENDIF
      CASE Lower( FN_ExtGet( cParam ) ) == ".hbm" .OR. ;
           Lower( FN_ExtGet( cParam ) ) == ".hbp"
         HBM_Load( hbmk, aParams, cParam, 1 ) /* Load parameters from script file */
      OTHERWISE
         AAdd( aParams, { cParam, "", 0 } )
      ENDCASE
   NEXT

   /* Process command line (1st pass) */
   lNOHBC := .F.
   FOR EACH aParam IN aParams
      IF Lower( aParam[ _PAR_cParam ] ) == "-nohbc"
         lNOHBC := .T.
      ENDIF
   NEXT

   /* Process automatic control files. */
   HBC_ProcessAll( hbmk, lNOHBC )

   /* Process command line (2nd pass) */
   FOR EACH aParam IN aParams

      cParam := aParam[ _PAR_cParam ]
      cParamL := Lower( cParam )

      DO CASE
      CASE Left( cParamL, 6 ) == "-comp=" .OR. ;
           Left( cParamL, 6 ) == "-arch=" .OR. ;
           cParamL            == "-hbrun" .OR. ;
           cParamL            == "-hbraw" .OR. ;
           cParamL            == "-hbcmp" .OR. ;
           cParamL            == "-hbcc"  .OR. ;
           cParamL            == "-hblnk" .OR. ;
           cParamL            == "-nohbc" .OR. ;
           cParamL            == "-xhb" .OR. ;
           cParamL            == "-hb10" .OR. ;
           cParamL            == "-hbc" .OR. ;
           cParamL            == "-nohblib" .OR. ;
           cParamL            == "-clipper" .OR. ;
           cParamL            == "-rtlink" .OR. ;
           cParamL            == "-blinker" .OR. ;
           cParamL            == "-exospace"

         /* Simply ignore. They were already processed in the first pass. */

      CASE cParamL == "-quiet"           ; hbmk[ _HBMK_lQuiet ] := .T. ; hbmk[ _HBMK_lInfo ] := .F.
      CASE cParamL == "-info"            ; hbmk[ _HBMK_lInfo ] := .T.
      CASE cParamL == "-pause"           ; lPause := .T.
      CASE cParamL == "-hblib"           ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .T. ; lCreateDyn := .F.
      CASE cParamL == "-hbdyn"           ; hbmk[ _HBMK_lInfo ] := .F. ; lStopAfterHarbour := .F. ; lStopAfterCComp := .T. ; lCreateLib := .F. ; lCreateDyn := .T.
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
      CASE cParamL == "-bldf"            ; l_lBLDFLGP   := l_lBLDFLGC := l_lBLDFLGL := .T.
      CASE cParamL == "-bldf-"           ; l_lBLDFLGP   := l_lBLDFLGC := l_lBLDFLGL := .F.
      CASE Left( cParamL, 6 ) == "-bldf="
         cParam := SubStr( cParam, 7 )
         l_lBLDFLGP := "p" $ cParam
         l_lBLDFLGC := "c" $ cParam
         l_lBLDFLGL := "l" $ cParam
      CASE cParamL == "-debug"           ; hbmk[ _HBMK_lDEBUG ]     := .T.
      CASE cParamL == "-debug-" .OR. ;
           cParamL == "-nodebug"         ; hbmk[ _HBMK_lDEBUG ]     := .F.
      CASE cParamL == "-optim"           ; hbmk[ _HBMK_lOPTIM ]     := .T.
      CASE cParamL == "-optim-" .OR. ;
           cParamL == "-noopt"           ; hbmk[ _HBMK_lOPTIM ]     := .F.
      CASE cParamL == "-debugtime"       ; hbmk[ _HBMK_lDEBUGTIME ] := .T.
      CASE cParamL == "-debuginc"        ; hbmk[ _HBMK_lDEBUGINC ]  := .T.
      CASE cParamL == "-debugstub"       ; hbmk[ _HBMK_lDEBUGSTUB ] := .T.
      CASE cParamL == "-debugi18n"       ; hbmk[ _HBMK_lDEBUGI18N ] := .T.
      CASE cParamL == "-nulrdd"          ; hbmk[ _HBMK_lNULRDD ]    := .T.
      CASE cParamL == "-nulrdd-"         ; hbmk[ _HBMK_lNULRDD ]    := .F.
      CASE cParamL == "-map"             ; hbmk[ _HBMK_lMAP ]       := .T.
      CASE cParamL == "-map-" .OR. ;
           cParamL == "-nomap"           ; hbmk[ _HBMK_lMAP ]       := .F.
      CASE cParamL == "-beep"            ; hbmk[ _HBMK_lBEEP ]      := .T.
      CASE cParamL == "-beep-" .OR. ;
           cParamL == "-nobeep"          ; hbmk[ _HBMK_lBEEP ]      := .F.
      CASE cParamL == "-rebuild"         ; hbmk[ _HBMK_lINC ]       := .T. ; hbmk[ _HBMK_lREBUILD ] := .T.
      CASE cParamL == "-rebuildpo"       ; hbmk[ _HBMK_lREBUILDPO ] := .T.
      CASE cParamL == "-minipo"          ; hbmk[ _HBMK_lMINIPO ]    := .T.
      CASE cParamL == "-minipo-" .OR. ;
           cParamL == "-nominipo"        ; hbmk[ _HBMK_lMINIPO ]    := .F.
      CASE cParamL == "-clean"           ; hbmk[ _HBMK_lINC ]       := .T. ; l_lCLEAN := .T.
      CASE cParamL == "-inc"             ; hbmk[ _HBMK_lINC ]       := .T.
      CASE cParamL == "-inc-" .OR. ;
           cParamL == "-noinc"           ; hbmk[ _HBMK_lINC ]       := .F.
      CASE cParamL == "-strip"           ; hbmk[ _HBMK_lSTRIP ]     := .T.
      CASE cParamL == "-strip-" .OR. ;
           cParamL == "-nostrip"         ; hbmk[ _HBMK_lSTRIP ]     := .F.
      CASE cParamL == "-compr" .OR. ;
           Left( cParamL, 7 ) == "-compr="

           DO CASE
           CASE SubStr( cParamL, 8 ) == "min" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MIN
           CASE SubStr( cParamL, 8 ) == "max" ; hbmk[ _HBMK_nCOMPR ] := _COMPR_MAX
           OTHERWISE                          ; hbmk[ _HBMK_nCOMPR ] := _COMPR_DEF
           ENDCASE

      CASE cParamL == "-compr-" .OR. ;
           cParamL == "-nocompr"         ; hbmk[ _HBMK_nCOMPR ]     := _COMPR_OFF

      CASE cParamL == "-head" .OR. ;
           Left( cParamL, 6 ) == "-head="

           DO CASE
           CASE SubStr( cParamL, 7 ) == "off"  ; hbmk[ _HBMK_nHEAD ] := _HEAD_OFF
           CASE SubStr( cParamL, 7 ) == "full" ; hbmk[ _HBMK_nHEAD ] := _HEAD_FULL
           OTHERWISE                           ; hbmk[ _HBMK_nHEAD ] := _HEAD_PARTIAL
           ENDCASE

      CASE cParamL == "-head-" .OR. ;
           cParamL == "-nohead"          ; hbmk[ _HBMK_nHEAD ]      := _HEAD_OFF

      CASE Left( cParamL, 5 ) == "-cpp="

           DO CASE
           CASE SubStr( cParamL, 6 ) == "def" ; hbmk[ _HBMK_lCPP ] := NIL
           CASE SubStr( cParamL, 6 ) == "yes" ; hbmk[ _HBMK_lCPP ] := .T.
           CASE SubStr( cParamL, 6 ) == "no"  ; hbmk[ _HBMK_lCPP ] := .F.
           ENDCASE

      CASE cParamL == "-cpp"             ; hbmk[ _HBMK_lCPP ]       := .T.
      CASE cParamL == "-cpp-" .OR. ;
           cParamL == "-nocpp"           ; hbmk[ _HBMK_lCPP ]       := .F.

      CASE cParamL == "-run"             ; hbmk[ _HBMK_lRUN ]       := .T.
      CASE cParamL == "-run-" .OR. ;
           cParamL == "-norun"           ; hbmk[ _HBMK_lRUN ]       := .F.
      CASE cParamL == "-trace"           ; hbmk[ _HBMK_lTRACE ]     := .T.
      CASE cParamL == "-trace-" .OR. ;
           cParamL == "-notrace"         ; hbmk[ _HBMK_lTRACE ]     := .F.
      CASE cParamL == "-traceonly"       ; hbmk[ _HBMK_lTRACE ]     := .T. ; hbmk[ _HBMK_lDONTEXEC ] := .T.

      CASE cParamL == "--hbdirbin"       ; lStopAfterInit := .T.

         OutStd( l_cHB_BIN_INSTALL )

      CASE cParamL == "--hbdirdyn"       ; lStopAfterInit := .T.

         OutStd( l_cHB_DYN_INSTALL )

      CASE cParamL == "--hbdirlib"       ; lStopAfterInit := .T.

         OutStd( l_cHB_LIB_INSTALL )

      CASE cParamL == "--hbdirinc"       ; lStopAfterInit := .T.

         OutStd( l_cHB_INC_INSTALL )

      CASE Left( cParamL, Len( "-jobs=" ) ) == "-jobs="

         cParam := ArchCompFilter( hbmk, SubStr( cParam, Len( "-jobs=" ) + 1 ) )
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

         hbmk[ _HBMK_cHBL ] := PathSepToTarget( hbmk, SubStr( cParam, 6 ) )
         hbmk[ _HBMK_cHBLDir ] := FN_DirGet( aParam[ _PAR_cFileName ] )

      CASE Left( cParamL, 4 ) == "-po="

         hbmk[ _HBMK_cPO ] := PathSepToTarget( hbmk, PathProc( SubStr( cParam, 5 ), FN_DirGet( aParam[ _PAR_cFileName ] ) ) )

      CASE Left( cParamL, 5 ) == "-hbl"

         hbmk[ _HBMK_cHBL ] := ""
         hbmk[ _HBMK_cHBLDir ] := ""

      CASE Left( cParamL, 6 ) == "-main="

         IF IsValidHarbourID( cParam := SubStr( cParam, 7 ) )
            l_cMAIN := "@" + cParam
         ELSE
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -main value ignored: %1$s" ), cParam ) )
         ENDIF

      CASE Left( cParamL, 3 ) == "-gt"

         cParam := ArchCompFilter( hbmk, SubStr( cParam, 2 ) )
         IF hbmk[ _HBMK_cGT ] == NIL
            IF ! SetupForGT( cParam, @hbmk[ _HBMK_cGT ], @hbmk[ _HBMK_lGUI ] )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Invalid -gt value ignored: %1$s" ), cParam ) )
               cParam := NIL
            ENDIF
         ENDIF
         IF ! Empty( cParam ) .AND. !( Lower( cParam ) == "gtnul" )
            IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {|tmp| Lower( tmp ) == cParamL } ) == 0 .AND. ;
               AScan( hbmk[ _HBMK_aLIBUSERGT ], {|tmp| Lower( tmp ) == cParamL } ) == 0
               AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], PathSepToTarget( hbmk, cParam ) )
            ENDIF
         ENDIF

#if ! defined( __PLATFORM__UNIX )
      CASE Left( cParamL, 2 ) == "/o" .AND. ! lStopAfterHarbour

         /* Swallow this switch. We don't pass it to Harbour, as it may badly
            interact with hbmk. */
#endif

      CASE Left( cParam, 2 ) == "-o" .AND. ! lStopAfterHarbour

         tmp := PathProc( MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, 3 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] )
         IF ! Empty( tmp )
            tmp := PathSepToSelf( tmp )
            hb_FNameSplit( tmp, @cDir, @cName, @cExt )
            IF ! Empty( cDir ) .AND. Empty( cName ) .AND. Empty( cExt )
               /* Only a dir was passed, let's store that and pick a default name later. */
               l_cPROGDIR := cDir
            ELSEIF ! Empty( tmp )
               l_cPROGDIR := NIL
               l_cPROGNAME := tmp
            ELSE
               l_cPROGDIR := NIL
               l_cPROGNAME := NIL
            ENDIF
         ENDIF

      CASE Left( cParam, 2 ) == "-L" .AND. ;
           Len( cParam ) > 2

         cParam := PathProc( MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, 3 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToTarget( hbmk, cParam ) )
         ENDIF

      CASE Left( cParamL, Len( "-instpath=" ) ) == "-instpath=" .AND. ;
           Len( cParamL ) > Len( "-instpath=" )

         cParam := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, tmp := ArchCompFilter( hbmk, SubStr( cParam, Len( "-instpath=" ) + 1 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) ) )
         IF ! Empty( cParam )
            IF AScan( hbmk[ _HBMK_aINSTPATH ], {|tmp| tmp == cParam } ) == 0
               AAdd( hbmk[ _HBMK_aINSTPATH ], PathSepToTarget( hbmk, cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-incpath=" ) ) == "-incpath=" .AND. ;
           Len( cParamL ) > Len( "-incpath=" )

         cParam := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, tmp := ArchCompFilter( hbmk, SubStr( cParam, Len( "-incpath=" ) + 1 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) ) )
         IF ! Empty( cParam )
            IF AScan( hbmk[ _HBMK_aINCPATH ], {|tmp| tmp == cParam } ) == 0
               AAdd( hbmk[ _HBMK_aINCPATH ], PathSepToTarget( hbmk, cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-inctrypath=" ) ) == "-inctrypath=" .AND. ;
           Len( cParamL ) > Len( "-inctrypath=" )

         cParam := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, tmp := ArchCompFilter( hbmk, SubStr( cParam, Len( "-inctrypath=" ) + 1 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) ) )
         IF ! Empty( cParam )
            IF AScan( hbmk[ _HBMK_aINCTRYPATH ], {|tmp| tmp == cParam } ) == 0
               AAdd( hbmk[ _HBMK_aINCTRYPATH ], PathSepToTarget( hbmk, cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, 2 ) == "-i" .AND. ;
           Len( cParamL ) > 2

         cParam := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, tmp := ArchCompFilter( hbmk, SubStr( cParam, 3 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) ) )
         IF ! Empty( cParam )
            IF AScan( hbmk[ _HBMK_aINCPATH ], {|tmp| tmp == cParam } ) == 0
               AAdd( hbmk[ _HBMK_aINCPATH ], PathSepToTarget( hbmk, cParam ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-stop" ) ) == "-stop"

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            lStopAfterInit := .T.
            hbmk[ _HBMK_lRUN ] := .F.
         ENDIF

      CASE Left( cParamL, Len( "-echo=" ) ) == "-echo="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-echo=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            OutStd( hb_StrFormat( I_( "%1$s" ), cParam ) + hb_osNewLine() )
         ENDIF

      CASE Left( cParamL, Len( "-prgflag=" ) ) == "-prgflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-prgflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            IF SubStr( cParamL, 2 ) == "gh"
               lStopAfterHarbour := .T.
            ENDIF
            IF !( SubStr( cParamL, 2, 1 ) == "o" )
               AAdd( hbmk[ _HBMK_aOPTPRG ] , PathSepToTarget( hbmk, cParam, 2 ) )
            ENDIF
         ENDIF

      CASE Left( cParamL, Len( "-cflag=" ) ) == "-cflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-cflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTC ]   , PathSepToTarget( hbmk, cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-resflag=" ) ) == "-resflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-resflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTRES ] , PathSepToTarget( hbmk, cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-ldflag=" ) ) == "-ldflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-ldflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTL ]   , PathSepToTarget( hbmk, cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-dflag=" ) ) == "-dflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-dflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTD ]   , PathSepToTarget( hbmk, cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-aflag=" ) ) == "-aflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-aflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF Left( cParam, 1 ) $ cOptPrefix
            AAdd( hbmk[ _HBMK_aOPTA ]   , PathSepToTarget( hbmk, cParam, 2 ) )
         ENDIF

      CASE Left( cParamL, Len( "-runflag=" ) ) == "-runflag="

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-runflag=" ) + 1 ) ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( l_aOPTRUN, cParam )
         ENDIF

      CASE Left( cParamL, Len( "-workdir=" ) ) == "-workdir="

         cWorkDir := PathSepToTarget( hbmk, PathProc( MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-workdir=" ) + 1 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) )

      CASE Left( cParamL, Len( "-vcshead=" ) ) == "-vcshead="

         l_cVCSDIR := FN_DirGet( aParam[ _PAR_cFileName ] )
         l_cVCSHEAD := PathSepToTarget( hbmk, PathProc( MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-vcshead=" ) + 1 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) )
         IF Empty( FN_ExtGet( l_cVCSHEAD ) )
            l_cVCSHEAD := FN_ExtSet( l_cVCSHEAD, ".ch" )
         ENDIF

      CASE Left( cParamL, Len( "-tshead=" ) ) == "-tshead="

         l_cTSHEAD := PathSepToTarget( hbmk, PathProc( MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, Len( "-tshead=" ) + 1 ) ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] ) )
         IF Empty( FN_ExtGet( l_cTSHEAD ) )
            l_cTSHEAD := FN_ExtSet( l_cTSHEAD, ".ch" )
         ENDIF

      CASE Left( cParam, 2 ) == "-l" .AND. ;
           Len( cParam ) > 2 .AND. ;
           !( Left( cParam, 3 ) == "-l-" )

         cParam := MacroProc( hbmk, ArchCompFilter( hbmk, SubStr( cParam, 3 ) ), aParam[ _PAR_cFileName ] )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aLIBUSER ], PathSepToTarget( hbmk, cParam ) )
         ENDIF

      CASE Left( cParam, 1 ) $ cOptPrefix

         DO CASE
         CASE lAcceptLDFlag
            AAddNotEmpty( hbmk[ _HBMK_aOPTL ], ArchCompFilter( hbmk, PathSepToTarget( hbmk, cParam, 2 ) ) )
         CASE lAcceptCFlag
            IF SubStr( cParamL, 2 ) == "c"
               lStopAfterCComp := .T.
            ELSE
               AAddNotEmpty( hbmk[ _HBMK_aOPTC ], ArchCompFilter( hbmk, PathSepToTarget( hbmk, cParam, 2 ) ) )
            ENDIF
         OTHERWISE
            IF SubStr( cParamL, 2 ) == "gh"
               lStopAfterHarbour := .T.
            ENDIF
            AAddNotEmpty( hbmk[ _HBMK_aOPTPRG ], PathSepToTarget( hbmk, cParam, 2 ) )
         ENDCASE

      CASE FN_ExtGet( cParamL ) == ".lib" .OR. ;
           ( ! Empty( cDynLibExt ) .AND. FN_ExtGet( cParamL ) == cDynLibExt )

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            AAdd( hbmk[ _HBMK_aLIBUSER ], PathSepToTarget( hbmk, cParam ) )
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".hbc"

         cParam := PathProc( MacroProc( hbmk, ArchCompFilter( hbmk, cParam ), aParam[ _PAR_cFileName ] ), aParam[ _PAR_cFileName ] )

         IF ! Empty( cParam )
            IF ! hb_FileExists( cParam )
               FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                  IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cParam, .T. ) ) ) + FN_NameExtGet( cParam ) )
                     cParam := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cParam, .T. ) ) ) + FN_NameExtGet( cParam )
                     EXIT
                  ENDIF
               NEXT
            ENDIF

            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cParam ) )
            ENDIF

            HBC_ProcessOne( hbmk, cParam, 1 )
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".prg"

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aPRG ], PathSepToTarget( hbmk, cParam ) )
               DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cParam )
            NEXT
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".rc"

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aRESSRC ], PathSepToTarget( hbmk, cParam ) )
            NEXT
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".res"

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            IF hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm"
               /* For MinGW family add .res files as source input, as they
                  will need to be converted to coff format with windres (just
                  like plain .rc files) before feeding them to gcc. */
               FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
                  AAdd( hbmk[ _HBMK_aRESSRC ], PathSepToTarget( hbmk, cParam ) )
               NEXT
            ELSE
               FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
                  AAdd( hbmk[ _HBMK_aRESCMP ], PathSepToTarget( hbmk, cParam ) )
               NEXT
            ENDIF
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".a"

         cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
         AAdd( l_aOBJA, PathSepToTarget( hbmk, cParam ) )

      CASE FN_ExtGet( cParamL ) == ".o" .OR. ;
           FN_ExtGet( cParamL ) == ".obj"

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aOBJUSER ], PathSepToTarget( hbmk, cParam ) )
               DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cParam )
            NEXT
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".c" .OR. ;
           FN_ExtGet( cParamL ) == ".cpp" /* .cc, .cxx, .cx */

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
               AAdd( hbmk[ _HBMK_aC ], PathSepToTarget( hbmk, cParam ) )
               DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cParam )
            NEXT
         ENDIF

      CASE FN_ExtGet( cParamL ) == ".po" .OR. ;
           FN_ExtGet( cParamL ) == ".pot"

         FOR EACH cParam IN FN_Expand( PathProc( cParam, aParam[ _PAR_cFileName ] ), Empty( aParam[ _PAR_cFileName ] ) )
            AAdd( hbmk[ _HBMK_aPO ], PathSepToTarget( hbmk, cParam ) )
         NEXT

      CASE FN_ExtGet( cParamL ) == ".hbl"

         hbmk[ _HBMK_cHBL ] := PathSepToTarget( hbmk, cParam )
         hbmk[ _HBMK_cHBLDir ] := FN_DirGet( aParam[ _PAR_cFileName ] )

      OTHERWISE

         cParam := ArchCompFilter( hbmk, cParam )
         IF ! Empty( cParam )
            cParam := PathProc( cParam, aParam[ _PAR_cFileName ] )
            AAdd( hbmk[ _HBMK_aPRG ], PathSepToTarget( hbmk, cParam ) )
            DEFAULT hbmk[ _HBMK_cFIRST ] TO PathSepToSelf( cParam )
         ENDIF

      ENDCASE
   NEXT

   IF lCreateDyn .AND. hbmk[ _HBMK_lSHARED ]
      hbmk[ _HBMK_lSHARED ] := .F.
   ENDIF

   /* Force MT mode off in 1.0.x and xhb/dos compatibility modes. */
   IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
      ( hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB .AND. hbmk[ _HBMK_cARCH ] == "dos" )
      hbmk[ _HBMK_lMT ] := .F.
   ENDIF

   /* Start doing the make process. */
   IF ! lStopAfterInit .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) + Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ) ) == 0
      hbmk_OutErr( hbmk, I_( "Error: No source files were specified." ) )
      IF hbmk[ _HBMK_lBEEP ]
         DoBeep( hbmk, .F. )
      ENDIF
      RETURN 4
   ENDIF

   /* Decide about output name */
   IF ! lStopAfterInit

      /* If -o with full name wasn't specified, let's
         make it the first source file specified. */
      DEFAULT l_cPROGNAME TO FN_NameGet( hbmk[ _HBMK_cFIRST ] )

      /* Combine output dir with output name. */
      IF ! Empty( l_cPROGDIR )
         hb_FNameSplit( l_cPROGNAME, @cDir, @cName, @cExt )
         l_cPROGNAME := hb_FNameMerge( iif( Empty( cDir ), l_cPROGDIR, cDir ), cName, cExt )
      ENDIF
   ENDIF

   /* Decide about working dir */
   IF ! lStopAfterInit
      IF hbmk[ _HBMK_lINC ]
         /* NOTE: We store -hbdyn objects in different dirs by default as - for Windows
                  platforms - they're always built using different compilation options
                  than normal targets. [vszakats] */
         /* NOTE: We only use different shared object flags when compiling for
                  "( win || wce ) & !( allmingw | cygwin )". This may change in the future.
                  IMPORTANT: Keep this condition in sync with setting -DHB_DYNLIB C compiler flag */
         IF lCreateDyn .AND. hbmk[ _HBMK_cARCH ] $ "win|wce" .AND. !( hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm|cygwin" )
            DEFAULT cWorkDir TO FN_DirGet( l_cPROGNAME ) + _WORKDIR_DEF_ + hb_osPathSeparator() + "hbdyn"
         ELSE
            DEFAULT cWorkDir TO FN_DirGet( l_cPROGNAME ) + _WORKDIR_DEF_
         ENDIF
         IF ! Empty( cWorkDir )
            IF ! DirBuild( cWorkDir )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Working directory cannot be created: %1$s" ), cWorkDir ) )
               IF hbmk[ _HBMK_lBEEP ]
                  DoBeep( hbmk, .F. )
               ENDIF
               RETURN 9
            ENDIF
         ENDIF
      ELSE
         cWorkDir := ""
      ENDIF
   ENDIF

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour

      IF hbmk[ _HBMK_cGT ] == hbmk[ _HBMK_cGTDEFAULT ]
         hbmk[ _HBMK_cGT ] := NIL
      ENDIF

      /* Merge user libs from command line and envvar. Command line has priority. */
      hbmk[ _HBMK_aLIBUSER ] := ArrayAJoin( { hbmk[ _HBMK_aLIBUSER ], hbmk[ _HBMK_aLIBUSERGT ], ListToArray( PathSepToTarget( hbmk, GetEnv( "HB_USER_LIBS" ) ) ) } )

      DEFAULT hbmk[ _HBMK_lSHAREDDIST ] TO lSysLoc

      IF hbmk[ _HBMK_lSHAREDDIST ]
         cPrefix := ""
      ELSE
         cPrefix := DirAddPathSep( l_cHB_DYN_INSTALL )
      ENDIF
#if 1
      cPostfix := ""
      HB_SYMBOL_UNUSED( cDL_Version )
#else
      cPostfix := cDL_Version
#endif

      DO CASE
      CASE hbmk[ _HBMK_cARCH ] $ "bsd|linux|hpux|sunos" .OR. hbmk[ _HBMK_cARCH ] == "darwin" /* Separated to avoid match with 'win' */
         IF Empty( cPrefix )
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cPostfix,;
                                                      "harbour"   + cPostfix ) }
         ELSE
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], cPrefix + cDynLibNamePrefix + "harbourmt" + cPostfix + cDynLibExt,;
                                                      cPrefix + cDynLibNamePrefix + "harbour"   + cPostfix + cDynLibExt ) }
         ENDIF
      CASE hbmk[ _HBMK_cARCH ] $ "os2|win|wce"
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], cDynLibNamePrefix + "harbourmt",;
                                                   cDynLibNamePrefix + "harbour" ) }
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
         {DB}     dir for binaries
         {DI}     dir for includes
         {DL}     dirs for libs
         {SCRIPT} save command line to script and pass it to command as @<filename>
      */

      /* Assemble library list */

      IF l_lNOHBLIB
         hbmk[ _HBMK_aLIBCOREGT ] := {}
      ENDIF

      l_aLIBVM := iif( hbmk[ _HBMK_lMT ], aLIB_BASE_MT, aLIB_BASE_ST )
      aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, hbmk[ _HBMK_aLIBCOREGT ] } )

      IF ! Empty( hbmk[ _HBMK_cGT ] ) .AND. !( Lower( hbmk[ _HBMK_cGT ] ) == "gtnul" )
         IF AScan( aLIB_BASE2, {|tmp| Lower( tmp ) == Lower( hbmk[ _HBMK_cGT ] ) } ) == 0
            AAdd( aLIB_BASE2, hbmk[ _HBMK_cGT ] )
         ENDIF
      ENDIF

      IF hbmk[ _HBMK_cCOMP ] == "watcom" .AND. hbmk[ _HBMK_lCPP ] == NIL
         hbmk[ _HBMK_lCPP ] := .T.
      ENDIF

      DO CASE
      /* GCC family */
      CASE ( hbmk[ _HBMK_cARCH ] == "bsd"    .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "darwin" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "hpux"   .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "sunos"  .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "linux"  .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "linux"  .AND. hbmk[ _HBMK_cCOMP ] == "icc" )

         nCmd_Esc := _ESC_NIX
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         IF hbmk[ _HBMK_cARCH ] == "darwin"
            cBin_Lib := "libtool"
            cOpt_Lib := "-static {FA} -o {OL} {LO}"
         ELSE
            IF hbmk[ _HBMK_cCOMP ] == "icc"
               cBin_Lib := "xiar"
            ELSE
               cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar"
            ENDIF
            cOpt_Lib := "{FA} rcs {OL} {LO}"
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] == "icc"
            cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], "icpc", "icc" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_GNU_SOURCE" )
         ELSE
            cBin_CompC := hbmk[ _HBMK_cCCPREFIX ] + iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], "g++", "gcc" ) + hbmk[ _HBMK_cCCPOSTFIX ]
         ENDIF
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         cOpt_CompC += " {FC}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
            cBin_Lib   := hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_Lib
            cBin_CompC := hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_CompC
            cBin_Link  := hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_Link
         ENDIF
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         IF ! lStopAfterCComp
            IF hbmk[ _HBMK_cARCH ] == "linux" .OR. ;
               hbmk[ _HBMK_cARCH ] == "bsd"
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, l_aLIBVM } )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            IF hbmk[ _HBMK_cARCH ] == "darwin"
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-map,{OM}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSTATICFULL ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-static" )
         ENDIF
         IF hbmk[ _HBMK_cARCH ] == "darwin"
            AAdd( hbmk[ _HBMK_aOPTC ], "-no-cpp-precomp" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-Wno-long-double" )
            IF hbmk[ _HBMK_lSHARED ]
               AAdd( hbmk[ _HBMK_aOPTL ], "-bind_as_load" )
               AAdd( hbmk[ _HBMK_aOPTL ], "-multiply_defined suppress" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            IF hbmk[ _HBMK_cARCH ] == "darwin"
               cBin_Post := "strip"
               cOpt_Post := "{OB}"
            ELSEIF !( hbmk[ _HBMK_cARCH ] == "sunos" )
               AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
            ENDIF
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               IF hbmk[ _HBMK_cARCH ] == "darwin"
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o {OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
               ENDIF
            ENDIF
         ELSE
            IF hbmk[ _HBMK_cARCH ] == "darwin"
               AAdd( hbmk[ _HBMK_aOPTL ], "-o {OE}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
            ENDIF
         ENDIF

         /* Always inherit/reproduce some flags from self */

         IF     "-mlp64" $ cSelfFlagC ; AAddNew( hbmk[ _HBMK_aOPTC ], "-mlp64" )
         ELSEIF "-mlp32" $ cSelfFlagC ; AAddNew( hbmk[ _HBMK_aOPTC ], "-mlp32" )
         ELSEIF "-m64"   $ cSelfFlagC ; AAddNew( hbmk[ _HBMK_aOPTC ], "-m64" )
         ELSEIF "-m32"   $ cSelfFlagC ; AAddNew( hbmk[ _HBMK_aOPTC ], "-m32" )
         ENDIF

         IF     "-fPIC"  $ cSelfFlagC ; AAddNew( hbmk[ _HBMK_aOPTC ], "-fPIC" )
         ELSEIF "-fpic"  $ cSelfFlagC ; AAddNew( hbmk[ _HBMK_aOPTC ], "-fpic" )
         ENDIF

         DO CASE
         CASE "-DHB_PCRE_REGEX" $ cSelfFlagC
            AAdd( l_aLIBSYS, "pcre" )
            l_lHB_PCRE := .F.
         CASE "-DHB_POSIX_REGEX" $ cSelfFlagC
            l_lHB_PCRE := .F.
         ENDCASE
         IF "-DHB_EXT_ZLIB" $ cSelfFlagC
            AAdd( l_aLIBSYS, "z" )
            l_lHB_ZLIB := .F.
         ENDIF
         IF "-DHAVE_GPM_H" $ cSelfFlagC
            AAdd( l_aLIBSYS, "gpm" )
         ENDIF

         /* Add system libraries */
         IF ! hbmk[ _HBMK_lSHARED ]
            AAdd( l_aLIBSYS, "m" )
            IF hbmk[ _HBMK_lMT ]
               AAdd( l_aLIBSYS, "pthread" )
            ENDIF
            DO CASE
            CASE hbmk[ _HBMK_cARCH ] == "linux"
               AAdd( l_aLIBSYS, "dl" )
               AAdd( l_aLIBSYS, "rt" )
            CASE hbmk[ _HBMK_cARCH ] == "sunos"
               AAdd( l_aLIBSYS, "rt" )
               AAdd( l_aLIBSYS, "socket" )
               AAdd( l_aLIBSYS, "nsl" )
               AAdd( l_aLIBSYS, "resolv" )
            CASE hbmk[ _HBMK_cARCH ] == "hpux"
               AAdd( l_aLIBSYS, "rt" )
            ENDCASE
         ENDIF

         IF IsGTRequested( hbmk, "gtcrs" )
            /* TOFIX: Sometimes 'ncur194' is needed. */
            AAdd( l_aLIBSYS, IIF( hbmk[ _HBMK_cARCH ] == "sunos", "curses", "ncurses" ) )
         ENDIF
         IF IsGTRequested( hbmk, "gtsln" )
            AAdd( l_aLIBSYS, "slang" )
            /* Add paths, where this isn't a system component */
            DO CASE
            CASE hbmk[ _HBMK_cARCH ] == "darwin"
               AAdd( hbmk[ _HBMK_aLIBPATH ], "/sw/lib" )
               AAdd( hbmk[ _HBMK_aLIBPATH ], "/opt/local/lib" )
            CASE hbmk[ _HBMK_cARCH ] == "bsd"
               AAdd( hbmk[ _HBMK_aLIBPATH ], "/usr/local/lib" )
            ENDCASE
         ENDIF
         IF IsGTRequested( hbmk, "gtxwc" )
            IF hb_DirExists( "/usr/X11R6/lib64" )
               AAdd( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib64" )
            ENDIF
            AAdd( hbmk[ _HBMK_aLIBPATH ], "/usr/X11R6/lib" )
            AAdd( l_aLIBSYS, "X11" )
         ENDIF

      CASE ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "gcc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "mingw64" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "mingwarm" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "cygwin" )

         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := hbmk[ _HBMK_cCCPREFIX ] + iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], "g++", "gcc" ) + hbmk[ _HBMK_cCCPOSTFIX ] + cCCEXT_mingw
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
            IF hbmk[ _HBMK_cCOMP ] $ "gcc|mingw"
               cOpt_CompC += " -march=i586 -mtune=pentiumpro"
            ENDIF
            IF ! hbmk[ _HBMK_lDEBUG ]
               cOpt_CompC += " -fomit-frame-pointer"
            ENDIF
         ENDIF
         cOpt_CompC += " {FC}"
         cOptIncMask := "-I{DI}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {LS} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Lib := hbmk[ _HBMK_cCCPREFIX ] + "ar" + cCCEXT_mingw
#if defined( __PLATFORM__WINDOWS )
         nCmd_Esc := _ESC_DBLQUOTE
#endif
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         cLibObjPrefix := NIL
         IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
            cBin_Lib   := FN_Escape( hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_Lib, nCmd_Esc )
            cBin_CompC := FN_Escape( hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_CompC, nCmd_Esc )
            cBin_Link  := FN_Escape( hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_Link, nCmd_Esc )
         ENDIF
         IF !( hbmk[ _HBMK_cARCH ] == "wce" )
            IF hbmk[ _HBMK_lGUI ]
               AAdd( hbmk[ _HBMK_aOPTL ], "-mwindows" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "-mconsole" )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
         ENDIF
         IF hbmk[ _HBMK_lSHARED ]
            AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_BIN_INSTALL )
         ENDIF
         IF ! lStopAfterCComp
            IF hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm"
               AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
               aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, l_aLIBVM } )
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
         ENDIF
         IF ! hbmk[ _HBMK_lSHARED ]
            l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         ENDIF
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] == "mingw64"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-x64",;
                                                      "harbour" + cDL_Version_Alter + "-x64" ) }
         CASE hbmk[ _HBMK_cCOMP ] == "mingwarm"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-arm",;
                                                      "harbour" + cDL_Version_Alter + "-arm" ) }
         OTHERWISE
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter,;
                                                      "harbour" + cDL_Version_Alter ) }
         ENDCASE

         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm" .AND. Len( hbmk[ _HBMK_aRESSRC ] ) > 0
            cBin_Res := hbmk[ _HBMK_cCCPREFIX ] + "windres" + cCCEXT_mingw
            cResExt := ".reso"
            cOpt_Res := "{FR} {IR} -O coff -o {OS}"
            IF ! Empty( hbmk[ _HBMK_cCCPATH ] )
               cBin_Res := FN_Escape( hbmk[ _HBMK_cCCPATH ] + hb_osPathSeparator() + cBin_Res, nCmd_Esc )
            ENDIF
         ENDIF

      CASE hbmk[ _HBMK_cARCH ] == "os2" .AND. hbmk[ _HBMK_cCOMP ] == "gcc"

         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], "g++.exe", "gcc.exe" )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         cOpt_CompC += " {FC}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Lib := "ar.exe"
         cOpt_Lib := "{FA} rcs {OL} {LO}"
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
         ENDIF
         IF hbmk[ _HBMK_lSHARED ]
            AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_BIN_INSTALL )
         ENDIF
         AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
         aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, l_aLIBVM } )
         IF ! hbmk[ _HBMK_lSHARED ]
            l_aLIBSYS := ArrayJoin( l_aLIBSYS, { "socket" } )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
         ENDIF
         /* OS/2 needs a space between -o and file name following it */
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o {OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o {OE}" )
         ENDIF

      CASE hbmk[ _HBMK_cARCH ] == "dos" .AND. hbmk[ _HBMK_cCOMP ] == "djgpp"

         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-g" )
         ENDIF
         cLibLibPrefix := "lib"
         cLibPrefix := "-l"
         cLibExt := ""
         cObjExt := ".o"
         cBin_CompC := iif( hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ], "gpp.exe", "gcc.exe" )
         cOpt_CompC := "-c"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -O3"
         ENDIF
         cOpt_CompC += " {FC}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -o {OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         cBin_Link := cBin_CompC
         cOpt_Link := "{LO} {LA} {FL} {DL}{SCRIPT}"
         cLibPathPrefix := "-L"
         cLibPathSep := " "
         cLibLibExt := ".a"
         cBin_Lib := "ar.exe"
         cOpt_Lib := "{FA} rcs {OL} {LO}{SCRIPT}"
         IF hbmk[ _HBMK_cCOMP ] == "djgpp"
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,--start-group {LL} {LB} -Wl,--end-group" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "{LL} {LB}" )
            aLIB_BASE2 := ArrayAJoin( { aLIB_BASE2, { "hbcommon", "hbrtl" }, l_aLIBVM } )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-Wl,-Map,{OM}" )
         ENDIF
         IF ! hbmk[ _HBMK_lSHARED ]
            l_aLIBSYS := ArrayJoin( l_aLIBSYS, { "m" } )
         ENDIF
         IF hbmk[ _HBMK_lSTRIP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-s" )
         ENDIF
         IF lStopAfterCComp
            IF ! lCreateLib .AND. ! lCreateDyn .AND. ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
            ENDIF
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-o{OE}" )
         ENDIF

      /* Watcom family */
      CASE hbmk[ _HBMK_cARCH ] == "dos" .AND. hbmk[ _HBMK_cCOMP ] == "watcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            cBin_CompC := "wpp386.exe"
         ELSE
            cBin_CompC := "wcc386.exe"
         ENDIF
         cOpt_CompC := ""
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -5r -fp5"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ENDIF
         cOpt_CompC += " -zq -bt=DOS {FC}"
         cOptIncMask := "-i{DI}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
            IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OO}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OD}" )
            ENDIF
         ENDIF
         cBin_Link := "wlink.exe"
         cOpt_Link := "SYS causeway {FL} NAME {OE} {LO} {DL} {LL} {LB}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "OP MAP" )
         ENDIF

      CASE hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "watcom"
         nCmd_Esc := _ESC_DBLQUOTE
         nScr_Esc := _ESC_SINQUOTE_WATCOM
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            cBin_CompC := "wpp386.exe"
         ELSE
            cBin_CompC := "wcc386.exe"
         ENDIF
         cOpt_CompC := ""
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -6s -fp6"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ELSE
            cOpt_CompC += " -3s"
         ENDIF
         cOpt_CompC += " -zq -bt=NT {FC}"
         cOptIncMask := "-i{DI}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
            IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OO}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OD}" )
            ENDIF
         ENDIF
         cBin_Link := "wlink.exe"
         cOpt_Link := "{FL} NAME {OE} {LO} {DL} {LL} {LB} {LS}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF hbmk[ _HBMK_lMT ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-bm" )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            /* NOTE: These could probably be optimized */
            AAdd( hbmk[ _HBMK_aOPTC ], "-bg" )
            AAdd( hbmk[ _HBMK_aOPTL ], "RU NAT" )
            AAdd( hbmk[ _HBMK_aOPTL ], "SYSTEM NT_WIN" )
         ELSE
            /* NOTE: These could probably be optimized */
            AAdd( hbmk[ _HBMK_aOPTC ], "-bc" )
            AAdd( hbmk[ _HBMK_aOPTL ], "RU CON" )
            AAdd( hbmk[ _HBMK_aOPTL ], "SYSTEM NT" )
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "OP MAP" )
         ENDIF
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + cLibExt,;
                                                   "harbour" + cDL_Version_Alter + cLibExt ) }

         IF hbmk[ _HBMK_lSHARED ]
            AAdd( hbmk[ _HBMK_aOPTL ], "FILE " + FN_ExtSet( l_cHB_LIB_INSTALL + hb_osPathSeparator() + iif( hbmk[ _HBMK_lGUI ], "hbmainwin", "hbmainstd" ), cLibExt ) )
         ENDIF

         IF Len( hbmk[ _HBMK_aRESSRC ] ) > 0
            cBin_Res := "wrc"
            cResExt := ".res"
            cOpt_Res := "-r {FR} -zm {IR} -fo={OS}"
            cResPrefix := "OP res="
         ENDIF

      CASE hbmk[ _HBMK_cARCH ] == "os2" .AND. hbmk[ _HBMK_cCOMP ] == "watcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".obj"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            cBin_CompC := "wpp386.exe"
         ELSE
            cBin_CompC := "wcc386.exe"
         ENDIF
         cOpt_CompC := ""
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -5s -fp5"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ENDIF
         cOpt_CompC += " -zq -bt=OS2 {FC}"
         cOptIncMask := "-i{DI}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
            IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OO}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OD}" )
            ENDIF
         ENDIF
         cBin_Link := "wlink.exe"
         cOpt_Link := "{FL} NAME {OE} {LO} {DL} {LL} {LB}{SCRIPT}"
         cBin_Lib := "wlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF hbmk[ _HBMK_lMT ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-bm" )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "OP MAP" )
         ENDIF

      CASE hbmk[ _HBMK_cARCH ] == "linux" .AND. hbmk[ _HBMK_cCOMP ] == "watcom"
         cLibPrefix := "LIB "
         cLibExt := ".lib"
         cObjPrefix := "FILE "
         cObjExt := ".o"
         cLibPathPrefix := "LIBPATH "
         cLibPathSep := " "
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            cBin_CompC := "wpp386"
         ELSE
            cBin_CompC := "wcc386"
         ENDIF
         cOpt_CompC := ""
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -6s -fp6"
            cOpt_CompC += " -onaehtr -s -ei -zp4 -zt0"
            IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
               cOpt_CompC += " -oi+"
            ELSE
               cOpt_CompC += " -oi"
            ENDIF
         ENDIF
         cOpt_CompC += " -zq -bt=linux {FC}"
         cOptIncMask := "-i{DI}"
         IF hbmk[ _HBMK_lINC ] .AND. ! Empty( cWorkDir )
            cOpt_CompC += " {IC} -fo={OO}"
         ELSE
            cOpt_CompC += " {LC}"
         ENDIF
         IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
            IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OO}" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-fo={OD}" )
            ENDIF
         ENDIF
         cBin_Link := "wlink"
         cOpt_Link := "SYS LINUX {FL} NAME {OE} {LO} {DL} {LL} {LB}{SCRIPT}"
         cBin_Lib := "wlib"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         IF hbmk[ _HBMK_lMT ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-bm" )
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-d2" )
            cOpt_Link := "DEBUG ALL" + cOpt_Link
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "OP MAP" )
         ENDIF

      /* Misc */
      CASE hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "bcc"
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-y -v" )
            AAdd( hbmk[ _HBMK_aOPTL ], "-v" )
         ELSE
            AAdd( l_aCLEAN, PathSepToTarget( hbmk, FN_ExtSet( l_cPROGNAME, ".tds" ) ) )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-tW" )
         ENDIF
         IF hbmk[ _HBMK_lCPP ] != NIL .AND. hbmk[ _HBMK_lCPP ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-P" )
         ENDIF
         nCmd_Esc := _ESC_DBLQUOTE
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cBin_Lib := "tlib.exe"
         cOpt_Lib := "{FA} {OL} {LO}{SCRIPT}"
         cLibLibExt := cLibExt
         cLibObjPrefix := "-+ "
         cOptIncMask := "-I{DI}"
         cBin_CompC := "bcc32.exe"
         cOpt_CompC := "-c -q -tWM"
         IF hbmk[ _HBMK_lOPTIM ]
            cOpt_CompC += " -d -6 -O2 -OS -Ov -Oi -Oc"
         ENDIF
         cOpt_CompC += " {FC} {LC}"
         cBin_Res := "brcc32.exe"
         cOpt_Res := "{FR} {IR} -fo{OS}"
         cResExt := ".res"
         cBin_Link := "ilink32.exe"
         cBin_Dyn := cBin_Link
         cOpt_Link := '-Gn -Tpe -L{DL} {FL} ' + iif( hbmk[ _HBMK_lGUI ], "c0w32.obj", "c0x32.obj" ) + " {LO}, {OE}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} cw32mt.lib import32.lib,, {LS}{SCRIPT}"
         cOpt_Dyn  := '-Gn -Tpd -L{DL} {FD} ' +                          "c0d32.obj"                + " {LO}, {OD}, " + iif( hbmk[ _HBMK_lMAP ], "{OM}", "nul" ) + ", {LL} {LB} cw32mt.lib import32.lib,, {LS}{SCRIPT}"
         cLibPathPrefix := ""
         cLibPathSep := ";"
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTL ], "-aa" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-aa" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "-ap" )
            AAdd( hbmk[ _HBMK_aOPTD ], "-ap" )
         ENDIF
         IF hbmk[ _HBMK_lINC ]
            IF ! Empty( cWorkDir )
               AAdd( hbmk[ _HBMK_aOPTC ], "-n" + FN_Escape( PathSepToTarget( hbmk, cWorkDir ), nCmd_Esc ) )
            ENDIF
         ELSE
            IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
               IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
                  AAdd( hbmk[ _HBMK_aOPTC ], "-o{OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-n{OD}" )
               ENDIF
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSHARED ]
            AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_BIN_INSTALL )
         ENDIF
         l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-bcc" + cLibExt,;
                                                   "harbour" + cDL_Version_Alter + "-bcc" + cLibExt ) }
         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )

      CASE ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] $ "msvc|msvc64|msvcia64|icc|iccia64" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "msvcarm" ) /* NOTE: Cross-platform: wce/ARM on win/x86 */

         IF Empty( nCCompVer )
            /* Compatibility with Harbour GNU Make system */
            IF Empty( GetEnv( "HB_VISUALC_VER_PRE80" ) )
               nCCompVer := 800 /* Visual Studio 2005 */
            ELSE
               nCCompVer := 710 /* Visual Studio .NET 2003 */
            ENDIF
            /*  900 : Visual Studio 2008 */
            /* 1000 : Visual Studio 2010 */
         ENDIF

         IF hbmk[ _HBMK_lDEBUG ]
            IF hbmk[ _HBMK_cCOMP ] == "msvcarm"
               AAdd( hbmk[ _HBMK_aOPTC ], "-Zi" )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTC ], "-MTd -Zi" )
            ENDIF
            AAdd( hbmk[ _HBMK_aOPTL ], "/debug" )
         ENDIF
         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTL ], "/subsystem:windows" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "/subsystem:console" )
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
         IF hbmk[ _HBMK_cCOMP ] $ "icc|iccia64"
            cBin_Lib := "xilib.exe"
            cBin_CompC := "icl.exe"
            cBin_Link := "xilink.exe"
            cBin_Dyn := cBin_Link
         ELSE
            cBin_Lib := "lib.exe"
            IF hbmk[ _HBMK_cCOMP ] == "msvcarm" .AND. nCCompVer < 800
               cBin_CompC := "clarm.exe"
            ELSE
               cBin_CompC := "cl.exe"
            ENDIF
            cBin_Link := "link.exe"
            cBin_Dyn := cBin_Link
         ENDIF
         nCmd_Esc := _ESC_DBLQUOTE
         cOpt_Lib := "/nologo {FA} /out:{OL} {LO}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LB} {LS}"
         cOpt_CompC := "-nologo -c -Gs"
         IF hbmk[ _HBMK_lOPTIM ]
            IF hbmk[ _HBMK_cCOMP ] == "msvcarm"
               IF nCCompVer >= 800
                  cOpt_CompC += " -Od -Os -Gy -GS- -Gm -Zi -GR-"
               ELSE
                  cOpt_CompC += " -Oxsb1 -YX -GF"
               ENDIF
            ELSE
               IF nCCompVer >= 800
                  cOpt_CompC += " -Ot2b1"
               ELSE
                  cOpt_CompC += " -Ogt2yb1p -GX- -G6 -YX"
               ENDIF
            ENDIF
         ENDIF
         cOpt_CompC += " {FC} {LC}"
         cOptIncMask := "-I{DI}"
         cOpt_Link := "/nologo /out:{OE} {LO} {DL} {FL} {LL} {LB} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "/map" )
            AAdd( hbmk[ _HBMK_aOPTD ], "/map" )
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] == "msvcarm"
            /* NOTE: Copied from .cf. Probably needs cleaning. */
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_WIN32_WCE=0x420" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNDER_CE=0x420" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DWIN32_PLATFORM_PSPC" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DWINCE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_WINCE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_WINDOWS" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DARM" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_ARM_" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DARMV4" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DPOCKETPC2003_UI_MODEL" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_M_ARM" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_UNICODE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_UWIN" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/subsystem:windowsce,4.20" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/machine:arm" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/armpadcode" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/stack:65536,4096" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/nodefaultlib:oldnames.lib" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/nodefaultlib:kernel32.lib" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/align:4096" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/opt:ref" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/opt:icf" )
            AAdd( hbmk[ _HBMK_aOPTL ], "/manifest:no" )
         ENDIF
         IF hbmk[ _HBMK_lINC ]
            IF ! Empty( cWorkDir )
               AAdd( hbmk[ _HBMK_aOPTC ], "-Fo" + FN_Escape( PathSepToTarget( hbmk, cWorkDir ) + hb_osPathSeparator(), nCmd_Esc ) ) /* NOTE: Ending path sep is important. */
            ENDIF
         ELSE
            IF lStopAfterCComp .AND. ! lCreateLib .AND. ! lCreateDyn
               IF ( Len( hbmk[ _HBMK_aPRG ] ) + Len( hbmk[ _HBMK_aC ] ) ) == 1
                  AAdd( hbmk[ _HBMK_aOPTC ], "-Fo{OO}" )
               ELSE
                  AAdd( hbmk[ _HBMK_aOPTC ], "-Fo{OD}" )
               ENDIF
            ENDIF
         ENDIF
         IF hbmk[ _HBMK_lSHARED ]
            AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_BIN_INSTALL )
         ENDIF
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] $ "msvc|icc"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + cLibExt ) }
         CASE hbmk[ _HBMK_cCOMP ] == "msvc64"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-x64" + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + "-x64" + cLibExt ) }
         CASE hbmk[ _HBMK_cCOMP ] $ "msvcia64|iccia64"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-ia64" + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + "-ia64" + cLibExt ) }
         CASE hbmk[ _HBMK_cCOMP ] == "msvcarm"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-arm" + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + "-arm" + cLibExt ) }
         ENDCASE

         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

         IF !( hbmk[ _HBMK_cCOMP ] $ "icc|iccia64" )
            cBin_Res := "rc.exe"
            cOpt_Res := "{FR} /fo {OS} {IR}"
            IF nCCompVer >= 1000
               cOpt_Res := "/nologo " + cOpt_Res  /* NOTE: Only in MSVC 2010 and upper. [vszakats] */
            ENDIF
            cResExt := ".res"
         ENDIF

      CASE ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "pocc" ) .OR. ;
           ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "pocc64" ) .OR. ; /* NOTE: Cross-platform: win/amd64 on win/x86 */
           ( hbmk[ _HBMK_cARCH ] == "wce" .AND. hbmk[ _HBMK_cCOMP ] == "poccarm" ) .OR. ; /* NOTE: Cross-platform: wce/ARM on win/x86 */
           ( hbmk[ _HBMK_cARCH ] == "win" .AND. hbmk[ _HBMK_cCOMP ] == "xcc" )

         IF hbmk[ _HBMK_lGUI ]
            AAdd( hbmk[ _HBMK_aOPTL ], "/subsystem:windows" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTL ], "/subsystem:console" )
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTC ], "-Zi" )
         ENDIF
         nCmd_Esc := _ESC_DBLQUOTE
         cLibPrefix := NIL
         cLibExt := ".lib"
         cObjExt := ".obj"
         cLibLibExt := cLibExt
         IF hbmk[ _HBMK_cCOMP ] == "xcc"
            cBin_CompC := "xcc.exe"
            cBin_Lib := "xlib.exe"
            cBin_Link := "xlink.exe"
            cBin_Res := "xrc.exe"
         ELSE
            cBin_CompC := "pocc.exe"
            cBin_Lib := "polib.exe"
            cBin_Link := "polink.exe"
            cBin_Res := "porc.exe"
         ENDIF
         cBin_Dyn := cBin_Link
         cOpt_CompC := "/c /Ze"
         IF !( hbmk[ _HBMK_cCOMP ] == "poccarm" ) .AND. ;
            !( hbmk[ _HBMK_cCOMP ] == "xcc" ) /* xcc doesn't have this enabled in default Harbour builds. */
            cOpt_CompC += " /MT"
         ENDIF
         IF !( hbmk[ _HBMK_cCOMP ] == "xcc" )
            cOpt_CompC += " /Go"
         ENDIF
         cOpt_CompC += " {FC} {IC} /Fo{OO}"
         IF Empty( cWorkDir )
            cWorkDir := "."
         ENDIF
         cOptIncMask := "/I{DI}"
         cOpt_Dyn := "{FD} /dll /out:{OD} {DL} {LO} {LL} {LB} {LS}"
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] == "pocc"
            IF hbmk[ _HBMK_lOPTIM ]
               AAdd( hbmk[ _HBMK_aOPTC ], "/Ot" )
            ENDIF
            AAdd( hbmk[ _HBMK_aOPTC ], "/Tx86-coff" )
         CASE hbmk[ _HBMK_cCOMP ] == "pocc64"
            AAdd( hbmk[ _HBMK_aOPTC ], "/Tamd64-coff" )
         CASE hbmk[ _HBMK_cCOMP ] == "poccarm"
            AAdd( hbmk[ _HBMK_aOPTC ], "/Tarm-coff" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_M_ARM" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-D_WINCE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DUNICODE" )
            AAdd( hbmk[ _HBMK_aOPTC ], "-DHB_NO_WIN_CONSOLE" )
         ENDCASE
         cOpt_Res := "{FR} /Fo{OS} {IR}"
         cResExt := ".res"
         cOpt_Lib := "{FA} /out:{OL} {LO}"
         IF hbmk[ _HBMK_lMT ]
            AAdd( hbmk[ _HBMK_aOPTC ], "/MT" )
         ENDIF
         cOpt_Link := "/out:{OE} {LO} {DL} {FL} {LL} {LB} {LS}"
         cLibPathPrefix := "/libpath:"
         cLibPathSep := " "
         IF hbmk[ _HBMK_lSHARED ]
            AAdd( hbmk[ _HBMK_aLIBPATH ], l_cHB_BIN_INSTALL )
         ENDIF
         IF hbmk[ _HBMK_lMAP ]
            AAdd( hbmk[ _HBMK_aOPTL ], "/map" )
         ENDIF
         IF hbmk[ _HBMK_lDEBUG ]
            AAdd( hbmk[ _HBMK_aOPTL ], "/debug" )
         ENDIF
         l_aLIBSYS := ArrayAJoin( { l_aLIBSYS, l_aLIBSYSCORE, l_aLIBSYSMISC } )
         DO CASE
         CASE hbmk[ _HBMK_cCOMP ] == "pocc64"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-x64" + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + "-x64" + cLibExt ) }
         CASE hbmk[ _HBMK_cCOMP ] == "poccarm"
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + "-arm" + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + "-arm" + cLibExt ) }
         OTHERWISE
            l_aLIBSHARED := { iif( hbmk[ _HBMK_lMT ], "harbourmt" + cDL_Version_Alter + cLibExt,;
                                                      "harbour" + cDL_Version_Alter + cLibExt ) }
         ENDCASE

         l_aLIBSHAREDPOST := { "hbmainstd", "hbmainwin" }

      ENDCASE

      /* NOTE: We only use different shared object flags when compiling for
               "( win || wce ) & !( allmingw | cygwin )". This may change in the future.
               IMPORTANT: Keep this condition in sync with workdir default settings */
      IF lCreateDyn .AND. hbmk[ _HBMK_cARCH ] $ "win|wce" .AND. !( hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm|cygwin" )
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB .OR. ;
            hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10
            AAdd( hbmk[ _HBMK_aOPTC ], "-D__EXPORT__" )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTC ], "-DHB_DYNLIB" )
         ENDIF
      ENDIF
   ENDIF

   DEFAULT nScr_Esc TO nCmd_Esc

   /* Delete all lib paths which contain late-evaluation macros. */
   FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ] DESCEND
      IF ( _MACRO_LATE_PREFIX + _MACRO_OPEN ) $ tmp
         hb_ADel( hbmk[ _HBMK_aLIBPATH ], tmp:__enumIndex(), .T. )
      ENDIF
   NEXT

   IF ! lStopAfterInit
      IF hbmk[ _HBMK_lINC ]
         IF ! Empty( cWorkDir )
            /* NOTE: Ending path sep is important. */
            /* Different escaping for internal and external compiler. */
            IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
               AAdd( hbmk[ _HBMK_aOPTPRG ], "-o" + cWorkDir + hb_osPathSeparator() )
            ELSE
               AAdd( hbmk[ _HBMK_aOPTPRG ], "-o" + FN_Escape( cWorkDir + hb_osPathSeparator(), nCmd_Esc ) )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF ! lStopAfterInit .AND. ! lStopAfterHarbour
      hb_FNameSplit( l_cPROGNAME, @cDir, @cName, @cExt )
      DO CASE
      CASE ! lStopAfterCComp
         IF Empty( cExt ) .AND. ! Empty( cBinExt )
            l_cPROGNAME := hb_FNameMerge( cDir, cName, cBinExt )
         ENDIF
      CASE lStopAfterCComp .AND. lCreateDyn
         IF Empty( cExt ) .AND. ! Empty( cDynLibExt )
            l_cPROGNAME := hb_FNameMerge( cDir, cName, cDynLibExt )
         ENDIF
      CASE lStopAfterCComp .AND. lCreateLib
         l_cPROGNAME := hb_FNameMerge( cDir, cLibLibPrefix + cName, iif( Empty( cLibLibExt ), cExt, cLibLibExt ) )
      ENDCASE
   ENDIF

   /* Generate header with repository ID information */

   IF ! lSkipBuild .AND. ! lStopAfterInit .AND. ! lStopAfterHarbour
      IF ! Empty( l_cVCSHEAD )
         tmp1 := VCSID( l_cVCSDIR, l_cVCSHEAD, @tmp2 )
         /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
         tmp := "/* Automatically generated by hbmk. Do not edit. */" + Chr( 10 ) +;
                "#define _HBMK_VCS_TYPE_ " + '"' + tmp2 + '"' + Chr( 10 ) +;
                "#define _HBMK_VCS_ID_   " + '"' + tmp1 + '"' + Chr( 10 )
         /* Update only if something changed to trigger rebuild only if really needed */
         IF !( hb_MemoRead( l_cVCSHEAD ) == tmp )
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Generating VCS header: %1$s" ), l_cVCSHEAD ) )
            ENDIF
            hb_MemoWrit( l_cVCSHEAD, tmp )
         ENDIF
      ENDIF
      IF ! Empty( l_cTSHEAD )
         /* Use the same EOL for all platforms to avoid unnecessary rebuilds. */
         tmp1 := hb_DateTime()
         tmp := "/* Automatically generated by hbmk. Do not edit. */" + Chr( 10 ) +;
                "#define _HBMK_BUILD_DATE_      " + '"' +            DToS( tmp1 )         + '"' + Chr( 10 ) +;
                "#define _HBMK_BUILD_TIME_      " + '"' + SubStr( hb_TToS( tmp1 ), 9, 6 ) + '"' + Chr( 10 ) +;
                "#define _HBMK_BUILD_TIMESTAMP_ " + '"' +         hb_TToS( tmp1 )         + '"' + Chr( 10 )
         IF hbmk[ _HBMK_lInfo ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Generating timestamp header: %1$s" ), l_cTSHEAD ) )
         ENDIF
         hb_MemoWrit( l_cTSHEAD, tmp )
      ENDIF
   ENDIF

   /* Header paths */

   IF ! lSkipBuild .AND. ! lStopAfterInit
      FOR EACH tmp IN hbmk[ _HBMK_aINCPATH ]
         /* Different escaping for internal and external compiler. */
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_NATIVE
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-i" + tmp )
         ELSE
            AAdd( hbmk[ _HBMK_aOPTPRG ], "-i" + FN_Escape( tmp, nCmd_Esc ) )
         ENDIF
         IF ! lStopAfterHarbour
            AAdd( hbmk[ _HBMK_aOPTC ], StrTran( cOptIncMask, "{DI}", FN_Escape( tmp, nCmd_Esc ) ) )
            AAdd( hbmk[ _HBMK_aOPTRES ], StrTran( cOptIncMask, "{DI}", FN_Escape( tmp, nCmd_Esc ) ) )
         ENDIF
      NEXT
   ENDIF

   /* Do header detection and create incremental file list for .c files */

   IF ! lSkipBuild .AND. ! lStopAfterInit .AND. ! lStopAfterHarbour

      headstate := NIL

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
         l_aC_TODO := {}
         l_aC_DONE := {}
         FOR EACH tmp IN hbmk[ _HBMK_aC ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: C %1$s %2$s", tmp, FN_DirExtSet( tmp, cWorkDir, cObjExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cObjExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, NIL, tmp2, ! Empty( hbmk[ _HBMK_aINCTRYPATH ] ), .T., @headstate ) )
               AAdd( l_aC_TODO, tmp )
            ELSE
               AAdd( l_aC_DONE, tmp )
            ENDIF
         NEXT
      ELSE
         l_aC_TODO := AClone( hbmk[ _HBMK_aC ] )
         l_aC_DONE := {}
      ENDIF

      /* Header dir detection if needed and if FindNewerHeaders() wasn't called yet. */
      IF ! Empty( hbmk[ _HBMK_aINCTRYPATH ] ) .AND. ! Empty( l_aC_TODO ) .AND. headstate == NIL
         FOR EACH tmp IN l_aC_TODO
            FindNewerHeaders( hbmk, tmp, NIL, NIL, .T., .T., @headstate )
         NEXT
      ENDIF
   ENDIF

   /* Create incremental file list for .prg files */

   IF ! lSkipBuild .AND. ! lStopAfterInit .AND. ! lStopAfterHarbour .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

      /* Incremental */

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
         l_aPRG_TODO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: PRG %1$s %2$s",;
                  FN_ExtSet( tmp, ".prg" ),;
                  FN_DirExtSet( tmp, cWorkDir, ".c" ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, ".c" ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( FN_ExtSet( tmp, ".prg" ), @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, FN_ExtSet( tmp, ".prg" ), NIL, tmp2, .F., .F., @headstate ) )
               AAdd( l_aPRG_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aPRG_TODO := hbmk[ _HBMK_aPRG ]
      ENDIF
   ELSE
      l_aPRG_TODO := hbmk[ _HBMK_aPRG ]
   ENDIF

   /* Harbour compilation */

   IF ! lSkipBuild .AND. ! lStopAfterInit .AND. Len( l_aPRG_TODO ) > 0 .AND. ! l_lCLEAN .AND. hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

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
            aCommand := ArrayAJoin( { { iif( lCreateLib .OR. lCreateDyn, "-n1", "-n2" ) },;
                                      aTODO,;
                                      iif( l_lBLDFLGP, { " " + cSelfFlagPRG }, {} ),;
                                      ListToArray( iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) ),;
                                      hbmk[ _HBMK_aOPTPRG ] } )

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  IF Len( aTODO:__enumBase() ) > 1
                     hbmk_OutStd( hbmk, hb_StrFormat( I_( "Harbour compiler command (internal) job #%1$s:" ), hb_ntos( aTODO:__enumIndex() ) ) )
                  ELSE
                     hbmk_OutStd( hbmk, I_( "Harbour compiler command (internal):" ) )
                  ENDIF
               ENDIF
               OutStd( FN_Escape( DirAddPathSep( PathSepToSelf( l_cHB_BIN_INSTALL ) ) + cBin_CompPRG + cBinExt, nCmd_Esc ) +;
                       " " + ArrayToList( aCommand ) + hb_osNewLine() )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ]
               IF hb_mtvm() .AND. Len( aTODO:__enumBase() ) > 1
                  AAdd( aThreads, { hb_threadStart( @hb_compile(), "harbour", aCommand ), aCommand } )
               ELSE
                  IF ( tmp := hb_compile( "harbour", aCommand ) ) != 0
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler. %1$s" ), hb_ntos( tmp ) ) )
                     IF ! hbmk[ _HBMK_lQuiet ]
                        OutErr( FN_Escape( DirAddPathSep( PathSepToSelf( l_cHB_BIN_INSTALL ) ) + cBin_CompPRG + cBinExt, nCmd_Esc ) +;
                                " " + ArrayToList( aCommand ) + hb_osNewLine() )
                     ENDIF
                     IF hbmk[ _HBMK_lBEEP ]
                        DoBeep( hbmk, .F. )
                     ENDIF
                     RETURN 6
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
                     OutErr( ArrayToList( thread[ 2 ] ) + hb_osNewLine() )
                  ENDIF
                  IF hbmk[ _HBMK_lBEEP ]
                     DoBeep( hbmk, .F. )
                  ENDIF
                  RETURN 6
               ENDIF
            NEXT
         ENDIF
      ELSE
         /* Use external compiler */

         cCommand := FN_Escape( DirAddPathSep( PathSepToSelf( l_cHB_BIN_INSTALL ) ) + cBin_CompPRG + cBinExt, nCmd_Esc ) +;
                     " " + iif( lCreateLib .OR. lCreateDyn, "-n1", iif( hbmk[ _HBMK_nHBMODE ] != _HBMODE_NATIVE, "-n", "-n2" ) ) +;
                     " " + ArrayToList( l_aPRG_TODO,, nCmd_Esc ) +;
                     iif( l_lBLDFLGP, " " + cSelfFlagPRG, "" ) +;
                     iif( ! Empty( GetEnv( "HB_USER_PRGFLAGS" ) ), " " + GetEnv( "HB_USER_PRGFLAGS" ), "" ) +;
                     iif( ! Empty( hbmk[ _HBMK_aOPTPRG ] ), " " + ArrayToList( hbmk[ _HBMK_aOPTPRG ] ), "" )

         cCommand := AllTrim( cCommand )

         IF hbmk[ _HBMK_lTRACE ]
            IF ! hbmk[ _HBMK_lQuiet ]
               hbmk_OutStd( hbmk, I_( "Harbour compiler command:" ) )
            ENDIF
            OutStd( cCommand + hb_osNewLine() )
         ENDIF

         IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running Harbour compiler. %1$s" ), hb_ntos( tmp ) ) )
            IF ! hbmk[ _HBMK_lQuiet ]
               OutErr( cCommand + hb_osNewLine() )
            ENDIF
            IF hbmk[ _HBMK_lBEEP ]
               DoBeep( hbmk, .F. )
            ENDIF
            RETURN 6
         ENDIF
      ENDIF
   ENDIF

   IF ! lSkipBuild .AND. ! lStopAfterInit .AND. ! lStopAfterHarbour

      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C

         /* Do entry function detection on platform required and supported */
         IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ! lStopAfterCComp .AND. l_cMAIN == NIL
            tmp := iif( Lower( FN_ExtGet( hbmk[ _HBMK_cFIRST ] ) ) == ".prg" .OR. Empty( FN_ExtGet( hbmk[ _HBMK_cFIRST ] ) ), FN_ExtSet( hbmk[ _HBMK_cFIRST ], ".c" ), hbmk[ _HBMK_cFIRST ] )
            IF ! Empty( tmp := getFirstFunc( hbmk, tmp ) )
               l_cMAIN := tmp
            ENDIF
         ENDIF

         /* HACK: Override entry point requested by user or detected by us,
                  and override the GT if requested by user. */
         IF ! lStopAfterCComp .AND. ;
            ! l_lCLEAN .AND. ;
            ( l_cMAIN != NIL .OR. ;
              ! Empty( hbmk[ _HBMK_aLIBUSERGT ] ) .OR. ;
              hbmk[ _HBMK_cGT ] != NIL )

            fhnd := hb_FTempCreateEx( @l_cCSTUB, NIL, "hbmk_", ".c" )
            IF fhnd != F_ERROR

               /* NOTE: This has to be kept synced with Harbour HB_IMPORT values. */
               DO CASE
               CASE ! hbmk[ _HBMK_lSHARED ] .OR. ;
                    !( hbmk[ _HBMK_cARCH ] $ "win|wce" ) .OR. ;
                    hbmk[ _HBMK_cCOMP ] $ "msvc|msvc64|msvcia64|icc|iccia64"

                  /* NOTE: MSVC gives the warning:
                           "LNK4217: locally defined symbol ... imported in function ..."
                           if using 'dllimport'. [vszakats] */
                  tmp := ""
               CASE hbmk[ _HBMK_cCOMP ] $ "gcc|mingw|mingw64|mingwarm|cygwin"
                  tmp := "__attribute__ (( dllimport ))"
               CASE hbmk[ _HBMK_cCOMP ] $ "bcc|watcom"
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
                  AEval( hbmk[ _HBMK_aLIBUSERGT ], {|tmp| AAdd( array, "HB_GT_" + Upper( SubStr( tmp, 3 ) ) ) } )
               ENDIF

               /* Build C stub */
               FWrite( fhnd, '/* This temp source file was generated by hbmk tool. */'                 + hb_osNewLine() +;
                             '/* You can safely delete it. */'                                         + hb_osNewLine() +;
                             ''                                                                        + hb_osNewLine() +;
                             '#include "hbapi.h"'                                                      + hb_osNewLine() )
               IF ! Empty( array )
                  AEval( array, {|tmp, i| array[ i ] := FuncNameEncode( tmp ) } )
                  FWrite( fhnd, ''                                                                     + hb_osNewLine() )
                  AEval( array, {|tmp| FWrite( fhnd, 'HB_FUNC_EXTERN( ' + tmp + ' );'                  + hb_osNewLine() ) } )
                  FWrite( fhnd, ''                                                                     + hb_osNewLine() )
                  FWrite( fhnd, 'void _hb_lnk_ForceLink_hbmk( void )'                                  + hb_osNewLine() )
                  FWrite( fhnd, '{'                                                                    + hb_osNewLine() )
                  AEval( array, {|tmp| FWrite( fhnd, '   HB_FUNC_EXEC( ' + tmp + ' );'                 + hb_osNewLine() ) } )
                  FWrite( fhnd, '}'                                                                    + hb_osNewLine() )
                  FWrite( fhnd, ''                                                                     + hb_osNewLine() )
               ENDIF

               IF hbmk[ _HBMK_cGT ] != NIL .OR. ;
                  l_cMAIN != NIL
                  IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                     hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
                     FWrite( fhnd, '#include "hbinit.h"'                                                  + hb_osNewLine() +;
                                   ''                                                                     + hb_osNewLine() +;
                                   'HB_EXTERN_BEGIN'                                                      + hb_osNewLine() +;
                                   'extern ' + tmp + ' const char * s_defaultGT;'                         + hb_osNewLine() +;
                                   'extern ' + tmp + ' const char * s_pszLinkedMain;'                     + hb_osNewLine() +;
                                   'HB_EXTERN_END'                                                        + hb_osNewLine() +;
                                   ''                                                                     + hb_osNewLine() +;
                                   'HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )'                         + hb_osNewLine() )
                  ELSE
                     FWrite( fhnd, '#include "hbinit.h"'                                                  + hb_osNewLine() +;
                                   ''                                                                     + hb_osNewLine() +;
                                   'HB_EXTERN_BEGIN'                                                      + hb_osNewLine() +;
                                   'extern ' + tmp + ' void hb_vmSetLinkedMain( const char * szMain );'   + hb_osNewLine() +;
                                   'extern ' + tmp + ' void hb_gtSetDefault( const char * szGtName );'    + hb_osNewLine() +;
                                   'HB_EXTERN_END'                                                        + hb_osNewLine() +;
                                   ''                                                                     + hb_osNewLine() +;
                                   'HB_CALL_ON_STARTUP_BEGIN( _hb_hbmk_setdef_ )'                         + hb_osNewLine() )
                  ENDIF
                  IF hbmk[ _HBMK_cGT ] != NIL
                     IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                        hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
                        FWrite( fhnd, '   s_defaultGT = "' + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) + '";'           + hb_osNewLine() )
                     ELSE
                        FWrite( fhnd, '   hb_gtSetDefault( "' + Upper( SubStr( hbmk[ _HBMK_cGT ], 3 ) ) + '" );'      + hb_osNewLine() )
                     ENDIF
                  ENDIF
                  IF l_cMAIN != NIL
                     IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 .OR. ;
                        hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
                        FWrite( fhnd, '   s_pszLinkedMain = "' + Upper( l_cMAIN ) + '";'                  + hb_osNewLine() )
                     ELSE
                        FWrite( fhnd, '   hb_vmSetLinkedMain( "' + Upper( l_cMAIN ) + '" );'              + hb_osNewLine() )
                     ENDIF
                  ENDIF
                  FWrite( fhnd, 'HB_CALL_ON_STARTUP_END( _hb_hbmk_setdef_ )'                           + hb_osNewLine() +;
                                ''                                                                     + hb_osNewLine() +;
                                '#if defined( HB_PRAGMA_STARTUP )'                                     + hb_osNewLine() +;
                                '   #pragma startup_hb_lnk_SetDefault_hbmk_'                           + hb_osNewLine() +;
                                '#elif defined( HB_MSC_STARTUP )'                                      + hb_osNewLine() +;
                                '   #if defined( HB_OS_WIN_64 )'                                       + hb_osNewLine() +;
                                '      #pragma section( HB_MSC_START_SEGMENT, long, read )'            + hb_osNewLine() +;
                                '   #endif'                                                            + hb_osNewLine() +;
                                '   #pragma data_seg( HB_MSC_START_SEGMENT )'                          + hb_osNewLine() +;
                                '   static HB_$INITSYM hb_vm_auto_hbmk_setdef_ = _hb_hbmk_setdef_;'    + hb_osNewLine() +;
                                '   #pragma data_seg()'                                                + hb_osNewLine() +;
                                '#endif'                                                               + hb_osNewLine() )
               ENDIF
               FClose( fhnd )

               IF hbmk[ _HBMK_lDEBUGSTUB ]
                  OutStd( "C stub dump:" + hb_osNewLine() )
                  OutStd( hb_MemoRead( l_cCSTUB ) )
               ENDIF
            ELSE
               hbmk_OutErr( hbmk, I_( "Warning: Stub helper .c program couldn't be created." ) )
               IF ! hbmk[ _HBMK_lINC ]
                  AEval( ListDirExt( hbmk[ _HBMK_aPRG ], cWorkDir, ".c" ), {|tmp| FErase( tmp ) } )
               ENDIF
               IF hbmk[ _HBMK_lBEEP ]
                  DoBeep( hbmk, .F. )
               ENDIF
               RETURN 5
            ENDIF
            AAdd( hbmk[ _HBMK_aC ], l_cCSTUB )
            AAdd( l_aC_TODO, l_cCSTUB )
         ENDIF

         /* Library list assembly */
         IF hbmk[ _HBMK_lSHARED ] .AND. ! Empty( l_aLIBSHARED )
            l_aLIBHB := ArrayAJoin( { l_aLIBSHAREDPOST,;
                                      aLIB_BASE_CPLR,;
                                      aLIB_BASE_DEBUG } )
         ELSE
            l_aLIBHB := ArrayAJoin( { aLIB_BASE1,;
                                      aLIB_BASE_CPLR,;
                                      aLIB_BASE_DEBUG,;
                                      l_aLIBVM,;
                                      iif( hbmk[ _HBMK_lNULRDD ], aLIB_BASE_NULRDD, iif( hbmk[ _HBMK_lMT ], aLIB_BASE_RDD_MT, aLIB_BASE_RDD_ST ) ),;
                                      aLIB_BASE2,;
                                      iif( l_lHB_PCRE, aLIB_BASE_PCRE, {} ),;
                                      iif( l_lHB_ZLIB, aLIB_BASE_ZLIB, {} ) } )
         ENDIF
      ELSE
         l_aLIBHB := {}
         l_aLIBSHARED := {}
         hbmk[ _HBMK_aPRG ] := {}
      ENDIF

      /* Merge lib lists. */
      l_aLIBRAW := ArrayAJoin( { hbmk[ _HBMK_aLIBUSER ], l_aLIBHB, l_aLIB3RD, l_aLIBSYS } )
      /* Dress lib names. */
      l_aLIB := {}
      l_aLIBA := {}
      ListCookLib( hbmk, l_aLIB, l_aLIBA, l_aLIBRAW, NIL, cLibExt )
      IF hbmk[ _HBMK_lSHARED ] .AND. ! Empty( l_aLIBSHARED )
         l_aLIBRAW := ArrayJoin( l_aLIBSHARED, l_aLIBRAW )
         ListCookLib( hbmk, l_aLIB, l_aLIBA, l_aLIBSHARED, NIL )
      ENDIF
      /* Dress obj names. */
      l_aOBJ := ListDirExt( ArrayJoin( hbmk[ _HBMK_aPRG ], hbmk[ _HBMK_aC ] ), cWorkDir, cObjExt )
      hbmk[ _HBMK_aOBJUSER ] := ListCook( hbmk[ _HBMK_aOBJUSER ], cObjExt )

      IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
         l_aRESSRC_TODO := {}
         FOR EACH tmp IN hbmk[ _HBMK_aRESSRC ]
            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: RESSRC %1$s %2$s", tmp, FN_DirExtSet( tmp, cWorkDir, cResExt ) ) )
            ENDIF
            IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cResExt ), @tmp2 ) .OR. ;
               ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
               tmp1 > tmp2 .OR. ;
               ( hbmk[ _HBMK_nHEAD ] != _HEAD_OFF .AND. FindNewerHeaders( hbmk, tmp, NIL, tmp2, .F., .T., @headstate ) )
               AAdd( l_aRESSRC_TODO, tmp )
            ENDIF
         NEXT
      ELSE
         l_aRESSRC_TODO := hbmk[ _HBMK_aRESSRC ]
      ENDIF

      IF hbmk[ _HBMK_nHBMODE ] != _HBMODE_RAW_C
         IF hbmk[ _HBMK_lREBUILDPO ]
            IF ! Empty( hbmk[ _HBMK_cPO ] ) .AND. ! Empty( hbmk[ _HBMK_aPRG ] )
               RebuildPO( hbmk, ListDirExt( hbmk[ _HBMK_aPRG ], cWorkDir, ".pot" ) )
            ENDIF
         ELSE
            IF ! Empty( hbmk[ _HBMK_cPO ] ) .AND. Len( l_aPRG_TODO ) > 0
               UpdatePO( hbmk, ListDirExt( l_aPRG_TODO, cWorkDir, ".pot" ) )
            ENDIF
         ENDIF

         IF Len( hbmk[ _HBMK_aPO ] ) > 0 .AND. hbmk[ _HBMK_cHBL ] != NIL .AND. ! l_lCLEAN

            /* Combine target dir with .hbl output name. */

            hb_FNameSplit( l_cPROGNAME, @tmp )
            IF Empty( tmp )
               hbmk[ _HBMK_cHBL ] := PathProc( hbmk[ _HBMK_cHBL ], hbmk[ _HBMK_cHBLDir ] )
            ELSE
               hbmk[ _HBMK_cHBL ] := PathProc( hbmk[ _HBMK_cHBL ], tmp )
            ENDIF

            MakeHBL( hbmk, hbmk[ _HBMK_cHBL ] )
         ENDIF
      ENDIF

      IF Len( l_aRESSRC_TODO ) > 0 .AND. ! Empty( cBin_Res ) .AND. ! l_lCLEAN

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, I_( "Compiling resources..." ) )
         ENDIF

         /* Compiling resource */

         nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Res, nScr_Esc, nCmd_Esc )

         cOpt_Res := StrTran( cOpt_Res, "{FR}"  , GetEnv( "HB_USER_RESFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTRES ] ) )
         cOpt_Res := StrTran( cOpt_Res, "{DI}"  , FN_Escape( l_cHB_INC_INSTALL, nOpt_Esc ) )

         IF "{IR}" $ cOpt_Res

            FOR EACH tmp IN l_aRESSRC_TODO

               cCommand := cOpt_Res
               cCommand := StrTran( cCommand, "{IR}", FN_Escape( tmp, nOpt_Esc ) )
               cCommand := StrTran( cCommand, "{OS}", FN_Escape( PathSepToTarget( hbmk, FN_DirExtSet( tmp, cWorkDir, cResExt ) ), nOpt_Esc ) )

               cCommand := cBin_Res + " " + AllTrim( cCommand )

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Resource compiler command:" ) )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hbmk_run( hbmk, cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running resource compiler. %1$s" ), hb_ntos( tmp1 ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
                  nErrorLevel := 6
                  EXIT
               ENDIF
            NEXT
         ELSE
            cOpt_Res := StrTran( cOpt_Res, "{LR}"  , ArrayToList( l_aRESSRC_TODO,, nOpt_Esc ) )

            cOpt_Res := AllTrim( cOpt_Res )

            /* Handle moving the whole command line to a script, if requested. */
            IF "{SCRIPT}" $ cOpt_Res
               fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
               IF fhnd != F_ERROR
                  FWrite( fhnd, StrTran( cOpt_Res, "{SCRIPT}" ) )
                  FClose( fhnd )
                  cOpt_Res := "@" + cScriptFile
               ELSE
                  hbmk_OutErr( hbmk, I_( "Warning: Resource compiler script couldn't be created, continuing in command line." ) )
               ENDIF
            ENDIF

            cCommand := cBin_Res + " " + cOpt_Res

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, I_( "Resource compiler command:" ) )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
               IF ! Empty( cScriptFile )
                  hbmk_OutStd( hbmk, I_( "Resource compiler script:" ) )
                  OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
               ENDIF
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running resource compiler. %1$s" ), hb_ntos( tmp ) ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + hb_osNewLine() )
               ENDIF
               nErrorLevel := 8
            ENDIF

            IF ! Empty( cScriptFile )
               FErase( cScriptFile )
            ENDIF
         ENDIF
      ENDIF

      IF nErrorLevel == 0

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]
            l_aPRG_TODO := {}
            l_aPRG_DONE := {}
            FOR EACH tmp IN hbmk[ _HBMK_aPRG ]
               IF hbmk[ _HBMK_lDEBUGINC ]
                  hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: CPRG %1$s %2$s",;
                     FN_DirExtSet( tmp, cWorkDir, ".c" ),;
                     FN_DirExtSet( tmp, cWorkDir, cObjExt ) ) )
               ENDIF
               IF ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, ".c" ), @tmp1 ) .OR. ;
                  ! hb_FGetDateTime( FN_DirExtSet( tmp, cWorkDir, cObjExt ), @tmp2 ) .OR. ;
                  tmp1 > tmp2
                  AAdd( l_aPRG_TODO, tmp )
               ELSE
                  AAdd( l_aPRG_DONE, tmp )
               ENDIF
            NEXT
         ELSE
            l_aPRG_TODO := hbmk[ _HBMK_aPRG ]
            l_aPRG_DONE := {}
         ENDIF
      ENDIF

      IF nErrorLevel == 0 .AND. ( Len( l_aPRG_TODO ) + Len( l_aC_TODO ) + iif( Empty( cBin_Link ), Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ), 0 ) ) > 0 .AND. ! l_lCLEAN

         IF ! Empty( cBin_CompC )

            IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
               hbmk_OutStd( hbmk, I_( "Compiling..." ) )
            ENDIF

            /* Compiling */

            nOpt_Esc := iif( "{SCRIPT}" $ cOpt_CompC, nScr_Esc, nCmd_Esc )

            /* Order is significant */
            cOpt_CompC := StrTran( cOpt_CompC, "{FC}"  , iif( l_lBLDFLGC, cSelfFlagC + " ", "" ) +;
                                                         GetEnv( "HB_USER_CFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTC ] ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{FL}"  , iif( l_lBLDFLGL, cSelfFlagL + " ", "" ) +;
                                                         GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTL ] ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LR}"  , ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], cWorkDir, cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LO}"  , ArrayToList( ArrayAJoin( { hbmk[ _HBMK_aOBJUSER ], ListCook( l_aPRG_DONE, cObjExt ), ListCook( l_aC_DONE, cObjExt ) } ),, nOpt_Esc, cObjPrefix ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LS}"  , ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], "", cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, cResPrefix ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LA}"  , ArrayToList( l_aOBJA,, nOpt_Esc ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, cLibPrefix ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OD}"  , FN_Escape( PathSepToTarget( hbmk, FN_DirGet( l_cPROGNAME ) ), nOpt_Esc ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OE}"  , FN_Escape( PathSepToTarget( hbmk, l_cPROGNAME ), nOpt_Esc ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{OM}"  , FN_Escape( PathSepToTarget( hbmk, FN_ExtSet( l_cPROGNAME, ".map" ) ), nOpt_Esc ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, cLibPathPrefix ) )
            cOpt_CompC := StrTran( cOpt_CompC, "{DB}"  , l_cHB_BIN_INSTALL )
            cOpt_CompC := StrTran( cOpt_CompC, "{DI}"  , FN_Escape( l_cHB_INC_INSTALL, nOpt_Esc ) )

            IF "{IC}" $ cOpt_CompC

               aThreads := {}
               FOR EACH aTODO IN ArraySplit( ArrayJoin( ListDirExt( l_aPRG_TODO, cWorkDir, ".c" ), l_aC_TODO ), l_nJOBS )
                  IF hb_mtvm() .AND. Len( aTODO:__enumBase() ) > 1
                     AAdd( aThreads, hb_threadStart( @CompileCLoop(), hbmk, aTODO, cBin_CompC, cOpt_CompC, cWorkDir, cObjExt, nOpt_Esc, aTODO:__enumIndex(), Len( aTODO:__enumBase() ) ) )
                  ELSE
                     IF ! CompileCLoop( hbmk, aTODO, cBin_CompC, cOpt_CompC, cWorkDir, cObjExt, nOpt_Esc, 0, 0 )
                        nErrorLevel := 6
                     ENDIF
                  ENDIF
               NEXT

               IF hb_mtvm() .AND. Len( aThreads ) > 1
                  FOR EACH thread IN aThreads
                     hb_threadJoin( thread, @tmp )
                     IF ! tmp
                        nErrorLevel := 6
                     ENDIF
                  NEXT
               ENDIF
            ELSE
               cOpt_CompC := StrTran( cOpt_CompC, "{OO}"  , FN_Escape( PathSepToTarget( hbmk, FN_ExtSet( l_cPROGNAME, cObjExt ) ), nOpt_Esc ) )
               cOpt_CompC := StrTran( cOpt_CompC, "{OW}"  , FN_Escape( PathSepToTarget( hbmk, cWorkDir ), nOpt_Esc ) )

               aThreads := {}
               FOR EACH aTODO IN ArraySplit( ArrayJoin( ListDirExt( l_aPRG_TODO, cWorkDir, ".c" ), l_aC_TODO ), l_nJOBS )

                  cOpt_CompCLoop := AllTrim( StrTran( cOpt_CompC, "{LC}"  , ArrayToList( aTODO,, nOpt_Esc ) ) )

                  /* Handle moving the whole command line to a script, if requested. */
                  IF "{SCRIPT}" $ cOpt_CompCLoop
                     fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".cpl" )
                     IF fhnd != F_ERROR
                        FWrite( fhnd, StrTran( cOpt_CompCLoop, "{SCRIPT}" ) )
                        FClose( fhnd )
                        cOpt_CompCLoop := "@" + cScriptFile
                     ELSE
                        hbmk_OutErr( hbmk, I_( "Warning: C compiler script couldn't be created, continuing in command line." ) )
                     ENDIF
                  ENDIF

                  cCommand := cBin_CompC + " " + cOpt_CompCLoop

                  IF hbmk[ _HBMK_lTRACE ]
                     IF ! hbmk[ _HBMK_lQuiet ]
                        IF Len( aTODO:__enumBase() ) > 1
                           hbmk_OutStd( hbmk, hb_StrFormat( I_( "C compiler command job #%1$s:" ), hb_ntos( aTODO:__enumIndex() ) ) )
                        ELSE
                           hbmk_OutStd( hbmk, I_( "C compiler command:" ) )
                        ENDIF
                     ENDIF
                     OutStd( cCommand + hb_osNewLine() )
                     IF ! Empty( cScriptFile )
                        hbmk_OutStd( hbmk, I_( "C compiler script:" ) )
                        OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                     ENDIF
                  ENDIF

                  IF ! hbmk[ _HBMK_lDONTEXEC ]
                     IF hb_mtvm() .AND. Len( aTODO:__enumBase() ) > 1
                        AAdd( aThreads, { hb_threadStart( @hbmk_run(), hbmk, cCommand ), cCommand } )
                     ELSE
                        IF ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
                           hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C compiler. %1$s" ), hb_ntos( tmp ) ) )
                           IF ! hbmk[ _HBMK_lQuiet ]
                              OutErr( cCommand + hb_osNewLine() )
                           ENDIF
                           nErrorLevel := 6
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
                           hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C compiler job #%1$s. %2$s" ), hb_ntos( thread:__enumIndex() ), hb_ntos( tmp ) ) )
                        ELSE
                           hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C compiler. %1$s" ), hb_ntos( tmp ) ) )
                        ENDIF
                        IF ! hbmk[ _HBMK_lQuiet ]
                           OutErr( thread[ 2 ] + hb_osNewLine() )
                        ENDIF
                        nErrorLevel := 6
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ELSE
            hbmk_OutErr( hbmk, I_( "Error: This architecture/compiler isn't implemented." ) )
            nErrorLevel := 8
         ENDIF
      ENDIF

      IF nErrorLevel == 0

         lTargetUpToDate := .F.

         IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lREBUILD ]

            IF hbmk[ _HBMK_lDEBUGINC ]
               hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: target %1$s", l_cPROGNAME ) )
            ENDIF

            IF hb_FGetDateTime( l_cPROGNAME, @tTarget )

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
                     IF ! Empty( tmp2 := FindLib( hbmk, tmp, hbmk[ _HBMK_aLIBPATH ], cLibExt ) )
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

      IF nErrorLevel == 0 .AND. ( Len( l_aOBJ ) + Len( hbmk[ _HBMK_aOBJUSER ] ) + Len( l_aOBJA ) ) > 0 .AND. ! l_lCLEAN

         IF lTargetUpToDate
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Target up to date: %1$s" ), l_cPROGNAME ) )
         ELSE
            IF ! DirBuild( FN_DirGet( l_cPROGNAME ) )
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create directory for target '%1$s'." ), l_cPROGNAME ) )
            ENDIF

            IF hbmk[ _HBMK_lREBUILD ] .OR. ;
               ( ! hbmk[ _HBMK_lINC ] .AND. lStopAfterCComp .AND. lCreateLib .AND. ! Empty( cBin_Lib ) ) /* non-incremental + static lib */
               IF hb_FileExists( PathSepToTarget( hbmk, l_cPROGNAME ) ) .AND. ;
                  FErase( PathSepToTarget( hbmk, l_cPROGNAME ) ) == F_ERROR
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot delete existing target '%1$s'." ), l_cPROGNAME ) )
               ENDIF
            ENDIF

            DO CASE
            CASE ! lStopAfterCComp .AND. ! Empty( cBin_Link )

               IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Linking... %1$s" ), PathSepToTarget( hbmk, l_cPROGNAME ) ) )
               ENDIF

               /* Linking */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Link, nScr_Esc, nCmd_Esc )

               /* Order is significant */
               cOpt_Link := StrTran( cOpt_Link, "{FL}"  , iif( l_lBLDFLGL, cSelfFlagL + " ", "" ) +;
                                                          GetEnv( "HB_USER_LDFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTL ] ) )
               cOpt_Link := StrTran( cOpt_Link, "{LO}"  , ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, cObjPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{LS}"  , ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], cWorkDir, cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, cResPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{LA}"  , ArrayToList( l_aOBJA,, nOpt_Esc ) )
               cOpt_Link := StrTran( cOpt_Link, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, cLibPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc ) )
               cOpt_Link := StrTran( cOpt_Link, "{OE}"  , FN_Escape( PathSepToTarget( hbmk, l_cPROGNAME ), nOpt_Esc ) )
               cOpt_Link := StrTran( cOpt_Link, "{OM}"  , FN_Escape( PathSepToTarget( hbmk, FN_ExtSet( l_cPROGNAME, ".map" ) ), nOpt_Esc ) )
               cOpt_Link := StrTran( cOpt_Link, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, cLibPathPrefix ) )
               cOpt_Link := StrTran( cOpt_Link, "{DB}"  , l_cHB_BIN_INSTALL )

               cOpt_Link := AllTrim( cOpt_Link )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_Link
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Link, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Link := "@" + cScriptFile
                  ELSE
                     hbmk_OutErr( hbmk, I_( "Warning: Link script couldn't be created, continuing in command line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Link + " " + cOpt_Link

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Linker command:" ) )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     hbmk_OutStd( hbmk, I_( "Linker script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running linker. %1$s" ), hb_ntos( tmp ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

               IF nErrorLevel == 0 .AND. hbmk[ _HBMK_lGUI ] .AND. hbmk[ _HBMK_cARCH ] == "darwin"
                  /* Build app bundle for OS X GUI apps. (experimental) */
                  tmp := FN_DirGet( l_cPROGNAME )
                  IF ! Empty( tmp )
                     tmp += hb_osPathSeparator()
                  ENDIF
                  tmp += FN_NameGet( l_cPROGNAME ) + ".app" + hb_osPathSeparator() + "Contents"
                  IF DirBuild( tmp + hb_osPathSeparator() + "MacOS" )
                     hb_FCopy( l_cPROGNAME, tmp + hb_osPathSeparator() + "MacOS" + hb_osPathSeparator() + FN_NameGet( l_cPROGNAME ) )
                     IF ! hb_FileExists( tmp + hb_osPathSeparator() + "Info.plist" )
                        hb_MemoWrit( tmp + hb_osPathSeparator() + "Info.plist", MacOSXFiles( hbmk, 1, FN_NameGet( l_cPROGNAME ) ) )
                     ENDIF
                     IF ! hb_FileExists( tmp + hb_osPathSeparator() + "PkgInfo" )
                        hb_MemoWrit( tmp + hb_osPathSeparator() + "PkgInfo", MacOSXFiles( hbmk, 2, FN_NameGet( l_cPROGNAME ) ) )
                     ENDIF
                  ENDIF
               ENDIF

            CASE lStopAfterCComp .AND. lCreateLib .AND. ! Empty( cBin_Lib )

               IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating static library... %1$s" ), PathSepToTarget( hbmk, l_cPROGNAME ) ) )
               ENDIF

               /* Lib creation (static) */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Lib, nScr_Esc, nCmd_Esc )

               /* Order is significant */
               cOpt_Lib := StrTran( cOpt_Lib, "{FA}"  , GetEnv( "HB_USER_AFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTA ] ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LO}"  , ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, cLibObjPrefix ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, cLibPrefix ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{OL}"  , FN_Escape( PathSepToTarget( hbmk, l_cPROGNAME ), nOpt_Esc ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, cLibPathPrefix ) )
               cOpt_Lib := StrTran( cOpt_Lib, "{DB}"  , l_cHB_BIN_INSTALL )

               cOpt_Lib := AllTrim( cOpt_Lib )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_Lib
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Lib, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Lib := "@" + cScriptFile
                  ELSE
                     hbmk_OutErr( hbmk, I_( "Warning: Lib script couldn't be created, continuing in command line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Lib + " " + cOpt_Lib

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Lib command:" ) )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     hbmk_OutStd( hbmk, I_( "Lib script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running lib command. %1$s" ), hb_ntos( tmp ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            CASE lStopAfterCComp .AND. lCreateDyn .AND. ! Empty( cBin_Dyn )

               IF hbmk[ _HBMK_lINC ] .AND. ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Creating dynamic library... %1$s" ), PathSepToTarget( hbmk, l_cPROGNAME ) ) )
               ENDIF

               /* Lib creation (dynamic) */

               nOpt_Esc := iif( "{SCRIPT}" $ cOpt_Dyn, nScr_Esc, nCmd_Esc )

               /* Order is significant */
               cOpt_Dyn := StrTran( cOpt_Dyn, "{FD}"  , GetEnv( "HB_USER_DFLAGS" ) + " " + ArrayToList( hbmk[ _HBMK_aOPTD ] ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LO}"  , ArrayToList( ArrayJoin( l_aOBJ, hbmk[ _HBMK_aOBJUSER ] ),, nOpt_Esc, cDynObjPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LS}"  , ArrayToList( ArrayJoin( ListDirExt( hbmk[ _HBMK_aRESSRC ], cWorkDir, cResExt ), hbmk[ _HBMK_aRESCMP ] ),, nOpt_Esc, cResPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LL}"  , ArrayToList( l_aLIB,, nOpt_Esc, cLibPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{LB}"  , ArrayToList( l_aLIBA,, nOpt_Esc ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OD}"  , FN_Escape( PathSepToTarget( hbmk, l_cPROGNAME ), nOpt_Esc ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{OM}"  , FN_Escape( PathSepToTarget( hbmk, FN_ExtSet( l_cPROGNAME, ".map" ) ), nOpt_Esc ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{DL}"  , ArrayToList( hbmk[ _HBMK_aLIBPATH ], cLibPathSep, nOpt_Esc, cLibPathPrefix ) )
               cOpt_Dyn := StrTran( cOpt_Dyn, "{DB}"  , l_cHB_BIN_INSTALL )

               cOpt_Dyn := AllTrim( cOpt_Dyn )

               /* Handle moving the whole command line to a script, if requested. */
               IF "{SCRIPT}" $ cOpt_Dyn
                  fhnd := hb_FTempCreateEx( @cScriptFile, NIL, NIL, ".lnk" )
                  IF fhnd != F_ERROR
                     FWrite( fhnd, StrTran( cOpt_Dyn, "{SCRIPT}" ) )
                     FClose( fhnd )
                     cOpt_Dyn := "@" + cScriptFile
                  ELSE
                     hbmk_OutErr( hbmk, I_( "Warning: Dynamic lib link script couldn't be created, continuing in command line." ) )
                  ENDIF
               ENDIF

               cCommand := cBin_Dyn + " " + cOpt_Dyn

               IF hbmk[ _HBMK_lTRACE ]
                  IF ! hbmk[ _HBMK_lQuiet ]
                     hbmk_OutStd( hbmk, I_( "Dynamic lib link command:" ) )
                  ENDIF
                  OutStd( cCommand + hb_osNewLine() )
                  IF ! Empty( cScriptFile )
                     hbmk_OutStd( hbmk, I_( "Dynamic lib link script:" ) )
                     OutStd( hb_MemoRead( cScriptFile ) + hb_osNewLine() )
                  ENDIF
               ENDIF

               IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running dynamic lib link command. %1$s" ), hb_ntos( tmp ) ) )
                  IF ! hbmk[ _HBMK_lQuiet ]
                     OutErr( cCommand + hb_osNewLine() )
                  ENDIF
                  nErrorLevel := 7
               ENDIF

               IF ! Empty( cScriptFile )
                  FErase( cScriptFile )
               ENDIF

            ENDCASE
         ENDIF
      ENDIF

      /* Cleanup */

      IF ! Empty( l_cCSTUB )
         FErase( l_cCSTUB )
         FErase( FN_DirExtSet( l_cCSTUB, cWorkDir, cObjExt ) )
      ENDIF
      IF ! hbmk[ _HBMK_lINC ] .OR. l_lCLEAN
         AEval( ListDirExt( hbmk[ _HBMK_aPRG ], cWorkDir, ".c" ), {|tmp| FErase( tmp ) } )
      ENDIF
      IF ! lStopAfterCComp .OR. lCreateLib .OR. lCreateDyn
         IF ! hbmk[ _HBMK_lINC ] .OR. l_lCLEAN
            IF ! Empty( cResExt )
               AEval( ListDirExt( hbmk[ _HBMK_aRESSRC ], cWorkDir, cResExt ), {|tmp| FErase( tmp ) } )
            ENDIF
            AEval( l_aOBJ, {|tmp| FErase( tmp ) } )
         ENDIF
      ENDIF
      AEval( l_aCLEAN, {|tmp| FErase( tmp ) } )
      IF l_lCLEAN
         DirUnbuild( cWorkDir )
      ENDIF

      IF nErrorLevel == 0 .AND. ! l_lCLEAN .AND. ! lTargetUpToDate

         IF ! Empty( cBin_Post )

            cOpt_Post := StrTran( cOpt_Post, "{OB}", PathSepToTarget( hbmk, l_cPROGNAME ) )
            cOpt_Post := AllTrim( cOpt_Post )

            cCommand := cBin_Post + " " + cOpt_Post

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, I_( "Post processor command:" ) )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running post processor command. %1$s:" ), hb_ntos( tmp ) ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + hb_osNewLine() )
               ENDIF
            ENDIF
         ENDIF

         /* Setup compressor for host platform */

         #if defined( __PLATFORM__LINUX )

            cBin_Cprs := "upx"
            cOpt_Cprs := "{OB}"
            cOpt_CprsMin := "-1"
            cOpt_CprsMax := "-9"

         #elif defined( __PLATFORM__WINDOWS ) .OR. ;
               defined( __PLATFORM__DOS )

            cBin_Cprs := "upx.exe"
            cOpt_Cprs := "{OB}"
            cOpt_CprsMin := "-1"
            cOpt_CprsMax := "-9"

         #else

            cBin_Cprs := NIL
            cOpt_Cprs := ""
            cOpt_CprsMin := ""
            cOpt_CprsMax := ""

         #endif

         IF hbmk[ _HBMK_nCOMPR ] != _COMPR_OFF .AND. ! lCreateLib .AND. ! Empty( cBin_Cprs )

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

            cOpt_Cprs := StrTran( cOpt_Cprs, "{OB}", PathSepToTarget( hbmk, l_cPROGNAME ) )
            cOpt_Cprs := AllTrim( cOpt_Cprs )

            cCommand := cBin_Cprs + " " + cOpt_Cprs

            IF hbmk[ _HBMK_lTRACE ]
               IF ! hbmk[ _HBMK_lQuiet ]
                  hbmk_OutStd( hbmk, I_( "Compression command:" ) )
               ENDIF
               OutStd( cCommand + hb_osNewLine() )
            ENDIF

            IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp := hbmk_run( hbmk, cCommand ) ) != 0
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Running compression command. %1$s:" ), hb_ntos( tmp ) ) )
               IF ! hbmk[ _HBMK_lQuiet ]
                  OutErr( cCommand + hb_osNewLine() )
               ENDIF
            ENDIF
         ENDIF

         IF ! Empty( hbmk[ _HBMK_aINSTPATH ] )
            FOR EACH tmp IN hbmk[ _HBMK_aINSTPATH ]
               IF Empty( FN_NameExtGet( tmp ) )
                  tmp1 := DirAddPathSep( PathSepToSelf( tmp ) ) + FN_NameExtGet( l_cPROGNAME )
               ELSE
                  tmp1 := PathSepToSelf( tmp )
               ENDIF
               IF DirBuild( FN_DirGet( tmp1 ) )
                  IF hb_FCopy( l_cPROGNAME, tmp1 ) == F_ERROR
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Copying target to %1$s failed with %2$s." ), tmp1, hb_ntos( FError() ) ) )
                  ELSEIF hbmk[ _HBMK_lInfo ]
                     hbmk_OutStd( hbmk, hb_StrFormat( I_( "Copied target to %1$s" ), tmp1 ) )
                  ENDIF
               ELSE
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot create directory for target install %1$s." ), tmp1 ) )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   IF hbmk[ _HBMK_lDEBUGTIME ]
      hbmk_OutStd( hbmk, hb_StrFormat( I_( "Running time: %1$ss" ), hb_ntos( TimeElapsed( nStart, Seconds() ) ) ) )
   ENDIF

   IF ! lSkipBuild .AND. hbmk[ _HBMK_lBEEP ]
      DoBeep( hbmk, nErrorLevel == 0 )
   ENDIF

   IF ! lStopAfterHarbour .AND. ! lStopAfterCComp .AND. ;
      ! lCreateLib .AND. ! lCreateDyn .AND. ;
      nErrorLevel == 0 .AND. ! l_lCLEAN .AND. hbmk[ _HBMK_lRUN ]
      cCommand := l_cPROGNAME
      #if defined( __PLATFORM__UNIX )
         IF Empty( FN_DirGet( l_cPROGNAME ) )
            cCommand := "." + hb_osPathSeparator() + l_cPROGNAME
         ENDIF
      #endif
      cCommand := PathSepToTarget( hbmk, cCommand )
      #if defined( __PLATFORM__WINDOWS )
         IF hbmk[ _HBMK_lGUI ]
            IF hb_osIsWinNT()
               cCommand := 'start "" ' + FN_Escape( cCommand, _ESC_DBLQUOTE )
            ELSE
               cCommand := "start " + cCommand
            ENDIF
         ENDIF
      #elif defined( __PLATFORM__DARWIN )
         IF hbmk[ _HBMK_lGUI ]
            cCommand := "open " + FN_Escape( cCommand + ".app", _ESC_NIX )
         ENDIF
      #endif
      cCommand := AllTrim( cCommand + " " + ArrayToList( l_aOPTRUN ) )
      IF hbmk[ _HBMK_lTRACE ]
         IF ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, I_( "Running executable:" ) )
         ENDIF
         OutStd( cCommand + hb_osNewLine() )
      ENDIF
      IF ! hbmk[ _HBMK_lDONTEXEC ]
         nErrorLevel := hb_run( cCommand )
      ENDIF
   ENDIF

   RETURN nErrorLevel

STATIC PROCEDURE DoBeep( hbmk, lSuccess )
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

STATIC FUNCTION CompileCLoop( hbmk, aTODO, cBin_CompC, cOpt_CompC, cWorkDir, cObjExt, nOpt_Esc, nJob, nJobs )
   LOCAL lResult := .T.
   LOCAL cCommand
   LOCAL tmp, tmp1

   FOR EACH tmp IN aTODO

      cCommand := cOpt_CompC
      cCommand := StrTran( cCommand, "{IC}", FN_Escape( tmp, nOpt_Esc ) )
      cCommand := StrTran( cCommand, "{OO}", FN_Escape( PathSepToTarget( hbmk, FN_DirExtSet( tmp, cWorkDir, cObjExt ) ), nOpt_Esc ) )

      cCommand := cBin_CompC + " " + AllTrim( cCommand )

      IF hbmk[ _HBMK_lTRACE ]
         IF ! hbmk[ _HBMK_lQuiet ]
            IF nJobs > 1
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "C compiler command job #%1$s:" ), hb_ntos( nJob ) ) )
            ELSE
               hbmk_OutStd( hbmk, I_( "C compiler command:" ) )
            ENDIF
         ENDIF
         OutStd( cCommand + hb_osNewLine() )
      ENDIF

      IF ! hbmk[ _HBMK_lDONTEXEC ] .AND. ( tmp1 := hbmk_run( hbmk, cCommand ) ) != 0
         IF nJobs > 1
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C compiler job #%1$s. %2$s" ), hb_ntos( nJob ), hb_ntos( tmp1 ) ) )
         ELSE
            hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running C compiler. %1$s" ), hb_ntos( tmp1 ) ) )
         ENDIF
         IF ! hbmk[ _HBMK_lQuiet ]
            OutErr( cCommand + hb_osNewLine() )
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
      CASE "gtpca"
      CASE "gtsln"
      CASE "gtstd"
      CASE "gtwin"
         lGUI := .F.
         EXIT

      CASE "gtgui"
      CASE "gtwvt"
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
   order to work, it will need #include "filename" format in source.
   If this isn't enough for your needs, feel free to update the code.
   [vszakats] */

#define _HEADSTATE_hFiles       1
#define _HEADSTATE_lAnyNewer    2
#define _HEADSTATE_MAX_         2

STATIC FUNCTION FindNewerHeaders( hbmk, cFileName, cParentDir, tTimeParent, lIncTry, lCMode, /* @ */ headstate, nNestingLevel )
   LOCAL cFile
   LOCAL fhnd
   LOCAL nPos
   LOCAL tTimeSelf
   LOCAL tmp
   LOCAL cNameExtL
   LOCAL cExt
   LOCAL cHeader

   STATIC l_aExcl := { "windows.h", "ole2.h", "os2.h" }

   DEFAULT nNestingLevel TO 1
   DEFAULT cParentDir TO FN_DirGet( cFileName )

   IF nNestingLevel == 1
      headstate := Array( _HEADSTATE_MAX_ )
      headstate[ _HEADSTATE_hFiles ] := hb_Hash()
      headstate[ _HEADSTATE_lAnyNewer ] := .F.
   ENDIF

   IF ! lIncTry .AND. hbmk[ _HBMK_nHEAD ] == _HEAD_OFF
      RETURN .F.
   ENDIF

   IF nNestingLevel > _HBMK_HEAD_NEST_MAX
      RETURN .F.
   ENDIF

   /* Don't spend time on some known headers */
   cNameExtL := Lower( FN_NameExtGet( cFileName ) )
   IF AScan( l_aExcl, { |tmp| Lower( tmp ) == cNameExtL } ) > 0
      RETURN .F.
   ENDIF

   cFileName := FindHeader( hbmk, cFileName, cParentDir, iif( lIncTry, hbmk[ _HBMK_aINCTRYPATH ], NIL ) )
   IF Empty( cFileName )
      RETURN .F.
   ENDIF

   IF hb_HPos( headstate[ _HEADSTATE_hFiles ], cFileName ) > 0
      RETURN .F.
   ENDIF
   hb_HSet( headstate[ _HEADSTATE_hFiles ], cFileName, .T. )

   IF hbmk[ _HBMK_lDEBUGINC ]
      hbmk_OutStd( hbmk, hb_StrFormat( "debuginc: HEADER %1$s", cFileName ) )
   ENDIF

   IF ! hb_FGetDateTime( cFileName, @tTimeSelf )
      RETURN .F.
   ENDIF

   IF tTimeParent != NIL .AND. tTimeSelf > tTimeParent
      headstate[ _HEADSTATE_lAnyNewer ] := .T.
      /* Let it continue if we want to scan for header locations */
      IF ! lIncTry
         RETURN .T.
      ENDIF
   ENDIF

   cExt := Lower( FN_ExtGet( cFileName ) )

   /* Filter out non-source format inputs for MinGW / windres */
   IF hbmk[ _HBMK_cCOMP ] $ "gcc|mingw|mingw64|mingwarm|cygwin" .AND. hbmk[ _HBMK_cARCH ] $ "win|wce" .AND. cExt == ".res"
      RETURN .F.
   ENDIF

   /* TODO: Add filter based on extension to avoid binary files */

   /* NOTE: Beef up this section if you need a more intelligent source
            parser. Notice that this code is meant to process both
            .prg, .c and .res sources. Please try to keep it simple,
            as speed and maintainability is also important. [vszakats] */

   IF lIncTry .OR. hbmk[ _HBMK_nHEAD ] == _HEAD_FULL
      cFile := MemoRead( cFileName )
   ELSE
      IF ( fhnd := FOpen( cFileName, FO_READ + FO_SHARED ) ) == F_ERROR
         RETURN .F.
      ENDIF
      cFile := Space( 16384 )
      FRead( fhnd, @cFile, Len( cFile ) )
      FClose( fhnd )
   ENDIF

   cHeader := NIL
   nPos := 1
   DO WHILE .T.

      IF ( tmp := hb_At( '#include "', cFile, nPos ) ) > 0
         nPos := tmp + Len( '#include "' )
         IF ( tmp := hb_At( '"', cFile, nPos ) ) > 0
            cHeader := SubStr( cFile, nPos, tmp - nPos )
         ENDIF
      ELSE
         EXIT
      ENDIF

      IF cHeader != NIL .AND. ;
         FindNewerHeaders( hbmk, cHeader, iif( lCMode, FN_DirGet( cFileName ), cParentDir ), tTimeParent, lIncTry, lCMode, @headstate, nNestingLevel + 1 )
         headstate[ _HEADSTATE_lAnyNewer ] := .T.
         /* Let it continue if we want to scan for header locations */
         IF ! lIncTry
            RETURN .T.
         ENDIF
         cHeader := NIL
      ENDIF
   ENDDO

   RETURN headstate[ _HEADSTATE_lAnyNewer ]

STATIC FUNCTION FindHeader( hbmk, cFileName, cParentDir, aINCTRYPATH )
   LOCAL cDir

   /* Check in current dir */
   IF hb_FileExists( cFileName )
      RETURN cFileName
   ENDIF

   /* Check in parent dir */

   IF hb_FileExists( DirAddPathSep( PathSepToSelf( cParentDir ) ) + cFileName )
      RETURN DirAddPathSep( PathSepToSelf( cParentDir ) ) + cFileName
   ENDIF

   /* Check in include path list specified via -incpath options */
   FOR EACH cDir IN hbmk[ _HBMK_aINCPATH ]
      IF hb_FileExists( DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName )
         RETURN DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName
      ENDIF
   NEXT

   /* Check in potential include path list */
   IF ! Empty( aINCTRYPATH )
      FOR EACH cDir IN aINCTRYPATH
         IF hb_FileExists( DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName )
            /* Add these dir to include paths */
            IF AScan( hbmk[ _HBMK_aINCPATH ], { |tmp| tmp == cDir } ) == 0
               AAdd( hbmk[ _HBMK_aINCPATH ], cDir )
               IF hbmk[ _HBMK_lInfo ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Autodetected header dir for %1$s: %2$s" ), cFileName, cDir ) )
               ENDIF
            ENDIF
            RETURN DirAddPathSep( PathSepToSelf( cDir ) ) + cFileName
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

/* Replicating logic used by compilers. */

STATIC FUNCTION FindLib( hbmk, cLib, aLIBPATH, cLibExt )
   LOCAL cDir
   LOCAL tmp

   /* Check libs in their full paths */
   IF hbmk[ _HBMK_cCOMP ] $ "msvc|msvc64|msvcarm|bcc|pocc|pocc64|poccarm|watcom"
      IF ! Empty( FN_DirGet( cLib ) )
         IF hb_FileExists( cLib := FN_ExtSet( cLib, cLibExt ) )
            RETURN cLib
         ENDIF
         IF hbmk[ _HBMK_cCOMP ] $ "pocc|pocc64|poccarm"
            IF hb_FileExists( cLib := FN_ExtSet( cLib, ".a" ) )
               RETURN cLib
            ENDIF
         ENDIF
         RETURN NIL
      ENDIF
   ENDIF

   /* Check in current dir */
   IF hbmk[ _HBMK_cCOMP ] $ "msvc|msvc64|msvcarm|bcc|pocc|pocc64|poccarm|watcom"
      IF ! Empty( tmp := LibExists( hbmk, "", cLib, cLibExt ) )
         RETURN tmp
      ENDIF
   ENDIF

   /* Check in libpaths */
   FOR EACH cDir IN aLIBPATH
      IF ! Empty( cDir )
         IF ! Empty( tmp := LibExists( hbmk, cDir, cLib, cLibExt ) )
            RETURN tmp
         ENDIF
      ENDIF
   NEXT

#if 0
   /* Check in certain other compiler specific locations. */
   IF hbmk[ _HBMK_cCOMP ] $ "msvc|msvc64|msvcarm"
      FOR EACH cDir IN hb_ATokens( GetEnv( "LIB" ), hb_osPathListSeparator(), .T., .T. )
         IF ! Empty( cDir )
            IF ! Empty( tmp := LibExists( hbmk, cDir, cLib, cLibExt ) )
               RETURN tmp
            ENDIF
         ENDIF
      NEXT
   ENDIF
#endif

   RETURN NIL

STATIC FUNCTION LibExists( hbmk, cDir, cLib, cLibExt )
   LOCAL tmp

   cDir := DirAddPathSep( PathSepToSelf( cDir ) )

   DO CASE
   CASE hbmk[ _HBMK_cCOMP ] $ "gcc|mingw|mingw64|mingwarm|cygwin" .AND. hbmk[ _HBMK_cARCH ] $ "win|wce"
      /* NOTE: ld/gcc option -dll-search-prefix isn't taken into account here,
               So, '<prefix>xxx.dll' format libs won't be found by hbmk. */
      DO CASE
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, ".dll.a" ) ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".a" )     ) ; RETURN tmp
      CASE hbmk[ _HBMK_cCOMP ] == "cygwin" .AND. hb_FileExists( tmp := cDir + "cyg" + FN_ExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".dll" )   ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, ".dll" )   ) ; RETURN tmp
      ENDCASE
   CASE hbmk[ _HBMK_cCOMP ] == "gcc" .AND. hbmk[ _HBMK_cARCH ] $ "linux|sunos"
      DO CASE
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".so" )    ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir + "lib" + FN_ExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   CASE hbmk[ _HBMK_cCOMP ] $ "pocc|pocc64|poccarm"
      DO CASE
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, cLibExt )  ) ; RETURN tmp
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, ".a" )     ) ; RETURN tmp
      ENDCASE
   OTHERWISE
      DO CASE
      CASE                           hb_FileExists( tmp := cDir +         FN_ExtSet( cLib, cLibExt )  ) ; RETURN tmp
      ENDCASE
   ENDCASE

   RETURN NIL

STATIC FUNCTION FindInPath( cFileName, cPath )
   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )
   #if defined( __PLATFORM__WINDOWS ) .OR. ;
       defined( __PLATFORM__DOS ) .OR. ;
       defined( __PLATFORM__OS2 )
      IF Empty( cExt )
         cExt := ".exe"
      ENDIF
   #endif

   /* Check in current dir. */
   IF hb_FileExists( cFileName := hb_FNameMerge( cDir, cName, cExt ) )
      RETURN cFileName
   ENDIF

   /* Check in the dir of this executable. */
   IF ! Empty( hb_DirBase() )
      IF hb_FileExists( cFileName := hb_FNameMerge( hb_DirBase(), cName, cExt ) )
         RETURN cFileName
      ENDIF
   ENDIF

   DEFAULT cPath TO GetEnv( "PATH" )

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

STATIC FUNCTION AAddNew( array, xItem )

   IF AScan( array, {|tmp| tmp == xItem } ) == 0
      AAdd( array, xItem )
   ENDIF

   RETURN array

STATIC FUNCTION AAddNotEmpty( array, xItem )

   IF ! Empty( xItem )
      AAdd( array, xItem )
   ENDIF

   RETURN array

STATIC FUNCTION ListDirExt( arraySrc, cDirNew, cExtNew )
   LOCAL array := AClone( arraySrc )
   LOCAL cFileName

   FOR EACH cFileName IN array
      cFileName := FN_DirExtSet( cFileName, cDirNew, cExtNew )
   NEXT

   RETURN array

/* Forms the list of libs as to appear on the command line */
STATIC FUNCTION ListCookLib( hbmk, aLIB, aLIBA, array, cPrefix, cExtNew )
   LOCAL cDir
   LOCAL cLibName
   LOCAL cLibNameCooked

   IF hbmk[ _HBMK_cCOMP ] $ "gcc|mingw|mingw64|mingwarm|djgpp|cygwin"
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
               cLibNameCooked := FN_ExtSet( cLibNameCooked, cExtNew )
            ENDIF
            AAdd( aLIB, cLibNameCooked )
         ELSE
            AAdd( aLIBA, cLibName )
         ENDIF
      NEXT
   ELSE
      FOR EACH cLibName IN array
         IF cExtNew != NIL
            AAdd( aLIB, FN_ExtSet( cLibName, cExtNew ) )
         ELSE
            AAdd( aLIB, cLibName )
         ENDIF
      NEXT
   ENDIF

   RETURN array

/* Append optional prefix and optional extension to all members */
STATIC FUNCTION ListCook( arraySrc, cExtNew )
   LOCAL array := AClone( arraySrc )
   LOCAL cItem

   IF cExtNew != NIL
      FOR EACH cItem IN array
         cItem := FN_ExtSet( cItem, cExtNew )
      NEXT
   ENDIF

   RETURN array

STATIC FUNCTION ArrayToList( array, cSeparator, nEscapeMode, cPrefix )
   LOCAL cString := ""
   LOCAL tmp

   DEFAULT cSeparator TO " "
   DEFAULT nEscapeMode TO _ESC_NONE
   DEFAULT cPrefix TO ""

   SWITCH nEscapeMode
   CASE _ESC_NONE
      FOR tmp := 1 TO Len( array )
         cString += cPrefix + array[ tmp ]
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
      EXIT
   CASE _ESC_DBLQUOTE
      FOR tmp := 1 TO Len( array )
         IF " " $ array[ tmp ] .OR. "-" $ array[ tmp ]
            /* Sloppy */
            IF Right( array[ tmp ], 1 ) == "\"
               array[ tmp ] += "\"
            ENDIF
            cString += cPrefix + '"' + array[ tmp ] + '"'
         ELSE
            cString += cPrefix + array[ tmp ]
         ENDIF
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
      EXIT
   CASE _ESC_SINQUOTE_WATCOM
      FOR tmp := 1 TO Len( array )
         IF " " $ array[ tmp ]
            /* Sloppy */
            IF Right( array[ tmp ], 1 ) == "\"
               array[ tmp ] += "\"
            ENDIF
            cString += cPrefix + "'" + array[ tmp ] + "'"
         ELSE
            cString += cPrefix + array[ tmp ]
         ENDIF
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
      EXIT
   CASE _ESC_NIX
      FOR tmp := 1 TO Len( array )
         cString += cPrefix + FN_Escape( array[ tmp ], nEscapeMode )
         IF tmp < Len( array )
            cString += cSeparator
         ENDIF
      NEXT
      EXIT
   ENDSWITCH

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

STATIC FUNCTION IsDriveSpec( cDir )
   RETURN ! Empty( hb_osDriveSeparator() ) .AND. ;
          Right( cDir, Len( hb_osDriveSeparator() ) ) == hb_osDriveSeparator()

/* NOTE: Can hurt if there are symlinks on the way. */
STATIC FUNCTION PathNormalize( cPath, lNormalize )
   LOCAL aDir
   LOCAL cDir

   IF ! Empty( cPath )

      DEFAULT lNormalize TO .T.

      IF lNormalize

         aDir := hb_ATokens( cPath, hb_osPathSeparator() )

         FOR EACH cDir IN aDir DESCEND
            IF cDir == "."
               hb_ADel( aDir, cDir:__enumIndex(), .T. )
            ELSEIF !( cDir == ".." ) .AND. ;
               ! Empty( cDir ) .AND. ;
               ! IsDriveSpec( cDir )
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
               cPath += hb_osPathSeparator()
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN cPath

STATIC FUNCTION PathProc( cPathR, cPathA )
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

STATIC FUNCTION PathSepToSelf( cFileName )
#if defined( __PLATFORM__WINDOWS ) .OR. ;
    defined( __PLATFORM__DOS ) .OR. ;
    defined( __PLATFORM__OS2 )
   RETURN StrTran( cFileName, "/", "\" )
#else
   RETURN StrTran( cFileName, "\", "/" )
#endif

STATIC FUNCTION PathSepToTarget( hbmk, cFileName, nStart )

   DEFAULT nStart TO 1

   IF hbmk[ _HBMK_cARCH ] $ "win|wce|dos|os2" .AND. !( hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm|cygwin" )
      RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "/", "\" )
   ENDIF

   RETURN Left( cFileName, nStart - 1 ) + StrTran( SubStr( cFileName, nStart ), "\", "/" )

STATIC FUNCTION DirAddPathSep( cDir )

   IF ! Empty( cDir ) .AND. !( Right( cDir, 1 ) == hb_osPathSeparator() )
      cDir += hb_osPathSeparator()
   ENDIF

   RETURN cDir

STATIC FUNCTION DirDelPathSep( cDir )

   IF Empty( hb_osDriveSeparator() )
      DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) == hb_osPathSeparator()
         cDir := hb_StrShrink( cDir, 1 )
      ENDDO
   ELSE
      DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) == hb_osPathSeparator() .AND. ;
               !( Right( cDir, 2 ) == hb_osDriveSeparator() + hb_osPathSeparator() )
         cDir := hb_StrShrink( cDir, 1 )
      ENDDO
   ENDIF

   RETURN cDir

#define hb_DirCreate( d ) MakeDir( d )

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
      ELSE
         cDirTemp := ""
      ENDIF

      FOR EACH cDirItem IN hb_ATokens( cDir, hb_osPathSeparator() )
         IF !( Right( cDirTemp, 1 ) == hb_osPathSeparator() ) .AND. ! Empty( cDirTemp )
            cDirTemp += hb_osPathSeparator()
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

#define hb_DirDelete( d ) DirRemove( d )

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
         IF ( tmp := RAt( hb_osPathSeparator(), cDirTemp ) ) == 0
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

STATIC FUNCTION FN_Escape( cFileName, nEscapeMode )

   DEFAULT nEscapeMode TO _ESC_NONE

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
   CASE _ESC_SINQUOTE_WATCOM
      IF " " $ cFileName
         /* Sloppy */
         IF Right( cFileName, 1 ) == "\"
            cFileName += "\"
         ENDIF
         RETURN "'" + cFileName + "'"
      ENDIF
      EXIT
   CASE _ESC_NIX
      cFileName := StrTran( cFileName, " ", "\ " )
      EXIT
   ENDSWITCH

   RETURN cFileName

STATIC FUNCTION FN_DirGet( cFileName )
   LOCAL cDir

   hb_FNameSplit( cFileName, @cDir )

   RETURN cDir

STATIC FUNCTION FN_NameGet( cFileName )
   LOCAL cName

   hb_FNameSplit( cFileName,, @cName )

   RETURN cName

STATIC FUNCTION FN_NameExtGet( cFileName )
   LOCAL cName, cExt

   hb_FNameSplit( cFileName,, @cName, @cExt )

   RETURN hb_FNameMerge( NIL, cName, cExt )

STATIC FUNCTION FN_ExtGet( cFileName )
   LOCAL cExt

   hb_FNameSplit( cFileName,,, @cExt )

   RETURN cExt

STATIC FUNCTION FN_ExtSet( cFileName, cExt )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName, cExt )

STATIC FUNCTION FN_DirExtSet( cFileName, cDirNew, cExtNew )
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
   LOCAL aDir

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

   IF ! FN_HasWildcard( cFileName )
      RETURN { cFileName }
   ENDIF

   aFileList := {}

   aDir := Directory( cFileName )
   FOR EACH aFile IN aDir
      AAdd( aFilelist, hb_FNameMerge( FN_DirGet( cFileName ), aFile[ F_NAME ] ) )
   NEXT

   RETURN aFileList

STATIC FUNCTION FN_HasWildcard( cFileName )
   RETURN "?" $ cFileName .OR. ;
          "*" $ cFileName

STATIC PROCEDURE HBC_ProcessAll( hbmk, lConfigOnly )
   LOCAL aFile
   LOCAL cDir
   LOCAL cFileName

   LOCAL aCFGDirs

   #if defined( __PLATFORM__UNIX )
      aCFGDirs := { GetEnv( "HOME" ) + "/.harbour/",;
                    "/etc/harbour",;
                    DirAddPathSep( hb_DirBase() ) + "../etc/harbour",;
                    DirAddPathSep( hb_DirBase() ) + "../etc",;
                    hb_DirBase() }
   #else
      aCFGDirs := { hb_DirBase() }
   #endif

   FOR EACH cDir IN aCFGDirs
      IF hb_FileExists( cFileName := ( PathNormalize( DirAddPathSep( cDir ) ) + _HBMK_CFG_NAME ) )
         IF ! hbmk[ _HBMK_lQuiet ]
            hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing configuration: %1$s" ), cFileName ) )
         ENDIF
         HBC_ProcessOne( hbmk, cFileName, 1 )
         EXIT
      ENDIF
   NEXT

   IF ! lConfigOnly
      FOR EACH aFile IN Directory( "*" + ".hbc" )
         cFileName := aFile[ F_NAME ]
         IF !( cFileName == _HBMK_CFG_NAME ) .AND. Lower( FN_ExtGet( cFileName ) ) == ".hbc"
            IF ! hbmk[ _HBMK_lQuiet ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cFileName ) )
            ENDIF
            HBC_ProcessOne( hbmk, cFileName, 1 )
         ENDIF
      NEXT
   ENDIF

   RETURN

#define _EOL                    Chr( 10 )

STATIC FUNCTION HBC_ProcessOne( hbmk, cFileName, nNestingLevel  )
   LOCAL cFile
   LOCAL cLine
   LOCAL cItem
   LOCAL tmp

   IF ! hb_FileExists( cFileName )
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Opening: %1$s" ), cFileName ) )
      RETURN .F.
   ENDIF

   cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */

   IF ! hb_osNewLine() == _EOL
      cFile := StrTran( cFile, hb_osNewLine(), _EOL )
   ENDIF
   IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
      cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _EOL )
   ENDIF

   FOR EACH cLine IN hb_ATokens( cFile, _EOL )

      cLine := AllTrim( ArchCompFilter( hbmk, AllTrim( cLine ) ) )

      DO CASE
      CASE Lower( Left( cLine, Len( "skip="        ) ) ) == "skip="          ; cLine := SubStr( cLine, Len( "skip="         ) + 1 )
         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ValueIsT( cLine )
            IF hbmk[ _HBMK_lInfo ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Skipping from: %1$s" ), cFileName ) )
            ENDIF
            EXIT
         ENDIF

      CASE Lower( Left( cLine, Len( "sources="     ) ) ) == "sources="       ; cLine := SubStr( cLine, Len( "sources="      ) + 1 )

         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), FN_DirGet( cFileName ) ) ) )
            IF ! Empty( cItem )
               DO CASE
               CASE FN_ExtGet( cItem ) == ".o" .OR. ;
                    FN_ExtGet( cItem ) == ".obj"
                  AAddNew( hbmk[ _HBMK_aOBJUSER ], PathSepToTarget( hbmk, cItem ) )
               CASE FN_ExtGet( cItem ) == ".c" .OR. ;
                    FN_ExtGet( cItem ) == ".cpp" /* .cc, .cxx, .cx */
                  AAddNew( hbmk[ _HBMK_aC ], PathSepToTarget( hbmk, cItem ) )
               CASE FN_ExtGet( cItem ) == ".po" .OR. ;
                    FN_ExtGet( cItem ) == ".pot"
                  AAddNew( hbmk[ _HBMK_aPO ], PathSepToTarget( hbmk, cItem ) )
               CASE FN_ExtGet( cItem ) == ".rc"
                  FOR EACH tmp IN FN_Expand( cItem )
                     AAddNew( hbmk[ _HBMK_aRESSRC ], PathSepToTarget( hbmk, tmp ) )
                  NEXT
               CASE FN_ExtGet( cItem ) == ".res"
                  IF hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm"
                     /* For MinGW family add .res files as source input, as they
                        will need to be converted to coff format with windres (just
                        like plain .rc files) before feeding them to gcc. */
                     FOR EACH tmp IN FN_Expand( cItem )
                        AAddNew( hbmk[ _HBMK_aRESSRC ], PathSepToTarget( hbmk, tmp ) )
                     NEXT
                  ELSE
                     FOR EACH tmp IN FN_Expand( cItem )
                        AAddNew( hbmk[ _HBMK_aRESCMP ], PathSepToTarget( hbmk, tmp ) )
                     NEXT
                  ENDIF
               OTHERWISE /* .prg */
                  AAddNew( hbmk[ _HBMK_aPRG ], PathSepToTarget( hbmk, cItem ) )
               ENDCASE
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libs="         ) ) ) == "libs="         ; cLine := SubStr( cLine, Len( "libs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF FN_ExtGet( cItem ) == ".hbc"
               cItem := PathProc( cItem, FN_DirGet( cFileName ) )
               IF nNestingLevel < _HBMK_NEST_MAX
                  IF ! hb_FileExists( cItem )
                     FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                        IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, .T. ) ) ) + FN_NameExtGet( cItem ) )
                           cItem := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, .T. ) ) ) + FN_NameExtGet( cItem )
                           EXIT
                        ENDIF
                     NEXT
                  ENDIF

                  IF hbmk[ _HBMK_lInfo ]
                     hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cItem ) )
                  ENDIF

                  HBC_ProcessOne( hbmk, cItem, nNestingLevel + 1 )
               ELSE
                  hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
               ENDIF
            ELSE
               cItem := PathSepToTarget( hbmk, cItem )
               IF AScan( hbmk[ _HBMK_aLIBUSER ], {|tmp| tmp == cItem } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSER ], cItem )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "hbcs="        ) ) ) == "hbcs="          ; cLine := SubStr( cLine, Len( "hbcs="         ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            IF nNestingLevel < _HBMK_NEST_MAX

               cItem := PathProc( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), FN_DirGet( cFileName ) )

               IF Empty( FN_ExtGet( cItem ) )
                  cItem := FN_ExtSet( cItem, ".hbc" )
               ENDIF

               IF ! hb_FileExists( cItem )
                  FOR EACH tmp IN hbmk[ _HBMK_aLIBPATH ]
                     IF hb_FileExists( DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, .T. ) ) ) + FN_NameExtGet( cItem ) )
                        cItem := DirAddPathSep( PathSepToSelf( MacroProc( hbmk, tmp, cItem, .T. ) ) ) + FN_NameExtGet( cItem )
                        EXIT
                     ENDIF
                  NEXT
               ENDIF

               IF hbmk[ _HBMK_lInfo ]
                  hbmk_OutStd( hbmk, hb_StrFormat( I_( "Processing: %1$s" ), cItem ) )
               ENDIF

               HBC_ProcessOne( hbmk, cItem, nNestingLevel + 1 )
            ELSE
               hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "libpaths="     ) ) ) == "libpaths="     ; cLine := SubStr( cLine, Len( "libpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), FN_DirGet( cFileName ) ) ) )
            IF ! Empty( cItem )
               IF AScan( hbmk[ _HBMK_aLIBPATH ], {|tmp| tmp == cItem } ) == 0
                  AAdd( hbmk[ _HBMK_aLIBPATH ], PathSepToTarget( hbmk, cItem ) )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "incpaths="     ) ) ) == "incpaths="     ; cLine := SubStr( cLine, Len( "incpaths="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), FN_DirGet( cFileName ) ) ) )
            IF ! Empty( cItem )
               IF AScan( hbmk[ _HBMK_aINCPATH ], {|tmp| tmp == cItem } ) == 0
                  AAdd( hbmk[ _HBMK_aINCPATH ], PathSepToTarget( hbmk, cItem ) )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "inctrypaths="  ) ) ) == "inctrypaths="  ; cLine := SubStr( cLine, Len( "inctrypaths="  ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), FN_DirGet( cFileName ) ) ) )
            IF ! Empty( cItem )
               IF AScan( hbmk[ _HBMK_aINCTRYPATH ], {|tmp| tmp == cItem } ) == 0
                  AAdd( hbmk[ _HBMK_aINCTRYPATH ], PathSepToTarget( hbmk, cItem ) )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "instpaths="    ) ) ) == "instpaths="    ; cLine := SubStr( cLine, Len( "instpaths="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathNormalize( PathSepToSelf( PathProc( MacroProc( hbmk, StrStripQuote( cItem ), cFileName ), FN_DirGet( cFileName ) ) ) )
            IF ! Empty( cItem )
               IF AScan( hbmk[ _HBMK_aINSTPATH ], {|tmp| tmp == cItem } ) == 0
                  AAdd( hbmk[ _HBMK_aINSTPATH ], PathSepToTarget( hbmk, cItem ) )
               ENDIF
            ENDIF
         NEXT

      /* NOTE: This keyword is used in hbmk.cfg and signals whether
               a given optional module (gtsln, gtcrs, gtxwc) is part of the
               Harbour shared library, so that we can automatically add
               the required libs here. [vszakats] */
      CASE Lower( Left( cLine, Len( "libdynhas="    ) ) ) == "libdynhas="    ; cLine := SubStr( cLine, Len( "libdynhas="    ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( hbmk, StrStripQuote( cItem ) )
            IF ! Empty( cItem )
               IF AScan( hbmk[ _HBMK_aLIBDYNHAS ], {|tmp| tmp == cItem } ) == 0
                  AAdd( hbmk[ _HBMK_aLIBDYNHAS ], cItem )
               ENDIF
               IF Lower( Left( cItem, 2 ) ) == "gt" .AND. ;
                  AScan( hbmk[ _HBMK_aLIBCOREGT ], {|tmp| Lower( tmp ) == Lower( cItem ) } ) == 0
                  AAdd( hbmk[ _HBMK_aLIBCOREGT ], cItem )
               ENDIF
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "echo="         ) ) ) == "echo="         ; cLine := SubStr( cLine, Len( "echo="         ) + 1 )
         cLine := MacroProc( hbmk, cLine, cFileName )
         IF ! Empty( cLine )
            OutStd( hb_StrFormat( I_( "%1$s" ), cLine ) + hb_osNewLine() )
         ENDIF

      CASE Lower( Left( cLine, Len( "prgflags="     ) ) ) == "prgflags="     ; cLine := SubStr( cLine, Len( "prgflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := PathSepToTarget( hbmk, MacroProc( hbmk, StrStripQuote( cItem ), cFileName ) )
            IF AScan( hbmk[ _HBMK_aOPTPRG ], {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( hbmk[ _HBMK_aOPTPRG ], cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "cflags="       ) ) ) == "cflags="       ; cLine := SubStr( cLine, Len( "cflags="       ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF AScan( hbmk[ _HBMK_aOPTC ], {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( hbmk[ _HBMK_aOPTC ], cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "resflags="     ) ) ) == "resflags="     ; cLine := SubStr( cLine, Len( "resflags="     ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF AScan( hbmk[ _HBMK_aOPTRES ], {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( hbmk[ _HBMK_aOPTRES ], cItem )
            ENDIF
         NEXT

      CASE Lower( Left( cLine, Len( "ldflags="      ) ) ) == "ldflags="      ; cLine := SubStr( cLine, Len( "ldflags="      ) + 1 )
         FOR EACH cItem IN hb_ATokens( cLine,, .T. )
            cItem := MacroProc( hbmk, StrStripQuote( cItem ), cFileName )
            IF AScan( hbmk[ _HBMK_aOPTL ], {|tmp| tmp == cItem } ) == 0
               AAddNotEmpty( hbmk[ _HBMK_aOPTL ], cItem )
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
         CASE Lower( cLine ) == "partial" ; hbmk[ _HBMK_nHEAD ] := _HEAD_PARTIAL
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
               IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( hbmk[ _HBMK_aLIBUSERGT ], {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], PathSepToTarget( hbmk, cLine ) )
               ENDIF
            ENDIF
         ENDIF

      CASE Lower( Left( cLine, Len( "gt="           ) ) ) == "gt="           ; cLine := SubStr( cLine, Len( "gt="           ) + 1 )
         IF ! Empty( cLine )
            IF hbmk[ _HBMK_cGT ] == NIL
               IF ! SetupForGT( cLine, @hbmk[ _HBMK_cGT ], @hbmk[ _HBMK_lGUI ] )
                  cLine := NIL
               ENDIF
            ENDIF
            IF ! Empty( cLine ) .AND. !( Lower( cLine ) == "gtnul" )
               IF AScan( hbmk[ _HBMK_aLIBCOREGT ], {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0 .AND. ;
                  AScan( hbmk[ _HBMK_aLIBUSERGT ], {|tmp| Lower( tmp ) == Lower( cLine ) } ) == 0
                  AAddNotEmpty( hbmk[ _HBMK_aLIBUSERGT ], PathSepToTarget( hbmk, cLine ) )
               ENDIF
            ENDIF
         ENDIF

      ENDCASE
   NEXT

   RETURN .T.

STATIC FUNCTION IsGTRequested( hbmk, cWhichGT )

   IF ! hbmk[ _HBMK_lSHARED ]
      /* Check if it's a core GT. */
      RETURN AScan( hbmk[ _HBMK_aLIBCOREGT ], {|tmp| Lower( tmp ) == cWhichGT } ) > 0
   ENDIF

   RETURN .F.

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

STATIC PROCEDURE HBM_Load( hbmk, aParams, cFileName, nNestingLevel )
   LOCAL cFile
   LOCAL cLine
   LOCAL cParam

   IF hb_FileExists( cFileName )

      cFile := MemoRead( cFileName ) /* NOTE: Intentionally using MemoRead() which handles EOF char. */

      IF ! hb_osNewLine() == _EOL
         cFile := StrTran( cFile, hb_osNewLine(), _EOL )
      ENDIF
      IF ! hb_osNewLine() == Chr( 13 ) + Chr( 10 )
         cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), _EOL )
      ENDIF

      FOR EACH cLine IN hb_ATokens( cFile, _EOL )
         IF !( Left( cLine, 1 ) == "#" )
            FOR EACH cParam IN hb_ATokens( cLine,, .T. )
               cParam := StrStripQuote( cParam )
               IF ! Empty( cParam )
                  DO CASE
                  CASE Lower( cParam ) == "-skip"
                     RETURN
                  CASE ( Len( cParam ) >= 1 .AND. Left( cParam, 1 ) == "@" )
                     IF nNestingLevel < _HBMK_NEST_MAX
                        cParam := SubStr( cParam, 2 )
                        IF Empty( FN_ExtGet( cParam ) )
                           cParam := FN_ExtSet( cParam, ".hbm" )
                        ENDIF
                        HBM_Load( hbmk, aParams, PathProc( cParam, cFileName ), nNestingLevel + 1 ) /* Load parameters from script file */
                     ELSE
                        hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
                     ENDIF
                  CASE Lower( FN_ExtGet( cParam ) ) == ".hbm" .OR. ;
                       Lower( FN_ExtGet( cParam ) ) == ".hbp"
                     IF nNestingLevel < _HBMK_NEST_MAX
                        HBM_Load( hbmk, aParams, PathProc( cParam, cFileName ), nNestingLevel + 1 ) /* Load parameters from script file */
                     ELSE
                        hbmk_OutErr( hbmk, hb_StrFormat( I_( "Warning: Cannot nest deeper in %1$s" ), cFileName ) )
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

   RETURN

/* Filter microformat:
   {[!][<arch|comp>]['&'|'|'][...]}
*/

STATIC FUNCTION ArchCompFilter( hbmk, cItem )
   LOCAL nStart, nEnd
   LOCAL cFilterSrc
   LOCAL cFilterHarb
   LOCAL bFilter
   LOCAL xResult
   LOCAL cValue
   LOCAL cChar

   LOCAL cExpr := "( hbmk_ARCH( hbmk ) == Lower( '%1' ) .OR. " +;
                    "hbmk_COMP( hbmk ) == Lower( '%1' ) .OR. " +;
                    "hbmk_KEYW( hbmk, Lower( '%1' ) ) )"

   IF ( nStart := At( _MACRO_OPEN, cItem ) ) > 0 .AND. ;
      !( SubStr( cItem, nStart - 1, 1 ) $ _MACRO_PREFIX_ALL ) .AND. ;
      ( nEnd := hb_At( _MACRO_CLOSE, cItem, nStart + Len( _MACRO_OPEN ) ) ) > 0

      /* Separate filter from the rest of the item */
      cFilterSrc := SubStr( cItem, nStart + Len( _MACRO_OPEN ), nEnd - nStart - Len( _MACRO_OPEN ) )
      cItem := Left( cItem, nStart - 1 ) + SubStr( cItem, nEnd + Len( _MACRO_CLOSE ) )

      IF ! Empty( cFilterSrc )

         /* Parse filter and convert it to Harbour expression */
         cFilterHarb := ""
         cValue := ""
         FOR EACH cChar IN cFilterSrc
            IF iif( Empty( cValue ),;
                  HB_ISFIRSTIDCHAR( cChar ),;
                  HB_ISNEXTIDCHAR( cChar ) )
               cValue += cChar
            ELSE
               IF ! Empty( cValue )
                  cFilterHarb += StrTran( cExpr, "%1", cValue ) + cChar
                  cValue := ""
               ELSE
                  cFilterHarb += cChar
               ENDIF
            ENDIF
         NEXT
         IF ! Empty( cValue )
            cFilterHarb += StrTran( cExpr, "%1", cValue )
         ENDIF

         cFilterHarb := StrTran( cFilterHarb, "&", ".AND." )
         cFilterHarb := StrTran( cFilterHarb, "|", ".OR." )

         /* Evaluate filter */
         BEGIN SEQUENCE WITH {| oError | Break( oError ) }
            bFilter := &( "{| hbmk |" + cFilterHarb + "}" )
         RECOVER
            bFilter := NIL
         END SEQUENCE

         IF ISBLOCK( bFilter ) .AND. ISLOGICAL( xResult := Eval( bFilter, hbmk ) ) .AND. xResult
            RETURN cItem
         ENDIF

         RETURN ""
      ENDIF
   ENDIF

   RETURN cItem

#define hb_CurDrive() CurDrive()

STATIC FUNCTION hb_pwd()
   RETURN DirAddPathSep( hb_CurDrive() + hb_osDriveSeparator() + hb_osPathSeparator() + CurDir() )

STATIC FUNCTION MacroProc( hbmk, cString, cFileName, lLateMode )
   LOCAL nStart
   LOCAL nEnd
   LOCAL cMacro

   LOCAL cStart

   LOCAL cStdOut

   IF lLateMode == NIL .OR. ! lLateMode
      cStart := _MACRO_NORM_PREFIX + _MACRO_OPEN
   ELSE
      cStart := _MACRO_LATE_PREFIX + _MACRO_OPEN
   ENDIF

   DO WHILE ( nStart := At( cStart, cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( _MACRO_CLOSE, cString, nStart + Len( cStart ) ) ) > 0

      cMacro := Upper( SubStr( cString, nStart + Len( cStart ), nEnd - nStart - Len( cStart ) ) )

      SWITCH cMacro
      CASE "HB_ROOT"
         cMacro := PathSepToSelf( DirAddPathSep( hb_DirBase() ) ) ; EXIT
      CASE "HB_DIR"
         cMacro := PathSepToSelf( FN_DirGet( cFileName ) ) ; EXIT
      CASE "HB_NAME"
         cMacro := PathSepToSelf( FN_NameGet( cFileName ) ) ; EXIT
      CASE "HB_CURDIR"
         cMacro := hb_pwd() ; EXIT
      CASE "HB_ARCH"
      CASE "HB_ARCHITECTURE" /* Compatibility */
         cMacro := hbmk[ _HBMK_cARCH ] ; EXIT
      CASE "HB_COMP"
      CASE "HB_COMPILER" /* Compatibility */
         cMacro := hbmk[ _HBMK_cCOMP ] ; EXIT
      CASE "HB_CPU"
         cMacro := hbmk_CPU( hbmk ) ; EXIT
      CASE "HB_WORK"
         cMacro := _WORKDIR_BASE_ ; EXIT
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
      CASE "HB_BIN"
         cMacro := hbmk[ _HBMK_cHB_BIN_INSTALL ] ; EXIT
      CASE "HB_LIB"
         cMacro := hbmk[ _HBMK_cHB_LIB_INSTALL ] ; EXIT
      CASE "HB_DYN"
         cMacro := hbmk[ _HBMK_cHB_DYN_INSTALL ] ; EXIT
      CASE "HB_INC"
         cMacro := hbmk[ _HBMK_cHB_INC_INSTALL ] ; EXIT
      CASE "HB_FIRST"
         cMacro := FN_NameGet( hbmk[ _HBMK_cFIRST ] ) ; EXIT
      OTHERWISE
         /* NOTE: If macro not found, try to interpret as
                  envvar. If it doesn't exist, empty string
                  will be returned (without warning) [vszakats] */
         cMacro := GetEnv( cMacro )
      ENDSWITCH

      cString := Left( cString, nStart - 1 ) + cMacro + SubStr( cString, nEnd + Len( _MACRO_CLOSE ) )
   ENDDO

   DO WHILE ( nStart := At( _CMDSUBST_OPEN, cString ) ) > 0 .AND. ;
            ( nEnd := hb_At( _CMDSUBST_CLOSE, cString, nStart + Len( _CMDSUBST_OPEN ) ) ) > 0
      cMacro := SubStr( cString, nStart + Len( _CMDSUBST_OPEN ), nEnd - nStart - Len( _CMDSUBST_OPEN ) )
      cStdOut := ""
      hbmk_run( hbmk, cMacro, @cStdOut )
      cString := Left( cString, nStart - 1 ) + cStdOut + SubStr( cString, nEnd + Len( _CMDSUBST_CLOSE ) )
   ENDDO

   RETURN cString

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
   IF hbmk[ _HBMK_cCOMP ] $ "gcc|mingw|mingw64|mingwarm|cygwin"
      hb_FNameSplit( cFile,,, @cExt )
      IF cExt == ".c"
         FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFile ), Chr( 13 ), Chr( 10 ) ), Chr( 10 ) )
            cLine := AllTrim( cLine )
            IF LEFTEQUAL( cLine, '{ "' ) .AND. "HB_FS_FIRST" $ cLine
               n := 4
               DO WHILE ( c := SubStr( cLine, n++, 1 ) ) != '"'
                  cFuncName += c
               ENDDO
               EXIT
            ENDIF
         NEXT
      ELSEIF ! Empty( cExecNM := FindInPath( hbmk[ _HBMK_cCCPREFIX ] + "nm" ) )
         cFuncList := ""
         hbmk_run( hbmk, cExecNM + " " + cFile + " -g -n --defined-only -C", @cFuncList )
         IF ( n := At( " T HB_FUN_", cFuncList ) ) != 0
            n += 10
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
   LOCAL aUnd
   LOCAL aDef
   LOCAL cMacro
   LOCAL nPos

   IF !( hbmk[ _HBMK_cARCH ] == hb_Version( HB_VERSION_BUILD_ARCH ) ) .OR. ;
      !( hbmk[ _HBMK_cCOMP ] == hb_Version( HB_VERSION_BUILD_COMP ) )

      aUnd := {}
      aDef := {}

      #if   defined( __PLATFORM__WINDOWS )
         AAdd( aUnd, "__PLATFORM__WINDOWS" )
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
            AAdd( aUnd, "__PLATFORM__Windows" )
         ENDIF
         #if defined( __PLATFORM__WINCE )
            AAdd( aUnd, "__PLATFORM__WINCE" )
         #endif
      #elif defined( __PLATFORM__DOS )
         AAdd( aUnd, "__PLATFORM__DOS" )
      #elif defined( __PLATFORM__OS2 )
         AAdd( aUnd, "__PLATFORM__OS2" )
      #elif defined( __PLATFORM__LINUX )
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
            AAdd( aUnd, "__PLATFORM__Linux" )
         ENDIF
         AAdd( aUnd, "__PLATFORM__LINUX" )
         AAdd( aUnd, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__DARWIN )
         AAdd( aUnd, "__PLATFORM__DARWIN" )
         AAdd( aUnd, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__BSD )
         AAdd( aUnd, "__PLATFORM__BSD" )
         AAdd( aUnd, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__SUNOS )
         AAdd( aUnd, "__PLATFORM__SUNOS" )
         AAdd( aUnd, "__PLATFORM__UNIX" )
      #elif defined( __PLATFORM__HPUX )
         AAdd( aUnd, "__PLATFORM__HPUX" )
         AAdd( aUnd, "__PLATFORM__UNIX" )
      #endif

      #if   defined( __ARCH16BIT__ )
         AAdd( aUnd, "__ARCH16BIT__" )
      #elif defined( __ARCH32BIT__ )
         AAdd( aUnd, "__ARCH32BIT__" )
      #elif defined( __ARCH64BIT__ )
         AAdd( aUnd, "__ARCH64BIT__" )
      #endif

      #if   defined( __LITTLE_ENDIAN__ )
         AAdd( aUnd, "__LITTLE_ENDIAN__" )
      #elif defined( __BIG_ENDIAN__ )
         AAdd( aUnd, "__BIG_ENDIAN__" )
      #elif defined( __PDP_ENDIAN__ )
         AAdd( aUnd, "__PDP_ENDIAN__" )
      #endif

      DO CASE
      CASE hbmk[ _HBMK_cARCH ] == "wce"
         AAdd( aDef, "__PLATFORM__WINDOWS" )
         AAdd( aDef, "__PLATFORM__WINCE" )
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
            AAdd( aDef, "__PLATFORM__Windows" )
         ENDIF
      CASE hbmk[ _HBMK_cARCH ] == "win"
         AAdd( aDef, "__PLATFORM__WINDOWS" )
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
            AAdd( aDef, "__PLATFORM__Windows" )
         ENDIF
      CASE hbmk[ _HBMK_cARCH ] == "dos"
         AAdd( aDef, "__PLATFORM__DOS" )
      CASE hbmk[ _HBMK_cARCH ] == "os2"
         AAdd( aDef, "__PLATFORM__OS2" )
      CASE hbmk[ _HBMK_cARCH ] == "linux"
         AAdd( aDef, "__PLATFORM__LINUX" )
         AAdd( aDef, "__PLATFORM__UNIX" )
         IF hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB
            AAdd( aDef, "__PLATFORM__Linux" )
         ENDIF
      CASE hbmk[ _HBMK_cARCH ] == "darwin"
         AAdd( aDef, "__PLATFORM__DARWIN" )
         AAdd( aDef, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cARCH ] == "bsd"
         AAdd( aDef, "__PLATFORM__BDS" )
         AAdd( aDef, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cARCH ] == "sunos"
         AAdd( aDef, "__PLATFORM__SUNOS" )
         AAdd( aDef, "__PLATFORM__UNIX" )
      CASE hbmk[ _HBMK_cARCH ] == "hpux"
         AAdd( aDef, "__PLATFORM__HPUX" )
         AAdd( aDef, "__PLATFORM__UNIX" )
      ENDCASE

      /* Setup those CPU flags which we can be sure about.
         This is not fully generic solution, cross builds
         to *nix systems aren't covered. Anyway, it's not
         recommended to use these macros in .prg code.
         [vszakats] */
      DO CASE
      CASE hbmk[ _HBMK_cARCH ] $ "dos|os2"
         AAdd( aDef, "__LITTLE_ENDIAN__" )
         AAdd( aDef, "__ARCH32BIT__" )
      CASE hbmk[ _HBMK_cARCH ] $ "wce|win"
         AAdd( aDef, "__LITTLE_ENDIAN__" ) /* Windows is currently little-endian on all supported CPUs. */
         IF hbmk[ _HBMK_cCOMP ] == "mingw64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "msvc64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "pocc64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "msvcia64" .OR. ;
            hbmk[ _HBMK_cCOMP ] == "iccia64"
            AAdd( aDef, "__ARCH64BIT__" )
         ELSE
            AAdd( aDef, "__ARCH32BIT__" )
         ENDIF
      OTHERWISE
         /* NOTE: Users will have to manually #define fitting macros for
                  given platform + compiler settings. We could only guess. */
      ENDCASE

      /* Delete macros present in both lists */
      FOR EACH cMacro IN aUnd DESCEND
         IF ( nPos := AScan( aDef, {| tmp | tmp == cMacro } ) ) > 0
            hb_ADel( aUnd, cMacro:__enumIndex(), .T. )
            hb_ADel( aDef, nPos, .T. )
         ENDIF
      NEXT

      FOR EACH cMacro IN aUnd
         AAdd( aOPTPRG, "-undef:" + cMacro )
      NEXT
      FOR EACH cMacro IN aDef
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
   STATIC hTrans := { ;
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
      IF Upper( cLib ) $ hTrans
         cLib := hTrans[ Upper( cLib ) ]
         IF cLib == NIL
            hb_ADel( aLibList, cLib:__enumIndex(), .T. )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE rtlnk_filetrans( aFileList )
   STATIC hTrans := { ;
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
      IF Upper( cFile ) $ hTrans
         cFile := hTrans[ Upper( cFile ) ]
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
      IF !Empty( cLine )
         FOR EACH cWord IN rtlnk_tokens( cLine )
            IF LEFTEQUAL( cWord, "#" )
               EXIT
            ELSEIF nMode == RTLNK_MODE_OUT
               cFileOut := cWord
               nMode := RTLNK_MODE_FILENEXT
            ELSEIF nMode == RTLNK_MODE_FILE
               IF !cWord == ","
                  IF AScan( aFileList, { |x| x == cWord } ) == 0
                     AAdd( aFileList, PathSepToTarget( hbmk, cWord ) )
                  ENDIF
                  nMode := RTLNK_MODE_FILENEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_LIB
               IF !cWord == ","
                  AAdd( aLibList, PathSepToTarget( hbmk, cWord ) )
                  nMode := RTLNK_MODE_LIBNEXT
               ENDIF
            ELSEIF nMode == RTLNK_MODE_SKIP
               IF !cWord == ","
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
               IF !rtlnk_process( hbmk, cCommands, @cFileOut, @aFileList, @aLibList, ;
                                  aPrevFiles )
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
         cHBL := FN_NameGet( hbmk[ _HBMK_aPO ][ 1 ] )
      ENDIF
      IF Empty( FN_ExtGet( cHBL ) )
         cHBL := FN_ExtSet( cHBL, ".hbl" )
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

   IF ! Empty( aNew )
      IF Empty( hbmk[ _HBMK_aLNG ] ) .OR. !( _LNG_MARKER $ cHBL )
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .hbl file '%1$s'" ), cHBL ) )
      ELSE
         hbmk_OutStd( hbmk, hb_StrFormat( I_( "Created .hbl file '%1$s' for language(s): %2$s" ), cHBL, ArrayToList( aNew, "," ) ) )
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
      hbmk_OutErr( hbmk, "LoadPOTFiles() didn't load anything" )
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
      IF !__i18n_potArraySave( cFileOut, aTrans, @cErrorMsg, ! hbmk[ _HBMK_lMINIPO ], ! hbmk[ _HBMK_lMINIPO ] )
         hbmk_OutErr( hbmk, hb_StrFormat( I_( ".pot merge error: %1$s" ), cErrorMsg ) )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE AutoTrans( hbmk, cFileIn, aFiles, cFileOut )
   LOCAL cErrorMsg

   IF !__I18N_potArraySave( cFileOut, ;
         __I18N_potArrayTrans( LoadPOTFiles( hbmk, {}, cFileIn, .F. ), ;
                               LoadPOTFilesAsHash( hbmk, aFiles ) ), @cErrorMsg, ! hbmk[ _HBMK_lMINIPO ], ! hbmk[ _HBMK_lMINIPO ] )
      hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: %1$s" ), cErrorMsg ) )
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

#define _VCS_UNKNOWN            0
#define _VCS_SVN                1
#define _VCS_GIT                2
#define _VCS_MERCURIAL          3
#define _VCS_CVS                4

STATIC FUNCTION VCSDetect( cDir )

   DEFAULT cDir TO ""

   IF ! Empty( cDir )
      cDir := DirAddPathSep( cDir )
   ENDIF

   DO CASE
   CASE hb_DirExists( cDir + ".svn" ) ; RETURN _VCS_SVN
   CASE hb_DirExists( cDir + ".git" ) ; RETURN _VCS_GIT
   CASE hb_DirExists( cDir + ".hg" )  ; RETURN _VCS_MERCURIAL
   CASE hb_DirExists( cDir + "CVS" )  ; RETURN _VCS_CVS
   CASE hb_DirExists( cDir + "_svn" ) ; RETURN _VCS_SVN /* NOTE: When SVN_ASP_DOT_NET_HACK envvar is set. [vszakats] */
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
      cCommand := "svnversion" + iif( Empty( cDir ), "", " " + cDir )
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
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN cResult

/* Keep this public, it's used from macro. */
FUNCTION hbmk_ARCH( hbmk )
   RETURN hbmk[ _HBMK_cARCH ]

/* Keep this public, it's used from macro. */
FUNCTION hbmk_COMP( hbmk )
   RETURN hbmk[ _HBMK_cCOMP ]

FUNCTION hbmk_CPU( hbmk )

   DO CASE
   CASE hbmk[ _HBMK_cARCH ] $ "dos|os2" .OR. ;
        hbmk[ _HBMK_cCOMP ] $ "gcc|cygwin|watcom|bcc|icc|xcc" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "mingw" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "msvc" .OR. ;
        hbmk[ _HBMK_cCOMP ] == "pocc"
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
   ENDCASE

   RETURN ""

FUNCTION hbmk_KEYW( hbmk, cKeyword )
   LOCAL tmp

   IF cKeyword == iif( hbmk[ _HBMK_lMT ]     , "mt"      , "st"      ) .OR. ;
      cKeyword == iif( hbmk[ _HBMK_lGUI ]    , "gui"     , "std"     ) .OR. ;
      cKeyword == iif( hbmk[ _HBMK_lDEBUG ]  , "debug"   , "nodebug" ) .OR. ;
      cKeyword == iif( hbmk[ _HBMK_lSHARED ] , "shared"  , "static"  ) .OR. ;
      cKeyword == iif( hbmk[ _HBMK_lUNICODE ], "unicode" , "ascii"   )
      RETURN .T.
   ENDIF

   IF ( cKeyword == "unix"     .AND. ( hbmk[ _HBMK_cARCH ] $ "bsd|hpux|sunos|linux" .OR. hbmk[ _HBMK_cARCH ] == "darwin" ) ) .OR. ;
      ( cKeyword == "allwin"   .AND. hbmk[ _HBMK_cARCH ] $ "win|wce"                                             ) .OR. ;
      ( cKeyword == "allgcc"   .AND. hbmk[ _HBMK_cCOMP ] $ "gcc|mingw|mingw64|mingwarm|cygwin|djgpp"             ) .OR. ;
      ( cKeyword == "allmingw" .AND. hbmk[ _HBMK_cCOMP ] $ "mingw|mingw64|mingwarm"                              ) .OR. ;
      ( cKeyword == "allmsvc"  .AND. hbmk[ _HBMK_cCOMP ] $ "msvc|msvc64|msvcarm"                                 ) .OR. ;
      ( cKeyword == "allpocc"  .AND. hbmk[ _HBMK_cCOMP ] $ "pocc|pocc64|poccarm"                                 ) .OR. ;
      ( cKeyword == "allicc"   .AND. hbmk[ _HBMK_cCOMP ] $ "icc|iccia64"                                         )
      RETURN .T.
   ENDIF

   IF ( cKeyword == "xhb" .AND. hbmk[ _HBMK_nHBMODE ] == _HBMODE_XHB ) .OR. ;
      ( cKeyword == "hb10" .AND. hbmk[ _HBMK_nHBMODE ] == _HBMODE_HB10 )
      RETURN .T.
   ENDIF

   IF cKeyword == hbmk_CPU( hbmk )
      RETURN .T.
   ENDIF

   tmp := GetEnv( cKeyword )
   IF ! Empty( tmp ) .AND. !( tmp == "0" ) .AND. !( Lower( tmp ) == "no" )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION MacOSXFiles( hbmk, nType, cPROGNAME )
   LOCAL cString

   HB_SYMBOL_UNUSED( hbmk )

   SWITCH nType
   CASE 1

      #pragma __cstream|cString:=%s
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
      ENDTEXT
      EXIT
   CASE 2
      cString := "%__APPTYPE__%%__APPSIGN__%"
      EXIT
   ENDSWITCH

   cString := StrTran( cString, "%TAB%", Chr( 9 ) )

   cString := StrTran( cString, "%__APPNAME__%", cPROGNAME )
   cString := StrTran( cString, "%__APPTYPE__%", "APPL" )
   cString := StrTran( cString, "%__APPSIGN__%", PadR( cPROGNAME, 4, "?" ) )

   RETURN cString

STATIC PROCEDURE GetUILangCDP( /* @ */ cLNG, /* @ */ cCDP )

   IF Empty( cLNG := GetEnv( "HB_LANG" ) )
      IF Empty( cLNG := hb_UserLang() )
         cLNG := "en-EN"
      ENDIF
   ENDIF

   cLNG := StrTran( cLNG, "_", "-" )
   cCDP := "" /* TODO: 1) Detect it 2) use it - this would need generic Harbour CP support */

   RETURN

STATIC PROCEDURE SetUILang( hbmk )
   LOCAL tmp

   IF hbmk[ _HBMK_cUILNG ] == "en-EN"
      hb_i18n_set( NIL )
   ELSE
      tmp := "${hb_root}hbmk2.${hb_lng}.hbl"
      tmp := StrTran( tmp, "${hb_root}", PathSepToSelf( DirAddPathSep( hb_DirBase() ) ) )
      tmp := StrTran( tmp, "${hb_lng}", StrTran( hbmk[ _HBMK_cUILNG ], "-", "_" ) )
      hb_i18n_set( iif( hb_i18n_check( tmp := hb_MemoRead( tmp ) ), hb_i18n_restoretable( tmp ), NIL ) )
   ENDIF

   /* Setup input CP of the translation */
   hb_cdpSelect( Upper( SubStr( I_( "cdp=EN" ), Len( "cdp=" ) + 1 ) ) )

   /* Setup output CP, separate for Windows/DOS/OS2 and *nix systems */
   /* NOTE: Intentionally doing runtime branching to include both strings in translation files. */
   tmp := Upper( SubStr( iif( hb_Version( HB_VERSION_UNIX_COMPAT ), I_( "nix=EN" ), I_( "wdo=EN" ) ), Len( "xxx=" ) + 1 ) )
   IF tmp == "UTF8" .OR. tmp == "UTF-8"
      hbmk[ _HBMK_lUTF8 ] := .T.
   ELSE
      hb_setDispCP( tmp )
   ENDIF

   RETURN

STATIC PROCEDURE ShowHeader( hbmk )

   OutStd( "Harbour Make " + HBRawVersion() + hb_osNewLine() +;
           "Copyright (c) 1999-2009, Viktor Szakats" + hb_osNewLine() +;
           "http://www.harbour-project.org/" + hb_osNewLine() )

   IF !( hbmk[ _HBMK_cUILNG ] == "en-EN" )
      OutStd( hb_StrFormat( I_( "Translation (%1$s): (add your name here)" ), hbmk[ _HBMK_cUILNG ] ) + hb_osNewLine() )
   ENDIF

   OutStd( hb_osNewLine() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

STATIC PROCEDURE ShowHelp( hbmk, lLong )

   LOCAL aText_Basic := {;
      I_( "Syntax:" ),;
      "",;
      I_( "  hbmk [options] [<script[s]>] <src[s][.prg|.c|.obj|.o|.rc|.res|.po|.pot|.hbl]>" ),;
      "",;
      I_( "Options:" ) }

   LOCAL aText_Supp := {;
      "",;
      I_( "Supported <comp> values for each supported <arch> value:" ),;
      "  - linux  : gcc, watcom, icc",;
      "  - darwin : gcc",;
      "  - win    : mingw, msvc, bcc, watcom, icc, pocc, cygwin, xcc,",;
      "  -          mingw64, msvc64, msvcia64, iccia64, pocc64",;
      "  - wce    : mingwarm, msvcarm, poccarm",;
      "  - os2    : gcc, watcom",;
      "  - dos    : djgpp, watcom",;
      "  - bsd    : gcc",;
      "  - hpux   : gcc",;
      "  - sunos  : gcc" }

   LOCAL aOpt_Basic := {;
      { "-o<outname>"       , I_( "output file name" ) },;
      { "-l<libname>"       , I_( "link with <libname> library. <libname> should be without path, extension and 'lib' prefix (unless part of libname)." ) },;
      { "-L<libpath>"       , I_( "additional path to search for libraries" ) },;
      { "-i<p>|-incpath=<p>", I_( "additional path to search for headers" ) },;
      { "-static|-shared"   , I_( "link with static/shared libs" ) },;
      { "-mt|-st"           , I_( "link with multi/single-thread VM" ) },;
      { "-gt<name>"         , I_( "link with GT<name> GT driver, can be repeated to link with more GTs. First one will be the default at runtime" ) },;
      { "-hblib"            , I_( "create static library" ) },;
      { "-hbdyn"            , I_( "create dynamic library" ) }}

   LOCAL aOpt_Help := {;
      { "-help|--help"      , I_( "long help" ) } }

   LOCAL aOpt_Long := {;
      NIL,;
      { "-gui|-std"         , I_( "create GUI/console executable" ) },;
      { "-main=<mainfunc>"  , I_( "override the name of starting function/procedure" ) },;
      { "-fullstatic"       , I_( "link with all static libs" ) },;
      { "-[full|fix]shared" , I_( "create shared Harbour binaries without/with absolute dir reference to Harbour library (default: 'fullshared' when Harbour is installed on system location, 'fixshared' otherwise) (fix/full option in *nix only)" ) },;
      { "-nulrdd[-]"        , I_( "link with nulrdd" ) },;
      { "-[no]debug"        , I_( "add/exclude C compiler debug info. For Harbour level debug, use Harbour option -b as usual" ) },;
      { "-[no]optim"        , I_( "toggle C compiler optimizations (default: on)" ) },;
      { "-[no]cpp[=def]"    , I_( "force C/C++ mode or reset to default" ) },;
      { "-[no]map"          , I_( "create (or not) a map file" ) },;
      { "-[no]strip"        , I_( "strip (no strip) binaries" ) },;
      { "-[no]trace"        , I_( "show commands executed" ) },;
      { "-[no]beep"         , I_( "enable (or disable) single beep on successful exit, double beep on failure" ) },;
      { "-traceonly"        , I_( "show commands to be executed, but don't execute them" ) },;
      { "-[no]compr[=lev]"  , I_( "compress executable/dynamic lib (needs UPX)\n<lev> can be: min, max, def" ) },;
      { "-[no]run"          , I_( "run/don't run output executable" ) },;
      { "-vcshead=<file>"   , I_( "generate .ch header file with local repository information. SVN, Git and Mercurial are currently supported. Generated header will define macro _HBMK_VCS_TYPE_ with the name of detected VCS and _HBMK_VCS_ID_ with the unique ID of local repository" ) },;
      { "-tshead=<file>"    , I_( "generate .ch header file with timestamp information. Generated header will define macros _HBMK_BUILD_DATE_, _HBMK_BUILD_TIME_, _HBMK_BUILD_TIMESTAMP_ with the date/time of build" ) },;
      { "-instpath=<path>"  , I_( "copy target to <path>. if <path> is a directory, it should end with path separator. can be specified multiple times" ) },;
      { "-nohbc"            , I_( "do not process .hbc files in current directory" ) },;
      { "-stop"             , I_( "stop without doing anything" ) },;
      { "-echo=<text>"      , I_( "echo text on screen" ) },;
      NIL,;
      { "-bldf[-]"          , I_( "inherit all/no (default) flags from Harbour build" ) },;
      { "-bldf=[p][c][l]"   , I_( "inherit .prg/.c/linker flags (or none) from Harbour build" ) },;
      { "-inctrypath=<p>"   , I_( "additional path to autodetect .c header locations" ) },;
      { "-prgflag=<f>"      , I_( "pass flag to Harbour" ) },;
      { "-cflag=<f>"        , I_( "pass flag to C compiler" ) },;
      { "-resflag=<f>"      , I_( "pass flag to resource compiler (Windows only)" ) },;
      { "-ldflag=<f>"       , I_( "pass flag to linker (executable)" ) },;
      { "-aflag=<f>"        , I_( "pass flag to linker (static library)" ) },;
      { "-dflag=<f>"        , I_( "pass flag to linker (dynamic library)" ) },;
      { "-runflag=<f>"      , I_( "pass flag to output executable when -run option is used" ) },;
      { "-jobs=<n>"         , I_( "start n compilation threads (multiprocess platforms only)" ) },;
      { "-inc"              , I_( "enable incremental build mode" ) },;
      { "-[no]head[=<m>]"   , I_( "control source header parsing (in incremental build mode)\n<m> can be: full, partial (default), off" ) },;
      { "-rebuild"          , I_( "rebuild all (in incremental build mode)" ) },;
      { "-clean"            , I_( "clean (in incremental build mode)" ) },;
      { "-workdir=<dir>"    , hb_StrFormat( I_( "working directory for incremental build mode\n(default: %1$s/arch/comp)" ), _WORKDIR_BASE_ ) },;
      NIL,;
      { "-hbl[=<output>]"   , hb_StrFormat( I_( "output .hbl filename. %1$s macro is accepted in filename" ), _LNG_MARKER ) },;
      { "-lng=<languages>"  , hb_StrFormat( I_( "list of languages to be replaced in %1$s macros in .pot/.po filenames and output .hbl/.po filenames. Comma separared list:\n-lng=en-EN,hu-HU,de" ), _LNG_MARKER ) },;
      { "-po=<output>"      , I_( "create/update .po file from source. Merge it with previous .po file of the same name" ) },;
      { "-[no]minipo"       , I_( "don't (or do) add Harbour version number and source file reference to .po (default: add them)" ) },;
      { "-rebuildpo"        , I_( "recreate .po file, thus removing all obsolete entries in it" ) },;
      NIL,;
      { "-target=<script>"  , I_( "specify a new build target. <script> can be .prg (or no extension) or .hbm file (available on command line only)" ) },;
      { "-target"           , I_( "marks beginning of options belonging to a new build target (available on command line only)" ) },;
      { "-alltarget"        , I_( "marks beginning of common options belonging to all targets (available on command line only)" ) },;
      NIL,;
      { "-hbrun"            , I_( "run target" ) },;
      { "-hbraw"            , I_( "stop after running Harbour compiler" ) },;
      { "-hbcmp|-clipper"   , I_( "stop after creating the object files\ncreate link/copy hbmk to hbcmp/clipper for the same effect" ) },;
      { "-hbcc"             , I_( "stop after creating the object files and accept raw C flags\ncreate link/copy hbmk to hbcc for the same effect" ) },;
      { "-hblnk"            , I_( "accept raw linker flags" ) },;
      { "-hb10"             , I_( "enable Harbour 1.0.x compatibility mode (experimental)" ) },;
      { "-xhb"              , I_( "enable xhb mode (experimental)" ) },;
      { "-hbc"              , I_( "enable pure C mode (experimental)" ) },;
      { "-nohblib"          , I_( "do not use static core Harbour libraries when linking" ) },;
      { "-rtlink"           , "" },;
      { "-blinker"          , "" },;
      { "-exospace"         , I_( "emulate Clipper compatible linker behavior\ncreate link/copy hbmk to rtlink/blinker/exospace for the same effect" ) },;
      NIL,;
      { "--hbdirbin"        , I_( "output Harbour binary directory" ) },;
      { "--hbdirdyn"        , I_( "output Harbour dynamic library directory" ) },;
      { "--hbdirlib"        , I_( "output Harbour static library directory" ) },;
      { "--hbdirinc"        , I_( "output Harbour header directory" ) },;
      NIL,;
      { "-arch=<arch>"      , I_( "assume specific architecure. Same as HB_ARCHITECTURE envvar" ) },;
      { "-comp=<comp>"      , I_( "use specific C compiler. Same as HB_COMPILER envvar\nSpecial value:\n - bld: use original build settings (default on *nix)" ) },;
      { "-lang=<lang>"      , I_( "override default language. Similar to HB_LANG envvar." ) },;
      { "--version"         , I_( "display version header only" ) },;
      { "-pause"            , I_( "force waiting for a key on exit in case of failure (with alternate GTs only)" ) },;
      { "-info"             , I_( "turn on informational messages" ) },;
      { "-quiet"            , I_( "suppress all screen messages" ) } }

   LOCAL aText_Notes := {;
      "",;
      I_( "Notes:" ) }

   LOCAL aNotes := {;
      I_( "<script> can be <@script> (.hbm format), <script.hbm>, <script.hbp> (marks a new target) or <script.hbc>." ),;
      I_( "Multiple -l, -L and <script> parameters are accepted." ),;
      I_( "Regular Harbour compiler options are also accepted." ),;
      hb_StrFormat( I_( "%1$s option file in hbmk directory is always processed if it exists. On *nix platforms ~/.harbour, /etc/harbour, <base>/etc/harbour, <base>/etc are checked (in that order) before the hbmk directory. The file format is the same as .hbc." ), _HBMK_CFG_NAME ),;
      hb_StrFormat( I_( "%1$s make script in current directory is always processed if it exists." ), _HBMK_AUTOHBM_NAME ),;
      I_( ".hbc config files in current dir are automatically processed." ),;
      I_( ".hbc options (they should come in separate lines): libs=[<libname[s]>], hbcs=[<.hbc file[s]>], gt=[gtname], prgflags=[Harbour flags], cflags=[C compiler flags], resflags=[resource compiler flags], ldflags=[linker flags], libpaths=[paths], sources=[source files], incpaths=[paths], inctrypaths=[paths], instpaths=[paths], gui|mt|shared|nulrdd|debug|opt|map|strip|run|inc=[yes|no], cpp=[yes|no|def], compr=[yes|no|def|min|max], head=[off|partial|full], skip=[yes|no], echo=<text>\nLines starting with '#' char are ignored" ),;
      I_( "Platform filters are accepted in each .hbc line and with several options.\nFilter format: {[!][<arch>|<comp>|<keyword>]}. Filters can be combined using '&', '|' operators and grouped by parantheses. Ex.: {win}, {gcc}, {linux|darwin}, {win&!pocc}, {(win|linux)&!watcom}, {unix&mt&gui}, -cflag={win}-DMYDEF, -stop{dos}, -stop{!allwin}, {allpocc|allgcc|allmingw|unix}, {allmsvc}, {x86|x86_64|ia64|arm}, {debug|nodebug|gui|std|mt|st|xhb}" ),;
      I_( "Certain .hbc lines (libs=, hbcs=, prgflags=, cflags=, ldflags=, libpaths=, inctrypaths=, instpaths=, echo=) and corresponding command line parameters will accept macros: ${hb_root}, ${hb_dir}, ${hb_name}, ${hb_arch}, ${hb_comp}, ${hb_cpu}, ${hb_bin}, ${hb_lib}, ${hb_dyn}, ${hb_inc}, ${<envvar>}. libpaths= also accepts %{hb_name} which translates to the name of the .hbc file under search." ),;
      I_( 'Options accepting macros also support command substitution. Enclose command inside ``, and, if the command contains space, also enclose in double quotes. F.e. "-cflag=`wx-config --cflags`", or ldflags={unix&gcc}"`wx-config --libs`".' ),;
      I_( "Defaults and feature support vary by architecture/compiler." ) }

   DEFAULT lLong TO .F.

   AEval( aText_Basic, {|tmp| OutStd( tmp + hb_osNewLine() ) } )
   AEval( aOpt_Basic, {|tmp| OutOpt( hbmk, tmp ) } )
   IF lLong
      AEval( aOpt_Long, {|tmp| OutOpt( hbmk, tmp ) } )
      AEval( aText_Notes, {|tmp| OutStd( tmp + hb_osNewLine() ) } )
      AEval( aNotes, {|tmp| OutNote( hbmk, tmp ) } )
      AEval( aText_Supp, {|tmp| OutStd( tmp + hb_osNewLine() ) } )
   ELSE
      AEval( aOpt_Help, {|tmp| OutOpt( hbmk, tmp ) } )
   ENDIF

   RETURN

STATIC PROCEDURE OutOpt( hbmk, aOpt )
   LOCAL nLine
   LOCAL nLines
   LOCAL tmp

   IF Empty( aOpt )
      OutStd( hb_osNewLine() )
   ELSE
      aOpt[ 2 ] := StrTran( aOpt[ 2 ], "\n", hb_osNewLine() )
      nLines := MLCount( aOpt[ 2 ], MaxCol() - 21 )
      FOR nLine := 1 TO nLines
         IF ! Empty( tmp := MemoLine( aOpt[ 2 ], MaxCol() - 21, nLine ) )
            OutStd( "  " )
            IF nLine == 1
               OutStd( PadR( aOpt[ 1 ], 19 ) )
            ELSE
               OutStd( Space( 19 ) )
            ENDIF
            OutStd( tmp + hb_osNewLine() )
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE OutNote( hbmk, cText )
   LOCAL nLine
   LOCAL nLines
   LOCAL tmp

   cText := StrTran( cText, "\n", hb_osNewLine() )
   nLines := MLCount( cText, MaxCol() - 4 )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := MemoLine( cText, MaxCol() - 4, nLine ) )
         IF nLine == 1
            OutStd( "  - " )
         ELSE
            OutStd( "    " )
         ENDIF
         OutStd( tmp + hb_osNewLine() )
     ENDIF
   NEXT

   RETURN

STATIC PROCEDURE hbmk_OutStd( hbmk, cText )
   LOCAL nLine
   LOCAL nLines
   LOCAL tmp

   cText := StrTran( cText, "\n", hb_osNewLine() )
   nLines := MLCount( cText, MaxCol() - 6 )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := MemoLine( cText, MaxCol() - 4, nLine ) )
         IF nLine == 1
            OutStd( "hbmk: " )
         ELSE
            OutStd( "      " )
         ENDIF
         OutStd( tmp + hb_osNewLine() )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE hbmk_OutErr( hbmk, cText )
   LOCAL nLine
   LOCAL nLines
   LOCAL tmp

   cText := StrTran( cText, "\n", hb_osNewLine() )
   nLines := MLCount( cText, MaxCol() - 6 )
   FOR nLine := 1 TO nLines
      IF ! Empty( tmp := MemoLine( cText, MaxCol() - 4, nLine ) )
         IF nLine == 1
            OutErr( "hbmk: " )
         ELSE
            OutErr( "      " )
         ENDIF
         OutErr( tmp + hb_osNewLine() )
      ENDIF
   NEXT

   RETURN

/* NOTE: Hacks to implement UTF-8 output.
         It's not yet natively supported by core. [vszakats] */

#undef OutStd
#undef OutErr

STATIC PROCEDURE low_OutStd( lUTF8, cString )
   OutStd( iif( lUTF8, hb_StrToUTF8( cString ), cString ) )
   RETURN

STATIC PROCEDURE low_OutErr( lUTF8, cString )
   OutErr( iif( lUTF8, hb_StrToUTF8( cString ), cString ) )
   RETURN
