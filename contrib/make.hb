#!/usr/bin/hbmk2
/*
 * Harbour Project source code:
 * Package build orchestrator script
 *
 * Copyright 2010-2014 Viktor Szakats (vszakats.net/harbour)
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
 * their web site at https://www.gnu.org/).
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

#define _ACT_INC_CLEAN          1
#define _ACT_INC                2
#define _ACT_INC_INST           3
#define _ACT_INC_REBUILD        4
#define _ACT_INC_REBUILD_INST   5

STATIC sc_hActions := { ;
   _ACT_INC_CLEAN        => "clean", ;
   _ACT_INC              => "build", ;
   _ACT_INC_INST         => "build and install", ;
   _ACT_INC_REBUILD      => "rebuild", ;
   _ACT_INC_REBUILD_INST => "rebuild and install" }

STATIC s_cBase
STATIC s_cHome
STATIC s_cRoot
STATIC s_cBinDir
STATIC s_cReBase

PROCEDURE Main( ... )

   LOCAL hProjectList
   LOCAL aParams

   hb_cdpSelect( "UTF8EX" )

   s_cBase := ""
   s_cReBase := ""
   IF Empty( GetEnv( "HB_HOST_BIN_DIR" ) )
      s_cHome := StrTran( hb_DirBase(), hb_ps(), "/" )
      s_cRoot := s_cHome + "../"
   ELSE
      s_cHome := ""
      s_cRoot := "../"
   ENDIF

#if defined( __HBSCRIPT__HBSHELL )
   s_cBinDir := hbshell_DirBase()
#else
   s_cBinDir := hb_DirBase()
#endif
   /* For *nixes */
   s_cBinDir := hb_PathNormalize( s_cBinDir )

   /* Load list of projects */

   hProjectList := { => }

   LoadProjectListAutomatic( hProjectList, s_cHome )
   LoadProjectListFromString( hProjectList, GetEnv( "HB_BUILD_ADDONS" ) )

   aParams := hb_AParams()

   IF AScanL( aParams, "verbose" ) > 0
      hb_SetEnv( "HB_BUILD_VERBOSE", "yes" )
   ENDIF

   /* Build */
   IF Empty( GetEnv( "HB_HOST_BIN_DIR" ) )
      Standalone( aParams, hProjectList )
   ELSE
      GNUMake( aParams, hProjectList )
   ENDIF

   RETURN

/* Workflow translation for standalone operation:

      GNU Make       parameter      nAction                hbmk2 options
   -- -------------- -------------- ---------------------- -------------------------
   #1 clean          clean          _ACT_INC_CLEAN         -inc -clean
   #2                               _ACT_INC               -inc
   #3 clean all      clean all      _ACT_INC_REBUILD       -inc -rebuildall
   #4 install        install        _ACT_INC_INST          -inc -instpath=
   #5 clean install  clean install  _ACT_INC_REBUILD_INST  -inc -rebuildall -instpath=
 */
STATIC PROCEDURE Standalone( aParams, hProjectList )

   LOCAL hProjectReqList

   LOCAL cOptionsUser

   LOCAL nAction
   LOCAL tmp
   LOCAL tmp1

   LOCAL lCustom
   LOCAL cCustomDir

   /* Processing cmdline options */

   DO CASE
   CASE AScanL( aParams, "clean" ) > 0 .AND. ;
        AScanL( aParams, "all" ) > 0 .AND. ;
        AScanL( aParams, "all" ) > AScanL( aParams, "clean" )
      nAction := _ACT_INC_REBUILD
   CASE AScanL( aParams, "rebuild" ) > 0
      nAction := _ACT_INC_REBUILD
   CASE AScanL( aParams, "clean" ) > 0 .AND. ;
        AScanL( aParams, "install" ) > 0 .AND. ;
        AScanL( aParams, "install" ) > AScanL( aParams, "clean" )
      nAction := _ACT_INC_REBUILD_INST
   CASE AScanL( aParams, "clean" ) > 0
      nAction := _ACT_INC_CLEAN
   CASE AScanL( aParams, "install" ) > 0
      nAction := _ACT_INC_INST
   OTHERWISE
      nAction := _ACT_INC
   ENDCASE

   /* Processing user options */

   cOptionsUser := ""
   lCustom := .F.
   FOR EACH tmp IN aParams
      IF !( Lower( tmp ) == "install" ) .AND. ;
         !( Lower( tmp ) == "clean" ) .AND. ;
         !( Lower( tmp ) == "all" ) .AND. ;
         !( Lower( tmp ) == "first" ) .AND. ;
         !( Lower( tmp ) == "rebuild" ) .AND. ;
         !( Lower( tmp ) == "verbose" )

         cOptionsUser += " " + tmp

         /* If anything else is passed than options or GNU Make keywords,
            consider it a custom project build, f.e. in tests */
         IF ! hb_LeftEq( tmp, "-" )
            lCustom := .T.
         ENDIF
      ENDIF
   NEXT

   /* Assemble list of primary targets (registered projects in current directory) */

   hProjectReqList := { => }

   IF hb_DirExists( AllTrim( cOptionsUser ) )
      cCustomDir := hb_cwd( AllTrim( cOptionsUser ) )
      cOptionsUser := ""
      lCustom := .F.
      s_cHome += "../"
      s_cRoot += "../"
   ENDIF

   IF ! lCustom
      /* Find out which projects are in current dir, these will be our primary targets */
      FOR EACH tmp IN hProjectList
         tmp1 := hb_ps() + hb_FNameDir( hb_DirSepToOS( tmp:__enumKey() ) )
         IF tmp1 == Right( hb_cwd(), Len( tmp1 ) ) /* Not ultimate solution */
            hProjectReqList[ tmp:__enumKey() ] := tmp:__enumKey()
            s_cReBase := SubStr( tmp1, 2 )
         ENDIF
      NEXT
      IF Empty( hProjectReqList )
         lCustom := .T.
      ELSE
         OutStd( hb_StrFormat( "! Package %1$s... %2$d project(s)", sc_hActions[ nAction ], Len( hProjectReqList ) ) + hb_eol() )
      ENDIF
   ENDIF

   IF lCustom
      mk_hb_processRun( s_cBinDir + "hbmk2" + cOptionsUser )
      RETURN
   ENDIF

   /* Start building */

   build_projects( nAction, hProjectList, hProjectReqList, cOptionsUser, .T. )

   IF ! Empty( cCustomDir )
      hb_cwd( cCustomDir )
   ENDIF

   RETURN

/* Workflow translation from GNU Make to hbmk2:

      GNU Make       parameter  HB_MAKECMDGOALS  nAction                hbmk2 options
   -- -------------- ---------- ---------------- ---------------------- -------------------------
   #1 clean          clean      clean            _ACT_INC_CLEAN         -inc -clean
   #2                all                         _ACT_INC               -inc
   #3 install        install    install          _ACT_INC_INST          -inc -instpath=
   #4 clean all      clean      clean all        _ACT_INC_CLEAN         -inc -clean
                     first      clean all        _ACT_INC_REBUILD       -inc -rebuildall
   #5 clean install  clean      clean install    _ACT_INC_CLEAN         -inc -clean
                     install    clean install    _ACT_INC_REBUILD_INST  -inc -rebuildall -instpath=
   #6 install clean  install    install clean    _ACT_INC_INST          -inc -instpath=
                     clean      install clean    _ACT_INC_CLEAN         -inc -clean
 */
STATIC PROCEDURE GNUMake( aParams, hProjectList )

   LOCAL cProject
   LOCAL hProjectReqList

   LOCAL cFilter
   LOCAL aFilter
   LOCAL lFilterNegative

   LOCAL aGNUMakeParams
   LOCAL nAction
   LOCAL tmp

   /* Check if the requirements are met and if we have anything to do */

   IF Empty( GetEnv( "HB_PLATFORM" ) ) .OR. ;
      Empty( GetEnv( "HB_COMPILER" ) ) .OR. ;
      Empty( GetEnv( "HB_HOST_BIN_DIR" ) )
      ErrorLevel( 9 )
      RETURN
   ENDIF

   /* Determine the mode of operation */

   aGNUMakeParams := hb_ATokens( Lower( GetEnv( "HB_MAKECMDGOALS" ) ) )

   DO CASE
   CASE AScanL( aParams, "clean" ) > 0
      IF AScanL( aGNUMakeParams, "clean" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "install" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "install" ) > AScanL( aGNUMakeParams, "clean" )
         nAction := _ACT_INC_CLEAN
      ELSE
         nAction := _ACT_INC_CLEAN
      ENDIF
   CASE AScanL( aParams, "install" ) > 0
      IF AScanL( aGNUMakeParams, "clean" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "install" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "install" ) > AScanL( aGNUMakeParams, "clean" )
         /* Use rebuild mode. This is needed because the clean phase
            might not have been called previously by GNU Make, f.e.
            because hbrun or hbmk2 wasn't available. -rebuildall is
            costless, so we do it to make sure to build cleanly.
            [vszakats] */
         nAction := _ACT_INC_REBUILD_INST
      ELSE
         nAction := _ACT_INC_INST
      ENDIF
   CASE AScanL( aParams, "first" ) > 0
      IF AScanL( aGNUMakeParams, "clean" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "all" ) > 0 .AND. ;
         AScanL( aGNUMakeParams, "all" ) > AScanL( aGNUMakeParams, "clean" )
         nAction := _ACT_INC_REBUILD
      ELSE
         nAction := _ACT_INC
      ENDIF
   CASE AScanL( aParams, "rebuild" ) > 0
      nAction := _ACT_INC_REBUILD
   OTHERWISE
      nAction := _ACT_INC
   ENDCASE

   /* Assemble list of projects to be built */

   IF ! Empty( cFilter := GetEnv( "HB_BUILD_CONTRIBS" ) )
      OutStd( hb_StrFormat( "! HB_BUILD_CONTRIBS: %1$s", cFilter ) + hb_eol() )
   ENDIF

   IF cFilter == "no"
      RETURN
   ENDIF

   aFilter := iif( Empty( cFilter ), {}, hb_ATokens( cFilter,, .T. ) )
   IF Len( aFilter ) >= 1 .AND. aFilter[ 1 ] == "no"
      hb_ADel( aFilter, 1, .T. )
      lFilterNegative := .T.
   ELSE
      lFilterNegative := .F.
   ENDIF

   hProjectReqList := { => }

   FOR EACH tmp IN hProjectList
      hProjectReqList[ tmp:__enumKey() ] := tmp:__enumKey()
   NEXT

   IF ! Empty( aFilter )
      IF ! lFilterNegative
         hProjectReqList := { => }
      ENDIF
      FOR EACH cProject IN aFilter
         FOR EACH tmp IN hProjectList
            IF hb_FileMatch( hb_DirSepToOS( cProject ), hb_DirSepToOS( tmp:__enumKey() ) ) .OR. ;
               hb_FileMatch( hb_DirSepToOS( cProject ), hb_DirSepDel( hb_FNameDir( hb_DirSepToOS( tmp:__enumKey() ) ) ) )
               IF lFilterNegative
                  IF tmp:__enumKey() $ hProjectReqList
                     hb_HDel( hProjectReqList, tmp:__enumKey() )
                  ENDIF
               ELSE
                  hProjectReqList[ tmp:__enumKey() ] := tmp:__enumKey()
               ENDIF
            ENDIF
         NEXT
      NEXT
   ENDIF

   IF Empty( hProjectReqList )
      RETURN
   ENDIF

   /* Clearing envvars that may interact with hbmk2 */

   /* Saving original install dirs to our own variables */
   hb_SetEnv( "_HB_INSTALL_BIN", GetEnv( "HB_INSTALL_BIN" ) )
   hb_SetEnv( "_HB_INSTALL_LIB", GetEnv( "HB_INSTALL_LIB" ) )
   hb_SetEnv( "_HB_INSTALL_DYN", GetEnv( "HB_INSTALL_DYN" ) )
   hb_SetEnv( "_HB_INSTALL_INC", GetEnv( "HB_INSTALL_INC" ) )
   hb_SetEnv( "_HB_INSTALL_MAN", GetEnv( "HB_INSTALL_MAN" ) )
   hb_SetEnv( "_HB_INSTALL_ETC", GetEnv( "HB_INSTALL_ETC" ) )
   hb_SetEnv( "_HB_INSTALL_CONTRIB", GetEnv( "HB_INSTALL_CONTRIB" ) )

   /* Override hbmk2 autodetection. WARNING: Must be in sync with global.mk logic */
   hb_SetEnv( "HB_INSTALL_PREFIX", s_cRoot )
   hb_SetEnv( "HB_INSTALL_BIN", s_cRoot + "bin/" + GetEnv( "HB_PLATFORM" ) + "/" + GetEnv( "HB_COMPILER" ) + GetEnv( "HB_BUILD_NAME" ) )
   hb_SetEnv( "HB_INSTALL_LIB", s_cRoot + "lib/" + GetEnv( "HB_PLATFORM" ) + "/" + GetEnv( "HB_COMPILER" ) + GetEnv( "HB_BUILD_NAME" ) )
   hb_SetEnv( "HB_INSTALL_DYN" )
   hb_SetEnv( "HB_INSTALL_INC", s_cRoot + "include" )

   /* Start building */

   OutStd( hb_StrFormat( "! Started package %1$s...", sc_hActions[ nAction ] ) + hb_eol() )

   build_projects( nAction, hProjectList, hProjectReqList, "", .F. )

   OutStd( hb_StrFormat( "! Finished package %1$s...", sc_hActions[ nAction ] ) + hb_eol() )

   RETURN

STATIC PROCEDURE build_projects( nAction, hProjectList, hProjectReqList, cOptionsUser, lStdAlone )

   LOCAL aPairList
   LOCAL aSortedList

   LOCAL cOptions
   LOCAL lInstall
   LOCAL cMakeFlags

   LOCAL cProject
   LOCAL cProjectPath
   LOCAL lPrimary
   LOCAL lContainer

   LOCAL cDynSuffix

   LOCAL nErrorLevel

   /* Signal that we're doing a Harbour build */
   hb_SetEnv( "_HB_BUILD_", "yes" )

   /* Preprocessing */

   IF Len( hProjectReqList ) > 1
      OutStd( hb_StrFormat( "! Calculating build order for %1$d projects...", Len( hProjectReqList ) ) + hb_eol() )
   ENDIF

   aPairList := {}

   FOR EACH cProject IN hProjectReqList
      call_hbmk2_hbinfo( s_cBase + s_cHome + cProject, hProjectList[ cProject ] )
      DeptLinesToDeptPairList( aPairList, cProject, hProjectList[ cProject ][ "aDept" ] )
   NEXT

   aSortedList := TopoSort( aPairList )

   /* Add referenced project not present in our list and featuring an .hbp file */
   FOR EACH cProject IN aSortedList
      IF AddProject( hProjectList, @cProject )
         call_hbmk2_hbinfo( s_cBase + s_cHome + cProject, hProjectList[ cProject ] )
         hProjectList[ cProject ][ "lFromContainer" ] := NIL
      ENDIF
   NEXT

   /* Load project information for dependencies too
      (we need "cType" to decide about dynamic build) */
   IF GetEnv( "HB_BUILD_CONTRIB_DYN" ) == "yes"
      FOR EACH cProject IN aSortedList
         IF !( cProject $ hProjectReqList ) .AND. ;
            cProject $ hProjectList .AND. ;
            !( "lChecked" $ hProjectList[ cProject ] )
            call_hbmk2_hbinfo( s_cBase + s_cHome + cProject, hProjectList[ cProject ] )
         ENDIF
      NEXT
   ENDIF

   /* Convert action to hbmk2 options */

   cOptions := " -inc"
   SWITCH nAction
   CASE _ACT_INC_CLEAN
      cOptions += " -clean"
      EXIT
   CASE _ACT_INC_REBUILD
   CASE _ACT_INC_REBUILD_INST
      cOptions += " -rebuildall"
      EXIT
   ENDSWITCH

   cMakeFlags := GetEnv( "MAKEFLAGS" )
   IF " -j " $ " " + cMakeFlags + " "
      /* GNU Make uses job server to limit number of concurrent operations
       * We cannot read it from MAKEFLAGS so I set it to arbitrary value: 8
       */
      cOptions += " -jobs=8"
   ENDIF

   lInstall := ;
      nAction == _ACT_INC_INST .OR. ;
      nAction == _ACT_INC_REBUILD_INST

   hb_SetEnv( iif( lStdAlone, "_HB_BUILD_INSTALL_STDALONE", "_HB_BUILD_INSTALL" ), iif( lInstall, "yes", NIL ) )

   /* Build the dependencies and primary targets in sorted order */

   FOR EACH cProject IN aSortedList DESCEND
      IF cProject $ hProjectList

         cProjectPath := s_cBase + s_cHome + cProject
         lPrimary := cProject $ hProjectReqList
         lContainer := "lFromContainer" $ hProjectList[ cProject ]

         IF ( nErrorLevel := call_hbmk2( cProjectPath, iif( lPrimary .OR. lContainer, iif( lContainer, cOptions, cOptions + cOptionsUser ), " -inc" ), NIL ) ) == 0

            /* Build dynamic lib */
            IF GetEnv( "HB_BUILD_CONTRIB_DYN" ) == "yes" .AND. hProjectList[ cProject ][ "cType" ] == "hblib"
               /* Is this a platform where import libs are used? */
               IF "|" + hProjectList[ cProject ][ "cPlatform" ] + "|" $ "|win|dos|os2|"
                  IF Empty( hProjectList[ cProject ][ "cDynSuffix" ] )
                     cDynSuffix := "_dll"
                  ELSE
                     cDynSuffix := hProjectList[ cProject ][ "cDynSuffix" ]
                  ENDIF
               ELSE
                  cDynSuffix := hb_libExt()
               ENDIF
               call_hbmk2( cProjectPath, iif( lPrimary .OR. lContainer, iif( lContainer, cOptions, cOptions + cOptionsUser ), " -inc" ), cDynSuffix )
            ENDIF

            IF lPrimary .OR. lContainer

               /* Compile documentation */
               IF lInstall
                  mk_hbd( hb_FNameDir( hb_DirSepToOS( cProjectPath ) ) )
               ENDIF
            ENDIF
         ELSE
            /* Ignore certain non-fatal hbmk2 return values */
            IF nErrorLevel != 10 .AND. ;
               nErrorLevel != 20 .AND. ;
               nErrorLevel != 50
               ErrorLevel( nErrorLevel )
               EXIT
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE call_hbmk2_hbinfo( cProjectPath, hProject )

   LOCAL cStdOut
   LOCAL tmp
   LOCAL hInfo

   LOCAL nErrorLevel

   hProject[ "cType" ] := ""
   hProject[ "aDept" ] := {}
   hProject[ "lChecked" ] := NIL

   IF ( nErrorLevel := call_hbmk2( cProjectPath, " --hbinfo",,, @cStdOut ) ) == 0

      IF hb_jsonDecode( cStdOut, @hInfo ) == 0
         OutStd( "! Warning: Received invalid result from 'hbmk2 --hbinfo'" + hb_eol() )
      ENDIF

      hProject[ "cType" ] := hbmk2_hbinfo_getitem( hInfo, "targettype" )
      hProject[ "cOutputName" ] := hbmk2_hbinfo_getitem( hInfo, "outputname" )
      hProject[ "cDynSuffix" ] := hbmk2_hbinfo_getitem( hInfo, "dynsuffix" )
      hProject[ "cPlatform" ] := hbmk2_hbinfo_getitem( hInfo, "platform" )

      FOR EACH tmp IN hb_ATokens( hbmk2_hbinfo_getitem( hInfo, "hbctree" ), Chr( 10 ) )
         IF ! Empty( tmp )
#ifdef __PLATFORM__DOS
            /* Ignore long filenames on MS-DOS hosts */
            IF Len( hb_FNameName( LTrim( tmp ) ) ) > 8
               LOOP
            ENDIF
#endif
            AAdd( hProject[ "aDept" ], { "nDepth" => Len( tmp ) - Len( LTrim( tmp ) ), ;
               "cFileName_HBP" => StrTran( hb_PathNormalize( hb_PathJoin( s_cRebase, hb_FNameExtSet( hb_DirSepToOS( LTrim( tmp ) ), ".hbp" ) ) ), "\", "/" ) } )
         ENDIF
      NEXT
   ELSE
      OutStd( hb_StrFormat( "! Warning: 'hbmk2 %1$s --hbinfo' failed with exit code %2$d", cProjectPath, nErrorLevel ) + hb_eol() )
   ENDIF

   RETURN

STATIC FUNCTION hbmk2_hbinfo_getitem( hInfo, cItem )
   RETURN iif( HB_ISHASH( hInfo ), hb_HGetDef( hInfo, cItem, "" ), "" )

STATIC FUNCTION call_hbmk2( cProjectPath, cOptionsPre, cDynSuffix, cStdErr, cStdOut )

   LOCAL nErrorLevel
   LOCAL cOptionsLibDyn := ""
   LOCAL cCommand

   /* Making sure that user settings do not interfere with the std build process. */
   hb_SetEnv( "HBMK_OPTIONS" )
   hb_SetEnv( "HARBOUR" )
   hb_SetEnv( "HARBOURCMD" )
   hb_SetEnv( "CLIPPER" )
   hb_SetEnv( "CLIPPERCMD" )

   IF cDynSuffix != NIL
      hb_SetEnv( "_HB_DYNSUFF", cDynSuffix ) /* Request dll version of Harbour contrib dependencies (the implibs) to be linked (experimental) */

      cOptionsPre += " -hbdyn"

      IF hb_FileExists( hb_FNameExtSet( cProjectPath, ".hbc" ) )
         cOptionsLibDyn += " " + hb_FNameExtSet( cProjectPath, ".hbc" )
      ENDIF
   ELSE
      hb_SetEnv( "_HB_DYNSUFF" )
   ENDIF

   hb_SetEnv( "_HB_CONTRIB_SUBDIR", hb_FNameDir( hb_DirSepToOS( cProjectPath ) ) )

   cCommand := s_cBinDir + "hbmk2" + ;
      " -lang=en -quiet -width=0 -autohbm-" + ;
      " @" + StrTran( s_cHome + "hbpre", "\", "/" ) + ;
      cOptionsPre + ;
      " " + StrTran( cProjectPath, "\", "/" ) + ;
      " @" + StrTran( s_cHome, "\", "/" ) + "hbpost" + ;
      cOptionsLibDyn

   IF PCount() >= 4
      nErrorLevel := hb_processRun( cCommand,, @cStdOut, @cStdErr )
   ELSE
      nErrorLevel := mk_hb_processRun( cCommand )
   ENDIF

   RETURN nErrorLevel

STATIC FUNCTION mk_hb_processRun( cCommand, ... )

   OutStd( cCommand + hb_eol() )

   RETURN hb_processRun( cCommand, ... )

STATIC FUNCTION mk_hbd( cDir )

   LOCAL cName
   LOCAL cDocDir
   LOCAL tmp

   LOCAL aErrMsg
   LOCAL aEntry

   IF ! Empty( cDocDir := GetEnv( "HB_INSTALL_DOC" ) ) .AND. ! cDocDir == "no"

      IF Empty( cName := DirGetName( cDir ) )
         cName := "harbour"
      ENDIF

      aErrMsg := {}
      aEntry := __hbdoc_LoadDir( cDir, cName, aErrMsg )

      FOR EACH tmp IN aErrMsg
         OutErr( hb_StrFormat( "! %1$s", tmp ) + hb_eol() )
      NEXT

      IF ! Empty( aEntry )
         cName := hb_DirSepToOS( cDocDir ) + hb_ps() + cName + ".hbd"
         IF __hbdoc_SaveHBD( cName, aEntry )
            OutStd( hb_StrFormat( "! Compiled documentation: %1$s <= %2$s", cName, cDir ) + hb_eol() )
            RETURN .T.
         ELSE
            OutErr( hb_StrFormat( "! Error: Saving '%1$s'", cName ) + hb_eol() )
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION AScanL( aArray, cString )
   RETURN AScan( aArray, {| tmp | Lower( tmp ) == cString } )

STATIC FUNCTION DirGetName( cDir )

   LOCAL cName := hb_FNameName( hb_DirSepDel( cDir ) )

   IF Empty( cName ) .OR. cName == "." .OR. cName == ".."
      RETURN ""
   ENDIF

   RETURN cName

/* Convert indented list of line to tree / list of parent-child pairs */
STATIC PROCEDURE DeptLinesToDeptPairList( aPairList, cParent, aFlatTree )

   LOCAL hFlatTreeElement
   LOCAL hNode, hNewNode, tmp
   LOCAL nLevel, nDepth

   AddDeptPair( aPairList, "", cParent )

   hNode := { "child" => {}, "name" => cParent, "parent" => }
   nLevel := 0
   FOR EACH hFlatTreeElement IN aFlatTree
      /* Min() protects against jumping more than one level down in one step */
      nDepth := Min( hFlatTreeElement[ "nDepth" ], nLevel + 1 )
      hNewNode := { "child" => {}, "name" => hFlatTreeElement[ "cFileName_HBP" ], "cargo" => hFlatTreeElement }
      IF nDepth > nLevel
         hNode := ATail( hNode[ "child" ] )
      ELSEIF nDepth < nLevel
         FOR tmp := nDepth + 1 TO nLevel
            hNode := hNode[ "parent" ]
         NEXT
      ENDIF
      hNewNode[ "parent" ] := hNode
      AAdd( hNode[ "child" ], hNewNode )
      nLevel := nDepth
      AddDeptPair( aPairList, hNewNode[ "parent" ][ "name" ], hNewNode[ "name" ] )
   NEXT

   RETURN

/* Add parent-child dependency to the list */
STATIC PROCEDURE AddDeptPair( aPairList, cParent, cChild )

   IF AScan( aPairList, {| tmp | tmp[ 1 ] == cParent .AND. tmp[ 2 ] == cChild } ) == 0
      AAdd( aPairList, { cParent, cChild } )
   ENDIF

   RETURN

/* Topological sort of the dependency graph */
STATIC FUNCTION TopoSort( aEdgeList )

   LOCAL aList := {}
   LOCAL hTopNodes := { => }

   LOCAL n, m
   LOCAL tmp

   FOR EACH n IN aEdgeList
      IF AScan( aEdgeList, {| tmp | tmp[ 2 ] == n[ 1 ] } ) == 0
         hTopNodes[ n[ 1 ] ] := NIL
      ENDIF
   NEXT

   DO WHILE ! Empty( hTopNodes )
      n := hb_HKeyAt( hTopNodes, 1 )
      hb_HDelAt( hTopNodes, 1 )

      IF ! Empty( n )
         AAdd( aList, n )
      ENDIF

      FOR EACH tmp IN aEdgeList
         IF tmp[ 1 ] == n
            m := tmp[ 2 ]
            tmp[ 1 ] := tmp[ 2 ] := NIL /* set to invalid value. TOOPT: Delete this member from list */
            IF AScan( aEdgeList, {| tmp | tmp[ 2 ] == m } ) == 0
               hTopNodes[ m ] := NIL
            ENDIF
         ENDIF
      NEXT
   ENDDO

   FOR EACH tmp IN aEdgeList
      IF !( tmp[ 1 ] == NIL .AND. tmp[ 2 ] == NIL )
         OutStd( hb_StrFormat( "! Warning: Circular reference in dependency tree (%1$s - %2$s)", tmp[ 1 ], tmp[ 2 ] ) + hb_eol() )
      ENDIF
   NEXT

   RETURN aList

STATIC FUNCTION AddProject( hProjectList, cFileName )

   LOCAL cDir
   LOCAL cName
   LOCAL cExt

   IF ! Empty( cFileName )

      cFileName := hb_DirSepToOS( AllTrim( cFileName ) )

      hb_FNameSplit( cFileName, @cDir, @cName, @cExt )

      IF Empty( cName )
         cName := DirGetName( cDir )
      ELSEIF Empty( cDir )
         cDir := cName
      ENDIF
      IF Empty( cExt )
         cExt := ".hbp"
      ENDIF

      cFileName := hb_FNameMerge( cDir, cName, cExt )

      IF hb_FileExists( s_cBase + s_cHome + cFileName )
         cFileName := StrTran( cFileName, "\", "/" )
         IF ! cFileName $ hProjectList
            hProjectList[ cFileName ] := { => }
            RETURN .T.
         ENDIF
      ENDIF

   ENDIF

   RETURN .F.

/* Build all contribs that have a .hbp file matching the
   name of its contrib subdir. Also support contribs
   with multiple subprojects if it has a 'makesub.txt'
   text file with a list of those subprojects. */
STATIC PROCEDURE LoadProjectListAutomatic( hProjectList, cDir )

   LOCAL aFile
   LOCAL tmp

   cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )

   FOR EACH aFile IN Directory( cDir, "D" )
      IF "D" $ aFile[ F_ATTR ] .AND. !( aFile[ F_NAME ] == "." ) .AND. !( aFile[ F_NAME ] == ".." )
         IF hb_FileExists( cDir + ( tmp := aFile[ F_NAME ] + hb_ps() + hb_FNameExtSet( aFile[ F_NAME ], ".hbp" ) ) )
            AddProject( hProjectList, tmp )
         ENDIF
         IF hb_FileExists( tmp := ( cDir + aFile[ F_NAME ] + hb_ps() + "makesub.txt" ) )
            FOR EACH tmp IN hb_ATokens( StrTran( hb_MemoRead( tmp ), Chr( 13 ) ), Chr( 10 ) )
               IF ! Empty( tmp )
                  AddProject( hProjectList, aFile[ F_NAME ] + hb_ps() + tmp )
               ENDIF
            NEXT
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE LoadProjectListFromString( hProjectList, cString )

   LOCAL cItem

   FOR EACH cItem IN hb_ATokens( cString,, .T. )
      AddProject( hProjectList, cItem )
   NEXT

   RETURN
