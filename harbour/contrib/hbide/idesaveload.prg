/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               28Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_saveSettings( oIde )
   LOCAL cPath

   hb_fNameSplit( oIde:cProjIni, @cPath )
   hbqt_QMainWindow_saveSettings( cPath + "settings.ide", "hbidesettings", oIde:oDlg:oWidget:pPtr )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_restSettings( oIde )
   LOCAL cPath

   hb_fNameSplit( oIde:cProjIni, @cPath )
   hbqt_QMainWindow_restSettings( cPath + "settings.ide", "hbidesettings", oIde:oDlg:oWidget:pPtr )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_getEditInfoAsString( oEdit )
   LOCAL qHScr   := QScrollBar():configure( oEdit:qEdit:horizontalScrollBar() )
   LOCAL qVScr   := QScrollBar():configure( oEdit:qEdit:verticalScrollBar() )
   LOCAL qCursor := QTextCursor():configure( oEdit:qEdit:textCursor() )
   LOCAL cBMarks := hbide_nArray2string( oEdit:oEdit:aBookMarks )

   RETURN hbide_pathNormalized( oEdit:sourceFile, .f. ) +  ","  + ;
                          hb_ntos( qCursor:position() ) +  ","  + ;
                          hb_ntos( qHScr:value()      ) +  ","  + ;
                          hb_ntos( qVScr:value()      ) +  ","  + ;
                          oEdit:cTheme                  +  ","  + ;
                          oEdit:cView                   +  ","  + ;
                          cBMarks                       +  ","

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveINI( oIde )
   LOCAL j, nTab, pTab, n, txt_, oEdit, nTabs, nn, s

HB_TRACE( HB_TR_ALWAYS, "hbide_saveINI( oIde )", 0, oIde:nRunMode, oIde:cProjIni )
   IF oIde:nRunMode != HBIDE_RUN_MODE_INI
      RETURN Nil
   ENDIF

   txt_:= {}
   //    Properties
   aadd( txt_, "[HBIDE]" )
   aadd( txt_, " " )
   aadd( txt_, "MainWindowGeometry="        + hbide_posAndSize( oIde:oDlg:oWidget )             )
   aadd( txt_, "ProjectTreeVisible="        + iif( oIde:lProjTreeVisible, "YES", "NO" )         )
   aadd( txt_, "FunctionListVisible="       + iif( oIde:lDockRVisible, "YES", "NO" )            )
   aadd( txt_, "RecentTabIndex="            + hb_ntos( oIde:qTabWidget:currentIndex() )         )
   aadd( txt_, "CurrentProject="            + oIde:cWrkProject                                  )
   aadd( txt_, "GotoDialogGeometry="        + oIde:aIni[ INI_HBIDE, GotoDialogGeometry ]        )
   aadd( txt_, "FindDialogGeometry="        + oIde:aIni[ INI_HBIDE, FindDialogGeometry ]        )
   aadd( txt_, "CurrentTheme="              + oIde:cWrkTheme                                    )
   aadd( txt_, "CurrentCodec="              + oIde:cWrkCodec                                    )
   aadd( txt_, "PathMk2="                   + oIde:aIni[ INI_HBIDE, PathMk2            ]        )
   aadd( txt_, "PathEnv="                   + oIde:aIni[ INI_HBIDE, PathEnv            ]        )
   aadd( txt_, "CurrentEnvironment="        + oIde:cWrkEnvironment                              )
   aadd( txt_, "CurrentFind="               + oIde:cWrkFind                                     )
   aadd( txt_, "CurrentFolderFind="         + oIde:cWrkFolderFind                               )
   aadd( txt_, "CurrentReplace="            + oIde:cWrkReplace                                  )
   aadd( txt_, "CurrentView="               + oIde:cWrkView                                     )
   aadd( txt_, "CurrentHarbour="            + oIde:cWrkHarbour                                  )
   aadd( txt_, "CurrentShortcuts="          + oIde:cPathShortcuts                               )
   aadd( txt_, "TextFileExtensions="        + oIde:cTextExtensions                              )
   aadd( txt_, "FindInFilesDialogGeometry=" + oIde:aIni[ INI_HBIDE, FindInFilesDialogGeometry ] )
   aadd( txt_, "FindInFilesDialogGeometry=" + oIde:aIni[ INI_HBIDE, FindInFilesDialogGeometry ] )
   aadd( txt_, "CurrentLineHighlightMode="  + iif( oIde:lCurrentLineHighlightEnabled, "YES", "NO" ) )
   aadd( txt_, "LineNumbersDisplayMode="    + iif( oIde:lLineNumbersVisible, "YES", "NO" )      )
   aadd( txt_, "HorzRulerDisplayMode="      + iif( oIde:lHorzRulerVisible, "YES", "NO" )        )
   aadd( txt_, "ToolsDialogGeometry="       + oIde:aIni[ INI_HBIDE, ToolsDialogGeometry ]       )
   aadd( txt_, "IdeTheme="                  + oIde:aIni[ INI_HBIDE, IdeTheme            ]       )
   aadd( txt_, "IdeAnimated="               + oIde:aIni[ INI_HBIDE, IdeAnimated         ]       )
   aadd( txt_, " " )

   aadd( txt_, "[PROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aProjects )
      aadd( txt_, "project_" + hb_ntos( n ) + "=" + hbide_pathNormalized( oIde:aProjects[ n, 2 ] ) )
   NEXT
   aadd( txt_, " " )

   /*-------------------   FILES   -------------------*/
   aadd( txt_, "[FILES]" )
   aadd( txt_, " " )
   nn := 0
   FOR j := 2 TO len( oIde:aViews )
      oIde:lClosing := .t.
      oIde:oDK:setView( oIde:aViews[ j ]:oWidget:objectName() )

      nTabs := oIde:qTabWidget:count()
      FOR n := 1 TO nTabs
         pTab  := oIde:qTabWidget:widget( n - 1 )
         nTab  := ascan( oIde:aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ 1 ]:oWidget:pPtr, pTab ) } )
         oEdit := oIde:aTabs[ nTab, TAB_OEDITOR ]

         IF !Empty( oEdit:sourceFile ) .AND. !( ".ppo" == lower( oEdit:cExt ) )
            IF oEdit:lLoaded
               aadd( txt_, "file_" + hb_ntos( ++nn ) + "=" + hbide_getEditInfoAsString( oEdit ) )

            ELSE
               aadd( txt_, "file_" + hb_ntos( ++nn ) + "=" + hbide_pathNormalized( oEdit:sourceFile, .f. ) + "," + ;
                           hb_ntos( oEdit:nPos  ) +  ","  + ;
                           hb_ntos( oEdit:nHPos ) +  ","  + ;
                           hb_ntos( oEdit:nVPos ) +  ","  + ;
                           oEdit:cTheme           +  ","  + ;
                           oEdit:cView            +  ","  + ;
                           hbide_nArray2string( oEdit:oEdit:aBookMarks ) +  ","  )
            ENDIF
         ENDIF
      NEXT
   NEXT

   aadd( txt_, " " )

   aadd( txt_, "[FIND]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_FIND ] )
      aadd( txt_, "find_" + hb_ntos( n ) + "=" + oIde:aIni[ INI_FIND, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[REPLACE]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_REPLACE ] )
      aadd( txt_, "replace_" + hb_ntos( n ) + "=" + oIde:aIni[ INI_REPLACE, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTFILES]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_RECENTFILES ] )
      aadd( txt_, "recentfile_" + hb_ntos( n ) + "=" + hbide_pathNormalized( oIde:aIni[ INI_RECENTFILES, n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_RECENTPROJECTS ] )
      aadd( txt_, "recentproject_" + hb_ntos( n ) + "=" + hbide_pathNormalized( oIde:aIni[ INI_RECENTPROJECTS, n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FOLDERS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_FOLDERS ] )
      aadd( txt_, "folder_" + hb_ntos( n ) + "=" + hbide_pathNormalized( oIde:aIni[ INI_FOLDERS, n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[VIEWS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_VIEWS ] )
      aadd( txt_, "view_" + hb_ntos( n ) + "=" + oIde:aIni[ INI_VIEWS, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TAGGEDPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_TAGGEDPROJECTS ] )
      aadd( txt_, "taggedproject_" + hb_ntos( n ) + "=" + oIde:aIni[ INI_TAGGEDPROJECTS, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TOOLS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_TOOLS ] )
      s := oIde:aIni[ INI_TOOLS, n, 1 ] + "," + oIde:aIni[ INI_TOOLS, n, 2 ] + "," + oIde:aIni[ INI_TOOLS, n, 3 ] + "," + ;
           oIde:aIni[ INI_TOOLS, n, 4 ] + "," + oIde:aIni[ INI_TOOLS, n, 5 ] + "," + oIde:aIni[ INI_TOOLS, n, 6 ] + "," + ;
           oIde:aIni[ INI_TOOLS, n, 7 ] + "," + oIde:aIni[ INI_TOOLS, n, 8 ] + "," + oIde:aIni[ INI_TOOLS, n, 9 ] + "," + ;
           oIde:aIni[ INI_TOOLS, n, 10 ]
      aadd( txt_, "tool_" + hb_ntos( n ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[USERTOOLBARS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_USERTOOLBARS ] )
      s := hbide_array2string( oIde:aIni[ INI_USERTOOLBARS, n ], "," )
      aadd( txt_, "usertoolbars_" + hb_ntos( n ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[General]" )
   aadd( txt_, " " )

   hbide_createTarget( oIde:cProjIni, txt_ )

   RETURN hbide_saveSettings( oIde )

/*----------------------------------------------------------------------*/

FUNCTION hbide_getIniPath( cHbideIni )
   LOCAL cPath, cIni

   IF empty( cHbideIni )
      IF ! hb_FileExists( cIni := hb_dirBase() + "hbide.ini" )
      #if defined( __PLATFORM__WINDOWS )
         cPath := hbide_DirAddPathSep( GetEnv( "APPDATA" ) ) + "hbide\"
      #elif defined( __PLATFORM__UNIX )
         cPath := hbide_DirAddPathSep( GetEnv( "HOME" ) ) + ".hbide/"
      #elif defined( __PLATFORM__OS2 )
         cPath := hbide_DirAddPathSep( GetEnv( "HOME" ) ) + ".hbide/"
      #endif
         IF ! hb_dirExists( cPath )
            MakeDir( cPath )
         ENDIF
         cIni := cPath + "hbide.ini"
      ENDIF
   ELSE
      cIni := cHbideIni
   ENDIF

   RETURN cIni

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadINI( oIde, cHbideIni )
   LOCAL aElem, s, n, nPart, cKey, cVal, a_
   LOCAL aIdeEle := { "mainwindowgeometry" , "projecttreevisible"  , "projecttreegeometry", ;
                      "functionlistvisible", "functionlistgeometry", "recenttabindex"     , ;
                      "currentproject"     , "gotodialoggeometry"  , "propsdialoggeometry", ;
                      "finddialoggeometry" , "themesdialoggeometry", "currenttheme"       , ;
                      "currentcodec"       , "pathmk2"             , "pathenv"            , ;
                      "currentenvironment" , "findinfilesdialoggeometry", "currentfind"   , ;
                      "currentreplace"     , "currentfolderfind"   , "currentview"        , ;
                      "currentharbour"     , "currentshortcuts"    , "textfileextensions" , ;
                      "currentlinehighlightmode", "linenumbersdisplaymode", "horzrulerdisplaymode", ;
                      "toolsdialoggeometry", "idetheme"            , "ideanimated"        ;
                    }

   #if 0
   IF empty( cHbideIni )
      IF hb_fileExists( "hbide.ini" )
         /* Please Check for *nixes */
         //cHbideIni := hb_curDrive() + hb_osDriveSeparator() + hb_osPathSeparator() + CurDir() + hb_osPathSeparator() + "hbide.ini"
         cHbideIni := "hbide.ini"
      ELSE
         cHbideIni := hb_dirBase() + "hbide.ini"
      ENDIF
   ENDIF
   oIde:cProjIni := cHbideIni
   #endif
   oIde:cProjIni := hbide_getIniPath( cHbideIni )

   IF hb_FileExists( oIde:cProjIni )
      aElem := hbide_readSource( oIde:cProjIni )

      FOR EACH s IN aElem

         s := alltrim( s )
         IF !empty( s )
            /*
             * OPT: Optimizations using SWITCH and converting section name to
             * Uppercase - if the user change the name of the section in the file
             * .INI manually, this ensures that the HBIDE continue finding the
             * section.
             * 01/01/2010 - 16:38:22 - vailtom
             */
            SWITCH Upper( s )

            CASE "[GENERAL]"
               nPart := INI_GENERAL
               EXIT
            CASE "[HBIDE]"
               nPart := INI_HBIDE
               EXIT
            CASE "[PROJECTS]"
               nPart := INI_PROJECTS
               EXIT
            CASE "[FILES]"
               nPart := INI_FILES
               EXIT
            CASE "[FIND]"
               nPart := INI_FIND
               EXIT
            CASE "[REPLACE]"
               nPart := INI_REPLACE
               EXIT
            CASE "[RECENTFILES]"
               nPart := INI_RECENTFILES
               EXIT
            CASE "[RECENTPROJECTS]"
               nPart := INI_RECENTPROJECTS
               EXIT
            CASE "[FOLDERS]"
               nPart := INI_FOLDERS
               EXIT
            CASE "[VIEWS]"
               nPart := INI_VIEWS
               EXIT
            CASE "[TAGGEDPROJECTS]"
               nPart := INI_TAGGEDPROJECTS
               EXIT
            CASE "[TOOLS]"
               nPart := INI_TOOLS
               EXIT
            CASE "[USERTOOLBARS]"
               nPart := INI_USERTOOLBARS
               EXIT
            OTHERWISE
                /*
                * If none of the previous sections are valid, do not let it
                * process. This prevents the HBIDE read a section that is
                * commented out or is invalid in the file .ini - For example,
                * open the file .ini file and change the name of the [PROJECTS]
                * for '[* PROJECTS]' and see how it behaves incorrectly.
                * 01/01/2010 - 18:09:40 - vailtom
                */
               DO CASE
               CASE Left( s, 1 ) $ '#['
                  * Nothing todo!

               CASE nPart == INI_GENERAL
                  * Qt Setttings, do nothing.

               CASE nPart == INI_HBIDE
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     cKey := lower( cKey )
                     IF ( n := ascan( aIdeEle, cKey ) ) > 0
                        oIde:aIni[ nPart, n ] := cVal  /* Further process */
                     ENDIF
                  ENDIF

               CASE nPart == INI_PROJECTS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( oIde:aIni[ nPart ], cVal )
                  ENDIF

               CASE nPart == INI_FILES
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseSourceComponents( cVal )
                     IF !Empty( a_[ 1 ] )
                        aadd( oIde:aIni[ nPart ], a_ )
                     ENDIF
                  ENDIF

               CASE nPart == INI_FIND
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( oIde:aIni[ nPart ], cVal )
                  ENDIF

               CASE nPart == INI_REPLACE
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( oIde:aIni[ nPart ], cVal )
                  ENDIF

               CASE nPart == INI_RECENTPROJECTS .OR. ;
                    nPart == INI_RECENTFILES

                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     IF Len( oIde:aIni[ nPart ] ) < 25
                        cVal := hbide_pathNormalized( cVal, .f. )
                        IF aScan( oIde:aIni[ nPart ], {|e| hbide_pathNormalized( e, .f. ) == cVal } ) == 0
                           AAdd( oIde:aIni[ nPart ], cVal )
                        ENDIF
                     ENDIF
                  ENDIF

               CASE nPart == INI_FOLDERS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( oIde:aIni[ nPart ], cVal )
                  ENDIF

               CASE nPart == INI_VIEWS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( oIde:aIni[ nPart ], cVal )
                  ENDIF

               CASE nPart == INI_TAGGEDPROJECTS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( oIde:aIni[ nPart ], cVal )
                  ENDIF

               CASE nPart == INI_TOOLS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseToolComponents( cVal )
                     aadd( oIde:aIni[ nPart ], a_ )
                  ENDIF

               CASE nPart == INI_USERTOOLBARS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseUserToolbarComponents( cVal )
                     aadd( oIde:aIni[ nPart ], a_ )
                  ENDIF

               ENDCASE
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadSkltns( oIde, cPathSkltns )
   LOCAL cPath, s, n, cSkltn, cCode

   IF empty( cPathSkltns )
      hb_fNameSplit( oIde:cProjIni, @cPath )
      cPath += "hbide.skl"

      IF hb_fileExists( cPath )
         cPathSkltns := cPath
      ELSE
         cPathSkltns := hb_dirBase() + "hbide.skl"
      ENDIF
   ENDIF
   oIde:cPathSkltns := cPathSkltns

   IF hb_fileExists( cPathSkltns )
      s := hb_memoread( cPathSkltns )

      DO WHILE .t.
         IF ( n := at( "<", s ) ) == 0
            EXIT
         ENDIF
         s := substr( s, n + 1 )
         IF ( n := at( ">", s ) ) == 0
            EXIT
         ENDIF
         cSkltn := substr( s, 1, n - 1 )
         s := substr( s, n + 1 )
         IF ( n := at( "</" + cSkltn + ">", s ) ) > 0
            cCode := substr( s, 1, n - 1 )
            cCode := alltrim( cCode )
            IF left( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 2 )
            ENDIF
            IF left( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 2 )
            ENDIF
            IF right( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 1, len( cCode ) - 1 )
            ENDIF
            IF right( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 1, len( cCode ) - 1 )
            ENDIF

            aadd( oIde:aSkltns, { cSkltn, cCode } )
            s := substr( s, n + len( "</" + cSkltn + ">" ) )
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveSkltns( oIde )
   LOCAL a_, txt_:= {}

   FOR EACH a_ IN oIde:aSkltns
      aadd( txt_, "<" + a_[ 1 ] + ">" )
      aeval( hbide_memoToArray( a_[ 2 ] ), {|e| aadd( txt_, e ) } )
      aadd( txt_, "</" + a_[ 1 ] + ">" )
      aadd( txt_, "" )
   NEXT

   RETURN hbide_createTarget( oIde:cPathSkltns, txt_ )

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadShortcuts( oIde, cFileShortcuts )
   LOCAL cPath, a_:= {}

   IF empty( cFileShortcuts )
      cFileShortcuts := oIde:cPathShortcuts
      IF empty( cFileShortcuts )
         cFileShortcuts := oIde:cProjIni
      ENDIF

      hb_fNameSplit( cFileShortcuts, @cPath )
      cPath += "hbide.scu"

      IF hb_fileExists( cPath )
         cFileShortcuts := cPath
      ELSE
         cFileShortcuts := hb_dirBase() + "hbide.scu"
      ENDIF
   ENDIF
   oIde:cPathShortcuts := cFileShortcuts

   IF hb_fileExists( cFileShortcuts )
      a_:= hb_deSerialize( hb_memoread( cFileShortcuts ) )
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveShortcuts( oIde, a_, cFileShortcuts )
   LOCAL cPath

   IF empty( cFileShortcuts )
      cFileShortcuts := oIde:cPathShortcuts
      IF empty( cFileShortcuts )
         cFileShortcuts := oIde:cProjIni
      ENDIF

      hb_fNameSplit( cFileShortcuts, @cPath )
      cPath += "hbide.scu"

      IF hb_fileExists( cPath )
         cFileShortcuts := cPath
      ELSE
         cFileShortcuts := hb_dirBase() + "hbide.scu"
      ENDIF
   ENDIF

   hb_memowrit( cFileShortcuts, hb_serialize( a_ ) )

   RETURN hb_fileExists( cFileShortcuts )

/*----------------------------------------------------------------------*/
//                             Class IdeSetup
/*----------------------------------------------------------------------*/

CLASS IdeSetup INHERIT IdeObject

   DATA   qOrgPalette
   DATA   aItems                                  INIT {}
   DATA   aTree                                   INIT { "General", "Selections", "Font", "Paths", "Variables", "Dictionaries" }
   DATA   aStyles                                 INIT { "default", "cleanlooks", "windows", "windowsxp", ;
                                                         "windowsvista", "cde", "motif", "plastique", "macintosh" }

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( cEvent, p )
   METHOD buildTree()
   METHOD setSystemStyle( cStyle )
   METHOD setBaseColor()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeSetup:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:destroy()

   IF !empty( ::oUI )
      ::disConnect( ::oUI:q_buttonClose, "clicked()"                )
      ::disConnect( ::oUI:q_treeWidget , "itemSelectionChanged()"   )
      ::disconnect( ::oUI:q_comboStyle , "currentIndexChanged(int)" )

      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:show()
   LOCAL cStyle

   IF empty( ::oUI )
      ::oUI := HbQtUI():new( hbide_uic( "setup" ), ::oDlg:oWidget ):build()

      ::oUI:setMaximumWidth( ::oUI:width() )
      ::oUI:setMinimumWidth( ::oUI:width() )
      ::oUI:setMaximumHeight( ::oUI:height() )
      ::oUI:setMinimumHeight( ::oUI:height() )

      ::oUI:q_treeWidget:setHeaderHidden( .t. )
      ::oUI:q_treeWidget:setIconSize( QSize():new( 12,12 ) )
      ::oUI:q_treeWidget:setIndentation( 12 )

      ::oUI:q_buttonAdd :setIcon( hbide_image( "dc_plus"   ) )
      ::oUI:q_buttonDel :setIcon( hbide_image( "dc_delete" ) )
      ::oUI:q_buttonUp  :setIcon( hbide_image( "dc_up"     ) )
      ::oUI:q_buttonDown:setIcon( hbide_image( "dc_down"   ) )


      ::oUI:q_buttonPathIni      :setIcon( hbide_image( "open" ) )
      ::oUI:q_buttonPathHbmk2    :setIcon( hbide_image( "open" ) )
      ::oUI:q_buttonPathSnippets :setIcon( hbide_image( "open" ) )
      ::oUI:q_buttonPathEnv      :setIcon( hbide_image( "open" ) )
      ::oUI:q_buttonPathShortcuts:setIcon( hbide_image( "open" ) )
      ::oUI:q_buttonPathThemes   :setIcon( hbide_image( "open" ) )

      ::buildTree()

      ::connect( ::oUI:q_buttonClose, "clicked()"             , {|| ::execEvent( "buttonClose_clicked" ) } )
      ::connect( ::oUI:q_treeWidget , "itemSelectionChanged()", {|| ::execEvent( "treeWidget_itemSelectionChanged" ) } )

      ::oUI:q_treeWidget:setCurrentItem( ::aItems[ 2 ] ) /* General */

      FOR EACH cStyle IN ::aStyles
         ::oUI:q_comboStyle:addItem( cStyle )
      NEXT
      ::oUI:q_comboStyle:setCurrentIndex( 0 )
      ::connect( ::oUI:q_comboStyle, "currentIndexChanged(int)", {|p| ::execEvent( "comboStyle_currentIndexChanged", p ) } )

      ::oUI:q_checkAnimated:setChecked( val( ::oIde:aINI[ INI_HBIDE, IdeAnimated ] ) > 0 )
      ::connect( ::oUI:q_checkAnimated, "stateChanged(int)", {|i| ::execEvent( "checkAnimated_stateChanged", i ) } )

      ::oUI:hide()
   ENDIF

   ::oUI:exec()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:execEvent( cEvent, p )
   LOCAL qItem, nIndex

   SWITCH cEvent
   CASE "checkAnimated_stateChanged"
      ::oDK:animateComponents( iif( p == 0, 0, 1 ) )
      EXIT
   CASE "treeWidget_itemSelectionChanged"
      qItem  := QTreeWidgetItem():from( ::oUI:q_treeWidget:currentItem() )
      IF ( nIndex := ascan( ::aTree, qItem:text() ) ) > 0
         ::oUI:q_stackedWidget:setCurrentIndex( nIndex - 1 )
      ENDIF
      EXIT

   CASE "buttonClose_clicked"
      ::oUI:done( 1 )
      EXIT

   CASE "comboStyle_currentIndexChanged"
      IF ( nIndex := ::oUI:q_comboStyle:currentIndex() ) > -1
         ::oIde:aINI[ INI_HBIDE, IdeTheme ] := ::aStyles[ nIndex + 1 ]
         ::setSystemStyle( ::aStyles[ nIndex + 1 ] )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:buildTree()
   LOCAL oRoot, oChild, s

   oRoot := QTreeWidgetItem():new()
   oRoot:setText( 0, "Parts" )
// oRoot:setIcon( 0, hbide_image( "dc_home" ) )
   oRoot:setToolTip( 0, "Parts" )

   ::oUI:q_treeWidget:addTopLevelItem( oRoot )

   aadd( ::aItems, oRoot )

   FOR EACH s IN ::aTree
      oChild := QTreeWidgetItem():new()
      oChild:setText( 0, s )
      oChild:setToolTip( 0, s )
      oRoot:addChild( oChild )
      aadd( ::aItems, oChild )
   NEXT

   oRoot:setExpanded( .t. )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:setSystemStyle( cStyle )
   LOCAL oApp, qFactory

   IF !empty( cStyle )
      oApp     := QApplication():new()
      qFactory := QStyleFactory():new()
      oApp:setStyle( qFactory:create( cStyle ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:setBaseColor()
   #if 0
   LOCAL qPalette, oApp, qBrush, qColor

   oApp := QApplication():new()

   ::qOrgPalette := QPalette():from( oApp:palette() )

   qColor := QColor():new( Qt_red )
   qBrush := QBrush():new( "QColor", qColor )

   qPalette := QPalette():from( oApp:palette() )
   qPalette:setBrush( QPalette_Window, qBrush )
   qPalette:setColor( QPalette_Window, qColor )
   qPalette:setColor( QPalette_Base, qColor )

   oApp:setPalette( qPalette )
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
//
//                            Class IdeINI
//
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS IdeINI INHERIT IdeObject

   DATA   aINI                                    INIT {}

   DATA   cMainWindowGeometry                     INIT  ""
   DATA   cGotoDialogGeometry                     INIT  ""
   DATA   cFindDialogGeometry                     INIT  ""
   DATA   cToolsDialogGeometry                    INIT  ""
   DATA   cRecentTabIndex                         INIT  ""
   DATA   cPathMk2                                INIT  ""
   DATA   cPathEnv                                INIT  ""
   DATA   cCurrentProject                         INIT  ""
   DATA   cCurrentTheme                           INIT  ""
   DATA   cCurrentCodec                           INIT  ""
   DATA   cCurrentEnvironment                     INIT  ""
   DATA   cCurrentFind                            INIT  ""
   DATA   cCurrentFolderFind                      INIT  ""
   DATA   cCurrentReplace                         INIT  ""
   DATA   cCurrentView                            INIT  ""
   DATA   cCurrentHarbour                         INIT  ""
   DATA   cCurrentShortcuts                       INIT  ""
   DATA   cCurrentLineHighlightMode               INIT  ""
   DATA   cTextFileExtensions                     INIT  ""
   DATA   cLineNumbersDisplayMode                 INIT  ""
   DATA   cHorzRulerDisplayMode                   INIT  ""
   DATA   cIdeTheme                               INIT  ""
   DATA   cIdeAnimated                            INIT  ""

   DATA   aProjects                               INIT  {}
   DATA   aFiles                                  INIT  {}
   DATA   aFind                                   INIT  {}
   DATA   aReplace                                INIT  {}
   DATA   aRecentProjects                         INIT  {}
   DATA   aRecentFiles                            INIT  {}
   DATA   aFolders                                INIT  {}
   DATA   aViews                                  INIT  {}
   DATA   aTaggedProjects                         INIT  {}
   DATA   aTools                                  INIT  {}
   DATA   aUserToolbars                           INIT  {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD load( cHbideIni )
   METHOD save( cHbideIni )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeINI:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:load( cHbideIni )
   LOCAL aElem, s, nPart, cKey, cVal, a_

   ::oIde:cProjIni := hbide_getIniPath( cHbideIni )

   IF hb_FileExists( ::oIde:cProjIni )
      aElem := hbide_readSource( ::oIde:cProjIni )

      FOR EACH s IN aElem

         s := alltrim( s )
         IF !empty( s )
            SWITCH Upper( s )

            CASE "[GENERAL]"
               nPart := INI_GENERAL
               EXIT
            CASE "[HBIDE]"
               nPart := INI_HBIDE
               EXIT
            CASE "[PROJECTS]"
               nPart := INI_PROJECTS
               EXIT
            CASE "[FILES]"
               nPart := INI_FILES
               EXIT
            CASE "[FIND]"
               nPart := INI_FIND
               EXIT
            CASE "[REPLACE]"
               nPart := INI_REPLACE
               EXIT
            CASE "[RECENTFILES]"
               nPart := INI_RECENTFILES
               EXIT
            CASE "[RECENTPROJECTS]"
               nPart := INI_RECENTPROJECTS
               EXIT
            CASE "[FOLDERS]"
               nPart := INI_FOLDERS
               EXIT
            CASE "[VIEWS]"
               nPart := INI_VIEWS
               EXIT
            CASE "[TAGGEDPROJECTS]"
               nPart := INI_TAGGEDPROJECTS
               EXIT
            CASE "[TOOLS]"
               nPart := INI_TOOLS
               EXIT
            CASE "[USERTOOLBARS]"
               nPart := INI_USERTOOLBARS
               EXIT
            OTHERWISE
               DO CASE
               CASE Left( s, 1 ) $ '#['
                  * Nothing todo!

               CASE nPart == INI_GENERAL
                  * Qt Setttings, do nothing.

               CASE nPart == INI_HBIDE
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )

                     SWITCH cKey

                     CASE "MainWindowGeometry"          ; ::cMainWindowGeometry          := cVal ; EXIT
                     CASE "GotoDialogGeometry"          ; ::cGotoDialogGeometry          := cVal ; EXIT
                     CASE "FindDialogGeometry"          ; ::cFindDialogGeometry          := cVal ; EXIT
                     CASE "ToolsDialogGeometry"         ; ::cToolsDialogGeometry         := cVal ; EXIT
                     //
                     CASE "CurrentLineHighlightMode"    ; ::cCurrentLineHighlightMode    := cVal ; EXIT
                     CASE "LineNumbersDisplayMode"      ; ::cLineNumbersDisplayMode      := cVal ; EXIT
                     CASE "HorzRulerDisplayMode"        ; ::cHorzRulerDisplayMode        := cVal ; EXIT
                     //
                     CASE "RecentTabIndex"              ; ::cRecentTabIndex              := cVal ; EXIT
                     //
                     CASE "IdeTheme"                    ; ::cIdeTheme                    := cVal ; EXIT
                     CASE "IdeAnimated"                 ; ::cIdeAnimated                 := cVal ; EXIT
                     //          /* Subject to be identified under this object only */
                     CASE "PathMk2"                     ; ::oIde:cWrkPathMk2             := cVal ; EXIT
                     CASE "PathEnv"                     ; ::oIde:cWrkPathEnv             := cVal ; EXIT
                     CASE "CurrentProject"              ; ::oIde:cWrkProject             := cVal ; EXIT
                     CASE "CurrentTheme"                ; ::oIde:cWrkTheme               := cVal ; EXIT
                     CASE "CurrentCodec"                ; ::oIde:cWrkCodec               := cVal ; EXIT
                     CASE "CurrentEnvironment"          ; ::oIde:cWrkEnvironment         := cVal ; EXIT
                     CASE "CurrentFind"                 ; ::oIde:cWrkFind                := cVal ; EXIT
                     CASE "CurrentFolderFind"           ; ::oIde:cWrkFolderFind          := cVal ; EXIT
                     CASE "CurrentReplace"              ; ::oIde:cWrkReplace             := cVal ; EXIT
                     CASE "CurrentView"                 ; ::oIde:cWrkView                := cVal ; EXIT
                     CASE "CurrentHarbour"              ; ::oIde:cWrkHarbour             := cVal ; EXIT
                     CASE "CurrentShortcuts"            ; ::oIde:cPathShortcuts          := cVal ; EXIT
                     CASE "TextFileExtensions"          ; ::oIde:cTextExtensions         := cVal ; EXIT

                     ENDSWITCH
                  ENDIF

               CASE nPart == INI_PROJECTS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aProjects, cVal )
                  ENDIF

               CASE nPart == INI_FILES
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseSourceComponents( cVal )
                     IF !Empty( a_[ 1 ] )
                        aadd( ::aFiles, a_ )
                     ENDIF
                  ENDIF

               CASE nPart == INI_FIND
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aFind, cVal )
                  ENDIF

               CASE nPart == INI_REPLACE
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aReplace, cVal )
                  ENDIF

               CASE nPart == INI_RECENTPROJECTS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     IF Len( ::aRecentProjects ) < 25
                        cVal := hbide_pathNormalized( cVal, .f. )
                        IF aScan( ::aRecentProjects, {|e| hbide_pathNormalized( e, .f. ) == cVal } ) == 0
                           AAdd( ::aRecentProjects, cVal )
                        ENDIF
                     ENDIF
                  ENDIF

               CASE nPart == INI_RECENTFILES
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     IF Len( ::aRecentFiles ) < 25
                        cVal := hbide_pathNormalized( cVal, .f. )
                        IF aScan( ::aRecentFiles, {|e| hbide_pathNormalized( e, .f. ) == cVal } ) == 0
                           AAdd( ::aRecentFiles, cVal )
                        ENDIF
                     ENDIF
                  ENDIF

               CASE nPart == INI_FOLDERS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aFolders, cVal )
                  ENDIF

               CASE nPart == INI_VIEWS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aViews, cVal )
                  ENDIF

               CASE nPart == INI_TAGGEDPROJECTS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd(::aTaggedProjects, cVal )
                  ENDIF

               CASE nPart == INI_TOOLS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseToolComponents( cVal )
                     aadd( ::aTools, a_ )
                  ENDIF

               CASE nPart == INI_USERTOOLBARS
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseUserToolbarComponents( cVal )
                     aadd( ::aUserToolbars, a_ )
                  ENDIF

               ENDCASE
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:save( cHbideIni )
   LOCAL j, nTab, pTab, n, txt_, oEdit, nTabs, nn, a_

   DEFAULT cHbideIni TO ::oIde:cProjIni

   HB_TRACE( HB_TR_ALWAYS, "IdeINI:saveINI( cHbideIni )" )

   IF ::oIde:nRunMode != HBIDE_RUN_MODE_INI
      RETURN Nil
   ENDIF

   txt_:= {}

   aadd( txt_, "[HBIDE]" )
   aadd( txt_, " " )
   //
   aadd( txt_, "MainWindowGeometry"        + "=" +   hbide_posAndSize( ::oDlg:oWidget )                    )
   aadd( txt_, "GotoDialogGeometry"        + "=" +   ::cGotoDialogGeometry                                 )
   aadd( txt_, "FindDialogGeometry"        + "=" +   ::cFindDialogGeometry                                 )
   aadd( txt_, "ToolsDialogGeometry"       + "=" +   ::cToolsDialogGeometry                                )
   //
   aadd( txt_, "CurrentLineHighlightMode"  + "=" +   iif( ::lCurrentLineHighlightEnabled, "YES", "NO" )    )
   aadd( txt_, "LineNumbersDisplayMode"    + "=" +   iif( ::lLineNumbersVisible, "YES", "NO" )             )
   aadd( txt_, "HorzRulerDisplayMode"      + "=" +   iif( ::lHorzRulerVisible, "YES", "NO" )               )
   //
   aadd( txt_, "RecentTabIndex"            + "=" +   hb_ntos( ::qTabWidget:currentIndex() )                )
   //
   aadd( txt_, "IdeTheme"                  + "=" +   ::cIdeTheme                                           )
   aadd( txt_, "IdeAnimated"               + "=" +   ::cIdeAnimated                                        )

   aadd( txt_, "PathMk2"                   + "=" +   ::oIde:cWrkPathMk2                                    )
   aadd( txt_, "PathEnv"                   + "=" +   ::oIde:cWrkPathEnv                                    )
   aadd( txt_, "CurrentProject"            + "=" +   ::oIde:cWrkProject                                    )
   aadd( txt_, "CurrentTheme"              + "=" +   ::oIde:cWrkTheme                                      )
   aadd( txt_, "CurrentCodec"              + "=" +   ::oIde:cWrkCodec                                      )
   aadd( txt_, "CurrentEnvironment"        + "=" +   ::oIde:cWrkEnvironment                                )
   aadd( txt_, "CurrentFind"               + "=" +   ::oIde:cWrkFind                                       )
   aadd( txt_, "CurrentFolderFind"         + "=" +   ::oIde:cWrkFolderFind                                 )
   aadd( txt_, "CurrentReplace"            + "=" +   ::oIde:cWrkReplace                                    )
   aadd( txt_, "CurrentView"               + "=" +   ::oIde:cWrkView                                       )
   aadd( txt_, "CurrentHarbour"            + "=" +   ::oIde:cWrkHarbour                                    )
   aadd( txt_, "CurrentShortcuts"          + "=" +   ::oIde:cPathShortcuts                                 )
   aadd( txt_, "TextFileExtensions"        + "=" +   ::oIde:cTextExtensions                                )
   //
   aadd( txt_, " " )

   aadd( txt_, "[PROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::oIde:aProjects )
      aadd( txt_, "project_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::oIde:aProjects[ n, 2 ], .f. ) )
   NEXT
   aadd( txt_, " " )

   /*-------------------   FILES   -------------------*/
   aadd( txt_, "[FILES]" )
   aadd( txt_, " " )
   nn := 0
   FOR j := 2 TO len( ::aViews )
      ::oIde:lClosing := .t.
      ::oIde:oDK:setView( ::aViews[ j ]:oWidget:objectName() )

      nTabs := ::qTabWidget:count()
      FOR n := 1 TO nTabs
         pTab  := ::qTabWidget:widget( n - 1 )
         nTab  := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ 1 ]:oWidget:pPtr, pTab ) } )
         oEdit := ::aTabs[ nTab, TAB_OEDITOR ]

         IF !Empty( oEdit:sourceFile ) .AND. !( ".ppo" == lower( oEdit:cExt ) )
            IF oEdit:lLoaded
               aadd( txt_, "file_" + hb_ntos( ++nn ) + "=" + hbide_getEditInfoAsString( oEdit ) )

            ELSE
               aadd( txt_, "file_" + hb_ntos( ++nn ) + "=" + hbide_pathNormalized( oEdit:sourceFile, .f. ) + "," + ;
                           hb_ntos( oEdit:nPos  ) +  ","  + ;
                           hb_ntos( oEdit:nHPos ) +  ","  + ;
                           hb_ntos( oEdit:nVPos ) +  ","  + ;
                           oEdit:cTheme           +  ","  + ;
                           oEdit:cView            +  ","  + ;
                           hbide_nArray2string( oEdit:oEdit:aBookMarks ) +  ","  )
            ENDIF
         ENDIF
      NEXT
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FIND]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aFind )
      aadd( txt_, "find_" + hb_ntos( n ) + "=" + ::aFind[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[REPLACE]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aReplace )
      aadd( txt_, "replace_" + hb_ntos( n ) + "=" + ::aReplace[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTFILES]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aRecentFiles )
      aadd( txt_, "recentfile_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aRecentFiles[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aRecentProjects )
      aadd( txt_, "recentproject_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aRecentProjects[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FOLDERS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aFolders )
      aadd( txt_, "folder_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aFolders[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[VIEWS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aViews )
      aadd( txt_, "view_" + hb_ntos( n ) + "=" + ::aViews[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TAGGEDPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aTaggedProjects )
      aadd( txt_, "taggedproject_" + hb_ntos( n ) + "=" + ::aTaggedProjects[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TOOLS]" )
   aadd( txt_, " " )
   FOR EACH a_ IN ::aTools
      aadd( txt_, "tool_" + hb_ntos( a_:__enumIndex() ) + "=" + hbide_array2string( a_, "," ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[USERTOOLBARS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aUserToolbars )
      aadd( txt_, "usertoolbars_" + hb_ntos( n ) + "=" + hbide_array2string( ::aUserToolbars[ n ], "," ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[General]" )
   aadd( txt_, " " )

   //hbide_createTarget( oIde:cProjIni, txt_ )
   //hbide_createTarget( "e:\temp\test.ini", txt_ )

   RETURN cHbideIni //hbide_saveSettings( ::oIde )

/*----------------------------------------------------------------------*/
