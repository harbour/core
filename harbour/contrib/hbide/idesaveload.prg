/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveINI( oIde )
   LOCAL nTab, pTab, n, txt_, qHScr, qVScr, oEdit, qCursor
   LOCAL nTabs := oIde:qTabWidget:count()

   txt_:= {}
   //    Properties
   aadd( txt_, "[HBIDE]" )
   aadd( txt_, " " )
   aadd( txt_, "MainWindowGeometry     = " + hbide_posAndSize( oIde:oDlg:oWidget )        )
   aadd( txt_, "ProjectTreeVisible     = " + IIF( oIde:lProjTreeVisible, "YES", "NO" )    )
   aadd( txt_, "ProjectTreeGeometry    = " + hbide_posAndSize( oIde:oProjTree:oWidget )   )
   aadd( txt_, "FunctionListVisible    = " + IIF( oIde:lDockRVisible, "YES", "NO" )       )
   aadd( txt_, "FunctionListGeometry   = " + hbide_posAndSize( oIde:oFuncList:oWidget )   )
   aadd( txt_, "RecentTabIndex         = " + hb_ntos( oIde:qTabWidget:currentIndex() )    )
   aadd( txt_, "CurrentProject         = " + oIde:cWrkProject                             )
   aadd( txt_, "GotoDialogGeometry     = " + oIde:aIni[ INI_HBIDE, GotoDialogGeometry   ] )
   aadd( txt_, "PropsDialogGeometry    = " + oIde:aIni[ INI_HBIDE, PropsDialogGeometry  ] )
   aadd( txt_, "FindDialogGeometry     = " + oIde:aIni[ INI_HBIDE, FindDialogGeometry   ] )
   aadd( txt_, "ThemesDialogGeometry   = " + oIde:aIni[ INI_HBIDE, ThemesDialogGeometry ] )
   aadd( txt_, "CurrentTheme           = " + oIde:cWrkTheme                               )
   aadd( txt_, "CurrentCodec           = " + oIde:cWrkCodec                               )
   aadd( txt_, " " )

   aadd( txt_, "[PROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aProjects )
      aadd( txt_, oIde:aProjects[ n, 2 ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FILES]" )
   aadd( txt_, " " )
   FOR n := 1 TO nTabs
      pTab      := oIde:qTabWidget:widget( n-1 )
      nTab      := ascan( oIde:aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ 1 ]:oWidget:pPtr, pTab ) } )
      oEdit     := oIde:aTabs[ nTab, TAB_OEDITOR ]

      IF !Empty( oEdit:sourceFile ) .and. !( ".ppo" == lower( oEdit:cExt ) )

         qHScr     := QScrollBar():configure( oEdit:qEdit:horizontalScrollBar() )
         qVScr     := QScrollBar():configure( oEdit:qEdit:verticalScrollBar() )
         qCursor   := QTextCursor():configure( oEdit:qEdit:textCursor() )

         aadd( txt_, oEdit:sourceFile + "," + ;
                     hb_ntos( iif( oEdit:lLoaded, qCursor:position(), oEdit:nPos  ) ) +  ","  + ;
                     hb_ntos( iif( oEdit:lLoaded, qHScr:value()     , oEdit:nHPos ) ) +  ","  + ;
                     hb_ntos( iif( oEdit:lLoaded, qVScr:value()     , oEdit:nVPos ) ) +  ","  + ;
                     oEdit:cTheme                                                     + ","   )
      ENDIF
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FIND]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_FIND ] )
      aadd( txt_, oIde:aIni[ INI_FIND, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[REPLACE]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_REPLACE ] )
      aadd( txt_, oIde:aIni[ INI_REPLACE, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTFILES]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_RECENTFILES ] )
      aadd( txt_, oIde:aIni[ INI_RECENTFILES, n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( oIde:aIni[ INI_RECENTPROJECTS ] )
      aadd( txt_, oIde:aIni[ INI_RECENTPROJECTS , n ] )
   NEXT
   aadd( txt_, " " )

   hbide_saveSettings( oIde )

   RETURN hbide_createTarget( oIde:cProjIni, txt_ )

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadINI( oIde, cHbideIni )
   LOCAL aElem, s, n, nPart, cKey, cVal, a_
   LOCAL aIdeEle := { "mainwindowgeometry" , "projecttreevisible"  , "projecttreegeometry", ;
                      "functionlistvisible", "functionlistgeometry", "recenttabindex"     , ;
                      "currentproject"     , "gotodialoggeometry"  , "propsdialoggeometry", ;
                      "finddialoggeometry" , "themesdialoggeometry", "currenttheme",        ;
                      "currentcodec" }

   /* Initiate the place holders */
   oIde:aIni := Array( INI_SECTIONS_COUNT )
   oIde:aIni[ 1 ] := afill( array( INI_HBIDE_VRBLS ), "" )
   //
   FOR n := 2 TO INI_SECTIONS_COUNT
      oIde:aIni[ n ] := Array( 0 )
   NEXT

   IF empty( cHbideIni )
      IF file( "hbide.ini" )
         cHbideIni := "hbide.ini"
      ELSE
         cHbideIni := hb_dirBase() + "hbide.ini"
      ENDIF
   ENDIF
   oIde:cProjIni := cHbideIni

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
               CASE Left( s, 1 ) == '['
                  * Nothing todo!

               CASE nPart == INI_HBIDE
                  IF ( n := at( "=", s ) ) > 0
                     cKey := alltrim( substr( s, 1, n-1 ) )
                     cVal := alltrim( substr( s, n+1 ) )
                     cKey := lower( cKey )
                     IF ( n := ascan( aIdeEle, cKey ) ) > 0
                        oIde:aIni[ nPart, n ] := cVal  /* Further process */
                     ENDIF
                  ENDIF

               CASE nPart == INI_PROJECTS
                  aadd( oIde:aIni[ nPart ], s )

               CASE nPart == INI_FILES
                  a_:= hb_atokens( s, "," )
                  asize( a_, 5 )
                  DEFAULT a_[ 1 ] TO ""
                  DEFAULT a_[ 2 ] TO ""
                  DEFAULT a_[ 3 ] TO ""
                  DEFAULT a_[ 4 ] TO ""
                  DEFAULT a_[ 5 ] TO ""
                  //
                  a_[ 2 ] := val( a_[ 2 ] )
                  a_[ 3 ] := val( a_[ 3 ] )
                  a_[ 4 ] := val( a_[ 4 ] )
                  a_[ 5 ] := a_[ 5 ]

                * Ignores invalid filenames...
                  IF !Empty( a_[ 1 ] )
                     aadd( oIde:aIni[ nPart ], a_ )
                  ENDIF

               CASE nPart == INI_FIND
                  aadd( oIde:aIni[ nPart ], s )

               CASE nPart == INI_REPLACE
                  aadd( oIde:aIni[ nPart ], s )

               CASE nPart == INI_RECENTPROJECTS        .OR. ;
                    nPart == INI_RECENTFILES

                  IF Len( oIde:aIni[ nPart ] ) < 25
                     s := hbide_pathNormalized( s, .f. )
                     IF aScan( oIde:aIni[ nPart ], {|e| hbide_pathNormalized( e, .f. ) == s } ) == 0
                        AAdd( oIde:aIni[ nPart ], s )
                     ENDIF
                  ENDIF

               ENDCASE
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_saveSettings( oIde )

   hbqt_QMainWindow_saveSettings( hb_dirBase() + "idesettings.ini", "hbIDE", oIde:oDlg:oWidget:pPtr )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_restSettings( oIde )

   hbqt_QMainWindow_restSettings( hb_dirBase() + "idesettings.ini", "hbIDE", oIde:oDlg:oWidget:pPtr )

   RETURN nil

/*----------------------------------------------------------------------*/
