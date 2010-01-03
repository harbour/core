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

FUNCTION saveINI( oIde )
   LOCAL nTab, pTab, n, txt_, qEdit, qHScr, qVScr, qSet, cTheme
   LOCAL nTabs := oIde:qTabWidget:count()
   //LOCAL qBArray

   txt_:= {}
   //    Properties
   aadd( txt_, "[HBIDE]" )
   aadd( txt_, "MainWindowGeometry     = " + PosAndSize( oIde:oDlg:oWidget )              )
   aadd( txt_, "ProjectTreeVisible     = " + IIF( oIde:lProjTreeVisible, "YES", "NO" )    )
   aadd( txt_, "ProjectTreeGeometry    = " + PosAndSize( oIde:oProjTree:oWidget )         )
   aadd( txt_, "FunctionListVisible    = " + IIF( oIde:lDockRVisible, "YES", "NO" )       )
   aadd( txt_, "FunctionListGeometry   = " + PosAndSize( oIde:oFuncList:oWidget )         )
   aadd( txt_, "RecentTabIndex         = " + hb_ntos( oIde:qTabWidget:currentIndex() )    )
   aadd( txt_, "CurrentProject         = " + ""                                           )
   aadd( txt_, "GotoDialogGeometry     = " + oIde:aIni[ INI_HBIDE, GotoDialogGeometry   ] )
   aadd( txt_, "PropsDialogGeometry    = " + oIde:aIni[ INI_HBIDE, PropsDialogGeometry  ] )
   aadd( txt_, "FindDialogGeometry     = " + oIde:aIni[ INI_HBIDE, FindDialogGeometry   ] )
   aadd( txt_, "ThemesDialogGeometry   = " + oIde:aIni[ INI_HBIDE, ThemesDialogGeometry ] )

   qSet := QSettings():new( "Harbour", "HbIde" )
   qSet:setValue( "state", oIde:oDlg:oWidget:saveState() )

   #if 0
   qBArray := QByteArray()
   qBArray:pPtr := oIde:oDlg:oWidget:saveState()
   HB_TRACE( HB_TR_ALWAYS, "QByteArray", 1 )
   HB_TRACE( HB_TR_ALWAYS, "QByteArray", qBArray:size(), qBArray:isNull() )
   HB_TRACE( HB_TR_ALWAYS, "QByteArray", 2, qBArray:constData() )

   aadd( txt_, "State                  = " + qBArray:data_1() )
   #endif

   aadd( txt_, " " )

   //    Projects
   aadd( txt_, "[PROJECTS]" )
   FOR n := 1 TO len( oIde:aProjects )
      aadd( txt_, oIde:aProjects[ n, 2 ] )
   NEXT
   aadd( txt_, " " )

   //    Files
   aadd( txt_, "[FILES]" )
   FOR n := 1 TO nTabs
      pTab      := oIde:qTabWidget:widget( n-1 )
      nTab      := ascan( oIde:aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ 1 ]:oWidget:pPtr, pTab ) } )
      qEdit     := oIde:aTabs[ nTab, TAB_QEDIT ]
      qHScr     := QScrollBar():configure( qEdit:horizontalScrollBar() )
      qVScr     := QScrollBar():configure( qEdit:verticalScrollBar() )
      oIde:qCursor := QTextCursor():configure( qEdit:textCursor() )

      cTheme := oIde:aTabs[ nTab, TAB_OEDITOR ]:cTheme

      aadd( txt_, oIde:aTabs[ nTab, TAB_SOURCEFILE ] +","+ ;
                  hb_ntos( oIde:qCursor:position() ) +","+ ;
                  hb_ntos( qHScr:value() ) + "," + ;
                  hb_ntos( qVScr:value() ) + "," + ;
                  cTheme + "," ;
           )
   NEXT
   aadd( txt_, " " )

   //    Find
   aadd( txt_, "[FIND]" )
   FOR n := 1 TO len( oIde:aIni[ INI_FIND ] )
      aadd( txt_, oIde:aIni[ INI_FIND, n ] )
   NEXT
   aadd( txt_, " " )

   //    Replace
   aadd( txt_, "[REPLACE]" )
   FOR n := 1 TO len( oIde:aIni[ INI_REPLACE ] )
      aadd( txt_, oIde:aIni[ INI_REPLACE, n ] )
   NEXT
   aadd( txt_, " " )

   RETURN CreateTarget( oIde:cProjIni, txt_ )

/*----------------------------------------------------------------------*/

FUNCTION loadINI( oIde, cHbideIni )
   LOCAL aElem, s, n, nPart, cKey, cVal, a_
   LOCAL aIdeEle := { "mainwindowgeometry" , "projecttreevisible"  , "projecttreegeometry", ;
                      "functionlistvisible", "functionlistgeometry", "recenttabindex"     , ;
                      "currentproject"     , "gotodialoggeometry"  , "propsdialoggeometry", ;
                      "finddialoggeometry" , "themesdialoggeometry" }

   DEFAULT cHbideIni TO "hbide.ini"

   cHbideIni := lower( cHbideIni )

   IF !file( cHbideIni )
      cHbideIni := hb_dirBase() + "hbide.ini"
   ENDIF

   IF !file( cHbideIni )
      cHbideIni := hb_dirBase() + "hbide.ini"
   ENDIF

   oIde:cProjIni := cHbideIni

   oIde:aIni := { afill( array( INI_HBIDE_VRBLS ), "" ), {}, {}, {}, {} }

   IF file( oIde:cProjIni )
      aElem := ReadSource( oIde:cProjIni )

      FOR EACH s IN aElem
         s := alltrim( s )
         IF !empty( s )
            DO CASE
            CASE s == "[HBIDE]"
               nPart := INI_HBIDE
            CASE s == "[PROJECTS]"
               nPart := INI_PROJECTS
            CASE s == "[FILES]"
               nPart := INI_FILES
            CASE s == "[FIND]"
               nPart := INI_FIND
            CASE s == "[REPLACE]"
               nPart := INI_REPLACE
            OTHERWISE
               DO CASE
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
                  oIde:loadProjectProperties( s, .f., .f., .f. )

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
                  a_[ 5 ] := a_[ 5 ]                  /* Just for reference */
                  aadd( oIde:aIni[ nPart ], a_ )

               CASE nPart == INI_FIND
                  aadd( oIde:aIni[ nPart ], s )
               CASE nPart == INI_REPLACE
                  aadd( oIde:aIni[ nPart ], s )
               ENDCASE
            ENDCASE
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/
