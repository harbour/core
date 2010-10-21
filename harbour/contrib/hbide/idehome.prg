/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                               04Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "xbp.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

#define browserStat_anchorClicked                 101
#define browserWelcome_contextMenuRequested       102
#define browserFaq_contextMenuRequested           103
#define tabWidget_currentChanged                  104

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_htmlImgAnchor( cHref, cImg, nWidth, nHeight )
   DEFAULT nWidth  TO 12
   DEFAULT nHeight TO 12
   RETURN '<a href="' + cHref + '"' + '>' + ' <img src="' + cImg + '"' + ;
          ' width="' + hb_ntos( nWidth ) + '" height="' + hb_ntos( nHeight ) + '"' + '</img>' + '</a>'

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_htmlAnchor( cHref, cText, cTooltip )

   RETURN '<a href="' + cHref + '"' + iif( empty( cTooltip ), '', ' title="' + cTooltip + '"' ) +'>' + cText + '</A>'

/*----------------------------------------------------------------------*/

CLASS IdeHome INHERIT IdeObject

   DATA   oWelcomeTab
   DATA   qWelcomeBrowser
   DATA   oFaqTab
   DATA   qFaqBrowser

   DATA   cClickedProject
   DATA   cClickedSource

   DATA   qCurBrowser
   DATA   qPrnDlg

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD refresh()
   METHOD execEvent( nMode, p )
   METHOD buildWelcomeTab()
   METHOD activateTab( mp1, mp2, oTab )
   METHOD addProjectsInfo( aHtm )
   METHOD buildProjectDetails( cProjectTitle )
   METHOD buildSourcesInfo( cProjectTitle, aSrcInfo )
   METHOD formatSourceInfo( aHtm, aSrc )
   METHOD buildFaqTab()
   METHOD setStyleSheetTextBrowser( qBrw )
   METHOD print()
   METHOD paintRequested( pPrinter )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeHome:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:create( oIde )
   LOCAL oStatFrame

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   oStatFrame := ::aViews[ 1 ]

   oStatFrame:oTabWidget:oWidget:setDocumentMode( .t. )
   oStatFrame:oTabWidget:oWidget:setStyleSheet( "QTabWidget::tab-bar {left: 5px;}" )
   oStatFrame:qLayout:setContentsMargins( 0,0,0,0 )

   ::buildWelcomeTab()
   ::buildFaqTab()

   oStatFrame:oTabWidget:oWidget:setCurrentIndex( 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:destroy()

   IF !empty( ::qPrnDlg )
      ::qPrnDlg:disconnect( "paintRequested(QPrinter)" )
      ::qPrnDlg := NIL
   ENDIF

   ::qWelcomeBrowser:disconnect( "anchorClicked(QUrl)"                )
   ::qWelcomeBrowser:disconnect( "customContextMenuRequested(QPoint)" )
   ::qFaqBrowser:disconnect( "customContextMenuRequested(QPoint)" )

   ::qWelcomeBrowser := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:execEvent( nMode, p )
   LOCAL cAct, qUrl, cText, cExt

   DO CASE
   CASE nMode == tabWidget_currentChanged
      IF p == 0
         ::qCurBrowser := ::qWelcomeBrowser
      ELSEIF p == 1
         ::qCurBrowser := ::qFaqBrowser
      ENDIF

   CASE nMode == browserStat_anchorClicked
      qUrl  := QUrlFromPointer( p )
      cText := lower( qUrl:toString() )

      IF "prj-" $ cText
         ::cClickedProject := substr( cText, 5 )
         ::buildProjectDetails( ::cClickedProject )

      ELSEIF "fle-" $ cText
         ::cClickedSource := substr( cText, 5 )

         /* Send it for Editing */
         hb_fNameSplit( ::cClickedSource, , , @cExt )
         IF lower( cExt ) == ".hbp"
            ::buildProjectDetails( ::cClickedSource )
         ELSE
            ::oSM:editSource( hbide_stripFilter( ::cClickedSource ) )
         ENDIF
         /* Stay on the Same Page */
         ::buildProjectDetails( ::cClickedProject )
      ENDIF

   CASE nMode == browserWelcome_contextMenuRequested  .OR. nMode == browserFaq_contextMenuRequested

      IF !empty( cAct := hbide_popupBrwContextMenu( ::qCurBrowser, p ) )
         IF cAct $ "Back,Forward,Home"
            ::refresh()
         ELSEIF cAct == "Print"
            ::print()
         ENDIF
      ENDIF

   ENDCASE
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:activateTab( mp1, mp2, oTab )

   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   IF oTab == ::oWelcomeTab
      ::qCurBrowser := ::qWelcomeBrowser
   ELSEIF oTab == ::oFaqTab
      ::qCurBrowser := ::qFaqBrowser
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:print()

   ::qPrnDlg := NIL
   IF empty( ::qPrnDlg )
      ::qPrnDlg := QPrintPreviewDialog()
      ::qPrnDlg:setWindowTitle( "Welcome::Projects" )
      ::qPrnDlg:setWindowIcon( hbide_image( "hbide" ) )
      ::qPrnDlg:connect( "paintRequested(QPrinter)", {|p| ::paintRequested( p ) } )
   ENDIF

   ::qPrnDlg:exec()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeHome:paintRequested( pPrinter )
   LOCAL qPrinter := QPrinterFromPointer( pPrinter )

   ::qCurBrowser:print( qPrinter )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:setStyleSheetTextBrowser( qBrw )

   qBrw:setStyleSheet( 'QTextBrowser { background-image: url(:/resources/hbidesplashwatermark.png); ' + ;
          'background-attachment: scroll; background-repeat:no-repeat; background-position:center; ' + ;
          'background-color: rgb(255,255,255); }' )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:buildWelcomeTab()
   LOCAL oTab, qBrw, qSList

   oTab := XbpTabPage():new( ::aViews[ 1 ], , { 5,5 }, { 700,400 }, , .t. )
   oTab:caption     := "Welcome"
   oTab:minimized   := .F.
   oTab:create()
   oTab:tabActivate := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }

   oTab:hbLayout    := HBPLAYOUT_TYPE_VERTBOX
   oTab:qLayout:setContentsMargins( 0,0,0,0 )

   qBrw := QTextBrowser()
   oTab:qLayout:addWidget( qBrw )
   qBrw:setContextMenuPolicy( Qt_CustomContextMenu )
   ::setStyleSheetTextBrowser( qBrw )

   ::oWelcomeTab     := oTab
   ::qWelcomeBrowser := qBrw
   ::qCurBrowser     := qBrw

   qBrw:connect( "anchorClicked(QUrl)"               , {|p| ::execEvent( browserStat_anchorClicked          , p ) } )
   qBrw:connect( "customContextMenuRequested(QPoint)", {|p| ::execEvent( browserWelcome_contextMenuRequested, p ) } )

   qSList := QStringList()
   qSList:append( "docs" )
   qBrw:setSearchPaths( qSList )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:refresh()
   LOCAL aHtm := {}

   aadd( aHtm, '<html>'                                    )
   aadd( aHtm, ' <body align=center valign=center>'        )
   aadd( aHtm, '  <table cols="7" width="95%">'            )
   aadd( aHtm, '   <tr>'                                   )
   aadd( aHtm, '    <td align="left" valign="center" colspan="5">' )
   aadd( aHtm, '     <img src=":/resources/project.png"' + '</img>' + '&nbsp;' + '&nbsp;' + '&nbsp;'      )
   aadd( aHtm, '     <font face="Times New Roman" color="#FF4719" size="7"><b><U>Projects</u></b></font>' )
   aadd( aHtm, '     &nbsp;' + '&nbsp;' + '&nbsp;'         )
   aadd( aHtm, '     <font face="Times New Roman" size="5">' + '( ' + hbide_pathNormalized( ::oIde:cProjIni, .f. )  + ' )' + '</font>'  )
   aadd( aHtm, '    </td>'                                 )
   aadd( aHtm, '   </tr>'                                  )
   aadd( aHtm, '   <tr bgcolor="#F0F0F5">'                 )
   aadd( aHtm, '    <th width="150"  >Title         </th>' )
   aadd( aHtm, '    <th width="50"   >Type          </th>' )
   aadd( aHtm, '    <th width="75"   >Sources       </th>' )
   aadd( aHtm, '    <th width="300"  >Location      </th>' )
   aadd( aHtm, '    <th width="120"  >Last Modified </th>' )
   AADD( aHtm, '   </tr>'                                  )
   //
   ::addProjectsInfo( @aHtm )
   //
   aadd( aHtm, '  </table>'                                )
   aadd( aHtm, ' </body>'                                  )
   aadd( aHtm, '</html>'                                   )
   //
   ::qWelcomeBrowser:clear()
   //
   ::qWelcomeBrowser:setHTML( hbide_arrayToMemo( aHtm ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:addProjectsInfo( aHtm )
   LOCAL a_, prp_, src_, dir_, cIcon

   IF empty( ::aProjects )
      aadd( aHtm, '   <tr>'   )
      aadd( aHtm, '    <td>'  )
      aadd( aHtm, '   ' + 'None' )
      aadd( aHtm, '    </td>' )
      aadd( aHtm, '   <tr>'  )
   ELSE
      FOR EACH a_ IN ::aProjects
         prp_  := a_[ 3, PRJ_PRP_PROPERTIES, 1 ]
         src_  := a_[ 3, PRJ_PRP_SOURCES   , 1 ]
         dir_  := directory( a_[ 1 ] )
         cIcon := hbide_imageForProjectType( prp_[ PRJ_PRP_TYPE ] )

         aadd( aHtm, '   <tr height="1">'    )
         aadd( aHtm, '    <td><b>' )
         aadd( aHtm, '   ' + hbide_htmlImgAnchor( 'prj-' + prp_[ PRJ_PRP_TITLE ], ':/resources/' + cIcon + '.png' ) + '&nbsp;' )
         aadd( aHtm, '   ' + hbide_htmlAnchor( 'prj-' + prp_[ PRJ_PRP_TITLE ], prp_[ PRJ_PRP_TITLE ], a_[ 1 ] ) )
         aadd( aHtm, '    </b></td>' )
         aadd( aHtm, '    <td align=center>' )
         aadd( aHtm, '    ' + left( prp_[ PRJ_PRP_TYPE ], 3 ) )
         aadd( aHtm, '    </td>' )
         aadd( aHtm, '    <td align=center>' )
         aadd( aHtm, '    ' + hb_ntos( len( src_ ) ) )
         aadd( aHtm, '    </td>' )
         aadd( aHtm, '    <td align=left>'   )
         aadd( aHtm, '    ' + hbide_pathNormalized( a_[ 1 ], .f. ) )
         aadd( aHtm, '    </td>' )
         aadd( aHtm, '    <td align=center>' )
         IF ! empty( dir_ )
            aadd( aHtm, '    ' + dtoc( dir_[ 1,3 ] ) + " " + dir_[ 1,4 ] )
         ELSE
            aadd( aHtm, '    ' )
         ENDIF
         aadd( aHtm, '    </td>' )
         aadd( aHtm, '   </tr>'  )
      NEXT
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:buildProjectDetails( cProjectTitle )
   LOCAL aSrc, cSrc, dir_, cRoot, cExt, cIcon, cName, cPath
   LOCAL aSrcInfo := {}, a_:= {}

   aSrc  := ::oPM:getSourcesByProjectTitle( cProjectTitle )
   cRoot := ::oPM:getProjectPathFromTitle( cProjectTitle )

   FOR EACH cSrc IN aSrc
      aadd( a_, hbide_syncProjPath( cRoot, cSrc ) )
   NEXT
   a_ := hbide_groupSources( "org", a_ )
   FOR EACH cSrc IN a_
      hb_fNameSplit( cSrc, @cPath, @cName, @cExt )
      cExt  := lower( cExt )
      cIcon := hbide_imageForFileType( cExt )
      IF !empty( dir_:= directory( cSrc ) )
         //                                                                         bytes        date         time
         aadd( aSrcInfo, { cSrc, hbide_pathNormalized( cPath,.f. ), cName, cExt, cIcon, dir_[ 1,2 ], dir_[ 1,3 ], dir_[ 1,4 ] } )
      ENDIF
   NEXT

   IF !empty( aSrcInfo )
      ::buildSourcesInfo( cProjectTitle, aSrcInfo )
   ELSE
      ::refresh()
   ENDIF

   RETURN aSrcInfo

/*----------------------------------------------------------------------*/

METHOD IdeHome:buildSourcesInfo( cProjectTitle, aSrcInfo )
   LOCAL aHtm := {}, aSrc
   LOCAL cIcon := hbide_imageForProjectType( ::oPM:getProjectTypeFromTitle( cProjectTitle ) )

   aadd( aHtm, '<html>'                                  )
   aadd( aHtm, ' <body align=center valign=center>'      )
   aadd( aHtm, '  <table cols="7" width="95%">'          )
   aadd( aHtm, '   <tr><td align=left>'                  )
   aadd( aHtm, '    <img src="' + ':/resources/' + cIcon + '.png' + '"' + '</img>' + '&nbsp;' + '&nbsp;' + '&nbsp;' )
   aadd( aHtm, '    <font color="#FF4719" size=5><u>' + cProjectTitle + '</u></font>' )
   aadd( aHtm, '   </td>'                                )
   aadd( aHtm, '   <tr bgcolor="#F0F0F5">'               )
   aadd( aHtm, '    <th width="150" >Source</th>'        )
   aadd( aHtm, '    <th width="50"  >Type</th>'          )
   aadd( aHtm, '    <th width="60"  >Size</th>'          )
   aadd( aHtm, '    <th width="250" >Location</th>'      )
   aadd( aHtm, '    <th width="120" >Last Modified</th>' )
   AADD( aHtm, '   </tr>'                                )
   //
   FOR EACH aSrc IN aSrcInfo
      ::formatSourceInfo( @aHtm, aSrc )
   NEXT
   //
   aadd( aHtm, '  </table>'                              )
   aadd( aHtm, ' </body>'                                )
   aadd( aHtm, '</html>'                                 )
   //
   ::qWelcomeBrowser:clear()
   //
   ::qWelcomeBrowser:setHTML( hbide_arrayToMemo( aHtm ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:formatSourceInfo( aHtm, aSrc )

   aadd( aHtm, '   <tr>'     )
   aadd( aHtm, '    <td><b>' )
   aadd( aHtm, '   ' + hbide_htmlImgAnchor( 'fle-' + aSrc[ 1 ], ':/resources/' + aSrc[ 5 ] + '.png' ) + '&nbsp;' )
   aadd( aHtm, '   ' + hbide_htmlAnchor( 'fle-' + aSrc[ 1 ], aSrc[ 3 ], aSrc[ 1 ] ) )
   aadd( aHtm, '    </b></td>' )
   aadd( aHtm, '    <td>'    )
   aadd( aHtm, '    ' + aSrc[ 4 ]  )
   aadd( aHtm, '    </td>'   )
   aadd( aHtm, '    <td align=center>'    )
   aadd( aHtm, '    ' + hb_ntos( aSrc[ 6 ] ) )
   aadd( aHtm, '    </td>'   )
   aadd( aHtm, '    <td align=left>'  )
   aadd( aHtm, '    ' + aSrc[ 2 ] )
   aadd( aHtm, '    </td>' )
   aadd( aHtm, '    <td align=center>'    )
   aadd( aHtm, '    ' + dtoc( aSrc[ 7 ] ) + " " + aSrc[ 8 ] )
   aadd( aHtm, '    </td>'   )
   aadd( aHtm, '   </tr>'  )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHome:buildFaqTab()
   LOCAL oTab, qBrw, aFaq, aHtm, a_, b_, s

   oTab := XbpTabPage():new( ::aViews[ 1 ], , { 5,5 }, { 700,400 }, , .t. )
   oTab:caption   := "FAQ's"
   oTab:minimized := .F.
   oTab:create()
   oTab:tabActivate := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }

   oTab:hbLayout  := HBPLAYOUT_TYPE_VERTBOX
   oTab:qLayout:setContentsMargins( 0,0,0,0 )

   qBrw := QTextBrowser()
   oTab:qLayout:addWidget( qBrw )
   qBrw:setContextMenuPolicy( Qt_CustomContextMenu )
   ::setStyleSheetTextBrowser( qBrw )

   qBrw:connect( "customContextMenuRequested(QPoint)", {|p| ::execEvent( browserFaq_contextMenuRequested, p  ) } )

   ::oFaqTab     := oTab
   ::qFaqBrowser := qBrw

   aFaq := hbide_getFaqs() ; a_:= aFaq[ 1 ]; b_:= aFaq[ 2 ]
   aHtm := {}

   aadd( aHtm, '<html>'                                  )
   aadd( aHtm, ' <body align=center valign=center>'      )
   aadd( aHtm, '  <table cols="7" width="95%">'          )
   FOR EACH s IN a_
   aadd( aHtm, '   <tr><td></td></tr>'                   )
   aadd( aHtm, '   <tr><td>'                             )
   aadd( aHtm, '    <font color="red" size=4>' + s + '</font>' )
   aadd( aHtm, '   </td></tr>'                           )
   aadd( aHtm, '   <tr><td>'                             )
   aadd( aHtm, '    <font color="black" size=3>' + b_[ s:__enumIndex() ] + '</font>' )
   aadd( aHtm, '   </td></tr>'                           )
   NEXT
   aadd( aHtm, '  </table>'                              )
   aadd( aHtm, ' </body>'                                )
   aadd( aHtm, '</html>'                                 )
   //
   ::qFaqBrowser:clear()
   //
   ::qFaqBrowser:setHTML( hbide_arrayToMemo( aHtm ) )

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getFaqs()
   LOCAL a_:= {}, b_:= {}

   aadd( a_, 'Does hbIDE support editing of same source at more than one place simultaneously ?' )
   aadd( b_, 'Yes. hbIDE provides for splitting an editing window, horizontally and vertically, ' + ;
             'both ways. Right-click anywhere in the editor, point to "Split..." and then ' + ;
             'click on required option.' )

   aadd( a_, 'Does hbIDE support session management ?' )
   aadd( b_, 'Yes. And in a unique way. It provides to construct n number of "Panels" where you can spread ' + ;
             'sources. Each panel is given a name as well as corresponding button on the left-toolbar, a colored ' + ;
             'circle, to bring it forward. New panel can be created from selecting "New..." from drop-down ' + ;
             'list on top-toolbar and providing a name in the editing dialog. For this reason you will never ' + ;
             'need to exit hbIDE and reenter again. It is a big productivity boost.' )

   aadd( a_, 'Does hbIDE allows to compile a project for different compilers and/or environments ?' )
   aadd( b_, 'Yes. Infact this feature had been the one of the main reasons of hbIDEs existance. ' + ;
             'All you have to do is to enter different environment settings in _ONE_ hbide.env file which ' + ;
             'resides alongside hbide.ini. hbIDE provides the interface to manipulate this .env settings ' + ;
             'which can be invoked from right-toolbar icon "Compiler Environments". Follow the input fields. ' + ;
             'A template is provided in "hbide.env", inspect it and you are through. Once hbide.env ' + ;
             'is ready, you will simply need to switch over the other environment which can be invoked ' + ;
             'via right-click on a project node in "Projects" tree and point to "Select an environment" ' + ;
             'and click on listed options. Next step is just to build the project.' )

   aadd( a_, 'Can we keep any other content in hbide.ini ?' )
   aadd( b_, 'No. hbide.ini is always re-written at the time hbIDE exits. Also this action is executed ' + ;
             'several times depending upon the underlying needs. However, you can alter the contents in some ' + ;
             'sections, i.e., [Files], [Projects], etc., but only before running hbIDE. At exit, it will ' + ;
             're-write it again.' )

   aadd( a_, 'Does hbIDE provides "intellisense" ? If yes, in what aspects ?' )
   aadd( b_, 'Yes. Not so powerful yet, but "yes". hbIDE embeds two components as the basis of this intellisense, ' + ;
             hb_eol() + ;
             '   1. Harbour Documentation, and ' + hb_eol() + ;
             '   2. Project(s) Prototype Tagging. ' + hb_eol() + ;
             'Both these components can be pressed in servive simultaneously and as per need. Both are loaded ' + ;
             'only when requested. Prototype Tagging extends one step forward as to offer to load only those ' + ;
             'projects which a developer will be working on during that session. Both components can be aligned ' + ;
             'on the side corners to provide always-on view of the actual contents. The moment you press "(" ' + ;
             'hbIDE examines the contents of these components, if the function is available in Harbour Docs, ' + ;
             'its details are dumped into the viewer, otherwise if the function is contained in the tagged ' + ;
             'prototypes, it is made current in the list, and a html formatted prototype is displayed as tooltip ' + ;
             'beneth the current caret position. ' + hb_eol() + ;
             hb_eol() + ;
             'Code completion tool is almost done with, and probably will make up its presence in the hbIDE soon. ' )

   RETURN { a_, b_ }

/*----------------------------------------------------------------------*/
