/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                               14Jul2012
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqtgui.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

#define __frameSrc_dragMoveEvent__                2001
#define __frameSrc_dragEnterEvent__               2002
#define __frameSrc_dropEvent__                    2003
#define __treeProps_itemSelectionChanged__        2004
#define __treeProps_itemCollapsed__               2005
#define __treeProps_itemExpanded__                2006
#define __treeSrc_itemExpanded__                  2007
#define __treeSrc_itemCollapsed__                 2008
#define __qTBtn_clicked__                         2009
#define __qSBtn_clicked__                         2010
#define __toolSrcDel_clicked__                    2011
#define __toolSrcMin_clicked__                    2012
#define __toolSrcMax_clicked__                    2013
#define __btnSwMin_clicked__                      2014
#define __btnSwMax_clicked__                      2015
#define __toolSrcGet_clicked__                    2016
#define __treeSrc_contextMenuRequested__          2017
#define __treeProps_contextMenuRequested__        2018
#define __treeSrc_doubleClicked__                 2019
#define __btnSw_clicked__                         2020
#define __treeProps_doubleClicked__               2021
#define __btnNew_clicked__                        2022
#define __btnSave_clicked__                       2023
#define __btnNext_clicked__                       2024
#define __btnBack_clicked__                       2025

/*----------------------------------------------------------------------*/

CREATE CLASS IdeProjectWizard INHERIT IdeObject

   DATA    lEdited                                INIT .f.
   DATA    aItmProps                              INIT {}
   DATA    aItmSrc                                INIT {}
   DATA    cProjPath

   METHOD  new( oIde )
   METHOD  create( oIde )
   METHOD  destroy()
   METHOD  show()
   METHOD  execEvent( nEvent, p, p1 )
   METHOD  loadDefaults()
   METHOD  saveProject()
   METHOD  clear()
   METHOD  loadSourcesSections()
   METHOD  loadSwichesSections()
   METHOD  deleteTreeItem( oChild )
   METHOD  addTreeItem( oParent )
   METHOD  addDropIndicator( oTree, oNode, nMsg, cCSS, nIndex )
   METHOD  addSourceFile( cFile )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:new( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:clear()

   ::oUI:editProjPath     : setText( "" )
   ::oUI:editProjName     : setText( "" )
   ::oUI:editOutName      : setText( "" )
   ::oUI:editOutPath      : setText( "" )
   ::oUI:editWorkPath     : setText( "" )
   ::oUI:editWorkPath     : setText( "" )
   ::oUI:editLaunchExe    : setText( "" )
   ::oUI:editLaunchParams : setText( "" )
   ::oUI:editStayIn       : setText( "" )

   ::oUI:plainSwitches    : setPlainText( "" )

   ::oUi:chkXhb           : setChecked( .F. )
   ::oUi:chkXBase         : setChecked( .F. )
   ::oUi:chkHbQt          : setChecked( .F. )
   ::oUi:chkXbp           : setChecked( .F. )
   ::oUi:chkFWH           : setChecked( .F. )
   ::oUi:chkHMG           : setChecked( .F. )
   ::oUi:chkOther         : setChecked( .F. )

   ::oUi:chkA             : setChecked( .T. )
   ::oUi:chkB             : setChecked( .F. )
   ::oUi:chkES            : setChecked( .T. )
   ::oUi:chkG             : setChecked( .F. )
   ::oUi:chkJ             : setChecked( .F. )
   ::oUi:chkL             : setChecked( .F. )
   ::oUi:chkM             : setChecked( .T. )
   ::oUi:chkN             : setChecked( .T. )
   ::oUi:chkV             : setChecked( .F. )
   ::oUi:chkW             : setChecked( .F. )
   ::oUi:chkZ             : setChecked( .F. )
   ::oUi:chkQ             : setChecked( .F. )
   ::oUi:chkBuild         : setChecked( .F. )
   ::oUi:chkCredits       : setChecked( .F. )

   ::oUi:chkInc           : setChecked( .T. )
   ::oUi:chkGui           : setChecked( .F. )
   ::oUi:chkInfo          : setChecked( .T. )
   ::oUi:chkShared        : setChecked( .F. )
   ::oUi:chkMt            : setChecked( .F. )
   ::oUi:chkTrace         : setChecked( .F. )
   ::oUi:chkFullstatic    : setChecked( .F. )

   ::oUi:chkGtgui         : setChecked( .F. )
   ::oUi:chkGtwin         : setChecked( .F. )
   ::oUi:chkGtwvt         : setChecked( .F. )
   ::oUi:chkGtwvg         : setChecked( .F. )
   ::oUi:chkGtxwc         : setChecked( .F. )
   ::oUi:chkGttrm         : setChecked( .F. )
   ::oUi:chkGtstd         : setChecked( .F. )
   ::oUi:chkGtsln         : setChecked( .F. )
   ::oUi:chkGtpca         : setChecked( .F. )
   ::oUi:chkGtos2         : setChecked( .F. )
   ::oUi:chkGtcrs         : setChecked( .F. )
   ::oUi:chkGtcgi         : setChecked( .F. )

   ::oUI:editES           : setText( "0" )
   ::oUI:editG            : setText( "" )
   ::oUI:editM            : setText( "" )
   ::oUI:editQ            : setText( "" )
   ::oUI:editW            : setText( "" )

   ::oUI:comboProjType    : setCurrentIndex( 0 )
   ::oUI:comboGT          : setCurrentIndex( 9 )

   ::oUI:treeProps        : clear()
   ::loadSwichesSections()

   ::oUI:treeSrc          : clear()
   ::loadSourcesSections()

   ::lEdited := .f.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:show()
   LOCAL oBrush

   IF empty( ::oUI )
      ::oUI := ui_projectWizard():new(  ::oIde:oDlg:oWidget )
      ::oUI:oWidget:connect( QEvent_Close, {|| ::oUI:oWidget:done( 0 ) } )

      ::oUI:btnSwAZ     : connect( "clicked()", {|| ::execEvent( __btnSw_clicked__, "az"    ) } )
      ::oUI:btnSwZA     : connect( "clicked()", {|| ::execEvent( __btnSw_clicked__, "za"    ) } )
      ::oUI:btnSwUpper  : connect( "clicked()", {|| ::execEvent( __btnSw_clicked__, "upper" ) } )
      ::oUI:btnSwLower  : connect( "clicked()", {|| ::execEvent( __btnSw_clicked__, "lower" ) } )
      ::oUI:btnSwDelete : connect( "clicked()", {|| ::execEvent( __btnSw_clicked__, "delete") } )

      ::oUI:btnNext     : connect( "clicked()", {|| ::execEvent( __btnNext_clicked__        ) } )
      ::oUI:btnBack     : connect( "clicked()", {|| ::execEvent( __btnBack_clicked__        ) } )
      ::oUI:btnNew      : connect( "clicked()", {|| ::execEvent( __btnNew_clicked__         ) } )
      ::oUI:btnSave     : connect( "clicked()", {|| ::execEvent( __btnSave_clicked__        ) } )
      ::oUI:btnCancel   : connect( "clicked()", {|| ::oUI:oWidget:done( 0 ) } )

      ::oUI:btnSwMax    : connect( "clicked()", {|| ::execEvent( __btnSwMax_clicked__       ) } )
      ::oUI:btnSwMin    : connect( "clicked()", {|| ::execEvent( __btnSwMin_clicked__       ) } )

      ::oUI:toolSrcGet  : connect( "clicked()", {|| ::execEvent( __toolSrcGet_clicked__     ) } )
      ::oUI:toolSrcMax  : connect( "clicked()", {|| ::execEvent( __toolSrcMax_clicked__     ) } )
      ::oUI:toolSrcMin  : connect( "clicked()", {|| ::execEvent( __toolSrcMin_clicked__     ) } )
      ::oUI:toolSrcDel  : connect( "clicked()", {|| ::execEvent( __toolSrcDel_clicked__     ) } )

      ::oUI:comboProjType : addItem( "Executable"        )
      ::oUI:comboProjType : addItem( "Library"           )
      ::oUI:comboProjType : addItem( "DLL - Without HVM" )
      ::oUI:comboProjType : addItem( "DLL - With HVM"    )

      ::oUI:comboGT : addItem( "gtCGI" )
      ::oUI:comboGT : addItem( "gtCRS" )
      ::oUI:comboGT : addItem( "gtDOS" )
      ::oUI:comboGT : addItem( "gtGUI" )
      ::oUI:comboGT : addItem( "gtOS2" )
      ::oUI:comboGT : addItem( "gtPCA" )
      ::oUI:comboGT : addItem( "gtSLN" )
      ::oUI:comboGT : addItem( "gtSTD" )
      ::oUI:comboGT : addItem( "gtTRM" )
      ::oUI:comboGT : addItem( "gtWIN" )
      ::oUI:comboGT : addItem( "gtWVT" )
      ::oUI:comboGT : addItem( "gtWVG" )
      ::oUI:comboGT : addItem( "gtXWC" )

      ::oUI:treeProps:setContextMenuPolicy( Qt_CustomContextMenu )
      ::oUI:treeProps:setDragEnabled( .t. )
      ::oUI:treeProps:setDropIndicatorShown( .t. )
      ::oUI:treeProps:setAcceptDrops( .t. )
      ::oUI:treeProps:setDragDropMode( QAbstractItemView_InternalMove )
      ::oUI:treeProps:setRootIsDecorated( .F. ) /* Important to present as a list */
      ::oUI:treeProps:header():resizeSection( 0, 237 )
      ::oUI:treeProps:connect( "itemCollapsed(QTreeWidgetItem*)"        , {|p   | ::execEvent( __treeProps_itemCollapsed__       , p     ) } )
      ::oUI:treeProps:connect( "itemExpanded(QTreeWidgetItem*)"         , {|p   | ::execEvent( __treeProps_itemExpanded__        , p     ) } )
      ::oUI:treeProps:connect( "customContextMenuRequested(QPoint)"     , {|p   | ::execEvent( __treeProps_contextMenuRequested__, p     ) } )
      ::oUI:treeProps:connect( "itemDoubleClicked(QTreeWidgetItem*,int)", {|p,p1| ::execEvent( __treeProps_doubleClicked__       , p, p1 ) } )
      ::oUI:treeProps:connect( "itemSelectionChanged()"                 , {|    | ::execEvent( __treeProps_itemSelectionChanged__        ) } )

      oBrush := QBrush( QColor( 248, 248, 248 ) )

      aadd( ::aItmProps, { NIL, "Hbc Files"                     , QBrush( QColor( 136, 136, 136 ) ),  oBrush, NIL, NIL, "background-color: rgb(136,136,136);" } )
      aadd( ::aItmProps, { NIL, "Libraries"                     , QBrush( QColor( 144, 144, 144 ) ),  oBrush, NIL, NIL, "background-color: rgb(144,144,144);" } )
      aadd( ::aItmProps, { NIL, "Library Paths"                 , QBrush( QColor( 152, 152, 152 ) ),  oBrush, NIL, NIL, "background-color: rgb(152,152,152);" } )
      aadd( ::aItmProps, { NIL, "Include Paths"                 , QBrush( QColor( 160, 160, 160 ) ),  oBrush, NIL, NIL, "background-color: rgb(160,160,160);" } )
      aadd( ::aItmProps, { NIL, "PRG Defines"                   , QBrush( QColor( 168, 168, 168 ) ),  oBrush, NIL, NIL, "background-color: rgb(168,168,168);" } )
      aadd( ::aItmProps, { NIL, "PRG Undefines"                 , QBrush( QColor( 176, 176, 176 ) ),  oBrush, NIL, NIL, "background-color: rgb(176,176,176);" } )
      aadd( ::aItmProps, { NIL, "hbmk2 Command-Line Params"     , QBrush( QColor( 184, 184, 184 ) ),  oBrush, NIL, NIL, "background-color: rgb(184,184,184);" } )
      aadd( ::aItmProps, { NIL, "Batch File Commands"           , QBrush( QColor( 192, 192, 192 ) ),  oBrush, NIL, NIL, "background-color: rgb(192,192,192);" } )
      aadd( ::aItmProps, { NIL, "Actions after Successful Build", QBrush( QColor( 200, 200, 200 ) ),  oBrush, NIL, NIL, "background-color: rgb(200,200,200);" } )


      ::oUI:treeSrc:setContextMenuPolicy( Qt_CustomContextMenu )
      ::oUI:treeSrc:setDragEnabled( .t. )
      ::oUI:treeSrc:setDropIndicatorShown( .t. )
      ::oUI:treeSrc:setAcceptDrops( .t. )
      ::oUI:treeSrc:setDragDropMode( QAbstractItemView_InternalMove )
      ::oUI:treeSrc:setRootIsDecorated( .F. ) /* Important to present as a list */
      ::oUI:treeSrc:header():resizeSection( 0, 393 )
      ::oUI:treeSrc:connect( "itemCollapsed(QTreeWidgetItem*)"        , {|p   | ::execEvent( __treeSrc_itemCollapsed__       , p     ) } )
      ::oUI:treeSrc:connect( "itemExpanded(QTreeWidgetItem*)"         , {|p   | ::execEvent( __treeSrc_itemExpanded__        , p     ) } )
      ::oUI:treeSrc:connect( "customContextMenuRequested(QPoint)"     , {|p   | ::execEvent( __treeSrc_contextMenuRequested__, p     ) } )
      ::oUI:treeSrc:connect( "itemDoubleClicked(QTreeWidgetItem*,int)", {|p,p1| ::execEvent( __treeSrc_doubleClicked__       , p, p1 ) } )

      aadd( ::aItmSrc, { NIL, "PRG Files"      , QBrush( QColor( 184, 184, 184 ) ), oBrush, ".prg", NIL, "background-color: rgb(184,184,184);" } )
      aadd( ::aItmSrc, { NIL, "C Files"        , QBrush( QColor( 176, 176, 176 ) ), oBrush, ".c"  , NIL, "background-color: rgb(176,176,176);" } )
      aadd( ::aItmSrc, { NIL, "CPP Files"      , QBrush( QColor( 168, 168, 168 ) ), oBrush, ".cpp", NIL, "background-color: rgb(168,168,168);" } )
      aadd( ::aItmSrc, { NIL, "CH Files"       , QBrush( QColor( 160, 160, 160 ) ), oBrush, ".ch" , NIL, "background-color: rgb(160,160,160);" } )
      aadd( ::aItmSrc, { NIL, "H Files"        , QBrush( QColor( 152, 152, 152 ) ), oBrush, ".h"  , NIL, "background-color: rgb(152,152,152);" } )
      aadd( ::aItmSrc, { NIL, "UI Files"       , QBrush( QColor( 144, 144, 144 ) ), oBrush, ".ui" , NIL, "background-color: rgb(144,144,144);" } )
      aadd( ::aItmSrc, { NIL, "All Other Files", QBrush( QColor( 136, 136, 136 ) ), oBrush, "*"   , NIL, "background-color: rgb(136,136,136);" } )


      ::clear()

      ::oUI:frameSrc:setAcceptDrops( .t. )
      ::oUI:frameSrc:connect( QEvent_DragEnter, {|p| ::execEvent( __frameSrc_dragEnterEvent__, p ) } )
      ::oUI:frameSrc:connect( QEvent_DragMove , {|p| ::execEvent( __frameSrc_dragMoveEvent__ , p ) } )
      ::oUI:frameSrc:connect( QEvent_Drop     , {|p| ::execEvent( __frameSrc_dropEvent__     , p ) } )

   ENDIF

   IF ::loadDefaults()
      ::oUI:exec()
      ::oUI:oWidget:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:execEvent( nEvent, p, p1 )
   LOCAL cText, qItm, n, i, oFont, aMenu, aFiles, aFilt, cFile, lTop, nChildren, qChild
   LOCAL qMime, qUrl, qList, aItm

   HB_SYMBOL_UNUSED( p1 )

   SWITCH nEvent

   CASE __frameSrc_dragMoveEvent__
   CASE __frameSrc_dragEnterEvent__
      p:acceptProposedAction()
      EXIT
   CASE __frameSrc_dropEvent__
      qMime := p:mimeData()
      IF qMime:hasUrls()
         qList := qMime:urls()
         FOR i := 0 TO qList:size() - 1
            qUrl := qList:at( i )
            ::addSourceFile( qUrl:toLocalFile() )  /* Raw Form Only */
         NEXT
         p:setDropAction( Qt_CopyAction )
         p:accept()
      ENDIF
      EXIT
   CASE __treeProps_itemSelectionChanged__
      IF ! empty( qItm := ::oUI:treeProps:currentItem() )
         lTop := ::oUI:treeProps:indexOfTopLevelItem( qItm ) >= 0
         ::oUI:btnSwPlus:setEnabled( lTop )
         ::oUI:btnSwAZ:setEnabled( lTop )
         ::oUI:btnSwZA:setEnabled( lTop )
      ENDIF
      EXIT
   CASE __treeProps_itemCollapsed__
   CASE __treeProps_itemExpanded__
      IF ( n := ::oUI:treeProps:indexOfTopLevelItem( p ) ) >= 0
         n++
         IF hb_isObject( ::aItmProps[ n, 6 ] )
            ::aItmProps[ n, 6 ]:setIcon( QIcon( hbide_image( iif( nEvent == __treeProps_itemExpanded__, "collapse_m", "expand_m" ) ) ) )
         ENDIF
         p:setSelected( .t. )
      ENDIF
      EXIT
   CASE __treeSrc_itemExpanded__
   CASE __treeSrc_itemCollapsed__
      IF ( n := ::oUI:treeSrc:indexOfTopLevelItem( p ) ) >= 0
         IF hb_isObject( ::aItmSrc[ n+1,6 ] )
            ::aItmSrc[ n+1,6 ]:setIcon( QIcon( hbide_image( iif( nEvent == __treeSrc_itemCollapsed__, "expand_m", "collapse_m" ) ) ) )
         ENDIF
         p:setSelected( .t. )
      ENDIF
      EXIT
   CASE __qTBtn_clicked__
      IF ::aItmProps[ p,1 ]:isExpanded()
         ::aItmProps[ p,1 ]:setExpanded( .f. )
      ELSE
         IF ::aItmProps[ p,1 ]:childCount() > 0
            ::aItmProps[ p,1 ]:setExpanded( .t. )
         ENDIF
      ENDIF
      IF ! empty( qItm := ::oUI:treeProps:currentItem() )
         qItm:setSelected( .f. )
      ENDIF
      ::aItmProps[ p,1 ]:setSelected( .t. )
      EXIT
   CASE __qSBtn_clicked__
      IF ::aItmSrc[ p,1 ]:isExpanded()
         ::aItmSrc[ p,1 ]:setExpanded( .f. )
      ELSE
         IF ::aItmSrc[ p,1 ]:childCount() > 0
            ::aItmSrc[ p,1 ]:setExpanded( .t. )
         ENDIF
      ENDIF
      IF ! empty( qItm := ::oUI:treeSrc:currentItem() )
         qItm:setSelected( .f. )
      ENDIF
      ::aItmSrc[ p,1 ]:setSelected( .t. )
      EXIT
   CASE __toolSrcDel_clicked__
      IF hbide_getYesNo( "Do you really want to delete all sources ?", "Dangerous Action", "Confirmation Required!", ::oUI:oWidget )
         FOR EACH aItm IN ::aItmSrc
            IF ! empty( aItm[ 1 ] )
               n := aItm[ 1 ]:childCount()
               FOR i := 1 TO n
                  aItm[ 1 ]:removeChild( aItm[ 1 ]:child( 0 ) )
               NEXT
            ENDIF
         NEXT
      ENDIF
      EXIT
   CASE __toolSrcMin_clicked__
      FOR EACH aItm IN ::aItmSrc
         IF ! empty( aItm[ 1 ] )
            aItm[ 1 ]:setExpanded( .F. )
         ENDIF
      NEXT
      EXIT
   CASE __toolSrcMax_clicked__
      FOR EACH aItm IN ::aItmSrc
         IF ! empty( aItm[ 1 ] )
            aItm[ 1 ]:setExpanded( .T. )
         ENDIF
      NEXT
      EXIT
   CASE __btnSwMin_clicked__
      FOR EACH aItm IN ::aItmProps
         IF ! empty( aItm[ 1 ] )
            aItm[ 1 ]:setExpanded( .F. )
         ENDIF
      NEXT
      EXIT
   CASE __btnSwMax_clicked__
      FOR EACH aItm IN ::aItmProps
         IF ! empty( aItm[ 1 ] )
            aItm[ 1 ]:setExpanded( .T. )
         ENDIF
      NEXT
      EXIT
   CASE __toolSrcGet_clicked__
      aFilt := {}
      aadd( aFilt, { "Program Files", "*.prg" } )
      aadd( aFilt, { "C Files"      , "*.c"   } )
      aadd( aFilt, { "CPP Files"    , "*.cpp" } )
      aadd( aFilt, { "H Files"      , "*.h"   } )
      aadd( aFilt, { "CH Files"     , "*.ch"  } )
      aadd( aFilt, { "All Others"   , "*"     } )

      aFiles := hbide_fetchAFile( ::oDlg, "Select Sources", aFilt, ::cProjPath, , .t. )
      IF ! empty( aFiles )
         oFont := QFont( "Courier New" )
         oFont:setPointSize( 8 )
         FOR EACH cFile IN aFiles
            ::addSourceFile( cFile )
         NEXT
      ENDIF
      EXIT
   CASE __treeSrc_contextMenuRequested__
      IF ! empty( qItm := ::oUI:treeSrc:itemAt( p ) )
         cText := substr( qItm:text( 0 ), 8 )
         aMenu := {}
         aadd( aMenu, { ::oAC:getAction( "Delete" ) } )
         IF ( n := ascan( ::aItmSrc, {|e_| e_[ 2 ] == cText } ) ) > 0
            aadd( aMenu, { ::oAC:getAction( "SortAZ" ) } )
            aadd( aMenu, { ::oAC:getAction( "SortZA" ) } )
         ENDIF
         cText := hbide_execPopup( aMenu, ::oUI:treeSrc:mapToGlobal( p ), ::oUI:treeSrc )
         IF cText == "Delete"
            IF n > 0
               n := qItm:childCount()
               FOR i := 1 TO n
                  ::deleteTreeItem( qItm:child( 0 ) )
               NEXT
            ELSE
               ::deleteTreeItem( qItm )
            ENDIF
         ELSEIF cText == "Sort Ascending"
            ::aItmSrc[ n,1 ]:sortChildren( 0, Qt_AscendingOrder )
         ELSEIF cText == "Sort Descending"
            ::aItmSrc[ n,1 ]:sortChildren( 0, Qt_DescendingOrder )
         ENDIF
      ENDIF
      EXIT
   CASE __treeProps_contextMenuRequested__
      IF ! empty( qItm := ::oUI:treeProps:itemAt( p ) )
         cText := substr( qItm:text( 0 ), 8 )
         aMenu := {}
         aadd( aMenu, { ::oAC:getAction( "Delete" ) } )
         IF ( n := ascan( ::aItmProps, {|e_| e_[ 2 ] == cText } ) ) > 0
            aadd( aMenu, { ::oAC:getAction( "SortAZ" ) } )
            aadd( aMenu, { ::oAC:getAction( "SortZA" ) } )
         ENDIF
         cText := hbide_execPopup( aMenu, ::oUI:treeProps:mapToGlobal( p ), ::oUI:treeProps )
         IF cText == "Delete"
            IF n > 0
               n := qItm:childCount()
               FOR i := 1 TO n
                  ::deleteTreeItem( qItm:child( 0 ) )
               NEXT
            ELSE
               ::deleteTreeItem( qItm )
            ENDIF
         ELSEIF cText == "Sort Ascending"
            ::aItmProps[ n,1 ]:sortChildren( 0, Qt_AscendingOrder )
         ELSEIF cText == "Sort Descending"
            ::aItmProps[ n,1 ]:sortChildren( 0, Qt_DescendingOrder )
         ENDIF
      ENDIF
      EXIT
   CASE __treeSrc_doubleClicked__
      EXIT
   CASE __btnSw_clicked__
      IF empty( qItm := ::oUI:treeProps:currentItem() )
         EXIT
      ENDIF
      IF ::oUI:treeProps:indexOfTopLevelItem( qItm ) >= 0
         IF p == "plus"
           ::addTreeItem( qItm )

         ELSEIF ( nChildren := qItm:childCount() ) > 0
            IF     p == "az"
               qItm:sortChildren( 0, Qt_AscendingOrder )
            ELSEIF p == "za"
               qItm:sortChildren( 0, Qt_DescendingOrder )
            ELSEIF p == "delete"
               FOR i := 1 TO nChildren
                  ::deleteTreeItem( qItm:child( 0 ) )
               NEXT
            ELSE
               FOR i := 1 TO nChildren
                  qChild := qItm:child( i-1 )
                  IF     p == "upper" ; qChild:setText( 0, upper( qChild:text( 0 ) ) )
                  ELSEIF p == "lower" ; qChild:setText( 0, lower( qChild:text( 0 ) ) )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      ELSE      // Indivisual node
         SWITCH p
         CASE "delete" ; ::deleteTreeItem( qItm )                  ; EXIT
         CASE "upper"  ; qItm:setText( 0, upper( qItm:text( 0 ) ) ); EXIT
         CASE "lower"  ; qItm:setText( 0, lower( qItm:text( 0 ) ) ); EXIT
         ENDSWITCH
      ENDIF
      EXIT
   CASE __treeProps_doubleClicked__
      IF ::oUI:treeProps:indexOfTopLevelItem( p ) >= 0
         ::addTreeItem( p )
      ENDIF
      EXIT
   CASE __btnNew_clicked__
      IF ::lEdited .AND. hbide_getYesNo( "Create new without saving current ?", "Current project has not been saved !", "Please Confirm", ::oUI:oWidget )
         ::clear()
         IF ! ::loadDefaults()
            ::oUI:oWidget:done( 0 )
         ENDIF
      ENDIF
      EXIT
   CASE __btnSave_clicked__
      ::saveProject()
      EXIT
   CASE __btnNext_clicked__
      IF ::oUI:stackedWidget:currentIndex() == 0
         ::oUI:stackedWidget:setcurrentIndex( 1 )
      ENDIF
      EXIT
   CASE __btnBack_clicked__
      IF ::oUI:stackedWidget:currentIndex() == 1
         ::oUI:stackedWidget:setcurrentIndex( 0 )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:addSourceFile( cFile )
   LOCAL oParent, qItm, cExt, n, cSource, lExists, i
   LOCAL oFont := QFont( "Courier New" )

   oFont:setPointSize( 8 )

   IF hbide_isValidText( cFile )
      hb_fNameSplit( cFile, , , @cExt )
      IF ( n := ascan( ::aItmSrc, {|e_| e_[ 5 ] == lower( cExt ) } ) ) == 0
         n := len( ::aItmSrc )
      ENDIF
      oParent := ::aItmSrc[ n,1 ]

   // cSource := hbide_prepareSourceForHbp( hbide_stripRoot( ::cProjPath, cFile ) ) /* this action be performed on final .hbp */
      cSource := cFile

      lExists := .f.
      IF ( n := oParent:childCount() ) > 0
         FOR i := 1 TO n
            IF lower( oParent:child( i-1 ):text( 0 ) ) == lower( cSource )
               lExists := .t.
               EXIT
            ENDIF
         NEXT
      ENDIF

      IF ! lExists
         oParent:setExpanded( .t. )
         qItm := QTreeWidgetItem()
         oParent:addChild( qItm )
         qItm:setFlags( 0 )
         qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDragEnabled, Qt_ItemIsEnabled ) )
         qItm:setText( 0, cSource )
         qItm:setTooltip( 0, cFile )
         qItm:setFont( 0, oFont )
         qItm:setBackground( 0, QBrush( QColor( 250, 250, 250 ) ) )
      ENDIF
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:addTreeItem( oParent )
   LOCAL oFont, qItm

   oFont := QFont( "Courier New" )
   oFont:setPointSize( 8 )

   oParent:setExpanded( .t. )

   qItm := QTreeWidgetItem()
   oParent:addChild( qItm )
   qItm:setFlags( 0 )
   qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDragEnabled, Qt_ItemIsEnabled, Qt_ItemIsEditable ) )
   qItm:setText( 0, "" )
   qItm:setFont( 0, oFont )
   qItm:setBackground( 0, QBrush( QColor( 250, 250, 250 ) ) )
   qItm:setFirstColumnSpanned( .t. )

   oParent:treeWidget():editItem( qItm, 0 )

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:deleteTreeItem( oChild )
   LOCAL oParent

   oParent := oChild:parent()
   oParent:removeChild( oChild )
   IF oParent:childCount() == 0
      oParent:setExpanded( .f. )
      oParent:setSelected( .t. )
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:addDropIndicator( oTree, oNode, nMsg, cCSS, nIndex )
   LOCAL qTBtn := QToolButton()

   qTBtn:setIcon( QIcon( hbide_image( "expand_m" ) ) )
   qTBtn:setAutoFillBackground( .t. )
   qTBtn:setAutoRaise( .t. )
   qTBtn:setMaximumWidth( 20 )
   qTBtn:setMaximumHeight( 20 )
   qTBtn:setStyleSheet( "" )
   qTBtn:setStyleSheet( cCSS )
   qTBtn:connect( "clicked()", {|| ::execEvent( nMsg, nIndex ) } )
   oTree:setItemWidget( oNode, 1, qTBtn )

   RETURN qTBtn

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:loadSwichesSections()
   LOCAL oTree := ::oUI:treeProps
   LOCAL qItm, aAct, oFont

   oFont := QTreeWidgetItem():font( 0 )
   oFont:setBold( .t. )
   FOR EACH aAct IN ::aItmProps
      qItm := QTreeWidgetItem()
      aAct[ 1 ] := qItm
      qItm:setFlags( 0 )
      qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDropEnabled, Qt_ItemIsEnabled ) )
      qItm:setText( 0, space( 7 ) + aAct[ 2 ] )
      qItm:setBackground( 0, aAct[ 3 ] )
      qItm:setForeground( 0, QBrush( QColor( 255,255,255 ) ) )
      qItm:setFont( 0, oFont )
      qItm:setTooltip( 0, "Double-click to add a value !" )
      oTree:addTopLevelItem( qItm )
      oTree:setFirstItemColumnSpanned( qItm, .t. )
      qItm:setChildIndicatorPolicy( QTreeWidgetItem_ShowIndicator )

      aAct[ 6 ] := ::addDropIndicator( oTree, aAct[ 1 ], __qTBtn_clicked__, aAct[ 7 ], aAct:__enumIndex() )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:loadSourcesSections()
   LOCAL oTree := ::oUI:treeSrc
   LOCAL qItm, aAct, oFont

   oFont := QTreeWidgetItem():font( 0 )
   oFont:setBold( .t. )
   FOR EACH aAct IN ::aItmSrc
      qItm := QTreeWidgetItem()
      aAct[ 1 ] := qItm
      qItm:setFlags( 0 )
      qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDropEnabled, Qt_ItemIsEnabled ) )
      qItm:setText( 0, space( 7 ) + aAct[ 2 ] )
      qItm:setBackground( 0, aAct[ 3 ] )
      qItm:setForeground( 0, QBrush( QColor( 255,255,255 ) ) )
      qItm:setFont( 0, oFont )
      qItm:setTooltip( 0, "Drag and drop a source OR select with open icon at the top !" )
      oTree:addTopLevelItem( qItm )
      oTree:setFirstItemColumnSpanned( qItm, .t. )
      qItm:setChildIndicatorPolicy( QTreeWidgetItem_ShowIndicator )

      aAct[ 6 ] := ::addDropIndicator( oTree, aAct[ 1 ], __qSBtn_clicked__, aAct[ 7 ], aAct:__enumIndex() )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:loadDefaults()
   LOCAL cProjPath, cPath, cName, cExt

   cProjPath := ::oUI:editProjPath:text()
   IF empty( cProjPath )
      cProjPath := hbide_fetchAFile( ::oDlg, "Create a Harbour Project File", { { "Harbour Project", ".hbp" } }, hb_dirBase() + "projects" + hb_ps(), "hbp" )
      IF empty( cProjPath )
         RETURN .f.
      ENDIF
      IF hb_fileExists( cProjPath )
         MsgBox( "Project file already exists, cannot reload in wizard!" )
         RETURN .f.
      ENDIF
      hb_fNameSplit( cProjPath, @cPath, @cName, @cExt )
      IF ! ( lower( cExt ) == ".hbp" )
         MsgBox( "Wrong type of project name !" )
         RETURN .f.
      ENDIF
      ::cProjPath := cPath

      ::oUI:editProjPath:setText( cProjPath )
      ::oUI:editProjName:setText( upper( substr( cName, 1, 1 ) ) + lower( substr( cName, 2 ) ) )
      ::oUI:editOutName:setText( cName )

      ::lEdited := .t.
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:saveProject()

   RETURN Self

/*----------------------------------------------------------------------*/
//                          CLASS IdeExProject
/*----------------------------------------------------------------------*/

CREATE CLASS IdeExProject

   DATA   cTmplt                                  INIT ""

   DATA   cPathTmplt
   DATA   cPathHbp

   DATA   aSrcALL                                 INIT {}
   DATA   aSrcPRG                                 INIT {}
   DATA   aSrcC                                   INIT {}
   DATA   aSrcCPP                                 INIT {}
   DATA   aSrcCH                                  INIT {}
   DATA   aSrcH                                   INIT {}
   DATA   aSrcUI                                  INIT {}
   DATA   aSrcRest                                INIT {}

   DATA   aHbc                                    INIT {}
   DATA   aFlags

   METHOD new( cPathTmplt )
   METHOD create( cPathTmplt )
   METHOD loadUI( oUI )
   METHOD saveUI( oUI )
   METHOD load()
   METHOD save( nMode )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeExProject:new( cPathTmplt )

   hb_default( cPathTmplt, ::cPathTmplt )

   ::cPathTmplt := cPathTmplt

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:create( cPathTmplt )

   hb_default( cPathTmplt, ::cPathTmplt )

   ::cPathTmplt := cPathTmplt

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:load()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:save( nMode )
   HB_SYMBOL_UNUSED( nMode )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:loadUI( oUI )
   HB_SYMBOL_UNUSED( oUI )


   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:saveUI( oUI )
   HB_SYMBOL_UNUSED( oUI )

   RETURN Self

/*----------------------------------------------------------------------*/

