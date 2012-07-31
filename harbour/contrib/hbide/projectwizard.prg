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

#define PROPS_TREENODE                            1
#define PROPS_PROPNAME                            2
#define PROPS_TREEBUTTON                          6

#define UI_LOAD_NORMAL                            0
#define UI_LOAD_DEFAULTS                          1

/*----------------------------------------------------------------------*/

CREATE CLASS IdeProjectWizard INHERIT IdeObject

   DATA    oProject
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

   ::oProject := IdeExProject():new()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:clear()

   ::oUI:comboProjType    : setCurrentIndex( 0 )
   ::oUI:comboGT          : setCurrentIndex( 9 )

   ::oUI:treeProps        : clear()
   ::loadSwichesSections()

   ::oUI:treeSrc          : clear()
   ::loadSourcesSections()

   ::oProject:loadUI( Self, UI_LOAD_DEFAULTS )

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
         IF hb_isObject( ::aItmProps[ n, PROPS_TREEBUTTON ] )
            ::aItmProps[ n, PROPS_TREEBUTTON ]:setIcon( QIcon( hbide_image( iif( nEvent == __treeProps_itemExpanded__, "collapse_m", "expand_m" ) ) ) )
         ENDIF
         p:setSelected( .t. )
      ENDIF
      EXIT
   CASE __treeSrc_itemExpanded__
   CASE __treeSrc_itemCollapsed__
      IF ( n := ::oUI:treeSrc:indexOfTopLevelItem( p ) ) >= 0
         IF hb_isObject( ::aItmSrc[ n+1, PROPS_TREEBUTTON ] )
            ::aItmSrc[ n+1, PROPS_TREEBUTTON ]:setIcon( QIcon( hbide_image( iif( nEvent == __treeSrc_itemCollapsed__, "expand_m", "collapse_m" ) ) ) )
         ENDIF
         p:setSelected( .t. )
      ENDIF
      EXIT
   CASE __qTBtn_clicked__
      IF ::aItmProps[ p, PROPS_TREENODE ]:isExpanded()
         ::aItmProps[ p, PROPS_TREENODE ]:setExpanded( .f. )
      ELSE
         IF ::aItmProps[ p, PROPS_TREENODE ]:childCount() > 0
            ::aItmProps[ p, PROPS_TREENODE ]:setExpanded( .t. )
         ENDIF
      ENDIF
      IF ! empty( qItm := ::oUI:treeProps:currentItem() )
         qItm:setSelected( .f. )
      ENDIF
      ::aItmProps[ p, PROPS_TREENODE ]:setSelected( .t. )
      EXIT
   CASE __qSBtn_clicked__
      IF ::aItmSrc[ p, PROPS_TREENODE ]:isExpanded()
         ::aItmSrc[ p, PROPS_TREENODE ]:setExpanded( .f. )
      ELSE
         IF ::aItmSrc[ p, PROPS_TREENODE ]:childCount() > 0
            ::aItmSrc[ p, PROPS_TREENODE ]:setExpanded( .t. )
         ENDIF
      ENDIF
      IF ! empty( qItm := ::oUI:treeSrc:currentItem() )
         qItm:setSelected( .f. )
      ENDIF
      ::aItmSrc[ p, PROPS_TREENODE ]:setSelected( .t. )
      EXIT
   CASE __toolSrcDel_clicked__
      IF hbide_getYesNo( "Do you really want to delete all sources ?", "Dangerous Action", "Confirmation Required!", ::oUI:oWidget )
         FOR EACH aItm IN ::aItmSrc
            IF ! empty( aItm[ PROPS_TREENODE ] )
               n := aItm[ PROPS_TREENODE ]:childCount()
               FOR i := 1 TO n
                  aItm[ PROPS_TREENODE ]:removeChild( aItm[ PROPS_TREENODE ]:child( 0 ) )
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
         IF ! empty( aItm[ PROPS_TREENODE ] )
            aItm[ PROPS_TREENODE ]:setExpanded( .T. )
         ENDIF
      NEXT
      EXIT
   CASE __btnSwMin_clicked__
      FOR EACH aItm IN ::aItmProps
         IF ! empty( aItm[ PROPS_TREENODE ] )
            aItm[ PROPS_TREENODE ]:setExpanded( .F. )
         ENDIF
      NEXT
      EXIT
   CASE __btnSwMax_clicked__
      FOR EACH aItm IN ::aItmProps
         IF ! empty( aItm[ PROPS_TREENODE ] )
            aItm[ PROPS_TREENODE ]:setExpanded( .T. )
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
            ::aItmSrc[ n, PROPS_TREENODE ]:sortChildren( 0, Qt_AscendingOrder )
         ELSEIF cText == "Sort Descending"
            ::aItmSrc[ n, PROPS_TREENODE ]:sortChildren( 0, Qt_DescendingOrder )
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
            ::aItmProps[ n, PROPS_TREENODE ]:sortChildren( 0, Qt_AscendingOrder )
         ELSEIF cText == "Sort Descending"
            ::aItmProps[ n, PROPS_TREENODE ]:sortChildren( 0, Qt_DescendingOrder )
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
      aAct[ PROPS_TREENODE ] := qItm
      qItm:setFlags( 0 )
      qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDropEnabled, Qt_ItemIsEnabled ) )
      qItm:setText( 0, space( 7 ) + aAct[ PROPS_PROPNAME ] )
      qItm:setBackground( 0, aAct[ 3 ] )
      qItm:setForeground( 0, QBrush( QColor( 255,255,255 ) ) )
      qItm:setFont( 0, oFont )
      qItm:setTooltip( 0, "Double-click to add a value !" )
      oTree:addTopLevelItem( qItm )
      oTree:setFirstItemColumnSpanned( qItm, .t. )
      qItm:setChildIndicatorPolicy( QTreeWidgetItem_ShowIndicator )

      aAct[ PROPS_TREEBUTTON ] := ::addDropIndicator( oTree, aAct[ PROPS_TREENODE ], __qTBtn_clicked__, aAct[ 7 ], aAct:__enumIndex() )
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
      aAct[ PROPS_TREENODE ] := qItm
      qItm:setFlags( 0 )
      qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDropEnabled, Qt_ItemIsEnabled ) )
      qItm:setText( 0, space( 7 ) + aAct[ PROPS_PROPNAME ] )
      qItm:setBackground( 0, aAct[ 3 ] )
      qItm:setForeground( 0, QBrush( QColor( 255,255,255 ) ) )
      qItm:setFont( 0, oFont )
      qItm:setTooltip( 0, "Drag and drop a source OR select with open icon at the top !" )
      oTree:addTopLevelItem( qItm )
      oTree:setFirstItemColumnSpanned( qItm, .t. )
      qItm:setChildIndicatorPolicy( QTreeWidgetItem_ShowIndicator )

      aAct[ PROPS_TREEBUTTON ] := ::addDropIndicator( oTree, aAct[ PROPS_TREENODE ], __qSBtn_clicked__, aAct[ 7 ], aAct:__enumIndex() )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:loadDefaults()
   LOCAL cProjPath, cPath, cName, cExt, lTmpltExists := .f.

   cProjPath := ::oUI:editProjPath:text()
   IF empty( cProjPath )
      cProjPath := hbide_fetchAFile( ::oDlg, "Create a Harbour Project File", { { "Harbour Project", ".hbp" } }, hb_dirBase() + "projects" + hb_ps(), "hbp" )
      IF empty( cProjPath )
         RETURN .f.
      ENDIF

      hb_fNameSplit( cProjPath, @cPath, @cName, @cExt )
      IF ! ( lower( cExt ) == ".hbp" )
         MsgBox( "Wrong type of project name !" )
         RETURN .f.
      ENDIF

      IF hb_fileExists( cPath + cName + ".tpl" )
         lTmpltExists := .t.
      ELSEIF hb_fileExists( cProjPath )
         MsgBox( "Project file already exists, cannot reload in wizard!" )
         RETURN .f.
      ENDIF
      ::cProjPath := cPath

      IF lTmpltExists
         ::oProject:load( cPath + cName + ".tpl" )
         ::oProject:loadUI( Self, UI_LOAD_NORMAL )
      ELSE
         ::oUI:editProjPath:setText( cProjPath )
         ::oUI:editProjName:setText( upper( substr( cName, 1, 1 ) ) + lower( substr( cName, 2 ) ) )
         ::oUI:editOutName:setText( cName )
      ENDIF

      ::lEdited := .t.
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:saveProject()
   LOCAL cPath, cFile, cExt

   ::oProject:saveUI( Self )
   hb_fNameSplit( ::oUI:editProjPath:text(), @cPath, @cFile, @cExt )
   ::oProject:save( cPath + cFile + ".tpl" )

   RETURN Self

/*----------------------------------------------------------------------*/
//                          CLASS IdeExProject
/*----------------------------------------------------------------------*/

CREATE CLASS IdeExProject

   DATA   cTmplt                                  INIT ""

   DATA   cPathTmplt
   DATA   cPathHbp

   DATA   cProjPath
   DATA   cProjName
   DATA   cProjType
   DATA   cOutName
   DATA   cOutPath
   DATA   cWorkPath
   DATA   cLaunchExe
   DATA   cLaunchParams
   DATA   cStayIn
   DATA   lXhb
   DATA   lXbase
   DATA   lHbQt
   DATA   lXbp
   DATA   lFwh
   DATA   lHmg
   DATA   lOther
   DATA   lA
   DATA   lB
   DATA   lES
   DATA   lG
   DATA   lJ
   DATA   lL
   DATA   lM
   DATA   lN
   DATA   lV
   DATA   lW
   DATA   lZ
   DATA   lQ
   DATA   lBuild
   DATA   lCredits
   DATA   cES
   DATA   cG
   DATA   cM
   DATA   cW
   DATA   cQ
   DATA   lInc
   DATA   lGui
   DATA   lMt
   DATA   lShared
   DATA   lFullStatic
   DATA   lTrace
   DATA   lInfo
   DATA   cGT
   DATA   lGtGui
   DATA   lGtWin
   DATA   lGtWvt
   DATA   lGtWvg
   DATA   lGtXwc
   DATA   lGtCgi
   DATA   lGtTrm
   DATA   lGtStd
   DATA   lGtSln
   DATA   lGtPca
   DATA   lGtOs2
   DATA   lGtCrs
   DATA   aExtras
   DATA   aPrpHbcs
   DATA   aPrpLibs
   DATA   aPrpLPaths
   DATA   aPrpIPaths
   DATA   aPrpDefines
   DATA   aPrpUnDefines
   DATA   aPrpHbmk2
   DATA   aPrpBatch
   DATA   aPrpAActions
   DATA   aSrcPrgs
   DATA   aSrcCs
   DATA   aSrcCpps
   DATA   aSrcChs
   DATA   aSrcHs
   DATA   aSrcUIs
   DATA   aSrcOthers

   METHOD new( cPathTmplt )
   METHOD create( cPathTmplt )
   METHOD defaults()
   METHOD setTmpltPath( cPathTmplt )              INLINE ::cPathTmplt := cPathTmplt
   METHOD load( cPathTmplt )
   METHOD save( cPathTmplt )
   METHOD loadUI( oWizard, nMode )
   METHOD saveUI( oWizard )
   METHOD retrieveProps( oWizard )
   METHOD retrieveSources( oWizard )
   METHOD retrieveExtras( oWizard )
   METHOD loadProps( oWizard )
   METHOD loadSources( oWizard )
   METHOD loadExtras( oWizard )
   METHOD addSection( aTxt, cSection, aValues )
   METHOD sectionToArray( cBuffer, cSection )
   METHOD getKeyValuePair( cStr )
   METHOD retrieveSection( cBuffer, cSection, aPost )

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

   ::defaults()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:defaults()

   ::cProjPath             := ""
   ::cProjName             := ""
   ::cProjType             := ""
   ::cOutName              := ""
   ::cOutPath              := ""
   ::cWorkPath             := ""
   ::cLaunchExe            := ""
   ::cLaunchParams         := ""
   ::cStayIn               := ""
   ::lXhb                  := .F.
   ::lXbase                := .F.
   ::lHbQt                 := .F.
   ::lXbp                  := .F.
   ::lFwh                  := .F.
   ::lHmg                  := .F.
   ::lOther                := .F.
   ::lA                    := .F.
   ::lB                    := .F.
   ::lES                   := .T.
   ::lG                    := .F.
   ::lJ                    := .F.
   ::lL                    := .F.
   ::lM                    := .T.
   ::lN                    := .T.
   ::lV                    := .F.
   ::lW                    := .F.
   ::lZ                    := .F.
   ::lQ                    := .F.
   ::lBuild                := .F.
   ::lCredits              := .F.
   ::cES                   := "0"
   ::cG                    := ""
   ::cM                    := ""
   ::cW                    := ""
   ::cQ                    := ""
   ::lInc                  := .T.
   ::lGui                  := .F.
   ::lMt                   := .F.
   ::lShared               := .F.
   ::lFullStatic           := .F.
   ::lTrace                := .F.
   ::lInfo                 := .T.
   ::cGT                   := ""
   ::lGtGui                := .F.
   ::lGtWin                := .F.
   ::lGtWvt                := .F.
   ::lGtWvg                := .F.
   ::lGtXwc                := .F.
   ::lGtCgi                := .F.
   ::lGtTrm                := .F.
   ::lGtStd                := .F.
   ::lGtSln                := .F.
   ::lGtPca                := .F.
   ::lGtOs2                := .F.
   ::lGtCrs                := .F.
   ::aExtras               := {}
   ::aPrpHbcs              := {}
   ::aPrpLibs              := {}
   ::aPrpLPaths            := {}
   ::aPrpIPaths            := {}
   ::aPrpDefines           := {}
   ::aPrpUnDefines         := {}
   ::aPrpHbmk2             := {}
   ::aPrpBatch             := {}
   ::aPrpAActions          := {}
   ::aSrcPrgs              := {}
   ::aSrcCs                := {}
   ::aSrcCpps              := {}
   ::aSrcChs               := {}
   ::aSrcHs                := {}
   ::aSrcUIs               := {}
   ::aSrcOthers            := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:loadUI( oWizard, nMode )
   LOCAL oUI := oWizard:oUI

   IF nMode == 1
      ::defaults()
   ENDIF

   oUI:editProjPath     : setText( ::cProjPath      )
   oUI:editProjName     : setText( ::cProjName      )
   oUI:editOutName      : setText( ::cOutName       )
   oUI:editOutPath      : setText( ::cOutPath       )
   oUI:editWorkPath     : setText( ::cWorkPath      )
   oUI:editWorkPath     : setText( ::cWorkPath      )
   oUI:editLaunchExe    : setText( ::cLaunchExe     )
   oUI:editLaunchParams : setText( ::cLaunchParams  )
   oUI:editStayIn       : setText( ::cStayIn        )
   oUi:chkXhb           : setChecked( ::lXhb        )
   oUi:chkXBase         : setChecked( ::lXBase      )
   oUi:chkHbQt          : setChecked( ::lHbQt       )
   oUi:chkXbp           : setChecked( ::lXbp        )
   oUi:chkFWH           : setChecked( ::lFWH        )
   oUi:chkHMG           : setChecked( ::lHMG        )
   oUi:chkOther         : setChecked( ::lOther      )
   oUi:chkA             : setChecked( ::lA          )
   oUi:chkB             : setChecked( ::lB          )
   oUi:chkES            : setChecked( ::lES         )
   oUi:chkG             : setChecked( ::lG          )
   oUi:chkJ             : setChecked( ::lJ          )
   oUi:chkL             : setChecked( ::lL          )
   oUi:chkM             : setChecked( ::lM          )
   oUi:chkN             : setChecked( ::lN          )
   oUi:chkV             : setChecked( ::lV          )
   oUi:chkW             : setChecked( ::lW          )
   oUi:chkZ             : setChecked( ::lZ          )
   oUi:chkQ             : setChecked( ::lQ          )
   oUi:chkBuild         : setChecked( ::lBuild      )
   oUi:chkCredits       : setChecked( ::lCredits    )
   oUi:chkInc           : setChecked( ::lInc        )
   oUi:chkGui           : setChecked( ::lGui        )
   oUi:chkInfo          : setChecked( ::lInfo       )
   oUi:chkShared        : setChecked( ::lShared     )
   oUi:chkMt            : setChecked( ::lMt         )
   oUi:chkTrace         : setChecked( ::lTrace      )
   oUi:chkFullstatic    : setChecked( ::lFullstatic )
   oUi:chkGtgui         : setChecked( ::lGtgui      )
   oUi:chkGtwin         : setChecked( ::lGtwin      )
   oUi:chkGtwvt         : setChecked( ::lGtwvt      )
   oUi:chkGtwvg         : setChecked( ::lGtwvg      )
   oUi:chkGtxwc         : setChecked( ::lGtxwc      )
   oUi:chkGttrm         : setChecked( ::lGttrm      )
   oUi:chkGtstd         : setChecked( ::lGtstd      )
   oUi:chkGtsln         : setChecked( ::lGtsln      )
   oUi:chkGtpca         : setChecked( ::lGtpca      )
   oUi:chkGtos2         : setChecked( ::lGtos2      )
   oUi:chkGtcrs         : setChecked( ::lGtcrs      )
   oUi:chkGtcgi         : setChecked( ::lGtcgi      )
   oUI:editES           : setText( ::cES            )
   oUI:editG            : setText( ::cG             )
   oUI:editM            : setText( ::cM             )
   oUI:editQ            : setText( ::cQ             )
   oUI:editW            : setText( ::cW             )

   ::loadProps( oWizard )
   ::loadSources( oWizard )
   ::loadExtras( oWizard )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:saveUI( oWizard )
   LOCAL oUI := oWizard:oUI

   ::defaults()  /* Clean the variables */

   ::cProjPath     := oUI:editProjPath     : text()
   ::cProjName     := oUI:editProjName     : text()
   ::cProjType     := oUI:comboProjType    : currentText()
   ::cOutName      := oUI:editOutName      : text()
   ::cOutPath      := oUI:editOutPath      : text()
   ::cWorkPath     := oUI:editWorkPath     : text()
   ::cLaunchExe    := oUI:editLaunchExe    : text()
   ::cLaunchParams := oUI:editLaunchParams : text()
   ::cStayIn       := oUI:editStayIn       : text()
   ::lXhb          := oUI:chkXhb           : isChecked()
   ::lXbase        := oUI:chkXbase         : isChecked()
   ::lHbQt         := oUI:chkHbQt          : isChecked()
   ::lXbp          := oUI:chkXbp           : isChecked()
   ::lFwh          := oUI:chkFwh           : isChecked()
   ::lHmg          := oUI:chkHmg           : isChecked()
   ::lOther        := oUI:chkOther         : isChecked()
   ::lA            := oUI:chkA             : isChecked()
   ::lB            := oUI:chkB             : isChecked()
   ::lES           := oUI:chkES            : isChecked()
   ::lG            := oUI:chkG             : isChecked()
   ::lJ            := oUI:chkJ             : isChecked()
   ::lL            := oUI:chkL             : isChecked()
   ::lM            := oUI:chkM             : isChecked()
   ::lN            := oUI:chkN             : isChecked()
   ::lV            := oUI:chkV             : isChecked()
   ::lW            := oUI:chkW             : isChecked()
   ::lZ            := oUI:chkZ             : isChecked()
   ::lQ            := oUI:chkQ             : isChecked()
   ::lBuild        := oUI:chkBuild         : isChecked()
   ::lCredits      := oUI:chkCredits       : isChecked()
   ::cES           := oUI:editES           : text()
   ::cG            := oUI:editG            : text()
   ::cM            := oUI:editM            : text()
   ::cW            := oUI:editW            : text()
   ::cQ            := oUI:editQ            : text()
   ::lInc          := oUI:chkInc           : isChecked()
   ::lGui          := oUI:chkGui           : isChecked()
   ::lMt           := oUI:chkMt            : isChecked()
   ::lShared       := oUI:chkShared        : isChecked()
   ::lFullStatic   := oUI:chkFullStatic    : isChecked()
   ::lTrace        := oUI:chkTrace         : isChecked()
   ::lInfo         := oUI:chkInfo          : isChecked()
   ::cGT           := oUI:comboGT          : currentText()
   ::lGtGui        := oUI:chkGtGui         : isChecked()
   ::lGtWin        := oUI:chkGtWin         : isChecked()
   ::lGtWvt        := oUI:chkGtWvt         : isChecked()
   ::lGtWvg        := oUI:chkGtWvg         : isChecked()
   ::lGtXwc        := oUI:chkGtXwc         : isChecked()
   ::lGtCgi        := oUI:chkGtCgi         : isChecked()
   ::lGtTrm        := oUI:chkGtTrm         : isChecked()
   ::lGtStd        := oUI:chkGtStd         : isChecked()
   ::lGtSln        := oUI:chkGtSln         : isChecked()
   ::lGtPca        := oUI:chkGtPca         : isChecked()
   ::lGtOs2        := oUI:chkGtOs2         : isChecked()
   ::lGtCrs        := oUI:chkGtCrs         : isChecked()

   ::retrieveProps( oWizard )
   ::retrieveSources( oWizard )
   ::retrieveExtras( oWizard )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:retrieveProps( oWizard )
   LOCAL a_, n, cNode, cText

   FOR EACH a_ IN oWizard:aItmProps
      IF hb_isObject( a_[ PROPS_TREENODE ] )
         cNode := alltrim( a_[ PROPS_TREENODE ]:text( 0 ) )
         FOR n := 1 TO a_[ PROPS_TREENODE ]:childCount()
            cText := alltrim( a_[ PROPS_TREENODE ]:child( n - 1 ):text( 0 ) )
            SWITCH cNode
            CASE "Hbc Files"                      ; AAdd( ::aPrpHbcs      , cText ) ; EXIT
            CASE "Libraries"                      ; AAdd( ::aPrpLibs      , cText ) ; EXIT
            CASE "Library Paths"                  ; AAdd( ::aPrpLPaths    , cText ) ; EXIT
            CASE "Include Paths"                  ; AAdd( ::aPrpIPaths    , cText ) ; EXIT
            CASE "PRG Defines"                    ; AAdd( ::aPrpDefines   , cText ) ; EXIT
            CASE "PRG Undefines"                  ; AAdd( ::aPrpUnDefines , cText ) ; EXIT
            CASE "hbmk2 Command-Line Params"      ; AAdd( ::aPrpHbmk2     , cText ) ; EXIT
            CASE "Batch File Commands"            ; AAdd( ::aPrpBatch     , cText ) ; EXIT
            CASE "Actions after Successful Build" ; AAdd( ::aPrpAActions  , cText ) ; EXIT
            ENDSWITCH
         NEXT
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:retrieveSources( oWizard )
   LOCAL a_, n, cNode, cText

   FOR EACH a_ IN oWizard:aItmSrc
      IF hb_isObject( a_[ PROPS_TREENODE ] )
         cNode := alltrim( a_[ PROPS_TREENODE ]:text( 0 ) )
         FOR n := 1 TO a_[ PROPS_TREENODE ]:childCount()
            cText := alltrim( a_[ PROPS_TREENODE ]:child( n - 1 ):text( 0 ) )
            SWITCH cNode
            CASE "PRG Files"       ; AAdd( ::aSrcPrgs   , cText ) ; EXIT
            CASE "C Files"         ; AAdd( ::aSrcCs     , cText ) ; EXIT
            CASE "CPP Files"       ; AAdd( ::aSrcCpps   , cText ) ; EXIT
            CASE "CH Files"        ; AAdd( ::aSrcChs    , cText ) ; EXIT
            CASE "H Files"         ; AAdd( ::aSrcHs     , cText ) ; EXIT
            CASE "UI Files"        ; AAdd( ::aSrcUIs    , cText ) ; EXIT
            CASE "All Other Files" ; AAdd( ::aSrcOthers , cText ) ; EXIT
            ENDSWITCH
         NEXT
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:retrieveExtras( oWizard )
   LOCAL s, a_:= hbide_memoToArray( oWizard:oUI:plainExtras:toPlainText() )

   FOR EACH s IN a_
      aadd( ::aExtras, alltrim( s ) )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:loadProps( oWizard )
   LOCAL a_, aValues, cValue, qItm

   FOR EACH a_ IN oWizard:aItmProps
      IF hb_isObject( a_[ PROPS_TREENODE ] )
         SWITCH alltrim( a_[ PROPS_TREENODE ]:text( 0 ) )
         CASE "Hbc Files"                      ; aValues := ::aPrpHbcs      ; EXIT
         CASE "Libraries"                      ; aValues := ::aPrpLibs      ; EXIT
         CASE "Library Paths"                  ; aValues := ::aPrpLPaths    ; EXIT
         CASE "Include Paths"                  ; aValues := ::aPrpIPaths    ; EXIT
         CASE "PRG Defines"                    ; aValues := ::aPrpDefines   ; EXIT
         CASE "PRG Undefines"                  ; aValues := ::aPrpUnDefines ; EXIT
         CASE "hbmk2 Command-Line Params"      ; aValues := ::aPrpHbmk2     ; EXIT
         CASE "Batch File Commands"            ; aValues := ::aPrpBatch     ; EXIT
         CASE "Actions after Successful Build" ; aValues := ::aPrpAActions  ; EXIT
         ENDSWITCH
         IF ! empty( aValues )
            FOR EACH cValue IN aValues
               IF ! empty( cValue )
                  qItm := QTreeWidgetItem()
                  qItm:setText( 0, cValue )
                  a_[ PROPS_TREENODE ]:addChild( qItm )
               ENDIF
            NEXT
         ENDIF
         a_[ PROPS_TREENODE ]:setExpanded( .t. )
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:loadSources( oWizard )
   LOCAL a_, aValues, cValue, qItm

   FOR EACH a_ IN oWizard:aItmSrc
      IF hb_isObject( a_[ PROPS_TREENODE ] )
         SWITCH alltrim( a_[ PROPS_TREENODE ]:text( 0 ) )
         CASE "PRG Files"       ; aValues := ::aSrcPrgs   ; EXIT
         CASE "C Files"         ; aValues := ::aSrcCs     ; EXIT
         CASE "CPP Files"       ; aValues := ::aSrcCpps   ; EXIT
         CASE "CH Files"        ; aValues := ::aSrcChs    ; EXIT
         CASE "H Files"         ; aValues := ::aSrcHs     ; EXIT
         CASE "UI Files"        ; aValues := ::aSrcUIs    ; EXIT
         CASE "All Other Files" ; aValues := ::aSrcOthers ; EXIT
         ENDSWITCH

         IF ! empty( aValues )
            FOR EACH cValue IN aValues
               IF ! empty( cValue )
                  qItm := QTreeWidgetItem()
                  qItm:setText( 0, cValue )
                  a_[ PROPS_TREENODE ]:addChild( qItm )
               ENDIF
            NEXT
         ENDIF
         a_[ PROPS_TREENODE ]:setExpanded( .t. )
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:loadExtras( oWizard )

   oWizard:oUI:plainExtras : setPlainText( hbide_arrayToMemo( ::aExtras ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:addSection( aTxt, cSection, aValues )
   LOCAL s

   AAdd( aTxt, "" )
   AAdd( aTxt, "      <" + cSection + ">" )
   FOR EACH s IN aValues
      AAdd( aTxt, "         " + s )
   NEXT
   AAdd( aTxt, "      </" + cSection + ">" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:save( cPathTmplt )
   LOCAL cTxt, aGen :={}, aTxt := {}

   AAdd( aGen, "cProjPath        = " + ::cProjPath         )
   AAdd( aGen, "cProjName        = " + ::cProjName         )
   AAdd( aGen, "cProjType        = " + ::cProjType         )
   AAdd( aGen, "cOutName         = " + ::cOutName          )
   AAdd( aGen, "cOutPath         = " + ::cOutPath          )
   AAdd( aGen, "cWorkPath        = " + ::cWorkPath         )
   AAdd( aGen, "cLaunchExe       = " + ::cLaunchExe        )
   AAdd( aGen, "cLaunchParams    = " + ::cLaunchParams     )
   AAdd( aGen, "cStayIn          = " + ::cStayIn           )
   AAdd( aGen, "lXhb             = " + iif( ::lXhb     , "YES", "NO" ) )
   AAdd( aGen, "lXbase           = " + iif( ::lXbase   , "YES", "NO" ) )
   AAdd( aGen, "lHbQt            = " + iif( ::lHbQt    , "YES", "NO" ) )
   AAdd( aGen, "lXbp             = " + iif( ::lXbp     , "YES", "NO" ) )
   AAdd( aGen, "lFwh             = " + iif( ::lFwh     , "YES", "NO" ) )
   AAdd( aGen, "lHmg             = " + iif( ::lHmg     , "YES", "NO" ) )
   AAdd( aGen, "lOther           = " + iif( ::lOther   , "YES", "NO" ) )
   AAdd( aGen, "lA               = " + iif( ::lA       , "YES", "NO" ) )
   AAdd( aGen, "lB               = " + iif( ::lB       , "YES", "NO" ) )
   AAdd( aGen, "lES              = " + iif( ::lES      , "YES", "NO" ) )
   AAdd( aGen, "lG               = " + iif( ::lG       , "YES", "NO" ) )
   AAdd( aGen, "lJ               = " + iif( ::lJ       , "YES", "NO" ) )
   AAdd( aGen, "lL               = " + iif( ::lL       , "YES", "NO" ) )
   AAdd( aGen, "lM               = " + iif( ::lM       , "YES", "NO" ) )
   AAdd( aGen, "lN               = " + iif( ::lN       , "YES", "NO" ) )
   AAdd( aGen, "lV               = " + iif( ::lV       , "YES", "NO" ) )
   AAdd( aGen, "lW               = " + iif( ::lW       , "YES", "NO" ) )
   AAdd( aGen, "lZ               = " + iif( ::lZ       , "YES", "NO" ) )
   AAdd( aGen, "lQ               = " + iif( ::lQ       , "YES", "NO" ) )
   AAdd( aGen, "lBuild           = " + iif( ::lBuild   , "YES", "NO" ) )
   AAdd( aGen, "lCredits         = " + iif( ::lCredits , "YES", "NO" ) )
   AAdd( aGen, "cES              = " + ::cES  )
   AAdd( aGen, "cG               = " + ::cG   )
   AAdd( aGen, "cM               = " + ::cM   )
   AAdd( aGen, "cW               = " + ::cW   )
   AAdd( aGen, "cQ               = " + ::cQ   )
   AAdd( aGen, "lInc             = " + iif( ::lInc        , "YES", "NO" ) )
   AAdd( aGen, "lGui             = " + iif( ::lGui        , "YES", "NO" ) )
   AAdd( aGen, "lMt              = " + iif( ::lMt         , "YES", "NO" ) )
   AAdd( aGen, "lShared          = " + iif( ::lShared     , "YES", "NO" ) )
   AAdd( aGen, "lFullStatic      = " + iif( ::lFullStatic , "YES", "NO" ) )
   AAdd( aGen, "lTrace           = " + iif( ::lTrace      , "YES", "NO" ) )
   AAdd( aGen, "lInfo            = " + iif( ::lInfo       , "YES", "NO" ) )
   AAdd( aGen, "cGT              = " + ::cGT  )
   AAdd( aGen, "lGtGui           = " + iif( ::lGtGui      , "YES", "NO" ) )
   AAdd( aGen, "lGtWin           = " + iif( ::lGtWin      , "YES", "NO" ) )
   AAdd( aGen, "lGtWvt           = " + iif( ::lGtWvt      , "YES", "NO" ) )
   AAdd( aGen, "lGtWvg           = " + iif( ::lGtWvg      , "YES", "NO" ) )
   AAdd( aGen, "lGtXwc           = " + iif( ::lGtXwc      , "YES", "NO" ) )
   AAdd( aGen, "lGtCgi           = " + iif( ::lGtCgi      , "YES", "NO" ) )
   AAdd( aGen, "lGtTrm           = " + iif( ::lGtTrm      , "YES", "NO" ) )
   AAdd( aGen, "lGtStd           = " + iif( ::lGtStd      , "YES", "NO" ) )
   AAdd( aGen, "lGtSln           = " + iif( ::lGtSln      , "YES", "NO" ) )
   AAdd( aGen, "lGtPca           = " + iif( ::lGtPca      , "YES", "NO" ) )
   AAdd( aGen, "lGtOs2           = " + iif( ::lGtOs2      , "YES", "NO" ) )
   AAdd( aGen, "lGtCrs           = " + iif( ::lGtCrs      , "YES", "NO" ) )

   AAdd( aTxt, "<HbIDE Project Template>" )
   AAdd( aTxt, "   <Version:1.0>" )
   //
   ::addSection( aTxt, "GENERAL"            , aGen            )
   ::addSection( aTxt, "EXTRAS"             , ::aExtras       )
   ::addSection( aTxt, "HBCS"               , ::aPrpHbcs      )
   ::addSection( aTxt, "LIBS"               , ::aPrpLibs      )
   ::addSection( aTxt, "LIBPATHS"           , ::aPrpLPaths    )
   ::addSection( aTxt, "INCLUDEPATHS"       , ::aPrpIPaths    )
   ::addSection( aTxt, "DEFINES"            , ::aPrpDefines   )
   ::addSection( aTxt, "UNDEFINES"          , ::aPrpUnDefines )
   ::addSection( aTxt, "HBMK2CMDLINEPARAMS" , ::aPrpHbmk2     )
   ::addSection( aTxt, "BATCHCOMMANDS"      , ::aPrpBatch     )
   ::addSection( aTxt, "ACTIONSAFTERBUILD"  , ::aPrpAActions  )
   ::addSection( aTxt, "SOURCESPRG"         , ::aSrcPrgs      )
   ::addSection( aTxt, "SOURCESC"           , ::aSrcCs        )
   ::addSection( aTxt, "SOURCESCPP"         , ::aSrcCpps      )
   ::addSection( aTxt, "SOURCESCH"          , ::aSrcChs       )
   ::addSection( aTxt, "SOURCESH"           , ::aSrcHs        )
   ::addSection( aTxt, "SOURCESUI"          , ::aSrcUIs       )
   ::addSection( aTxt, "SOURCESOTHER"       , ::aSrcOthers    )
   //
   AAdd( aTxt, "" )
   AAdd( aTxt, "   </Version:1.0>" )
   AAdd( aTxt, "</HbIDE Project Template>" )

   cTxt := ""
   aeval( aTxt, {|e| cTxt += e + hb_eol() } )

   RETURN hb_memowrit( cPathTmplt, cTxt )

/*----------------------------------------------------------------------*/

METHOD IdeExProject:sectionToArray( cBuffer, cSection )
   LOCAL cTxt, n, nn, cTknB, cTknE
   LOCAL a_:={}

   cTknB := "<" + cSection + ">"
   cTknE := "</" + cSection + ">"

   IF ( n := at( cTknB, cBuffer ) ) > 0
      IF( nn := at( cTknE, cBuffer ) ) > 0
         cTxt := SubStr( cBuffer, n + Len( cTknB ), nn - 1 - ( n + Len( cTknB ) ) )
      ENDIF
      IF ! Empty( cTxt )
         a_:= hb_ATokens( cTxt, Chr( 10 ) )
      ENDIF
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeExProject:getKeyValuePair( cStr )
   LOCAL n

   IF ( n := at( "=", cStr ) ) > 0
      RETURN { alltrim( substr( cStr, 1, n - 1 ) ), alltrim( substr( cStr, n + 1 ) ) }
   ENDIF

   RETURN {}

/*----------------------------------------------------------------------*/

METHOD IdeExProject:retrieveSection( cBuffer, cSection, aPost )
   LOCAL s

   FOR EACH s IN ::sectiontoArray( cBuffer, cSection )
      AAdd( aPost, alltrim( s ) )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeExProject:load( cPathTmplt )
   LOCAL cBuffer := hb_memoRead( cPathTmplt )
   LOCAL cValid := "<HbIDE Project Template>"
   LOCAL s, a_

   IF ! ( left( cBuffer,  len( cValid ) ) == cValid )
      RETURN Self
   ENDIF

   IF !( hb_eol() == Chr( 10 ) )
      cBuffer := StrTran( cBuffer, hb_eol(), Chr( 10 ) )
   ENDIF
   IF !( hb_eol() == Chr( 13 ) + Chr( 10 ) )
      cBuffer := StrTran( cBuffer, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   ENDIF

   ::defaults()  /* Clear variables */

   FOR EACH s IN ::sectiontoArray( cBuffer, "GENERAL" )
      IF ! empty( a_:= ::getKeyValuePair( s ) )
         SWITCH a_[ 1 ]
         CASE "cProjPath"        ; ::cProjPath     := a_[ 2 ]          ; EXIT
         CASE "cProjName"        ; ::cProjName     := a_[ 2 ]          ; EXIT
         CASE "cProjType"        ; ::cProjType     := a_[ 2 ]          ; EXIT
         CASE "cOutName"         ; ::cOutName      := a_[ 2 ]          ; EXIT
         CASE "cOutPath"         ; ::cOutPath      := a_[ 2 ]          ; EXIT
         CASE "cWorkPath"        ; ::cWorkPath     := a_[ 2 ]          ; EXIT
         CASE "cLaunchExe"       ; ::cLaunchExe    := a_[ 2 ]          ; EXIT
         CASE "cLaunchParams"    ; ::cLaunchParams := a_[ 2 ]          ; EXIT
         CASE "cStayIn"          ; ::cStayIn       := a_[ 2 ]          ; EXIT
         CASE "lXhb"             ; ::lXhb          := a_[ 2 ] == "YES" ; EXIT
         CASE "lXbase"           ; ::lXbase        := a_[ 2 ] == "YES" ; EXIT
         CASE "lHbQt"            ; ::lHbQt         := a_[ 2 ] == "YES" ; EXIT
         CASE "lXbp"             ; ::lXbp          := a_[ 2 ] == "YES" ; EXIT
         CASE "lFwh"             ; ::lFwh          := a_[ 2 ] == "YES" ; EXIT
         CASE "lHmg"             ; ::lHmg          := a_[ 2 ] == "YES" ; EXIT
         CASE "lOther"           ; ::lOther        := a_[ 2 ] == "YES" ; EXIT
         CASE "lA"               ; ::lA            := a_[ 2 ] == "YES" ; EXIT
         CASE "lB"               ; ::lB            := a_[ 2 ] == "YES" ; EXIT
         CASE "lES"              ; ::lES           := a_[ 2 ] == "YES" ; EXIT
         CASE "lG"               ; ::lG            := a_[ 2 ] == "YES" ; EXIT
         CASE "lJ"               ; ::lJ            := a_[ 2 ] == "YES" ; EXIT
         CASE "lL"               ; ::lL            := a_[ 2 ] == "YES" ; EXIT
         CASE "lM"               ; ::lM            := a_[ 2 ] == "YES" ; EXIT
         CASE "lN"               ; ::lN            := a_[ 2 ] == "YES" ; EXIT
         CASE "lV"               ; ::lV            := a_[ 2 ] == "YES" ; EXIT
         CASE "lW"               ; ::lW            := a_[ 2 ] == "YES" ; EXIT
         CASE "lZ"               ; ::lZ            := a_[ 2 ] == "YES" ; EXIT
         CASE "lQ"               ; ::lQ            := a_[ 2 ] == "YES" ; EXIT
         CASE "lBuild"           ; ::lBuild        := a_[ 2 ] == "YES" ; EXIT
         CASE "lCredits"         ; ::lCredits      := a_[ 2 ] == "YES" ; EXIT
         CASE "cES"              ; ::cES           := a_[ 2 ]          ; EXIT
         CASE "cG"               ; ::cG            := a_[ 2 ]          ; EXIT
         CASE "cM"               ; ::cM            := a_[ 2 ]          ; EXIT
         CASE "cW"               ; ::cW            := a_[ 2 ]          ; EXIT
         CASE "cQ"               ; ::cQ            := a_[ 2 ]          ; EXIT
         CASE "lInc"             ; ::lInc          := a_[ 2 ] == "YES" ; EXIT
         CASE "lGui"             ; ::lGui          := a_[ 2 ] == "YES" ; EXIT
         CASE "lMt"              ; ::lMt           := a_[ 2 ] == "YES" ; EXIT
         CASE "lShared"          ; ::lShared       := a_[ 2 ] == "YES" ; EXIT
         CASE "lFullStatic"      ; ::lFullStatic   := a_[ 2 ] == "YES" ; EXIT
         CASE "lTrace"           ; ::lTrace        := a_[ 2 ] == "YES" ; EXIT
         CASE "lInfo"            ; ::lInfo         := a_[ 2 ] == "YES" ; EXIT
         CASE "cGT"              ; ::cGT           := a_[ 2 ]          ; EXIT
         CASE "lGtGui"           ; ::lGtGui        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtWin"           ; ::lGtWin        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtWvt"           ; ::lGtWvt        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtWvg"           ; ::lGtWvg        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtXwc"           ; ::lGtXwc        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtCgi"           ; ::lGtCgi        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtTrm"           ; ::lGtTrm        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtStd"           ; ::lGtStd        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtSln"           ; ::lGtSln        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtPca"           ; ::lGtPca        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtOs2"           ; ::lGtOs2        := a_[ 2 ] == "YES" ; EXIT
         CASE "lGtCrs"           ; ::lGtCrs        := a_[ 2 ] == "YES" ; EXIT
         ENDSWITCH
      ENDIF
   NEXT

   ::retrieveSection( cBuffer, "EXTRAS"             , ::aExtras       )
   ::retrieveSection( cBuffer, "HBCS"               , ::aPrpHbcs      )
   ::retrieveSection( cBuffer, "LIBS"               , ::aPrpLibs      )
   ::retrieveSection( cBuffer, "LIBPATHS"           , ::aPrpLPaths    )
   ::retrieveSection( cBuffer, "INCLUDEPATHS"       , ::aPrpIPaths    )
   ::retrieveSection( cBuffer, "DEFINES"            , ::aPrpDefines   )
   ::retrieveSection( cBuffer, "UNDEFINES"          , ::aPrpUnDefines )
   ::retrieveSection( cBuffer, "HBMK2CMDLINEPARAMS" , ::aPrpHbmk2     )
   ::retrieveSection( cBuffer, "BATCHCOMMANDS"      , ::aPrpBatch     )
   ::retrieveSection( cBuffer, "ACTIONSAFTERBUILD"  , ::aPrpAActions  )
   ::retrieveSection( cBuffer, "SOURCESPRG"         , ::aSrcPrgs      )
   ::retrieveSection( cBuffer, "SOURCESC"           , ::aSrcCs        )
   ::retrieveSection( cBuffer, "SOURCESCPP"         , ::aSrcCpps      )
   ::retrieveSection( cBuffer, "SOURCESCH"          , ::aSrcChs       )
   ::retrieveSection( cBuffer, "SOURCESH"           , ::aSrcHs        )
   ::retrieveSection( cBuffer, "SOURCESUI"          , ::aSrcUIs       )
   ::retrieveSection( cBuffer, "SOURCESOTHER"       , ::aSrcOthers    )

   RETURN Self

/*----------------------------------------------------------------------*/
