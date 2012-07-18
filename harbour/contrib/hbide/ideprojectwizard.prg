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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
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
//
//                          Class IdeProjectWizard
//
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
   METHOD  execEvent( xEvent, p, p1 )
   METHOD  loadDefaults()
   METHOD  saveProject()
   METHOD  clear()
   METHOD  loadSourcesSections()
   METHOD  loadSwichesSections()
   METHOD  deleteTreeItem( oChild )
   METHOD  addTreeItem( oParent )

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

      ::oUI:btnSwPlus   : connect( "clicked()", {|| ::execEvent( "btnSw_clicked", "plus"  ) } )
      ::oUI:btnSwAZ     : connect( "clicked()", {|| ::execEvent( "btnSw_clicked", "az"    ) } )
      ::oUI:btnSwZA     : connect( "clicked()", {|| ::execEvent( "btnSw_clicked", "za"    ) } )
      ::oUI:btnSwUpper  : connect( "clicked()", {|| ::execEvent( "btnSw_clicked", "upper" ) } )
      ::oUI:btnSwLower  : connect( "clicked()", {|| ::execEvent( "btnSw_clicked", "lower" ) } )
      ::oUI:btnSwDelete : connect( "clicked()", {|| ::execEvent( "btnSw_clicked", "delete") } )

      ::oUI:btnNext     : connect( "clicked()", {|| ::execEvent( "btnNext_clicked" ) } )
      ::oUI:btnBack     : connect( "clicked()", {|| ::execEvent( "btnBack_clicked" ) } )
      ::oUI:btnNew      : connect( "clicked()", {|| ::execEvent( "btnNew_clicked"  ) } )
      ::oUI:btnSave     : connect( "clicked()", {|| ::execEvent( "btnSave_clicked" ) } )
      ::oUI:btnCancel   : connect( "clicked()", {|| ::oUI:oWidget:done( 0 ) } )

      ::oUI:toolGetSrc  : connect( "clicked()", {|| ::execEvent( "toolGetSrc_clicked" ) } )

      ::oUI:comboProjType : addItem( "Executable" )
      ::oUI:comboProjType : addItem( "Library" )
      ::oUI:comboProjType : addItem( "DLL - Without HVM" )
      ::oUI:comboProjType : addItem( "DLL - With HVM" )

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
      ::oUI:treeProps:connect( "itemCollapsed(QTreeWidgetItem*)"        , {|p   | ::execEvent( "treeProps_itemCollapsed"       , p     ) } )
      ::oUI:treeProps:connect( "itemExpanded(QTreeWidgetItem*)"         , {|p   | ::execEvent( "treeProps_itemExpanded"        , p     ) } )
      ::oUI:treeProps:connect( "customContextMenuRequested(QPoint)"     , {|p   | ::execEvent( "treeProps_contextMenuRequested", p     ) } )
      ::oUI:treeProps:connect( "itemDoubleClicked(QTreeWidgetItem*,int)", {|p,p1| ::execEvent( "treeProps_doubleClicked"       , p, p1 ) } )
      ::oUI:treeProps:connect( "itemSelectionChanged()"                 , {|    | ::execEvent( "treeProps_itemSelectionChanged"        ) } )

      oBrush := QBrush( QColor( 248, 248, 248 ) )

      aadd( ::aItmProps, { NIL, "Libraries"                , QBrush( QColor( 144, 144, 144 ) ),  oBrush, NIL, NIL, "background-color: rgb(144,144,144);" } )
      aadd( ::aItmProps, { NIL, "Library Paths"            , QBrush( QColor( 152, 152, 152 ) ),  oBrush, NIL, NIL, "background-color: rgb(152,152,152);" } )
      aadd( ::aItmProps, { NIL, "Include Paths"            , QBrush( QColor( 160, 160, 160 ) ),  oBrush, NIL, NIL, "background-color: rgb(160,160,160);" } )
      aadd( ::aItmProps, { NIL, "PRG Defines"              , QBrush( QColor( 168, 168, 168 ) ),  oBrush, NIL, NIL, "background-color: rgb(168,168,168);" } )
      aadd( ::aItmProps, { NIL, "PRG Undefines"            , QBrush( QColor( 176, 176, 176 ) ),  oBrush, NIL, NIL, "background-color: rgb(176,176,176);" } )
      aadd( ::aItmProps, { NIL, "hbmk2 Command-line params", QBrush( QColor( 184, 184, 184 ) ),  oBrush, NIL, NIL, "background-color: rgb(184,184,184);" } )


      ::oUI:treeSrc:setContextMenuPolicy( Qt_CustomContextMenu )
      ::oUI:treeSrc:setDragEnabled( .t. )
      ::oUI:treeSrc:setDropIndicatorShown( .t. )
      ::oUI:treeSrc:setAcceptDrops( .t. )
      ::oUI:treeSrc:setDragDropMode( QAbstractItemView_InternalMove )
      ::oUI:treeSrc:setRootIsDecorated( .F. ) /* Important to present as a list */
      ::oUI:treeSrc:header():resizeSection( 0, 393 )
      ::oUI:treeSrc:connect( "itemCollapsed(QTreeWidgetItem*)"        , {|p   | ::execEvent( "treeSrc_itemCollapsed"       , p     ) } )
      ::oUI:treeSrc:connect( "itemExpanded(QTreeWidgetItem*)"         , {|p   | ::execEvent( "treeSrc_itemExpanded"        , p     ) } )
      ::oUI:treeSrc:connect( "customContextMenuRequested(QPoint)"     , {|p   | ::execEvent( "treeSrc_contextMenuRequested", p     ) } )
      ::oUI:treeSrc:connect( "itemDoubleClicked(QTreeWidgetItem*,int)", {|p,p1| ::execEvent( "treeSrc_doubleClicked"       , p, p1 ) } )

      aadd( ::aItmSrc, { NIL, "PRG Files"      , QBrush( QColor( 184, 184, 184 ) ), oBrush, ".prg", NIL, "background-color: rgb(184,184,184);" } )
      aadd( ::aItmSrc, { NIL, "C Files"        , QBrush( QColor( 176, 176, 176 ) ), oBrush, ".c"  , NIL, "background-color: rgb(176,176,176);" } )
      aadd( ::aItmSrc, { NIL, "CPP Files"      , QBrush( QColor( 168, 168, 168 ) ), oBrush, ".cpp", NIL, "background-color: rgb(168,168,168);" } )
      aadd( ::aItmSrc, { NIL, "CH Files"       , QBrush( QColor( 160, 160, 160 ) ), oBrush, ".ch" , NIL, "background-color: rgb(160,160,160);" } )
      aadd( ::aItmSrc, { NIL, "H Files"        , QBrush( QColor( 152, 152, 152 ) ), oBrush, ".h"  , NIL, "background-color: rgb(152,152,152);" } )
      aadd( ::aItmSrc, { NIL, "UI Files"       , QBrush( QColor( 144, 144, 144 ) ), oBrush, ".ui" , NIL, "background-color: rgb(144,144,144);" } )
      aadd( ::aItmSrc, { NIL, "All Other Files", QBrush( QColor( 136, 136, 136 ) ), oBrush, "*"   , NIL, "background-color: rgb(136,136,136);" } )


      ::clear()
   ENDIF

   IF ::loadDefaults()
      ::oUI:exec()
      ::oUI:oWidget:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION blockBtnClicked( o, cEvent, nAct )
   RETURN {|| o:execEvent( cEvent, nAct ) }

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:loadSwichesSections()
   LOCAL oTree := ::oUI:treeProps
   LOCAL qItm, aAct, oFont, qTBtn

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
   NEXT
   FOR EACH aAct IN ::aItmProps
      qTBtn := QToolButton()
      aAct[ 6 ] := qTBtn
      qTBtn:setIcon( QIcon( hbide_image( "expand_m" ) ) )
      qTBtn:setAutoFillBackground( .t. )
      qTBtn:setAutoRaise( .t. )
      qTBtn:setMaximumWidth( 20 )
      qTBtn:setMaximumHeight( 20 )
      qTBtn:setStyleSheet( "" )
      qTBtn:setStyleSheet( aAct[ 7 ] )
      qTBtn:connect( "clicked()", blockBtnClicked( Self, "qTBtn_clicked", aAct:__enumIndex() ) )
      oTree:setItemWidget( aAct[ 1 ], 1, qTBtn )
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:loadSourcesSections()
   LOCAL oTree := ::oUI:treeSrc
   LOCAL qItm, aAct, oFont, qTBtn

   oFont := QTreeWidgetItem():font( 0 )
   oFont:setBold( .t. )
   FOR EACH aAct IN ::aItmSrc
      qItm := QTreeWidgetItem()
      aAct[ 1 ] := qItm
      qItm:setFlags( 0 )
      qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDropEnabled, Qt_ItemIsEnabled ) )
      qItm:setText( 0, "        " + aAct[ 2 ] )
      qItm:setBackground( 0, aAct[ 3 ] )
      qItm:setForeground( 0, QBrush( QColor( 255,255,255 ) ) )
      qItm:setFont( 0, oFont )
      qItm:setTooltip( 0, "Drag and drop a source OR select with open icon at the top !" )
      oTree:addTopLevelItem( qItm )
      oTree:setFirstItemColumnSpanned( qItm, .t. )
      qItm:setChildIndicatorPolicy( QTreeWidgetItem_ShowIndicator )
   NEXT
   FOR EACH aAct IN ::aItmSrc
      qTBtn := QToolButton()
      aAct[ 6 ] := qTBtn
      qTBtn:setIcon( QIcon( hbide_image( "expand_m" ) ) )
      qTBtn:setAutoFillBackground( .t. )
      qTBtn:setAutoRaise( .t. )
      qTBtn:setMaximumWidth( 20 )
      qTBtn:setMaximumHeight( 20 )
      qTBtn:setStyleSheet( "" )
      qTBtn:setStyleSheet( aAct[ 7 ] )
      qTBtn:connect( "clicked()", blockBtnClicked( Self, "qSBtn_clicked", aAct:__enumIndex() ) )
      oTree:setItemWidget( aAct[ 1 ], 1, qTBtn )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:execEvent( xEvent, p, p1 )
   LOCAL cText, qItm, n, i, oFont, aMenu, oParent, aFiles, aFilt, cFile, cExt, lTop, nChildren, qChild

   HB_SYMBOL_UNUSED( p )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH xEvent

   CASE "treeProps_itemSelectionChanged"
      IF ! empty( qItm := ::oUI:treeProps:currentItem() )
         lTop := ::oUI:treeProps:indexOfTopLevelItem( qItm ) >= 0
         ::oUI:btnSwPlus:setEnabled( lTop )
         ::oUI:btnSwAZ:setEnabled( lTop )
         ::oUI:btnSwZA:setEnabled( lTop )
      ENDIF
      EXIT
   CASE "treeProps_itemCollapsed"
   CASE "treeProps_itemExpanded"
      IF ( n := ::oUI:treeProps:indexOfTopLevelItem( p ) ) >= 0
         n++
         IF hb_isObject( ::aItmProps[ n, 6 ] )
            ::aItmProps[ n, 6 ]:setIcon( QIcon( hbide_image( iif( xEvent == "treeProps_itemExpanded", "collapse_m", "expand_m" ) ) ) )
         ENDIF
         p:setSelected( .t. )
      ENDIF
      EXIT
   CASE "treeSrc_itemExpanded"
   CASE "treeSrc_itemCollapsed"
      IF ( n := ::oUI:treeSrc:indexOfTopLevelItem( p ) ) >= 0
         IF hb_isObject( ::aItmSrc[ n+1,6 ] )
            ::aItmSrc[ n+1,6 ]:setIcon( QIcon( hbide_image( iif( xEvent == "treeSrc_itemCollapsed", "expand_m", "collapse_m" ) ) ) )
         ENDIF
         p:setSelected( .t. )
      ENDIF
      EXIT
   CASE "qTBtn_clicked"
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
   CASE "qSBtn_clicked"
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
   CASE "toolGetSrc_clicked"
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
            hb_fNameSplit( cFile, , , @cExt )
            IF ( n := ascan( ::aItmSrc, {|e_| e_[ 5 ] == lower( cExt ) } ) ) == 0
               n := len( ::aItmSrc )
            ENDIF
            oParent := ::aItmSrc[ n,1 ]
            ::oUI:treeSrc:expandItem( oParent )
            qItm := QTreeWidgetItem()
            oParent:addChild( qItm )
            qItm:setFlags( 0 )
            qItm:setFlags( hb_bitOr( Qt_ItemIsSelectable, Qt_ItemIsDragEnabled, Qt_ItemIsEnabled ) )
            qItm:setText( 0, cFile )
            qItm:setFont( 0, oFont )
            qItm:setBackground( 0, ::aItmSrc[ 1, 4 ] )
         NEXT
      ENDIF
      EXIT
   CASE "treeSrc_contextMenuRequested"
      IF ! empty( qItm := ::oUI:treeSrc:itemAt( p ) )
         cText := substr( qItm:text( 0 ), 8 )
         aMenu := {}
         IF ( n := ascan( ::aItmSrc, {|e_| e_[ 2 ] == cText } ) ) == 0
            aadd( aMenu, { ::oAC:getAction( "Delete" ) } )
         ELSE
            aadd( aMenu, { ::oAC:getAction( "SortAZ" ) } )
            aadd( aMenu, { ::oAC:getAction( "SortZA" ) } )
         ENDIF
         cText := hbide_execPopup( aMenu, ::oUI:treeSrc:mapToGlobal( p ), ::oUI:treeSrc )
         IF cText == "Delete"
            ::deleteTreeItem( qItm )
         ELSEIF cText == "Sort Ascending"
            ::aItmSrc[ n,1 ]:sortChildren( 0, Qt_AscendingOrder )
         ELSEIF cText == "Sort Descending"
            ::aItmSrc[ n,1 ]:sortChildren( 0, Qt_DescendingOrder )
         ENDIF
      ENDIF
      EXIT
   CASE "treeProps_contextMenuRequested"
      IF ! empty( qItm := ::oUI:treeProps:itemAt( p ) )
         cText := substr( qItm:text( 0 ), 8 )
         aMenu := {}
         IF ( n := ascan( ::aItmProps, {|e_| e_[ 2 ] == cText } ) ) == 0
            aadd( aMenu, { ::oAC:getAction( "Delete" ) } )
         ELSE
            aadd( aMenu, { ::oAC:getAction( "SortAZ" ) } )
            aadd( aMenu, { ::oAC:getAction( "SortZA" ) } )
         ENDIF
         cText := hbide_execPopup( aMenu, ::oUI:treeProps:mapToGlobal( p ), ::oUI:treeProps )
         IF cText == "Delete"
            ::deleteTreeItem( qItm )
         ELSEIF cText == "Sort Ascending"
            ::aItmProps[ n,1 ]:sortChildren( 0, Qt_AscendingOrder )
         ELSEIF cText == "Sort Descending"
            ::aItmProps[ n,1 ]:sortChildren( 0, Qt_DescendingOrder )
         ENDIF
      ENDIF
      EXIT
   CASE "treeSrc_doubleClicked"
      EXIT
   CASE "btnSw_clicked"
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
   CASE "treeProps_doubleClicked"
      IF ::oUI:treeProps:indexOfTopLevelItem( p ) >= 0
         ::addTreeItem( p )
      ENDIF
      EXIT
   CASE "btnNew_clicked"
      IF ::lEdited .AND. hbide_getYesNo( "Create new without saving current ?", "Current project has not been saved !", "Please Confirm", ::oUI:oWidget )
         ::clear()
         IF ! ::loadDefaults()
            ::oUI:oWidget:done( 0 )
         ENDIF
      ENDIF
      EXIT
   CASE "btnSave_clicked"
      ::saveProject()
      EXIT
   CASE "btnNext_clicked"
      IF ::oUI:stackedWidget:currentIndex() == 0
         ::oUI:stackedWidget:setcurrentIndex( 1 )
      ENDIF
      EXIT
   CASE "btnBack_clicked"
      IF ::oUI:stackedWidget:currentIndex() == 1
         ::oUI:stackedWidget:setcurrentIndex( 0 )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

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
   qItm:setBackground( 0, QBrush( QColor( 245,245,245 ) ) )
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
