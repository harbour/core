/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                               20Feb2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define buttonInstall_clicked                     1
#define editInstall_textChanged                   2
#define buttonRefresh_clicked                     3
#define treeDoc_doubleClicked                     4

/*----------------------------------------------------------------------*/

#define DOC_FUN_BEGINS                           -5
#define DOC_FUN_ENDS                             -1
#define DOC_FUN_NONE                              0
#define DOC_FUN_TEMPLATE                          1
#define DOC_FUN_FUNCNAME                          2
#define DOC_FUN_CATEGORY                          3
#define DOC_FUN_SUBCATEGORY                       4
#define DOC_FUN_ONELINER                          5
#define DOC_FUN_SYNTAX                            6
#define DOC_FUN_ARGUMENTS                         7
#define DOC_FUN_RETURNS                           8
#define DOC_FUN_DESCRIPTION                       9
#define DOC_FUN_EXAMPLES                          10
#define DOC_FUN_TESTS                             11
#define DOC_FUN_FILES                             12
#define DOC_FUN_STATUS                            13
#define DOC_FUN_PLATFORMS                         14
#define DOC_FUN_SEEALSO                           15

/*----------------------------------------------------------------------*/

CLASS IdeHarbourHelp INHERIT IdeObject

   DATA   oUI
   DATA   cPathInstall
   DATA   cDocPrefix
   DATA   aNodes                                  INIT {}
   DATA   aFunctions                              INIT {}

   DATA   nCurTVItem

   DATA   SetColorBG                              INIT "#ffffc0"
   DATA   SetColorTable                           INIT "#ffff80"
   DATA   SetColorText                            INIT "#0000ff"

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()

   METHOD execEvent( nMode, p, p1 )

   METHOD setImages()
   METHOD setTooltips()
   METHOD setParameters()

   METHOD installSignals()
   METHOD refreshDocTree()
   METHOD populateFuncDetails( n )
   METHOD buildView( oFunc )
   METHOD parseTextFile( cTextFile, oParent )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   ::oUI := HbQtUI():new( ::resPath + "docviewgenerator.uic", ::oDlg:oWidget ):build()

   ::setImages()
   ::setTooltips()
   ::installSignals()
   ::setParameters()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:destroy()
   #if 0
   LOCAL a_
   FOR EACH a_ IN ::aNodes
      IF a_[ 2 ] == "Function"
         a_[ 1 ] := NIL
      ENDIF
   NEXT
   FOR EACH a_ IN ::aNodes
      IF a_[ 2 ] == "File"
         a_[ 1 ] := NIL
      ENDIF
   NEXT
   FOR EACH a_ IN ::aNodes
      IF a_[ 2 ] == "Path"
         a_[ 1 ] := NIL
      ENDIF
   NEXT
   ::aNodes[ 1, 1 ] := NIL

   ::aNodes := NIL
   #endif

   //::disconnect( ::oUI:q_treeDoc, "itemDoubleClicked(QTWItem)" )

   ::oUI:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:setImages()
   LOCAL oUI := ::oUI

   oUI:q_buttonHome:setIcon( ::resPath + "dc_home.png" )
   oUI:q_buttonBackward:setIcon( ::resPath + "dc_left.png" )
   oUI:q_buttonForward:setIcon( ::resPath + "dc_right.png" )
   oUI:q_buttonUp:setIcon( ::resPath + "dc_up.png" )
   oUI:q_buttonRefresh:setIcon( ::resPath + "dc_refresh.png" )
   oUI:q_buttonPrint:setIcon( ::resPath + "dc_print.png" )

   oUI:q_buttonSave:setIcon( ::resPath + "save.png" )
   oUI:q_buttonExit:setIcon( ::resPath + "dc_quit.png" )

   oUI:q_buttonInstall:setIcon( ::resPath + "dc_folder.png" )

   oUI:q_buttonArgPlus:setIcon( ::resPath + "dc_plus.png" )
   oUI:q_buttonArgMinus:setIcon( ::resPath + "dc_delete.png" )
   oUI:q_buttonArgUp:setIcon( ::resPath + "dc_up.png" )
   oUI:q_buttonArgDown:setIcon( ::resPath + "dc_down.png" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:setTooltips()
   LOCAL oUI := ::oUI

   oUI:q_buttonHome:setToolTip( "Home" )
   oUI:q_buttonBackward:setToolTip( "Backward" )
   oUI:q_buttonForward:setToolTip( "Forward" )
   oUI:q_buttonRefresh:setToolTip( "Refresh" )
   oUI:q_buttonUp:setToolTip( "Up" )
   oUI:q_buttonPrint:setToolTip( "Print" )

   oUI:q_buttonSave:setToolTip( "Save" )
   oUI:q_buttonExit:setToolTip( "Exit" )

   oUI:q_buttonInstall:setToolTip( "Select Harbour Installation Path" )

   oUI:q_buttonArgPlus:setToolTip( "Add new argument" )
   oUI:q_buttonArgMinus:setToolTip( "Delete argument" )
   oUI:q_buttonArgUp:setToolTip( "Up one position" )
   oUI:q_buttonArgDown:setToolTip( "Down one position" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:setParameters()
   LOCAL oUI := ::oUI

   oUI:q_treeDoc:setHeaderHidden( .t. )
   oUI:q_treeCategory:setHeaderHidden( .t. )
   oUI:q_editInstall:setText( ::cWrkHarbour )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:installSignals()

   ::oUI:signal( "buttonInstall", "clicked()"                 , {|    | ::execEvent( buttonInstall_clicked )      } )
   ::oUI:signal( "buttonRefresh", "clicked()"                 , {|    | ::execEvent( buttonRefresh_clicked )      } )
   ::oUI:signal( "editInstall"  , "textChanged(QString)"      , {|p   | ::execEvent( editInstall_textChanged, p ) } )


   ::connect( ::oUI:q_treeDoc   , "itemDoubleClicked(QTWItem)", {|p,p1| ::execEvent( treeDoc_doubleClicked, p, p1 ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:execEvent( nMode, p, p1 )
   LOCAL cPath, qTWItem, cText, n

   SWITCH nMode

   CASE buttonInstall_clicked
      cPath := hbide_fetchADir( ::oDocViewDock, "Harbour Install Root" )
      IF !empty( cPath )
         ::oUI:q_editInstall:setText( cPath )
      ENDIF
      EXIT

   CASE editInstall_textChanged
      IF hb_dirExists( p )
         ::oUI:q_editInstall:setStyleSheet( "" )
         ::cPathInstall := hbide_pathStripLastSlash( hbide_pathNormalized( p, .f. ) )
         ::oIde:cWrkHarbour := ::cPathInstall
      ELSE
         ::oUI:q_editInstall:setStyleSheet( getStyleSheet( "PathIsWrong" ) )
      ENDIF
      EXIT

   CASE buttonRefresh_clicked
      ::refreshDocTree()
      EXIT

   CASE treeDoc_doubleClicked
      qTWItem := QTreeWidgetItem():from( p )
      cText := qTWItem:text( p1 )

      IF ( n := ascan( ::aNodes, {|e_| e_[ 5 ] == cText } ) ) > 0
         ::nCurTVItem := n

         IF ::aNodes[ n, 2 ] == "File"
            ::parseTextFile( ::aNodes[ n, 4 ], ::aNodes[ n, 1 ] )
         ELSEIF ::aNodes[ n, 2 ] == "Function"
            ::populateFuncDetails( n )
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:refreshDocTree()
   LOCAL aPaths, cFolder, cNFolder, aDocs, oTWItem, oParent, oParentF, cIcon
   LOCAL aDir, a_, cTextFile, cStripped

   IF empty( ::cPathInstall ) .OR. ! hb_dirExists( ::cPathInstall )
      RETURN Self
   ENDIF

   #if 0 /* To be reviewed later */
   IF empty( ::oUI:q_editDocPrefix )
      ::cDocPrefix := "doc/en"
   ELSE
      ::cDocPrefix := ::oUI:q_editDocPrefix:text()
   ENDIF
   #endif

   cIcon := ::resPath + "dc_folder.png"

   aPaths := {} ; aDocs := {}
   hbide_fetchSubPaths( @aPaths, ::cPathInstall, .t. )

   FOR EACH cFolder IN aPaths
      cNFolder := hbide_pathNormalized( cFolder, .t. )
      IF ( "/doc" $ cNFolder ) .OR. ( "/doc/en" $ cNFolder )
         aadd( aDocs, cFolder )
      ENDIF
   NEXT

   oTWItem := QTreeWidgetItem():new()
   oTWItem:setText( 0, ::cPathInstall )
   oTWItem:setIcon( 0, ::resPath + "dc_home.png" )
   oTWItem:setToolTip( 0, ::cPathInstall )
   oTWItem:setExpanded( .t. )

   ::oUI:q_treeDoc:addTopLevelItem( oTWItem )
   aadd( ::aNodes, { oTWItem, "Root", NIL /*oParent*/, ::cPathInstall, ::cPathInstall } )
   oParent := oTWItem

   FOR EACH cFolder IN aDocs
      oTWItem := QTreeWidgetItem():new()
      oTWItem:setText( 0, ( cStripped := hbide_stripRoot( ::cPathInstall, cFolder ) ) )
      oTWItem:setIcon( 0, cIcon )
      oTWItem:setToolTip( 0, cFolder )
      oParent:addChild( oTWItem )
      aadd( ::aNodes, { oTWItem, "Folder", oParent, cFolder, cStripped } )

      oParentF := oTWItem
      aDir := directory( cFolder + "*.txt" )
      FOR EACH a_ IN aDir
         IF a_[ 5 ] != "D"
            cTextFile := cFolder + a_[ 1 ]
            oTWItem := QTreeWidgetItem():new()
            oTWItem:setText( 0, a_[ 1 ]  )
            oTWItem:setIcon( 0, ::resPath + "dc_textdoc.png" )
            oTWItem:setToolTip( 0, cTextFile )
            oParentF:addChild( oTWItem )
            aadd( ::aNodes, { oTWItem, "File", oParentF, cTextFile, a_[ 1 ] } )
         ENDIF
      NEXT
   NEXT

   ::oUI:q_treeDoc:expandItem( oParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:parseTextFile( cTextFile, oParent )
   LOCAL a_, s, nPart, oFunc
   LOCAL lIsFunc := .f.
   LOCAL nIndex  := 0
   LOCAL oTWItem := 0
   LOCAL aInfo   := {}
   LOCAL cIcon   := ::resPath + "dc_function.png"

   nPart := DOC_FUN_NONE

   a_:= hbide_readSource( cTextFile )

   FOR EACH s IN a_
      DO CASE

      CASE "$DOC$"         $ s
         nIndex++
         lIsFunc := .t.
         nPart := DOC_FUN_BEGINS
         oFunc := IdeDocFunction():new()

      CASE "$END$"         $ s
         lIsFunc := .f.
         nPart := DOC_FUN_ENDS
         oTWItem := QTreeWidgetItem():new()
         oTWItem:setText( 0, oFunc:cName )
         oTWItem:setIcon( 0, cIcon )
         oTWItem:setTooltip( 0, oFunc:cName )
         oParent:addChild( oTWItem )
         aadd( ::aNodes, { oTWItem, "Function", oParent, cTextFile + "<::>" + oFunc:cName, oFunc:cName } )
         aadd( ::aFunctions, { cTextFile, oFunc:cName, oFunc, oTWItem } )

      CASE "$TEMPLATE$"    $ s
         nPart := DOC_FUN_TEMPLATE
      CASE "$FUNCNAME$"    $ s   .OR.  "$NAME$" $ s
         nPart := DOC_FUN_FUNCNAME
      CASE "$CATEGORY$"    $ s
         nPart := DOC_FUN_CATEGORY
      CASE "$SUBCATEGORY$" $ s
         nPart := DOC_FUN_SUBCATEGORY
      CASE "$ONELINER$"    $ s
         nPart := DOC_FUN_ONELINER
      CASE "$SYNTAX$"      $ s
         nPart := DOC_FUN_SYNTAX
      CASE "$ARGUMENTS$"   $ s
         nPart := DOC_FUN_ARGUMENTS
      CASE "$RETURNS$"     $ s
         nPart := DOC_FUN_RETURNS
      CASE "$DESCRIPTION$" $ s
         nPart := DOC_FUN_DESCRIPTION
      CASE "$EXAMPLES$"    $ s
         nPart := DOC_FUN_EXAMPLES
      CASE "$TESTS$"       $ s
         nPart := DOC_FUN_TESTS
      CASE "$FILES$"       $ s
         nPart := DOC_FUN_FILES
      CASE "$STATUS$"       $ s
         nPart := DOC_FUN_STATUS
      CASE "$PLATFORMS$"   $ s  .OR.  "$COMPLIANCE$" $ s
         nPart := DOC_FUN_PLATFORMS
      CASE "$SEEALSO$"     $ s
         nPart := DOC_FUN_SEEALSO
      OTHERWISE
         IF ! lIsFunc
            LOOP   // It is a fake line not within $DOC$ => $END$ block
         ENDIF
         s := hbide_stripDocPrefix( s )

         SWITCH nPart
         CASE DOC_FUN_BEGINS
            EXIT
         CASE DOC_FUN_TEMPLATE
            oFunc:cTemplate    := s
            EXIT
         CASE DOC_FUN_FUNCNAME
            oFunc:cName        := s
            EXIT
         CASE DOC_FUN_CATEGORY
            oFunc:cCategory    := s
            EXIT
         CASE DOC_FUN_SUBCATEGORY
            oFunc:cSubCategory := s
            EXIT
         CASE DOC_FUN_ONELINER
            oFunc:cOneLiner    := s
            EXIT
         CASE DOC_FUN_SYNTAX
            aadd( oFunc:aSyntax, s )
            EXIT
         CASE DOC_FUN_ARGUMENTS
            aadd( oFunc:aArguments, s )
            EXIT
         CASE DOC_FUN_RETURNS
            aadd( oFunc:aReturns, s )
            EXIT
         CASE DOC_FUN_DESCRIPTION
            aadd( oFunc:aDescription, s )
            EXIT
         CASE DOC_FUN_EXAMPLES
            aadd( oFunc:aExamples, s )
            EXIT
         CASE DOC_FUN_TESTS
            aadd( oFunc:aTests, s )
            EXIT
         CASE DOC_FUN_FILES
            aadd( oFunc:aFiles, s )
            EXIT
         CASE DOC_FUN_STATUS
            oFunc:cStatus := s
            EXIT
         CASE DOC_FUN_PLATFORMS
            oFunc:cPlatForms := s
            EXIT
         CASE DOC_FUN_SEEALSO
            oFunc:cSeaAlso := s
            EXIT
         OTHERWISE
            nPart := DOC_FUN_NONE
            oFunc := NIL
            EXIT
         ENDSWITCH
      ENDCASE
   NEXT

   hbide_justACall( nIndex, nPart, s, oTWItem, aInfo, oParent )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populateFuncDetails( n )
   LOCAL oTWItem := ::aNodes[ n, 1 ]
   LOCAL nIndex, oFunc

   nIndex := ascan( ::aFunctions, {|e_| e_[ 4 ] == oTWItem } )
   oFunc := ::aFunctions[ nIndex, 3 ]

   ::oUI:q_editTemplate    :setText( iif( empty( oFunc:cTemplate ), "FUNCTION", oFunc:cTemplate ) )
   ::oUI:q_editName        :setText( oFunc:cName        )
   ::oUI:q_editCategory    :setText( oFunc:cCategory    )
   ::oUI:q_editSubCategory :setText( oFunc:cSubCategory )
   ::oUI:q_editOneLiner    :setText( oFunc:cOneLiner    )
   ::oUI:q_editSeeAlso     :setText( oFunc:cSeaAlso     )
   ::oUI:q_editStatus      :setText( oFunc:cStatus      )
   ::oUI:q_editPlatforms   :setText( oFunc:cPlatforms   )

   ::oUI:q_editReturns     :setText( hbide_arrayToMemoEx( oFunc:aReturns ) )  //TODO : a line

   ::oUI:q_plainSyntax     :setPlainText( hbide_arrayToMemoEx2( oFunc:aSyntax      ) )
   ::oUI:q_plainFiles      :setPlainText( hbide_arrayToMemoEx2( oFunc:aFiles       ) )
   ::oUI:q_plainDescription:setPlainText( hbide_arrayToMemoEx2( oFunc:aDescription ) )
   ::oUI:q_plainExamples   :setPlainText( hbide_arrayToMemoEx2( oFunc:aExamples    ) )
   ::oUI:q_plainTests      :setPlainText( hbide_arrayToMemoEx2( oFunc:aTests       ) )
   ::oUI:q_plainArguments  :setPlainText( hbide_arrayToMemoEx2( oFunc:aArguments   ) )

   ::oUI:q_editTextPath    :setText( ::aFunctions[ nIndex, 1 ] )

   ::buildView( oFunc )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:buildView( oFunc )
   LOCAL s, x, y, v, w, z
   LOCAL oVw := ::oUI:q_browserView
   LOCAL aHtm := {}

   aadd( aHtm, "<HTML>" )

   aadd( aHtm, '<head>                                                             ' )
   aadd( aHtm, '  <meta name="Author" CONTENT=Pritpal Bedi [pritpal@vouchcac.com]">' )
   aadd( aHtm, '  <meta http-equiv="content-style-type" content="text/css" >       ' )
   aadd( aHtm, '  <meta http-equiv="content-script-type" content="text/javascript">' )
   aadd( aHtm, '                                                                   ' )
   aadd( aHtm, '  <style type="text/css">                                          ' )
   aadd( aHtm, '    th                                                             ' )
   aadd( aHtm, '    {                                                              ' )
   aadd( aHtm, '      colspan          : 1;                                        ' )
   aadd( aHtm, '      text-align       : center;                                   ' )
   aadd( aHtm, '      vertical-align   : baseline;                                 ' )
   aadd( aHtm, '      horizontal-align : left;                                     ' )
   aadd( aHtm, '    }                                                              ' )
   aadd( aHtm, '    td                                                             ' )
   aadd( aHtm, '    {                                                              ' )
   aadd( aHtm, '      vertical-align   : top;                                      ' )
   aadd( aHtm, '      horizontal-align : left;                                     ' )
   aadd( aHtm, '    }                                                              ' )
   aadd( aHtm, '    pre                                                            ' )
   aadd( aHtm, '    {                                                              ' )
   aadd( aHtm, '      font-family      : Courier New;                              ' )
   aadd( aHtm, '      font-size        : .9;                                       ' )
   aadd( aHtm, '      color            : black;                                    ' )
   aadd( aHtm, '      cursor           : text;                                     ' )
   aadd( aHtm, '    }                                                              ' )
   aadd( aHtm, '  </style>                                                         ' )
   aadd( aHtm, '</head>                                                            ' )

   aadd( aHtm, '<BODY>' )
   aadd( aHtm, '<CENTER>' )

   s := '<TABLE ' +;
        'Border='      + '0 '   +;
        'Frame='       + 'ALL ' +;
        'CellPadding=' + '0 '   +;
        'CellSpacing=' + '0 '   +;
        'Cols='        + '1 '   +;
        'Width='       + '95% ' +;
        '>'
   aadd( aHtm, s )

   aadd( aHtm, '<CAPTION align=TOP><FONT SIZE="6"><B>' + oFunc:cName + '</B></FONT></CAPTION>' )

   aadd( aHtm, "<TR><TD align=CENTER ><B>" + oFunc:cOneLiner + "</B></TD></TR>" )

//   x := '<TR><TD align=LEFT><font size="5" color="#008AE6">' ; y := "</font></TD></TR>"
   x := '<TR><TD align=LEFT><font size="5" color="#FF4719">' ; y := "</font></TD></TR>"
   v := '<TR><TD margin-left: 20px><pre>' ; w := "</pre></TD></TR>"
   z := "<TR><TD>&nbsp;</TD></TR>"

   aadd( aHtm, x + "Syntax"      + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aSyntax      ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Arguments"   + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aArguments   ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Returns"     + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aReturns     ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Description" + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aDescription ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Examples"    + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aExamples    ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Files"       + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aFiles       ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Platforms"   + y )
   aadd( aHtm, v + oFunc:cPlatforms + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Status"      + y )
   aadd( aHtm, v + oFunc:cStatus + w )
   aadd( aHtm, z )

   aadd( aHtm, "</TABLE>"  )
   aadd( aHtm, "</CENTER>" )
   aadd( aHtm, "</BODY>"   )
   aadd( aHtm, "</HTML>"   )

   oVw:setHTML( hbide_arrayToMemo( aHtm ) )

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_stripDocPrefix( s )
   RETURN  substr( s, 9 )

/*----------------------------------------------------------------------*/

CLASS IdeDocFunction

   DATA   cName                                   INIT ""
   DATA   cTemplate                               INIT ""
   DATA   cCategory                               INIT ""
   DATA   cSubCategory                            INIT ""
   DATA   cOneliner                               INIT ""
   DATA   aSyntax                                 INIT {}
   DATA   aArguments                              INIT {}
   DATA   aReturns                                INIT {}
   DATA   aDescription                            INIT {}
   DATA   aExamples                               INIT {}
   DATA   aTests                                  INIT {}
   DATA   aFiles                                  INIT {}
   DATA   cStatus                                 INIT ""
   DATA   cPlatforms                              INIT ""
   DATA   cSeaAlso                                INIT ""

   DATA   aSource                                 INIT {}

   DATA   oTVItem
   DATA   cSourceTxt                              INIT ""

   METHOD new()                                   INLINE Self

   ENDCLASS

/*----------------------------------------------------------------------*/

#if 0
/*  $DOC$
 *  $FUNCNAME$
 *      hb_BTreeNew()
 *  $CATEGORY$
 *      BTree API
 *  $ONELINER$
 *      Create a new BTree file
 *  $SYNTAX$
 *      C Prototype
 *
 *      #include "hb_btree.api"
 *      hb_BTreeNew( CHAR <cFileName>, int <nPageSize>, int <nKeySize>, [ ULONG <nFlags> ], [ USHORT <nBuffers>=1 ] ) -> ( struct hb_BTree * )pHBTree
 *
 *      Harbour Prototype
 *
 *      hb_BTreeNew( CHAR <cFileName>, <nPageSize>, <nKeySize>, [ <nFlags> ], [ <nBuffers>=1 ] ) -> ( int )hb_BTree_Handle
 *
 *      Harbour Class Prototype
 *
 *      TBTreeNew( CHAR <cFileName>, <nPageSize>, <nKeySize>, [ <nFlags> ], [ <nBuffers>=1 ] ) -> <tBTreeInstance>
 *  $ARGUMENTS$
 *      <cFileName> Name of BTree file to create.  This parameter is optional
 *      if the flag HB_BTREE_INMEMORY is used
 *
 *      <nPageSize> Number of bytes one file 'page' is to be; must be a multiple of 2048.
 *      If the hb_btree library is compiled with the value HB_BTREE_HEADERSIZE
 *      defined to another value, that is used in place of 2048
 *
 *      <nKeySize> Number of bytes a key value is to be; must be 8 bytes or greater
 *
 *      <nFlags> Flags that determine the file access mode(s) and BTree mode(s)
 *
 *      <nBuffers> Number of internal I/O buffers to use - not currently supported for shared/dynamic use
 *  $RETURNS$
 *      C Prototype
 *
 *      <pBTree> A pointer to an hb_BTree structure, to be used by other hb_BTree C API calls
 *
 *      Harbour Prototype
 *
 *      <hb_BTree_Handle> A handle, to be used by other hb_BTree Harbour API calls
 *
 *      Harbour Class Prototype
 *
 *      <tBTreeInstance> An instance of the TBTree class
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *  $FILES$
 *      Library is hb_btree</par>
 *      Header is hb_btree.ch</par>
 *      C Header is hb_btree.api</par>
 *  $PLATFORMS$
 *      All
 *  $SEEALSO$
 *      BTree Flags
 *  $END$
 */
#endif

/*----------------------------------------------------------------------*/
