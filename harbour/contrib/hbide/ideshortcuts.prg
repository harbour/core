/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                              Harbour IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               04Apr2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqt.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

#define listMethods_itemDoubleClicked             101
#define listMethods_currentRowChanged             102
#define tableMacros_itemSelectionChanged          103
#define tableMacros_itemDoubleClicked             104
#define buttonSet_clicked                         105
#define buttonNew_clicked                         106

/*----------------------------------------------------------------------*/

CLASS IdeShortcuts INHERIT IdeObject

   DATA   aHdr                                    INIT {}
   DATA   aKeys                                   INIT {}
   DATA   aMethods                                INIT {}
   DATA   aMtdItms                                INIT {}
   DATA   aDftSCuts                               INIT {}
   DATA   aDftSCutsItms                           INIT {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( nMode, p )
   METHOD buildUI()
   METHOD buildSignals()

   METHOD evalMacro( cString )
   METHOD fetchAndExecMacro()

   METHOD getWord( lSelect )
   METHOD getLine( lSelect )
   METHOD getText()
   METHOD execTool( ... )

   METHOD loadDftSCuts()
   METHOD loadMethods()
   METHOD loadKeys()

   METHOD populateData( nMode )
   METHOD populateDftSCuts()
   METHOD populateKeys()
   METHOD populateMethods()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   ::loadMethods()
   ::loadKeys()
   ::loadDftSCuts()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:destroy()
   LOCAL a_, qItm

   IF !empty( ::oUI )
      FOR EACH qItm IN ::aHdr
         qItm := NIL
      NEXT
      ::aHdr := {}

      FOR EACH qItm IN ::aMtdItms
         qItm := NIL
      NEXT
      ::aMtdItms := {}

      FOR EACH a_ IN ::aDftSCutsItms
         FOR EACH qItm IN a_
            qItm := NIL
         NEXT
      NEXT
      ::aDftSCutsItms := {}

      ::oUI:destroy()
   ENDIF


   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:show()

   IF empty( ::oUI )
      ::buildUI()
      ::populateData( 1 )
   ENDIF

   ::oUI:show()
   ::oUI:raise()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:execEvent( nMode, p )
   LOCAL nRow, nKey, cKey

   SWITCH nMode
   CASE buttonNew_clicked

      EXIT

   CASE buttonSet_clicked
      nRow := ::oUI:q_tableMacros:currentRow()
      IF nRow >= 0 .AND. nRow < len( ::aDftSCuts )
         nRow++
         ::aDftSCuts[ nRow, 1 ] := ::oUI:q_editName:text()
         ::aDftSCuts[ nRow, 2 ] := ::aKeys[ ::oUI:q_comboKey:currentIndex() + 1, 2 ]
         ::aDftSCuts[ nRow, 3 ] := iif( ::oUI:q_checkAlt  :isChecked(), "YES", "NO" )
         ::aDftSCuts[ nRow, 4 ] := iif( ::oUI:q_checkCtrl :isChecked(), "YES", "NO" )
         ::aDftSCuts[ nRow, 5 ] := iif( ::oUI:q_checkShift:isChecked(), "YES", "NO" )
         ::aDftSCuts[ nRow, 6 ] := ::oUI:q_editMenu:text()
         ::aDftSCuts[ nRow, 7 ] := ::oUI:q_plainBlock:toPlainText()

         ::aDftSCutsItms[ nRow, 1 ]:setText( ::aDftSCuts[ nRow, 1 ] )
         ::aDftSCutsItms[ nRow, 2 ]:setText( ::aDftSCuts[ nRow, 2 ] )
         ::aDftSCutsItms[ nRow, 3 ]:setIcon( hbide_image( iif( ::aDftSCuts[ nRow, 3 ] == "YES", "check", "" ) ) )
         ::aDftSCutsItms[ nRow, 4 ]:setIcon( hbide_image( iif( ::aDftSCuts[ nRow, 4 ] == "YES", "check", "" ) ) )
         ::aDftSCutsItms[ nRow, 5 ]:setIcon( hbide_image( iif( ::aDftSCuts[ nRow, 5 ] == "YES", "check", "" ) ) )

      ENDIF
      EXIT
   CASE tableMacros_itemDoubleClicked

      EXIT
   CASE tableMacros_itemSelectionChanged
      nRow := ::oUI:q_tableMacros:currentRow()
      IF nRow >= 0 .AND. nRow < len( ::aDftSCuts )
         nRow++

         ::oUI:q_editName:setText( ::aDftSCuts[ nRow, 1 ] )

         cKey := ::aDftSCuts[ nRow, 2 ]
         IF ( nKey := ascan( ::aKeys, {|e_| e_[ 2 ] == cKey } ) ) > 0
            ::oUI:q_comboKey:setCurrentIndex( nKey - 1 )
         ENDIF

         ::oUI:q_checkAlt  :setChecked( ::aDftSCuts[ nRow, 3 ] == "YES" )
         ::oUI:q_checkCtrl :setChecked( ::aDftSCuts[ nRow, 4 ] == "YES" )
         ::oUI:q_checkShift:setChecked( ::aDftSCuts[ nRow, 5 ] == "YES" )

         ::oUI:q_editMenu:setText( ::aDftSCuts[ nRow, 6 ] )

         ::oUI:q_plainBlock:setPlainText( ::aDftSCuts[ nRow, 7 ] )
      ENDIF
      EXIT
   CASE listMethods_itemDoubleClicked
      EXIT
   CASE listMethods_currentRowChanged
      IF p >= 0 .AND. p < len( ::aMethods )
         ::oUI:q_texteditSyntax:setPlainText( ::aMethods[ p+1, 2 ] )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:buildUI()
   LOCAL oTbl, n, qItm
   LOCAL hdr_:= { { "Name", 190 }, { "Key", 50 }, { "Alt", 30 }, { "Ctrl", 30 }, { "Shift", 30 } }

   ::oUI := HbQtUI():new( hbide_uic( "shortcuts" ) ):build()
   ::oUI:setWindowIcon( hbide_image( "hbide" ) )

   oTbl := ::oUI:q_tableMacros                              /* Build Table Header */
   QHeaderView():from( oTbl:verticalHeader() ):hide()
   QHeaderView():from( oTbl:horizontalHeader() ):stretchLastSection( .t. )
   oTbl:setAlternatingRowColors( .t. )
   oTbl:setColumnCount( len( hdr_ ) )
   oTbl:setShowGrid( .f. )
   oTbl:setSelectionMode( QAbstractItemView_SingleSelection )
   oTbl:setSelectionBehavior( QAbstractItemView_SelectRows )
   FOR n := 1 TO len( hdr_ )
      qItm := QTableWidgetItem():new()
      qItm:setText( hdr_[ n,1 ] )
      oTbl:setHorizontalHeaderItem( n-1, qItm )
      oTbl:setColumnWidth( n-1, hdr_[ n,2 ] )
      aadd( ::aHdr, qItm )
   NEXT

   ::oUI:q_listMethods:setAlternatingRowColors( .t. )       /* Public Methods List */

   ::buildSignals()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:buildSignals()

   ::oUI:signal( "buttonNew"  , "clicked()"                   , {| | ::execEvent( buttonNew_clicked ) } )
   ::oUI:signal( "buttonSet"  , "clicked()"                   , {| | ::execEvent( buttonSet_clicked ) } )

   ::oUI:signal( "listMethods", "itemDoubleClicked(QLWItem)"  , {|p| ::execEvent( listMethods_itemDoubleClicked, p ) } )
   ::oUI:signal( "listMethods", "currentRowChanged(int)"      , {|p| ::execEvent( listMethods_currentRowChanged, p ) } )

   ::oUI:signal( "tableMacros", "itemSelectionChanged()"      , {| | ::execEvent( tableMacros_itemSelectionChanged ) } )
   ::oUI:signal( "tableMacros", "itemDoubleClicked(QTblWItem)", {|p| ::execEvent( tableMacros_itemDoubleClicked, p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:populateData( nMode )

   IF nMode == 1
      ::populateMethods()
      ::populateKeys()
      ::populateDftSCuts()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:populateDftSCuts()
   LOCAL a_, q1,q2,q3,q4,q5, n
   LOCAL oTbl := ::oUI:q_tableMacros

   oTbl:setRowCount( len( ::aDftSCuts ) )

   n := 0
   FOR EACH a_ IN ::aDftSCuts
      q1 := QTableWidgetItem():new()
      q1:setText( a_[ 1 ] )
      oTbl:setItem( n, 0, q1 )

      q2 := QTableWidgetItem():new()
      q2:setText( a_[ 2 ] )
      oTbl:setItem( n, 1, q2 )

      q3 := QTableWidgetItem():new()
      q3:setIcon( iif( a_[ 3 ] == "YES", hbide_image( "check" ), "" ) )
      oTbl:setItem( n, 2, q3 )

      q4 := QTableWidgetItem():new()
      q4:setIcon( iif( a_[ 4 ] == "YES", hbide_image( "check" ), "" ) )
      oTbl:setItem( n, 3, q4 )

      q5 := QTableWidgetItem():new()
      q5:setIcon( iif( a_[ 5 ] == "YES", hbide_image( "check" ), "" ) )
      oTbl:setItem( n, 4, q5 )

      oTbl:setRowHeight( n, 16 )
      QApplication():processEvents()
      aadd( ::aDftSCutsItms, { q1, q2, q3, q4, q5 } )
      n++
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:populateMethods()
   LOCAL qItem, a_
   LOCAL qLW := ::oUI:q_listMethods

   //qLW:setSortingEnabled( .t. )

   FOR EACH a_ IN ::aMethods
      IF !empty( a_[ 1 ] )
         qItem := QListWidgetItem():new()
         qItem:setText( a_[ 1 ] )
         aadd( ::aMtdItms, qItem )
         qLW:addItem_1( qItem )
      ENDIF
   NEXT
   qLW:setCurrentRow( 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:populateKeys()
   LOCAL a_
   LOCAL oCB := ::oUI:q_comboKey

   FOR EACH a_ IN ::aKeys
      oCB:addItem( a_[ 2 ] )
   NEXT
   oCB:setCurrentIndex( -1 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:loadDftSCuts()

   /*                    Name              Key   Alt   Ctrl   Sh  Menu Expr Icon */
   //
   aadd( ::aDftSCuts, { "Duplicate Line" , "D" , "NO", "YES", "NO", "", "", "" } )
   aadd( ::aDftSCuts, { "Current Line Up", "Up", "NO", "YES", "NO", "", "", "" } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:loadKeys()
   LOCAL a_

   aadd( ::aKeys, { Qt_Key_Escape        , "Escape          " } )
   aadd( ::aKeys, { Qt_Key_Tab           , "Tab             " } )
   aadd( ::aKeys, { Qt_Key_Backtab       , "Backtab         " } )
   aadd( ::aKeys, { Qt_Key_Backspace     , "Backspace       " } )
   aadd( ::aKeys, { Qt_Key_Return        , "Return          " } )
   aadd( ::aKeys, { Qt_Key_Enter         , "Enter           " } )
   aadd( ::aKeys, { Qt_Key_Insert        , "Insert          " } )
   aadd( ::aKeys, { Qt_Key_Delete        , "Delete          " } )
   aadd( ::aKeys, { Qt_Key_Pause         , "Pause           " } )
   aadd( ::aKeys, { Qt_Key_Print         , "Print           " } )
   aadd( ::aKeys, { Qt_Key_SysReq        , "SysReq          " } )
   aadd( ::aKeys, { Qt_Key_Clear         , "Clear           " } )
   aadd( ::aKeys, { Qt_Key_Home          , "Home            " } )
   aadd( ::aKeys, { Qt_Key_End           , "End             " } )
   aadd( ::aKeys, { Qt_Key_Left          , "Left            " } )
   aadd( ::aKeys, { Qt_Key_Up            , "Up              " } )
   aadd( ::aKeys, { Qt_Key_Right         , "Right           " } )
   aadd( ::aKeys, { Qt_Key_Down          , "Down            " } )
   aadd( ::aKeys, { Qt_Key_PageUp        , "PageUp          " } )
   aadd( ::aKeys, { Qt_Key_PageDown      , "PageDown        " } )
   aadd( ::aKeys, { Qt_Key_CapsLock      , "CapsLock        " } )
   aadd( ::aKeys, { Qt_Key_NumLock       , "NumLock         " } )
   aadd( ::aKeys, { Qt_Key_ScrollLock    , "ScrollLock      " } )
   aadd( ::aKeys, { Qt_Key_F1            , "F1              " } )
   aadd( ::aKeys, { Qt_Key_F2            , "F2              " } )
   aadd( ::aKeys, { Qt_Key_F3            , "F3              " } )
   aadd( ::aKeys, { Qt_Key_F4            , "F4              " } )
   aadd( ::aKeys, { Qt_Key_F5            , "F5              " } )
   aadd( ::aKeys, { Qt_Key_F6            , "F6              " } )
   aadd( ::aKeys, { Qt_Key_F7            , "F7              " } )
   aadd( ::aKeys, { Qt_Key_F8            , "F8              " } )
   aadd( ::aKeys, { Qt_Key_F9            , "F9              " } )
   aadd( ::aKeys, { Qt_Key_F10           , "F10             " } )
   aadd( ::aKeys, { Qt_Key_F11           , "F11             " } )
   aadd( ::aKeys, { Qt_Key_F12           , "F12             " } )
   aadd( ::aKeys, { Qt_Key_F13           , "F13             " } )
   aadd( ::aKeys, { Qt_Key_F14           , "F14             " } )
   aadd( ::aKeys, { Qt_Key_F15           , "F15             " } )
   aadd( ::aKeys, { Qt_Key_F16           , "F16             " } )
   aadd( ::aKeys, { Qt_Key_F17           , "F17             " } )
   aadd( ::aKeys, { Qt_Key_F18           , "F18             " } )
   aadd( ::aKeys, { Qt_Key_F19           , "F19             " } )
   aadd( ::aKeys, { Qt_Key_F20           , "F20             " } )
   aadd( ::aKeys, { Qt_Key_F21           , "F21             " } )
   aadd( ::aKeys, { Qt_Key_F22           , "F22             " } )
   aadd( ::aKeys, { Qt_Key_F23           , "F23             " } )
   aadd( ::aKeys, { Qt_Key_F24           , "F24             " } )
   aadd( ::aKeys, { Qt_Key_F25           , "F25             " } )
   aadd( ::aKeys, { Qt_Key_F26           , "F26             " } )
   aadd( ::aKeys, { Qt_Key_F27           , "F27             " } )
   aadd( ::aKeys, { Qt_Key_F28           , "F28             " } )
   aadd( ::aKeys, { Qt_Key_F29           , "F29             " } )
   aadd( ::aKeys, { Qt_Key_F30           , "F30             " } )
   aadd( ::aKeys, { Qt_Key_F31           , "F31             " } )
   aadd( ::aKeys, { Qt_Key_F32           , "F32             " } )
   aadd( ::aKeys, { Qt_Key_F33           , "F33             " } )
   aadd( ::aKeys, { Qt_Key_F34           , "F34             " } )
   aadd( ::aKeys, { Qt_Key_F35           , "F35             " } )
   aadd( ::aKeys, { Qt_Key_Space         , "Space           " } )
   aadd( ::aKeys, { Qt_Key_Exclam        , "Exclam          " } )
   aadd( ::aKeys, { Qt_Key_QuoteDbl      , "QuoteDbl        " } )
   aadd( ::aKeys, { Qt_Key_NumberSign    , "NumberSign      " } )
   aadd( ::aKeys, { Qt_Key_Dollar        , "Dollar          " } )
   aadd( ::aKeys, { Qt_Key_Percent       , "Percent         " } )
   aadd( ::aKeys, { Qt_Key_Ampersand     , "Ampersand       " } )
   aadd( ::aKeys, { Qt_Key_Apostrophe    , "Apostrophe      " } )
   aadd( ::aKeys, { Qt_Key_ParenLeft     , "ParenLeft       " } )
   aadd( ::aKeys, { Qt_Key_ParenRight    , "ParenRight      " } )
   aadd( ::aKeys, { Qt_Key_Asterisk      , "Asterisk        " } )
   aadd( ::aKeys, { Qt_Key_Plus          , "Plus            " } )
   aadd( ::aKeys, { Qt_Key_Comma         , "Comma           " } )
   aadd( ::aKeys, { Qt_Key_Minus         , "Minus           " } )
   aadd( ::aKeys, { Qt_Key_Period        , "Period          " } )
   aadd( ::aKeys, { Qt_Key_Slash         , "Slash           " } )
   aadd( ::aKeys, { Qt_Key_0             , "0               " } )
   aadd( ::aKeys, { Qt_Key_1             , "1               " } )
   aadd( ::aKeys, { Qt_Key_2             , "2               " } )
   aadd( ::aKeys, { Qt_Key_3             , "3               " } )
   aadd( ::aKeys, { Qt_Key_4             , "4               " } )
   aadd( ::aKeys, { Qt_Key_5             , "5               " } )
   aadd( ::aKeys, { Qt_Key_6             , "6               " } )
   aadd( ::aKeys, { Qt_Key_7             , "7               " } )
   aadd( ::aKeys, { Qt_Key_8             , "8               " } )
   aadd( ::aKeys, { Qt_Key_9             , "9               " } )
   aadd( ::aKeys, { Qt_Key_Colon         , "Colon           " } )
   aadd( ::aKeys, { Qt_Key_Semicolon     , "Semicolon       " } )
   aadd( ::aKeys, { Qt_Key_Less          , "Less            " } )
   aadd( ::aKeys, { Qt_Key_Equal         , "Equal           " } )
   aadd( ::aKeys, { Qt_Key_Greater       , "Greater         " } )
   aadd( ::aKeys, { Qt_Key_Question      , "Question        " } )
   aadd( ::aKeys, { Qt_Key_At            , "At              " } )
   aadd( ::aKeys, { Qt_Key_A             , "A               " } )
   aadd( ::aKeys, { Qt_Key_B             , "B               " } )
   aadd( ::aKeys, { Qt_Key_C             , "C               " } )
   aadd( ::aKeys, { Qt_Key_D             , "D               " } )
   aadd( ::aKeys, { Qt_Key_E             , "E               " } )
   aadd( ::aKeys, { Qt_Key_F             , "F               " } )
   aadd( ::aKeys, { Qt_Key_G             , "G               " } )
   aadd( ::aKeys, { Qt_Key_H             , "H               " } )
   aadd( ::aKeys, { Qt_Key_I             , "I               " } )
   aadd( ::aKeys, { Qt_Key_J             , "J               " } )
   aadd( ::aKeys, { Qt_Key_K             , "K               " } )
   aadd( ::aKeys, { Qt_Key_L             , "L               " } )
   aadd( ::aKeys, { Qt_Key_M             , "M               " } )
   aadd( ::aKeys, { Qt_Key_N             , "N               " } )
   aadd( ::aKeys, { Qt_Key_O             , "O               " } )
   aadd( ::aKeys, { Qt_Key_P             , "P               " } )
   aadd( ::aKeys, { Qt_Key_Q             , "Q               " } )
   aadd( ::aKeys, { Qt_Key_R             , "R               " } )
   aadd( ::aKeys, { Qt_Key_S             , "S               " } )
   aadd( ::aKeys, { Qt_Key_T             , "T               " } )
   aadd( ::aKeys, { Qt_Key_U             , "U               " } )
   aadd( ::aKeys, { Qt_Key_V             , "V               " } )
   aadd( ::aKeys, { Qt_Key_W             , "W               " } )
   aadd( ::aKeys, { Qt_Key_X             , "X               " } )
   aadd( ::aKeys, { Qt_Key_Y             , "Y               " } )
   aadd( ::aKeys, { Qt_Key_Z             , "Z               " } )
   aadd( ::aKeys, { Qt_Key_BracketLeft   , "BracketLeft     " } )
   aadd( ::aKeys, { Qt_Key_Backslash     , "Backslash       " } )
   aadd( ::aKeys, { Qt_Key_BracketRight  , "BracketRight    " } )
   aadd( ::aKeys, { Qt_Key_AsciiCircum   , "AsciiCircum     " } )
   aadd( ::aKeys, { Qt_Key_Underscore    , "Underscore      " } )
   aadd( ::aKeys, { Qt_Key_QuoteLeft     , "QuoteLeft       " } )
   aadd( ::aKeys, { Qt_Key_BraceLeft     , "BraceLeft       " } )
   aadd( ::aKeys, { Qt_Key_Bar           , "Bar             " } )
   aadd( ::aKeys, { Qt_Key_BraceRight    , "BraceRight      " } )
   aadd( ::aKeys, { Qt_Key_AsciiTilde    , "AsciiTilde      " } )

   FOR EACH a_ IN ::aKeys
      a_[ 2 ] := trim( a_[ 2 ] )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:evalMacro( cString )
   LOCAL bError := ErrorBlock( {|o| break( o ) } )
   LOCAL oErr, bBlock, n, cBlock, cParam

   IF ( n := at( "|", cString ) ) > 0
      cString := substr( cString, n + 1 )
      IF ( n := at( "|", cString ) ) == 0
         RETURN Self
      ENDIF
      cParam  := substr( cString, 1, n - 1 )
      cString := substr( cString, n + 1 )
      cBlock  := "{|o," + cParam + "|" + cString + " }"
   ELSE
      cBlock := "{|o| " + cString + " }"
   ENDIF
   cBlock := strtran( cBlock, "::", "o:" )

   bBlock := &( cBlock )

hbide_dbg( cBlock )
   BEGIN SEQUENCE
      eval( bBlock, self )
   RECOVER USING oErr
      MsgBox( "Wrongly defined block. Syntax is |var| method_call( var ) --- " + oErr:description )
   END SEQUENCE

   ErrorBlock( bError )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:fetchAndExecMacro()
   LOCAL cStr

   cStr := hbide_fetchAString( ::oDlg:oWidget, "", "Macro", "Compilation" )
   IF !empty( cStr )
      ::evalMacro( cStr )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:loadMethods()

   aadd( ::aMethods, { "getWord( lSelect )", "Returns text of the word under cursor. If <lSelect == .T.> text appears as selected." } )
   aadd( ::aMethods, { "getLine( lSelect )", "Returns text of the current line. If <lSelect == .T.> text appears as selected." } )
   aadd( ::aMethods, { "getText()"         , "Returns current selected text." } )
   aadd( ::aMethods, { "execTool( ... )"   , "Executes a 'Tool' under tools menu or without an entry." } )
   #if 0
   aadd( ::aMethods, { "", "" } )
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:execTool( ... )
   RETURN ::oTM:execTool( ... )

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:getWord( lSelect )
   RETURN ::oEM:getWord( lSelect )

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:getLine( lSelect )
   RETURN ::oEM:getLine( lSelect )

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:getText()
   RETURN ::oEM:getText()

/*----------------------------------------------------------------------*/

