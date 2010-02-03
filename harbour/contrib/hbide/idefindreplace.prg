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
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS IdeFindReplace INHERIT IdeObject

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD onClickReplace()
   METHOD replaceSelection( cReplWith )
   METHOD replace()
   METHOD onClickFind()
   METHOD find( lWarn )
   METHOD updateFindReplaceData( cMode )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:destroy()

   ::oUI:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   #ifdef HBIDE_USE_UIC
   ::oUI := HbQtUI():new( ::oIde:resPath + "finddialog.uic", ::oIde:oDlg:oWidget ):build()
   #else
   ::oUI := HbQtUI():new( ::oIde:resPath + "finddialog.ui", ::oIde:oDlg:oWidget ):create()
   #endif
   ::oUI:setWindowFlags( Qt_Sheet )

   aeval( ::oIde:aIni[ INI_FIND    ], {|e| ::oUI:q_comboFindWhat:addItem( e ) } )
   aeval( ::oIde:aIni[ INI_REPLACE ], {|e| ::oUI:q_comboReplaceWith:addItem( e ) } )

   ::oUI:q_radioFromCursor:setChecked( .t. )
   ::oUI:q_radioDown:setChecked( .t. )

   ::oUI:signal( "buttonFind"   , "clicked()", {|| ::onClickFind() } )
   ::oUI:signal( "buttonReplace", "clicked()", {|| ::onClickReplace() } )
   ::oUI:signal( "buttonClose"  , "clicked()", ;
         {|| ::oIde:aIni[ INI_HBIDE, FindDialogGeometry ] := hbide_posAndSize( ::oUI:oWidget ), ::oUI:hide() } )

   ::oUI:signal( "comboFindWhat", "currentIndexChanged(text)", ;
                               {|p| ::oIde:oSBar:getItem( SB_PNL_SEARCH ):caption := "FIND: " + p } )

   ::oUI:signal( "checkListOnly", "stateChanged(int)", {|p| ;
                                        ::oUI:q_comboReplaceWith:setEnabled( p == 0 ), ;
                                   iif( p == 1, ::oUI:q_buttonReplace:setEnabled( .f. ), NIL ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:show()
   LOCAL cText, qLineEdit

   ::oUI:q_buttonReplace:setEnabled( .f. )
   ::oUI:q_checkGlobal:setEnabled( .f. )
   ::oUI:q_checkNoPrompting:setEnabled( .f. )
   ::oUI:q_checkListOnly:setChecked( .f. )
   ::oIde:setPosByIni( ::oUI:oWidget, FindDialogGeometry )
   ::oUI:q_comboFindWhat:setFocus()

   qLineEdit := QLineEdit():configure( ::oUI:q_comboFindWhat:lineEdit() )
   IF !empty( cText := ::oEM:getSelectedText() )
      qLineEdit:setText( cText )
   ENDIF
   qLineEdit:selectAll()

   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:onClickReplace()

   ::updateFindReplaceData( "replace" )

   IF ::oUI:q_comboReplaceWith:isEnabled()
      ::replace()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:replaceSelection( cReplWith )
   LOCAL nB, nL, cBuffer, qCursor

   DEFAULT cReplWith TO ""

   qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
   IF qCursor:hasSelection() .and. !empty( cBuffer := qCursor:selectedText() )
      nL := len( cBuffer )
      nB := qCursor:position() - nL

      qCursor:beginEditBlock()
      qCursor:removeSelectedText()
      qCursor:insertText( cReplWith )
      qCursor:setPosition( nB )
      qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, len( cReplWith ) )
      ::qCurEdit:setTextCursor( qCursor )
      qCursor:endEditBlock()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:replace()
   LOCAL cReplWith
   LOCAL nFound

   IF !empty( ::qCurEdit )
      cReplWith := QLineEdit():configure( ::oUI:q_comboReplaceWith:lineEdit() ):text()
      ::replaceSelection( cReplWith )

      IF ::oUI:q_checkGlobal:isChecked()
         IF ::oUI:q_checkNoPrompting:isChecked()
            nFound := 1
            DO WHILE ::find( .f. )
               nFound++
               ::replaceSelection( cReplWith )
            ENDDO
            ::oSBar:getItem( SB_PNL_MAIN ):caption := '<font color="2343212"><b>Replaced [' + hb_ntos( nFound ) + "] : "+ cReplWith + "</b></font>"
            ::oUI:q_buttonReplace:setEnabled( .f. )
            ::oUI:q_checkGlobal:setChecked( .f. )
            ::oUI:q_checkNoPrompting:setChecked( .f. )
         ELSE
            ::find()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:onClickFind()
   LOCAL lFound, nPos, qCursor

   ::updateFindReplaceData( "find" )

   IF ::oUI:q_radioEntire:isChecked()
      ::oUI:q_radioFromCursor:setChecked( .t. )
      qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
      nPos := qCursor:position()

      qCursor:setPosition( 0 )
      ::qCurEdit:setTextCursor( qCursor )
      IF !( lFound := ::find() )
         qCursor:setPosition( nPos )
         ::qCurEdit:setTextCursor( qCursor )
      ENDIF
   ELSE
      lFound := ::find()
   ENDIF

   IF lFound
      ::oUI:q_buttonReplace:setEnabled( .t. )
      ::oUI:q_checkGlobal:setEnabled( .t. )
      ::oUI:q_checkNoPrompting:setEnabled( .t. )
   ELSE
      ::oUI:q_buttonReplace:setEnabled( .f. )
      ::oUI:q_checkGlobal:setEnabled( .f. )
      ::oUI:q_checkNoPrompting:setEnabled( .f. )

      ::oUI:q_buttonFind:setFocus_1()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:find( lWarn )
   LOCAL nFlags
   LOCAL cText := QLineEdit():configure( ::oUI:q_comboFindWhat:lineEdit() ):text()
   LOCAL lFound := .f.

   DEFAULT lWarn TO .t.

   IF !empty( cText )
      nFlags := 0
      nFlags += iif( ::oUI:q_checkMatchCase:isChecked(), QTextDocument_FindCaseSensitively, 0 )
      nFlags += iif( ::oUI:q_radioUp:isChecked(), QTextDocument_FindBackward, 0 )

      //IF !( lFound := ::qCurEdit:find( cText, nFlags ) ) .and. lWarn
      IF !( lFound := ::oEM:getEditCurrent():find( cText, nFlags ) ) .and. lWarn
         hbide_showWarning( "Cannot find : " + cText )
      ENDIF
   ENDIF

   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:updateFindReplaceData( cMode )
   LOCAL cData

   IF cMode == "find"
      cData := QLineEdit():configure( ::oUI:q_comboFindWhat:lineEdit() ):text()
      IF !empty( cData )
         IF ascan( ::oIde:aIni[ INI_FIND ], {|e| e == cData } ) == 0
            hb_ains( ::oIde:aIni[ INI_FIND ], 1, cData, .t. )
            ::oUI:q_comboFindWhat:insertItem( 0, cData )
         ENDIF
      ENDIF
      //
      ::oSBar:getItem( SB_PNL_SEARCH ):caption := "FIND: " + cData
   ELSE
      cData := QLineEdit():configure( ::oUI:q_comboReplaceWith:lineEdit() ):text()
      IF !empty( cData )
         IF ascan( ::oIde:aIni[ INI_REPLACE ], cData ) == 0
            hb_ains( ::oIde:aIni[ INI_REPLACE ], 1, cData, .t. )
            ::oUI:q_comboReplaceWith:insertItem( 0, cData )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                         Class IdeFindInFiles
//
/*----------------------------------------------------------------------*/

CLASS IdeFindInFiles INHERIT IdeObject

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   #ifdef HBIDE_USE_UIC
   ::oUI := HbQtUI():new( ::oIde:resPath + "findinfiles.uic", ::oIde:oDlg:oWidget ):build()
   #else
   ::oUI := HbQtUI():new( ::oIde:resPath + "findinfiles.ui", ::oIde:oDlg:oWidget ):create()
   #endif
   ::oUI:setWindowFlags( Qt_Sheet )
   ::oUI:exec()

   ::destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:destroy()

   ::oUI:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:show()

   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/


