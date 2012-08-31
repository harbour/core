/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                               31Aug2012
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

CLASS IdeParts INHERIT IdeObject

   DATA   oIde
   DATA   nCurStacksIndex                         INIT   IDE_PART_EDITOR

   DATA   oLayoutDA
   DATA   oLayoutEditor
   DATA   oLayoutDbu

   DATA   oStackWidget

   DATA   oStackEditor
   DATA   oStackDbu

   DATA   oSettings

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()                               VIRTUAL

   METHOD buildParts()
   METHOD buildLayout( nLayout )

   METHOD setStack( nIndex )                      INLINE ::oStackWidget:setCurrentIndex( nIndex )

   METHOD execStackIndexChanged( nIndex )
   METHOD addWidget( nPart, oWidget, nFromRow, nFromColumn, nRowSpan, nColumnSpan )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeParts:new( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeParts:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeParts:buildLayout( nLayout )
   LOCAL oLayout

   SWITCH nLayout
   CASE 0
      oLayout := QGridLayout()
      oLayout:setContentsMargins( 0,0,0,0 )
      oLayout:setHorizontalSpacing( 0 )
      oLayout:setVerticalSpacing( 0 )
      EXIT
   CASE 1  /* QHBoxLayout */
      EXIT
   CASE 2  /* QVBoxLayout */
      EXIT
   ENDSWITCH

   RETURN oLayout

/*----------------------------------------------------------------------*/

METHOD IdeParts:buildParts()

   ::oLayoutDA     := ::buildLayout( 0 )
   ::oLayoutDbu    := ::buildLayout( 0 )
   ::oLayoutEditor := ::buildLayout( 0 )

   ::oDa:setLayout( ::oLayoutDA )

   ::oStackWidget  := QStackedWidget( ::oDa:oWidget )
   //
   ::oStackEditor := QWidget( ::oStackWidget )
   ::oStackDbu    := QWidget( ::oStackWidget )
   //
   ::oStackWidget:addWidget( ::oStackEditor )
   ::oStackWidget:addWidget( ::oStackDbu    )

   ::oStackEditor:setLayout( ::oLayoutEditor )
   ::oStackDbu   :setLayout( ::oLayoutDbu    )

   ::oLayoutDA:addWidget( ::oStackWidget, 0, 0, 1, 1 )

   ::oStackWidget:setCurrentIndex( 0 )
   ::oStackWidget:connect( "currentChanged(int)", {|i| ::execStackIndexChanged( i ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeParts:execStackIndexChanged( nIndex )

   IF ! Empty( ::oSettings )
      ::oDlg:oWidget:restoreState( ::oSettings )
   ENDIF
   ::oSettings := ::oDlg:oWidget:saveState()

   SWITCH nIndex

   CASE IDE_PART_EDITOR
      ::oIde:oSBar:show()
      EXIT

   CASE IDE_PART_DBU
      ::oIde:oSBar:hide()
      ::oDK:hideAllDocks()
      ::oBM:showInIdeDBU()
      EXIT

   ENDSWITCH

   ::nCurStacksIndex := nIndex

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeParts:addWidget( nPart, oWidget, nFromRow, nFromColumn, nRowSpan, nColumnSpan )

   SWITCH nPart

   CASE IDE_PART_EDITOR
      ::oLayoutEditor:addWidget( oWidget, nFromRow, nFromColumn, nRowSpan, nColumnSpan )
      EXIT

   CASE IDE_PART_DBU
      ::oLayoutDbu:addWidget( oWidget, nFromRow, nFromColumn, nRowSpan, nColumnSpan )
      EXIT

   CASE IDE_PART_REPORTSDESIGNER
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

