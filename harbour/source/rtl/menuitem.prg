/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MENUITEM class
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "common.ch"
#include "button.ch"
//--------------------------------------------------------------------------//
function MenuItem( cCaption, boData, nShortcut, cMsg, nID )

   LOCAL oClass

   if ValType( boData ) == "B" .or. ValType( boData ) == "O"
      boData := if( cCaption != MENU_SEPARATOR, boData, nil )
   endif

   DEFAULT  cCaption    TO "",   ;
            boData      TO nil,  ;
            nShortcut   TO 0,    ;
            cMsg        TO "",   ;
            nID         TO 0

   oClass := TClass():New( "MENUITEM" )

   oClass:AddData( "caption"  ,  cCaption )
   oClass:AddData( "cargo" )
   oClass:AddData( "checked"  ,  FALSE )
   oClass:AddData( "column"   ,  0 )
   oClass:AddData( "data"     ,  boData )
   oClass:AddData( "enabled"  ,  if( cCaption != MENU_SEPARATOR, TRUE, FALSE ) )
   oClass:AddData( "id"       ,  nID )
   oClass:AddData( "message"  ,  cMsg )
   oClass:AddData( "row"      ,  0 )
   oClass:AddData( "shortcut" ,  nShortcut )
   oClass:AddData( "style"    ,  MENU_STYLE )

   oClass:AddMethod( "isPopup",  @isPopup() )

   oClass:Create()

return oClass:Instance()
//--------------------------------------------------------------------------//
static function isPopUp()

   LOCAL Self  := QSelf()

   if ValType( ::data ) == "O" .and. ::data:ClassName() == "POPUPMENU"
      return TRUE
   endif

return FALSE
//--------------------------------------------------------------------------//
