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

#include "common.ch"
#include "button.ch"
#include "hbsetup.ch"

#ifdef HB_COMPAT_C53

//--------------------------------------------------------------------------//
function MenuItem( cCaption, boData, nShortcut, cMsg, nID )

   LOCAL oClass

   if ISBLOCK( boData ) .or. ISOBJECT( boData )
      boData := iif( cCaption != MENU_SEPARATOR, boData, nil )
   endif

   DEFAULT cCaption  TO ""
   DEFAULT boData    TO nil
   DEFAULT nShortcut TO 0
   DEFAULT cMsg      TO ""
   DEFAULT nID       TO 0

   oClass := TClass():New( "MENUITEM" )

   oClass:AddData( "caption"  ,  cCaption )
   oClass:AddData( "cargo" )
   oClass:AddData( "checked"  ,  FALSE )
   oClass:AddData( "column"   ,  0 )
   oClass:AddData( "data"     ,  boData )
   oClass:AddData( "enabled"  ,  iif( cCaption != MENU_SEPARATOR, TRUE, FALSE ) )
   oClass:AddData( "id"       ,  nID )
   oClass:AddData( "message"  ,  cMsg )
   oClass:AddData( "row"      ,  0 )
   oClass:AddData( "shortcut" ,  nShortcut )
   oClass:AddData( "style"    ,  HB_TMENUITEM_STYLE )

   oClass:AddMethod( "isPopup",  @isPopup() )

   oClass:Create()

return oClass:Instance()

//--------------------------------------------------------------------------//
static function isPopUp()

   LOCAL Self  := QSelf()

   if ISOBJECT( ::data ) .and. ::data:ClassName() == "POPUPMENU"
      return TRUE
   endif

return FALSE

//--------------------------------------------------------------------------//

#endif
