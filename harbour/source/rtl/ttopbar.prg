/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TOPBAR menu class
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

#include "box.ch"
#include "button.ch"
#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbsetup.ch"

#ifdef HB_COMPAT_C53

#define HB_DEBUG_MENU_COLORS  "N/BG, W+/N, GR+/BG, GR+/N, N/BG, N/BG"

/* TOFIX: Harbour doesn't check if the colorSpec instance
          var has always six pairs of colors. It should
          do so and throw an error. [jlalin]
*/

/* NOTE: In the Get* methods we are breaking the "only one return" rule. I
         know this isn't a good practice however we are eliminating a variable,
         an exit statement and two assigments which is good for speed critical
         and small functions. [jlalin]
*/
//--------------------------------------------------------------------------//
function TopBar( nRow, nLeft, nRight )

   LOCAL oClass

   if ISNUMBER( 1 ) .and. ISNUMBER( 2 ) .and. ISNUMBER( 3 )

      oClass := TClass():New( "TOPBARMENU" )
      oClass:AddData( "cargo" )
      oClass:AddData( "colorSpec"   ,  "N/W,W/N,W+/W,W+/N,N+/W,W/N" )
      oClass:AddData( "current"     ,  0 )
      oClass:AddData( "itemCount"   ,  0 )
      oClass:AddData( "left"        ,  nLeft )
      oClass:AddData( "right"       ,  nRight )
      oClass:AddData( "row"         ,  nRow )
      oClass:AddData( "aItems"      ,  {} )

      oClass:AddMethod( "AddItem"   ,  @AddItem() )
      oClass:AddMethod( "DelItem"   ,  @DelItem() )
      oClass:AddMethod( "Display"   ,  @Display() )
      oClass:AddMethod( "GetAccel"  ,  @GetAccel() )
      oClass:AddMethod( "GetFirst"  ,  @GetFirst() )
      oClass:AddMethod( "GetItem"   ,  @GetItem() )
      oClass:AddMethod( "GetLast"   ,  @GetLast() )
      oClass:AddMethod( "GetNext"   ,  @GetNext() )
      oClass:AddMethod( "GetPrev"   ,  @GetPrev() )

      /* NOTE: This method exists but it is not
               documented in the manuals nor the NG's [jlalin]
      */
      oClass:AddMethod( "GetShortct",  @GetShortct() )

      oClass:AddMethod( "HitTest"   ,  @HitTest() )
      oClass:AddMethod( "InsItem"   ,  @InsItem() )
      oClass:AddMethod( "Select"    ,  @_Select() )
      oClass:AddMethod( "SetItem"   ,  @SetItem() )
  
      oClass:Create()
   else
      return nil
   endif

return oClass:Instance()

//--------------------------------------------------------------------------//
static function AddItem( oItem )

   LOCAL Self  := QSelf()
   LOCAL oLast

   ::itemCount++

   if ::itemCount > 1
      oLast := ATail( ::aItems )
      oItem:column := oLast:column + Len( StrTran( oLast:caption, "&", "" ) ) + 2
   endif

   aAdd( ::aItems, oItem )

return Self

//--------------------------------------------------------------------------//
static function DelItem( nPos )

   LOCAL Self  := QSelf()

   if nPos > 0 .and. nPos <= Len( ::aItems )
      aDel( ::aItems, nPos )
      aSize( ::aItems, Len( ::aItems ) - 1 )
   endif

return Self

//--------------------------------------------------------------------------//
static function GetFirst()

   LOCAL Self  := QSelf()
   LOCAL n

   for n := 1 to ::itemCount
      if ::aItems[ n ]:enabled
         return n
      endif
   next

return 0

//--------------------------------------------------------------------------//
static function GetItem( nPos )

   LOCAL Self  := QSelf()
   LOCAL oItem

   if nPos > 0 .and. nPos <= ::itemCount
      oItem := ::aItems[ nPos ]
   endif

return oItem

//--------------------------------------------------------------------------//
static function GetLast()

   LOCAL Self  := QSelf()
   LOCAL n

   for n := ::itemCount to 1 step -1
      if ::aItems[ n ]:enabled
         return n
      endif
   next

return 0

//--------------------------------------------------------------------------//
static function GetNext()

   LOCAL Self  := QSelf()
   LOCAL n

   if ::current < ::itemCount
      for n := ::current + 1 to ::itemCount
         if ::aItems[ n ]:enabled
            return n
         endif
      next
  endif

return 0

//--------------------------------------------------------------------------//
static function GetPrev()

   LOCAL Self  := QSelf()
   LOCAL n

   if ::current > 1
      for n := ::current - 1 to 1 step -1
         if ::aItems[ n ]:enabled
            return n
         endif
      next
   endif

return 0

//--------------------------------------------------------------------------//
/* NOTE: This method corrects two bugs in Cl*pper:
         1) when two menuitems have the same key and the
            first item is disabled
         2) when a menuitem is disabled it will ignore the key [jlalin]
*/
static function GetAccel( nKey )

   LOCAL Self  := QSelf()
   LOCAL nAt   := 0
   LOCAL cKey  := Upper( __AltToKey( nKey ) ) /* By now */
   LOCAL n

   for n := 1 to ::itemCount
      nAt := At( "&", ::aItems[ n ]:caption )
      if nAt > 0 .and. ::aItems[ n ]:enabled .and. ;
            Upper( SubStr( ::aItems[ n ]:caption, nAt + 1, 1 ) ) == cKey
         return n
      endif
   next

return 0

//--------------------------------------------------------------------------//
/* NOTE: In my tests I can't get other values than HTNOWHERE or a value
         greather than 0 (selected item), althought the NG's says that
         it returns other HT* values [jlalin]

         This method correct a bug in Cl*pper:
         when click on a disabled menuitem it will ignore it [jlalin]
*/
static function HitTest( nRow, nCol )

   LOCAL Self  := QSelf()
   LOCAL n

   if ::row == nRow
      for n := 1 to ::itemCount
         if nCol >= ::aItems[ n ]:column .and. ;
               nCol <= ::aItems[ n ]:column + Len( ::aItems[ n ]:caption ) .and. ;
               ::aItems[ n ]:enabled
            return n
         endif
      next
   endif

return HTNOWHERE

//--------------------------------------------------------------------------//
static function InsItem( nPos, oItem )

   LOCAL Self  := QSelf()
   LOCAL n

   if nPos > 0 .and. nPos <= ::itemCount

      aSize( ::aItems, ::itemCount + 1 )

      for n := ::itemCount to nPos
         ::aItems[ n ] := ::aItems[ n - 1 ]
      next

      ::aItems[ nPos ] := oItem
      ::itemCount := Len( ::aItems )

   endif

return Self

//--------------------------------------------------------------------------//
static function _Select( nPos )

   LOCAL Self  := QSelf()

   if ( nPos > 0 .and. nPos <= ::itemCount ) .and. ;
         nPos != ::current .and. ::aItems[ nPos ]:enabled

      if ::current > 0
         if ::aItems[ ::current ]:isPopUp()
            ::aItems[ ::current ]:data:Close()
         endif
      endif

      ::current := nPos
   else
      ::current := 0
   endif

return Self

//--------------------------------------------------------------------------//
static function SetItem( nPos, oItem )

   LOCAL Self  := QSelf()

   if nPos > 0 .and. nPos <= ::itemCount
      ::aItems[ nPos ] := oItem
   endif

return Self

//--------------------------------------------------------------------------//
static function GetShortct( nKey )

   LOCAL Self  := QSelf()
   LOCAL n

   for n := 1 to ::itemCount
      if ::aItems[ n ]:shortcut == nKey
         return n
      endif
   next

return 0

//--------------------------------------------------------------------------//
static function Display()

   LOCAL Self  := QSelf()
   LOCAL oPopup
   LOCAL nAt
   LOCAL n
   LOCAL cPrompt

   LOCAL nOldRow  := Row()
   LOCAL nOldCol  := Col()
   LOCAL lOldCur  := MSetCursor( FALSE )

   DispBegin()

   DispOutAt( ::row, ::left, ;
              Space( ::right - ::left + 1 ), hb_ColorIndex( ::colorSpec, CLR_STANDARD ) )

   for n := 1 to ::itemCount

      nAt := At( "&", ::aItems[ n ]:caption )
      cPrompt := " " + StrTran( ::aItems[ n ]:caption, "&", "" ) + " "

      DispOutAt( ;
         ::row, ::aItems[ n ]:column, ;
         cPrompt, ;
         hb_ColorIndex( ::colorSpec, ;
            iif( ::aItems[ n ]:enabled, ;
               iif( n == ::current, CLR_ENHANCED, CLR_STANDARD ), ;
               CLR_UNSELECTED ) ) )

      if nAt > 0
         DispOutAt( ::row, ::aItems[ n ]:column + nAt, ;
            SubStr( ::aItems[ n ]:caption, nAt + 1, 1 ), ;
            hb_ColorIndex( ::colorSpec, ;
               iif( n == ::current, CLR_BACKGROUND, CLR_BORDER ) ) )
      endif

      if ::aItems[ n ]:isPopup()
         ::aItems[ n ]:data:SetCoors( n, ::row + 1, ::aItems[ n ]:column )
      endif

   next

   if ::current > 0 .and. ::aItems[ ::current ]:isPopup()
      oPopUp  := ::aItems[ ::current ]:data
      if oPopUp:isOpen()
         oPopUp:display()
      endif
   endif

   DevPos( nOldRow, nOldCol )
   MSetCursor( lOldCur )

   DispEnd()

return Self
//--------------------------------------------------------------------------//

static function __AltToKey( nKey )

   local nIndex := AScan( { K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F,;
                            K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L,;
                            K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R,;
                            K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X,;
                            K_ALT_Y, K_ALT_Z }, nKey )

return iif( nIndex > 0, SubStr( "ABCDEFGHIJKLMNOPQRSTUVWXYZ", nIndex, 1 ), "" )

#endif
