/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszel] */

#include "hbclass.ch"
#include "hbmemvar.ch"
#include "box.ch"
#include "inkey.ch"
#include "common.ch"
#include "setcurs.ch"

CLASS TDbWindow  // Debugger windows and dialogs

   DATA   nTop, nLeft, nBottom, nRight
   DATA   cCaption
   DATA   cBackImage, cColor
   DATA   lFocused, bGotFocus, bLostFocus
   DATA   bKeyPressed, bPainted
   DATA   lShadow
   DATA   Cargo

   METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor )
   METHOD Hide()
   METHOD IsOver( nRow, nCol )
   METHOD nWidth() INLINE ::nRight - ::nLeft + 1
   METHOD ScrollUp( nLines )
   METHOD SetCaption( cCaption )
   METHOD SetFocus( lOnOff )
   METHOD Show( lFocused )
   METHOD ShowModal()
   METHOD Move()
   METHOD KeyPressed( nKey )

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor ) CLASS TDbWindow

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight
   ::cCaption := cCaption
   ::cColor   := cColor
   ::lShadow  := .f.

return Self

METHOD Hide() CLASS TDbWindow

   RestScreen( ::nTop, ::nLeft, ::nBottom + iif( ::lShadow, 1, 0 ),;
               ::nRight + iif( ::lShadow, 2, 0 ), ::cBackImage )
   ::cBackImage := nil

return nil

METHOD IsOver( nRow, nCol ) CLASS TDbWindow

return nRow >= ::nTop .and. nRow <= ::nBottom .and. ;
       nCol >= ::nLeft .and. nCol <= ::nRight

METHOD ScrollUp( nLines ) CLASS TDbWindow

   DEFAULT nLines TO 1

   SetColor( ::cColor )
   Scroll( ::nTop + 1, ::nLeft + 1, ::nBottom - 1, ::nRight - 1, nLines )

return nil

METHOD SetCaption( cCaption ) CLASS TDbWindow

   local nOldLen := iif( ::cCaption != nil, Len( ::cCaption ), 0 )

   ::cCaption := cCaption

   if ! Empty( cCaption )
      DispOutAt( ::nTop, ::nLeft + ( ( ::nRight - ::nLeft ) / 2 ) - ;
         ( ( Len( cCaption ) + 2 ) / 2 ),;
         " " + cCaption + " ", ::cColor )
   endif

return nil

METHOD SetFocus( lOnOff ) CLASS TDbWindow

   if ! lOnOff .and. ::bLostFocus != nil
      Eval( ::bLostFocus, Self )
   endif

   DispBegin()

   ::lFocused := lOnOff

   @ ::nTop, ::nLeft, ::nBottom, ::nRight BOX iif( lOnOff, B_DOUBLE, B_SINGLE ) ;
      COLOR ::cColor

   DispOutAt( ::nTop, ::nLeft + 1, "[" + Chr( 254 ) + "]", ::cColor )

   if ! Empty( ::cCaption )
      ::SetCaption( ::cCaption )
   endif

   if ::bPainted != nil
      Eval( ::bPainted, Self )
   endif

   DispEnd()

   if lOnOff .and. ::bGotFocus != nil
      Eval( ::bGotFocus, Self )
   endif

return nil

METHOD Show( lFocused ) CLASS TDbWindow

   DEFAULT lFocused TO .f.

   ::cBackImage := SaveScreen( ::nTop, ::nLeft, ::nBottom + iif( ::lShadow, 1, 0 ),;
                              ::nRight + iif( ::lShadow, 2, 0 ) )
   SetColor( ::cColor )
   Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight )
   ::SetFocus( lFocused )

   If ::lShadow
      hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   endif

return nil

METHOD ShowModal() CLASS TDbWindow

   local lExit := .f.
   local nKey

   ::lShadow := .t.
   ::Show()

   while ! lExit
      nKey := InKey( 0 )

      if ::bKeyPressed != nil
         Eval( ::bKeyPressed, nKey )
      endif

      do case
         case nKey == K_ESC
              lExit := .t.
      endcase
   end

   ::Hide()

return nil

/*Method move()
Move a window across the screen
Copyright Luiz Rafael Culik 1999
*/
METHOD Move() Class TDbWindow

   local nOldTop    := ::nTop
   local nOldLeft   := ::nLeft
   local nOldBottom := ::nbottom
   local nOldRight  := ::nright
   local nKey

   while .t.
      RestScreen( ,,,, ::cbackimage )
      DispBox( ::nTop, ::nLeft, ::nRight, ::nBottom, Replicate( Chr( 176 ), 8 ) + " " )

      nKey := Inkey( 0 )

      do case
         case nkey == K_UP
              if ::ntop != 0
                 ::ntop--
                 ::nbottom--
              endif

         case nKey == K_DOWN
              if ::nBottom != MaxRow()
                 ::nTop++
                 ::nBottom++
              endif

         case nKey == K_LEFT
              if ::nLeft != 0
                 ::nLeft--
                 ::nRight--
              endif

         case nKey == K_RIGHT
              if ::nBottom != MaxRow()
                 ::nLeft++
                 ::nRight++
              endif

         case nKey == K_ESC
              ::nTop    := nOldTop
              ::nLeft   := nOldLeft
              ::nBottom := nOldBottom
              ::nRight  := nOldRight
      endcase

      if nKey == K_ESC .or. nKey == K_ENTER
         exit
      end
   end

   // __keyboard( chr( 0 ) ), inkey() )

return nil

METHOD KeyPressed( nKey ) CLASS TDbWindow

   if ::bKeyPressed != nil
      Eval( ::bKeyPressed, nKey, Self )
   endif

return nil
