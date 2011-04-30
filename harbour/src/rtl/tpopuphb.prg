/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * POPUP menu class (Harbour extended)
 *
 * Copyright 2011 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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

#include "hbclass.ch"

#ifdef HB_COMPAT_C53

CREATE CLASS hb_POPUPMENU INHERIT hbPOPUPMENU

   EXPORTED:

   METHOD shadowed( lShadowed ) SETGET

   METHOD setCoors( nRow, nCol, lTop )
   METHOD isShortCut( nKey, nID )
   METHOD isQuick( nKey, nID )

ENDCLASS

METHOD shadowed( lShadowed ) CLASS hb_POPUPMENU

   IF lShadowed != NIL
      ::lShadowed := __eInstVar53( Self, "SHADOWED", lShadowed, "L", 1001 )
   ENDIF

   RETURN ::lShadowed

METHOD setCoors( nRow, nCol, lTop ) CLASS hb_POPUPMENU
   LOCAL oItem
   LOCAL nDif

   ::setMetrics()

   IF ::nTop == -1 .OR. ::nLeft == -1
      ::nTop    := nRow
      ::nLeft   := nCol
      ::nBottom := ::nTop + ::nItemCount + 1
      ::nRight  := ::nLeft + ::nWidth - 1

      IF ::nRight > MaxCol()
         nDif     := ::nRight - MaxCol()
         ::nRight -= nDif
         ::nLeft  -= nDif
         IF !lTop
            ::nTop++
            ::nBottom++
         ENDIF
      ENDIF

      IF ::nLeft < 0
         nDif     := ::nLeft
         ::nRight -= nDif
         ::nLeft  -= nDif
      ENDIF

      IF ::nBottom > MaxRow()
         nDif      := ::nBottom - MaxRow()
         ::nBottom -= nDif
         ::nTop    -= nDif
      ENDIF

      IF ::nTop < 0
         nDif      := ::nTop
         ::nBottom -= nDif
         ::nTop    -= nDif
      ENDIF

      FOR EACH oItem IN ::aItems
         IF oItem:isPopup()
            oItem:data:setCoors( nRow + oItem:__enumIndex(), ::nRight + 1, .F. )
         ENDIF
      NEXT
   ENDIF

   RETURN Self

METHOD isShortCut( nKey, nID ) CLASS hb_POPUPMENU

   LOCAL nItem
   LOCAL nTotal
   LOCAL nShortCut
   LOCAL oItem
   LOCAL i

   DO CASE
   // Test and assign top menu item shortCut, enabled, and !PopUp:
   // Changed by enclosing assignment before ':Enabled':
   CASE ( ( nShortCut := ::getShortCt( nKey ) ) > 0 ) .AND. ;
          ( ( oItem := ::getItem( nShortcut ) ):enabled ) .AND. ;
          ( !( oItem:isPopUp() ) )
      ::select( nShortCut )
      Eval( oItem:data, oItem )
      nID := oItem:id

      RETURN .T.

   // Test and assignment for TopBar MenuItem:
   CASE nShortCut == 0
      nTotal := ::nItemCount
      nItem  := ::nCurrent
      IF nItem == 0
         nItem := 1
      ENDIF

      // Loop to wrap around through TopMenu from Current Item:
      FOR i := 1 TO nTotal
         IF !( oItem := ::getItem( nItem ) ):enabled
         ELSEIF !oItem:isPopUp()
         ELSEIF oItem:data:isQuick( nKey, @nID )
            RETURN .T.
         ENDIF
         IF ++nItem > nTotal
            nItem := 1
         ENDIF
      NEXT

   ENDCASE

   RETURN .F.

METHOD isQuick( nKey, nID ) CLASS hb_POPUPMENU

   LOCAL nItem
   LOCAL nTotal
   LOCAL nShortCut
   LOCAL oItem

   IF ( nShortCut := ::getShortCt( nKey ) ) == 0

      nTotal := ::nItemCount

      FOR nItem := 1 TO nTotal
         IF !( oItem := ::getItem( nItem ) ):Enabled
         ELSEIF ! oItem:isPopUp()
         ELSEIF oItem:Data:isQuick( nKey, @nID )
            RETURN .T.
         ENDIF
      NEXT

   ELSEIF !( oItem := ::getItem( nShortCut ) ):IsPopUp()

      IF oItem:enabled
         ::select( nShortCut )
         Eval( oItem:Data, oItem )
         nID := oItem:id
         RETURN .T.
      ENDIF

   ENDIF

   RETURN .F.

#endif
